package hearth.kindlings.scalacheckderivation.internal.compiletime

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

import org.scalacheck.{Arbitrary, Gen}

trait ArbitraryMacrosImpl
    extends rules.ArbitraryUseCachedRuleImpl
    with rules.ArbitraryUseImplicitRuleImpl
    with rules.ArbitraryBuiltInRuleImpl
    with rules.ArbitraryHandleAsOptionRuleImpl
    with rules.ArbitraryHandleAsCollectionRuleImpl
    with rules.ArbitraryHandleAsSingletonRuleImpl
    with rules.ArbitraryHandleAsCaseClassRuleImpl
    with rules.ArbitraryHandleAsEnumRuleImpl { this: MacroCommons & StdExtensions & LoadStandardExtensionsOnce =>

  // Entrypoint
  @scala.annotation.nowarn("msg=is never used")
  def deriveArbitrary[A: Type]: Expr[Arbitrary[A]] = {
    val macroName = "DeriveArbitrary.derived"
    implicit val ArbitraryA: Type[Arbitrary[A]] = ArbitraryTypes.Arbitrary[A]
    implicit val GenA: Type[Gen[A]] = ArbitraryTypes.Gen[A]

    if (Type[A] =:= Type.of[Nothing].asInstanceOf[Type[A]] || Type[A] =:= Type.of[Any].asInstanceOf[Type[A]])
      Environment.reportErrorAndAbort(
        s"$macroName: type parameter was inferred as ${Type[A].prettyPrint}, which is likely unintended.\n" +
          "Provide an explicit type parameter, e.g.: DeriveArbitrary.derived[MyType]\n" +
          "or add a type ascription to the result variable."
      )

    Log
      .namedScope(s"Deriving $macroName[${Type[A].prettyPrint}] at: ${Environment.currentPosition.prettyPrint}") {
        MIO.scoped { runSafe =>
          val selfType: Option[??] = Some(Type[A].as_??)

          // Step 1: Run derivation once to populate cache
          val ctx = ArbitraryCtx.from[A](derivedType = selfType)
          runSafe {
            for {
              _ <- ensureStandardExtensionsLoaded()
              _ <- deriveArbitraryRecursively[A](using ctx)
            } yield ()
          }

          // Step 2: Get the cached helper reference and cache
          val helperOpt = runSafe(ctx.getHelper[A])
          val cache = runSafe(ctx.cache.get)

          // Step 3: Build the Arbitrary instance
          val directGenOpt: Option[Expr[Gen[A]]] =
            if (helperOpt.isDefined) None
            else {
              // For types handled without a cached helper (options, collections),
              // build a direct Gen[A] that we can use in the Arbitrary body
              Some(runSafe {
                val freshCtx = ArbitraryCtx.from[A](derivedType = selfType)
                for {
                  _ <- ensureStandardExtensionsLoaded()
                  result <- deriveArbitraryRecursively[A](using freshCtx)
                  freshCache <- freshCtx.cache.get
                } yield freshCache.toValDefs.use(_ => result)
              })
            }

          // Step 4: Wrap cached defs around the Arbitrary instance
          cache.toValDefs.use { _ =>
            val genExpr: Expr[Gen[A]] = helperOpt match {
              case Some(helper) =>
                // Call the cached helper method
                helper(Expr.quote(()))
              case None =>
                // Use the direct Gen[A]
                directGenOpt.get
            }

            Expr.quote {
              _root_.org.scalacheck.Arbitrary(Expr.splice(genExpr))
            }
          }
        }
      }
      .flatTap(result => Log.info(s"Derived final result: ${result.prettyPrint}"))
      .runToExprOrFail(
        macroName,
        infoRendering = if (shouldWeLogArbitraryDerivation) RenderFrom(Log.Level.Info) else DontRender,
        errorRendering = if (shouldWeLogArbitraryDerivation) RenderFrom(Log.Level.Info) else DontRender
      ) { (errorLogs, errors) =>
        val errorsRendered = errors.map(e => "  - " + e.getMessage).mkString("\n")
        val hint =
          "Enable debug logging with: import hearth.kindlings.scalacheckderivation.debug.logDerivationForScalaCheckDerivation"
        if (errorLogs.nonEmpty) s"Macro derivation failed:\n$errorsRendered\nlogs:\n$errorLogs\n$hint"
        else s"Macro derivation failed:\n$errorsRendered\n$hint"
      }
  }

  // Context

  final case class ArbitraryCtx[A](
      tpe: Type[A],
      cache: MLocal[ValDefsCache],
      derivedType: Option[??]
  ) {
    def nest[B: Type]: ArbitraryCtx[B] =
      ArbitraryCtx(Type[B], cache, derivedType)

    def getHelper[B: Type]: MIO[Option[Expr[Unit] => Expr[Gen[B]]]] = {
      implicit val UnitT: Type[Unit] = ArbitraryTypes.Unit
      implicit val GenB: Type[Gen[B]] = ArbitraryTypes.Gen[B]
      cache.get1Ary[Unit, Gen[B]]("cached-arbitrary-method")
    }
  }

  object ArbitraryCtx {
    def from[A: Type](derivedType: Option[??]): ArbitraryCtx[A] =
      ArbitraryCtx(Type[A], ValDefsCache.mlocal, derivedType)
  }

  def arbctx[A](implicit A: ArbitraryCtx[A]): ArbitraryCtx[A] = A
  implicit def arbitraryCtxType[A: ArbitraryCtx]: Type[A] = arbctx.tpe

  abstract class ArbitraryDerivationRule(val name: String) extends Rule {
    def apply[A: ArbitraryCtx]: MIO[Rule.Applicability[Expr[Gen[A]]]]
  }

  // Recursive derivation

  def deriveArbitraryRecursively[A: ArbitraryCtx]: MIO[Expr[Gen[A]]] =
    Log.namedScope(s"Deriving Arbitrary for ${Type[A].prettyPrint}") {
      Rules(
        ArbitraryUseCachedRule,
        ArbitraryUseImplicitRule,
        ArbitraryBuiltInRule,
        ArbitraryHandleAsOptionRule,
        ArbitraryHandleAsCollectionRule,
        ArbitraryHandleAsSingletonRule,
        ArbitraryHandleAsCaseClassRule,
        ArbitraryHandleAsEnumRule
      )(_[A]).flatMap {
        case Right(result) =>
          Log.info(s"Derived Arbitrary for ${Type[A].prettyPrint}") >> MIO.pure(result)
        case Left(reasons) =>
          val reasonsStrings = reasons.toListMap
            .removed(ArbitraryUseCachedRule)
            .view
            .map { case (rule, reasons) =>
              if (reasons.isEmpty) s"The rule ${rule.name} was not applicable"
              else s" - ${rule.name}: ${reasons.mkString(", ")}"
            }
            .toList
          val err = ArbitraryDerivationError.UnsupportedType(Type[A].prettyPrint, reasonsStrings)
          Log.error(err.message) >> MIO.fail(err)
      }
    }

  protected object ArbitraryTypes {
    def Arbitrary: Type.Ctor1[org.scalacheck.Arbitrary] = Type.Ctor1.of[org.scalacheck.Arbitrary]
    def Gen: Type.Ctor1[org.scalacheck.Gen] = Type.Ctor1.of[org.scalacheck.Gen]
    val Unit: Type[Unit] = Type.of[Unit]
    val Any: Type[Any] = Type.of[Any]
    val ListAny: Type[List[Any]] = Type.of[List[Any]]
    val ListGenAny: Type[List[org.scalacheck.Gen[Any]]] = Type.of[List[org.scalacheck.Gen[Any]]]
    val LogDerivation: Type[hearth.kindlings.scalacheckderivation.LogDerivation] =
      Type.of[hearth.kindlings.scalacheckderivation.LogDerivation]
  }

  def shouldWeLogArbitraryDerivation: Boolean = {
    implicit val LogDerivationType: Type[hearth.kindlings.scalacheckderivation.LogDerivation] =
      ArbitraryTypes.LogDerivation
    Expr.summonImplicit[hearth.kindlings.scalacheckderivation.LogDerivation].isDefined || (for {
      data <- Environment.typedSettings.toOption
      cd <- data.get("scalacheckDerivation")
      shouldLog <- cd.get("logDerivation").flatMap(_.asBoolean)
    } yield shouldLog).getOrElse(false)
  }
}

sealed private[compiletime] trait ArbitraryDerivationError
    extends util.control.NoStackTrace
    with Product
    with Serializable {
  def message: String
  override def getMessage(): String = message
}

private[compiletime] object ArbitraryDerivationError {
  final case class UnsupportedType(tpeName: String, reasons: List[String]) extends ArbitraryDerivationError {
    override def message: String =
      s"The type $tpeName was not handled by any Arbitrary derivation rule:\n${reasons.mkString("\n")}"
  }
}
