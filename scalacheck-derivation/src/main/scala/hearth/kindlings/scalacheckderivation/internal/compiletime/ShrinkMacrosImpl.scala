package hearth.kindlings.scalacheckderivation.internal.compiletime

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

import org.scalacheck.Shrink

trait ShrinkMacrosImpl
    extends hearth.kindlings.derivation.compiletime.DerivationTimeout
    with rules.ShrinkUseCachedRuleImpl
    with rules.ShrinkUseImplicitRuleImpl
    with rules.ShrinkBuiltInRuleImpl
    with rules.ShrinkHandleAsValueTypeRuleImpl
    with rules.ShrinkHandleAsOptionRuleImpl
    with rules.ShrinkHandleAsMapRuleImpl
    with rules.ShrinkHandleAsCollectionRuleImpl
    with rules.ShrinkHandleAsSingletonRuleImpl
    with rules.ShrinkHandleAsCaseClassRuleImpl
    with rules.ShrinkHandleAsEnumRuleImpl { this: MacroCommons & StdExtensions =>

  override protected def derivationSettingsNamespace: String = "scalacheckDerivation"

  // Entrypoint
  @scala.annotation.nowarn("msg=is never used")
  def deriveShrink[A: Type]: Expr[Shrink[A]] = {
    val macroName = "DeriveShrink.derived"
    implicit val ShrinkA: Type[Shrink[A]] = ShrinkTypes.Shrink[A]

    if (Type[A] =:= Type.of[Nothing].asInstanceOf[Type[A]] || Type[A] =:= Type.of[Any].asInstanceOf[Type[A]])
      Environment.reportErrorAndAbort(
        s"$macroName: type parameter was inferred as ${Type[A].prettyPrint}, which is likely unintended.\n" +
          "Provide an explicit type parameter, e.g.: DeriveShrink.derived[MyType]\n" +
          "or add a type ascription to the result variable."
      )

    Log
      .namedScope(s"Deriving $macroName[${Type[A].prettyPrint}] at: ${Environment.currentPosition.prettyPrint}") {
        MIO.scoped { runSafe =>
          val selfType: Option[??] = Some(Type[A].as_??)

          // Step 1: Run derivation once to populate cache
          val ctx = ShrinkCtx.from[A](derivedType = selfType)
          runSafe {
            for {
              _ <- Environment.loadStandardExtensions().toMIO(allowFailures = false)
              _ <- deriveShrinkRecursively[A](using ctx)
            } yield ()
          }

          // Step 2: Get the cached helper reference and cache
          val helperOpt = runSafe(ctx.getHelper[A])
          val cache = runSafe(ctx.cache.get)

          // Step 3: Build the Shrink instance
          val directShrinkOpt: Option[Expr[Shrink[A]]] =
            if (helperOpt.isDefined) None
            else {
              Some(runSafe {
                val freshCtx = ShrinkCtx.from[A](derivedType = selfType)
                for {
                  _ <- Environment.loadStandardExtensions().toMIO(allowFailures = false)
                  result <- deriveShrinkRecursively[A](using freshCtx)
                  freshCache <- freshCtx.cache.get
                } yield freshCache.toValDefs.use(_ => result)
              })
            }

          // Step 4: Wrap cached defs around the Shrink instance
          cache.toValDefs.use { _ =>
            val shrinkExpr: Expr[Shrink[A]] = helperOpt match {
              case Some(helper) =>
                helper(Expr.quote(()))
              case None =>
                directShrinkOpt.get
            }
            shrinkExpr
          }
        }
      }
      .flatTap(result => Log.info(s"Derived final result: ${result.prettyPrint}"))
      .runToExprOrFail(
        macroName,
        infoRendering = if (shouldWeLogShrinkDerivation) RenderFrom(Log.Level.Info) else DontRender,
        errorRendering = if (shouldWeLogShrinkDerivation) RenderFrom(Log.Level.Info) else DontRender,
        timeout = derivationTimeout
      ) { (errorLogs, errors) =>
        val errorsRendered = errors.map(e => "  - " + e.getMessage).mkString("\n")
        val hint =
          "Enable debug logging with: import hearth.kindlings.scalacheckderivation.debug.logDerivationForScalaCheckDerivation"
        if (errorLogs.nonEmpty) s"Macro derivation failed:\n$errorsRendered\nlogs:\n$errorLogs\n$hint"
        else s"Macro derivation failed:\n$errorsRendered\n$hint"
      }
  }

  // Context

  final case class ShrinkCtx[A](
      tpe: Type[A],
      cache: MLocal[ValDefsCache],
      derivedType: Option[??]
  ) {
    def nest[B: Type]: ShrinkCtx[B] =
      ShrinkCtx(Type[B], cache, derivedType)

    def getHelper[B: Type]: MIO[Option[Expr[Unit] => Expr[Shrink[B]]]] = {
      implicit val UnitT: Type[Unit] = ShrinkTypes.Unit
      implicit val ShrinkB: Type[Shrink[B]] = ShrinkTypes.Shrink[B]
      cache.get1Ary[Unit, Shrink[B]]("cached-shrink-method")
    }
  }

  object ShrinkCtx {
    def from[A: Type](derivedType: Option[??]): ShrinkCtx[A] =
      ShrinkCtx(Type[A], ValDefsCache.mlocal, derivedType)
  }

  def shrinkctx[A](implicit A: ShrinkCtx[A]): ShrinkCtx[A] = A
  implicit def shrinkCtxType[A: ShrinkCtx]: Type[A] = shrinkctx.tpe

  abstract class ShrinkDerivationRule(val name: String) extends Rule {
    def apply[A: ShrinkCtx]: MIO[Rule.Applicability[Expr[Shrink[A]]]]
  }

  // Recursive derivation

  def deriveShrinkRecursively[A: ShrinkCtx]: MIO[Expr[Shrink[A]]] =
    Log.namedScope(s"Deriving Shrink for ${Type[A].prettyPrint}") {
      Rules(
        ShrinkUseCachedRule,
        ShrinkUseImplicitRule,
        ShrinkBuiltInRule,
        ShrinkHandleAsValueTypeRule,
        ShrinkHandleAsOptionRule,
        ShrinkHandleAsMapRule,
        ShrinkHandleAsCollectionRule,
        ShrinkHandleAsSingletonRule,
        ShrinkHandleAsCaseClassRule,
        ShrinkHandleAsEnumRule
      )(_[A]).flatMap {
        case Right(result) =>
          Log.info(s"Derived Shrink for ${Type[A].prettyPrint}") >> MIO.pure(result)
        case Left(reasons) =>
          val reasonsStrings = reasons.toListMap
            .removed(ShrinkUseCachedRule)
            .view
            .map { case (rule, reasons) =>
              if (reasons.isEmpty) s"The rule ${rule.name} was not applicable"
              else s" - ${rule.name}: ${reasons.mkString(", ")}"
            }
            .toList
          val err = ShrinkDerivationError.UnsupportedType(Type[A].prettyPrint, reasonsStrings)
          Log.error(err.message) >> MIO.fail(err)
      }
    }

  protected object ShrinkTypes {
    def Shrink: Type.Ctor1[org.scalacheck.Shrink] = Type.Ctor1.of[org.scalacheck.Shrink]
    val Unit: Type[Unit] = Type.of[Unit]
    val Any: Type[Any] = Type.of[Any]
    val ArrayAny: Type[Array[Any]] = Type.of[Array[Any]]
    val LogDerivation: Type[hearth.kindlings.scalacheckderivation.LogDerivation] =
      Type.of[hearth.kindlings.scalacheckderivation.LogDerivation]
  }

  def shouldWeLogShrinkDerivation: Boolean = {
    implicit val LogDerivationType: Type[hearth.kindlings.scalacheckderivation.LogDerivation] =
      ShrinkTypes.LogDerivation
    Expr.summonImplicit[hearth.kindlings.scalacheckderivation.LogDerivation].isDefined || (for {
      data <- Environment.typedSettings.toOption
      cd <- data.get("scalacheckDerivation")
      shouldLog <- cd.get("logDerivation").flatMap(_.asBoolean)
    } yield shouldLog).getOrElse(false)
  }
}

sealed private[compiletime] trait ShrinkDerivationError
    extends util.control.NoStackTrace
    with Product
    with Serializable {
  def message: String
  override def getMessage(): String = message
}

private[compiletime] object ShrinkDerivationError {
  final case class UnsupportedType(tpeName: String, reasons: List[String]) extends ShrinkDerivationError {
    override def message: String =
      s"The type $tpeName was not handled by any Shrink derivation rule:\n${reasons.mkString("\n")}"
  }
}
