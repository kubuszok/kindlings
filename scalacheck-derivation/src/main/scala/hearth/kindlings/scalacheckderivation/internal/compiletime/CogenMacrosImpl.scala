package hearth.kindlings.scalacheckderivation.internal.compiletime

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

import org.scalacheck.Cogen

trait CogenMacrosImpl
    extends hearth.kindlings.derivation.compiletime.DerivationTimeout
    with rules.CogenUseCachedRuleImpl
    with rules.CogenUseImplicitRuleImpl
    with rules.CogenBuiltInRuleImpl
    with rules.CogenHandleAsOptionRuleImpl
    with rules.CogenHandleAsCollectionRuleImpl
    with rules.CogenHandleAsSingletonRuleImpl
    with rules.CogenHandleAsCaseClassRuleImpl
    with rules.CogenHandleAsEnumRuleImpl { this: MacroCommons & StdExtensions =>

  override protected def derivationSettingsNamespace: String = "scalacheckDerivation"

  @scala.annotation.nowarn("msg=is never used")
  def deriveCogen[A: Type]: Expr[Cogen[A]] = {
    val macroName = "DeriveCogen.derived"
    implicit val CogenA: Type[Cogen[A]] = CogenTypes.Cogen[A]

    if (Type[A] =:= Type.of[Nothing].asInstanceOf[Type[A]] || Type[A] =:= Type.of[Any].asInstanceOf[Type[A]])
      Environment.reportErrorAndAbort(
        s"$macroName: type parameter was inferred as ${Type[A].prettyPrint}, which is likely unintended.\n" +
          "Provide an explicit type parameter, e.g.: DeriveCogen.derived[MyType]"
      )

    Log
      .namedScope(s"Deriving $macroName[${Type[A].prettyPrint}] at: ${Environment.currentPosition.prettyPrint}") {
        MIO.scoped { runSafe =>
          val selfType: Option[??] = Some(Type[A].as_??)
          val ctx = CogenCtx.from[A](derivedType = selfType)
          runSafe {
            for {
              _ <- Environment.loadStandardExtensions().toMIO(allowFailures = false)
              _ <- deriveCogenRecursively[A](using ctx)
            } yield ()
          }
          val helperOpt = runSafe(ctx.getHelper[A])
          val cache = runSafe(ctx.cache.get)

          val directCogenOpt: Option[Expr[Cogen[A]]] =
            if (helperOpt.isDefined) None
            else {
              Some(runSafe {
                val freshCtx = CogenCtx.from[A](derivedType = selfType)
                for {
                  _ <- Environment.loadStandardExtensions().toMIO(allowFailures = false)
                  result <- deriveCogenRecursively[A](using freshCtx)
                  freshCache <- freshCtx.cache.get
                } yield freshCache.toValDefs.use(_ => result)
              })
            }

          cache.toValDefs.use { _ =>
            helperOpt match {
              case Some(helper) => helper(Expr.quote(()))
              case None         => directCogenOpt.get
            }
          }
        }
      }
      .flatTap(result => Log.info(s"Derived final result: ${result.prettyPrint}"))
      .runToExprOrFail(
        macroName,
        infoRendering = if (shouldWeLogCogenDerivation) RenderFrom(Log.Level.Info) else DontRender,
        errorRendering = if (shouldWeLogCogenDerivation) RenderFrom(Log.Level.Info) else DontRender,
        timeout = derivationTimeout
      ) { (errorLogs, errors) =>
        val errorsRendered = errors.map(e => "  - " + e.getMessage).mkString("\n")
        val hint = "Enable debug logging with: import hearth.kindlings.scalacheckderivation.debug.logDerivationForScalaCheckDerivation"
        if (errorLogs.nonEmpty) s"Macro derivation failed:\n$errorsRendered\nlogs:\n$errorLogs\n$hint"
        else s"Macro derivation failed:\n$errorsRendered\n$hint"
      }
  }

  final case class CogenCtx[A](
      tpe: Type[A],
      cache: MLocal[ValDefsCache],
      derivedType: Option[??]
  ) {
    def nest[B: Type]: CogenCtx[B] = CogenCtx(Type[B], cache, derivedType)

    def getHelper[B: Type]: MIO[Option[Expr[Unit] => Expr[Cogen[B]]]] = {
      implicit val UnitT: Type[Unit] = CogenTypes.Unit
      implicit val CogenB: Type[Cogen[B]] = CogenTypes.Cogen[B]
      cache.get1Ary[Unit, Cogen[B]]("cached-cogen-method")
    }
  }

  object CogenCtx {
    def from[A: Type](derivedType: Option[??]): CogenCtx[A] =
      CogenCtx(Type[A], ValDefsCache.mlocal, derivedType)
  }

  def cogenctx[A](implicit A: CogenCtx[A]): CogenCtx[A] = A
  implicit def cogenCtxType[A: CogenCtx]: Type[A] = cogenctx.tpe

  abstract class CogenDerivationRule(val name: String) extends Rule {
    def apply[A: CogenCtx]: MIO[Rule.Applicability[Expr[Cogen[A]]]]
  }

  def deriveCogenRecursively[A: CogenCtx]: MIO[Expr[Cogen[A]]] =
    Log.namedScope(s"Deriving Cogen for ${Type[A].prettyPrint}") {
      Rules(
        CogenUseCachedRule,
        CogenUseImplicitRule,
        CogenBuiltInRule,
        CogenHandleAsOptionRule,
        CogenHandleAsCollectionRule,
        CogenHandleAsSingletonRule,
        CogenHandleAsCaseClassRule,
        CogenHandleAsEnumRule
      )(_[A]).flatMap {
        case Right(result) =>
          Log.info(s"Derived Cogen for ${Type[A].prettyPrint}") >> MIO.pure(result)
        case Left(reasons) =>
          val reasonsStrings = reasons.toListMap.removed(CogenUseCachedRule).view
            .map { case (rule, reasons) =>
              if (reasons.isEmpty) s"The rule ${rule.name} was not applicable"
              else s" - ${rule.name}: ${reasons.mkString(", ")}"
            }
            .toList
          val err = CogenDerivationError.UnsupportedType(Type[A].prettyPrint, reasonsStrings)
          Log.error(err.message) >> MIO.fail(err)
      }
    }

  protected object CogenTypes {
    def Cogen: Type.Ctor1[org.scalacheck.Cogen] = Type.Ctor1.of[org.scalacheck.Cogen]
    val Seed: Type[org.scalacheck.rng.Seed] = Type.of[org.scalacheck.rng.Seed]
    val Unit: Type[Unit] = Type.of[Unit]
    val Any: Type[Any] = Type.of[Any]
    val Long: Type[Long] = Type.of[Long]
    val LogDerivation: Type[hearth.kindlings.scalacheckderivation.LogDerivation] =
      Type.of[hearth.kindlings.scalacheckderivation.LogDerivation]
  }

  def shouldWeLogCogenDerivation: Boolean = {
    implicit val LogDerivationType: Type[hearth.kindlings.scalacheckderivation.LogDerivation] = CogenTypes.LogDerivation
    Expr.summonImplicit[hearth.kindlings.scalacheckderivation.LogDerivation].isDefined || (for {
      data <- Environment.typedSettings.toOption
      cd <- data.get("scalacheckDerivation")
      shouldLog <- cd.get("logDerivation").flatMap(_.asBoolean)
    } yield shouldLog).getOrElse(false)
  }
}

sealed private[compiletime] trait CogenDerivationError extends util.control.NoStackTrace with Product with Serializable {
  def message: String
  override def getMessage(): String = message
}

private[compiletime] object CogenDerivationError {
  final case class UnsupportedType(tpeName: String, reasons: List[String]) extends CogenDerivationError {
    override def message: String =
      s"The type $tpeName was not handled by any Cogen derivation rule:\n${reasons.mkString("\n")}"
  }
}
