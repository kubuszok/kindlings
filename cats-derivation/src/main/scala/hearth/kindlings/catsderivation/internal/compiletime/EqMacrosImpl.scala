package hearth.kindlings.catsderivation.internal.compiletime

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

import hearth.kindlings.catsderivation.LogDerivation

trait EqMacrosImpl
    extends rules.EqUseCachedRuleImpl
    with rules.EqUseImplicitRuleImpl
    with rules.EqValueTypeRuleImpl
    with rules.EqOptionRuleImpl
    with rules.EqSingletonRuleImpl
    with rules.EqCaseClassRuleImpl
    with rules.EqEnumRuleImpl { this: MacroCommons & StdExtensions =>

  @scala.annotation.nowarn("msg=is never used")
  def deriveEq[A: Type]: Expr[cats.kernel.Eq[A]] = {
    val macroName = "Eq.derived"
    implicit val EqA: Type[cats.kernel.Eq[A]] = EqTypes.Eq[A]
    implicit val BooleanType: Type[Boolean] = EqTypes.Boolean
    val selfType: Option[??] = Some(Type[A].as_??)

    deriveEqFromCtxAndAdaptForEntrypoint[A, cats.kernel.Eq[A]](macroName) { fromCtx =>
      Expr.quote {
        new cats.kernel.Eq[A] {
          def eqv(x: A, y: A): Boolean = {
            val _ = x
            val _ = y
            Expr.splice {
              fromCtx(EqCtx.from(Expr.quote(x), Expr.quote(y), derivedType = selfType))
            }
          }
        }
      }
    }
  }

  private def deriveEqFromCtxAndAdaptForEntrypoint[A: Type, Out: Type](macroName: String)(
      provideCtxAndAdapt: (EqCtx[A] => Expr[Boolean]) => Expr[Out]
  ): Expr[Out] = {
    if (Type[A] =:= Type.of[Nothing].asInstanceOf[Type[A]] || Type[A] =:= Type.of[Any].asInstanceOf[Type[A]])
      Environment.reportErrorAndAbort(
        s"$macroName: type parameter was inferred as ${Type[A].prettyPrint}, which is likely unintended."
      )

    Log
      .namedScope(s"Deriving Eq[${Type[A].prettyPrint}] at: ${Environment.currentPosition.prettyPrint}") {
        MIO.scoped { runSafe =>
          val fromCtx: EqCtx[A] => Expr[Boolean] = (ctx: EqCtx[A]) =>
            runSafe {
              for {
                _ <- Environment.loadStandardExtensions().toMIO(allowFailures = false)
                result <- deriveEqRecursively[A](using ctx)
                cache <- ctx.cache.get
              } yield cache.toValDefs.use(_ => result)
            }
          provideCtxAndAdapt(fromCtx)
        }
      }
      .flatTap(result => Log.info(s"Derived final result: ${result.prettyPrint}"))
      .runToExprOrFail(
        macroName,
        infoRendering = if (shouldWeLogEqDerivation) RenderFrom(Log.Level.Info) else DontRender,
        errorRendering = if (shouldWeLogEqDerivation) RenderFrom(Log.Level.Info) else DontRender
      ) { (errorLogs, errors) =>
        val errorsRendered = errors.map(e => "  - " + e.getMessage).mkString("\n")
        val hint =
          "Enable debug logging with: import hearth.kindlings.catsderivation.debug.logDerivationForCatsDerivation"
        if (errorLogs.nonEmpty)
          s"Macro derivation failed:\n$errorsRendered\nlogs:\n$errorLogs\n$hint"
        else
          s"Macro derivation failed:\n$errorsRendered\n$hint"
      }
  }

  // Context for Eq derivation - carries two values to compare

  final case class EqCtx[A](
      tpe: Type[A],
      x: Expr[A],
      y: Expr[A],
      cache: MLocal[ValDefsCache],
      derivedType: Option[??]
  ) {
    def nest[B: Type](newX: Expr[B], newY: Expr[B]): EqCtx[B] = EqCtx(Type[B], newX, newY, cache, derivedType)
  }
  object EqCtx {
    def from[A: Type](x: Expr[A], y: Expr[A], derivedType: Option[??]): EqCtx[A] =
      EqCtx(Type[A], x, y, ValDefsCache.mlocal, derivedType)
  }

  def eqctx[A](implicit A: EqCtx[A]): EqCtx[A] = A
  implicit def eqCtxType[A: EqCtx]: Type[A] = eqctx.tpe

  abstract class EqDerivationRule(val name: String) extends Rule {
    def apply[A: EqCtx]: MIO[Rule.Applicability[Expr[Boolean]]]
  }

  // Recursive derivation

  def deriveEqRecursively[A: EqCtx]: MIO[Expr[Boolean]] =
    Log.namedScope(s"Deriving Eq for ${Type[A].prettyPrint}") {
      Rules(
        EqUseCachedRule,
        EqUseImplicitRule,
        EqValueTypeRule,
        EqOptionRule,
        EqSingletonRule,
        EqCaseClassRule,
        EqEnumRule
      )(_[A]).flatMap {
        case Right(result) =>
          Log.info(s"Derived Eq for ${Type[A].prettyPrint}") >> MIO.pure(result)
        case Left(reasons) =>
          val reasonsStrings = reasons.toListMap
            .removed(EqUseCachedRule)
            .view
            .map { case (rule, reasons) =>
              if (reasons.isEmpty) s"The rule ${rule.name} was not applicable"
              else s" - ${rule.name}: ${reasons.mkString(", ")}"
            }
            .toList
          val err = EqDerivationError.UnsupportedType(Type[A].prettyPrint, reasonsStrings)
          Log.error(err.message) >> MIO.fail(err)
      }
    }

  // Types

  protected object EqTypes {
    def Eq: Type.Ctor1[cats.kernel.Eq] = Type.Ctor1.of[cats.kernel.Eq]
    val Boolean: Type[Boolean] = Type.of[Boolean]
    val Int: Type[Int] = Type.of[Int]
    val String: Type[String] = Type.of[String]
    val LogDerivation: Type[hearth.kindlings.catsderivation.LogDerivation] =
      Type.of[hearth.kindlings.catsderivation.LogDerivation]
  }

  def shouldWeLogEqDerivation: Boolean = {
    implicit val LogDerivationType: Type[LogDerivation] = EqTypes.LogDerivation
    def logDerivationImported = Expr.summonImplicit[LogDerivation].isDefined
    def logDerivationSetGlobally = (for {
      data <- Environment.typedSettings.toOption
      catsDerivation <- data.get("catsDerivation")
      shouldLog <- catsDerivation.get("logDerivation").flatMap(_.asBoolean)
    } yield shouldLog).getOrElse(false)
    logDerivationImported || logDerivationSetGlobally
  }
}

sealed private[compiletime] trait EqDerivationError extends util.control.NoStackTrace with Product with Serializable {
  def message: String
  override def getMessage(): String = message
}
private[compiletime] object EqDerivationError {
  final case class UnsupportedType(tpeName: String, reasons: List[String]) extends EqDerivationError {
    override def message: String =
      s"The type $tpeName was not handled by any Eq derivation rule:\n${reasons.mkString("\n")}"
  }
  final case class NoChildrenInSealedTrait(tpeName: String) extends EqDerivationError {
    override def message: String = s"The type $tpeName does not have any children!"
  }
}
