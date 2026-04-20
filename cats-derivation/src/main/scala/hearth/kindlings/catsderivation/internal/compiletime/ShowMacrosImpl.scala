package hearth.kindlings.catsderivation.internal.compiletime

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

import hearth.kindlings.catsderivation.LogDerivation

trait ShowMacrosImpl
    extends rules.ShowUseCachedRuleImpl
    with rules.ShowUseImplicitRuleImpl
    with rules.ShowBuiltInRuleImpl
    with rules.ShowValueTypeRuleImpl
    with rules.ShowOptionRuleImpl
    with rules.ShowMapRuleImpl
    with rules.ShowCollectionRuleImpl
    with rules.ShowSingletonRuleImpl
    with rules.ShowCaseClassRuleImpl
    with rules.ShowEnumRuleImpl
    with CatsDerivationTimeout { this: MacroCommons & StdExtensions =>

  // Entrypoint

  @scala.annotation.nowarn("msg=is never used")
  def deriveShow[A: Type]: Expr[cats.Show[A]] = {
    val macroName = "Show.derived"
    implicit val ShowA: Type[cats.Show[A]] = ShowTypes.Show[A]
    implicit val StringType: Type[String] = ShowTypes.String
    val selfType: Option[??] = Some(Type[A].as_??)

    deriveShowFromCtxAndAdaptForEntrypoint[A, cats.Show[A]](macroName) { fromCtx =>
      Expr.quote {
        new cats.Show[A] {
          def show(value: A): String = {
            val _ = value
            Expr.splice {
              fromCtx(ShowCtx.from(Expr.quote(value), derivedType = selfType))
            }
          }
        }
      }
    }
  }

  // CPS-style entrypoint that handles logging, error reporting and cache

  private def deriveShowFromCtxAndAdaptForEntrypoint[A: Type, Out: Type](macroName: String)(
      provideCtxAndAdapt: (ShowCtx[A] => Expr[String]) => Expr[Out]
  ): Expr[Out] = {
    if (Type[A] =:= Type.of[Nothing].asInstanceOf[Type[A]] || Type[A] =:= Type.of[Any].asInstanceOf[Type[A]])
      Environment.reportErrorAndAbort(
        s"$macroName: type parameter was inferred as ${Type[A].prettyPrint}, which is likely unintended.\n" +
          s"Provide an explicit type parameter, e.g.: $macroName[MyType]\n" +
          "or add a type ascription to the result variable."
      )

    Log
      .namedScope(
        s"Deriving Show[${Type[A].prettyPrint}] at: ${Environment.currentPosition.prettyPrint}"
      ) {
        MIO.scoped { runSafe =>
          val fromCtx: ShowCtx[A] => Expr[String] = (ctx: ShowCtx[A]) =>
            runSafe {
              for {
                _ <- Environment.loadStandardExtensions().toMIO(allowFailures = false)
                result <- deriveShowRecursively[A](using ctx)
                cache <- ctx.cache.get
              } yield cache.toValDefs.use(_ => result)
            }
          provideCtxAndAdapt(fromCtx)
        }
      }
      .flatTap(result => Log.info(s"Derived final result: ${result.prettyPrint}"))
      .runToExprOrFail(
        macroName,
        infoRendering = if (shouldWeLogShowDerivation) RenderFrom(Log.Level.Info) else DontRender,
        errorRendering = if (shouldWeLogShowDerivation) RenderFrom(Log.Level.Info) else DontRender,
        timeout = derivationTimeout
      ) { (errorLogs, errors) =>
        val errorsRendered = errors.map(e => "  - " + e.getMessage).mkString("\n")
        val hint =
          "Enable debug logging with: import hearth.kindlings.catsderivation.debug.logDerivationForCatsDerivation"
        if (errorLogs.nonEmpty)
          s"Macro derivation failed with the following errors:\n$errorsRendered\nand the following logs:\n$errorLogs\n$hint"
        else
          s"Macro derivation failed with the following errors:\n$errorsRendered\n$hint"
      }
  }

  // Context

  final case class ShowCtx[A](
      tpe: Type[A],
      value: Expr[A],
      cache: MLocal[ValDefsCache],
      derivedType: Option[??]
  ) {
    def nest[B: Type](newValue: Expr[B]): ShowCtx[B] = ShowCtx(Type[B], newValue, cache, derivedType)
  }
  object ShowCtx {
    def from[A: Type](value: Expr[A], derivedType: Option[??]): ShowCtx[A] =
      ShowCtx(Type[A], value, ValDefsCache.mlocal, derivedType)
  }

  def sctx[A](implicit A: ShowCtx[A]): ShowCtx[A] = A
  implicit def showCtxType[A: ShowCtx]: Type[A] = sctx.tpe

  abstract class ShowDerivationRule(val name: String) extends Rule {
    def apply[A: ShowCtx]: MIO[Rule.Applicability[Expr[String]]]
  }

  // Recursive derivation

  def deriveShowRecursively[A: ShowCtx]: MIO[Expr[String]] =
    Log.namedScope(s"Deriving Show for ${Type[A].prettyPrint}") {
      Rules(
        ShowUseCachedRule,
        ShowUseImplicitRule,
        ShowBuiltInRule,
        ShowValueTypeRule,
        ShowOptionRule,
        ShowMapRule,
        ShowCollectionRule,
        ShowSingletonRule,
        ShowCaseClassRule,
        ShowEnumRule
      )(_[A]).flatMap {
        case Right(result) =>
          Log.info(s"Derived Show for ${Type[A].prettyPrint}: ${result.prettyPrint}") >> MIO.pure(result)
        case Left(reasons) =>
          val reasonsStrings = reasons.toListMap
            .removed(ShowUseCachedRule)
            .view
            .map { case (rule, reasons) =>
              if (reasons.isEmpty) s"The rule ${rule.name} was not applicable"
              else s" - The rule ${rule.name} was not applicable: ${reasons.mkString(", ")}"
            }
            .toList
          val err = ShowDerivationError.UnsupportedType(Type[A].prettyPrint, reasonsStrings)
          Log.error(err.message) >> MIO.fail(err)
      }
    }

  // Types helper

  protected object ShowTypes {
    def Show: Type.Ctor1[cats.Show] = Type.Ctor1.of[cats.Show]
    val LogDerivation: Type[LogDerivation] = Type.of[LogDerivation]
    val Boolean: Type[Boolean] = Type.of[Boolean]
    val Byte: Type[Byte] = Type.of[Byte]
    val Short: Type[Short] = Type.of[Short]
    val Int: Type[Int] = Type.of[Int]
    val Long: Type[Long] = Type.of[Long]
    val Float: Type[Float] = Type.of[Float]
    val Double: Type[Double] = Type.of[Double]
    val Char: Type[Char] = Type.of[Char]
    val String: Type[String] = Type.of[String]
  }

  def shouldWeLogShowDerivation: Boolean = {
    implicit val LogDerivationType: Type[LogDerivation] = ShowTypes.LogDerivation
    def logDerivationImported = Expr.summonImplicit[LogDerivation].isDefined

    def logDerivationSetGlobally = (for {
      data <- Environment.typedSettings.toOption
      catsDerivation <- data.get("catsDerivation")
      shouldLog <- catsDerivation.get("logDerivation").flatMap(_.asBoolean)
    } yield shouldLog).getOrElse(false)

    logDerivationImported || logDerivationSetGlobally
  }
}

sealed private[compiletime] trait ShowDerivationError extends util.control.NoStackTrace with Product with Serializable {
  def message: String
  override def getMessage(): String = message
}
private[compiletime] object ShowDerivationError {
  final case class UnsupportedType(tpeName: String, reasons: List[String]) extends ShowDerivationError {
    override def message: String =
      s"The type $tpeName was not handled by any Show derivation rule:\n${reasons.mkString("\n")}"
  }
  final case class NoChildrenInSealedTrait(tpeName: String) extends ShowDerivationError {
    override def message: String =
      s"The type $tpeName does not have any children!"
  }
}
