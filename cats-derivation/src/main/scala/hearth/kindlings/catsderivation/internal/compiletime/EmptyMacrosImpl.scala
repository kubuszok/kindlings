package hearth.kindlings.catsderivation.internal.compiletime

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

import hearth.kindlings.catsderivation.LogDerivation

/** Empty derivation: product (all fields: Empty) and coproduct (exactly one variant: Empty). */
trait EmptyMacrosImpl
    extends rules.EmptyUseCachedRuleImpl
    with rules.EmptyUseImplicitRuleImpl
    with rules.EmptyBuiltInRuleImpl
    with rules.EmptyCaseClassRuleImpl
    with rules.EmptyEnumRuleImpl { this: MacroCommons & StdExtensions =>

  @scala.annotation.nowarn("msg=is never used")
  def deriveEmpty[A: Type]: Expr[alleycats.Empty[A]] = {
    val macroName = "Empty.derived"
    implicit val EmptyA: Type[alleycats.Empty[A]] = EmptyTypes.Empty[A]
    val selfType: Option[??] = Some(Type[A].as_??)

    deriveEmptyEntrypoint[A, alleycats.Empty[A]](macroName, selfType) { emptyExpr =>
      Expr.quote {
        new alleycats.Empty[A] {
          def empty: A = Expr.splice(emptyExpr)
        }
      }
    }
  }

  private def deriveEmptyEntrypoint[A: Type, Out: Type](macroName: String, selfType: Option[??])(
      adapt: Expr[A] => Expr[Out]
  ): Expr[Out] = {
    if (Type[A] =:= Type.of[Nothing].asInstanceOf[Type[A]] || Type[A] =:= Type.of[Any].asInstanceOf[Type[A]])
      Environment.reportErrorAndAbort(
        s"$macroName: type parameter was inferred as ${Type[A].prettyPrint}, which is likely unintended."
      )

    Log
      .namedScope(s"Deriving Empty[${Type[A].prettyPrint}] at: ${Environment.currentPosition.prettyPrint}") {
        MIO.scoped { runSafe =>
          runSafe {
            val ctx = EmptyCtx.from[A](selfType)
            for {
              _ <- Environment.loadStandardExtensions().toMIO(allowFailures = false)
              result <- deriveEmptyRecursively[A](using ctx)
              cache <- ctx.cache.get
            } yield adapt(cache.toValDefs.use(_ => result))
          }
        }
      }
      .flatTap(result => Log.info(s"Derived final result: ${result.prettyPrint}"))
      .runToExprOrFail(
        macroName,
        infoRendering = if (shouldWeLogEmptyDerivation) RenderFrom(Log.Level.Info) else DontRender,
        errorRendering = if (shouldWeLogEmptyDerivation) RenderFrom(Log.Level.Info) else DontRender
      ) { (errorLogs, errors) =>
        val errorsRendered = errors.map(e => "  - " + e.getMessage).mkString("\n")
        val hint =
          "Enable debug logging with: import hearth.kindlings.catsderivation.debug.logDerivationForCatsDerivation"
        if (errorLogs.nonEmpty) s"Macro derivation failed:\n$errorsRendered\nlogs:\n$errorLogs\n$hint"
        else s"Macro derivation failed:\n$errorsRendered\n$hint"
      }
  }

  // Context for Empty derivation — no value parameter (produces, doesn't consume)

  final case class EmptyCtx[A](
      tpe: Type[A],
      cache: MLocal[ValDefsCache],
      derivedType: Option[??]
  ) {
    def nestType[B: Type]: EmptyCtx[B] = EmptyCtx(Type[B], cache, derivedType)
  }
  object EmptyCtx {
    def from[A: Type](derivedType: Option[??]): EmptyCtx[A] =
      EmptyCtx(Type[A], ValDefsCache.mlocal, derivedType)
  }

  def ectx[A](implicit A: EmptyCtx[A]): EmptyCtx[A] = A
  implicit def emptyCtxType[A: EmptyCtx]: Type[A] = ectx.tpe

  abstract class EmptyDerivationRule(val name: String) extends Rule {
    def apply[A: EmptyCtx]: MIO[Rule.Applicability[Expr[A]]]
  }

  // Recursive derivation

  def deriveEmptyRecursively[A: EmptyCtx]: MIO[Expr[A]] =
    Log.namedScope(s"Deriving Empty for ${Type[A].prettyPrint}") {
      Rules(
        EmptyUseCachedRule,
        EmptyUseImplicitRule,
        EmptyBuiltInRule,
        EmptyCaseClassRule,
        EmptyEnumRule
      )(_[A]).flatMap {
        case Right(result) =>
          Log.info(s"Derived Empty for ${Type[A].prettyPrint}") >> MIO.pure(result)
        case Left(reasons) =>
          val reasonsStrings = reasons.toListMap
            .removed(EmptyUseCachedRule)
            .view
            .map { case (rule, reasons) =>
              if (reasons.isEmpty) s"The rule ${rule.name} was not applicable"
              else s" - ${rule.name}: ${reasons.mkString(", ")}"
            }
            .toList
          val err = EmptyDerivationError.UnsupportedType(Type[A].prettyPrint, reasonsStrings)
          Log.error(err.message) >> MIO.fail(err)
      }
    }

  // Types

  protected object EmptyTypes {
    def Empty: Type.Ctor1[alleycats.Empty] = Type.Ctor1.of[alleycats.Empty]
    val Boolean: Type[Boolean] = Type.of[Boolean]
    val Byte: Type[Byte] = Type.of[Byte]
    val Short: Type[Short] = Type.of[Short]
    val Int: Type[Int] = Type.of[Int]
    val Long: Type[Long] = Type.of[Long]
    val Float: Type[Float] = Type.of[Float]
    val Double: Type[Double] = Type.of[Double]
    val Char: Type[Char] = Type.of[Char]
    val String: Type[String] = Type.of[String]
    val LogDerivation: Type[hearth.kindlings.catsderivation.LogDerivation] =
      Type.of[hearth.kindlings.catsderivation.LogDerivation]
  }

  def shouldWeLogEmptyDerivation: Boolean = {
    implicit val LogDerivationType: Type[LogDerivation] = EmptyTypes.LogDerivation
    def logDerivationImported = Expr.summonImplicit[LogDerivation].isDefined
    def logDerivationSetGlobally = (for {
      data <- Environment.typedSettings.toOption
      catsDerivation <- data.get("catsDerivation")
      shouldLog <- catsDerivation.get("logDerivation").flatMap(_.asBoolean)
    } yield shouldLog).getOrElse(false)
    logDerivationImported || logDerivationSetGlobally
  }
}

sealed private[compiletime] trait EmptyDerivationError
    extends util.control.NoStackTrace
    with Product
    with Serializable {
  def message: String
  override def getMessage(): String = message
}
private[compiletime] object EmptyDerivationError {
  final case class UnsupportedType(tpeName: String, reasons: List[String]) extends EmptyDerivationError {
    override def message: String =
      s"The type $tpeName was not handled by any Empty derivation rule:\n${reasons.mkString("\n")}"
  }
  final case class MultipleEmptyVariants(tpeName: String) extends EmptyDerivationError {
    override def message: String =
      s"Cannot derive Empty for $tpeName: multiple variants have Empty instances (need exactly one)"
  }
}
