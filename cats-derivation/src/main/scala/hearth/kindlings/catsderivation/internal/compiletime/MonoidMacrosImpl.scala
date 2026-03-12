package hearth.kindlings.catsderivation.internal.compiletime

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

/** Monoid derivation: extends Semigroup with empty (constructed from field Monoid.empty values). */
trait MonoidMacrosImpl
    extends SemigroupMacrosImpl
    with rules.MonoidUseCachedRuleImpl
    with rules.MonoidUseImplicitRuleImpl
    with rules.MonoidBuiltInRuleImpl
    with rules.MonoidCaseClassRuleImpl { this: MacroCommons & StdExtensions =>

  final case class MonoidDerivationResult[A](empty: Expr[A], combine: (Expr[A], Expr[A]) => MIO[Expr[A]])

  // Context

  final case class MonoidCtx[A](
      tpe: Type[A],
      cache: MLocal[ValDefsCache],
      derivedType: Option[??]
  )
  object MonoidCtx {
    def from[A: Type](derivedType: Option[??]): MonoidCtx[A] =
      MonoidCtx(Type[A], ValDefsCache.mlocal, derivedType)
  }

  def moidctx[A](implicit A: MonoidCtx[A]): MonoidCtx[A] = A
  implicit def monoidCtxType[A: MonoidCtx]: Type[A] = moidctx.tpe

  abstract class MonoidDerivationRule(val name: String) extends Rule {
    def apply[A: MonoidCtx]: MIO[Rule.Applicability[MonoidDerivationResult[A]]]
  }

  // Recursive derivation

  def deriveMonoidRecursively[A: MonoidCtx]: MIO[MonoidDerivationResult[A]] =
    Log.namedScope(s"Deriving Monoid for ${Type[A].prettyPrint}") {
      Rules(
        MonoidUseCachedRule,
        MonoidUseImplicitRule,
        MonoidBuiltInRule,
        MonoidCaseClassRule
      )(_[A]).flatMap {
        case Right(result) =>
          Log.info(s"Derived Monoid for ${Type[A].prettyPrint}") >> MIO.pure(result)
        case Left(reasons) =>
          val reasonsStrings = reasons.toListMap
            .removed(MonoidUseCachedRule)
            .view
            .map { case (rule, reasons) =>
              if (reasons.isEmpty) s"The rule ${rule.name} was not applicable"
              else s" - ${rule.name}: ${reasons.mkString(", ")}"
            }
            .toList
          val err = MonoidDerivationError.UnsupportedType(Type[A].prettyPrint, reasonsStrings)
          Log.error(err.message) >> MIO.fail(err)
      }
    }

  @scala.annotation.nowarn("msg=is never used")
  def deriveMonoid[A: Type]: Expr[cats.kernel.Monoid[A]] = {
    val macroName = "Monoid.derived"
    implicit val MonoidA: Type[cats.kernel.Monoid[A]] = MonoidTypes.Monoid[A]

    deriveMonoidEntrypoint[A, cats.kernel.Monoid[A]](macroName) { (doEmpty, doCombine) =>
      Expr.quote {
        new cats.kernel.Monoid[A] {
          def empty: A = Expr.splice(doEmpty)
          def combine(x: A, y: A): A = {
            val _ = x
            val _ = y
            Expr.splice(doCombine(Expr.quote(x), Expr.quote(y)))
          }
        }
      }
    }
  }

  protected def deriveMonoidEntrypoint[A: Type, Out: Type](macroName: String)(
      adapt: (Expr[A], (Expr[A], Expr[A]) => Expr[A]) => Expr[Out]
  ): Expr[Out] = {
    if (Type[A] =:= Type.of[Nothing].asInstanceOf[Type[A]] || Type[A] =:= Type.of[Any].asInstanceOf[Type[A]])
      Environment.reportErrorAndAbort(
        s"$macroName: type parameter was inferred as ${Type[A].prettyPrint}, which is likely unintended."
      )

    Log
      .namedScope(s"Deriving Monoid[${Type[A].prettyPrint}] at: ${Environment.currentPosition.prettyPrint}") {
        MIO.scoped { runSafe =>
          runSafe {
            val ctx = MonoidCtx.from[A](Some(Type[A].as_??))
            for {
              _ <- Environment.loadStandardExtensions().toMIO(allowFailures = false)
              result <- deriveMonoidRecursively[A](using ctx)
              cache <- ctx.cache.get
            } yield {
              val doEmpty = cache.toValDefs.use(_ => result.empty)
              // combine creates its own fresh SemigroupCtx with its own cache (via the rules),
              // so we just need to runSafe the MIO
              val doCombine: (Expr[A], Expr[A]) => Expr[A] = (xExpr, yExpr) => runSafe(result.combine(xExpr, yExpr))
              adapt(doEmpty, doCombine)
            }
          }
        }
      }
      .flatTap(result => Log.info(s"Derived final result: ${result.prettyPrint}"))
      .runToExprOrFail(
        macroName,
        infoRendering = if (shouldWeLogSemigroupDerivation) RenderFrom(Log.Level.Info) else DontRender,
        errorRendering = if (shouldWeLogSemigroupDerivation) RenderFrom(Log.Level.Info) else DontRender
      ) { (errorLogs, errors) =>
        val errorsRendered = errors.map(e => "  - " + e.getMessage).mkString("\n")
        val hint =
          "Enable debug logging with: import hearth.kindlings.catsderivation.debug.logDerivationForCatsDerivation"
        if (errorLogs.nonEmpty) s"Macro derivation failed:\n$errorsRendered\nlogs:\n$errorLogs\n$hint"
        else s"Macro derivation failed:\n$errorsRendered\n$hint"
      }
  }

  protected object MonoidTypes {
    def Monoid: Type.Ctor1[cats.kernel.Monoid] = Type.Ctor1.of[cats.kernel.Monoid]
  }
}

sealed private[compiletime] trait MonoidDerivationError
    extends util.control.NoStackTrace
    with Product
    with Serializable {
  def message: String
  override def getMessage(): String = message
}
private[compiletime] object MonoidDerivationError {
  final case class UnsupportedType(tpeName: String, reasons: List[String]) extends MonoidDerivationError {
    override def message: String =
      s"The type $tpeName was not handled by any Monoid derivation rule:\n${reasons.mkString("\n")}"
  }
}
