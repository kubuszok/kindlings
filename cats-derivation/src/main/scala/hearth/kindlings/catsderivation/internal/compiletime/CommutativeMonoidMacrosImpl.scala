package hearth.kindlings.catsderivation.internal.compiletime

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

/** CommutativeMonoid derivation: delegates to Monoid derivation and wraps in CommutativeMonoid. */
trait CommutativeMonoidMacrosImpl extends MonoidMacrosImpl { this: MacroCommons & StdExtensions =>

  @scala.annotation.nowarn("msg=is never used")
  def deriveCommutativeMonoid[A: Type]: Expr[cats.kernel.CommutativeMonoid[A]] = {
    val macroName = "CommutativeMonoid.derived"
    implicit val CMA: Type[cats.kernel.CommutativeMonoid[A]] = CommutativeMonoidTypes.CommutativeMonoid[A]

    deriveCommutativeMonoidEntrypoint[A, cats.kernel.CommutativeMonoid[A]](macroName) { (doEmpty, doCombine) =>
      Expr.quote {
        new cats.kernel.CommutativeMonoid[A] {
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

  private def deriveCommutativeMonoidEntrypoint[A: Type, Out: Type](macroName: String)(
      adapt: (Expr[A], (Expr[A], Expr[A]) => Expr[A]) => Expr[Out]
  ): Expr[Out] = {
    if (Type[A] =:= Type.of[Nothing].asInstanceOf[Type[A]] || Type[A] =:= Type.of[Any].asInstanceOf[Type[A]])
      Environment.reportErrorAndAbort(
        s"$macroName: type parameter was inferred as ${Type[A].prettyPrint}, which is likely unintended."
      )

    Log
      .namedScope(s"Deriving CommutativeMonoid[${Type[A].prettyPrint}] at: ${Environment.currentPosition.prettyPrint}") {
        CaseClass.parse[A].toEither match {
          case Right(caseClass) =>
            deriveMonoidEmpty[A](caseClass).flatMap { emptyExpr =>
              MIO.scoped { runSafe =>
                val doCombine: (Expr[A], Expr[A]) => Expr[A] = (xExpr, yExpr) =>
                  runSafe {
                    for {
                      _ <- Environment.loadStandardExtensions().toMIO(allowFailures = false)
                      result <- deriveSemigroupCombine[A](caseClass, xExpr, yExpr)
                    } yield result
                  }

                adapt(emptyExpr, doCombine)
              }
            }
          case Left(reason) =>
            MIO.fail(
              new RuntimeException(
                s"$macroName: Cannot derive CommutativeMonoid for ${Type[A].prettyPrint}: $reason. " +
                  "CommutativeMonoid can only be derived for case classes."
              )
            )
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

  protected object CommutativeMonoidTypes {
    def CommutativeMonoid: Type.Ctor1[cats.kernel.CommutativeMonoid] =
      Type.Ctor1.of[cats.kernel.CommutativeMonoid]
  }
}
