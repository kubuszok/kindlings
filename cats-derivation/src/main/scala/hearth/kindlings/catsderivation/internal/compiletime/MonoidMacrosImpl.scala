package hearth.kindlings.catsderivation.internal.compiletime

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

/** Monoid derivation: extends Semigroup with empty (constructed from field Monoid.empty values). */
trait MonoidMacrosImpl extends SemigroupMacrosImpl { this: MacroCommons & StdExtensions =>

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

  private def deriveMonoidEntrypoint[A: Type, Out: Type](macroName: String)(
      adapt: (Expr[A], (Expr[A], Expr[A]) => Expr[A]) => Expr[Out]
  ): Expr[Out] = {
    if (Type[A] =:= Type.of[Nothing].asInstanceOf[Type[A]] || Type[A] =:= Type.of[Any].asInstanceOf[Type[A]])
      Environment.reportErrorAndAbort(
        s"$macroName: type parameter was inferred as ${Type[A].prettyPrint}, which is likely unintended."
      )

    Log
      .namedScope(s"Deriving Monoid[${Type[A].prettyPrint}] at: ${Environment.currentPosition.prettyPrint}") {
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
                s"$macroName: Cannot derive Monoid for ${Type[A].prettyPrint}: $reason. " +
                  "Monoid can only be derived for case classes."
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

  protected def deriveMonoidEmpty[A: Type](
      caseClass: CaseClass[A]
  ): MIO[Expr[A]] = {
    val constructor = caseClass.primaryConstructor
    val fields = constructor.parameters.flatten.toList

    val emptyFields: List[(String, Expr_??)] = fields.map { case (fieldName, param) =>
      import param.tpe.Underlying as Field
      val monoidExpr = MonoidTypes.Monoid[Field].summonExprIgnoring().toEither match {
        case Right(m)     => m
        case Left(reason) =>
          throw new RuntimeException(
            s"No Monoid instance found for field $fieldName: ${Field.prettyPrint}: $reason"
          )
      }
      val emptyExpr: Expr[Field] = Expr.quote(Expr.splice(monoidExpr).empty)
      (fieldName, emptyExpr.as_??)
    }

    caseClass.primaryConstructor(emptyFields.toMap) match {
      case Right(constructExpr) => MIO.pure(constructExpr)
      case Left(error)          =>
        MIO.fail(new RuntimeException(s"Cannot construct empty ${Type[A].prettyPrint}: $error"))
    }
  }

  protected object MonoidTypes {
    def Monoid: Type.Ctor1[cats.kernel.Monoid] = Type.Ctor1.of[cats.kernel.Monoid]
  }
}
