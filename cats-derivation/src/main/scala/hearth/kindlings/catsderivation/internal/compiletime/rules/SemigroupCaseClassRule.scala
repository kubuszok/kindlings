package hearth.kindlings.catsderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.data.NonEmptyList
import hearth.fp.effect.*
import hearth.fp.syntax.*
import hearth.std.*

trait SemigroupCaseClassRuleImpl {
  this: SemigroupMacrosImpl & MacroCommons & StdExtensions =>

  object SemigroupCaseClassRule extends SemigroupDerivationRule("Semigroup as case class") {
    def apply[A: SemigroupCtx]: MIO[Rule.Applicability[Expr[A]]] =
      CaseClass.parse[A].toEither match {
        case Right(caseClass) =>
          val defBuilder = ValDefBuilder.ofDef2[A, A, A](s"combine_${Type[A].shortName}")
          for {
            _ <- sgctx.cache.forwardDeclare("cached-semigroup-method", defBuilder)
            _ <- MIO.scoped { runSafe =>
              runSafe(sgctx.cache.buildCachedWith("cached-semigroup-method", defBuilder) { case (_, (x, y)) =>
                runSafe(deriveCaseClassSemigroup[A](caseClass, x, y))
              })
            }
            result <- SemigroupUseCachedRule[A]
          } yield result
        case Left(reason) =>
          MIO.pure(Rule.yielded(reason.toString))
      }

    private def deriveCaseClassSemigroup[A: SemigroupCtx](
        caseClass: CaseClass[A],
        x: Expr[A],
        y: Expr[A]
    ): MIO[Expr[A]] = {
      val fieldsX = caseClass.caseFieldValuesAt(x).toList
      val fieldsY = caseClass.caseFieldValuesAt(y).toList

      NonEmptyList.fromList(fieldsX.zip(fieldsY)) match {
        case Some(fieldPairs) =>
          fieldPairs
            .traverse { case ((fieldName, fieldValueX), (_, fieldValueY)) =>
              import fieldValueX.Underlying as Field
              val fx = fieldValueX.value.asInstanceOf[Expr[Field]]
              val fy = fieldValueY.value.asInstanceOf[Expr[Field]]
              Log.namedScope(s"Deriving Semigroup for field $fieldName: ${Field.prettyPrint}") {
                deriveSemigroupRecursively[Field](using sgctx.nest(fx, fy)).map(r => (fieldName, r.as_??))
              }
            }
            .flatMap { combinedFields =>
              val fieldMap: Map[String, Expr_??] = combinedFields.toList.toMap
              caseClass.primaryConstructor(fieldMap) match {
                case Right(constructExpr) => MIO.pure(constructExpr)
                case Left(error)          =>
                  MIO.fail(new RuntimeException(s"Cannot construct ${Type[A].prettyPrint}: $error"))
              }
            }
        case None =>
          // No fields — just construct an empty instance
          caseClass.primaryConstructor(Map.empty) match {
            case Right(constructExpr) => MIO.pure(constructExpr)
            case Left(error)          =>
              MIO.fail(new RuntimeException(s"Cannot construct ${Type[A].prettyPrint}: $error"))
          }
      }
    }
  }
}
