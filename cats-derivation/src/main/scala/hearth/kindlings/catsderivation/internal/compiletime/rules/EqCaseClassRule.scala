package hearth.kindlings.catsderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.data.NonEmptyList
import hearth.fp.effect.*
import hearth.fp.syntax.*
import hearth.std.*

trait EqCaseClassRuleImpl {
  this: EqMacrosImpl & MacroCommons & StdExtensions =>

  object EqCaseClassRule extends EqDerivationRule("Eq as case class") {
    def apply[A: EqCtx]: MIO[Rule.Applicability[Expr[Boolean]]] =
      CaseClass.parse[A].toEither match {
        case Right(caseClass) =>
          implicit val BooleanType: Type[Boolean] = EqTypes.Boolean
          val defBuilder = ValDefBuilder.ofDef2[A, A, Boolean](s"eqv_${Type[A].shortName}")
          for {
            _ <- eqctx.cache.forwardDeclare("cached-eq-method", defBuilder)
            _ <- MIO.scoped { runSafe =>
              runSafe(eqctx.cache.buildCachedWith("cached-eq-method", defBuilder) { case (_, (x, y)) =>
                runSafe(deriveCaseClassEq[A](caseClass, x, y))
              })
            }
            result <- EqUseCachedRule[A]
          } yield result
        case Left(reason) =>
          MIO.pure(Rule.yielded(reason.toString))
      }

    private def deriveCaseClassEq[A: EqCtx](
        caseClass: CaseClass[A],
        x: Expr[A],
        y: Expr[A]
    ): MIO[Expr[Boolean]] = {
      val fields = caseClass.caseFieldValuesAt(x).toList
      val fieldsY = caseClass.caseFieldValuesAt(y).toList

      NonEmptyList.fromList(fields.zip(fieldsY)) match {
        case Some(fieldPairs) =>
          fieldPairs
            .traverse { case ((fieldName, fieldValueX), (_, fieldValueY)) =>
              import fieldValueX.Underlying as Field
              val fx = fieldValueX.value.asInstanceOf[Expr[Field]]
              val fy = fieldValueY.value.asInstanceOf[Expr[Field]]
              Log.namedScope(s"Deriving Eq for field $fieldName: ${Field.prettyPrint}") {
                deriveEqRecursively[Field](using eqctx.nest(fx, fy))
              }
            }
            .map { results =>
              results.toList.reduceLeft { (acc, next) =>
                Expr.quote(Expr.splice(acc) && Expr.splice(next))
              }
            }
        case None =>
          MIO.pure(Expr(true))
      }
    }
  }
}
