package hearth.kindlings.catsderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.data.NonEmptyList
import hearth.fp.effect.*
import hearth.fp.syntax.*
import hearth.std.*

trait OrderCaseClassRuleImpl {
  this: OrderMacrosImpl & MacroCommons & StdExtensions =>

  object OrderCaseClassRule extends OrderDerivationRule("Order as case class") {
    def apply[A: OrderCtx]: MIO[Rule.Applicability[Expr[Int]]] =
      CaseClass.parse[A].toEither match {
        case Right(caseClass) =>
          implicit val IntType: Type[Int] = OrderTypes.Int
          val defBuilder = ValDefBuilder.ofDef2[A, A, Int](s"compare_${Type[A].shortName}")
          for {
            _ <- octx.cache.forwardDeclare("cached-order-method", defBuilder)
            _ <- MIO.scoped { runSafe =>
              runSafe(octx.cache.buildCachedWith("cached-order-method", defBuilder) { case (_, (x, y)) =>
                runSafe(deriveCaseClassOrder[A](caseClass, x, y))
              })
            }
            result <- OrderUseCachedRule[A]
          } yield result
        case Left(reason) =>
          MIO.pure(Rule.yielded(reason.toString))
      }

    private def deriveCaseClassOrder[A: OrderCtx](
        caseClass: CaseClass[A],
        x: Expr[A],
        y: Expr[A]
    ): MIO[Expr[Int]] = {
      val fieldsX = caseClass.caseFieldValuesAt(x).toList
      val fieldsY = caseClass.caseFieldValuesAt(y).toList

      NonEmptyList.fromList(fieldsX.zip(fieldsY)) match {
        case Some(fieldPairs) =>
          fieldPairs
            .traverse { case ((fieldName, fieldValueX), (_, fieldValueY)) =>
              import fieldValueX.Underlying as Field
              val fx = fieldValueX.value.asInstanceOf[Expr[Field]]
              val fy = fieldValueY.value.asInstanceOf[Expr[Field]]
              deriveOrderRecursively[Field](using octx.nest(fx, fy)).map(r => (fieldName, r))
            }
            .map { results =>
              results.toList.reverse.foldLeft(Expr(0): Expr[Int]) { case (rest, (_, cmp)) =>
                Expr.quote {
                  val c = Expr.splice(cmp)
                  if (c != 0) c else Expr.splice(rest)
                }
              }
            }
        case None =>
          MIO.pure(Expr(0))
      }
    }
  }
}
