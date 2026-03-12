package hearth.kindlings.catsderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

trait OrderEnumRuleImpl {
  this: OrderMacrosImpl & MacroCommons & StdExtensions =>

  object OrderEnumRule extends OrderDerivationRule("Order as enum") {
    def apply[A: OrderCtx]: MIO[Rule.Applicability[Expr[Int]]] =
      Enum.parse[A].toEither match {
        case Right(enumm) =>
          implicit val IntType: Type[Int] = OrderTypes.Int
          val defBuilder = ValDefBuilder.ofDef2[A, A, Int](s"compare_${Type[A].shortName}")
          for {
            _ <- octx.cache.forwardDeclare("cached-order-method", defBuilder)
            _ <- MIO.scoped { runSafe =>
              runSafe(octx.cache.buildCachedWith("cached-order-method", defBuilder) { case (_, (x, y)) =>
                runSafe(deriveEnumOrder[A](enumm, x, y))
              })
            }
            result <- OrderUseCachedRule[A]
          } yield result
        case Left(reason) =>
          MIO.pure(Rule.yielded(reason.toString))
      }

    @scala.annotation.nowarn("msg=is unchecked")
    private def deriveEnumOrder[A: OrderCtx](
        enumm: Enum[A],
        x: Expr[A],
        y: Expr[A]
    ): MIO[Expr[Int]] = {
      implicit val IntType: Type[Int] = OrderTypes.Int
      enumm
        .matchOn[MIO, Int](x) { matchedX =>
          import matchedX.{value as caseX, Underlying as EnumCase}
          val caseY = Expr.quote(Expr.splice(y).asInstanceOf[EnumCase])
          val isInstance = Expr.quote(Expr.splice(y).isInstanceOf[EnumCase])
          deriveOrderRecursively[EnumCase](using octx.nest(caseX, caseY)).map { innerCmp =>
            Expr.quote {
              if (Expr.splice(isInstance)) Expr.splice(innerCmp)
              else {
                java.lang.Integer.compare(Expr.splice(x).hashCode(), Expr.splice(y).hashCode())
              }
            }
          }
        }
        .flatMap {
          case Some(result) => MIO.pure(result)
          case None         => MIO.fail(OrderDerivationError.NoChildrenInSealedTrait(Type[A].prettyPrint))
        }
    }
  }
}
