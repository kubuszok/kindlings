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

      val ordinalDefBuilder = ValDefBuilder.ofDef1[A, Int](s"ordinal_${Type[A].shortName}")
      for {
        _ <- octx.cache.forwardDeclare("cached-ordinal-method", ordinalDefBuilder)
        _ <- MIO.scoped { runSafe =>
          runSafe(octx.cache.buildCachedWith("cached-ordinal-method", ordinalDefBuilder) { case (_, a) =>
            runSafe(deriveEnumOrdinal[A](enumm, a))
          })
        }
        ordinalFn <- octx.cache.get1Ary[A, Int]("cached-ordinal-method").flatMap {
          case Some(fn) => MIO.pure(fn)
          case None     => MIO.fail(OrderDerivationError.NoChildrenInSealedTrait(Type[A].prettyPrint))
        }
        ordinalOfY = ordinalFn(y)
        result <- {
          var childOrdinal = 0
          enumm
            .matchOn[MIO, Int](x) { matchedX =>
              import matchedX.{value as caseX, Underlying as EnumCase}
              val currentOrdinal = childOrdinal
              childOrdinal += 1
              val caseY = Expr.quote(Expr.splice(y).asInstanceOf[EnumCase])
              val isInstance = Expr.quote(Expr.splice(y).isInstanceOf[EnumCase])
              deriveOrderRecursively[EnumCase](using octx.nest(caseX, caseY)).map { innerCmp =>
                Expr.quote {
                  if (Expr.splice(isInstance)) Expr.splice(innerCmp)
                  else java.lang.Integer.compare(Expr.splice(Expr(currentOrdinal)), Expr.splice(ordinalOfY))
                }
              }
            }
            .flatMap {
              case Some(result) => MIO.pure(result)
              case None         => MIO.fail(OrderDerivationError.NoChildrenInSealedTrait(Type[A].prettyPrint))
            }
        }
      } yield result
    }

    private def deriveEnumOrdinal[A: OrderCtx](
        enumm: Enum[A],
        a: Expr[A]
    ): MIO[Expr[Int]] = {
      implicit val IntType: Type[Int] = OrderTypes.Int
      var childOrdinal = 0
      enumm
        .matchOn[MIO, Int](a) { _ =>
          val ordinal = childOrdinal
          childOrdinal += 1
          MIO.pure(Expr(ordinal))
        }
        .flatMap {
          case Some(result) => MIO.pure(result)
          case None         => MIO.fail(OrderDerivationError.NoChildrenInSealedTrait(Type[A].prettyPrint))
        }
    }
  }
}
