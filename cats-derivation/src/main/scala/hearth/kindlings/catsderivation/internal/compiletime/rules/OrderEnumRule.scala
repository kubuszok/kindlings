package hearth.kindlings.catsderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*
import hearth.kindlings.derivation.compiletime.StableChildrenOrder

trait OrderEnumRuleImpl {
  this: OrderMacrosImpl & MacroCommons & StdExtensions =>

  object OrderEnumRule extends OrderDerivationRule("Order as enum") {
    def apply[A: OrderCtx]: MIO[Rule.Applicability[Expr[Int]]] =
      parseEnum[A].toEither match {
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
          val ordinalOf = enumOrdinals[A](enumm)
          enumm
            .matchOn[MIO, Int](x) { matchedX =>
              import matchedX.{value as caseX, Underlying as EnumCase}
              val currentOrdinal = ordinalOf(Type[EnumCase].as_??)
              SingletonValue.unapply(Type[EnumCase]) match {
                case Some(sv) =>
                  // Singleton (parameterless enum case / case object): use reference equality
                  // to avoid unchecked type test warning on erased singleton types in Scala 3.
                  // Two equal singletons always compare as 0.
                  val singletonExpr = sv.singletonExpr
                  MIO.pure(Expr.quote {
                    if (Expr.splice(y).asInstanceOf[AnyRef] eq Expr.splice(singletonExpr).asInstanceOf[AnyRef]) 0
                    else java.lang.Integer.compare(Expr.splice(Expr(currentOrdinal)), Expr.splice(ordinalOfY))
                  })
                case None =>
                  val caseY = Expr.quote(Expr.splice(y).asInstanceOf[EnumCase])
                  val isInstance = Expr.quote(Expr.splice(y).isInstanceOf[EnumCase])
                  deriveOrderRecursively[EnumCase](using octx.nest(caseX, caseY)).map { innerCmp =>
                    Expr.quote {
                      if (Expr.splice(isInstance)) Expr.splice(innerCmp)
                      else java.lang.Integer.compare(Expr.splice(Expr(currentOrdinal)), Expr.splice(ordinalOfY))
                    }
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

    /** The ordinal of each subtype: its position in the build-independent subtype order (declaration order on Scala 3,
      * like kittens; by name on Scala 2, where kittens has no Order for several subtypes), not the order of the match
      * cases.
      */
    private def enumOrdinals[A](enumm: Enum[A]): ?? => Int = {
      val children = StableChildrenOrder.directChildren(OrderEnumRuleImpl.this)(enumm).map(_._2.Underlying.as_??)
      (tpe: ??) => {
        import tpe.Underlying as C
        val index = children.indexWhere { child =>
          import child.Underlying as D
          Type[D] =:= Type[C]
        }
        if (index >= 0) index else children.indexWhere(child => Type[C] <:< child.Underlying)
      }
    }

    private def deriveEnumOrdinal[A: OrderCtx](
        enumm: Enum[A],
        a: Expr[A]
    ): MIO[Expr[Int]] = {
      implicit val IntType: Type[Int] = OrderTypes.Int
      val ordinalOf = enumOrdinals[A](enumm)
      enumm
        .matchOn[MIO, Int](a) { matched =>
          import matched.Underlying as EnumCase
          MIO.pure(Expr(ordinalOf(Type[EnumCase].as_??)))
        }
        .flatMap {
          case Some(result) => MIO.pure(result)
          case None         => MIO.fail(OrderDerivationError.NoChildrenInSealedTrait(Type[A].prettyPrint))
        }
    }
  }
}
