package hearth.kindlings.catsderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

trait EqEnumRuleImpl {
  this: EqMacrosImpl & MacroCommons & StdExtensions =>

  object EqEnumRule extends EqDerivationRule("Eq as enum") {
    def apply[A: EqCtx]: MIO[Rule.Applicability[Expr[Boolean]]] =
      Enum.parse[A].toEither match {
        case Right(enumm) =>
          implicit val BooleanType: Type[Boolean] = EqTypes.Boolean
          val defBuilder = ValDefBuilder.ofDef2[A, A, Boolean](s"eqv_${Type[A].shortName}")
          for {
            _ <- eqctx.cache.forwardDeclare("cached-eq-method", defBuilder)
            _ <- MIO.scoped { runSafe =>
              runSafe(eqctx.cache.buildCachedWith("cached-eq-method", defBuilder) { case (_, (x, y)) =>
                runSafe(deriveEnumEq[A](enumm, x, y))
              })
            }
            result <- EqUseCachedRule[A]
          } yield result
        case Left(reason) =>
          MIO.pure(Rule.yielded(reason.toString))
      }

    @scala.annotation.nowarn("msg=is never used|is unchecked")
    private def deriveEnumEq[A: EqCtx](
        enumm: Enum[A],
        x: Expr[A],
        y: Expr[A]
    ): MIO[Expr[Boolean]] = {
      implicit val BooleanType: Type[Boolean] = EqTypes.Boolean
      enumm
        .matchOn[MIO, Boolean](x) { matchedX =>
          import matchedX.{value as caseX, Underlying as EnumCase}
          Log.namedScope(s"Deriving Eq for enum case ${EnumCase.prettyPrint}") {
            val caseY = Expr.quote(Expr.splice(y).asInstanceOf[EnumCase])
            val isInstance = Expr.quote {
              Expr.splice(y).isInstanceOf[EnumCase]
            }
            deriveEqRecursively[EnumCase](using eqctx.nest(caseX, caseY)).map { eqResult =>
              Expr.quote {
                Expr.splice(isInstance) && Expr.splice(eqResult)
              }
            }
          }
        }
        .flatMap {
          case Some(result) => MIO.pure(result)
          case None         =>
            val err = EqDerivationError.NoChildrenInSealedTrait(Type[A].prettyPrint)
            Log.error(err.message) >> MIO.fail(err)
        }
    }
  }
}
