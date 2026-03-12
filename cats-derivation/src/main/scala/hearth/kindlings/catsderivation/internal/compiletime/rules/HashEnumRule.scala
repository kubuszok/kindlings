package hearth.kindlings.catsderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

trait HashEnumRuleImpl {
  this: HashMacrosImpl & MacroCommons & StdExtensions =>

  object HashEnumRule extends HashDerivationRule("Hash as enum") {

    def apply[A: HashCtx]: MIO[Rule.Applicability[Expr[Int]]] =
      Enum.parse[A].toEither match {
        case Right(enumm) =>
          implicit val IntType: Type[Int] = HashTypes.Int
          val defBuilder = ValDefBuilder.ofDef1[A, Int](s"hash_${Type[A].shortName}")
          for {
            _ <- hctx.cache.forwardDeclare("cached-hash-method", defBuilder)
            _ <- MIO.scoped { runSafe =>
              runSafe(hctx.cache.buildCachedWith("cached-hash-method", defBuilder) { case (_, value) =>
                runSafe(deriveEnumHash[A](enumm, value))
              })
            }
            result <- HashUseCachedRule[A]
          } yield result
        case Left(reason) =>
          MIO.pure(Rule.yielded(reason.toString))
      }

    @scala.annotation.nowarn("msg=is unchecked")
    private def deriveEnumHash[A: HashCtx](
        enumm: Enum[A],
        value: Expr[A]
    ): MIO[Expr[Int]] = {
      implicit val IntType: Type[Int] = HashTypes.Int
      enumm
        .matchOn[MIO, Int](value) { matchedValue =>
          import matchedValue.{value as caseValue, Underlying as EnumCase}
          deriveHashRecursively[EnumCase](using hctx.nest(caseValue))
        }
        .flatMap {
          case Some(result) => MIO.pure(result)
          case None         => MIO.pure(Expr.quote(Expr.splice(value).hashCode()))
        }
    }
  }
}
