package hearth.kindlings.catsderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

trait ShowEnumRuleImpl {
  this: ShowMacrosImpl & MacroCommons & StdExtensions =>

  object ShowEnumRule extends ShowDerivationRule("Show as enum") {

    def apply[A: ShowCtx]: MIO[Rule.Applicability[Expr[String]]] =
      Log.info(s"Checking enum for Show[${Type[A].prettyPrint}]") >> {
        Enum.parse[A].toEither match {
          case Right(enumm) =>
            implicit val StringType: Type[String] = ShowTypes.String
            val defBuilder = ValDefBuilder.ofDef1[A, String](s"show_${Type[A].shortName}")
            for {
              _ <- sctx.cache.forwardDeclare("cached-show-method", defBuilder)
              _ <- MIO.scoped { runSafe =>
                runSafe(sctx.cache.buildCachedWith("cached-show-method", defBuilder) { case (_, value) =>
                  runSafe(deriveEnumShow[A](enumm, value))
                })
              }
              result <- ShowUseCachedRule[A]
            } yield result
          case Left(reason) =>
            MIO.pure(Rule.yielded(reason.toString))
        }
      }

    private def deriveEnumShow[A: ShowCtx](
        enumm: Enum[A],
        value: Expr[A]
    ): MIO[Expr[String]] = {
      implicit val StringType: Type[String] = ShowTypes.String
      enumm
        .matchOn[MIO, String](value) { matched =>
          import matched.{value as caseValue, Underlying as EnumCase}
          Log.namedScope(s"Deriving Show for enum case ${EnumCase.prettyPrint}") {
            deriveShowRecursively[EnumCase](using sctx.nest(caseValue))
          }
        }
        .flatMap {
          case Some(result) => MIO.pure(result)
          case None         =>
            val err = ShowDerivationError.NoChildrenInSealedTrait(Type[A].prettyPrint)
            Log.error(err.message) >> MIO.fail(err)
        }
    }
  }
}
