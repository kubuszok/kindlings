package hearth.kindlings.catsderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

trait ShowPrettyEnumRuleImpl {
  this: ShowPrettyMacrosImpl & MacroCommons & StdExtensions =>

  object ShowPrettyEnumRule extends ShowDerivationRule("ShowPretty as enum") {

    def apply[A: ShowCtx]: MIO[Rule.Applicability[Expr[String]]] =
      Log.info(s"Checking enum for ShowPretty[${Type[A].prettyPrint}]") >> {
        Enum.parse[A].toEither match {
          case Right(enumm) =>
            implicit val SensitiveDataType: Type[hearth.kindlings.catsderivation.annotations.sensitiveData] =
              ShowTypes.SensitiveData
            if (hasTypeAnnotation[hearth.kindlings.catsderivation.annotations.sensitiveData, A]) {
              val reason =
                getTypeAnnotationStringArg[hearth.kindlings.catsderivation.annotations.sensitiveData, A]
              val text = reason.filter(_.nonEmpty).fold("[redacted]")(r => s"[redacted: $r]")
              MIO.pure(Rule.matched(Expr(text)))
            } else {
              implicit val StringType: Type[String] = ShowTypes.String
              val defBuilder = ValDefBuilder.ofDef1[A, String](s"showPretty_${Type[A].shortName}")
              for {
                _ <- sctx.cache.forwardDeclare("cached-show-pretty-method", defBuilder)
                _ <- MIO.scoped { runSafe =>
                  runSafe(sctx.cache.buildCachedWith("cached-show-pretty-method", defBuilder) { case (_, value) =>
                    runSafe(deriveEnumShowPretty[A](enumm, value))
                  })
                }
                result <- ShowPrettyUseCachedRule[A]
              } yield result
            }
          case Left(reason) =>
            MIO.pure(Rule.yielded(reason.toString))
        }
      }

    private def deriveEnumShowPretty[A: ShowCtx](
        enumm: Enum[A],
        value: Expr[A]
    ): MIO[Expr[String]] = {
      implicit val StringType: Type[String] = ShowTypes.String
      enumm
        .matchOn[MIO, String](value) { matched =>
          import matched.{value as caseValue, Underlying as EnumCase}
          Log.namedScope(s"Deriving ShowPretty for enum case ${EnumCase.prettyPrint}") {
            deriveShowPrettyRecursively[EnumCase](using sctx.nest(caseValue))
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
