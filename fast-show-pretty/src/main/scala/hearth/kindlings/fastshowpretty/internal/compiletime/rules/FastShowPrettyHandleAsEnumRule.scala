package hearth.kindlings.fastshowpretty.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

trait FastShowPrettyHandleAsEnumRuleImpl { this: FastShowPrettyMacrosImpl & MacroCommons & StdExtensions =>

  object FastShowPrettyHandleAsEnumRule extends DerivationRule("handle as enum when possible") {

    def apply[A: DerivationCtx]: MIO[Rule.Applicability[Expr[StringBuilder]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as an enum") >> {
        Enum.parse[A].toEither match {
          case Right(enumm) =>
            deriveEnumCases[A](enumm).map(Rule.matched)
          case Left(reason) =>
            MIO.pure(Rule.yielded(reason))
        }
      }

    private def deriveEnumCases[A: DerivationCtx](
        enumm: Enum[A]
    ): MIO[Expr[StringBuilder]] = {
      val name = Expr(Type[A].shortName)

      implicit val StringBuilder: Type[StringBuilder] = Types.StringBuilder

      enumm
        .parMatchOn[MIO, StringBuilder](ctx.value) { matched =>
          import matched.{value as enumCaseValue, Underlying as EnumCase}
          Log.namedScope(s"Deriving the value ${enumCaseValue.prettyPrint}: ${EnumCase.prettyPrint}") {
            // Use incrementLevel so nested case classes in enum cases are indented properly
            deriveResultRecursively[EnumCase](using ctx.incrementLevel.nest(enumCaseValue)).map { enumCaseResult =>
              Expr.quote {
                val _ = Expr.splice(ctx.sb).append("(")
                Expr.splice(enumCaseResult).append("): ").append(Expr.splice(name))
              }
            }
          }
        }
        .flatMap {
          case Some(result) =>
            MIO.pure(result)
          case None =>
            val err = DerivationError.NoChildrenInSealedTrait(Type[A].prettyPrint)
            Log.error(err.message) >> MIO.fail(err)
        }
    }

  }
}
