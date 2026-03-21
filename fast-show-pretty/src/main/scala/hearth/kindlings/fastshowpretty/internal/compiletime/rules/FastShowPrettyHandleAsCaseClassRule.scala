package hearth.kindlings.fastshowpretty.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.data.NonEmptyList
import hearth.fp.effect.*
import hearth.fp.syntax.*
import hearth.std.*

import hearth.kindlings.fastshowpretty.internal.runtime.FastShowPrettyUtils

trait FastShowPrettyHandleAsCaseClassRuleImpl { this: FastShowPrettyMacrosImpl & MacroCommons & StdExtensions =>

  object FastShowPrettyHandleAsCaseClassRule extends DerivationRule("handle as case class when possible") {

    def apply[A: DerivationCtx]: MIO[Rule.Applicability[Expr[StringBuilder]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as a case class") >> {
        CaseClass.parse[A].toEither match {
          case Right(caseClass) =>
            deriveCaseClassFields[A](caseClass).map(Rule.matched)

          case Left(reason) =>
            MIO.pure(Rule.yielded(reason))
        }
      }

    private def deriveCaseClassFields[A: DerivationCtx](
        caseClass: CaseClass[A]
    ): MIO[Expr[StringBuilder]] = {
      val name = Expr(Type[A].shortName)

      NonEmptyList.fromList(caseClass.caseFieldValuesAt(ctx.value).toList) match {
        case Some(fieldValues) =>
          fieldValues
            .parTraverse { case (fieldName, fieldValue) =>
              import fieldValue.{Underlying as Field, value as fieldExpr}
              Log.namedScope(s"Deriving the value ${ctx.value.prettyPrint}.$fieldName: ${Field.prettyPrint}") {
                // Use incrementLevel so nested case classes are indented properly
                deriveResultRecursively[Field](using ctx.incrementLevel.nest(fieldExpr)).map { fieldResult =>
                  (fieldName, fieldResult)
                }
              }
            }
            .map { toAppend =>
              val renderLeftParenthesisAndHeadField = toAppend.head match {
                case (fieldName, fieldResult) =>
                  Expr.quote {
                    val _ = Expr
                      .splice(ctx.sb)
                      .append(Expr.splice(name))
                      .append("(\n")
                    val _ = FastShowPrettyUtils
                      .appendIndent(
                        Expr.splice(ctx.sb),
                        Expr.splice(ctx.config).indentString,
                        Expr.splice(ctx.level) + 1
                      )
                      .append(Expr.splice(Expr(fieldName)))
                      .append(" = ")
                    Expr.splice(fieldResult)
                  }
              }
              val renderAllFields = toAppend.tail.foldLeft(renderLeftParenthesisAndHeadField) {
                case (renderPreviousFields, (fieldName, fieldResult)) =>
                  Expr.quote {
                    val _ = Expr
                      .splice(renderPreviousFields)
                      .append(",\n")
                    val _ = FastShowPrettyUtils
                      .appendIndent(
                        Expr.splice(ctx.sb),
                        Expr.splice(ctx.config).indentString,
                        Expr.splice(ctx.level) + 1
                      )
                      .append(Expr.splice(Expr(fieldName)))
                      .append(" = ")
                    Expr.splice(fieldResult)
                  }
              }

              Expr.quote {
                val _ = Expr.splice(renderAllFields).append("\n")
                FastShowPrettyUtils
                  .appendIndent(
                    Expr.splice(ctx.sb),
                    Expr.splice(ctx.config).indentString,
                    Expr.splice(ctx.level)
                  )
                  .append(")")
              }
            }
        case None =>
          MIO.pure {
            Expr.quote {
              Expr.splice(ctx.sb).append(Expr.splice(name)).append("()")
            }
          }
      }
    }

  }
}
