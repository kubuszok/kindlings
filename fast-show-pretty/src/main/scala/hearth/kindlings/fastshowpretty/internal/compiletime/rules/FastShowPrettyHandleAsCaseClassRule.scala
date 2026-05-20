package hearth.kindlings.fastshowpretty.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.data.NonEmptyList
import hearth.fp.effect.*
import hearth.fp.syntax.*
import hearth.std.*

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
              implicit val StringType: Type[String] = Types.String
              val className = Type[A].shortName

              val fieldIndentExpr: Expr[String] = Expr.quote {
                Expr.splice(ctx.config).indentString * (Expr.splice(ctx.level) + 1)
              }
              val closingIndentExpr: Expr[String] = Expr.quote {
                Expr.splice(ctx.config).indentString * Expr.splice(ctx.level)
              }

              ValDefs.createVal[String](fieldIndentExpr).use { fieldIndent =>
                ValDefs.createVal[String](closingIndentExpr).use { closingIndent =>
                  val renderLeftParenthesisAndHeadField = toAppend.head match {
                    case (fieldName, fieldResult) =>
                      val header = Expr(className + "(\n")
                      val fieldPrefix = Expr(fieldName + " = ")
                      Expr.quote {
                        val _ = Expr.splice(ctx.sb).append(Expr.splice(header))
                        val _ = Expr.splice(ctx.sb).append(Expr.splice(fieldIndent)).append(Expr.splice(fieldPrefix))
                        Expr.splice(fieldResult)
                      }
                  }
                  val renderAllFields = toAppend.tail.foldLeft(renderLeftParenthesisAndHeadField) {
                    case (renderPreviousFields, (fieldName, fieldResult)) =>
                      val fieldPrefix = Expr(fieldName + " = ")
                      Expr.quote {
                        val _ = Expr.splice(renderPreviousFields).append(",\n")
                        val _ = Expr.splice(ctx.sb).append(Expr.splice(fieldIndent)).append(Expr.splice(fieldPrefix))
                        Expr.splice(fieldResult)
                      }
                  }

                  Expr.quote {
                    val _ = Expr.splice(renderAllFields).append("\n")
                    Expr.splice(ctx.sb).append(Expr.splice(closingIndent)).append(")")
                  }
                }
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
