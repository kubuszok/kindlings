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
            implicit val SensitiveDataType: Type[hearth.kindlings.fastshowpretty.annotations.sensitiveData] =
              Types.SensitiveData
            if (hasTypeAnnotation[hearth.kindlings.fastshowpretty.annotations.sensitiveData, A]) {
              val reason = getTypeAnnotationStringArg[hearth.kindlings.fastshowpretty.annotations.sensitiveData, A]
              val text = redactedText(reason)
              MIO.pure(Rule.matched(Expr.quote(Expr.splice(ctx.sb).append(Expr.splice(Expr(text))))))
            } else {
              deriveCaseClassFields[A](caseClass).map(Rule.matched)
            }

          case Left(reason) =>
            MIO.pure(Rule.yielded(reason))
        }
      }

    private def redactedText(reason: Option[String]): String =
      reason.filter(_.nonEmpty).fold("[redacted]")(r => s"[redacted: $r]")

    private def deriveCaseClassFields[A: DerivationCtx](
        caseClass: CaseClass[A]
    ): MIO[Expr[StringBuilder]] = {
      val name = Expr(Type[A].shortName)
      implicit val SensitiveDataType: Type[hearth.kindlings.fastshowpretty.annotations.sensitiveData] =
        Types.SensitiveData
      val paramsByName: Map[String, Parameter] = caseClass.primaryConstructor.totalParameters.flatten.toMap

      NonEmptyList.fromList(caseClass.caseFieldValuesAt(ctx.value).toList) match {
        case Some(fieldValues) =>
          fieldValues
            .parTraverse { case (fieldName, fieldValue) =>
              import fieldValue.{Underlying as Field, value as fieldExpr}
              Log.namedScope(s"Deriving the value ${ctx.value.prettyPrint}.$fieldName: ${Field.prettyPrint}") {
                paramsByName.get(fieldName) match {
                  case Some(param)
                      if hasAnnotationType[hearth.kindlings.fastshowpretty.annotations.sensitiveData](param) =>
                    val reason =
                      getAnnotationStringArg[hearth.kindlings.fastshowpretty.annotations.sensitiveData](param)
                    val text = redactedText(reason)
                    MIO.pure((fieldName, Expr.quote(Expr.splice(ctx.sb).append(Expr.splice(Expr(text))))))
                  case _ =>
                    deriveResultRecursively[Field](using ctx.incrementLevel.nest(fieldExpr)).map { fieldResult =>
                      (fieldName, fieldResult)
                    }
                }
              }
            }
            .map(toAppend => renderFieldsFlat[A](Type[A].shortName + "(\n", toAppend))
        case None =>
          MIO.pure {
            Expr.quote {
              Expr.splice(ctx.sb).append(Expr.splice(name)).append("()")
            }
          }
      }
    }

  }

  /** Renders `header`, then each field as `name = <value>` on its own indented line, then the closing `)`.
    *
    * One flat statement per field: `nextField(<previous field's rendering>, ...)`. Nesting each field's appends inside
    * the previous one's (a `foldLeft` of blocks) made the Scala 2.13 StackMapTable quadratic in the number of fields
    * (503 KB for 90 fields).
    */
  @scala.annotation.nowarn("msg=is never used")
  protected def renderFieldsFlat[A: DerivationCtx](
      header: String,
      fields: NonEmptyList[(String, Expr[StringBuilder])]
  ): Expr[StringBuilder] = {
    implicit val StringType: Type[String] = Types.String
    implicit val IntType: Type[Int] = Types.Int

    def renderRemaining(
        previous: Expr[StringBuilder],
        remaining: List[(String, Expr[StringBuilder])]
    ): Expr[StringBuilder] = remaining match {
      case (fieldName, fieldResult) :: tail =>
        val prefix = Expr(fieldName + " = ")
        Expr.quote {
          FastShowPrettyUtils.nextField(
            Expr.splice(previous),
            Expr.splice(ctx.sb),
            Expr.splice(ctx.config).indentString,
            Expr.splice(ctx.level) + 1,
            Expr.splice(prefix)
          )
          Expr.splice(renderRemaining(fieldResult, tail))
        }
      case Nil =>
        Expr.quote {
          FastShowPrettyUtils.closeFields(
            Expr.splice(previous),
            Expr.splice(ctx.sb),
            Expr.splice(ctx.config).indentString,
            Expr.splice(ctx.level)
          )
        }
    }

    val (headName, headResult) = fields.head
    val headerExpr = Expr(header)
    val headPrefix = Expr(headName + " = ")
    Expr.quote {
      FastShowPrettyUtils.openFields(
        Expr.splice(ctx.sb),
        Expr.splice(headerExpr),
        Expr.splice(ctx.config).indentString,
        Expr.splice(ctx.level) + 1,
        Expr.splice(headPrefix)
      )
      Expr.splice(renderRemaining(headResult, fields.tail))
    }
  }
}
