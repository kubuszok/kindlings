package hearth.kindlings.catsderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.data.NonEmptyList
import hearth.fp.effect.*
import hearth.fp.syntax.*
import hearth.std.*

trait ShowPrettyCaseClassRuleImpl {
  this: ShowPrettyMacrosImpl & MacroCommons & StdExtensions =>

  @scala.annotation.nowarn("msg=is never used")
  object ShowPrettyCaseClassRule extends ShowDerivationRule("ShowPretty as case class") {

    def apply[A: ShowCtx]: MIO[Rule.Applicability[Expr[String]]] =
      Log.info(s"Checking case class for ShowPretty[${Type[A].prettyPrint}]") >> {
        CaseClass.parse[A].toEither match {
          case Right(caseClass) =>
            implicit val StringType: Type[String] = ShowTypes.String
            val defBuilder = ValDefBuilder.ofDef1[A, String](s"showPretty_${Type[A].shortName}")
            for {
              _ <- sctx.cache.forwardDeclare("cached-show-pretty-method", defBuilder)
              _ <- MIO.scoped { runSafe =>
                runSafe(sctx.cache.buildCachedWith("cached-show-pretty-method", defBuilder) { case (_, value) =>
                  runSafe(deriveCaseClassShowPretty[A](caseClass, value))
                })
              }
              result <- ShowPrettyUseCachedRule[A]
            } yield result
          case Left(reason) =>
            MIO.pure(Rule.yielded(reason.toString))
        }
      }

    @scala.annotation.nowarn("msg=is never used")
    private def indentFieldValue(fieldResult: Expr[String]): Expr[String] =
      Expr.quote(
        hearth.kindlings.catsderivation.internal.runtime.ShowPrettyRuntime
          .indentSubsequentLines(Expr.splice(fieldResult))
      )

    private def deriveCaseClassShowPretty[A: ShowCtx](
        caseClass: CaseClass[A],
        value: Expr[A]
    ): MIO[Expr[String]] = {
      val name = Type[A].shortName

      NonEmptyList.fromList(caseClass.caseFieldValuesAt(value).toList) match {
        case Some(fieldValues) =>
          fieldValues
            .traverse { case (fieldName, fieldValue) =>
              import fieldValue.{Underlying as Field, value as fieldExpr}
              Log.namedScope(s"Deriving ShowPretty for $fieldName: ${Field.prettyPrint}") {
                deriveShowPrettyRecursively[Field](using sctx.nest(fieldExpr)).map(r => (fieldName, r))
              }
            }
            .map { fields =>
              val fieldList = fields.toList
              fieldList match {
                case (firstName, firstResult) :: rest =>
                  val firstIndented = indentFieldValue(firstResult)
                  val firstPrefix = Expr(s"$name(\n  $firstName = ")
                  var combined: Expr[String] =
                    Expr.quote(Expr.splice(firstPrefix) + Expr.splice(firstIndented))
                  rest.foreach { case (fieldName, fieldResult) =>
                    val indented = indentFieldValue(fieldResult)
                    val fieldPrefix = Expr(s",\n  $fieldName = ")
                    combined = Expr.quote(Expr.splice(combined) + Expr.splice(fieldPrefix) + Expr.splice(indented))
                  }
                  val suffix = Expr("\n)")
                  Expr.quote(Expr.splice(combined) + Expr.splice(suffix))
                case Nil =>
                  Expr(s"$name()")
              }
            }
        case None =>
          MIO.pure(Expr(s"$name()"))
      }
    }
  }
}
