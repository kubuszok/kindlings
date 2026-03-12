package hearth.kindlings.catsderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.data.NonEmptyList
import hearth.fp.effect.*
import hearth.fp.syntax.*
import hearth.std.*

trait ShowCaseClassRuleImpl {
  this: ShowMacrosImpl & MacroCommons & StdExtensions =>

  object ShowCaseClassRule extends ShowDerivationRule("Show as case class") {

    def apply[A: ShowCtx]: MIO[Rule.Applicability[Expr[String]]] =
      Log.info(s"Checking case class for Show[${Type[A].prettyPrint}]") >> {
        CaseClass.parse[A].toEither match {
          case Right(caseClass) =>
            implicit val StringType: Type[String] = ShowTypes.String
            val defBuilder = ValDefBuilder.ofDef1[A, String](s"show_${Type[A].shortName}")
            for {
              _ <- sctx.cache.forwardDeclare("cached-show-method", defBuilder)
              _ <- MIO.scoped { runSafe =>
                runSafe(sctx.cache.buildCachedWith("cached-show-method", defBuilder) { case (_, value) =>
                  runSafe(deriveCaseClassShow[A](caseClass, value))
                })
              }
              result <- ShowUseCachedRule[A]
            } yield result
          case Left(reason) =>
            MIO.pure(Rule.yielded(reason.toString))
        }
      }

    private def deriveCaseClassShow[A: ShowCtx](
        caseClass: CaseClass[A],
        value: Expr[A]
    ): MIO[Expr[String]] = {
      val name = Type[A].shortName

      NonEmptyList.fromList(caseClass.caseFieldValuesAt(value).toList) match {
        case Some(fieldValues) =>
          fieldValues
            .traverse { case (fieldName, fieldValue) =>
              import fieldValue.{Underlying as Field, value as fieldExpr}
              Log.namedScope(s"Deriving Show for $fieldName: ${Field.prettyPrint}") {
                deriveShowRecursively[Field](using sctx.nest(fieldExpr)).map(r => (fieldName, r))
              }
            }
            .map { fields =>
              val fieldStrings: List[Expr[String]] = fields.toList.map { case (fieldName, fieldResult) =>
                Expr.quote(Expr.splice(Expr(fieldName)) + " = " + Expr.splice(fieldResult))
              }
              fieldStrings match {
                case head :: tail =>
                  val combined = tail.foldLeft(head) { (acc, next) =>
                    Expr.quote(Expr.splice(acc) + ", " + Expr.splice(next))
                  }
                  Expr.quote(Expr.splice(Expr(name)) + "(" + Expr.splice(combined) + ")")
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
