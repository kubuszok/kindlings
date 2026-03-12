package hearth.kindlings.catsderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.data.NonEmptyList
import hearth.fp.effect.*
import hearth.fp.syntax.*
import hearth.std.*

trait EmptyCaseClassRuleImpl {
  this: EmptyMacrosImpl & MacroCommons & StdExtensions =>

  object EmptyCaseClassRule extends EmptyDerivationRule("Empty as case class") {

    def apply[A: EmptyCtx]: MIO[Rule.Applicability[Expr[A]]] =
      CaseClass.parse[A].toEither match {
        case Right(caseClass) =>
          val defBuilder = ValDefBuilder.ofLazy[A](s"empty_${Type[A].shortName}")
          for {
            _ <- ectx.cache.forwardDeclare("cached-empty-value", defBuilder)
            _ <- MIO.scoped { runSafe =>
              runSafe(ectx.cache.buildCachedWith("cached-empty-value", defBuilder) { _ =>
                runSafe(deriveCaseClassEmpty[A](caseClass))
              })
            }
            result <- EmptyUseCachedRule[A]
          } yield result
        case Left(reason) =>
          MIO.pure(Rule.yielded(reason.toString))
      }

    @scala.annotation.nowarn("msg=is never used")
    private def deriveCaseClassEmpty[A: EmptyCtx](
        caseClass: CaseClass[A]
    ): MIO[Expr[A]] = {
      val constructor = caseClass.primaryConstructor
      val fields = constructor.parameters.flatten.toList

      NonEmptyList.fromList(fields) match {
        case Some(fieldList) =>
          fieldList
            .traverse { case (fieldName, param) =>
              import param.tpe.Underlying as Field
              Log.namedScope(s"Deriving Empty for field $fieldName: ${Field.prettyPrint}") {
                deriveEmptyRecursively[Field](using ectx.nestType[Field]).map(r => (fieldName, r.as_??))
              }
            }
            .flatMap { emptyFields =>
              val fieldMap: Map[String, Expr_??] = emptyFields.toList.toMap
              caseClass.primaryConstructor(fieldMap) match {
                case Right(constructExpr) => MIO.pure(constructExpr)
                case Left(error)          =>
                  MIO.fail(new RuntimeException(s"Cannot construct empty ${Type[A].prettyPrint}: $error"))
              }
            }
        case None =>
          caseClass.primaryConstructor(Map.empty) match {
            case Right(constructExpr) => MIO.pure(constructExpr)
            case Left(error)          =>
              MIO.fail(new RuntimeException(s"Cannot construct empty ${Type[A].prettyPrint}: $error"))
          }
      }
    }
  }
}
