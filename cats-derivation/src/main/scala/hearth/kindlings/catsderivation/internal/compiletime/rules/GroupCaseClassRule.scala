package hearth.kindlings.catsderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.data.NonEmptyList
import hearth.fp.effect.*
import hearth.fp.syntax.*
import hearth.std.*

trait GroupCaseClassRuleImpl {
  this: GroupMacrosImpl & MacroCommons & StdExtensions =>

  @scala.annotation.nowarn("msg=is never used")
  object GroupCaseClassRule extends GroupDerivationRule("Group as case class") {
    def apply[A: GroupCtx]: MIO[Rule.Applicability[GroupDerivationResult[A]]] =
      CaseClass.parse[A].toEither match {
        case Right(caseClass) =>
          deriveGroupCaseClass[A](caseClass).map(Rule.matched(_))
        case Left(reason) =>
          MIO.pure(Rule.yielded(reason.toString))
      }

    private def deriveGroupCaseClass[A: GroupCtx](
        caseClass: CaseClass[A]
    ): MIO[GroupDerivationResult[A]] = {
      val constructor = caseClass.primaryConstructor
      val fields = constructor.totalParameters.flatten.toList

      NonEmptyList.fromList(fields) match {
        case Some(fieldList) =>
          fieldList
            .traverse { case (fieldName, param) =>
              import param.tpe.Underlying as Field
              Log.namedScope(s"Deriving Group for field $fieldName: ${Field.prettyPrint}") {
                deriveGroupRecursively[Field](using GroupCtx(Type.of[Field], gctx.cache, gctx.derivedType)).map {
                  fieldResult =>
                    (fieldName, fieldResult.empty.as_??)
                }
              }
            }
            .flatMap { emptyFields =>
              val fieldMap: Map[String, Expr_??] = emptyFields.toList.toMap
              constructInstanceFree(caseClass.primaryConstructor, "Constructor", s"empty ${Type[A].prettyPrint}")(
                fieldMap
              ).map { emptyExpr =>
                val empty: Expr[A] = emptyExpr.value.asInstanceOf[Expr[A]]

                val combine: (Expr[A], Expr[A]) => MIO[Expr[A]] = (x, y) => {
                  val sgCtx = SemigroupCtx.from(x, y, gctx.derivedType)
                  for {
                    result <- deriveSemigroupRecursively[A](using sgCtx)
                    cache <- sgCtx.cache.get
                  } yield cache.toValDefs.use(_ => result)
                }

                val inverse: Expr[A] => MIO[Expr[A]] = (aExpr) => {
                  val fieldValues = caseClass.caseFieldValuesAt(aExpr).toList
                  NonEmptyList
                    .fromList(fieldValues)
                    .get
                    .traverse { case (fieldName, fieldValue) =>
                      import fieldValue.Underlying as Field
                      val fieldExpr = fieldValue.value.asInstanceOf[Expr[Field]]
                      deriveGroupRecursively[Field](using GroupCtx(Type.of[Field], gctx.cache, gctx.derivedType))
                        .flatMap(_.inverse(fieldExpr).map(_.as_??))
                        .map(inv => (fieldName, inv))
                    }
                    .flatMap { invertedFields =>
                      val invertedMap: Map[String, Expr_??] = invertedFields.toList.toMap
                      constructInstanceFree(
                        caseClass.primaryConstructor,
                        "Constructor",
                        s"inverse ${Type[A].prettyPrint}"
                      )(invertedMap).map(constructExpr => constructExpr.value.asInstanceOf[Expr[A]])
                    }
                }

                GroupDerivationResult(empty, combine, inverse)
              }
            }

        case None =>
          // No fields — empty case class
          constructInstanceFree(caseClass.primaryConstructor, "Constructor", s"empty ${Type[A].prettyPrint}")(Map.empty)
            .map { constructExprE =>
              val constructExpr: Expr[A] = constructExprE.value.asInstanceOf[Expr[A]]
              val combine: (Expr[A], Expr[A]) => MIO[Expr[A]] = (_, _) => MIO.pure(constructExpr)
              val inverse: Expr[A] => MIO[Expr[A]] = _ => MIO.pure(constructExpr)
              GroupDerivationResult(constructExpr, combine, inverse)
            }
      }
    }
  }
}
