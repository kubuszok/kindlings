package hearth.kindlings.catsderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.data.NonEmptyList
import hearth.fp.effect.*
import hearth.fp.syntax.*
import hearth.std.*

trait MonoidCaseClassRuleImpl {
  this: MonoidMacrosImpl & MacroCommons & StdExtensions =>

  @scala.annotation.nowarn("msg=is never used")
  object MonoidCaseClassRule extends MonoidDerivationRule("Monoid as case class") {
    def apply[A: MonoidCtx]: MIO[Rule.Applicability[MonoidDerivationResult[A]]] =
      CaseClass.parse[A].toEither match {
        case Right(caseClass) =>
          deriveMonoidCaseClassEmpty[A](caseClass).map { empty =>
            val combine: (Expr[A], Expr[A]) => MIO[Expr[A]] = (x, y) => {
              // Use a fresh SemigroupCtx with its own cache so combine defs are self-contained
              val sgCtx = SemigroupCtx.from(x, y, moidctx.derivedType)
              for {
                result <- deriveSemigroupRecursively[A](using sgCtx)
                cache <- sgCtx.cache.get
              } yield cache.toValDefs.use(_ => result)
            }
            Rule.matched(MonoidDerivationResult(empty, combine))
          }
        case Left(reason) =>
          MIO.pure(Rule.yielded(reason.toString))
      }

    private def deriveMonoidCaseClassEmpty[A: MonoidCtx](
        caseClass: CaseClass[A]
    ): MIO[Expr[A]] = {
      val constructor = caseClass.primaryConstructor
      val fields = constructor.totalParameters.flatten.toList

      NonEmptyList.fromList(fields) match {
        case Some(fieldList) =>
          fieldList
            .traverse { case (fieldName, param) =>
              import param.tpe.Underlying as Field
              Log.namedScope(s"Deriving Monoid.empty for field $fieldName: ${Field.prettyPrint}") {
                deriveMonoidRecursively[Field](using MonoidCtx(Type.of[Field], moidctx.cache, moidctx.derivedType)).map {
                  result =>
                    (fieldName, result.empty.as_??)
                }
              }
            }
            .flatMap { emptyFields =>
              val fieldMap: Map[String, Expr_??] = emptyFields.toList.toMap
              caseClass.primaryConstructor.fold(
                onInstance = _ => throw new RuntimeException("Constructor should not need instance"),
                onTypes = _ => Map.empty,
                onValues = _ => fieldMap
              ) match {
                case Right(constructExpr) =>
                  MIO.pure(constructExpr.value.asInstanceOf[Expr[A]])
                case Left(error) =>
                  MIO.fail(new RuntimeException(s"Cannot construct empty ${Type[A].prettyPrint}: $error"))
              }
            }
        case None =>
          // No fields — construct empty instance
          caseClass.primaryConstructor.fold(
            onInstance = _ => throw new RuntimeException("Constructor should not need instance"),
            onTypes = _ => Map.empty,
            onValues = _ => Map.empty
          ) match {
            case Right(constructExpr) =>
              MIO.pure(constructExpr.value.asInstanceOf[Expr[A]])
            case Left(error) =>
              MIO.fail(new RuntimeException(s"Cannot construct empty ${Type[A].prettyPrint}: $error"))
          }
      }
    }
  }
}
