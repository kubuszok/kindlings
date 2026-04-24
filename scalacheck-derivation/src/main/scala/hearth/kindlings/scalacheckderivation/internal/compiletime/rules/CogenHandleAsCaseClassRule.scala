package hearth.kindlings.scalacheckderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.data.NonEmptyList
import hearth.fp.effect.*
import hearth.fp.syntax.*
import hearth.std.*

import org.scalacheck.Cogen

trait CogenHandleAsCaseClassRuleImpl { this: CogenMacrosImpl & MacroCommons & StdExtensions =>

  object CogenHandleAsCaseClassRule extends CogenDerivationRule("handle as case class when possible") {
    def apply[A: CogenCtx]: MIO[Rule.Applicability[Expr[Cogen[A]]]] =
      CaseClass.parse[A].toEither match {
        case Right(caseClass) =>
          implicit val UnitT: Type[Unit] = CogenTypes.Unit
          implicit val CogenA: Type[Cogen[A]] = CogenTypes.Cogen[A]
          val defBuilder = ValDefBuilder.ofDef1[Unit, Cogen[A]](s"cogenCaseClass_${Type[A].shortName}")
          for {
            _ <- cogenctx.cache.forwardDeclare("cached-cogen-method", defBuilder)
            _ <- MIO.scoped { runSafe =>
              runSafe(cogenctx.cache.buildCachedWith("cached-cogen-method", defBuilder) { case (_, _) =>
                runSafe(deriveCaseClassCogen[A](caseClass))
              })
            }
            result <- CogenUseCachedRule[A]
          } yield result
        case Left(reason) =>
          MIO.pure(Rule.yielded(reason.toString))
      }

    @scala.annotation.nowarn
    private def deriveCaseClassCogen[A: CogenCtx](
        caseClass: CaseClass[A]
    ): MIO[Expr[Cogen[A]]] = {
      val constructor = caseClass.primaryConstructor
      val fieldsList = constructor.parameters.flatten.toList
      implicit val CogenA: Type[Cogen[A]] = CogenTypes.Cogen[A]
      implicit val AnyT: Type[Any] = CogenTypes.Any

      NonEmptyList.fromList(fieldsList) match {
        case None =>
          // No fields — identity cogen (return seed unchanged)
          MIO.pure[Expr[Cogen[A]]](Expr.quote {
            hearth.kindlings.scalacheckderivation.internal.runtime.CogenUtils.cogenIdentity[A]
          })

        case Some(fields) =>
          // Derive Cogen for each field
          fields
            .parTraverse { case (fieldName, param) =>
              import param.tpe.Underlying as Field
              Log.namedScope(s"Deriving Cogen for field $fieldName: ${Type[Field].prettyPrint}") {
                deriveCogenRecursively[Field](using cogenctx.nest[Field]).map { cogenExpr =>
                  val cogenAny: Expr[Cogen[Any]] = Expr.quote(Expr.splice(cogenExpr).asInstanceOf[Cogen[Any]])
                  cogenAny
                }
              }
            }
            .map { fieldCogenData =>
              val cogenList = fieldCogenData.toList
              val numFields = fieldsList.size
              val numFieldsExpr = Expr(numFields)

              // Build list of Cogen[Any]
              val cogensListExpr: Expr[List[Cogen[Any]]] =
                cogenList.foldRight(Expr.quote(List.empty[Cogen[Any]])) { (cogenExpr, acc) =>
                  Expr.quote(Expr.splice(cogenExpr) :: Expr.splice(acc))
                }

              Expr.quote {
                hearth.kindlings.scalacheckderivation.internal.runtime.CogenUtils.cogenCaseClass[A](
                  Expr.splice(cogensListExpr),
                  Expr.splice(numFieldsExpr)
                )
              }
            }
      }
    }
  }
}
