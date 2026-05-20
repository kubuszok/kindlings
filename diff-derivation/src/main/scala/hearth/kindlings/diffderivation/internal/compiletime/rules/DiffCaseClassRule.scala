package hearth.kindlings.diffderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.data.NonEmptyList
import hearth.fp.effect.*
import hearth.fp.syntax.*
import hearth.std.*
import hearth.kindlings.diffderivation.*

trait DiffCaseClassRuleImpl { this: DiffMacrosImpl & MacroCommons & StdExtensions =>

  object DiffCaseClassRule extends DiffDerivationRule("Diff as case class") {

    def apply[A: DiffCtx]: MIO[Rule.Applicability[Expr[DiffResult]]] =
      CaseClass.parse[A].toEither match {
        case Right(caseClass) =>
          implicit val DRT: Type[DiffResult] = DiffTypes.DiffResultType
          val defBuilder = ValDefBuilder.ofDef2[A, A, DiffResult](s"diff_${Type[A].shortName}")
          for {
            _ <- dctx.cache.forwardDeclare("cached-diff-method", defBuilder)
            _ <- MIO.scoped { runSafe =>
              runSafe(dctx.cache.buildCachedWith("cached-diff-method", defBuilder) { case (_, (left, right)) =>
                runSafe(deriveCaseClassDiff[A](caseClass, left, right))
              })
            }
            result <- DiffUseCachedRule[A]
          } yield result
        case Left(reason) =>
          MIO.pure(Rule.yielded(reason.toString))
      }

    private def deriveCaseClassDiff[A: DiffCtx](
        caseClass: CaseClass[A],
        left: Expr[A],
        right: Expr[A]
    ): MIO[Expr[DiffResult]] = {
      implicit val DRT: Type[DiffResult] = DiffTypes.DiffResultType
      val fieldsLeft = caseClass.caseFieldValuesAt(left).toList
      val fieldsRight = caseClass.caseFieldValuesAt(right).toList
      val pn = Expr(Type.prettyPrint[A])
      val fn = Expr(Type.plainPrint[A])
      val sn = Expr(Type.shortName[A])

      NonEmptyList.fromList(fieldsLeft.zip(fieldsRight)) match {
        case Some(fieldPairs) =>
          fieldPairs
            .traverse { case ((fieldName, fieldValueL), (_, fieldValueR)) =>
              import fieldValueL.Underlying as Field
              val fl = fieldValueL.value.asInstanceOf[Expr[Field]]
              val fr = fieldValueR.value.asInstanceOf[Expr[Field]]
              Log.namedScope(s"Deriving Diff for field $fieldName: ${Field.prettyPrint}") {
                deriveDiffRecursively[Field](using dctx.nest(fl, fr)).map(r => (fieldName, r))
              }
            }
            .map { results =>
              val fieldExprs = results.toList.map { case (name, expr) =>
                Expr.quote((Expr.splice(Expr(name)), Expr.splice(expr)))
              }
              val fieldsVec = fieldExprs.foldRight(Expr.quote(Vector.empty[(String, DiffResult)])) { (item, acc) =>
                Expr.quote(Expr.splice(item) +: Expr.splice(acc))
              }
              Expr.quote {
                DiffResult.Record(
                  Expr.splice(pn),
                  Expr.splice(fn),
                  Expr.splice(sn),
                  Expr.splice(sn),
                  Expr.splice(fieldsVec)
                )
              }
            }
        case None =>
          MIO.pure(Expr.quote {
            DiffResult.Record(
              Expr.splice(pn),
              Expr.splice(fn),
              Expr.splice(sn),
              Expr.splice(sn),
              Vector.empty[(String, DiffResult)]
            )
          })
      }
    }
  }
}
