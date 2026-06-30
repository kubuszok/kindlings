package hearth.kindlings.diffderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*
import hearth.kindlings.diffderivation.*

trait DiffEnumRuleImpl { this: DiffMacrosImpl & MacroCommons & StdExtensions =>

  object DiffEnumRule extends DiffDerivationRule("Diff as enum") {

    def apply[A: DiffCtx]: MIO[Rule.Applicability[Expr[DiffResult]]] =
      Enum.parse[A].toEither match {
        case Right(enumm) =>
          implicit val DRT: Type[DiffResult] = DiffTypes.DiffResultType
          val defBuilder = ValDefBuilder.ofDef2[A, A, DiffResult](s"diff_${Type[A].shortName}")
          for {
            _ <- dctx.cache.forwardDeclare("cached-diff-method", defBuilder)
            _ <- MIO.scoped { runSafe =>
              runSafe(dctx.cache.buildCachedWith("cached-diff-method", defBuilder) { case (_, (left, right)) =>
                runSafe(deriveEnumDiff[A](enumm, left, right))
              })
            }
            result <- DiffUseCachedRule[A]
          } yield result
        case Left(reason) =>
          MIO.pure(Rule.yielded(reason.toString))
      }

    @scala.annotation.nowarn("msg=is never used|unused|is unchecked")
    private def deriveEnumDiff[A: DiffCtx](
        enumm: Enum[A],
        left: Expr[A],
        right: Expr[A]
    ): MIO[Expr[DiffResult]] = {
      implicit val DRT: Type[DiffResult] = DiffTypes.DiffResultType
      implicit val ST: Type[String] = DiffTypes.StringType
      val pn = Expr(Type.prettyPrint[A])
      val fn = Expr(Type.plainPrint[A])
      val sn = Expr(Type.shortName[A])

      enumm
        .matchOn[MIO, DiffResult](left) { matchedLeft =>
          import matchedLeft.{value as caseLeft, Underlying as EnumCase}
          val caseName = Expr(Type.shortName[EnumCase])

          Log.namedScope(s"Deriving Diff for enum case ${EnumCase.prettyPrint}") {
            val isInstance = Expr.quote(Expr.splice(right).isInstanceOf[EnumCase])

            deriveSnapshotRecursively[EnumCase](using
              SnapshotCtx[EnumCase](Type[EnumCase], caseLeft, dctx.cache, dctx.derivedType)
            ).flatMap { leftSnapshotExpr =>
              val caseRight = Expr.quote(Expr.splice(right).asInstanceOf[EnumCase])
              deriveDiffRecursively[EnumCase](using dctx.nest(caseLeft, caseRight)).map { diffResult =>
                Expr.quote {
                  if (Expr.splice(isInstance)) {
                    DiffResult.Variant(
                      Expr.splice(pn),
                      Expr.splice(fn),
                      Expr.splice(sn),
                      Expr.splice(sn),
                      Expr.splice(caseName),
                      Expr.splice(diffResult)
                    )
                  } else {
                    DiffResult.TypeMismatch(
                      Expr.splice(pn),
                      Expr.splice(fn),
                      Expr.splice(sn),
                      Expr.splice(sn),
                      Expr.splice(caseName),
                      Expr.splice(leftSnapshotExpr),
                      Expr.splice(right).getClass.getSimpleName.stripSuffix("$"),
                      DiffResult.Identical(
                        Expr.splice(pn),
                        Expr.splice(fn),
                        Expr.splice(sn),
                        Expr.splice(sn),
                        Expr.splice(right).toString
                      )
                    )
                  }
                }
              }
            }
          }
        }
        .flatMap {
          case Some(result) => MIO.pure(result)
          case None         =>
            MIO.pure(Expr.quote {
              DiffResult.Identical(
                Expr.splice(pn),
                Expr.splice(fn),
                Expr.splice(sn),
                Expr.splice(sn),
                Expr.splice(left).toString
              )
            })
        }
    }
  }
}
