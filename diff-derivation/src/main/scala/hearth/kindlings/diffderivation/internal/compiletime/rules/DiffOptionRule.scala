package hearth.kindlings.diffderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*
import hearth.kindlings.diffderivation._
import hearth.kindlings.diffderivation.internal.runtime._

trait DiffOptionRuleImpl { this: DiffMacrosImpl & MacroCommons & StdExtensions =>

  @scala.annotation.nowarn("msg=is never used|unused")
  object DiffOptionRule extends DiffDerivationRule("Diff as Option") {

    def apply[A: DiffCtx]: MIO[Rule.Applicability[Expr[DiffResult]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as Option") >> {
        Type[A] match {
          case IsOption(isOption) =>
            import isOption.Underlying as Inner
            implicit val DRT: Type[DiffResult] = DiffTypes.DiffResultType
            implicit val ST: Type[String] = DiffTypes.StringType
            implicit val DiffInnerT: Type[Diff[Inner]] = DiffTypes.Diff[Inner]
            val pn = Expr(Type.prettyPrint[A])
            val fn = Expr(Type.plainPrint[A])
            val sn = Expr(Type.shortName[A])
            DiffInnerT.summonExprIgnoring(ignoredImplicits*).toEither match {
              case Right(elemDiffExpr) =>
                val ed = elemDiffExpr.asInstanceOf[Expr[Diff[Inner]]]
                MIO.pure(Rule.matched(Expr.quote {
                  DiffRuntime.diffOption[Inner](
                    Expr.splice(pn), Expr.splice(fn), Expr.splice(sn), Expr.splice(sn),
                    Expr.splice(dctx.left).asInstanceOf[Option[Inner]],
                    Expr.splice(dctx.right).asInstanceOf[Option[Inner]],
                    Expr.splice(ed)
                  )
                }))
              case Left(reason) =>
                MIO.pure(Rule.yielded(s"No Diff[${Inner.prettyPrint}]: $reason"))
            }
          case _ =>
            MIO.pure(Rule.yielded(s"${Type[A].prettyPrint} is not an Option"))
        }
      }
  }
}
