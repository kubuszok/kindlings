package hearth.kindlings.diffderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*
import hearth.kindlings.diffderivation._
import hearth.kindlings.diffderivation.internal.runtime._

trait DiffCollectionRuleImpl { this: DiffMacrosImpl & MacroCommons & StdExtensions =>

  @scala.annotation.nowarn("msg=is never used|unused")
  object DiffCollectionRule extends DiffDerivationRule("Diff as collection") {

    def apply[A: DiffCtx]: MIO[Rule.Applicability[Expr[DiffResult]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as a collection") >> {
        Type[A] match {
          case IsCollection(isCollection) =>
            import isCollection.Underlying as Item
            implicit val DRT: Type[DiffResult] = DiffTypes.DiffResultType
            implicit val ST: Type[String] = DiffTypes.StringType
            implicit val DiffItemT: Type[Diff[Item]] = DiffTypes.Diff[Item]
            val pn = Expr(Type.prettyPrint[A])
            val fn = Expr(Type.plainPrint[A])
            val sn = Expr(Type.shortName[A])
            DiffItemT.summonExprIgnoring(ignoredImplicits*).toEither match {
              case Right(elemDiffExpr) =>
                val ed = elemDiffExpr.asInstanceOf[Expr[Diff[Item]]]
                val iterLeft = isCollection.value.asIterable(dctx.left)
                val iterRight = isCollection.value.asIterable(dctx.right)
                MIO.pure(Rule.matched(Expr.quote {
                  DiffRuntime.diffSeq[Item](
                    Expr.splice(pn), Expr.splice(fn), Expr.splice(sn), Expr.splice(sn),
                    Expr.splice(iterLeft),
                    Expr.splice(iterRight),
                    Expr.splice(ed)
                  )
                }))
              case Left(reason) =>
                MIO.pure(Rule.yielded(s"No Diff[${Item.prettyPrint}]: $reason"))
            }
          case _ =>
            MIO.pure(Rule.yielded(s"${Type[A].prettyPrint} is not a collection"))
        }
      }
  }
}
