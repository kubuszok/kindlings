package hearth.kindlings.diffderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*
import hearth.kindlings.diffderivation.*
import hearth.kindlings.diffderivation.internal.runtime.*

trait DiffMapRuleImpl { this: DiffMacrosImpl & MacroCommons & StdExtensions =>

  @scala.annotation.nowarn("msg=is never used|unused")
  object DiffMapRule extends DiffDerivationRule("Diff as Map") {

    def apply[A: DiffCtx]: MIO[Rule.Applicability[Expr[DiffResult]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as a Map") >> {
        Type[A] match {
          case IsMap(isMapEx) =>
            import isMapEx.Underlying as Pair
            deriveMapDiff[A, Pair](isMapEx.value)
          case _ =>
            MIO.pure(Rule.yielded(s"${Type[A].prettyPrint} is not a Map"))
        }
      }

    private def deriveMapDiff[A: DiffCtx, Pair: Type](
        isMap: IsMapOf[A, Pair]
    ): MIO[Rule.Applicability[Expr[DiffResult]]] = {
      import isMap.{Key, Value}
      implicit val DRT: Type[DiffResult] = DiffTypes.DiffResultType
      implicit val DiffVT: Type[Diff[Value]] = DiffTypes.Diff[Value]
      val pn = Expr(Type.prettyPrint[A])
      val fn = Expr(Type.plainPrint[A])
      val sn = Expr(Type.shortName[A])
      summonOrDeriveDiffInstance[Value](dctx.cache, dctx.derivedType).flatMap {
        case Some(vd) =>
          val iterLeft = isMap.asIterable(dctx.left)
          val iterRight = isMap.asIterable(dctx.right)
          MIO.pure(Rule.matched(Expr.quote {
            DiffRuntime.diffMap[Key, Value](
              Expr.splice(pn),
              Expr.splice(fn),
              Expr.splice(sn),
              Expr.splice(sn),
              Expr.splice(iterLeft).asInstanceOf[Iterable[(Key, Value)]],
              Expr.splice(iterRight).asInstanceOf[Iterable[(Key, Value)]],
              (k: Key) => k.toString,
              Expr.splice(vd)
            )
          }))
        case None =>
          MIO.pure(Rule.yielded(s"No Diff[${Value.prettyPrint}]"))
      }
    }
  }
}
