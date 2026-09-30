package hearth.kindlings.scalacheckderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

import org.scalacheck.Shrink

trait ShrinkHandleAsMapRuleImpl { this: ShrinkMacrosImpl & MacroCommons & StdExtensions =>

  @scala.annotation.nowarn("msg=is never used")
  object ShrinkHandleAsMapRule extends ShrinkDerivationRule("handle as Map when possible") {
    def apply[A: ShrinkCtx]: MIO[Rule.Applicability[Expr[Shrink[A]]]] =
      Type[A] match {
        case IsMap(isMap) =>
          import isMap.Underlying as Pair
          deriveMapShrink[A, Pair](isMap.value)
        case _ =>
          MIO.pure(Rule.yielded(s"The type ${Type[A].prettyPrint} is not a Map"))
      }

    private def deriveMapShrink[A: ShrinkCtx, Pair: Type](
        isMap: IsMapOf[A, Pair]
    ): MIO[Rule.Applicability[Expr[Shrink[A]]]] = {
      import isMap.{Key, Value}
      implicit val ShrinkA: Type[Shrink[A]] = ShrinkTypes.Shrink[A]
      implicit val ShrinkPair: Type[Shrink[Pair]] = ShrinkTypes.Shrink[Pair]

      // Read through `asIterable` and rebuild candidates through `build`, dropping the rejected ones (issue #218).
      collectionBuildFn[A, Pair](isMap) match {
        case Some(buildFn) =>
          val toIterableFn = collectionToIterableFn[A, Pair](isMap)
          for {
            keyShrink <- deriveShrinkRecursively[Key](using shrinkctx.nest[Key])
            valueShrink <- deriveShrinkRecursively[Value](using shrinkctx.nest[Value])
          } yield {
            val pairShrink: Expr[Shrink[Pair]] = Expr.quote {
              hearth.kindlings.scalacheckderivation.internal.runtime.ShrinkUtils.shrinkPair(
                Expr.splice(keyShrink),
                Expr.splice(valueShrink),
                (p: Pair) => Expr.splice(isMap.key(Expr.quote(p))),
                (p: Pair) => Expr.splice(isMap.value(Expr.quote(p))),
                (k: Key, v: Value) => Expr.splice(isMap.pair(Expr.quote(k), Expr.quote(v)))
              )
            }
            Rule.matched(Expr.quote {
              hearth.kindlings.scalacheckderivation.internal.runtime.ShrinkUtils
                .shrinkCollectionWithBuild(Expr.splice(pairShrink), Expr.splice(toIterableFn), Expr.splice(buildFn))
            })
          }
        case None =>
          MIO.pure(Rule.yielded(unsupportedCollectionBuild[A](isMap)))
      }
    }
  }
}
