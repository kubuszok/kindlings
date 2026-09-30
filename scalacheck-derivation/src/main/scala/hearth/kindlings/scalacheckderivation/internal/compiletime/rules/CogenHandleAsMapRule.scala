package hearth.kindlings.scalacheckderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

import org.scalacheck.Cogen

trait CogenHandleAsMapRuleImpl { this: CogenMacrosImpl & MacroCommons & StdExtensions =>

  @scala.annotation.nowarn("msg=is never used")
  object CogenHandleAsMapRule extends CogenDerivationRule("handle as Map when possible") {
    def apply[A: CogenCtx]: MIO[Rule.Applicability[Expr[Cogen[A]]]] =
      Type[A] match {
        case IsMap(isMap) =>
          import isMap.Underlying as Pair
          deriveMapCogen[A, Pair](isMap.value)
        case _ =>
          MIO.pure(Rule.yielded(s"The type ${Type[A].prettyPrint} is not a Map"))
      }

    private def deriveMapCogen[A: CogenCtx, Pair: Type](
        isMap: IsMapOf[A, Pair]
    ): MIO[Rule.Applicability[Expr[Cogen[A]]]] = {
      import isMap.{Key, Value}
      implicit val CogenA: Type[Cogen[A]] = CogenTypes.Cogen[A]
      implicit val CogenPair: Type[Cogen[Pair]] = CogenTypes.Cogen[Pair]

      // Read through `asIterable` instead of casting to `Map` (issue #218).
      val toIterableFn = collectionToIterableFn[A, Pair](isMap)
      for {
        keyCogen <- deriveCogenRecursively[Key](using cogenctx.nest[Key])
        valueCogen <- deriveCogenRecursively[Value](using cogenctx.nest[Value])
      } yield {
        val pairCogen: Expr[Cogen[Pair]] = Expr.quote {
          hearth.kindlings.scalacheckderivation.internal.runtime.CogenUtils.cogenPair(
            Expr.splice(keyCogen),
            Expr.splice(valueCogen),
            (p: Pair) => Expr.splice(isMap.key(Expr.quote(p))),
            (p: Pair) => Expr.splice(isMap.value(Expr.quote(p)))
          )
        }
        Rule.matched(Expr.quote {
          hearth.kindlings.scalacheckderivation.internal.runtime.CogenUtils
            .cogenCollectionWithIterable(Expr.splice(pairCogen), Expr.splice(toIterableFn))
        })
      }
    }
  }
}
