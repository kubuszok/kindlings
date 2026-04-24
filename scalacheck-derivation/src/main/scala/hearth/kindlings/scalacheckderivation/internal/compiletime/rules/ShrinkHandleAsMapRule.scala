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
      import isMap.{Key, Value, CtorResult}
      implicit val ShrinkA: Type[Shrink[A]] = ShrinkTypes.Shrink[A]

      for {
        keyShrink <- deriveShrinkRecursively[Key](using shrinkctx.nest[Key])
        valueShrink <- deriveShrinkRecursively[Value](using shrinkctx.nest[Value])
      } yield {
        Rule.matched(Expr.quote {
          hearth.kindlings.scalacheckderivation.internal.runtime.ShrinkUtils
            .shrinkMap(
              Expr.splice(keyShrink).asInstanceOf[Shrink[Any]],
              Expr.splice(valueShrink).asInstanceOf[Shrink[Any]]
            )
            .asInstanceOf[Shrink[A]]
        })
      }
    }
  }
}
