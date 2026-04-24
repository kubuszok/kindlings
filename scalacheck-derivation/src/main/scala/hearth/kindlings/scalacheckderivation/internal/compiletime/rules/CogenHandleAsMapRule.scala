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

      for {
        keyCogen <- deriveCogenRecursively[Key](using cogenctx.nest[Key])
        valueCogen <- deriveCogenRecursively[Value](using cogenctx.nest[Value])
      } yield {
        Rule.matched(Expr.quote {
          hearth.kindlings.scalacheckderivation.internal.runtime.CogenUtils
            .cogenMap(
              Expr.splice(keyCogen).asInstanceOf[Cogen[Any]],
              Expr.splice(valueCogen).asInstanceOf[Cogen[Any]]
            )
            .asInstanceOf[Cogen[A]]
        })
      }
    }
  }
}
