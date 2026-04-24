package hearth.kindlings.scalacheckderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

import org.scalacheck.Gen

trait ArbitraryHandleAsMapRuleImpl { this: ArbitraryMacrosImpl & MacroCommons & StdExtensions =>

  @scala.annotation.nowarn("msg=is never used")
  object ArbitraryHandleAsMapRule extends ArbitraryDerivationRule("handle as Map when possible") {
    def apply[A: ArbitraryCtx]: MIO[Rule.Applicability[Expr[Gen[A]]]] =
      Type[A] match {
        case IsMap(isMap) =>
          import isMap.Underlying as Pair
          deriveMapArbitrary[A, Pair](isMap.value)
        case _ =>
          MIO.pure(Rule.yielded(s"The type ${Type[A].prettyPrint} is not a Map"))
      }

    private def deriveMapArbitrary[A: ArbitraryCtx, Pair: Type](
        isMap: IsMapOf[A, Pair]
    ): MIO[Rule.Applicability[Expr[Gen[A]]]] = {
      import isMap.{Key, Value, CtorResult}
      implicit val GenA: Type[Gen[A]] = ArbitraryTypes.Gen[A]

      for {
        keyGen <- deriveArbitraryRecursively[Key](using arbctx.nest[Key])
        valueGen <- deriveArbitraryRecursively[Value](using arbctx.nest[Value])
      } yield {
        val factoryExpr = isMap.factory
        Rule.matched(Expr.quote {
          _root_.org.scalacheck.Gen
            .sized { n =>
              _root_.org.scalacheck.Gen.resize(
                scala.math.max(n / 2, 0),
                _root_.org.scalacheck.Gen.listOf(
                  for {
                    k <- Expr.splice(keyGen)
                    v <- Expr.splice(valueGen)
                  } yield Expr.splice(isMap.pair(Expr.quote(k), Expr.quote(v)))
                )
              )
            }
            .map { pairs =>
              Expr.splice(factoryExpr).fromSpecific(pairs).asInstanceOf[CtorResult]
            }
            .asInstanceOf[Gen[A]]
        })
      }
    }
  }
}
