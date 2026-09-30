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
      import isMap.{Key, Value}
      implicit val GenA: Type[Gen[A]] = ArbitraryTypes.Gen[A]
      implicit val GenPair: Type[Gen[Pair]] = ArbitraryTypes.Gen[Pair]

      // Construct through the provider's `build` (not by treating `CtorResult` as `A`), see issue #218.
      collectionBuildFn[A, Pair](isMap) match {
        case Some(buildFn) =>
          for {
            keyGen <- deriveArbitraryRecursively[Key](using arbctx.nest[Key])
            valueGen <- deriveArbitraryRecursively[Value](using arbctx.nest[Value])
          } yield {
            val pairGen: Expr[Gen[Pair]] = Expr.quote {
              for {
                k <- Expr.splice(keyGen)
                v <- Expr.splice(valueGen)
              } yield Expr.splice(isMap.pair(Expr.quote(k), Expr.quote(v)))
            }
            Rule.matched(Expr.quote {
              hearth.kindlings.scalacheckderivation.internal.runtime.ScalaCheckUtils
                .genCollection(Expr.splice(pairGen), Expr.splice(buildFn))
            })
          }
        case None =>
          MIO.pure(Rule.yielded(unsupportedCollectionBuild[A](isMap)))
      }
    }
  }
}
