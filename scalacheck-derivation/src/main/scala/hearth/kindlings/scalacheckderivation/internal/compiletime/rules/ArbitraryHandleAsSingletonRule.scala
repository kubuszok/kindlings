package hearth.kindlings.scalacheckderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

import org.scalacheck.Gen

trait ArbitraryHandleAsSingletonRuleImpl { this: ArbitraryMacrosImpl & MacroCommons & StdExtensions =>

  object ArbitraryHandleAsSingletonRule extends ArbitraryDerivationRule("handle as singleton when possible") {
    def apply[A: ArbitraryCtx]: MIO[Rule.Applicability[Expr[Gen[A]]]] =
      SingletonValue.parse[A].toEither match {
        case Right(sv) =>
          Log.info(s"Handling ${Type[A].prettyPrint} as singleton") >>
            MIO.pure(Rule.matched(Expr.quote {
              // through `Any`: on Scala 2 a `scala.Enumeration` value `Mon` is typed `Value`, not `Mon.type`, so
              // `Gen.const[Mon.type](Mon)` would not compile
              hearth.kindlings.scalacheckderivation.internal.runtime.ScalaCheckUtils
                .constSingleton[A](Expr.splice(sv.singletonExpr))
            }))
        case Left(reason) =>
          MIO.pure(Rule.yielded(reason))
      }
  }
}
