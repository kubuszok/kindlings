package hearth.kindlings.scalacheckderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

import org.scalacheck.Gen

trait ArbitraryHandleAsOptionRuleImpl { this: ArbitraryMacrosImpl & MacroCommons & StdExtensions =>

  object ArbitraryHandleAsOptionRule extends ArbitraryDerivationRule("handle as Option when possible") {
    def apply[A: ArbitraryCtx]: MIO[Rule.Applicability[Expr[Gen[A]]]] =
      Type[A] match {
        case IsOption(isOption) =>
          import isOption.Underlying as Inner
          implicit val GenA: Type[Gen[A]] = ArbitraryTypes.Gen[A]

          Log.info(s"Handling ${Type[A].prettyPrint} as Option") >>
            deriveArbitraryRecursively[Inner](using arbctx.nest[Inner]).map { elemGen =>
              if (!canContainItself(Type[Inner].as_??))
                // Exactly ScalaCheck's `arbOption` (what both upstream derivations use): the same values for a seed.
                Rule.matched(Expr.quote(_root_.org.scalacheck.Gen.option(Expr.splice(elemGen)).asInstanceOf[Gen[A]]))
              else
                Rule.matched(Expr.quote {
                  // `Inner` can contain another `Option[Inner]`: favour None at small sizes so that generation
                  // terminates (e.g. `Option[RecursiveNode]`)
                  _root_.org.scalacheck.Gen
                    .sized { n =>
                      if (n <= 0) _root_.org.scalacheck.Gen.const(None)
                      else
                        _root_.org.scalacheck.Gen.resize(n - 1, _root_.org.scalacheck.Gen.option(Expr.splice(elemGen)))
                    }
                    .asInstanceOf[Gen[A]]
                })
            }
        case _ =>
          MIO.pure(Rule.yielded(s"The type ${Type[A].prettyPrint} is not an Option"))
      }
  }
}
