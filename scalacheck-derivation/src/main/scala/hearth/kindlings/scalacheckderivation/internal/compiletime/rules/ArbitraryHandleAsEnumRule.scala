package hearth.kindlings.scalacheckderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.data.NonEmptyList
import hearth.fp.effect.*
import hearth.fp.syntax.*
import hearth.std.*

import org.scalacheck.Gen

trait ArbitraryHandleAsEnumRuleImpl { this: ArbitraryMacrosImpl & MacroCommons & StdExtensions =>

  object ArbitraryHandleAsEnumRule extends ArbitraryDerivationRule("handle as enum when possible") {
    def apply[A: ArbitraryCtx]: MIO[Rule.Applicability[Expr[Gen[A]]]] =
      Enum.parse[A].toEither match {
        case Right(enumData) =>
          Log.info(s"Handling ${Type[A].prettyPrint} as enum") >>
            deriveEnumArbitrary[A](enumData).map(Rule.matched(_))
        case Left(reason) =>
          MIO.pure(Rule.yielded(reason))
      }

    @scala.annotation.nowarn("msg=is never used")
    private def deriveEnumArbitrary[A: ArbitraryCtx](enumData: Enum[A]): MIO[Expr[Gen[A]]] = {
      implicit val GenA: Type[Gen[A]] = ArbitraryTypes.Gen[A]

      // Get all direct children
      val childrenList = enumData.directChildren.toList

      childrenList match {
        case Nil =>
          MIO.fail(new RuntimeException(s"Enum ${Type[A].prettyPrint} has no cases"))
        case children =>
          // Derive Gen for each enum case
          NonEmptyList
            .fromList(children)
            .get
            .parTraverse { case (_, enumChild) =>
              import enumChild.Underlying as CaseType
              Log.namedScope(s"Deriving Arbitrary for enum case ${CaseType.prettyPrint}") {
                deriveArbitraryRecursively[CaseType](using arbctx.nest[CaseType]).map { caseGen =>
                  Expr.quote(Expr.splice(caseGen).asInstanceOf[org.scalacheck.Gen[A]])
                }
              }
            }
            .map { caseGens =>
              caseGens.toList match {
                case singleGen :: Nil =>
                  // Single case
                  singleGen
                case firstGen :: secondGen :: rest =>
                  // Multiple cases: use Gen.oneOf with at least 2 explicit args
                  if (rest.isEmpty) {
                    // Exactly 2 cases - no varargs needed
                    Expr.quote {
                      _root_.org.scalacheck.Gen.oneOf(Expr.splice(firstGen), Expr.splice(secondGen))
                    }
                  } else {
                    // 3+ cases - build list by prepending to avoid empty type parameter
                    // Start with last element as a singleton list
                    val reversedRest = rest.reverse
                    val restGens: Expr[_root_.scala.List[_root_.org.scalacheck.Gen[A]]] =
                      reversedRest.tail.foldLeft(Expr.quote(_root_.scala.List(Expr.splice(reversedRest.head)))) {
                        (accExpr, genExpr) =>
                          Expr.quote(Expr.splice(genExpr) :: Expr.splice(accExpr))
                      }

                    Expr.quote {
                      _root_.org.scalacheck.Gen
                        .oneOf(Expr.splice(firstGen), Expr.splice(secondGen), Expr.splice(restGens)*)
                    }
                  }
                case Nil =>
                  // Should never happen due to NonEmptyList, but handle for safety
                  throw new RuntimeException(s"Enum ${Type[A].prettyPrint} has no cases")
              }
            }
      }
    }
  }
}
