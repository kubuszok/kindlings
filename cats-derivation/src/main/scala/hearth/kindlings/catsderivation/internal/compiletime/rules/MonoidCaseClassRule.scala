package hearth.kindlings.catsderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

trait MonoidCaseClassRuleImpl {
  this: MonoidMacrosImpl & MacroCommons & StdExtensions =>

  object MonoidCaseClassRule extends MonoidDerivationRule("Monoid as case class") {
    def apply[A: MonoidCtx]: MIO[Rule.Applicability[MonoidDerivationResult[A]]] =
      CaseClass.parse[A].toEither match {
        case Right(caseClass) =>
          deriveMonoidEmpty[A](caseClass).map { empty =>
            val combine: (Expr[A], Expr[A]) => MIO[Expr[A]] = (x, y) =>
              deriveSemigroupCombine[A](caseClass, x, y)
            Rule.matched(MonoidDerivationResult(empty, combine))
          }
        case Left(reason) =>
          MIO.pure(Rule.yielded(reason.toString))
      }
  }
}
