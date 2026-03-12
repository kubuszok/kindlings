package hearth.kindlings.catsderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

trait SemigroupCaseClassRuleImpl {
  this: SemigroupMacrosImpl & MacroCommons & StdExtensions =>

  object SemigroupCaseClassRule extends SemigroupDerivationRule("Semigroup as case class") {
    def apply[A: SemigroupCtx]: MIO[Rule.Applicability[Expr[A]]] =
      CaseClass.parse[A].toEither match {
        case Right(caseClass) =>
          deriveSemigroupCombine[A](caseClass, sgctx.x, sgctx.y).map(Rule.matched(_))
        case Left(reason) =>
          MIO.pure(Rule.yielded(reason.toString))
      }
  }
}
