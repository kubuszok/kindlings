package hearth.kindlings.catsderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

/** `Option[X]` without an implicit `Monoid[Option[X]]` (see [[SemigroupOptionRuleImpl]]): empty is `None`. */
@scala.annotation.nowarn("msg=is never used")
trait MonoidOptionRuleImpl {
  this: MonoidMacrosImpl & MacroCommons & StdExtensions =>

  object MonoidOptionRule extends MonoidDerivationRule("Monoid as Option") {
    def apply[A: MonoidCtx]: MIO[Rule.Applicability[MonoidDerivationResult[A]]] =
      Type[A] match {
        case IsOption(isOption) =>
          val combine: (Expr[A], Expr[A]) => MIO[Expr[A]] = (x, y) =>
            deriveSemigroupRecursively[A](using SemigroupCtx(Type[A], x, y, moidctx.cache, moidctx.derivedType))
          MIO.pure(Rule.matched(MonoidDerivationResult(isOption.value.empty, combine)))
        case _ =>
          MIO.pure(Rule.yielded(s"${Type[A].prettyPrint} is not an Option"))
      }
  }
}
