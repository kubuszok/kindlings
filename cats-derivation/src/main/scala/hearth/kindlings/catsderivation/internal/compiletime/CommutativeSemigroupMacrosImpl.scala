package hearth.kindlings.catsderivation.internal.compiletime

import hearth.MacroCommons
import hearth.std.*

/** CommutativeSemigroup derivation: delegates to Semigroup derivation and wraps in CommutativeSemigroup. */
trait CommutativeSemigroupMacrosImpl extends SemigroupMacrosImpl { this: MacroCommons & StdExtensions =>

  @scala.annotation.nowarn("msg=is never used")
  def deriveCommutativeSemigroup[A: Type]: Expr[cats.kernel.CommutativeSemigroup[A]] = {
    val macroName = "CommutativeSemigroup.derived"
    implicit val CSA: Type[cats.kernel.CommutativeSemigroup[A]] = CommutativeSemigroupTypes.CommutativeSemigroup[A]

    deriveSemigroupEntrypoint[A, cats.kernel.CommutativeSemigroup[A]](macroName) { doCombine =>
      Expr.quote {
        new cats.kernel.CommutativeSemigroup[A] {
          def combine(x: A, y: A): A = {
            val _ = x
            val _ = y
            Expr.splice(doCombine(Expr.quote(x), Expr.quote(y)))
          }
        }
      }
    }
  }

  protected object CommutativeSemigroupTypes {
    def CommutativeSemigroup: Type.Ctor1[cats.kernel.CommutativeSemigroup] =
      Type.Ctor1.of[cats.kernel.CommutativeSemigroup]
  }
}
