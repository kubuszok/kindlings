package hearth.kindlings.catsderivation.internal.compiletime

import hearth.MacroCommons
import hearth.std.*

/** CommutativeMonoid derivation: delegates to Monoid derivation and wraps in CommutativeMonoid. */
trait CommutativeMonoidMacrosImpl extends MonoidMacrosImpl { this: MacroCommons & StdExtensions =>

  @scala.annotation.nowarn("msg=is never used")
  def deriveCommutativeMonoid[A: Type]: Expr[cats.kernel.CommutativeMonoid[A]] = {
    val macroName = "CommutativeMonoid.derived"
    implicit val CMA: Type[cats.kernel.CommutativeMonoid[A]] = CommutativeMonoidTypes.CommutativeMonoid[A]

    deriveMonoidEntrypoint[A, cats.kernel.CommutativeMonoid[A]](macroName) { (doEmpty, doCombine) =>
      Expr.quote {
        new cats.kernel.CommutativeMonoid[A] {
          def empty: A = Expr.splice(doEmpty)
          def combine(x: A, y: A): A = {
            val _ = x
            val _ = y
            Expr.splice(doCombine(Expr.quote(x), Expr.quote(y)))
          }
        }
      }
    }
  }

  protected object CommutativeMonoidTypes {
    def CommutativeMonoid: Type.Ctor1[cats.kernel.CommutativeMonoid] =
      Type.Ctor1.of[cats.kernel.CommutativeMonoid]
  }
}
