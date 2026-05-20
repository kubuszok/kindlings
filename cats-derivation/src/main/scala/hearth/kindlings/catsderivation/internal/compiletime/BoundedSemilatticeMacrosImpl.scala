package hearth.kindlings.catsderivation.internal.compiletime

import hearth.MacroCommons
import hearth.std.*

trait BoundedSemilatticeMacrosImpl extends MonoidMacrosImpl { this: MacroCommons & StdExtensions =>

  @scala.annotation.nowarn("msg=is never used")
  def deriveBoundedSemilattice[A: Type]: Expr[cats.kernel.BoundedSemilattice[A]] = {
    val macroName = "BoundedSemilattice.derived"
    implicit val BSLA: Type[cats.kernel.BoundedSemilattice[A]] =
      BoundedSemilatticeTypes.BoundedSemilattice[A]

    deriveMonoidEntrypoint[A, cats.kernel.BoundedSemilattice[A]](macroName) { (doEmpty, doCombine) =>
      Expr.quote {
        hearth.kindlings.catsderivation.internal.runtime.CatsDerivationFactories.boundedSemilatticeInstance[A](
          Expr.splice(doEmpty),
          (x: A, y: A) => {
            val _ = x
            val _ = y
            Expr.splice(doCombine(Expr.quote(x), Expr.quote(y)))
          }
        )
      }
    }
  }

  protected object BoundedSemilatticeTypes {
    def BoundedSemilattice: Type.Ctor1[cats.kernel.BoundedSemilattice] =
      Type.Ctor1.of[cats.kernel.BoundedSemilattice]
  }
}
