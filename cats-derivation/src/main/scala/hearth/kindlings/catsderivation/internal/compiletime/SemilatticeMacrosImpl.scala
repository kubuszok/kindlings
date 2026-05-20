package hearth.kindlings.catsderivation.internal.compiletime

import hearth.MacroCommons
import hearth.std.*

trait SemilatticeMacrosImpl extends SemigroupMacrosImpl { this: MacroCommons & StdExtensions =>

  @scala.annotation.nowarn("msg=is never used")
  def deriveSemilattice[A: Type]: Expr[cats.kernel.Semilattice[A]] = {
    val macroName = "Semilattice.derived"
    implicit val SLA: Type[cats.kernel.Semilattice[A]] = SemilatticeTypes.Semilattice[A]

    deriveSemigroupEntrypoint[A, cats.kernel.Semilattice[A]](macroName) { doCombine =>
      Expr.quote {
        hearth.kindlings.catsderivation.internal.runtime.CatsDerivationFactories.semilatticeInstance[A] {
          (x: A, y: A) =>
            val _ = x
            val _ = y
            Expr.splice(doCombine(Expr.quote(x), Expr.quote(y)))
        }
      }
    }
  }

  protected object SemilatticeTypes {
    def Semilattice: Type.Ctor1[cats.kernel.Semilattice] =
      Type.Ctor1.of[cats.kernel.Semilattice]
  }
}
