package hearth.kindlings.catsderivation.internal.compiletime

import hearth.MacroCommons
import hearth.std.*

trait BandMacrosImpl extends SemigroupMacrosImpl { this: MacroCommons & StdExtensions =>

  @scala.annotation.nowarn("msg=is never used")
  def deriveBand[A: Type]: Expr[cats.kernel.Band[A]] = {
    val macroName = "Band.derived"
    implicit val BA: Type[cats.kernel.Band[A]] = BandTypes.Band[A]

    deriveSemigroupEntrypoint[A, cats.kernel.Band[A]](macroName) { doCombine =>
      Expr.quote {
        hearth.kindlings.catsderivation.internal.runtime.CatsDerivationFactories.bandInstance[A] { (x: A, y: A) =>
          val _ = x
          val _ = y
          Expr.splice(doCombine(Expr.quote(x), Expr.quote(y)))
        }
      }
    }
  }

  protected object BandTypes {
    def Band: Type.Ctor1[cats.kernel.Band] =
      Type.Ctor1.of[cats.kernel.Band]
  }
}
