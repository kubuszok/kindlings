package hearth.kindlings.catsderivation.internal.compiletime

import hearth.MacroCommons
import hearth.std.*

trait CommutativeGroupMacrosImpl extends GroupMacrosImpl { this: MacroCommons & StdExtensions =>

  @scala.annotation.nowarn("msg=is never used")
  def deriveCommutativeGroup[A: Type]: Expr[cats.kernel.CommutativeGroup[A]] = {
    val macroName = "CommutativeGroup.derived"
    implicit val CGA: Type[cats.kernel.CommutativeGroup[A]] = CommutativeGroupTypes.CommutativeGroup[A]

    deriveGroupEntrypoint[A, cats.kernel.CommutativeGroup[A]](macroName) { (doEmpty, doCombine, doInverse) =>
      Expr.quote {
        hearth.kindlings.catsderivation.internal.runtime.CatsDerivationFactories.commutativeGroupInstance[A](
          Expr.splice(doEmpty),
          (x: A, y: A) => {
            val _ = x
            val _ = y
            Expr.splice(doCombine(Expr.quote(x), Expr.quote(y)))
          },
          (a: A) => {
            val _ = a
            Expr.splice(doInverse(Expr.quote(a)))
          }
        )
      }
    }
  }

  protected object CommutativeGroupTypes {
    def CommutativeGroup: Type.Ctor1[cats.kernel.CommutativeGroup] =
      Type.Ctor1.of[cats.kernel.CommutativeGroup]
  }
}
