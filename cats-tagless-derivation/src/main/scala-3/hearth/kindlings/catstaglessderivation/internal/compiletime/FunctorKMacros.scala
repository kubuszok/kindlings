package hearth.kindlings.catstaglessderivation
package internal.compiletime

import hearth.MacroCommonsScala3
import scala.quoted.*

@scala.annotation.nowarn("msg=unused local definition") // cross-quotes plugin artifact of Type.CtorK1.of
final private[catstaglessderivation] class FunctorKMacros(q: Quotes)
    extends MacroCommonsScala3(using q),
      FunctorKMacrosImpl {

  /** Create Type.CtorK1[HKT] — plugin rewrites Type.CtorK1.of[HKT] here because MacroCommons is in scope. */
  def mkCtorK1[HKT[_[_]]](using scala.quoted.Type[HKT]): Type.CtorK1[HKT] = Type.CtorK1.of[HKT]
}
private[catstaglessderivation] object FunctorKMacros {

  def deriveFunctorKImpl[Alg[_[_]]: Type](using q: Quotes): Expr[cats.tagless.FunctorK[Alg]] = {
    val m = new FunctorKMacros(q)
    m.deriveFunctorK[Alg](m.mkCtorK1[Alg])
  }
}
