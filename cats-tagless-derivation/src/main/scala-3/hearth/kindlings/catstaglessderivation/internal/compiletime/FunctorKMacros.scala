package hearth.kindlings.catstaglessderivation
package internal.compiletime

import hearth.MacroCommonsScala3
import scala.quoted.*

@scala.annotation.nowarn("msg=unused local definition")
final private[catstaglessderivation] class FunctorKMacros(protected val q: Quotes)
    extends MacroCommonsScala3(using q),
      FunctorKMacrosImpl,
      TaglessKBridgeScala3 {

  def mkCtorK1[HKT[_[_]]](using scala.quoted.Type[HKT]): Type.CtorK1[HKT] = Type.CtorK1.of[HKT]
  def mkCtor1[G[_]](using scala.quoted.Type[G]): Type.Ctor1[G] = Type.Ctor1.of[G]

  protected def mkWCtor1: Type.Ctor1[
    hearth.kindlings.catstaglessderivation.internal.runtime.CatsTaglessFactories.WCtor1
  ] = mkCtor1[hearth.kindlings.catstaglessderivation.internal.runtime.CatsTaglessFactories.WCtor1]

  protected def mkWCtor2: Type.Ctor1[
    hearth.kindlings.catstaglessderivation.internal.runtime.CatsTaglessFactories.WCtor2
  ] = mkCtor1[hearth.kindlings.catstaglessderivation.internal.runtime.CatsTaglessFactories.WCtor2]

  def mkFunctorKType[HKT[_[_]]](using scala.quoted.Type[HKT]): Type[cats.tagless.FunctorK[HKT]] =
    scala.quoted.Type.of[cats.tagless.FunctorK[HKT]].asInstanceOf[Type[cats.tagless.FunctorK[HKT]]]

  def mkInvariantKType[HKT[_[_]]](using scala.quoted.Type[HKT]): Type[cats.tagless.InvariantK[HKT]] =
    scala.quoted.Type.of[cats.tagless.InvariantK[HKT]].asInstanceOf[Type[cats.tagless.InvariantK[HKT]]]
}
private[catstaglessderivation] object FunctorKMacros {

  def deriveFunctorKImpl[Alg[_[_]]: Type](using q: Quotes): Expr[cats.tagless.FunctorK[Alg]] = {
    val m = new FunctorKMacros(q)
    m.deriveFunctorK[Alg](m.mkCtorK1[Alg], m.mkFunctorKType[Alg])
  }
}
