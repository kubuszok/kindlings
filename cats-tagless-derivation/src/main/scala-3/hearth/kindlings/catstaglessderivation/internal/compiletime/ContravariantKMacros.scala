package hearth.kindlings.catstaglessderivation
package internal.compiletime

import hearth.MacroCommonsScala3
import scala.quoted.*

@scala.annotation.nowarn("msg=unused local definition")
final private[catstaglessderivation] class ContravariantKMacros(protected val q: Quotes)
    extends MacroCommonsScala3(using q),
      ContravariantKMacrosImpl,
      TaglessKBridgeScala3 {

  def mkCtorK1[HKT[_[_]]](using scala.quoted.Type[HKT]): Type.CtorK1[HKT] = Type.CtorK1.of[HKT]
  def mkCtor1[G[_]](using scala.quoted.Type[G]): Type.Ctor1[G] = Type.Ctor1.of[G]

  protected def mkWCtor1: Type.Ctor1[
    hearth.kindlings.catstaglessderivation.internal.runtime.CatsTaglessFactories.WCtor1
  ] = mkCtor1[hearth.kindlings.catstaglessderivation.internal.runtime.CatsTaglessFactories.WCtor1]

  protected def mkWCtor2: Type.Ctor1[
    hearth.kindlings.catstaglessderivation.internal.runtime.CatsTaglessFactories.WCtor2
  ] = mkCtor1[hearth.kindlings.catstaglessderivation.internal.runtime.CatsTaglessFactories.WCtor2]

  def mkContravariantKType[HKT[_[_]]](using scala.quoted.Type[HKT]): Type[cats.tagless.ContravariantK[HKT]] =
    scala.quoted.Type.of[cats.tagless.ContravariantK[HKT]].asInstanceOf[Type[cats.tagless.ContravariantK[HKT]]]

  def mkInvariantKType[HKT[_[_]]](using scala.quoted.Type[HKT]): Type[cats.tagless.InvariantK[HKT]] =
    scala.quoted.Type.of[cats.tagless.InvariantK[HKT]].asInstanceOf[Type[cats.tagless.InvariantK[HKT]]]
}
private[catstaglessderivation] object ContravariantKMacros {

  def deriveContravariantKImpl[Alg[_[_]]: Type](using q: Quotes): Expr[cats.tagless.ContravariantK[Alg]] = {
    val m = new ContravariantKMacros(q)
    m.deriveContravariantK[Alg](m.mkCtorK1[Alg], m.mkContravariantKType[Alg])
  }
}
