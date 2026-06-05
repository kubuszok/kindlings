package hearth.kindlings.catstaglessderivation
package internal.compiletime

import hearth.MacroCommonsScala2
import scala.reflect.macros.blackbox

final private[catstaglessderivation] class InvariantKMacros(val c: blackbox.Context)
    extends MacroCommonsScala2
    with InvariantKMacrosImpl
    with TaglessKBridgeScala2 {

  protected def mkWCtor1: Type.Ctor1[
    hearth.kindlings.catstaglessderivation.internal.runtime.CatsTaglessFactories.WCtor1
  ] = {
    val tpe = c.universe
      .weakTypeOf[hearth.kindlings.catstaglessderivation.internal.runtime.CatsTaglessFactories.WCtor1[Any]]
      .typeConstructor
    Type.Ctor1
      .fromUntyped[hearth.kindlings.catstaglessderivation.internal.runtime.CatsTaglessFactories.WCtor1](tpe)
  }

  protected def mkWCtor2: Type.Ctor1[
    hearth.kindlings.catstaglessderivation.internal.runtime.CatsTaglessFactories.WCtor2
  ] = {
    val tpe = c.universe
      .weakTypeOf[hearth.kindlings.catstaglessderivation.internal.runtime.CatsTaglessFactories.WCtor2[Any]]
      .typeConstructor
    Type.Ctor1
      .fromUntyped[hearth.kindlings.catstaglessderivation.internal.runtime.CatsTaglessFactories.WCtor2](tpe)
  }

  def deriveInvariantKImpl[Alg[_[_]]](implicit
      at: c.WeakTypeTag[Alg[Option]]
  ): c.Expr[cats.tagless.InvariantK[Alg]] = {
    val untypedAlg: UntypedType = at.tpe.typeConstructor
    val algCtorK1: Type.CtorK1[Alg] = Type.CtorK1.fromUntyped[Alg](untypedAlg)

    val invariantKCtor =
      c.universe
        .weakTypeOf[cats.tagless.InvariantK[
          hearth.kindlings.catstaglessderivation.internal.runtime.CatsTaglessFactories.DummyHKT
        ]]
        .typeConstructor
    val invariantKAlgTpe = c.universe.appliedType(invariantKCtor, List(at.tpe.typeConstructor))
    val invariantKAlgType =
      c.WeakTypeTag[cats.tagless.InvariantK[Alg]](invariantKAlgTpe)
        .asInstanceOf[Type[cats.tagless.InvariantK[Alg]]]

    deriveInvariantK[Alg](algCtorK1, invariantKAlgType)
  }
}
