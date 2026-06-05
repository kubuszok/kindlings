package hearth.kindlings.catstaglessderivation
package internal.compiletime

import hearth.MacroCommonsScala2
import scala.reflect.macros.blackbox

final private[catstaglessderivation] class FunctorKMacros(val c: blackbox.Context)
    extends MacroCommonsScala2
    with FunctorKMacrosImpl
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

  def deriveFunctorKImpl[Alg[_[_]]](implicit
      at: c.WeakTypeTag[Alg[Option]]
  ): c.Expr[cats.tagless.FunctorK[Alg]] = {
    val untypedAlg: UntypedType = at.tpe.typeConstructor
    val algCtorK1: Type.CtorK1[Alg] = Type.CtorK1.fromUntyped[Alg](untypedAlg)

    val functorKCtor =
      c.universe
        .weakTypeOf[cats.tagless.FunctorK[
          hearth.kindlings.catstaglessderivation.internal.runtime.CatsTaglessFactories.DummyHKT
        ]]
        .typeConstructor
    val functorKAlgTpe = c.universe.appliedType(functorKCtor, List(at.tpe.typeConstructor))
    val functorKAlgType =
      c.WeakTypeTag[cats.tagless.FunctorK[Alg]](functorKAlgTpe)
        .asInstanceOf[Type[cats.tagless.FunctorK[Alg]]]

    deriveFunctorK[Alg](algCtorK1, functorKAlgType)
  }
}
