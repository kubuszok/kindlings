package hearth.kindlings.catstaglessderivation
package internal.compiletime

import hearth.MacroCommonsScala2
import scala.reflect.macros.blackbox

final private[catstaglessderivation] class InstrumentMacros(val c: blackbox.Context)
    extends MacroCommonsScala2
    with InstrumentMacrosImpl
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

  def deriveInstrumentImpl[Alg[_[_]]](implicit
      at: c.WeakTypeTag[Alg[Option]]
  ): c.Expr[cats.tagless.aop.Instrument[Alg]] = {
    val untypedAlg: UntypedType = at.tpe.typeConstructor
    val algCtorK1: Type.CtorK1[Alg] = Type.CtorK1.fromUntyped[Alg](untypedAlg)

    val instrumentCtor =
      c.universe
        .weakTypeOf[cats.tagless.aop.Instrument[
          hearth.kindlings.catstaglessderivation.internal.runtime.CatsTaglessFactories.DummyHKT
        ]]
        .typeConstructor
    val instrumentAlgTpe = c.universe.appliedType(instrumentCtor, List(at.tpe.typeConstructor))
    val instrumentAlgType =
      c.WeakTypeTag[cats.tagless.aop.Instrument[Alg]](instrumentAlgTpe)
        .asInstanceOf[Type[cats.tagless.aop.Instrument[Alg]]]

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

    val algebraName = at.tpe.typeConstructor.typeSymbol.name.toString

    deriveInstrument[Alg](algCtorK1, instrumentAlgType, functorKAlgType, algebraName)
  }
}
