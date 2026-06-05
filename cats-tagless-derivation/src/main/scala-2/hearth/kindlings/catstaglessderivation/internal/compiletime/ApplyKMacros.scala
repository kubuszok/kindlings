package hearth.kindlings.catstaglessderivation
package internal.compiletime

import hearth.MacroCommonsScala2
import scala.reflect.macros.blackbox

final private[catstaglessderivation] class ApplyKMacros(val c: blackbox.Context)
    extends MacroCommonsScala2
    with ApplyKMacrosImpl
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

  protected def mkWCtor3: Type.Ctor1[
    hearth.kindlings.catstaglessderivation.internal.runtime.CatsTaglessFactories.WCtor3
  ] = {
    val tpe = c.universe
      .weakTypeOf[hearth.kindlings.catstaglessderivation.internal.runtime.CatsTaglessFactories.WCtor3[Any]]
      .typeConstructor
    Type.Ctor1
      .fromUntyped[hearth.kindlings.catstaglessderivation.internal.runtime.CatsTaglessFactories.WCtor3](tpe)
  }

  def deriveApplyKImpl[Alg[_[_]]](implicit
      at: c.WeakTypeTag[Alg[Option]]
  ): c.Expr[cats.tagless.ApplyK[Alg]] = {
    val untypedAlg: UntypedType = at.tpe.typeConstructor
    val algCtorK1: Type.CtorK1[Alg] = Type.CtorK1.fromUntyped[Alg](untypedAlg)

    val applyKCtor =
      c.universe
        .weakTypeOf[cats.tagless.ApplyK[
          hearth.kindlings.catstaglessderivation.internal.runtime.CatsTaglessFactories.DummyHKT
        ]]
        .typeConstructor
    val applyKAlgTpe = c.universe.appliedType(applyKCtor, List(at.tpe.typeConstructor))
    val applyKAlgType =
      c.WeakTypeTag[cats.tagless.ApplyK[Alg]](applyKAlgTpe)
        .asInstanceOf[Type[cats.tagless.ApplyK[Alg]]]

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

    val semigroupalKCtor =
      c.universe
        .weakTypeOf[cats.tagless.SemigroupalK[
          hearth.kindlings.catstaglessderivation.internal.runtime.CatsTaglessFactories.DummyHKT
        ]]
        .typeConstructor
    val semigroupalKAlgTpe = c.universe.appliedType(semigroupalKCtor, List(at.tpe.typeConstructor))
    val semigroupalKAlgType =
      c.WeakTypeTag[cats.tagless.SemigroupalK[Alg]](semigroupalKAlgTpe)
        .asInstanceOf[Type[cats.tagless.SemigroupalK[Alg]]]

    deriveApplyK[Alg](algCtorK1, applyKAlgType, functorKAlgType, semigroupalKAlgType)
  }
}
