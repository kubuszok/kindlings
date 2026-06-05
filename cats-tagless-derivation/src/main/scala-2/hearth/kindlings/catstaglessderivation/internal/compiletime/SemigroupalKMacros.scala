package hearth.kindlings.catstaglessderivation
package internal.compiletime

import hearth.MacroCommonsScala2
import scala.reflect.macros.blackbox

final private[catstaglessderivation] class SemigroupalKMacros(val c: blackbox.Context)
    extends MacroCommonsScala2
    with SemigroupalKMacrosImpl
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

  def deriveSemigroupalKImpl[Alg[_[_]]](implicit
      at: c.WeakTypeTag[Alg[Option]]
  ): c.Expr[cats.tagless.SemigroupalK[Alg]] = {
    val untypedAlg: UntypedType = at.tpe.typeConstructor
    val algCtorK1: Type.CtorK1[Alg] = Type.CtorK1.fromUntyped[Alg](untypedAlg)

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

    deriveSemigroupalK[Alg](algCtorK1, semigroupalKAlgType)
  }
}
