package hearth.kindlings.catsderivation
package internal.compiletime

import hearth.MacroCommonsScala2
import scala.reflect.macros.blackbox

final private[catsderivation] class BifunctorMacros(val c: blackbox.Context)
    extends MacroCommonsScala2
    with BifunctorMacrosImpl {

  def deriveBifunctorImpl[F[_, _]](implicit ft: c.WeakTypeTag[F[Any, Any]]): c.Expr[cats.Bifunctor[F]] = {
    val untypedF: UntypedType = ft.tpe.typeConstructor
    val fCtor: Type.Ctor2[F] = Type.Ctor2.fromUntyped[F](untypedF)

    val bifunctorCtor = c.universe.weakTypeOf[cats.Bifunctor[Either]].typeConstructor
    val bifunctorFTpe = c.universe.appliedType(bifunctorCtor, List(ft.tpe.typeConstructor))
    val bifunctorFType = c.WeakTypeTag[cats.Bifunctor[F]](bifunctorFTpe).asInstanceOf[Type[cats.Bifunctor[F]]]

    deriveBifunctor[F](fCtor, bifunctorFType)
  }
}
