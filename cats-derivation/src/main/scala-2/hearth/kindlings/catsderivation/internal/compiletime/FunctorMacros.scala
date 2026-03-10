package hearth.kindlings.catsderivation
package internal.compiletime

import hearth.MacroCommonsScala2
import scala.reflect.macros.blackbox

final private[catsderivation] class FunctorMacros(val c: blackbox.Context)
    extends MacroCommonsScala2
    with FunctorMacrosImpl {

  def deriveFunctorImpl[F[_]](implicit ft: c.WeakTypeTag[F[Any]]): c.Expr[cats.Functor[F]] = {
    // Use fromUntyped to create the Ctor1 at expansion time from the concrete type constructor.
    val untypedF: UntypedType = ft.tpe.typeConstructor
    val fCtor: Type.Ctor1[F] = Type.Ctor1.fromUntyped[F](untypedF)

    val functorCtor = c.universe.weakTypeOf[cats.Functor[Any]].typeConstructor
    val functorFTpe = c.universe.appliedType(functorCtor, List(ft.tpe.typeConstructor))
    val functorFType = c.WeakTypeTag[cats.Functor[F]](functorFTpe).asInstanceOf[Type[cats.Functor[F]]]

    deriveFunctor[F](fCtor, functorFType)
  }
}
