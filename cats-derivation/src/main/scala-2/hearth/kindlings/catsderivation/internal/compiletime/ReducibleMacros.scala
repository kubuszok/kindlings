package hearth.kindlings.catsderivation
package internal.compiletime

import hearth.MacroCommonsScala2
import scala.reflect.macros.blackbox

final private[catsderivation] class ReducibleMacros(val c: blackbox.Context)
    extends MacroCommonsScala2
    with ReducibleMacrosImpl {

  def deriveReducibleImpl[F[_]](implicit ft: c.WeakTypeTag[F[Any]]): c.Expr[cats.Reducible[F]] = {
    val untypedF: UntypedType = ft.tpe.typeConstructor
    val fCtor: Type.Ctor1[F] = Type.Ctor1.fromUntyped[F](untypedF)

    val reducibleCtor = c.universe.weakTypeOf[cats.Reducible[Any]].typeConstructor
    val reducibleFTpe = c.universe.appliedType(reducibleCtor, List(ft.tpe.typeConstructor))
    val reducibleFType = c.WeakTypeTag[cats.Reducible[F]](reducibleFTpe).asInstanceOf[Type[cats.Reducible[F]]]

    deriveReducible[F](fCtor, reducibleFType)
  }
}
