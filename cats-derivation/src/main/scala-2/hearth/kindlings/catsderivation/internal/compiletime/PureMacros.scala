package hearth.kindlings.catsderivation
package internal.compiletime

import hearth.MacroCommonsScala2
import scala.reflect.macros.blackbox

final private[catsderivation] class PureMacros(val c: blackbox.Context) extends MacroCommonsScala2 with PureMacrosImpl {

  def derivePureImpl[F[_]](implicit ft: c.WeakTypeTag[F[Any]]): c.Expr[alleycats.Pure[F]] = {
    val untypedF: UntypedType = ft.tpe.typeConstructor
    val fCtor: Type.Ctor1[F] = Type.Ctor1.fromUntyped[F](untypedF)

    val pureCtor = c.universe.weakTypeOf[alleycats.Pure[Any]].typeConstructor
    val pureFTpe = c.universe.appliedType(pureCtor, List(ft.tpe.typeConstructor))
    val pureFType = c.WeakTypeTag[alleycats.Pure[F]](pureFTpe).asInstanceOf[Type[alleycats.Pure[F]]]

    derivePure[F](fCtor, pureFType)
  }
}
