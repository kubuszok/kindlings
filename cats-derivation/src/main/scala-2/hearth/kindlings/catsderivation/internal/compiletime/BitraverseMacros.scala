package hearth.kindlings.catsderivation
package internal.compiletime

import hearth.MacroCommonsScala2
import scala.reflect.macros.blackbox

final private[catsderivation] class BitraverseMacros(val c: blackbox.Context)
    extends MacroCommonsScala2
    with BitraverseMacrosImpl {

  def deriveBitraverseImpl[F[_, _]](implicit ft: c.WeakTypeTag[F[Any, Any]]): c.Expr[cats.Bitraverse[F]] = {
    val untypedF: UntypedType = ft.tpe.typeConstructor
    val fCtor: Type.Ctor2[F] = Type.Ctor2.fromUntyped[F](untypedF)

    val bitraverseCtor = c.universe.weakTypeOf[cats.Bitraverse[Either]].typeConstructor
    val bitraverseFTpe = c.universe.appliedType(bitraverseCtor, List(ft.tpe.typeConstructor))
    val bitraverseFType =
      c.WeakTypeTag[cats.Bitraverse[F]](bitraverseFTpe).asInstanceOf[Type[cats.Bitraverse[F]]]

    deriveBitraverse[F](fCtor, bitraverseFType)
  }
}
