package hearth.kindlings.catsderivation
package internal.compiletime

import hearth.MacroCommonsScala2
import scala.reflect.macros.blackbox

final private[catsderivation] class EmptyKMacros(val c: blackbox.Context)
    extends MacroCommonsScala2
    with EmptyKMacrosImpl {

  def deriveEmptyKImpl[F[_]](implicit ft: c.WeakTypeTag[F[Any]]): c.Expr[alleycats.EmptyK[F]] = {
    val untypedF: UntypedType = ft.tpe.typeConstructor
    val fCtor: Type.Ctor1[F] = Type.Ctor1.fromUntyped[F](untypedF)

    val emptyKCtor = c.universe.weakTypeOf[alleycats.EmptyK[Any]].typeConstructor
    val emptyKFTpe = c.universe.appliedType(emptyKCtor, List(ft.tpe.typeConstructor))
    val emptyKFType = c.WeakTypeTag[alleycats.EmptyK[F]](emptyKFTpe).asInstanceOf[Type[alleycats.EmptyK[F]]]

    deriveEmptyK[F](fCtor, emptyKFType)
  }
}
