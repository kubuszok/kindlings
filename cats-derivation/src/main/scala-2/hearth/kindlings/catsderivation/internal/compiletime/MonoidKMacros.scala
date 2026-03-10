package hearth.kindlings.catsderivation
package internal.compiletime

import hearth.MacroCommonsScala2
import scala.reflect.macros.blackbox

final private[catsderivation] class MonoidKMacros(val c: blackbox.Context)
    extends MacroCommonsScala2
    with MonoidKMacrosImpl {

  def deriveMonoidKImpl[F[_]](implicit ft: c.WeakTypeTag[F[Any]]): c.Expr[cats.MonoidK[F]] = {
    val untypedF: UntypedType = ft.tpe.typeConstructor
    val fCtor: Type.Ctor1[F] = Type.Ctor1.fromUntyped[F](untypedF)

    val monoidKCtor = c.universe.weakTypeOf[cats.MonoidK[Any]].typeConstructor
    val monoidKFTpe = c.universe.appliedType(monoidKCtor, List(ft.tpe.typeConstructor))
    val monoidKFType = c.WeakTypeTag[cats.MonoidK[F]](monoidKFTpe).asInstanceOf[Type[cats.MonoidK[F]]]

    deriveMonoidK[F](fCtor, monoidKFType)
  }
}
