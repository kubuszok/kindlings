package hearth.kindlings.catsderivation
package internal.compiletime

import hearth.MacroCommonsScala2
import scala.reflect.macros.blackbox

final private[catsderivation] class ConsKMacros(val c: blackbox.Context)
    extends MacroCommonsScala2
    with ConsKMacrosImpl {

  def deriveConsKImpl[F[_]](implicit ft: c.WeakTypeTag[F[Any]]): c.Expr[alleycats.ConsK[F]] = {
    val untypedF: UntypedType = ft.tpe.typeConstructor
    val fCtor: Type.Ctor1[F] = Type.Ctor1.fromUntyped[F](untypedF)

    val consKCtor = c.universe.weakTypeOf[alleycats.ConsK[Any]].typeConstructor
    val consKFTpe = c.universe.appliedType(consKCtor, List(ft.tpe.typeConstructor))
    val consKFType =
      c.WeakTypeTag[alleycats.ConsK[F]](consKFTpe).asInstanceOf[Type[alleycats.ConsK[F]]]

    deriveConsK[F](fCtor, consKFType)
  }

  protected def summonConsKForFieldType(fieldType: Type[Any]): Option[Expr[Any]] = {
    val tpe = fieldType.asInstanceOf[c.WeakTypeTag[Any]].tpe
    val ctor = tpe.typeConstructor
    val consKCtor = c.universe.weakTypeOf[alleycats.ConsK[List]].typeConstructor
    val consKGType = c.universe.appliedType(consKCtor, List(ctor))
    c.inferImplicitValue(consKGType) match {
      case c.universe.EmptyTree => None
      case tree                 => Some(c.Expr[Any](tree).asInstanceOf[Expr[Any]])
    }
  }
}
