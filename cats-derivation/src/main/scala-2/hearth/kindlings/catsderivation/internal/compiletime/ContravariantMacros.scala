package hearth.kindlings.catsderivation
package internal.compiletime

import hearth.MacroCommonsScala2
import scala.reflect.macros.blackbox

final private[catsderivation] class ContravariantMacros(val c: blackbox.Context)
    extends MacroCommonsScala2
    with ContravariantMacrosImpl {

  def deriveContravariantImpl[F[_]](implicit ft: c.WeakTypeTag[F[Any]]): c.Expr[cats.Contravariant[F]] = {
    val untypedF: UntypedType = ft.tpe.typeConstructor
    val fCtor: Type.Ctor1[F] = Type.Ctor1.fromUntyped[F](untypedF)

    val contravariantCtor = c.universe.weakTypeOf[cats.Contravariant[Any]].typeConstructor
    val contravariantFTpe = c.universe.appliedType(contravariantCtor, List(ft.tpe.typeConstructor))
    val contravariantFType =
      c.WeakTypeTag[cats.Contravariant[F]](contravariantFTpe).asInstanceOf[Type[cats.Contravariant[F]]]

    deriveContravariant[F](fCtor, contravariantFType)
  }
}
