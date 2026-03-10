package hearth.kindlings.catsderivation
package internal.compiletime

import hearth.MacroCommonsScala2
import scala.reflect.macros.blackbox

final private[catsderivation] class AlternativeMacros(val c: blackbox.Context)
    extends MacroCommonsScala2
    with AlternativeMacrosImpl {

  def deriveAlternativeImpl[F[_]](implicit ft: c.WeakTypeTag[F[Any]]): c.Expr[cats.Alternative[F]] = {
    val untypedF: UntypedType = ft.tpe.typeConstructor
    val fCtor: Type.Ctor1[F] = Type.Ctor1.fromUntyped[F](untypedF)

    val altCtor = c.universe.weakTypeOf[cats.Alternative[Any]].typeConstructor
    val altFTpe = c.universe.appliedType(altCtor, List(ft.tpe.typeConstructor))
    val altFType =
      c.WeakTypeTag[cats.Alternative[F]](altFTpe).asInstanceOf[Type[cats.Alternative[F]]]

    deriveAlternative[F](fCtor, altFType)
  }
}
