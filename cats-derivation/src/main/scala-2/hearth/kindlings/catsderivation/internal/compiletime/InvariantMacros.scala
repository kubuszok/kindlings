package hearth.kindlings.catsderivation
package internal.compiletime

import hearth.MacroCommonsScala2
import scala.reflect.macros.blackbox

final private[catsderivation] class InvariantMacros(val c: blackbox.Context)
    extends MacroCommonsScala2
    with InvariantMacrosImpl {

  def deriveInvariantImpl[F[_]](implicit ft: c.WeakTypeTag[F[Any]]): c.Expr[cats.Invariant[F]] = {
    val untypedF: UntypedType = ft.tpe.typeConstructor
    val fCtor: Type.Ctor1[F] = Type.Ctor1.fromUntyped[F](untypedF)

    val invariantCtor = c.universe.weakTypeOf[cats.Invariant[Any]].typeConstructor
    val invariantFTpe = c.universe.appliedType(invariantCtor, List(ft.tpe.typeConstructor))
    val invariantFType =
      c.WeakTypeTag[cats.Invariant[F]](invariantFTpe).asInstanceOf[Type[cats.Invariant[F]]]

    deriveInvariant[F](fCtor, invariantFType)
  }
}
