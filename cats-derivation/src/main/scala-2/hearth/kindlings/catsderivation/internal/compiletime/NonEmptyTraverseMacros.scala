package hearth.kindlings.catsderivation
package internal.compiletime

import hearth.MacroCommonsScala2
import scala.reflect.macros.blackbox

final private[catsderivation] class NonEmptyTraverseMacros(val c: blackbox.Context)
    extends MacroCommonsScala2
    with NonEmptyTraverseMacrosImpl {

  def deriveNonEmptyTraverseImpl[F[_]](implicit
      ft: c.WeakTypeTag[F[Any]]
  ): c.Expr[cats.NonEmptyTraverse[F]] = {
    val untypedF: UntypedType = ft.tpe.typeConstructor
    val fCtor: Type.Ctor1[F] = Type.Ctor1.fromUntyped[F](untypedF)

    val netCtor = c.universe.weakTypeOf[cats.NonEmptyTraverse[Any]].typeConstructor
    val netFTpe = c.universe.appliedType(netCtor, List(ft.tpe.typeConstructor))
    val netFType =
      c.WeakTypeTag[cats.NonEmptyTraverse[F]](netFTpe).asInstanceOf[Type[cats.NonEmptyTraverse[F]]]

    deriveNonEmptyTraverse[F](fCtor, netFType)
  }
}
