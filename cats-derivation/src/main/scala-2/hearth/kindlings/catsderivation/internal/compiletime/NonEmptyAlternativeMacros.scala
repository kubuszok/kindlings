package hearth.kindlings.catsderivation
package internal.compiletime

import hearth.MacroCommonsScala2
import scala.reflect.macros.blackbox

final private[catsderivation] class NonEmptyAlternativeMacros(val c: blackbox.Context)
    extends MacroCommonsScala2
    with NonEmptyAlternativeMacrosImpl {

  def deriveNonEmptyAlternativeImpl[F[_]](implicit
      ft: c.WeakTypeTag[F[Any]]
  ): c.Expr[cats.NonEmptyAlternative[F]] = {
    val untypedF: UntypedType = ft.tpe.typeConstructor
    val fCtor: Type.Ctor1[F] = Type.Ctor1.fromUntyped[F](untypedF)

    val neaCtor = c.universe.weakTypeOf[cats.NonEmptyAlternative[Any]].typeConstructor
    val neaFTpe = c.universe.appliedType(neaCtor, List(ft.tpe.typeConstructor))
    val neaFType =
      c.WeakTypeTag[cats.NonEmptyAlternative[F]](neaFTpe).asInstanceOf[Type[cats.NonEmptyAlternative[F]]]

    deriveNonEmptyAlternative[F](fCtor, neaFType)
  }
}
