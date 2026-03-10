package hearth.kindlings.catsderivation
package internal.compiletime

import hearth.MacroCommonsScala2
import scala.reflect.macros.blackbox

final private[catsderivation] class ApplicativeMacros(val c: blackbox.Context)
    extends MacroCommonsScala2
    with ApplicativeMacrosImpl {

  def deriveApplicativeImpl[F[_]](implicit ft: c.WeakTypeTag[F[Any]]): c.Expr[cats.Applicative[F]] = {
    val untypedF: UntypedType = ft.tpe.typeConstructor
    val fCtor: Type.Ctor1[F] = Type.Ctor1.fromUntyped[F](untypedF)

    val applicativeCtor = c.universe.weakTypeOf[cats.Applicative[Any]].typeConstructor
    val applicativeFTpe = c.universe.appliedType(applicativeCtor, List(ft.tpe.typeConstructor))
    val applicativeFType =
      c.WeakTypeTag[cats.Applicative[F]](applicativeFTpe).asInstanceOf[Type[cats.Applicative[F]]]

    deriveApplicative[F](fCtor, applicativeFType)
  }
}
