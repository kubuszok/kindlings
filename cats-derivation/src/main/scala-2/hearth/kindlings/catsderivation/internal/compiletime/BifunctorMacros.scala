package hearth.kindlings.catsderivation
package internal.compiletime

import hearth.MacroCommonsScala2
import scala.reflect.macros.blackbox

final private[catsderivation] class BifunctorMacros(val c: blackbox.Context)
    extends MacroCommonsScala2
    with BifunctorMacrosImpl {

  // Entry-point glue only: recover the statically-known `F` from the macro's `WeakTypeTag[F[Any, Any]]`.
  def deriveBifunctorImpl[F[_, _]](implicit ft: c.WeakTypeTag[F[Any, Any]]): c.Expr[cats.Bifunctor[F]] =
    deriveBifunctor[F](Type.Ctor2.fromUntyped[F](ft.tpe.typeConstructor))
}
