package hearth.kindlings.catsderivation
package internal.compiletime

import hearth.MacroCommonsScala2
import scala.reflect.macros.blackbox

final private[catsderivation] class BifunctorMacros(val c: blackbox.Context)
    extends MacroCommonsScala2
    with BifunctorMacrosImpl {

  // Entry-point glue only: `of[F]` reads the statically-known `F` from the implicit `WeakTypeTag[F[Any, Any]]`.
  def deriveBifunctorImpl[F[_, _]](implicit ft: c.WeakTypeTag[F[Any, Any]]): c.Expr[cats.Bifunctor[F]] =
    deriveBifunctor[F](Type.Ctor2.of[F])
}
