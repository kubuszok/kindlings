package hearth.kindlings.catsderivation
package internal.compiletime

import hearth.MacroCommonsScala2
import scala.reflect.macros.blackbox

final private[catsderivation] class FunctorMacros(val c: blackbox.Context)
    extends MacroCommonsScala2
    with FunctorMacrosImpl {

  // Entry-point glue only: `of[F]` reads the statically-known `F` from the implicit `WeakTypeTag[F[Any]]`.
  def deriveFunctorImpl[F[_]](implicit ft: c.WeakTypeTag[F[Any]]): c.Expr[cats.Functor[F]] =
    deriveFunctor[F](Type.Ctor1.of[F])
}
