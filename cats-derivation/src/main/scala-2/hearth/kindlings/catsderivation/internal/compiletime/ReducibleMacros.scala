package hearth.kindlings.catsderivation
package internal.compiletime

import hearth.MacroCommonsScala2
import scala.reflect.macros.blackbox

final private[catsderivation] class ReducibleMacros(val c: blackbox.Context)
    extends MacroCommonsScala2
    with ReducibleMacrosImpl {

  // Entry-point glue only: `of[F]` reads the statically-known `F` from the implicit `WeakTypeTag[F[Any]]`.
  def deriveReducibleImpl[F[_]](implicit ft: c.WeakTypeTag[F[Any]]): c.Expr[cats.Reducible[F]] =
    deriveReducible[F](Type.Ctor1.of[F])
}
