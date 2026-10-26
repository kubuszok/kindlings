package hearth.kindlings.catsderivation
package internal.compiletime

import hearth.MacroCommonsScala2
import scala.reflect.macros.blackbox

final private[catsderivation] class EmptyKMacros(val c: blackbox.Context)
    extends MacroCommonsScala2
    with EmptyKMacrosImpl {

  // Entry-point glue only: `of[F]` reads the statically-known `F` from the implicit `WeakTypeTag[F[Any]]`.
  def deriveEmptyKImpl[F[_]](implicit ft: c.WeakTypeTag[F[Any]]): c.Expr[alleycats.EmptyK[F]] =
    deriveEmptyK[F](Type.Ctor1.of[F])
}
