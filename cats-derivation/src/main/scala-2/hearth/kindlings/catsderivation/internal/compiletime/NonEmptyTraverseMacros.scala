package hearth.kindlings.catsderivation
package internal.compiletime

import hearth.MacroCommonsScala2
import scala.reflect.macros.blackbox

final private[catsderivation] class NonEmptyTraverseMacros(val c: blackbox.Context)
    extends MacroCommonsScala2
    with NonEmptyTraverseMacrosImpl {

  // Entry-point glue only: `of[F]` reads the statically-known `F` from the implicit `WeakTypeTag[F[Any]]`.
  def deriveNonEmptyTraverseImpl[F[_]](implicit ft: c.WeakTypeTag[F[Any]]): c.Expr[cats.NonEmptyTraverse[F]] =
    deriveNonEmptyTraverse[F](Type.Ctor1.of[F])
}
