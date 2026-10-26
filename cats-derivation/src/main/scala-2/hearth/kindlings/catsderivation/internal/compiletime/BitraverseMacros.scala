package hearth.kindlings.catsderivation
package internal.compiletime

import hearth.MacroCommonsScala2
import scala.reflect.macros.blackbox

final private[catsderivation] class BitraverseMacros(val c: blackbox.Context)
    extends MacroCommonsScala2
    with BitraverseMacrosImpl {

  // Entry-point glue only: `of[F]` reads the statically-known `F` from the implicit `WeakTypeTag[F[Any, Any]]`.
  def deriveBitraverseImpl[F[_, _]](implicit ft: c.WeakTypeTag[F[Any, Any]]): c.Expr[cats.Bitraverse[F]] =
    deriveBitraverse[F](Type.Ctor2.of[F])
}
