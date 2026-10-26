package hearth.kindlings.catsderivation
package internal.compiletime

import hearth.MacroCommonsScala2
import scala.reflect.macros.blackbox

final private[catsderivation] class AlternativeMacros(val c: blackbox.Context)
    extends MacroCommonsScala2
    with AlternativeMacrosImpl {

  // Entry-point glue only: `of[F]` reads the statically-known `F` from the implicit `WeakTypeTag[F[Any]]`.
  def deriveAlternativeImpl[F[_]](implicit ft: c.WeakTypeTag[F[Any]]): c.Expr[cats.Alternative[F]] =
    deriveAlternative[F](Type.Ctor1.of[F])
}
