package hearth.kindlings.catsderivation
package internal.compiletime

import hearth.MacroCommonsScala2
import scala.reflect.macros.blackbox

final private[catsderivation] class ApplyMacros(val c: blackbox.Context)
    extends MacroCommonsScala2
    with ApplyMacrosImpl {

  // Entry-point glue only: `of[F]` reads the statically-known `F` from the implicit `WeakTypeTag[F[Any]]`.
  def deriveApplyImpl[F[_]](implicit ft: c.WeakTypeTag[F[Any]]): c.Expr[cats.Apply[F]] =
    deriveApply[F](Type.Ctor1.of[F])
}
