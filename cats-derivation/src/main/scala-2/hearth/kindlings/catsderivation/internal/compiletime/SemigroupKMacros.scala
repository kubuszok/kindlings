package hearth.kindlings.catsderivation
package internal.compiletime

import hearth.MacroCommonsScala2
import scala.reflect.macros.blackbox

final private[catsderivation] class SemigroupKMacros(val c: blackbox.Context)
    extends MacroCommonsScala2
    with SemigroupKMacrosImpl {

  // Entry-point glue only: `of[F]` reads the statically-known `F` from the implicit `WeakTypeTag[F[Any]]`.
  def deriveSemigroupKImpl[F[_]](implicit ft: c.WeakTypeTag[F[Any]]): c.Expr[cats.SemigroupK[F]] =
    deriveSemigroupK[F](Type.Ctor1.of[F])
}
