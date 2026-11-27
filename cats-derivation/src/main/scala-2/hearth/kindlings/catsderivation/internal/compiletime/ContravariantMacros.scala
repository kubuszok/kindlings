package hearth.kindlings.catsderivation
package internal.compiletime

import hearth.MacroCommonsScala2
import scala.reflect.macros.blackbox

final private[catsderivation] class ContravariantMacros(val c: blackbox.Context)
    extends MacroCommonsScala2
    with AliasAwareCtor1
    with ContravariantMacrosImpl {

  // Entry-point glue only: `of[F]` reads the statically-known `F` from the implicit `WeakTypeTag[F[Any]]`.
  def deriveContravariantImpl[F[_]](implicit ft: c.WeakTypeTag[F[Any]]): c.Expr[cats.Contravariant[F]] =
    deriveContravariant[F](aliasAwareCtor1[F](ft)(Type.Ctor1.of[F]))
}
