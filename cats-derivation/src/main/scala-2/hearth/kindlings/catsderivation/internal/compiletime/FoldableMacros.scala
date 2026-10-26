package hearth.kindlings.catsderivation
package internal.compiletime

import hearth.MacroCommonsScala2
import scala.reflect.macros.blackbox

final private[catsderivation] class FoldableMacros(val c: blackbox.Context)
    extends MacroCommonsScala2
    with FoldableMacrosImpl {

  // Entry-point glue only: recover the statically-known `F` from the macro's `WeakTypeTag[F[Any]]`.
  def deriveFoldableImpl[F[_]](implicit ft: c.WeakTypeTag[F[Any]]): c.Expr[cats.Foldable[F]] =
    deriveFoldable[F](Type.Ctor1.fromUntyped[F](ft.tpe.typeConstructor))
}
