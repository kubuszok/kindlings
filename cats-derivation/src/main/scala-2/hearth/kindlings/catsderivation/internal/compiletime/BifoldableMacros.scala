package hearth.kindlings.catsderivation
package internal.compiletime

import hearth.MacroCommonsScala2
import scala.reflect.macros.blackbox

final private[catsderivation] class BifoldableMacros(val c: blackbox.Context)
    extends MacroCommonsScala2
    with BifoldableMacrosImpl {

  // Entry-point glue only: recover the statically-known `F` from the macro's `WeakTypeTag[F[Any, Any]]`.
  def deriveBifoldableImpl[F[_, _]](implicit ft: c.WeakTypeTag[F[Any, Any]]): c.Expr[cats.Bifoldable[F]] =
    deriveBifoldable[F](Type.Ctor2.fromUntyped[F](ft.tpe.typeConstructor))
}
