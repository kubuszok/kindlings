package hearth.kindlings.catsderivation
package internal.compiletime

import hearth.MacroCommonsScala2
import scala.reflect.macros.blackbox

final private[catsderivation] class BifoldableMacros(val c: blackbox.Context)
    extends MacroCommonsScala2
    with BifoldableMacrosImpl {

  def deriveBifoldableImpl[F[_, _]](implicit ft: c.WeakTypeTag[F[Any, Any]]): c.Expr[cats.Bifoldable[F]] = {
    val untypedF: UntypedType = ft.tpe.typeConstructor
    val fCtor: Type.Ctor2[F] = Type.Ctor2.fromUntyped[F](untypedF)

    val bifoldableCtor = c.universe.weakTypeOf[cats.Bifoldable[Either]].typeConstructor
    val bifoldableFTpe = c.universe.appliedType(bifoldableCtor, List(ft.tpe.typeConstructor))
    val bifoldableFType =
      c.WeakTypeTag[cats.Bifoldable[F]](bifoldableFTpe).asInstanceOf[Type[cats.Bifoldable[F]]]

    deriveBifoldable[F](fCtor, bifoldableFType)
  }
}
