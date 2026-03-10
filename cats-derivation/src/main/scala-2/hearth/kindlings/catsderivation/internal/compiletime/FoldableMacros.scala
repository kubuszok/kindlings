package hearth.kindlings.catsderivation
package internal.compiletime

import hearth.MacroCommonsScala2
import scala.reflect.macros.blackbox

final private[catsderivation] class FoldableMacros(val c: blackbox.Context)
    extends MacroCommonsScala2
    with FoldableMacrosImpl {

  def deriveFoldableImpl[F[_]](implicit ft: c.WeakTypeTag[F[Any]]): c.Expr[cats.Foldable[F]] = {
    val untypedF: UntypedType = ft.tpe.typeConstructor
    val fCtor: Type.Ctor1[F] = Type.Ctor1.fromUntyped[F](untypedF)

    val foldableCtor = c.universe.weakTypeOf[cats.Foldable[Any]].typeConstructor
    val foldableFTpe = c.universe.appliedType(foldableCtor, List(ft.tpe.typeConstructor))
    val foldableFType = c.WeakTypeTag[cats.Foldable[F]](foldableFTpe).asInstanceOf[Type[cats.Foldable[F]]]

    deriveFoldable[F](fCtor, foldableFType)
  }
}
