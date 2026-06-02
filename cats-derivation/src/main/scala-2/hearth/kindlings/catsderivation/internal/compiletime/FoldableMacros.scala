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

  protected def mkCtor1FromTypeFoldable(appliedType: Type[Any]): Option[UntypedType] = {
    val tpe = appliedType.asInstanceOf[c.WeakTypeTag[Any]].tpe
    val ctor = tpe.typeConstructor
    if (ctor == tpe) None
    else Some(ctor.asInstanceOf[UntypedType])
  }

  protected def summonFoldableForFieldType(fieldType: Type[Any]): Option[Expr[Any]] = {
    val tpe = fieldType.asInstanceOf[c.WeakTypeTag[Any]].tpe
    val ctor = tpe.typeConstructor
    val foldableCtor = c.universe.weakTypeOf[cats.Foldable[List]].typeConstructor
    val foldableGType = c.universe.appliedType(foldableCtor, List(ctor))
    c.inferImplicitValue(foldableGType) match {
      case c.universe.EmptyTree => None
      case tree                 => Some(c.Expr[Any](tree).asInstanceOf[Expr[Any]])
    }
  }
}
