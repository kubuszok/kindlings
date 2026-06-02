package hearth.kindlings.catsderivation
package internal.compiletime

import hearth.MacroCommonsScala2
import scala.reflect.macros.blackbox

final private[catsderivation] class TraverseMacros(val c: blackbox.Context)
    extends MacroCommonsScala2
    with TraverseMacrosImpl {

  def deriveTraverseImpl[F[_]](implicit ft: c.WeakTypeTag[F[Any]]): c.Expr[cats.Traverse[F]] = {
    val untypedF: UntypedType = ft.tpe.typeConstructor
    val fCtor: Type.Ctor1[F] = Type.Ctor1.fromUntyped[F](untypedF)

    val traverseCtor = c.universe.weakTypeOf[cats.Traverse[Any]].typeConstructor
    val traverseFTpe = c.universe.appliedType(traverseCtor, List(ft.tpe.typeConstructor))
    val traverseFType = c.WeakTypeTag[cats.Traverse[F]](traverseFTpe).asInstanceOf[Type[cats.Traverse[F]]]

    deriveTraverse[F](fCtor, traverseFType)
  }

  protected def mkCtor1FromTypeTraverse(appliedType: Type[Any]): Option[UntypedType] = {
    val tpe = appliedType.asInstanceOf[c.WeakTypeTag[Any]].tpe
    val ctor = tpe.typeConstructor
    if (ctor == tpe) None
    else Some(ctor.asInstanceOf[UntypedType])
  }

  protected def summonTraverseForFieldType(fieldType: Type[Any]): Option[Expr[Any]] = {
    val tpe = fieldType.asInstanceOf[c.WeakTypeTag[Any]].tpe
    val ctor = tpe.typeConstructor
    val traverseCtor = c.universe.weakTypeOf[cats.Traverse[List]].typeConstructor
    val traverseGType = c.universe.appliedType(traverseCtor, List(ctor))
    c.inferImplicitValue(traverseGType) match {
      case c.universe.EmptyTree => None
      case tree                 => Some(c.Expr[Any](tree).asInstanceOf[Expr[Any]])
    }
  }
}
