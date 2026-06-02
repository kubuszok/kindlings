package hearth.kindlings.catsderivation
package internal.compiletime

import hearth.MacroCommonsScala2
import scala.reflect.macros.blackbox

final private[catsderivation] class FunctorMacros(val c: blackbox.Context)
    extends MacroCommonsScala2
    with FunctorMacrosImpl {

  def deriveFunctorImpl[F[_]](implicit ft: c.WeakTypeTag[F[Any]]): c.Expr[cats.Functor[F]] = {
    // Use fromUntyped to create the Ctor1 at expansion time from the concrete type constructor.
    val untypedF: UntypedType = ft.tpe.typeConstructor
    val fCtor: Type.Ctor1[F] = Type.Ctor1.fromUntyped[F](untypedF)

    val functorCtor = c.universe.weakTypeOf[cats.Functor[Any]].typeConstructor
    val functorFTpe = c.universe.appliedType(functorCtor, List(ft.tpe.typeConstructor))
    val functorFType = c.WeakTypeTag[cats.Functor[F]](functorFTpe).asInstanceOf[Type[cats.Functor[F]]]

    deriveFunctor[F](fCtor, functorFType)
  }

  protected def extractSingleTypeArg(appliedType: Type[Any]): Option[Type[Any]] = {
    val tpe = appliedType.asInstanceOf[c.WeakTypeTag[Any]].tpe
    tpe.dealias.typeArgs match {
      case arg :: Nil => Some(c.WeakTypeTag[Any](arg).asInstanceOf[Type[Any]])
      case _          => None
    }
  }

  protected def isNestedSelfRecursive(fieldType: Type[Any], parentCtor: UntypedType): Boolean = {
    val tpe = fieldType.asInstanceOf[c.WeakTypeTag[Any]].tpe
    val dealiased = tpe.dealias
    dealiased.typeArgs match {
      case innerArg :: Nil =>
        val innerCtor = innerArg.typeConstructor
        // Compare type symbols for reliable equality
        innerCtor.typeSymbol == parentCtor.asInstanceOf[c.Type].typeSymbol
      case _ => false
    }
  }

  protected def mkCtor1FromType(appliedType: Type[Any]): Option[(Type.Ctor1[AnyK], UntypedType)] = {
    val tpe = appliedType.asInstanceOf[c.WeakTypeTag[Any]].tpe
    val ctor = tpe.typeConstructor
    if (ctor == tpe) None
    else {
      val untyped = ctor.asInstanceOf[UntypedType]
      Some((Type.Ctor1.fromUntyped[List](untyped).asInstanceOf[Type.Ctor1[AnyK]], untyped))
    }
  }

  protected def summonFunctorForFieldType(fieldType: Type[Any]): Option[Expr[Any]] = {
    val tpe = fieldType.asInstanceOf[c.WeakTypeTag[Any]].tpe
    val ctor = tpe.typeConstructor
    val functorCtor = c.universe.weakTypeOf[cats.Functor[List]].typeConstructor
    val functorGType = c.universe.appliedType(functorCtor, List(ctor))
    c.inferImplicitValue(functorGType) match {
      case c.universe.EmptyTree => None
      case tree                 => Some(c.Expr[Any](tree).asInstanceOf[Expr[Any]])
    }
  }
}
