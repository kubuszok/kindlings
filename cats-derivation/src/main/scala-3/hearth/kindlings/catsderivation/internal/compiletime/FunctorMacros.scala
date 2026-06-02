package hearth.kindlings.catsderivation
package internal.compiletime

import hearth.MacroCommonsScala3
import scala.quoted.*

final private[catsderivation] class FunctorMacros(q: Quotes) extends MacroCommonsScala3(using q), FunctorMacrosImpl {

  /** Create Type.Ctor1[G] — plugin rewrites Type.Ctor1.of[G] here because MacroCommons is in scope. */
  def mkCtor1[G[_]](using scala.quoted.Type[G]): Type.Ctor1[G] = Type.Ctor1.of[G]

  /** Create Type[cats.Functor[G]] from scala.quoted.Type. */
  def mkFunctorType[G[_]](using scala.quoted.Type[G]): Type[cats.Functor[G]] =
    scala.quoted.Type.of[cats.Functor[G]].asInstanceOf[Type[cats.Functor[G]]]

  protected def mkCtor1FromType(appliedType: Type[Any]): Option[(Type.Ctor1[AnyK], UntypedType)] = {
    import q.reflect.*
    val repr = TypeRepr.of(using appliedType.asInstanceOf[scala.quoted.Type[Any]])
    repr.dealias match {
      case AppliedType(fieldCtor, _ :: Nil) =>
        val untyped = fieldCtor.asInstanceOf[UntypedType]
        Some((Type.Ctor1.fromUntyped[List](untyped).asInstanceOf[Type.Ctor1[AnyK]], untyped))
      case _ => None
    }
  }

  protected def extractSingleTypeArg(appliedType: Type[Any]): Option[Type[Any]] = {
    import q.reflect.*
    val repr = TypeRepr.of(using appliedType.asInstanceOf[scala.quoted.Type[Any]])
    repr.dealias match {
      case AppliedType(_, arg :: Nil) =>
        Some(arg.asType match { case '[t] => scala.quoted.Type.of[t].asInstanceOf[Type[Any]] })
      case _ => None
    }
  }

  protected def isNestedSelfRecursive(fieldType: Type[Any], parentCtor: UntypedType): Boolean = {
    import q.reflect.*
    val repr = TypeRepr.of(using fieldType.asInstanceOf[scala.quoted.Type[Any]])
    repr.dealias match {
      case AppliedType(_, innerArg :: Nil) =>
        innerArg.dealias match {
          case AppliedType(innerCtor, _) =>
            // Compare type symbols for reliable cross-context equality
            innerCtor.typeSymbol == parentCtor.asInstanceOf[TypeRepr].typeSymbol
          case _ => false
        }
      case _ => false
    }
  }

  protected def summonFunctorForFieldType(fieldType: Type[Any]): Option[Expr[Any]] = {
    import q.reflect.*
    val repr = TypeRepr.of(using fieldType.asInstanceOf[scala.quoted.Type[Any]])
    repr.dealias match {
      case AppliedType(fieldCtor, _ :: Nil) =>
        val functorCtor = TypeRepr.of[cats.Functor[List]] match {
          case AppliedType(ctor, _) => ctor
          case _                    => return None
        }
        val functorGType = functorCtor.appliedTo(fieldCtor)
        Implicits.search(functorGType) match {
          case iss: ImplicitSearchSuccess => Some(iss.tree.asExpr.asInstanceOf[Expr[Any]])
          case _                          => None
        }
      case _ => None
    }
  }
}
private[catsderivation] object FunctorMacros {

  def deriveFunctorImpl[F[_]: Type](using q: Quotes): Expr[cats.Functor[F]] = {
    val m = new FunctorMacros(q)
    m.deriveFunctor[F](m.mkCtor1[F], m.mkFunctorType[F])
  }
}
