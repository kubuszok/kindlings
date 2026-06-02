package hearth.kindlings.catsderivation
package internal.compiletime

import hearth.MacroCommonsScala3
import scala.quoted.*

final private[catsderivation] class TraverseMacros(q: Quotes) extends MacroCommonsScala3(using q), TraverseMacrosImpl {

  def mkCtor1[G[_]](using scala.quoted.Type[G]): Type.Ctor1[G] = Type.Ctor1.of[G]

  def mkTraverseType[G[_]](using scala.quoted.Type[G]): Type[cats.Traverse[G]] =
    scala.quoted.Type.of[cats.Traverse[G]].asInstanceOf[Type[cats.Traverse[G]]]

  protected def mkCtor1FromTypeTraverse(appliedType: Type[Any]): Option[UntypedType] = {
    import q.reflect.*
    val repr = TypeRepr.of(using appliedType.asInstanceOf[scala.quoted.Type[Any]])
    repr.dealias match {
      case AppliedType(fieldCtor, _ :: Nil) => Some(fieldCtor.asInstanceOf[UntypedType])
      case _                                => None
    }
  }

  protected def summonTraverseForFieldType(fieldType: Type[Any]): Option[Expr[Any]] = {
    import q.reflect.*
    val repr = TypeRepr.of(using fieldType.asInstanceOf[scala.quoted.Type[Any]])
    repr.dealias match {
      case AppliedType(fieldCtor, _ :: Nil) =>
        val traverseCtor = TypeRepr.of[cats.Traverse[List]] match {
          case AppliedType(ctor, _) => ctor
          case _                    => return None
        }
        val traverseGType = traverseCtor.appliedTo(fieldCtor)
        Implicits.search(traverseGType) match {
          case iss: ImplicitSearchSuccess => Some(iss.tree.asExpr.asInstanceOf[Expr[Any]])
          case _                          => None
        }
      case _ => None
    }
  }
}
private[catsderivation] object TraverseMacros {

  def deriveTraverseImpl[F[_]: Type](using q: Quotes): Expr[cats.Traverse[F]] = {
    val m = new TraverseMacros(q)
    m.deriveTraverse[F](m.mkCtor1[F], m.mkTraverseType[F])
  }
}
