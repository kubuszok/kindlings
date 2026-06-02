package hearth.kindlings.catsderivation
package internal.compiletime

import hearth.MacroCommonsScala3
import scala.quoted.*

final private[catsderivation] class FoldableMacros(q: Quotes) extends MacroCommonsScala3(using q), FoldableMacrosImpl {

  def mkCtor1[G[_]](using scala.quoted.Type[G]): Type.Ctor1[G] = Type.Ctor1.of[G]

  def mkFoldableType[G[_]](using scala.quoted.Type[G]): Type[cats.Foldable[G]] =
    scala.quoted.Type.of[cats.Foldable[G]].asInstanceOf[Type[cats.Foldable[G]]]

  protected def mkCtor1FromTypeFoldable(appliedType: Type[Any]): Option[UntypedType] = {
    import q.reflect.*
    val repr = TypeRepr.of(using appliedType.asInstanceOf[scala.quoted.Type[Any]])
    repr.dealias match {
      case AppliedType(fieldCtor, _ :: Nil) => Some(fieldCtor.asInstanceOf[UntypedType])
      case _                                => None
    }
  }

  protected def summonFoldableForFieldType(fieldType: Type[Any]): Option[Expr[Any]] = {
    import q.reflect.*
    val repr = TypeRepr.of(using fieldType.asInstanceOf[scala.quoted.Type[Any]])
    repr.dealias match {
      case AppliedType(fieldCtor, _ :: Nil) =>
        val foldableCtor = TypeRepr.of[cats.Foldable[List]] match {
          case AppliedType(ctor, _) => ctor
          case _                    => return None
        }
        val foldableGType = foldableCtor.appliedTo(fieldCtor)
        Implicits.search(foldableGType) match {
          case iss: ImplicitSearchSuccess => Some(iss.tree.asExpr.asInstanceOf[Expr[Any]])
          case _                          => None
        }
      case _ => None
    }
  }
}
private[catsderivation] object FoldableMacros {

  def deriveFoldableImpl[F[_]: Type](using q: Quotes): Expr[cats.Foldable[F]] = {
    val m = new FoldableMacros(q)
    m.deriveFoldable[F](m.mkCtor1[F], m.mkFoldableType[F])
  }
}
