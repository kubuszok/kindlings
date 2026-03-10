package hearth.kindlings.catsderivation
package internal.compiletime

import hearth.MacroCommonsScala3
import scala.quoted.*

final private[catsderivation] class FoldableMacros(q: Quotes) extends MacroCommonsScala3(using q), FoldableMacrosImpl {

  def mkCtor1[G[_]](using scala.quoted.Type[G]): Type.Ctor1[G] = Type.Ctor1.of[G]

  def mkFoldableType[G[_]](using scala.quoted.Type[G]): Type[cats.Foldable[G]] =
    scala.quoted.Type.of[cats.Foldable[G]].asInstanceOf[Type[cats.Foldable[G]]]
}
private[catsderivation] object FoldableMacros {

  def deriveFoldableImpl[F[_]: Type](using q: Quotes): Expr[cats.Foldable[F]] = {
    val m = new FoldableMacros(q)
    m.deriveFoldable[F](m.mkCtor1[F], m.mkFoldableType[F])
  }
}
