package hearth.kindlings.catsderivation
package internal.compiletime

import hearth.MacroCommonsScala3
import scala.quoted.*

final private[catsderivation] class TraverseMacros(q: Quotes) extends MacroCommonsScala3(using q), TraverseMacrosImpl {

  def mkCtor1[G[_]](using scala.quoted.Type[G]): Type.Ctor1[G] = Type.Ctor1.of[G]

  def mkTraverseType[G[_]](using scala.quoted.Type[G]): Type[cats.Traverse[G]] =
    scala.quoted.Type.of[cats.Traverse[G]].asInstanceOf[Type[cats.Traverse[G]]]
}
private[catsderivation] object TraverseMacros {

  def deriveTraverseImpl[F[_]: Type](using q: Quotes): Expr[cats.Traverse[F]] = {
    val m = new TraverseMacros(q)
    m.deriveTraverse[F](m.mkCtor1[F], m.mkTraverseType[F])
  }
}
