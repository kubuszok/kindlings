package hearth.kindlings.catsderivation
package internal.compiletime

import hearth.MacroCommonsScala3
import scala.quoted.*

final private[catsderivation] class BifoldableMacros(q: Quotes)
    extends MacroCommonsScala3(using q),
      BifoldableMacrosImpl {

  def mkCtor2[G[_, _]](using scala.quoted.Type[G]): Type.Ctor2[G] = Type.Ctor2.of[G]

  def mkBifoldableType[G[_, _]](using scala.quoted.Type[G]): Type[cats.Bifoldable[G]] =
    scala.quoted.Type.of[cats.Bifoldable[G]].asInstanceOf[Type[cats.Bifoldable[G]]]
}
private[catsderivation] object BifoldableMacros {

  def deriveBifoldableImpl[F[_, _]: Type](using q: Quotes): Expr[cats.Bifoldable[F]] = {
    val m = new BifoldableMacros(q)
    m.deriveBifoldable[F](m.mkCtor2[F], m.mkBifoldableType[F])
  }
}
