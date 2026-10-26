package hearth.kindlings.catsderivation
package internal.compiletime

import hearth.MacroCommonsScala3
import scala.quoted.*

final private[catsderivation] class BifoldableMacros(q: Quotes)
    extends MacroCommonsScala3(using q),
      BifoldableMacrosImpl {

  /** Create Type.Ctor2[G] - plugin rewrites Type.Ctor2.of[G] here because MacroCommons is in scope. */
  def mkCtor2[G[_, _]](using scala.quoted.Type[G]): Type.Ctor2[G] = Type.Ctor2.of[G]
}
private[catsderivation] object BifoldableMacros {

  def deriveBifoldableImpl[F[_, _]: Type](using q: Quotes): Expr[cats.Bifoldable[F]] = {
    val m = new BifoldableMacros(q)
    m.deriveBifoldable[F](m.mkCtor2[F])
  }
}
