package hearth.kindlings.catsderivation
package internal.compiletime

import hearth.MacroCommonsScala3
import scala.quoted.*

final private[catsderivation] class BifunctorMacros(q: Quotes)
    extends MacroCommonsScala3(using q),
      BifunctorMacrosImpl {

  /** Create Type.Ctor2[G] - plugin rewrites Type.Ctor2.of[G] here because MacroCommons is in scope. */
  def mkCtor2[G[_, _]](using scala.quoted.Type[G]): Type.Ctor2[G] = Type.Ctor2.of[G]
}
private[catsderivation] object BifunctorMacros {

  def deriveBifunctorImpl[F[_, _]: Type](using q: Quotes): Expr[cats.Bifunctor[F]] = {
    val m = new BifunctorMacros(q)
    m.deriveBifunctor[F](m.mkCtor2[F])
  }
}
