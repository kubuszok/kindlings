package hearth.kindlings.catsderivation
package internal.compiletime

import hearth.MacroCommonsScala3
import scala.quoted.*

final private[catsderivation] class BifunctorMacros(q: Quotes)
    extends MacroCommonsScala3(using q),
      BifunctorMacrosImpl {

  def mkCtor2[G[_, _]](using scala.quoted.Type[G]): Type.Ctor2[G] = Type.Ctor2.of[G]

  def mkBifunctorType[G[_, _]](using scala.quoted.Type[G]): Type[cats.Bifunctor[G]] =
    scala.quoted.Type.of[cats.Bifunctor[G]].asInstanceOf[Type[cats.Bifunctor[G]]]
}
private[catsderivation] object BifunctorMacros {

  def deriveBifunctorImpl[F[_, _]: Type](using q: Quotes): Expr[cats.Bifunctor[F]] = {
    val m = new BifunctorMacros(q)
    m.deriveBifunctor[F](m.mkCtor2[F], m.mkBifunctorType[F])
  }
}
