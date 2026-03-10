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
}
private[catsderivation] object FunctorMacros {

  def deriveFunctorImpl[F[_]: Type](using q: Quotes): Expr[cats.Functor[F]] = {
    val m = new FunctorMacros(q)
    m.deriveFunctor[F](m.mkCtor1[F], m.mkFunctorType[F])
  }
}
