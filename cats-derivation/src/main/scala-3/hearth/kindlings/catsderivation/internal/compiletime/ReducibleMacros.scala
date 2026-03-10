package hearth.kindlings.catsderivation
package internal.compiletime

import hearth.MacroCommonsScala3
import scala.quoted.*

final private[catsderivation] class ReducibleMacros(q: Quotes)
    extends MacroCommonsScala3(using q),
      ReducibleMacrosImpl {

  def mkCtor1[G[_]](using scala.quoted.Type[G]): Type.Ctor1[G] = Type.Ctor1.of[G]

  def mkReducibleType[G[_]](using scala.quoted.Type[G]): Type[cats.Reducible[G]] =
    scala.quoted.Type.of[cats.Reducible[G]].asInstanceOf[Type[cats.Reducible[G]]]
}
private[catsderivation] object ReducibleMacros {

  def deriveReducibleImpl[F[_]: Type](using q: Quotes): Expr[cats.Reducible[F]] = {
    val m = new ReducibleMacros(q)
    m.deriveReducible[F](m.mkCtor1[F], m.mkReducibleType[F])
  }
}
