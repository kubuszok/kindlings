package hearth.kindlings.catsderivation
package internal.compiletime

import hearth.MacroCommonsScala3
import scala.quoted.*

final private[catsderivation] class InvariantMacros(q: Quotes)
    extends MacroCommonsScala3(using q),
      InvariantMacrosImpl {

  def mkCtor1[G[_]](using scala.quoted.Type[G]): Type.Ctor1[G] = Type.Ctor1.of[G]

  def mkInvariantType[G[_]](using scala.quoted.Type[G]): Type[cats.Invariant[G]] =
    scala.quoted.Type.of[cats.Invariant[G]].asInstanceOf[Type[cats.Invariant[G]]]
}
private[catsderivation] object InvariantMacros {

  def deriveInvariantImpl[F[_]: Type](using q: Quotes): Expr[cats.Invariant[F]] = {
    val m = new InvariantMacros(q)
    m.deriveInvariant[F](m.mkCtor1[F], m.mkInvariantType[F])
  }
}
