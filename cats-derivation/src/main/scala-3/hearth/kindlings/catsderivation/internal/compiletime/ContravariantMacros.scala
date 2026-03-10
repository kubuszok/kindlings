package hearth.kindlings.catsderivation
package internal.compiletime

import hearth.MacroCommonsScala3
import scala.quoted.*

final private[catsderivation] class ContravariantMacros(q: Quotes)
    extends MacroCommonsScala3(using q),
      ContravariantMacrosImpl {

  def mkCtor1[G[_]](using scala.quoted.Type[G]): Type.Ctor1[G] = Type.Ctor1.of[G]

  def mkContravariantType[G[_]](using scala.quoted.Type[G]): Type[cats.Contravariant[G]] =
    scala.quoted.Type.of[cats.Contravariant[G]].asInstanceOf[Type[cats.Contravariant[G]]]
}
private[catsderivation] object ContravariantMacros {

  def deriveContravariantImpl[F[_]: Type](using q: Quotes): Expr[cats.Contravariant[F]] = {
    val m = new ContravariantMacros(q)
    m.deriveContravariant[F](m.mkCtor1[F], m.mkContravariantType[F])
  }
}
