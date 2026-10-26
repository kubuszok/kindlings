package hearth.kindlings.catsderivation
package internal.compiletime

import hearth.MacroCommonsScala3
import scala.quoted.*

final private[catsderivation] class ContravariantMacros(q: Quotes)
    extends MacroCommonsScala3(using q),
      ContravariantMacrosImpl {

  /** Create Type.Ctor1[G] - plugin rewrites Type.Ctor1.of[G] here because MacroCommons is in scope. */
  def mkCtor1[G[_]](using scala.quoted.Type[G]): Type.Ctor1[G] = Type.Ctor1.of[G]
}
private[catsderivation] object ContravariantMacros {

  def deriveContravariantImpl[F[_]: Type](using q: Quotes): Expr[cats.Contravariant[F]] = {
    val m = new ContravariantMacros(q)
    m.deriveContravariant[F](m.mkCtor1[F])
  }
}
