package hearth.kindlings.catsderivation
package internal.compiletime

import hearth.MacroCommonsScala3
import scala.quoted.*

final private[catsderivation] class ReducibleMacros(q: Quotes)
    extends MacroCommonsScala3(using q),
      ReducibleMacrosImpl {

  /** Create Type.Ctor1[G] - plugin rewrites Type.Ctor1.of[G] here because MacroCommons is in scope. */
  def mkCtor1[G[_]](using scala.quoted.Type[G]): Type.Ctor1[G] = Type.Ctor1.of[G]
}
private[catsderivation] object ReducibleMacros {

  def deriveReducibleImpl[F[_]: Type](using q: Quotes): Expr[cats.Reducible[F]] = {
    val m = new ReducibleMacros(q)
    m.deriveReducible[F](m.mkCtor1[F])
  }
}
