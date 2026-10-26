package hearth.kindlings.catsderivation
package internal.compiletime

import hearth.MacroCommonsScala3
import scala.quoted.*

final private[catsderivation] class EmptyKMacros(q: Quotes) extends MacroCommonsScala3(using q), EmptyKMacrosImpl {

  /** Create Type.Ctor1[G] - plugin rewrites Type.Ctor1.of[G] here because MacroCommons is in scope. */
  def mkCtor1[G[_]](using scala.quoted.Type[G]): Type.Ctor1[G] = Type.Ctor1.of[G]
}
private[catsderivation] object EmptyKMacros {

  def deriveEmptyKImpl[F[_]: Type](using q: Quotes): Expr[alleycats.EmptyK[F]] = {
    val m = new EmptyKMacros(q)
    m.deriveEmptyK[F](m.mkCtor1[F])
  }
}
