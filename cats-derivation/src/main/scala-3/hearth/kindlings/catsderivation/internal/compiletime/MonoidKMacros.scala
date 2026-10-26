package hearth.kindlings.catsderivation
package internal.compiletime

import hearth.MacroCommonsScala3
import scala.quoted.*

final private[catsderivation] class MonoidKMacros(q: Quotes) extends MacroCommonsScala3(using q), MonoidKMacrosImpl {

  /** Create Type.Ctor1[G] - plugin rewrites Type.Ctor1.of[G] here because MacroCommons is in scope. */
  def mkCtor1[G[_]](using scala.quoted.Type[G]): Type.Ctor1[G] = Type.Ctor1.of[G]
}
private[catsderivation] object MonoidKMacros {

  def deriveMonoidKImpl[F[_]: Type](using q: Quotes): Expr[cats.MonoidK[F]] = {
    val m = new MonoidKMacros(q)
    m.deriveMonoidK[F](m.mkCtor1[F])
  }
}
