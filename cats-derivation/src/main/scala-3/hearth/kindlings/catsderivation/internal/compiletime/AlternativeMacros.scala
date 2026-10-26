package hearth.kindlings.catsderivation
package internal.compiletime

import hearth.MacroCommonsScala3
import scala.quoted.*

final private[catsderivation] class AlternativeMacros(q: Quotes)
    extends MacroCommonsScala3(using q),
      AlternativeMacrosImpl {

  /** Create Type.Ctor1[G] - plugin rewrites Type.Ctor1.of[G] here because MacroCommons is in scope. */
  def mkCtor1[G[_]](using scala.quoted.Type[G]): Type.Ctor1[G] = Type.Ctor1.of[G]
}
private[catsderivation] object AlternativeMacros {

  def deriveAlternativeImpl[F[_]: Type](using q: Quotes): Expr[cats.Alternative[F]] = {
    val m = new AlternativeMacros(q)
    m.deriveAlternative[F](m.mkCtor1[F])
  }
}
