package hearth.kindlings.catsderivation
package internal.compiletime

import hearth.MacroCommonsScala3
import scala.quoted.*

final private[catsderivation] class BitraverseMacros(q: Quotes)
    extends MacroCommonsScala3(using q),
      BitraverseMacrosImpl {

  /** Create Type.Ctor2[G] - plugin rewrites Type.Ctor2.of[G] here because MacroCommons is in scope. */
  def mkCtor2[G[_, _]](using scala.quoted.Type[G]): Type.Ctor2[G] = Type.Ctor2.of[G]
}
private[catsderivation] object BitraverseMacros {

  def deriveBitraverseImpl[F[_, _]: Type](using q: Quotes): Expr[cats.Bitraverse[F]] = {
    val m = new BitraverseMacros(q)
    m.deriveBitraverse[F](m.mkCtor2[F])
  }
}
