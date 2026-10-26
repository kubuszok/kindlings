package hearth.kindlings.catsderivation
package internal.compiletime

import hearth.MacroCommonsScala3
import scala.quoted.*

final private[catsderivation] class SemigroupKMacros(q: Quotes)
    extends MacroCommonsScala3(using q),
      SemigroupKMacrosImpl {

  /** Create Type.Ctor1[G] - plugin rewrites Type.Ctor1.of[G] here because MacroCommons is in scope. */
  def mkCtor1[G[_]](using scala.quoted.Type[G]): Type.Ctor1[G] = Type.Ctor1.of[G]
}
private[catsderivation] object SemigroupKMacros {

  def deriveSemigroupKImpl[F[_]: Type](using q: Quotes): Expr[cats.SemigroupK[F]] = {
    val m = new SemigroupKMacros(q)
    m.deriveSemigroupK[F](m.mkCtor1[F])
  }
}
