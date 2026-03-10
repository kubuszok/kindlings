package hearth.kindlings.catsderivation
package internal.compiletime

import hearth.MacroCommonsScala3
import scala.quoted.*

final private[catsderivation] class SemigroupKMacros(q: Quotes)
    extends MacroCommonsScala3(using q),
      SemigroupKMacrosImpl {

  def mkCtor1[G[_]](using scala.quoted.Type[G]): Type.Ctor1[G] = Type.Ctor1.of[G]

  def mkSemigroupKType[G[_]](using scala.quoted.Type[G]): Type[cats.SemigroupK[G]] =
    scala.quoted.Type.of[cats.SemigroupK[G]].asInstanceOf[Type[cats.SemigroupK[G]]]
}
private[catsderivation] object SemigroupKMacros {

  def deriveSemigroupKImpl[F[_]: Type](using q: Quotes): Expr[cats.SemigroupK[F]] = {
    val m = new SemigroupKMacros(q)
    m.deriveSemigroupK[F](m.mkCtor1[F], m.mkSemigroupKType[F])
  }
}
