package hearth.kindlings.catsderivation
package internal.compiletime

import hearth.MacroCommonsScala3
import scala.quoted.*

final private[catsderivation] class MonoidKMacros(q: Quotes) extends MacroCommonsScala3(using q), MonoidKMacrosImpl {

  def mkCtor1[G[_]](using scala.quoted.Type[G]): Type.Ctor1[G] = Type.Ctor1.of[G]

  def mkMonoidKType[G[_]](using scala.quoted.Type[G]): Type[cats.MonoidK[G]] =
    scala.quoted.Type.of[cats.MonoidK[G]].asInstanceOf[Type[cats.MonoidK[G]]]
}
private[catsderivation] object MonoidKMacros {

  def deriveMonoidKImpl[F[_]: Type](using q: Quotes): Expr[cats.MonoidK[F]] = {
    val m = new MonoidKMacros(q)
    m.deriveMonoidK[F](m.mkCtor1[F], m.mkMonoidKType[F])
  }
}
