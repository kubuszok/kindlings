package hearth.kindlings.catsderivation
package internal.compiletime

import hearth.MacroCommonsScala3
import scala.quoted.*

final private[catsderivation] class EmptyKMacros(q: Quotes) extends MacroCommonsScala3(using q), EmptyKMacrosImpl {

  def mkCtor1[G[_]](using scala.quoted.Type[G]): Type.Ctor1[G] = Type.Ctor1.of[G]

  def mkEmptyKType[G[_]](using scala.quoted.Type[G]): Type[alleycats.EmptyK[G]] =
    scala.quoted.Type.of[alleycats.EmptyK[G]].asInstanceOf[Type[alleycats.EmptyK[G]]]
}
private[catsderivation] object EmptyKMacros {

  def deriveEmptyKImpl[F[_]: Type](using q: Quotes): Expr[alleycats.EmptyK[F]] = {
    val m = new EmptyKMacros(q)
    m.deriveEmptyK[F](m.mkCtor1[F], m.mkEmptyKType[F])
  }
}
