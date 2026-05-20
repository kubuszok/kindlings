package hearth.kindlings.catsderivation
package internal.compiletime

import hearth.MacroCommonsScala3
import scala.quoted.*

final private[catsderivation] class BitraverseMacros(q: Quotes)
    extends MacroCommonsScala3(using q),
      BitraverseMacrosImpl {

  def mkCtor2[G[_, _]](using scala.quoted.Type[G]): Type.Ctor2[G] = Type.Ctor2.of[G]

  def mkBitraverseType[G[_, _]](using scala.quoted.Type[G]): Type[cats.Bitraverse[G]] =
    scala.quoted.Type.of[cats.Bitraverse[G]].asInstanceOf[Type[cats.Bitraverse[G]]]
}
private[catsderivation] object BitraverseMacros {

  def deriveBitraverseImpl[F[_, _]: Type](using q: Quotes): Expr[cats.Bitraverse[F]] = {
    val m = new BitraverseMacros(q)
    m.deriveBitraverse[F](m.mkCtor2[F], m.mkBitraverseType[F])
  }
}
