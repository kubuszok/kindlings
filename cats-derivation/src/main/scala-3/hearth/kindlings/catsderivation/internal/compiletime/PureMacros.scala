package hearth.kindlings.catsderivation
package internal.compiletime

import hearth.MacroCommonsScala3
import scala.quoted.*

final private[catsderivation] class PureMacros(q: Quotes) extends MacroCommonsScala3(using q), PureMacrosImpl {

  def mkCtor1[G[_]](using scala.quoted.Type[G]): Type.Ctor1[G] = Type.Ctor1.of[G]

  def mkPureType[G[_]](using scala.quoted.Type[G]): Type[alleycats.Pure[G]] =
    scala.quoted.Type.of[alleycats.Pure[G]].asInstanceOf[Type[alleycats.Pure[G]]]
}
private[catsderivation] object PureMacros {

  def derivePureImpl[F[_]: Type](using q: Quotes): Expr[alleycats.Pure[F]] = {
    val m = new PureMacros(q)
    m.derivePure[F](m.mkCtor1[F], m.mkPureType[F])
  }
}
