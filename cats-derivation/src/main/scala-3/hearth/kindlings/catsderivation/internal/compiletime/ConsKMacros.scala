package hearth.kindlings.catsderivation
package internal.compiletime

import hearth.MacroCommonsScala3
import scala.quoted.*

final private[catsderivation] class ConsKMacros(q: Quotes) extends MacroCommonsScala3(using q), ConsKMacrosImpl {

  /** Create Type.Ctor1[G] — plugin rewrites Type.Ctor1.of[G] here because MacroCommons is in scope. */
  def mkCtor1[G[_]](using scala.quoted.Type[G]): Type.Ctor1[G] = Type.Ctor1.of[G]

  /** Create Type[alleycats.ConsK[G]] from scala.quoted.Type. */
  def mkConsKType[G[_]](using scala.quoted.Type[G]): Type[alleycats.ConsK[G]] =
    scala.quoted.Type.of[alleycats.ConsK[G]].asInstanceOf[Type[alleycats.ConsK[G]]]
}
private[catsderivation] object ConsKMacros {

  def deriveConsKImpl[F[_]: Type](using q: Quotes): Expr[alleycats.ConsK[F]] = {
    val m = new ConsKMacros(q)
    m.deriveConsK[F](m.mkCtor1[F], m.mkConsKType[F])
  }
}
