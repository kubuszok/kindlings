package hearth.kindlings.catsderivation
package internal.compiletime

import hearth.MacroCommonsScala3
import scala.quoted.*

final private[catsderivation] class ApplyMacros(q: Quotes) extends MacroCommonsScala3(using q), ApplyMacrosImpl {

  def mkCtor1[G[_]](using scala.quoted.Type[G]): Type.Ctor1[G] = Type.Ctor1.of[G]

  def mkApplyType[G[_]](using scala.quoted.Type[G]): Type[cats.Apply[G]] =
    scala.quoted.Type.of[cats.Apply[G]].asInstanceOf[Type[cats.Apply[G]]]
}
private[catsderivation] object ApplyMacros {

  def deriveApplyImpl[F[_]: Type](using q: Quotes): Expr[cats.Apply[F]] = {
    val m = new ApplyMacros(q)
    m.deriveApply[F](m.mkCtor1[F], m.mkApplyType[F])
  }
}
