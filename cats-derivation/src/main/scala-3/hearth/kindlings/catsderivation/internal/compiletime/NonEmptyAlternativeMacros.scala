package hearth.kindlings.catsderivation
package internal.compiletime

import hearth.MacroCommonsScala3
import scala.quoted.*

final private[catsderivation] class NonEmptyAlternativeMacros(q: Quotes)
    extends MacroCommonsScala3(using q),
      NonEmptyAlternativeMacrosImpl {

  def mkCtor1[G[_]](using scala.quoted.Type[G]): Type.Ctor1[G] = Type.Ctor1.of[G]

  def mkNEAType[G[_]](using scala.quoted.Type[G]): Type[cats.NonEmptyAlternative[G]] =
    scala.quoted.Type.of[cats.NonEmptyAlternative[G]].asInstanceOf[Type[cats.NonEmptyAlternative[G]]]
}
private[catsderivation] object NonEmptyAlternativeMacros {

  def deriveNonEmptyAlternativeImpl[F[_]: Type](using q: Quotes): Expr[cats.NonEmptyAlternative[F]] = {
    val m = new NonEmptyAlternativeMacros(q)
    m.deriveNonEmptyAlternative[F](m.mkCtor1[F], m.mkNEAType[F])
  }
}
