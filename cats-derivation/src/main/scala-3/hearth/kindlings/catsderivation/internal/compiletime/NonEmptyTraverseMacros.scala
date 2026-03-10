package hearth.kindlings.catsderivation
package internal.compiletime

import hearth.MacroCommonsScala3
import scala.quoted.*

final private[catsderivation] class NonEmptyTraverseMacros(q: Quotes)
    extends MacroCommonsScala3(using q),
      NonEmptyTraverseMacrosImpl {

  def mkCtor1[G[_]](using scala.quoted.Type[G]): Type.Ctor1[G] = Type.Ctor1.of[G]

  def mkNETType[G[_]](using scala.quoted.Type[G]): Type[cats.NonEmptyTraverse[G]] =
    scala.quoted.Type.of[cats.NonEmptyTraverse[G]].asInstanceOf[Type[cats.NonEmptyTraverse[G]]]
}
private[catsderivation] object NonEmptyTraverseMacros {

  def deriveNonEmptyTraverseImpl[F[_]: Type](using q: Quotes): Expr[cats.NonEmptyTraverse[F]] = {
    val m = new NonEmptyTraverseMacros(q)
    m.deriveNonEmptyTraverse[F](m.mkCtor1[F], m.mkNETType[F])
  }
}
