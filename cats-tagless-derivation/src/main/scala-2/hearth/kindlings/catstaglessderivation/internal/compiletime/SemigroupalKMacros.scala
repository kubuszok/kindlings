package hearth.kindlings.catstaglessderivation
package internal.compiletime

import hearth.MacroCommonsScala2
import scala.reflect.macros.blackbox

final private[catstaglessderivation] class SemigroupalKMacros(val c: blackbox.Context)
    extends MacroCommonsScala2
    with SemigroupalKMacrosImpl {

  // Entry-point glue only: a Scala 2 macro receives the algebra `Alg[_[_]]` as a `WeakTypeTag[Alg[Option]]`;
  // `Type.CtorK1.of[Alg]` reads the constructor from that implicit tag.
  // Everything else (type-class types, nested fields) is shared code.
  def deriveSemigroupalKImpl[Alg[_[_]]](implicit
      at: c.WeakTypeTag[Alg[Option]]
  ): c.Expr[cats.tagless.SemigroupalK[Alg]] =
    deriveSemigroupalK[Alg](Type.CtorK1.of[Alg])
}
