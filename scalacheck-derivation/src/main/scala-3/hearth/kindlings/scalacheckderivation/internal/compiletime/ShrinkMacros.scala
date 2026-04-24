package hearth.kindlings.scalacheckderivation
package internal.compiletime

import hearth.MacroCommonsScala3
import scala.quoted.*

final private[scalacheckderivation] class ShrinkMacros(q: Quotes)
    extends MacroCommonsScala3(using q),
      LoadStandardExtensionsOnce,
      ShrinkMacrosImpl

private[scalacheckderivation] object ShrinkMacros {

  def deriveShrink[A: Type](using q: Quotes): Expr[org.scalacheck.Shrink[A]] =
    new ShrinkMacros(q).deriveShrink[A]
}
