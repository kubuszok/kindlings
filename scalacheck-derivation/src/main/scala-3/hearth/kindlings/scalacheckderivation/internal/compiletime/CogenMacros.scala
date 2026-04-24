package hearth.kindlings.scalacheckderivation
package internal.compiletime

import hearth.MacroCommonsScala3
import scala.quoted.*

final private[scalacheckderivation] class CogenMacros(q: Quotes)
    extends MacroCommonsScala3(using q),
      LoadStandardExtensionsOnce,
      CogenMacrosImpl

private[scalacheckderivation] object CogenMacros {

  def deriveCogen[A: Type](using q: Quotes): Expr[org.scalacheck.Cogen[A]] =
    new CogenMacros(q).deriveCogen[A]
}
