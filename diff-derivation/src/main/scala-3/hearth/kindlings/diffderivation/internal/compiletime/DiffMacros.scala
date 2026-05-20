package hearth.kindlings.diffderivation
package internal.compiletime

import hearth.MacroCommonsScala3
import scala.quoted.*

final private[diffderivation] class DiffMacros(q: Quotes)
    extends MacroCommonsScala3(using q),
      DiffMacrosImpl

private[diffderivation] object DiffMacros {

  def deriveTypeClassImpl[A: Type](using q: Quotes): Expr[Diff[A]] =
    new DiffMacros(q).deriveTypeClass[A]
}
