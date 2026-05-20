package hearth.kindlings.catsderivation
package internal.compiletime

import hearth.MacroCommonsScala3
import scala.quoted.*

final private[catsderivation] class GroupMacros(q: Quotes) extends MacroCommonsScala3(using q), GroupMacrosImpl
private[catsderivation] object GroupMacros {

  def deriveGroupImpl[A: Type](using q: Quotes): Expr[cats.kernel.Group[A]] =
    new GroupMacros(q).deriveGroup[A]
}
