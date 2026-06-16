package hearth.kindlings.optics
package internal.compiletime

import hearth.MacroCommonsScala3
import scala.quoted.*

final private[optics] class ModifyMacros(q: Quotes) extends MacroCommonsScala3(using q), ModifyMacrosImpl

private[optics] object ModifyMacros {

  def modifyImpl[S: Type, A: Type](obj: Expr[S], path: Expr[S => A])(using q: Quotes): Expr[PathModify[S, A]] =
    new ModifyMacros(q).modify[S, A](obj, path)
}
