package hearth.kindlings.di
package internal.compiletime

import hearth.MacroCommonsScala2
import scala.reflect.macros.blackbox

final private[di] class WiringMacros(val c: blackbox.Context) extends MacroCommonsScala2 with WiringMacrosImpl {

  def wireImpl[A: c.WeakTypeTag]: c.Expr[A] = wire[A]

  def wireRecImpl[A: c.WeakTypeTag]: c.Expr[A] = wireRec[A]

  def wireSetImpl[A: c.WeakTypeTag]: c.Expr[Set[A]] = wireSet[A]

  def wireListImpl[A: c.WeakTypeTag]: c.Expr[List[A]] = wireList[A]

  def wireWithImpl[RES: c.WeakTypeTag](factory: c.Tree): c.Expr[RES] = wireWith[RES](c.Expr[Any](factory))

  def autowireImpl[A: c.WeakTypeTag](dependencies: c.Expr[Any]*): c.Expr[A] =
    autowire[A](dependencies.toList.map(preciseExpr))
}
