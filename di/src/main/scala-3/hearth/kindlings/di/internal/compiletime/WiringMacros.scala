package hearth.kindlings.di
package internal.compiletime

import hearth.MacroCommonsScala3
import scala.quoted.*

final private[di] class WiringMacros(q: Quotes) extends MacroCommonsScala3(using q), WiringMacrosImpl

private[di] object WiringMacros {

  def wireImpl[A: Type](using q: Quotes): Expr[A] = new WiringMacros(q).wire[A]

  def wireRecImpl[A: Type](using q: Quotes): Expr[A] = new WiringMacros(q).wireRec[A]

  def wireSetImpl[A: Type](using q: Quotes): Expr[Set[A]] = new WiringMacros(q).wireSet[A]

  def wireListImpl[A: Type](using q: Quotes): Expr[List[A]] = new WiringMacros(q).wireList[A]

  def wireWithImpl[RES: Type](factory: Expr[Any])(using q: Quotes): Expr[RES] =
    new WiringMacros(q).wireWith[RES](factory)

  def autowireImpl[A: Type](dependencies: Expr[Seq[Any]])(using q: Quotes): Expr[A] = {
    val m = new WiringMacros(q)
    dependencies match {
      case Varargs(es) => m.autowire[A](es.toList.map(m.preciseExpr))
      case _           =>
        q.reflect.report.errorAndAbort("autowire dependencies must be provided directly as varargs parameters.")
    }
  }
}
