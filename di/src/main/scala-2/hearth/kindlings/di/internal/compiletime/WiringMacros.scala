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
    autowireWithMembers[A](dependencies.toList)

  def wiredInModuleImpl(in: c.Tree): c.Expr[Wired] = wiredInModule(c.Expr[Any](in))

  /** `DI.plan[A]....build` — the builder chain is the `plan` field of the enclosing `DIPlanBuildOps` value class,
    * recovered from `c.prefix` (`new DIPlanBuildOps(plan)`), then handed to the shared [[buildPlan]].
    */
  def buildPlanImpl[A: c.WeakTypeTag]: c.Expr[A] = {
    import c.universe.*
    val planTree: Tree = c.prefix.tree match {
      case Apply(_, List(inner)) => inner
      case other                 =>
        c.abort(
          c.enclosingPosition,
          s"`DI.plan(...).build` could not extract the builder chain from: ${showRaw(other)}"
        )
    }
    buildPlan[A](c.Expr[DIPlan[A]](planTree))
  }
}
