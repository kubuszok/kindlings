package hearth.kindlings.optics
package internal.compiletime

import hearth.MacroCommonsScala2
import scala.reflect.macros.blackbox

final private[optics] class ModifyMacros(val c: blackbox.Context) extends MacroCommonsScala2 with ModifyMacrosImpl {

  /** `obj.modify(path)` desugars to `new syntax.ModifyOps[S](obj).modify[A](path)`. The macro method only receives
    * `path` as a value argument; the wrapped `obj` is recovered from the implicit-class application in `c.prefix` (`new
    * ModifyOps(obj)`), mirroring the mock module's `dslOperands`.
    */
  def modifyImpl[S: c.WeakTypeTag, A: c.WeakTypeTag](path: c.Expr[S => A]): c.Expr[PathModify[S, A]] = {
    import c.universe.*
    val objTree: Tree = c.prefix.tree match {
      case Apply(_, List(inner)) => inner
      case other                 =>
        c.abort(c.enclosingPosition, s"`modify` could not extract the source object from: ${showRaw(other)}")
    }
    modify[S, A](c.Expr[S](objTree), path)
  }
}
