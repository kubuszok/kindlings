package hearth.kindlings.optics
package internal.compiletime

import hearth.MacroCommonsScala2
import scala.reflect.macros.blackbox

final private[optics] class ModifyMacros(val c: blackbox.Context) extends MacroCommonsScala2 with ModifyMacrosImpl {

  /** `obj.modify(path)` desugars to `new syntax.ModifyOps[S](obj).modify[A](path)`. The macro method only receives
    * `path` as a value argument; the wrapped `obj` is recovered from the implicit-class application in `c.prefix` (`new
    * ModifyOps(obj)`), mirroring the mock module's `dslOperands`.
    */
  def modifyImpl[S: c.WeakTypeTag, A: c.WeakTypeTag](path: c.Expr[S => A]): c.Expr[PathModify[S, A]] =
    modify[S, A](objFromPrefix[S], path)

  /** `obj.modifyAll(path, paths*)` — the wrapped `obj` is recovered from the implicit-class application in `c.prefix`.
    */
  def modifyAllImpl[S: c.WeakTypeTag, A: c.WeakTypeTag](
      path: c.Expr[S => A],
      paths: c.Expr[S => A]*
  ): c.Expr[PathModify[S, A]] =
    modifyAll[S, A](objFromPrefix[S], (path +: paths).toList)

  /** `modifyLens[T](path)` — no object to recover; `c.prefix` is the `ModifyLensFactory[T]`. */
  def modifyLensImpl[T: c.WeakTypeTag, Foc: c.WeakTypeTag](path: c.Expr[T => Foc]): c.Expr[PathLazyModify[T, Foc]] =
    modifyLens[T, Foc](path)

  /** `modifyAllLens[T](path, paths*)` — no object to recover. */
  def modifyAllLensImpl[T: c.WeakTypeTag, Foc: c.WeakTypeTag](
      path: c.Expr[T => Foc],
      paths: c.Expr[T => Foc]*
  ): c.Expr[PathLazyModify[T, Foc]] =
    modifyAllLens[T, Foc]((path +: paths).toList)

  /** Recover the wrapped object from `new ModifyOps(obj)` in `c.prefix`. */
  private def objFromPrefix[S: c.WeakTypeTag]: c.Expr[S] = {
    import c.universe.*
    val objTree: Tree = c.prefix.tree match {
      case Apply(_, List(inner)) => inner
      case other                 =>
        c.abort(c.enclosingPosition, s"`modify` could not extract the source object from: ${showRaw(other)}")
    }
    c.Expr[S](objTree)
  }
}
