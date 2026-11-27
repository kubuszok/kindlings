package hearth.kindlings.optics
package internal.compiletime

import hearth.MacroCommonsScala2
import hearth.std.StdExtensions
import scala.reflect.macros.whitebox

/** A **whitebox** bundle (`val c: whitebox.Context`): `modify`/`modifyAll`/... still return their declared types (so
  * they behave exactly like blackbox macros), but `isElementOfImpl` must return a type MORE SPECIFIC than its signature
  * (`IsElementOf.Aux[C, Elem]` from `IsElementOf[C]`) to materialize the `.each` evidence — only whitebox macros may do
  * that. `whitebox.Context <: blackbox.Context`, so Hearth's `MacroCommonsScala2` (which only needs `blackbox.Context`)
  * is satisfied; Hearth is no different from any other Scala 2 macro here.
  */
final private[optics] class ModifyMacros(val c: whitebox.Context)
    extends MacroCommonsScala2
    with StdExtensions
    with ModifyMacrosImpl {

  /** Materializer for the `.each` evidence (`IsElementOf`), backed by the std SPI; whitebox so the refined `Elem`
    * reaches the call site.
    */
  def isElementOfImpl[C: c.WeakTypeTag]: c.Expr[IsElementOf[C]] = deriveIsElementOf[C]
  def isIndexedElementOfImpl[C: c.WeakTypeTag]: c.Expr[IsIndexedElementOf[C]] = deriveIsIndexedElementOf[C]
  def isSingleElementOfImpl[C: c.WeakTypeTag]: c.Expr[IsSingleElementOf[C]] = deriveIsSingleElementOf[C]
  def isEitherImpl[C: c.WeakTypeTag]: c.Expr[_root_.hearth.kindlings.optics.IsEither[C]] = deriveIsEither[C]

  /** `obj.modify(path)` desugars to `new syntax.ModifyOps[S](obj).modify[A](path)`. The macro method only receives
    * `path` as a value argument; the wrapped `obj` is recovered from the implicit-class application in `c.prefix` (`new
    * ModifyOps(obj)`), mirroring the mock module's `dslOperands`.
    */
  def modifyImpl[S: c.WeakTypeTag, A: c.WeakTypeTag](path: c.Expr[S => A]): c.Expr[PathModify[S, A]] =
    modify[S, A, S => A](objFromPrefix[S], detached(path))

  /** `obj.modifyAll(path, paths*)` — the wrapped `obj` is recovered from the implicit-class application in `c.prefix`.
    */
  def modifyAllImpl[S: c.WeakTypeTag, A: c.WeakTypeTag](
      path: c.Expr[S => A],
      paths: c.Expr[S => A]*
  ): c.Expr[PathModify[S, A]] =
    modifyAll[S, A, S => A](objFromPrefix[S], detached(path), paths.map(detached(_)))

  /** `modify(obj)(path)` — the function spelling, `obj` is a regular argument. */
  def modifyFnImpl[S: c.WeakTypeTag, A: c.WeakTypeTag](obj: c.Expr[S])(path: c.Expr[S => A]): c.Expr[PathModify[S, A]] =
    modify[S, A, S => A](obj, detached(path))

  /** `modifyAll(obj)(path, paths*)` — the function spelling. */
  def modifyAllFnImpl[S: c.WeakTypeTag, A: c.WeakTypeTag](obj: c.Expr[S])(
      path: c.Expr[S => A],
      paths: c.Expr[S => A]*
  ): c.Expr[PathModify[S, A]] =
    modifyAll[S, A, S => A](obj, detached(path), paths.map(detached(_)))

  /** `modifyLens[T](path)` — no object to recover; `c.prefix` is the `ModifyLensFactory[T]`. */
  def modifyLensImpl[T: c.WeakTypeTag, Foc: c.WeakTypeTag](path: c.Expr[T => Foc]): c.Expr[PathLazyModify[T, Foc]] =
    modifyLens[T, Foc, T => Foc](detached(path))

  /** `modifyAllLens[T](path, paths*)` — no object to recover. */
  def modifyAllLensImpl[T: c.WeakTypeTag, Foc: c.WeakTypeTag](
      path: c.Expr[T => Foc],
      paths: c.Expr[T => Foc]*
  ): c.Expr[PathLazyModify[T, Foc]] =
    modifyAllLens[T, Foc, T => Foc](detached(path), paths.map(detached(_)))

  /** The arguments of a path's steps (the predicate of `.eachWhere`, an index, a default) are moved out of the path
    * lambda into the generated code. Definitions inside them (e.g. the predicate's own lambda) are owned by the path
    * lambda's symbol, which does not survive the expansion: left as it is, a predicate capturing a local `val` crashes
    * the compiler in lambdalift ("Could not find proxy for val ..."). Re-own them to the macro's call site, the owner
    * every regular macro argument has.
    */
  private def detached[P](path: c.Expr[P]): c.Expr[P] = {
    import c.universe.*
    path.tree match {
      case fn @ Function(_, body) if fn.symbol != null && fn.symbol != NoSymbol =>
        val _ = c.internal.changeOwner(body, fn.symbol, c.internal.enclosingOwner)
      case _ =>
    }
    path
  }

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
