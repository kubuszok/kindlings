package hearth.kindlings.optics
package internal.compiletime

import hearth.MacroCommonsScala3
import hearth.std.StdExtensions
import scala.quoted.*

final private[optics] class ModifyMacros(q: Quotes) extends MacroCommonsScala3(using q), StdExtensions, ModifyMacrosImpl

private[optics] object ModifyMacros {

  // Paths are context functions `OpticsContext ?=> (S => A)`; the shared `ModifyMacrosImpl.parsePath` peels the
  // contextual layer with Hearth's cross-platform `DestructuredExpr.skipContextualWrappers`, so they are passed as-is.
  private type Path[S, A] = OpticsContext ?=> (S => A)

  def modifyImpl[S: Type, A: Type](obj: Expr[S], path: Expr[Path[S, A]])(using
      q: Quotes
  ): Expr[PathModify[S, A]] =
    new ModifyMacros(q).modify[S, A, Path[S, A]](obj, path)

  /** Materializers for the marker evidences (each a `transparent inline given`), backed by the std SPI. */
  def isElementOfImpl[C: Type](using q: Quotes): Expr[IsElementOf[C]] =
    new ModifyMacros(q).deriveIsElementOf[C]

  def isIndexedElementOfImpl[C: Type](using q: Quotes): Expr[IsIndexedElementOf[C]] =
    new ModifyMacros(q).deriveIsIndexedElementOf[C]

  def isSingleElementOfImpl[C: Type](using q: Quotes): Expr[IsSingleElementOf[C]] =
    new ModifyMacros(q).deriveIsSingleElementOf[C]

  def isEitherImpl[C: Type](using q: Quotes): Expr[IsEither[C]] =
    new ModifyMacros(q).deriveIsEither[C]

  def modifyAllImpl[S: Type, A: Type](
      obj: Expr[S],
      path: Expr[OpticsContext ?=> (S => A)],
      paths: Expr[Seq[OpticsContext ?=> (S => A)]]
  )(using q: Quotes): Expr[PathModify[S, A]] =
    new ModifyMacros(q).modifyAll[S, A, Path[S, A]](obj, path, paths)

  def modifyLensImpl[T: Type, U: Type](path: Expr[OpticsContext ?=> (T => U)])(using
      q: Quotes
  ): Expr[PathLazyModify[T, U]] =
    new ModifyMacros(q).modifyLens[T, U, Path[T, U]](path)

  def modifyAllLensImpl[T: Type, U: Type](
      path: Expr[OpticsContext ?=> (T => U)],
      paths: Expr[Seq[OpticsContext ?=> (T => U)]]
  )(using q: Quotes): Expr[PathLazyModify[T, U]] =
    new ModifyMacros(q).modifyAllLens[T, U, Path[T, U]](path, paths)
}
