package hearth.kindlings.optics
package internal.compiletime

import hearth.MacroCommonsScala3
import hearth.std.StdExtensions
import scala.quoted.*

final private[optics] class ModifyMacros(q: Quotes) extends MacroCommonsScala3(using q), StdExtensions, ModifyMacrosImpl

private[optics] object ModifyMacros {

  def modifyImpl[S: Type, A: Type](obj: Expr[S], path: Expr[S => A])(using q: Quotes): Expr[PathModify[S, A]] =
    new ModifyMacros(q).modify[S, A](obj, path)

  /** Materializers for the marker evidences (each a `transparent inline given`), backed by the std SPI. */
  def isElementOfImpl[C: Type](using q: Quotes): Expr[IsElementOf[C]] =
    new ModifyMacros(q).deriveIsElementOf[C]

  def isIndexedElementOfImpl[C: Type](using q: Quotes): Expr[IsIndexedElementOf[C]] =
    new ModifyMacros(q).deriveIsIndexedElementOf[C]

  def isSingleElementOfImpl[C: Type](using q: Quotes): Expr[IsSingleElementOf[C]] =
    new ModifyMacros(q).deriveIsSingleElementOf[C]

  def isEitherImpl[C: Type](using q: Quotes): Expr[IsEither[C]] =
    new ModifyMacros(q).deriveIsEither[C]

  def modifyAllImpl[S: Type, A: Type](obj: Expr[S], path: Expr[S => A], paths: Expr[Seq[S => A]])(using
      q: Quotes
  ): Expr[PathModify[S, A]] =
    new ModifyMacros(q).modifyAll[S, A](obj, path :: varargs(paths))

  def modifyLensImpl[T: Type, U: Type](path: Expr[T => U])(using q: Quotes): Expr[PathLazyModify[T, U]] =
    new ModifyMacros(q).modifyLens[T, U](path)

  def modifyAllLensImpl[T: Type, U: Type](path: Expr[T => U], paths: Expr[Seq[T => U]])(using
      q: Quotes
  ): Expr[PathLazyModify[T, U]] =
    new ModifyMacros(q).modifyAllLens[T, U](path :: varargs(paths))

  private def varargs[A](paths: Expr[Seq[A]])(using Quotes, Type[A]): List[Expr[A]] =
    paths match {
      case Varargs(es) => es.toList
      case _           =>
        quotes.reflect.report.errorAndAbort(s"`modifyAll` expects an explicit list of paths, got: ${paths.show}")
    }
}
