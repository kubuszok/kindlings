package hearth.kindlings.optics
package internal.compiletime

import hearth.MacroCommonsScala3
import hearth.std.StdExtensions
import scala.quoted.*

final private[optics] class ModifyMacros(q: Quotes) extends MacroCommonsScala3(using q), StdExtensions, ModifyMacrosImpl

private[optics] object ModifyMacros {

  def modifyImpl[S: Type, A: Type](obj: Expr[S], path: Expr[OpticsContext ?=> (S => A)])(using
      q: Quotes
  ): Expr[PathModify[S, A]] =
    new ModifyMacros(q).modify[S, A](obj, peelContext(path))

  /** Peel the `OpticsContext ?=> _` layer off a path so the rest of the macro sees a plain `S => A` lambda, exactly as
    * on Scala 2. The context function is applied to a throwaway `OpticsContext` (the path body is deconstructed, never
    * run) and `Expr.betaReduce` reduces the application to the inner lambda; the now-`null` `OpticsContext` only sits
    * in the marker evidence positions, which the parser strips (`OpticsContext <: PathStepEvidence`).
    */
  private def peelContext[S: Type, A: Type](path: Expr[OpticsContext ?=> (S => A)])(using q: Quotes): Expr[S => A] = {
    import q.reflect.*
    // `betaReduce($path(using null))` reduces the context-function application but lands a `Block` that binds the
    // throwaway argument to a `val` (e.g. `{ val contextual$1 = null; (s => body) }`); `DestructuredExpr.parse` only
    // accepts a bare lambda, so strip the leading `Block`/`Inlined` wrappers down to the inner `S => A` closure. The
    // closure body still references the dropped `val` in its marker-evidence positions, but those args are
    // `OpticsContext`-typed and the parser discards them (`OpticsContext <: PathStepEvidence`), so they never surface.
    // A lambda is itself encoded as `Block(List(DefDef($anonfun)), Closure)` — keep that intact; only strip the
    // betaReduce-introduced wrapper block that binds the throwaway `OpticsContext` val (its expr is the inner lambda).
    def unwrap(term: Term): Term = term match {
      case Inlined(_, _, inner)               => unwrap(inner)
      case Block(List(_: DefDef), _: Closure) => term
      case Block(_, expr)                     => unwrap(expr)
      case other                              => other
    }
    unwrap(Expr.betaReduce('{ $path(using null.asInstanceOf[OpticsContext]) }).asTerm).asExprOf[S => A]
  }

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
    new ModifyMacros(q).modifyAll[S, A](obj, peelContext(path) :: varargs(paths).map(peelContext[S, A]))

  def modifyLensImpl[T: Type, U: Type](path: Expr[OpticsContext ?=> (T => U)])(using
      q: Quotes
  ): Expr[PathLazyModify[T, U]] =
    new ModifyMacros(q).modifyLens[T, U](peelContext(path))

  def modifyAllLensImpl[T: Type, U: Type](
      path: Expr[OpticsContext ?=> (T => U)],
      paths: Expr[Seq[OpticsContext ?=> (T => U)]]
  )(using q: Quotes): Expr[PathLazyModify[T, U]] =
    new ModifyMacros(q).modifyAllLens[T, U](peelContext(path) :: varargs(paths).map(peelContext[T, U]))

  private def varargs[A](paths: Expr[Seq[A]])(using Quotes, Type[A]): List[Expr[A]] =
    paths match {
      case Varargs(es) => es.toList
      case _           =>
        quotes.reflect.report.errorAndAbort(s"`modifyAll` expects an explicit list of paths, got: ${paths.show}")
    }
}
