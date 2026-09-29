package hearth.kindlings.parser

import hearth.kindlings.parser.internal.runtime.{Flatten, Recorder}

/** A grammar symbol producing a value of type `A`: a [[NonTerminal]], a [[Terminal]], or a structural symbol (an inline
  * group of alternatives, `opt`, `rep`, `sepBy`, ...).
  *
  * Symbols are values of the grammar DSL: they exist so that the compiler can check the wiring of types between symbols
  * and actions. The macro reads the grammar structure at compile time; at run time the grammar block is evaluated once
  * to collect the action functions.
  */
sealed abstract class Sym[+A]

/** A non-terminal declared with `nonTerminal[A]` inside a grammar block; its productions are added with `::=`. */
final class NonTerminal[A] private[parser] (private[parser] val id: Int, recorder: Recorder) extends Sym[A] {

  /** Adds the alternatives `alts` as productions of this non-terminal. May be used several times. */
  def ::=(alts: Alt[A]): Unit = recorder.production(id, alts)
}

/** A terminal (token): a literal string or a regular expression, optionally mapped with `.map`.
  *
  * Its value is the matched text converted by the (composed) `.map` functions.
  */
final class Terminal[+A] private[parser] (private[parser] val convert: String => Any) extends Sym[A] {

  /** Converts the matched text. The function runs when the enclosing production is reduced. */
  def map[B](f: A => B): Terminal[B] = new Terminal[B](convert.andThen(f.asInstanceOf[Any => Any]))
}
object Terminal {

  private[parser] val identity: String => Any = (s: String) => s

  @scala.annotation.nowarn("msg=unused")
  private[parser] def literal[S <: String](text: S): Terminal[S] = new Terminal[S](identity)
}

final private[parser] class GroupSym[A](val alt: Alt[A]) extends Sym[A]
final private[parser] class OptSym[A](val sym: Sym[A]) extends Sym[Option[A]]

/** A repetition (`rep`, `rep1`, `sepBy`, `sepBy1`) of `A`s, collected into a `List[A]` by default.
  *
  * `.as[C]` collects into any collection `C` supported by Hearth's `IsCollection` standard extension - the Scala and
  * Java collections, arrays, and whatever providers are on the classpath (e.g. cats `NonEmptyList` / `Chain` with
  * `kindlings-cats-integration`). The macro checks that `C` is supported and that `A` fits its elements, and generates
  * code that appends each element to `C`'s own mutable builder (no intermediate `List`).
  */
sealed abstract class Repetition[A] extends Sym[List[A]] {

  /** Collect the repeated values into `C` instead of a `List` (only with `Grammar.grammar`, not `Grammar.interpreted`).
    */
  def as[C]: Sym[C] = this.asInstanceOf[Sym[C]]
}

final private[parser] class RepSym[A](val sym: Sym[A], val atLeastOne: Boolean) extends Repetition[A]
final private[parser] class SepBySym[A](val sym: Sym[A], val sep: Sym[Any], val atLeastOne: Boolean)
    extends Repetition[A]

/** One or more alternatives (productions) producing `A`, combined with `||`. */
final class Alt[+A] private[parser] (private[parser] val alternatives: List[Alt.Single]) {

  /** Adds `other`'s alternatives after this one's. */
  def ||[B >: A](other: Alt[B]): Alt[B] = new Alt[B](alternatives ++ other.alternatives)
}
object Alt {

  /** The run-time view of one alternative: its symbols and what to do on reduction. */
  final private[parser] class Single(val syms: List[Sym[Any]], val action: Flatten.FAction[Action])

  /** A user action: `fn` receives the (converted) values of the alternative's symbols. */
  final private[parser] class Action(val fn: Array[Any] => Any, val effectful: Boolean)

  private[parser] def user[A](syms: List[Sym[Any]], effectful: Boolean, fn: Array[Any] => Any): Alt[A] =
    new Alt[A](List(new Single(syms, Flatten.User(new Action(fn, effectful)))))

  private[parser] def pass[A](sym: Sym[A]): Alt[A] = new Alt[A](List(new Single(List(sym), Flatten.Pass)))

  private[parser] def literal[S <: String](text: S): Alt[S] =
    if (text.isEmpty) new Alt[S](List(new Single(Nil, Flatten.Const(""))))
    else pass[S](Terminal.literal(text))
}
