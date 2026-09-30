package hearth.kindlings.parser

import hearth.kindlings.parser.internal.runtime.Recorder

import scala.annotation.nowarn
import scala.util.matching.Regex

/** The grammar DSL, available inside `Grammar.grammar[R, F] { g => import g.*; ... }`.
  *
  *   - `nonTerminal[A]` declares a non-terminal; `nt ::= alternatives` adds its productions.
  *   - `terminal("regex")`, inline `"literal"` strings and inline `"regex".r` are terminals; `.map(f)` converts the
  *     matched text.
  *   - `all(s1, ..., sN) { (v1, ..., vN) => F[R] }` is an alternative with an effectful action; `all(...).pure { ... }`
  *     one with a pure action. Alternatives are combined with `||`.
  *   - `opt(s)`, `rep(s)`, `rep1(s)`, `sepBy(s, sep)`, `sepBy1(s, sep)` and inline groups (`"+" || "-"` used as a
  *     symbol) are expanded into helper non-terminals; repetitions produce a `List` or, with `.as[C]`, any collection
  *     supported by Hearth's `IsCollection` (see [[Repetition]]).
  *   - `left(...)`, `right(...)`, `nonassoc(...)` declare operator precedence (later declarations bind tighter), like
  *     yacc's `%left`/`%right`/`%nonassoc`; `all(...).prec(op)` overrides a production's precedence (`%prec`).
  *   - `skip("regex")` declares text to skip between tokens (whitespace, comments).
  *   - `enable(flag)` / `disable(flag)` set compile-time options ([[GrammarFlag]]): `ThrowingInRuntime`, `RequireLL1`,
  *     `RequireLALR`.
  *
  * All declarations must come before the productions that use them.
  */
final class Dsl[F[_]] private[parser] () extends DslSequences[F] with DslLiterals {

  private[parser] val recorder: Recorder = new Recorder

  /** Declares a non-terminal producing `A`. */
  def nonTerminal[A]: NonTerminal[A] = recorder.newNonTerminal[A]()

  /** A terminal matching the regular expression `pattern` (a DFA-compatible subset of Java regex syntax). */
  @nowarn("msg=unused")
  def terminal(pattern: String): Terminal[String] = new Terminal[String](Terminal.identity)

  /** Turns on a compile-time option of this grammar (see [[GrammarFlag]]), e.g. `enable(RequireLL1)`. */
  @nowarn("msg=unused")
  def enable(flag: GrammarFlag): Unit = ()

  /** Turns off a compile-time option of this grammar (see [[GrammarFlag]]); flags are off unless enabled. */
  @nowarn("msg=unused")
  def disable(flag: GrammarFlag): Unit = ()

  /** See [[GrammarFlag.ThrowingInRuntime]]. */
  val ThrowingInRuntime: GrammarFlag = GrammarFlag.ThrowingInRuntime

  /** See [[GrammarFlag.RequireLL1]]. */
  val RequireLL1: GrammarFlag = GrammarFlag.RequireLL1

  /** See [[GrammarFlag.RequireLALR]]. */
  val RequireLALR: GrammarFlag = GrammarFlag.RequireLALR

  /** Text matching `pattern` is skipped between tokens. */
  @nowarn("msg=unused")
  def skip(pattern: String): Unit = ()

  /** Left-associative operators, binding tighter than all previously declared ones. */
  @nowarn("msg=unused")
  def left(operators: Sym[Any]*): Unit = ()

  /** Right-associative operators, binding tighter than all previously declared ones. */
  @nowarn("msg=unused")
  def right(operators: Sym[Any]*): Unit = ()

  /** Non-associative operators (`a < b < c` is an error), binding tighter than all previously declared ones. */
  @nowarn("msg=unused")
  def nonassoc(operators: Sym[Any]*): Unit = ()

  /** Zero or one `sym`. */
  def opt[A](sym: Sym[A]): Sym[Option[A]] = new OptSym(sym)

  /** Zero or more `sym`s. */
  def rep[A](sym: Sym[A]): Repetition[A] = new RepSym(sym, atLeastOne = false)

  /** One or more `sym`s. */
  def rep1[A](sym: Sym[A]): Repetition[A] = new RepSym(sym, atLeastOne = true)

  /** Zero or more `sym`s separated by `sep`. */
  def sepBy[A](sym: Sym[A], sep: Sym[Any]): Repetition[A] = new SepBySym(sym, sep, atLeastOne = false)

  /** One or more `sym`s separated by `sep`. */
  def sepBy1[A](sym: Sym[A], sep: Sym[Any]): Repetition[A] = new SepBySym(sym, sep, atLeastOne = true)

  /** An inline regex is a terminal whose value is the matched text. */
  @nowarn("msg=unused")
  implicit def reSym(regex: Regex): Terminal[String] = new Terminal[String](Terminal.identity)

  /** An inline regex as a whole alternative. */
  implicit def reAlt(regex: Regex): Alt[String] = Alt.pass(reSym(regex))

  /** A single symbol as a whole alternative: its value is passed through. */
  implicit def symAlt[A](sym: Sym[A]): Alt[A] = Alt.pass(sym)

  /** Inline alternatives used as a symbol (an anonymous helper non-terminal). */
  implicit def group[A](alt: Alt[A]): Sym[A] = new GroupSym(alt)
}
