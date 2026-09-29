package hearth.kindlings.parser
package internal.compiletime

/** The compile-time view of a grammar block, extracted from the typed tree by the per-compiler `GrammarMacros` and
  * compiled by [[GrammarCompiler]]. Free of any compiler API: positions carry the compiler's position object opaquely.
  */
private[parser] object GrammarIR {

  /** A source position; `underlying` is the compiler's own position, used to report diagnostics. */
  final case class Pos(file: String, line: Int, column: Int)(val underlying: Any) {
    def show: String = s"$file:$line:$column"
  }

  sealed trait Pattern {
    def display: String
  }
  final case class LiteralPattern(text: String) extends Pattern {
    def display: String = internal.runtime.Machine.quote(text)
  }
  final case class RegexPattern(regex: String) extends Pattern {
    def display: String = s"/$regex/"
  }

  sealed trait Sym
  final case class NtRef(id: Int) extends Sym

  /** @param converters
    *   the `.map` functions applied to the matched text, in order (compiler trees, used by code generation)
    */
  final case class Term(pattern: Pattern, name: Option[String], pos: Pos, converters: List[Any] = Nil) extends Sym
  final case class Group(alts: List[Alternative]) extends Sym
  final case class Opt(sym: Sym) extends Sym
  final case class Rep(sym: Sym, atLeastOne: Boolean, collection: Collection) extends Sym
  final case class SepBy(sym: Sym, sep: Sym, atLeastOne: Boolean, collection: Collection) extends Sym

  /** The collection a repetition's values are collected into (compiler types, used by code generation).
    *
    * @param tpe
    *   the collection type (`List[A]` unless chosen with `.as[C]`)
    * @param element
    *   the type of the repeated values
    */
  final case class Collection(tpe: Any, element: Any, pos: Pos)

  sealed trait Kind
  object Kind {
    case object Pure extends Kind
    case object Effectful extends Kind
    case object Pass extends Kind
    case object Empty extends Kind
  }

  /** A user action (compiler trees, used by code generation).
    *
    * @param tree
    *   the action function
    * @param paramTypes
    *   the types of the action's parameters (one per symbol)
    * @param resultType
    *   the action's result type (`F[R]` for effectful actions)
    * @param used
    *   whether each parameter is referenced by the action's body (unused values are not converted)
    */
  final case class Action(tree: Any, paramTypes: List[Any], resultType: Any, used: List[Boolean])

  final case class Alternative(syms: List[Sym], kind: Kind, prec: Option[Sym], pos: Pos, action: Option[Action] = None)

  sealed trait Assoc
  object Assoc {
    case object Left extends Assoc
    case object Right extends Assoc
    case object NonAssoc extends Assoc
  }

  sealed trait Statement
  final case class Production(lhs: Int, alts: List[Alternative], pos: Pos) extends Statement
  final case class Precedence(assoc: Assoc, operators: List[Sym], pos: Pos) extends Statement
  final case class Skip(regex: String, pos: Pos) extends Statement

  final case class NonTerminalDecl(name: String, pos: Pos)

  final case class Grammar(
      nonTerminals: Vector[NonTerminalDecl],
      statements: Vector[Statement],
      root: Int,
      rootPos: Pos
  )

  final case class Diagnostic(pos: Pos, message: String)
}
