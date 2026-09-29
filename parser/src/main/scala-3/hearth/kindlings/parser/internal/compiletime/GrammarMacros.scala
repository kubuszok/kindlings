package hearth.kindlings.parser
package internal.compiletime

import scala.collection.mutable
import scala.quoted.*

import GrammarIR.{Statement as IRStatement, Term as IRTerm, *}

/** Scala 3 bridge: reads the grammar block from the typed tree into a [[GrammarIR.Grammar]], compiles it with
  * [[GrammarCompiler]] and emits the call to the run-time `Builder`.
  *
  * The grammar block is read with the raw compiler API because the constructs it needs (local `val` declarations and
  * references to them) are not exposed by Hearth's `DestructuredExpr`; everything after extraction is shared.
  */
private[parser] object GrammarMacros {

  def interpretedImpl[R: Type, F[_]: Type](
      body: Expr[Dsl[F] => NonTerminal[R]],
      engine: Expr[ParserEngine[F]]
  )(using q: Quotes): Expr[Parser[F, R]] = {
    val out = compile(new Extractor(using q).grammar(body))
    '{
      _root_.hearth.kindlings.parser.internal.runtime.Builder.build[F, R](
        ${ Expr(out.tables) },
        ${ Expr(out.fingerprint) },
        $body,
        $engine
      )
    }
  }

  def grammarImpl[R: Type, F[_]: Type](
      body: Expr[Dsl[F] => NonTerminal[R]],
      engine: Expr[ParserEngine[F]]
  )(using q: Quotes): Expr[Parser[F, R]] = {
    val out = compile(new Extractor(using q).grammar(body))
    '{
      _root_.hearth.kindlings.parser.internal.runtime.Builder.generated[F, R](
        ${ Expr(out.tables) },
        $engine,
        new _root_.hearth.kindlings.parser.internal.runtime.GeneratedReductions {
          def action(p: Int, values: Array[Any], base: Int): Any = ${ Codegen.actions(out, 'p, 'values, 'base) }
          def convert(id: Int, raw: String): Any = ${ Codegen.converters(out, 'id, 'raw) }
        }
      )
    }
  }

  /** Code generation inside the splices of `grammarImpl`: every function uses the `Quotes` of its splice, whose
    * `spliceOwner` becomes the new owner of the actions and conversions moved out of the grammar block.
    */
  private object Codegen {

    def actions(out: GrammarCompiler.Output, p: Expr[Int], values: Expr[Array[Any]], base: Expr[Int])(using
        q: Quotes
    ): Expr[Any] = {
      import q.reflect.*
      val cases = out.prods.toList.map { plan =>
        val args = plan.rhs.toList.zipWithIndex.map { case (rhs, i) =>
          val tpe = plan.action.paramTypes(i).asInstanceOf[TypeRepr]
          if !plan.action.used(i) then cast('{ null }.asTerm, tpe)
          else {
            val raw = '{ $values($base + ${ Expr(i) }) }
            val value = rhs match {
              case GrammarCompiler.NtPlan(false) => raw.asTerm
              case GrammarCompiler.NtPlan(true)  =>
                '{ _root_.hearth.kindlings.parser.internal.runtime.GeneratedReductions.listify($raw) }.asTerm
              case GrammarCompiler.TermPlan(None)     => raw.asTerm
              case GrammarCompiler.TermPlan(Some(id)) =>
                convert(out.converters(id), '{ $raw.asInstanceOf[String] }.asTerm)
            }
            cast(value, tpe)
          }
        }
        CaseDef(Literal(IntConstant(plan.p)), None, applyFn(plan.action.tree, args))
      }
      val fallback =
        CaseDef(Wildcard(), None, '{ throw new IllegalStateException("no action for production " + $p) }.asTerm)
      Match(p.asTerm, cases :+ fallback).asExprOf[Any]
    }

    def converters(out: GrammarCompiler.Output, id: Expr[Int], raw: Expr[String])(using q: Quotes): Expr[Any] = {
      import q.reflect.*
      val cases = out.converters.toList.zipWithIndex.map { case (chain, i) =>
        CaseDef(Literal(IntConstant(i)), None, convert(chain, raw.asTerm))
      }
      val fallback = CaseDef(Wildcard(), None, '{ throw new IllegalStateException("no converter " + $id) }.asTerm)
      Match(id.asTerm, cases :+ fallback).asExprOf[Any]
    }

    private def cast(using q: Quotes)(term: q.reflect.Term, tpe: q.reflect.TypeRepr): q.reflect.Term = {
      import q.reflect.*
      TypeApply(Select.unique(term, "asInstanceOf"), List(Inferred(tpe)))
    }

    private def applyFn(using q: Quotes)(fn: Any, args: List[q.reflect.Term]): q.reflect.Term = {
      import q.reflect.*
      val f = fn.asInstanceOf[Term].changeOwner(Symbol.spliceOwner)
      val call = Select.unique(f, "apply").appliedToArgs(args)
      Term.betaReduce(call).getOrElse(call)
    }

    private def convert(using q: Quotes)(chain: List[Any], raw: q.reflect.Term): q.reflect.Term =
      chain.foldLeft(raw)((acc, fn) => applyFn(fn, List(acc)))
  }

  private def compile(grammar: Grammar)(using q: Quotes): GrammarCompiler.Output = {
    import q.reflect.*
    GrammarCompiler.compile(grammar) match {
      case Left(errors) =>
        errors.foreach(d => report.error(d.message, d.pos.underlying.asInstanceOf[Position]))
        report.errorAndAbort(s"the grammar has ${errors.size} error(s)")
      case Right(out) =>
        out.warnings.foreach(d => report.warning(d.message, d.pos.underlying.asInstanceOf[Position]))
        out
    }
  }

  final private class Extractor(using val q: Quotes) {
    import q.reflect.*

    private val nonTerminals = mutable.LinkedHashMap.empty[Symbol, (Int, NonTerminalDecl)]
    private val terminals = mutable.HashMap.empty[Symbol, IRTerm]
    private val statements = Vector.newBuilder[IRStatement]

    private def pos(tree: Tree): Pos = {
      val p = tree.pos
      Pos(p.sourceFile.name, p.startLine + 1, p.startColumn + 1)(p)
    }

    private def fail(tree: Tree, message: String): Nothing = report.errorAndAbort(message, tree.pos)

    private def strip(tree: q.reflect.Term): q.reflect.Term = tree match {
      case Inlined(_, Nil, t) => strip(t)
      case Inlined(_, _, t)   => strip(t)
      case Typed(t, _)        => strip(t)
      case Block(Nil, t)      => strip(t)
      case _                  => tree
    }

    def grammar(body: Expr[Any]): Grammar = grammarTerm(body.asTerm)

    private def grammarTerm(body: q.reflect.Term): Grammar = strip(body) match {
      case Block(List(DefDef(_, List(TermParamClause(List(param))), _, Some(rhs))), _: Closure) =>
        dslParam = param.symbol
        val (stats, result) = rhs match {
          case Block(ss, expr) => (ss, expr)
          case expr            => (Nil, expr)
        }
        stats.foreach(statement)
        val root = strip(result) match {
          case id: Ident if nonTerminals.contains(id.symbol) => nonTerminals(id.symbol)._1
          case other => fail(other, "the grammar block must end with the start non-terminal")
        }
        Grammar(nonTerminals.values.map(_._2).toVector, statements.result(), root, pos(result))
      case other => fail(other, "`grammar` expects a lambda literal: grammar[R, F] { g => import g.*; ... }")
    }

    private def isDsl(sym: Symbol): Boolean =
      !sym.isNoSymbol && sym.owner.fullName.startsWith("hearth.kindlings.parser")

    /** `recv.name[targs](args)(args2)...` on a DSL method: `(name, receiver, all value args)`. */
    private object DslCall {
      def unapply(tree: Tree): Option[(String, q.reflect.Term, List[q.reflect.Term])] = {
        def flattenArgs(args: List[q.reflect.Term]): List[q.reflect.Term] = args.flatMap { a =>
          strip(a) match {
            case Repeated(elems, _) => elems
            case other              => List(a)
          }
        }
        def loop(
            t: q.reflect.Term,
            args: List[q.reflect.Term]
        ): Option[(String, q.reflect.Term, List[q.reflect.Term])] =
          t match {
            case Apply(fun, as)                            => loop(fun, flattenArgs(as) ++ args)
            case TypeApply(fun, _)                         => loop(fun, args)
            case s @ Select(recv, name) if isDsl(s.symbol) => Some((name, recv, args))
            case i @ Ident(name) if isDsl(i.symbol)        => Some((name, i, args))
            case Inlined(_, _, inner)                      => loop(inner, args)
            case _                                         => None
          }
        tree match {
          case t: q.reflect.Term => loop(strip(t), Nil)
          case _                 => None
        }
      }
    }

    private def statement(stat: Statement): Unit = stat match {
      case _: Import                       => ()
      case vd @ ValDef(name, _, Some(rhs)) =>
        strip(rhs) match {
          case DslCall("nonTerminal", _, Nil) =>
            nonTerminals(vd.symbol) = nonTerminals.size -> NonTerminalDecl(name, pos(vd))
          case other =>
            sym(other, Some(name)) match {
              case t: IRTerm => terminals(vd.symbol) = t
              case _         =>
                fail(
                  vd,
                  "only `nonTerminal[A]` and terminal declarations (`terminal(...)`, optionally `.map(...)`) are allowed as vals in a grammar block"
                )
            }
        }
      case DslCall("::=", lhs, List(alts)) =>
        strip(lhs) match {
          case id: Ident if nonTerminals.contains(id.symbol) =>
            statements += Production(nonTerminals(id.symbol)._1, alternatives(alts), pos(stat))
          case other => fail(other, "the left-hand side of `::=` must be a non-terminal declared in this grammar block")
        }
      case DslCall(assoc @ ("left" | "right" | "nonassoc"), _, ops) =>
        val a = assoc match {
          case "left"  => Assoc.Left
          case "right" => Assoc.Right
          case _       => Assoc.NonAssoc
        }
        statements += Precedence(a, ops.map(sym(_, None)), pos(stat))
      case DslCall("skip", _, List(arg)) =>
        statements += Skip(stringLiteral(arg), pos(stat))
      case _: DefDef =>
        fail(stat, "helper methods are not supported in grammar blocks yet")
      case other =>
        fail(
          other,
          "grammar blocks may only contain declarations (`val x = nonTerminal[A]`, `val t = terminal(...)`), productions (`x ::= ...`), precedence declarations and `skip(...)`"
        )
    }

    /** The grammar's own symbols (the DSL parameter, declared non-terminals and terminals) do not exist at run time in
      * generated code, so actions and conversions must not refer to them.
      */
    private var dslParam: Symbol = Symbol.noSymbol

    private def checkNoGrammarRefs(fn: q.reflect.Term): Unit = {
      val acc = new TreeAccumulator[Unit] {
        def foldTree(u: Unit, tree: Tree)(owner: Symbol): Unit = tree match {
          case id: Ident
              if id.symbol == dslParam || nonTerminals.contains(id.symbol) || terminals.contains(id.symbol) =>
            fail(
              id,
              "grammar symbols (and the grammar DSL) can only be used in productions, not inside actions or `.map` functions"
            )
          case _ => foldOverTree(u, tree)(owner)
        }
      }
      acc.foldTree((), fn)(Symbol.spliceOwner)
    }

    private def action(fn: q.reflect.Term): Action = {
      checkNoGrammarRefs(fn)
      val f = strip(fn)
      val types = f.tpe.widen.dealias.typeArgs
      val paramTypes = types.init
      val used = f match {
        case Block(List(DefDef(_, List(TermParamClause(params)), _, Some(rhs))), _: Closure) =>
          params.map { p =>
            val acc = new TreeAccumulator[Boolean] {
              def foldTree(found: Boolean, tree: Tree)(owner: Symbol): Boolean =
                found || (tree match {
                  case id: Ident if id.symbol == p.symbol => true
                  case _                                  => foldOverTree(found, tree)(owner)
                })
            }
            acc.foldTree(false, rhs)(Symbol.spliceOwner)
          }
        case _ => paramTypes.map(_ => true)
      }
      Action(f, paramTypes, types.last, used)
    }

    private def stringLiteral(tree: q.reflect.Term): String = strip(tree) match {
      case Literal(StringConstant(s)) => s
      case other => fail(other, "expected a string literal (patterns are compiled at compile time)")
    }

    /** The literal of `"...".r` (through the implicit `augmentString`/`StringOps` wrapper). */
    private def regexLiteral(tree: q.reflect.Term): String = strip(tree) match {
      case Select(qual, "r")             => unwrapString(qual)
      case Apply(Select(qual, "r"), Nil) => unwrapString(qual)
      case other                         => fail(other, "expected an inline regex literal: \"...\".r")
    }
    private def unwrapString(tree: q.reflect.Term): String = strip(tree) match {
      case Literal(StringConstant(s)) => s
      case Apply(_, List(arg))        => unwrapString(arg)
      case other                      => fail(other, "regex patterns must be string literals")
    }

    private def alternatives(tree: q.reflect.Term): List[Alternative] = strip(tree) match {
      case DslCall("||", left, List(right))                      => alternatives(left) ++ alternatives(right)
      case DslCall(kind @ ("apply" | "pure"), builder, List(fn)) =>
        val (syms, prec) = sequence(builder)
        List(
          Alternative(syms, if kind == "pure" then Kind.Pure else Kind.Effectful, prec, pos(tree), Some(action(fn)))
        )
      case DslCall("litAlt", _, List(arg)) =>
        stringLiteral(arg) match {
          case ""   => List(Alternative(Nil, Kind.Empty, None, pos(tree)))
          case text => List(Alternative(List(IRTerm(LiteralPattern(text), None, pos(arg))), Kind.Pass, None, pos(tree)))
        }
      case DslCall("reAlt", _, List(arg)) =>
        List(Alternative(List(IRTerm(RegexPattern(regexLiteral(arg)), None, pos(arg))), Kind.Pass, None, pos(tree)))
      case DslCall("symAlt", _, List(arg)) => List(Alternative(List(sym(arg, None)), Kind.Pass, None, pos(tree)))
      case other                           =>
        fail(
          other,
          "expected alternatives: `all(...) { ... }`, `all(...).pure { ... }`, a symbol, or several of them combined with `||`"
        )
    }

    /** `all(s1, ..., sN)` optionally followed by `.prec(op)`. */
    private def sequence(tree: q.reflect.Term): (List[Sym], Option[Sym]) = strip(tree) match {
      case DslCall("prec", inner, List(op)) =>
        val (syms, _) = sequence(inner)
        (syms, Some(sym(op, None)))
      case DslCall("all", _, args) => (args.map(sym(_, None)), None)
      case other                   => fail(other, "expected `all(...)`")
    }

    private def sym(tree: q.reflect.Term, name: Option[String]): Sym = strip(tree) match {
      case id: Ident if nonTerminals.contains(id.symbol) => NtRef(nonTerminals(id.symbol)._1)
      case id: Ident if terminals.contains(id.symbol)    => terminals(id.symbol)
      case DslCall("litSym", _, List(arg))               => IRTerm(LiteralPattern(stringLiteral(arg)), name, pos(tree))
      case DslCall("reSym", _, List(arg))                => IRTerm(RegexPattern(regexLiteral(arg)), name, pos(tree))
      case DslCall("terminal", _, List(arg))             => IRTerm(RegexPattern(stringLiteral(arg)), name, pos(tree))
      case DslCall("map", terminal, List(fn))            =>
        checkNoGrammarRefs(fn)
        sym(terminal, name) match {
          case t: IRTerm => t.copy(converters = t.converters :+ strip(fn))
          case _         => fail(tree, "`.map` is only available on terminals")
        }
      case DslCall("group", _, List(alts))    => Group(alternatives(alts))
      case DslCall("opt", _, List(s))         => Opt(sym(s, None))
      case DslCall("rep", _, List(s))         => Rep(sym(s, None), atLeastOne = false)
      case DslCall("rep1", _, List(s))        => Rep(sym(s, None), atLeastOne = true)
      case DslCall("sepBy", _, List(s, sep))  => SepBy(sym(s, None), sym(sep, None), atLeastOne = false)
      case DslCall("sepBy1", _, List(s, sep)) => SepBy(sym(s, None), sym(sep, None), atLeastOne = true)
      case other                              =>
        fail(
          other,
          "expected a grammar symbol declared in this grammar block, a string literal, an inline \"...\".r regex, or opt/rep/rep1/sepBy/sepBy1(...)"
        )
    }
  }
}
