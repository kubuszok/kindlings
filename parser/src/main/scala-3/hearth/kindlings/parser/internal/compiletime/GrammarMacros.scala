package hearth.kindlings.parser
package internal.compiletime

import scala.collection.mutable
import scala.quoted.*

import hearth.kindlings.parser.internal.runtime.Machine

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
    val out = compile(new Extractor(allowAs = false)(using q).grammar(body), generated = false)
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
    val out = compile(new Extractor(allowAs = true)(using q).grammar(body), generated = true)
    val colls = collectionCodes[F](out)
    '{
      _root_.hearth.kindlings.parser.internal.runtime.Builder.generated[F, R](
        ${ Expr(out.tables) },
        $engine,
        new _root_.hearth.kindlings.parser.internal.runtime.GeneratedReductions {
          def reduce(p: Int, m: _root_.hearth.kindlings.parser.internal.runtime.Machine): Boolean = {
            val values = m.stackValues
            val top = m.stackTop
            ${ Codegen.reduce(out, colls, 'p, 'm, 'values, 'top, 'collectionFactories) }
          }
          def hasStringLexer: Boolean = ${ Expr(out.lexer.isDefined) }
          def lexString(m: _root_.hearth.kindlings.parser.internal.runtime.Machine, text: String, from: Int): Int =
            ${ Codegen.lexString(out, 'm, 'text, 'from) }
          protected def factories(): Array[Any] = ${ Codegen.factories(colls) }
        }
      )
    }
  }

  /** The code of a repetition collection (see [[CollectionCodegen]]), as compiler trees. */
  final private case class Collections(factory: Any, newBuilder: Any, add: Any, result: Any)

  private def collectionCodes[F[_]: Type](out: GrammarCompiler.Output)(using q: Quotes): Vector[Collections] = {
    import q.reflect.*
    val helper = new CollectionHelper(q)
    lazy val hasErrorChannel = Implicits.search(TypeRepr.of[ErrorChannel[F]]) match {
      case _: ImplicitSearchSuccess => true
      case _                        => false
    }
    val effect = TypeRepr.of[F].show
    val codes = out.collections.map { coll =>
      helper.collectionCode(
        coll.tpe.asInstanceOf[helper.UntypedType],
        coll.element.asInstanceOf[helper.UntypedType]
      ) match {
        case Right(code) if code.rejectable && !hasErrorChannel =>
          val shown = coll.tpe.asInstanceOf[TypeRepr].show
          val msg =
            "`.as[" + shown + "]`: " + shown + " has a smart constructor that can reject the repeated values, which needs an effect whose error channel reports it (Option, Try, Either[E, *], Future, a cats-effect F, ...) instead of " + effect + " (no ErrorChannel[" + effect + "]). " +
              "To throw the rejection as a ParseError instead, opt in with `import hearth.kindlings.parser.ErrorChannel.throwing._`."
          report.error(msg, coll.pos.underlying.asInstanceOf[Position])
          Left(msg)
        case Right(code) => Right(Collections(code.factory, code.newBuilder, code.add, code.result))
        case Left(msg)   =>
          report.error(msg, coll.pos.underlying.asInstanceOf[Position])
          Left(msg)
      }
    }
    val failed = codes.count(_.isLeft)
    if failed > 0 then report.errorAndAbort(s"the grammar has $failed error(s)")
    codes.collect { case Right(code) => code }
  }

  /** Code generation inside the splices of `grammarImpl`: every function uses the `Quotes` of its splice, whose
    * `spliceOwner` becomes the new owner of the actions and conversions moved out of the grammar block.
    */
  private object Codegen {

    def reduce(
        out: GrammarCompiler.Output,
        colls: Vector[Collections],
        p: Expr[Int],
        m: Expr[Machine],
        values: Expr[Array[Any]],
        top: Expr[Int],
        factories: Expr[Array[Any]]
    )(using q: Quotes): Expr[Boolean] = {
      import q.reflect.*
      def value(rhs: CodegenPlan.RhsPlan, raw: Expr[Any]): Term = rhs match {
        case CodegenPlan.NtPlan(None)       => raw.asTerm
        case CodegenPlan.NtPlan(Some(id))   => applyFn(colls(id).result, List(raw.asTerm))
        case CodegenPlan.TermPlan(None)     => raw.asTerm
        case CodegenPlan.TermPlan(Some(id)) =>
          convert(out.converters(id), '{ $raw.asInstanceOf[String] }.asTerm)
      }
      val cases = out.reduces.toList.map { r =>
        def raw(i: Int): Expr[Any] = '{ $values($top + ${ Expr(i - r.len + 1) }) }
        val newTop = '{ $top - ${ Expr(r.len) } }
        val result: Term = r.body match {
          case CodegenPlan.ReduceBody.User(action, rhs, _) =>
            val args = rhs.toList.zipWithIndex.map { case (plan, i) =>
              val tpe = action.paramTypes(i).asInstanceOf[TypeRepr]
              if !action.used(i) then cast('{ null }.asTerm, tpe)
              else cast(value(plan, raw(i)), tpe)
            }
            applyFn(action.tree, args)
          case CodegenPlan.ReduceBody.Pass(rhs)         => value(rhs, raw(0))
          case CodegenPlan.ReduceBody.Const(text)       => Literal(StringConstant(text))
          case CodegenPlan.ReduceBody.OptNone           => '{ None }.asTerm
          case CodegenPlan.ReduceBody.OptSome(rhs)      => '{ Some(${ value(rhs, raw(0)).asExprOf[Any] }) }.asTerm
          case CodegenPlan.ReduceBody.Collect(id, step) =>
            val code = colls(id)
            def newBuilder = applyFn(code.newBuilder, List('{ $factories(${ Expr(id) }) }.asTerm))
            step match {
              case CodegenPlan.CollectionStep.Empty        => newBuilder
              case CodegenPlan.CollectionStep.One(element) =>
                applyFn(code.add, List(newBuilder, value(element, raw(0))))
              case CodegenPlan.CollectionStep.Append(i, element) =>
                applyFn(code.add, List(raw(0).asTerm, value(element, raw(i))))
            }
        }
        val v = result.asExprOf[Any]
        val body: Expr[Boolean] = r.body match {
          case CodegenPlan.ReduceBody.User(_, _, true) =>
            '{ val value: Any = $v; $m.suspend($newTop, ${ Expr(r.lhs) }, value); true }
          case _ =>
            r.goto match {
              case Some(state) => '{ val value: Any = $v; $m.reducedTo($newTop, ${ Expr(state) }, value); false }
              case None        => '{ val value: Any = $v; $m.reduced($newTop, ${ Expr(r.lhs) }, value); false }
            }
        }
        CaseDef(Literal(IntConstant(r.p)), None, body.asTerm)
      }
      val fallback =
        CaseDef(Wildcard(), None, '{ throw new IllegalStateException("no reduction for production " + $p) }.asTerm)
      Match(p.asTerm, cases :+ fallback).asExprOf[Boolean]
    }

    def lexString(out: GrammarCompiler.Output, m: Expr[Machine], text: Expr[String], from: Expr[Int])(using
        q: Quotes
    ): Expr[Int] = out.lexer match {
      case None        => '{ throw new UnsupportedOperationException("no generated lexer") }
      case Some(lexer) =>
        '{
          val len = $text.length
          var p = $from
          var result = -2
          while result == -2 do if p >= len then {
            $m.token(0, p, p)
            result = Machine.Done
          } else {
            var i = p
            var acc = -1
            var accEnd = p
            var state = 0
            while state >= 0 do ${
              new LexerEmitter(
                text,
                'len,
                'i,
                x => '{ i = $x },
                x => '{ acc = $x; accEnd = i },
                x => '{ state = $x }
              ).states(lexer, 'state)
            }
            if acc < 0 then {
              $m.lexError(p)
              result = Machine.Error
            } else if acc >= ${ Expr(lexer.skipFrom) } then p = accEnd
            else {
              $m.token(acc, p, accEnd)
              result = Machine.Done
            }
          }
          result
        }
    }

    private object LexerEmitter {

      /** Assigns a lexer variable; takes the `Quotes` of the splice it is used in. */
      type Setter = Expr[Int] => Quotes ?=> Expr[Unit]
    }

    /** Emits the code of [[CodegenPlan.LexNode]]s over the lexer's local variables (see `lexString`). */
    final private class LexerEmitter(
        text: Expr[String],
        len: Expr[Int],
        i: Expr[Int],
        setI: LexerEmitter.Setter,
        accept: LexerEmitter.Setter,
        goto: LexerEmitter.Setter
    ) {

      def states(lexer: CodegenPlan.Lexer, state: Expr[Int])(using q: Quotes): Expr[Unit] = {
        import q.reflect.*
        val cases = lexer.states.toList.map { case (s, node) =>
          CaseDef(Literal(IntConstant(s)), None, emit(node).asTerm)
        }
        val fallback = CaseDef(Wildcard(), None, goto(Expr(-1)).asTerm)
        Match(state.asTerm, cases :+ fallback).asExprOf[Unit]
      }

      private def inRanges(ch: Expr[Char], ranges: List[(Int, Int)])(using Quotes): Expr[Boolean] =
        ranges
          .map { case (lo, hi) =>
            if lo == hi then '{ $ch == ${ Expr(lo.toChar) } }
            else '{ $ch >= ${ Expr(lo.toChar) } && $ch <= ${ Expr(hi.toChar) } }
          }
          .reduceLeftOption((a, b) => '{ $a || $b })
          .getOrElse('{ false })

      /** Membership test using the set or its complement, whichever has fewer ranges. */
      private def member(ch: Expr[Char], ranges: List[(Int, Int)])(using Quotes): Expr[Boolean] = {
        val complement = CodegenPlan.complement(ranges)
        if complement.size < ranges.size then '{ ! ${ inRanges(ch, complement) } } else inRanges(ch, ranges)
      }

      private def advance(next: Expr[Unit])(using Quotes): Expr[Unit] = '{ ${ setI('{ $i + 1 }) }; $next }

      def emit(node: CodegenPlan.LexNode)(using q: Quotes): Expr[Unit] = {
        import q.reflect.*
        node match {
          case CodegenPlan.LexNode.Block(nodes) =>
            nodes.map(emit).reduceLeft((a, b) => '{ $a; $b })
          case CodegenPlan.LexNode.SelfLoop(ranges) =>
            '{ while $i < $len && { val d = $text.charAt($i); ${ member('d, ranges) } } do ${ setI('{ $i + 1 }) } }
          case CodegenPlan.LexNode.Accept(token)              => accept(Expr(token))
          case CodegenPlan.LexNode.Goto(target)               => goto(Expr(target))
          case CodegenPlan.LexNode.Dispatch(cases, otherwise) =>
            def dispatch(ch: Expr[Char])(using q2: Quotes): Expr[Unit] = {
              import q2.reflect.*
              val asciiCases = cases.flatMap { case (ranges, next) =>
                val chars = ranges.flatMap { case (lo, hi) => (lo to math.min(hi, 127)).toList }
                if chars.isEmpty then Nil
                else {
                  val literals = chars.map(c => Literal(CharConstant(c.toChar)))
                  val pattern = if literals.size == 1 then literals.head else Alternatives(literals)
                  List(CaseDef(pattern, None, advance(emit(next)).asTerm))
                }
              }
              val nonAscii = cases.flatMap { case (ranges, next) =>
                val high = ranges.collect { case (lo, hi) if hi >= 128 => (math.max(lo, 128), hi) }
                if high.isEmpty then Nil else List(high -> next)
              }
              val fallback = nonAscii.foldRight(emit(otherwise)) { case ((ranges, next), elseBranch) =>
                '{ if ${ inRanges(ch, ranges) } then ${ advance(emit(next)) } else $elseBranch }
              }
              Match(ch.asTerm, asciiCases :+ CaseDef(Wildcard(), None, fallback.asTerm)).asExprOf[Unit]
            }
            '{
              if $i < $len then {
                val c = $text.charAt($i)
                ${ dispatch('c) }
              } else ${ emit(otherwise) }
            }
        }
      }
    }

    def factories(colls: Vector[Collections])(using q: Quotes): Expr[Array[Any]] = {
      import q.reflect.*
      val exprs = colls.map(code => code.factory.asInstanceOf[Term].changeOwner(Symbol.spliceOwner).asExprOf[Any])
      '{ Array[Any](${ Varargs(exprs) }*) }
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

  private def compile(grammar: Grammar, generated: Boolean)(using q: Quotes): GrammarCompiler.Output = {
    import q.reflect.*
    GrammarCompiler.compile(grammar, generated) match {
      case Left(errors) =>
        errors.foreach(d => report.error(d.message, d.pos.underlying.asInstanceOf[Position]))
        report.errorAndAbort(s"the grammar has ${errors.size} error(s)")
      case Right(out) =>
        out.warnings.foreach(d => report.warning(d.message, d.pos.underlying.asInstanceOf[Position]))
        out
    }
  }

  /** @param allowAs
    *   whether repetitions may choose their collection with `.as[C]` (generated code only)
    */
  final private class Extractor(allowAs: Boolean)(using val q: Quotes) {
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

    /** The type argument of `tree`'s type seen as `base[_]`. */
    private def typeArg(tree: q.reflect.Term, base: Symbol): TypeRepr = tree.tpe.widen.baseType(base) match {
      case AppliedType(_, List(arg)) => arg
      case other                     => fail(tree, s"unexpected type of a grammar symbol: ${other.show}")
    }

    /** The default collection of a repetition: a `List` of its elements. */
    private def listOf(tree: q.reflect.Term): Collection = {
      val element = typeArg(tree, TypeRepr.of[Repetition[Any]].typeSymbol)
      Collection(TypeRepr.of[List].appliedTo(element), element, pos(tree))
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
      case DslCall("rep", _, List(s))         => Rep(sym(s, None), atLeastOne = false, listOf(tree))
      case DslCall("rep1", _, List(s))        => Rep(sym(s, None), atLeastOne = true, listOf(tree))
      case DslCall("sepBy", _, List(s, sep))  => SepBy(sym(s, None), sym(sep, None), atLeastOne = false, listOf(tree))
      case DslCall("sepBy1", _, List(s, sep)) => SepBy(sym(s, None), sym(sep, None), atLeastOne = true, listOf(tree))
      case DslCall("as", inner, Nil)          =>
        if !allowAs then fail(
          tree,
          "`.as[C]` is only supported by `Grammar.grammar` (`Grammar.interpreted` collects repetitions into Lists)"
        )
        val target = typeArg(tree, TypeRepr.of[hearth.kindlings.parser.Sym[Any]].typeSymbol)
        sym(inner, name) match {
          case r: Rep   => r.copy(collection = r.collection.copy(tpe = target, pos = pos(tree)))
          case s: SepBy => s.copy(collection = s.collection.copy(tpe = target, pos = pos(tree)))
          case _        => fail(tree, "`.as[C]` is only available on repetitions (rep, rep1, sepBy, sepBy1)")
        }
      case other =>
        fail(
          other,
          "expected a grammar symbol declared in this grammar block, a string literal, an inline \"...\".r regex, or opt/rep/rep1/sepBy/sepBy1(...)"
        )
    }
  }
}
