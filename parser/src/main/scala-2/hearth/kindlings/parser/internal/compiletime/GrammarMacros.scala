package hearth.kindlings.parser
package internal.compiletime

import scala.collection.mutable
import scala.reflect.macros.blackbox

import GrammarIR.{Alternative as IRAlternative, Statement as IRStatement, Term as IRTerm, *}

/** Scala 2 bridge: reads the grammar block from the typed tree into a [[GrammarIR.Grammar]], compiles it with
  * [[GrammarCompiler]] and emits the call to the run-time `Builder`.
  *
  * The grammar block is read with the raw compiler API because the constructs it needs (local `val` declarations and
  * references to them) are not exposed by Hearth's `DestructuredExpr`; everything after extraction is shared.
  */
final private[parser] class GrammarMacros(val c: blackbox.Context) {

  import c.universe.*

  def interpretedImpl[R, F[_]](body: c.Tree)(engine: c.Tree): c.Tree = {
    val out = compile(new Extractor(allowAs = false).grammar(body), generated = false)
    val targs = typeArgs(c.macroApplication)
    val tables = out.tables.map(chunk => Literal(Constant(chunk)))
    q"""_root_.hearth.kindlings.parser.internal.runtime.Builder.build[..${targs.reverse}](
          _root_.scala.List(..$tables),
          ${out.fingerprint},
          $body,
          $engine
        )"""
  }

  def grammarImpl[R, F[_]](body: c.Tree)(engine: c.Tree): c.Tree = {
    val out = compile(new Extractor(allowAs = true).grammar(body), generated = true)
    val targs = typeArgs(c.macroApplication)
    val collections = collectionCodes(out, targs(1).tpe)
    val tables = out.tables.map(chunk => Literal(Constant(chunk)))
    val p = TermName(c.freshName("p"))
    val builder = TermName(c.freshName("builder"))
    val values = TermName(c.freshName("values"))
    val base = TermName(c.freshName("base"))
    val id = TermName(c.freshName("id"))
    val raw = TermName(c.freshName("raw"))

    /** A function tree applied to arguments: untypechecked so that it is re-typed (with fresh owners) where it is
      * spliced, with an ascription providing the parameter types that type inference cannot recover.
      */
    def applyFn(fn: Any, fnType: Type, args: List[Tree]): Tree =
      q"(${c.untypecheck(fn.asInstanceOf[Tree])}: ${TypeTree(fnType)}).apply(..$args)"
    def convert(chain: List[Any], rawValue: Tree): Tree =
      chain.foldLeft(rawValue) { (acc, fn) =>
        val tree = fn.asInstanceOf[Tree]
        applyFn(tree, tree.tpe.widen, List(acc))
      }

    val anyToAny = typeOf[Any => Any]
    val anyAnyToAny = typeOf[(Any, Any) => Any]
    def collect(id: Int, rawValue: Tree): Tree = applyFn(collections(id).result, anyToAny, List(rawValue))
    def value(rhs: GrammarCompiler.RhsPlan, i: Int): Tree = {
      val rawValue = q"$values($base + $i)"
      rhs match {
        case GrammarCompiler.NtPlan(None)       => rawValue
        case GrammarCompiler.NtPlan(Some(id))   => collect(id, rawValue)
        case GrammarCompiler.TermPlan(None)     => rawValue
        case GrammarCompiler.TermPlan(Some(id)) =>
          convert(out.converters(id), q"$rawValue.asInstanceOf[_root_.java.lang.String]")
      }
    }

    val actionCases = out.prods.toList.map { plan =>
      val args = plan.rhs.toList.zipWithIndex.map { case (rhs, i) =>
        val tpe = TypeTree(plan.action.paramTypes(i).asInstanceOf[Type])
        if (!plan.action.used(i)) q"null.asInstanceOf[$tpe]"
        else q"${value(rhs, i)}.asInstanceOf[$tpe]"
      }
      val fn = plan.action.tree.asInstanceOf[Tree]
      cq"${plan.p} => ${applyFn(fn, fn.tpe.widen, args)}"
    }
    val collectionCases = out.collectionPlans.toList.map { plan =>
      val code = collections(plan.collection)
      def newBuilder = applyFn(code.newBuilder, anyToAny, List(q"collectionFactories(${plan.collection})"))
      val body = plan.step match {
        case GrammarCompiler.CollectionStep.Empty        => newBuilder
        case GrammarCompiler.CollectionStep.One(element) =>
          applyFn(code.add, anyAnyToAny, List(newBuilder, value(element, 0)))
        case GrammarCompiler.CollectionStep.Append(i, element) =>
          applyFn(code.add, anyAnyToAny, List(q"$values($base)", value(element, i)))
      }
      cq"${plan.p} => $body"
    }
    val converterCases = out.converters.toList.zipWithIndex.map { case (chain, i) =>
      cq"$i => ${convert(chain, q"$raw")}"
    }
    val collectCases = collections.toList.zipWithIndex.map { case (_, i) => cq"$i => ${collect(i, q"$builder")}" }
    val factories = collections.toList.map(code => c.untypecheck(code.factory))

    q"""_root_.hearth.kindlings.parser.internal.runtime.Builder.generated[..${targs.reverse}](
          _root_.scala.List(..$tables),
          $engine,
          new _root_.hearth.kindlings.parser.internal.runtime.GeneratedReductions {
            def action($p: _root_.scala.Int, $values: _root_.scala.Array[_root_.scala.Any], $base: _root_.scala.Int): _root_.scala.Any =
              ($p: @_root_.scala.annotation.switch) match {
                case ..$actionCases
                case ..$collectionCases
                case _ => throw new _root_.java.lang.IllegalStateException("no action for production " + $p)
              }
            def convert($id: _root_.scala.Int, $raw: _root_.java.lang.String): _root_.scala.Any =
              ($id: @_root_.scala.annotation.switch) match {
                case ..$converterCases
                case _ => throw new _root_.java.lang.IllegalStateException("no converter " + $id)
              }
            def collect($id: _root_.scala.Int, $builder: _root_.scala.Any): _root_.scala.Any =
              ($id: @_root_.scala.annotation.switch) match {
                case ..$collectCases
                case _ => throw new _root_.java.lang.IllegalStateException("no collection " + $id)
              }
            protected def factories(): _root_.scala.Array[_root_.scala.Any] =
              _root_.scala.Array[_root_.scala.Any](..$factories)
          }
        )"""
  }

  /** The code of each repetition collection (see [[CollectionCodegen]]), as untypechecked trees. */
  private def collectionCodes(out: GrammarCompiler.Output, effect: Type): Vector[Collections] = {
    val helper = new CollectionHelper(c)
    lazy val hasErrorChannel = c.inferImplicitValue(
      appliedType(typeOf[ErrorChannel[Option]].typeConstructor, List(effect)),
      silent = true
    ) != EmptyTree
    val codes = out.collections.map { coll =>
      helper.collectionCode(
        coll.tpe.asInstanceOf[helper.UntypedType],
        coll.element.asInstanceOf[helper.UntypedType]
      ) match {
        case Right(code) if code.rejectable && !hasErrorChannel =>
          val shown = coll.tpe.asInstanceOf[Type].toString
          val msg =
            "`.as[" + shown + "]`: " + shown + " has a smart constructor that can reject the repeated values, which needs an effect whose error channel reports it (Option, Try, Either[E, *], Future, a cats-effect F, ...) instead of " + effect + " (no ErrorChannel[" + effect + "]). " +
              "To throw the rejection as a ParseError instead, opt in with `import hearth.kindlings.parser.ErrorChannel.throwing._`."
          c.error(coll.pos.underlying.asInstanceOf[Position], msg)
          Left(msg)
        case Right(code) =>
          def tree(t: Any): Tree = c.untypecheck(t.asInstanceOf[Tree])
          Right(Collections(tree(code.factory), tree(code.newBuilder), tree(code.add), tree(code.result)))
        case Left(msg) =>
          c.error(coll.pos.underlying.asInstanceOf[Position], msg)
          Left(msg)
      }
    }
    val failed = codes.count(_.isLeft)
    if (failed > 0) c.abort(c.enclosingPosition, s"the grammar has $failed error(s)")
    codes.collect { case Right(code) => code }
  }

  final private case class Collections(factory: Tree, newBuilder: Tree, add: Tree, result: Tree)

  private def compile(grammar: Grammar, generated: Boolean): GrammarCompiler.Output =
    GrammarCompiler.compile(grammar, generated) match {
      case Left(errors) =>
        errors.foreach(d => c.error(d.pos.underlying.asInstanceOf[Position], d.message))
        c.abort(c.enclosingPosition, s"the grammar has ${errors.size} error(s)")
      case Right(out) =>
        out.warnings.foreach(d => c.warning(d.pos.underlying.asInstanceOf[Position], d.message))
        out
    }

  private def typeArgs(tree: Tree): List[Tree] = tree match {
    case TypeApply(_, targs) => targs
    case Apply(fun, _)       => typeArgs(fun)
    case _                   => c.abort(c.enclosingPosition, "unexpected shape of the `grammar` call")
  }

  private def pos(tree: Tree): Pos = {
    val p = if (tree.pos == NoPosition) c.enclosingPosition else tree.pos
    Pos(p.source.file.name, p.line, p.column)(p)
  }

  /** @param allowAs
    *   whether repetitions may choose their collection with `.as[C]` (generated code only)
    */
  final private class Extractor(allowAs: Boolean) {

    private val nonTerminals = mutable.LinkedHashMap.empty[Symbol, (Int, NonTerminalDecl)]
    private val terminals = mutable.HashMap.empty[Symbol, IRTerm]
    private val statements = Vector.newBuilder[IRStatement]

    private def fail(tree: Tree, message: String): Nothing = c.abort(tree.pos, message)

    def grammar(body: Tree): Grammar = strip(body) match {
      case Function(List(param), rhs) =>
        dslParam = param.symbol
        val (stats, result) = strip(rhs) match {
          case Block(ss, expr) => (ss, expr)
          case expr            => (Nil, expr)
        }
        stats.foreach(statement)
        val root = strip(result) match {
          case id: Ident if nonTerminals.contains(id.symbol) => nonTerminals(id.symbol)._1
          case other => fail(other, "the grammar block must end with the start non-terminal")
        }
        Grammar(nonTerminals.values.map(_._2).toVector, statements.result(), root, pos(result))
      case other => fail(other, "`grammar` expects a lambda literal: grammar[R, F] { g => import g._; ... }")
    }

    private def strip(tree: Tree): Tree = tree match {
      case Typed(t, _)     => strip(t)
      case Block(Nil, t)   => strip(t)
      case Annotated(_, t) => strip(t)
      case _               => tree
    }

    private def isDsl(sym: Symbol): Boolean =
      sym != null && sym != NoSymbol && sym.owner.fullName.startsWith("hearth.kindlings.parser")

    /** `recv.name[targs](args)(args2)...` on a DSL method: `(name, receiver, all value args)`. */
    private object DslCall {
      def unapply(tree: Tree): Option[(String, Tree, List[Tree])] = {
        def loop(t: Tree, args: List[Tree]): Option[(String, Tree, List[Tree])] = t match {
          case Apply(fun, as)                            => loop(fun, as ++ args)
          case TypeApply(fun, _)                         => loop(fun, args)
          case s @ Select(recv, name) if isDsl(s.symbol) => Some((name.decodedName.toString, recv, args))
          case _                                         => None
        }
        tree match {
          case _: Apply | _: TypeApply | _: Select => loop(strip(tree), Nil)
          case _                                   => None
        }
      }
    }

    private def statement(stat: Tree): Unit = stat match {
      case _: Import  => ()
      case vd: ValDef =>
        val name = vd.name.decodedName.toString.trim
        strip(vd.rhs) match {
          case DslCall("nonTerminal", _, Nil) =>
            nonTerminals(vd.symbol) = nonTerminals.size -> NonTerminalDecl(name, pos(vd))
          case rhs =>
            sym(rhs, Some(name)) match {
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
    private var dslParam: Symbol = NoSymbol

    private def checkNoGrammarRefs(fn: Tree): Unit =
      fn.foreach {
        case id: Ident
            if id.symbol != NoSymbol && (id.symbol == dslParam || nonTerminals.contains(id.symbol) || terminals
              .contains(id.symbol)) =>
          fail(
            id,
            "grammar symbols (and the grammar DSL) can only be used in productions, not inside actions or `.map` functions"
          )
        case _ => ()
      }

    private def action(fn: Tree): Action = {
      checkNoGrammarRefs(fn)
      val f = strip(fn)
      val types = f.tpe.widen.dealias.typeArgs
      val paramTypes = types.init
      val used = f match {
        case Function(params, rhs) => params.map(p => rhs.exists(_.symbol == p.symbol))
        case _                     => paramTypes.map(_ => true)
      }
      Action(f, paramTypes, types.last, used)
    }

    /** The type argument of `tree`'s type seen as `base[_]`. */
    private def typeArg(tree: Tree, base: Symbol): Type = tree.tpe.widen.baseType(base).typeArgs match {
      case List(arg) => arg
      case _         => fail(tree, s"unexpected type of a grammar symbol: ${tree.tpe}")
    }

    /** The default collection of a repetition: a `List` of its elements. */
    private def listOf(tree: Tree): Collection = {
      val element = typeArg(tree, symbolOf[Repetition[?]])
      Collection(appliedType(typeOf[List[Any]].typeConstructor, element), element, pos(tree))
    }

    private def stringLiteral(tree: Tree): String = strip(tree) match {
      case Literal(Constant(s: String)) => s
      case other => fail(other, "expected a string literal (patterns are compiled at compile time)")
    }

    /** The literal of `"...".r` (through the implicit `augmentString`/`StringOps` wrapper). */
    private def regexLiteral(tree: Tree): String = strip(tree) match {
      case Select(q, TermName("r"))             => unwrapString(q)
      case Apply(Select(q, TermName("r")), Nil) => unwrapString(q)
      case other                                => fail(other, "expected an inline regex literal: \"...\".r")
    }
    private def unwrapString(tree: Tree): String = strip(tree) match {
      case Literal(Constant(s: String)) => s
      case Apply(_, List(arg))          => unwrapString(arg)
      case other                        => fail(other, "regex patterns must be string literals")
    }

    private def alternatives(tree: Tree): List[IRAlternative] = strip(tree) match {
      case DslCall("||", left, List(right))                      => alternatives(left) ++ alternatives(right)
      case DslCall(kind @ ("apply" | "pure"), builder, List(fn)) =>
        val (syms, prec) = sequence(builder)
        List(IRAlternative(syms, if (kind == "pure") Kind.Pure else Kind.Effectful, prec, pos(tree), Some(action(fn))))
      case DslCall("litAlt", _, List(arg)) =>
        stringLiteral(arg) match {
          case ""   => List(IRAlternative(Nil, Kind.Empty, None, pos(tree)))
          case text =>
            List(IRAlternative(List(IRTerm(LiteralPattern(text), None, pos(arg))), Kind.Pass, None, pos(tree)))
        }
      case DslCall("reAlt", _, List(arg)) =>
        List(IRAlternative(List(IRTerm(RegexPattern(regexLiteral(arg)), None, pos(arg))), Kind.Pass, None, pos(tree)))
      case DslCall("symAlt", _, List(arg)) => List(IRAlternative(List(sym(arg, None)), Kind.Pass, None, pos(tree)))
      case other                           =>
        fail(
          other,
          "expected alternatives: `all(...) { ... }`, `all(...).pure { ... }`, a symbol, or several of them combined with `||`"
        )
    }

    /** `all(s1, ..., sN)` optionally followed by `.prec(op)`. */
    private def sequence(tree: Tree): (List[Sym], Option[Sym]) = strip(tree) match {
      case DslCall("prec", inner, List(op)) =>
        val (syms, _) = sequence(inner)
        (syms, Some(sym(op, None)))
      case DslCall("all", _, args) => (args.map(sym(_, None)), None)
      case other                   => fail(other, "expected `all(...)`")
    }

    private def sym(tree: Tree, name: Option[String]): Sym = strip(tree) match {
      case id: Ident if nonTerminals.contains(id.symbol) => NtRef(nonTerminals(id.symbol)._1)
      case id: Ident if terminals.contains(id.symbol)    =>
        terminals(id.symbol)
      case DslCall("litSym", _, List(arg))    => IRTerm(LiteralPattern(stringLiteral(arg)), name, pos(tree))
      case DslCall("reSym", _, List(arg))     => IRTerm(RegexPattern(regexLiteral(arg)), name, pos(tree))
      case DslCall("terminal", _, List(arg))  => IRTerm(RegexPattern(stringLiteral(arg)), name, pos(tree))
      case DslCall("map", terminal, List(fn)) =>
        checkNoGrammarRefs(fn)
        sym(terminal, name) match {
          case t: IRTerm => t.copy(converters = t.converters :+ strip(fn))
          case _         => fail(tree, "`.map` is only available on terminals")
        }
      case DslCall("group", _, List(alts))        => Group(alternatives(alts))
      case DslCall("opt", _, List(s))             => Opt(sym(s, None))
      case t @ DslCall("rep", _, List(s))         => Rep(sym(s, None), atLeastOne = false, listOf(t))
      case t @ DslCall("rep1", _, List(s))        => Rep(sym(s, None), atLeastOne = true, listOf(t))
      case t @ DslCall("sepBy", _, List(s, sep))  => SepBy(sym(s, None), sym(sep, None), atLeastOne = false, listOf(t))
      case t @ DslCall("sepBy1", _, List(s, sep)) => SepBy(sym(s, None), sym(sep, None), atLeastOne = true, listOf(t))
      case DslCall("as", inner, Nil)              =>
        if (!allowAs)
          fail(
            tree,
            "`.as[C]` is only supported by `Grammar.grammar` (`Grammar.interpreted` collects repetitions into Lists)"
          )
        val target = typeArg(tree, symbolOf[hearth.kindlings.parser.Sym[?]])
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
