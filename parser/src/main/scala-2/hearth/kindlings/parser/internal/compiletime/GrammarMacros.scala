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
    val factories = collections.toList.map(code => c.untypecheck(code.factory))
    val codegen = new Codegen(out, collections)
    q"""_root_.hearth.kindlings.parser.internal.runtime.Builder.generated[..${targs.reverse}](
          _root_.scala.List(..$tables),
          $engine,
          new _root_.hearth.kindlings.parser.internal.runtime.GeneratedReductions {
            ${codegen.reduce}
            ${codegen.slice}
            ${codegen.hasLL}
            ${codegen.runLL}
            ${codegen.hasDescent}
            ${codegen.descend}
            ${codegen.hasStringLexer}
            ${codegen.lexString}
            protected def factories(): _root_.scala.Array[_root_.scala.Any] =
              _root_.scala.Array[_root_.scala.Any](..$factories)
          }
        )"""
  }

  /** The members of the generated `GeneratedReductions` (see [[CodegenPlan]]). */
  final private class Codegen(out: GrammarCompiler.Output, collections: Vector[Collections]) {

    private def fresh(name: String): TermName = TermName(c.freshName(name))
    private val MachineType = tq"_root_.hearth.kindlings.parser.internal.runtime.Machine"
    private val anyToAny = typeOf[Any => Any]
    private val anyAnyToAny = typeOf[(Any, Any) => Any]

    /** `fn(args)` with `fn` inlined when it is a function literal (arguments are bound to fresh vals first, so they
      * cannot see the lambda's parameter names), otherwise a call of its `apply`. The trees are untypechecked so that
      * they are re-typed (with fresh owners) where they are spliced.
      */
    private def applyFn(fn: Any, fnType: Type, args: List[Tree]): Tree = {
      val tree = fn.asInstanceOf[Tree]
      val paramTypes = fnType.dealias.typeArgs.init
      tree match {
        case Function(params, body) if params.size == args.size && paramTypes.size == args.size =>
          // unused parameters are not bound (their arguments are stack reads or placeholders, free of effects); the
          // typed tree tells which are used, the untypechecked one is spliced (its references are plain names)
          val usedParams = params.map(param => body.exists(_.symbol == param.symbol))
          c.untypecheck(tree) match {
            case Function(untypedParams, untypedBody) =>
              val used = untypedParams.zip(args).zip(paramTypes).zip(usedParams).collect {
                case (((param, arg), tpe), true) => (param, arg, tpe, fresh("arg"))
              }
              val bindArgs = used.map { case (_, arg, tpe, name) => q"val $name: ${TypeTree(tpe)} = $arg" }
              val bindParams = used.map { case (param, _, tpe, name) =>
                q"val ${param.name}: ${TypeTree(tpe)} = $name"
              }
              q"{ ..$bindArgs; ..$bindParams; $untypedBody }"
            case untyped => q"($untyped: ${TypeTree(fnType)}).apply(..$args)"
          }
        case _ => q"(${c.untypecheck(tree)}: ${TypeTree(fnType)}).apply(..$args)"
      }
    }

    /** Applies the `.map` chain, casting the value to each function's parameter type (the raw value is the matched
      * text, or the `.mapSlice` result).
      */
    private def convert(chain: List[Any], raw: Tree): Tree =
      chain.foldLeft(raw) { (acc, fn) =>
        val tree = fn.asInstanceOf[Tree]
        val fnType = tree.tpe.widen
        applyFn(tree, fnType, List(q"$acc.asInstanceOf[${TypeTree(fnType.dealias.typeArgs.head)}]"))
      }

    // --- reductions ----------------------------------------------------------------------------------------------

    private val m = fresh("m")
    private val vs = fresh("values")
    private val ps = fresh("prims")
    private val top = fresh("top")
    // which stacks the generated cases read (unused local vals would be warnings in the user's code)
    private var readsValues = false
    private var readsPrims = false

    /** The primitive value of `kind` whose bits are `bits` (see `Prims`). */
    private def decode(kind: Int, bits: Tree): Tree = {
      import hearth.kindlings.parser.internal.runtime.Prims.*
      kind match {
        case IntKind     => q"$bits.toInt"
        case LongKind    => bits
        case DoubleKind  => q"_root_.java.lang.Double.longBitsToDouble($bits)"
        case FloatKind   => q"_root_.java.lang.Float.intBitsToFloat($bits.toInt)"
        case BooleanKind => q"$bits != 0L"
        case CharKind    => q"$bits.toChar"
        case ShortKind   => q"$bits.toShort"
        case _           => q"$bits.toByte"
      }
    }

    /** The bits of the primitive value `value` of `kind`. */
    private def encode(kind: Int, value: Tree): Tree = {
      import hearth.kindlings.parser.internal.runtime.Prims.*
      kind match {
        case IntKind     => q"($value.asInstanceOf[_root_.scala.Int]).toLong"
        case LongKind    => q"$value.asInstanceOf[_root_.scala.Long]"
        case DoubleKind  => q"_root_.java.lang.Double.doubleToRawLongBits($value.asInstanceOf[_root_.scala.Double])"
        case FloatKind   => q"_root_.java.lang.Float.floatToRawIntBits($value.asInstanceOf[_root_.scala.Float]).toLong"
        case BooleanKind => q"if ($value.asInstanceOf[_root_.scala.Boolean]) 1L else 0L"
        case CharKind    => q"($value.asInstanceOf[_root_.scala.Char]).toLong"
        case ShortKind   => q"($value.asInstanceOf[_root_.scala.Short]).toLong"
        case _           => q"($value.asInstanceOf[_root_.scala.Byte]).toLong"
      }
    }

    /** The value of a reduction's `body`; `value(plan, i)` gives right-hand side position `i`'s value, `builder` the
      * builder a collection step appends to.
      */
    private def bodyTree(
        body: CodegenPlan.ReduceBody,
        value: (CodegenPlan.RhsPlan, Int) => Tree,
        builder: => Tree
    ): Tree =
      body match {
        case CodegenPlan.ReduceBody.User(action, rhs, _) =>
          val args = rhs.toList.zipWithIndex.map { case (plan, i) =>
            val tpe = TypeTree(action.paramTypes(i).asInstanceOf[Type])
            if (!action.used(i)) q"null.asInstanceOf[$tpe]"
            else q"${value(plan, i)}.asInstanceOf[$tpe]"
          }
          val fn = action.tree.asInstanceOf[Tree]
          applyFn(fn, fn.tpe.widen, args)
        case CodegenPlan.ReduceBody.Pass(rhs)         => value(rhs, 0)
        case CodegenPlan.ReduceBody.Const(text)       => Literal(Constant(text))
        case CodegenPlan.ReduceBody.OptNone           => q"_root_.scala.None"
        case CodegenPlan.ReduceBody.OptSome(rhs)      => q"_root_.scala.Some(${value(rhs, 0)})"
        case CodegenPlan.ReduceBody.Collect(id, step) =>
          val code = collections(id)
          def newBuilder = applyFn(code.newBuilder, anyToAny, List(q"collectionFactories($id)"))
          step match {
            case CodegenPlan.CollectionStep.Empty        => newBuilder
            case CodegenPlan.CollectionStep.One(element) =>
              applyFn(code.add, anyAnyToAny, List(newBuilder, value(element, 0)))
            case CodegenPlan.CollectionStep.Append(i, element) =>
              applyFn(code.add, anyAnyToAny, List(builder, value(element, i)))
          }
      }

    /** The value of a right-hand side symbol from its `raw` value (as on the value stack). */
    private def rhsValue(rhs: CodegenPlan.RhsPlan, raw: Tree): Tree = rhs match {
      case CodegenPlan.NtPlan(Some(id), _) => applyFn(collections(id).result, anyToAny, List(raw))
      case CodegenPlan.TermPlan(Some(id))  => convert(out.converters(id), raw)
      case _                               => raw
    }

    private def reduceCase(r: CodegenPlan.Reduce): Tree = {
      def raw(i: Int): Tree = { readsValues = true; q"$vs($top + ${i - r.len + 1})" }
      def bits(i: Int): Tree = { readsPrims = true; q"$ps($top + ${i - r.len + 1})" }

      /** The value of right-hand side position `i`. */
      def value(rhs: CodegenPlan.RhsPlan, i: Int): Tree = rhs match {
        case CodegenPlan.NtPlan(None, prim) if prim != 0 => decode(prim, bits(i))
        case _                                           => rhsValue(rhs, raw(i))
      }
      val newTop = q"$top - ${r.len}"
      val result: Tree = bodyTree(r.body, value, raw(0))
      val v = fresh("value")
      r.body match {
        case CodegenPlan.ReduceBody.User(_, _, true) =>
          cq"${r.p} => { val $v: _root_.scala.Any = $result; $m.suspend($newTop, ${r.lhs}, $v); true }"
        case _ if r.lhsPrim != 0 =>
          // the value stays unboxed: its bits go to the primitive stack
          val b = fresh("bits")
          val finish = r.goto match {
            case Some(state) => q"$m.reducedToPrim($newTop, $state, $b); false"
            case None        => q"$m.reducedPrim($newTop, ${r.lhs}, $b); false"
          }
          cq"${r.p} => { val $b: _root_.scala.Long = ${encode(r.lhsPrim, result)}; $finish }"
        case _ =>
          val finish = r.goto match {
            case Some(state) => q"$m.reducedTo($newTop, $state, $v); false"
            case None        => q"$m.reduced($newTop, ${r.lhs}, $v); false"
          }
          cq"${r.p} => { val $v: _root_.scala.Any = $result; $finish }"
      }
    }

    def reduce: Tree = {
      val p = fresh("p")
      val cases = out.reduces.toList.map(reduceCase)
      val stacks =
        (if (readsValues) List(q"val $vs = $m.stackValues") else Nil) ++
          (if (readsPrims) List(q"val $ps = $m.stackPrims") else Nil)
      q"""def reduce($p: _root_.scala.Int, $m: $MachineType): _root_.scala.Boolean = {
            ..$stacks
            val $top = $m.stackTop
            $p match {
              case ..$cases
              case _ => throw new _root_.java.lang.IllegalStateException("no reduction for production " + $p)
            }
          }"""
    }

    // --- lexer ---------------------------------------------------------------------------------------------------

    def slice: Tree = {
      val (token, input, start, end) = (fresh("token"), fresh("input"), fresh("start"), fresh("end"))
      val cases = out.slicers.toList.map { case (id, fn) =>
        val tree = fn.asInstanceOf[Tree]
        cq"$id => ${applyFn(tree, tree.tpe.widen, List(q"$input", q"$start", q"$end"))}"
      }
      q"""def slice(
            $token: _root_.scala.Int,
            $input: _root_.java.lang.String,
            $start: _root_.scala.Int,
            $end: _root_.scala.Int
          ): _root_.scala.Any = $token match {
            case ..$cases
            case _ => throw new _root_.java.lang.IllegalStateException("no slice conversion for token " + $token)
          }"""
    }

    // --- the recursive-descent parser ---------------------------------------------------------------------------

    def hasDescent: Tree = q"def hasDescent: _root_.scala.Boolean = ${out.descent.isDefined}"

    def descend: Tree = {
      val (dm, text) = (fresh("m"), fresh("text"))
      val body = out.descent match {
        case None => q"throw new _root_.java.lang.UnsupportedOperationException(${"no recursive-descent parser"})"
        case Some(program) => new DescentEmitter(program, dm, text).body
      }
      q"""def descend($dm: $MachineType, $text: _root_.java.lang.String): _root_.scala.Any = $body"""
    }

    /** Emits the methods of a [[DescentPlan.Program]] as local `def`s of `descend`. */
    final private class DescentEmitter(program: DescentPlan.Program, m: TermName, text: TermName) {
      private val bodies = out.reduces.map(r => r.p -> r).toMap
      private val slicers = out.slicers.toMap
      private val methods = program.methods.map { case (nt, _) => nt -> fresh(s"nt$nt") }.toMap
      private val scanners = program.scanners.map { case (t, _) => t -> fresh(s"scan$t") }.toMap

      private def consume(tok: DescentPlan.Tok): Tree = tok.read match {
        case DescentPlan.Read.OneChar(ch)       => q"$m.descentChar($ch, ${tok.token})"
        case DescentPlan.Read.Word(word)        => q"$m.descentWord($word, ${tok.token})"
        case DescentPlan.Read.Lexed             => q"$m.descentToken(${tok.token})"
        case DescentPlan.Read.Decided           => q"$m.descentSkip(1)"
        case DescentPlan.Read.DecidedWord(word) => q"$m.descentRest($word, ${tok.token})"
        case DescentPlan.Read.Scan              =>
          val (start, end) = (fresh("start"), fresh("end"))
          q"""{
                val $start = $m.descentScanStart()
                val $end = ${scanners(tok.token)}($start)
                if ($end >= 0) $m.descentTokenAt($start, $end) else $m.descentToken(${tok.token})
              }"""
      }

      /** The token's value, as the machine shifts it. */
      private def tokenValue(tok: DescentPlan.Tok): Tree =
        if (tok.sliced) {
          val fn = slicers(tok.token).asInstanceOf[Tree]
          q"""{
                ${consume(tok)}
                ${applyFn(fn, fn.tpe.widen, List(q"$text", q"$m.tokenStartIndex", q"$m.tokenEndIndex"))}
              }"""
        } else
          tok.literal match {
            case Some(lit) => q"{ ${consume(tok)}; $lit }"
            case None      => q"{ ${consume(tok)}; $text.substring($m.tokenStartIndex, $m.tokenEndIndex) }"
          }

      /** `def scan(from: Int): Int`: the end of the token at `from`, `-1` if it does not start there. */
      private def scanner(t: Int, lexer: CodegenPlan.Lexer): Tree = {
        val (from, len, i, acc, accEnd, state) =
          (fresh("from"), fresh("len"), fresh("i"), fresh("acc"), fresh("accEnd"), fresh("state"))
        q"""def ${scanners(t)}($from: _root_.scala.Int): _root_.scala.Int = {
              val $len = $text.length
              var $i = $from
              var $acc = -1
              var $accEnd = $from
              var $state = 0
              ${dfaLoop(lexer, text, len, i, acc, accEnd, state)}
              if ($acc < 0) -1 else $accEnd
            }"""
      }

      private def item(it: DescentPlan.Item, acc: Option[TermName]): Tree = it match {
        case tok: DescentPlan.Tok      => tokenValue(tok)
        case DescentPlan.Call(nt)      => q"${methods(nt)}()"
        case DescentPlan.Inline(_, cd) => code(cd)
        case DescentPlan.Acc           => q"${acc.get}"
      }

      private def production(prod: DescentPlan.Prod, acc: Option[TermName]): Tree = {
        val r = bodies(prod.p)
        val names = prod.items.map(_ => fresh("a"))
        val stats = prod.items.zip(names).zip(prod.used).flatMap {
          case ((DescentPlan.Acc, _), _)          => Nil
          case ((tok: DescentPlan.Tok, _), false) => List(consume(tok))
          case ((it, _), false)                   => List(item(it, acc))
          case ((it, name), true)                 => List(q"val $name = ${item(it, acc)}")
        }
        def local(i: Int): Tree = prod.items(i) match {
          case DescentPlan.Acc => q"${acc.get}"
          case _               => q"${names(i)}"
        }
        val result = bodyTree(r.body, (plan, i) => rhsValue(plan, local(i)), local(0))
        q"{ ..$stats; ($result: _root_.scala.Any) }"
      }

      private def decision(d: DescentPlan.Decision): Tree = {
        val fallback = if (d.failByDefault) q"$m.descentFail()" else q"-1"
        def result(k: Int): Tree = if (k < 0) fallback else Literal(Constant(k))
        def ints(xs: List[Int]): Tree =
          if (xs.size == 1) Literal(Constant(xs.head)) else Alternative(xs.map(x => Literal(Constant(x))))
        val c = fresh("c")
        val tokenCases = d.tokens.map { case (ts, k) => cq"${ints(ts)} => ${result(k)}" }
        val byToken = q"$m.descentLex() match { case ..$tokenCases; case _ => $fallback }"
        val charCases = d.chars.map { case (cs, k) => cq"${ints(cs)} => ${result(k)}" }
        q"""{
              val $c: _root_.scala.Int = $m.descentPeek()
              $c match {
                case ..$charCases
                case -1 => ${result(d.eof)}
                case _ => $byToken
              }
            }"""
      }

      private def code(cd: DescentPlan.Code): Tree = cd match {
        case DescentPlan.Single(prod)       => production(prod, None)
        case DescentPlan.Choose(dec, prods) =>
          val cases = prods.toList.zipWithIndex.map { case (prod, k) => cq"$k => ${production(prod, None)}" }
          q"(${decision(dec)} match { case ..$cases; case _ => $m.descentFail() }): _root_.scala.Any"
        case DescentPlan.Loop(base, dec, append) =>
          val acc = fresh("acc")
          q"""{
                var $acc: _root_.scala.Any = ${production(base, None)}
                while (${decision(dec)} == 0) $acc = ${production(append, Some(acc))}
                $acc
              }"""
      }

      def body: Tree = {
        val defs = program.methods.toList.map { case (nt, cd) =>
          val v = fresh("v")
          val impl =
            if (program.recursive(nt))
              q"{ $m.descentEnter(); val $v: _root_.scala.Any = ${code(cd)}; $m.descentExit(); $v }"
            else code(cd)
          q"def ${methods(nt)}(): _root_.scala.Any = $impl"
        }
        val scanDefs = program.scanners.toList.map { case (t, lexer) => scanner(t, lexer) }
        val result = fresh("result")
        q"""{
              ..$scanDefs
              ..$defs
              val $result: _root_.scala.Any = ${item(program.root, None)}
              $m.descentEnd()
              $result
            }"""
      }
    }

    // --- the LL(1) program --------------------------------------------------------------------------------------------

    def hasLL: Tree = q"def hasLL: _root_.scala.Boolean = ${out.ll.isDefined}"

    def runLL: Tree = {
      val (lm, budget, state, steps, status, r, e) =
        (fresh("m"), fresh("budget"), fresh("state"), fresh("steps"), fresh("status"), fresh("r"), fresh("e"))
      val Machine = q"_root_.hearth.kindlings.parser.internal.runtime.Machine"
      val body = out.ll match {
        case None          => q"throw new _root_.java.lang.UnsupportedOperationException(${"no LL(1) program"})"
        case Some(program) =>
          val readIfNeeded =
            q"if ($lm.lookaheadToken < 0) { val $r = $lm.readToken($state); if ($r != $Machine.Done) $status = $r }"
          def tokens(ts: List[Int]): Tree =
            if (ts.size == 1) Literal(Constant(ts.head)) else Alternative(ts.map(t => Literal(Constant(t))))
          val cases = program.ops.toList.zipWithIndex.map {
            case (LLProgram.Expect(t, next), i) =>
              val generic = q"""{
                  $readIfNeeded
                  if ($status == -1) {
                    if ($lm.lookaheadToken == $t) { $lm.shiftToken(); $state = $next }
                    else $status = $lm.llUnexpected($state)
                  }
                }"""
              out.singleCharTokens.get(t) match {
                case Some(ch) =>
                  cq"$i => if ($lm.lookaheadToken < 0 && $lm.expectChar($ch, $t)) $state = $next else $generic"
                case None => cq"$i => $generic"
              }
            case (LLProgram.Call(_, entry, next), i) => cq"$i => { $lm.pushFrame($next); $state = $entry }"
            case (LLProgram.Reduce(p, next), i)      =>
              cq"$i => { $state = $next; if (reduce($p, $lm)) $status = $Machine.Effect }"
            case (LLProgram.Return, i)                       => cq"$i => $state = $lm.popFrame()"
            case (LLProgram.Predict(choices, default, _), i) =>
              val choiceCases = choices.map { case (ts, target) => cq"${tokens(ts)} => $state = $target" }
              val fallback = default.fold(q"$status = $lm.llUnexpected($state)")(d => q"$state = $d")
              cq"""$i => {
                  $readIfNeeded
                  if ($status == -1) $lm.lookaheadToken match {
                    case ..$choiceCases
                    case _ => $fallback
                  }
                }"""
            case (LLProgram.Accept, i) =>
              cq"""$i => {
                  $readIfNeeded
                  if ($status == -1) {
                    if ($lm.lookaheadToken == 0) $status = $lm.llAccept() else $status = $lm.llUnexpected($state)
                  }
                }"""
          }
          q"""{
                var $state = $lm.llResumeState
                var $steps = 0
                var $status = -1
                try {
                  while ($status == -1) {
                    if ($steps >= $budget) $status = $Machine.Yield
                    else {
                      $steps += 1
                      $state match {
                        case ..$cases
                        case _ => throw new _root_.java.lang.IllegalStateException("no LL state " + $state)
                      }
                    }
                  }
                } catch {
                  case $e: _root_.hearth.kindlings.parser.internal.runtime.RejectedValue =>
                    $status = $lm.llRejected($e.getMessage)
                }
                $lm.llSuspendAt($state)
                $status
              }"""
      }
      q"def runLL($lm: $MachineType, $budget: _root_.scala.Int): _root_.scala.Int = $body"
    }

    def hasStringLexer: Tree = q"def hasStringLexer: _root_.scala.Boolean = ${out.lexer.isDefined}"

    def lexString: Tree = {
      val (lm, text, from) = (fresh("m"), fresh("text"), fresh("from"))
      val body = out.lexer match {
        case None        => q"throw new _root_.java.lang.UnsupportedOperationException(${"no generated lexer"})"
        case Some(lexer) => lexerBody(lexer, lm, text, from)
      }
      q"""def lexString($lm: $MachineType, $text: _root_.java.lang.String, $from: _root_.scala.Int): _root_.scala.Int =
            $body"""
    }

    private def inRanges(ch: Tree, ranges: List[(Int, Int)]): Tree =
      ranges
        .map { case (lo, hi) =>
          if (lo == hi) q"$ch == $lo" else q"$ch >= $lo && $ch <= $hi"
        }
        .reduceLeftOption((a, b) => q"$a || $b")
        .getOrElse(q"false")

    /** Membership test using the set or its complement, whichever has fewer ranges. */
    private def member(ch: Tree, ranges: List[(Int, Int)]): Tree = {
      val complement = CodegenPlan.complement(ranges)
      if (complement.size < ranges.size) q"!(${inRanges(ch, complement)})" else inRanges(ch, ranges)
    }

    /** `while (state >= 0) state match { ... }`: runs the DFA of `lexer` over `text` from `i`, recording the last
      * accepted token and its end in `acc` / `accEnd`.
      */
    private def dfaLoop(
        lexer: CodegenPlan.Lexer,
        text: TermName,
        len: TermName,
        i: TermName,
        acc: TermName,
        accEnd: TermName,
        state: TermName
    ): Tree = {
      def emit(node: CodegenPlan.LexNode): Tree = node match {
        case CodegenPlan.LexNode.Block(nodes)     => q"{ ..${nodes.map(emit)} }"
        case CodegenPlan.LexNode.SelfLoop(ranges) =>
          val d = fresh("d")
          q"while ($i < $len && { val $d = $text.charAt($i); ${member(q"$d", ranges)} }) $i += 1"
        case CodegenPlan.LexNode.Accept(token)              => q"$acc = $token; $accEnd = $i"
        case CodegenPlan.LexNode.Goto(target)               => q"$state = $target"
        case CodegenPlan.LexNode.Dispatch(cases, otherwise) =>
          val ch = fresh("c")
          val asciiCases = cases.flatMap { case (ranges, next) =>
            val chars = ranges.flatMap { case (lo, hi) => (lo to math.min(hi, 127)).toList }
            if (chars.isEmpty) Nil
            else {
              val pattern =
                if (chars.size == 1) Literal(Constant(chars.head.toChar))
                else Alternative(chars.map(ch => Literal(Constant(ch.toChar))))
              List(cq"$pattern => { $i += 1; ${emit(next)} }")
            }
          }
          val nonAscii = cases.flatMap { case (ranges, next) =>
            val high = ranges.collect { case (lo, hi) if hi >= 128 => (math.max(lo, 128), hi) }
            if (high.isEmpty) Nil else List(high -> next)
          }
          val fallback = nonAscii.foldRight(emit(otherwise)) { case ((ranges, next), elseBranch) =>
            q"if (${inRanges(q"$ch", ranges)}) { $i += 1; ${emit(next)} } else $elseBranch"
          }
          q"""if ($i < $len) {
                val $ch: _root_.scala.Char = $text.charAt($i)
                $ch match {
                  case ..$asciiCases
                  case _ => $fallback
                }
              } else ${emit(otherwise)}"""
      }
      val stateCases = lexer.states.toList.map { case (s, node) => cq"$s => ${emit(node)}" }
      q"""while ($state >= 0) $state match {
            case ..$stateCases
            case _ => $state = -1
          }"""
    }

    private def lexerBody(lexer: CodegenPlan.Lexer, lm: TermName, text: TermName, from: TermName): Tree = {
      val (len, p, result, i, acc, accEnd, state) =
        (fresh("len"), fresh("p"), fresh("result"), fresh("i"), fresh("acc"), fresh("accEnd"), fresh("state"))
      // runs of chars that are always skipped text (whitespace) are skipped without the DFA
      val skipRuns =
        if (lexer.simpleSkip.isEmpty) Nil
        else {
          val d = fresh("d")
          List(q"while ($p < $len && { val $d = $text.charAt($p); ${inRanges(q"$d", lexer.simpleSkip)} }) $p += 1")
        }
      q"""{
            val $len = $text.length
            var $p = $from
            var $result = -2
            while ($result == -2) {
              ..$skipRuns
              if ($p >= $len) {
                $lm.token(0, $p, $p)
                $result = _root_.hearth.kindlings.parser.internal.runtime.Machine.Done
              } else {
                var $i = $p
                var $acc = -1
                var $accEnd = $p
                var $state = 0
                ${dfaLoop(lexer, text, len, i, acc, accEnd, state)}
                if ($acc < 0) {
                  $lm.lexError($p)
                  $result = _root_.hearth.kindlings.parser.internal.runtime.Machine.Error
                } else if ($acc >= ${lexer.skipFrom}) $p = $accEnd
                else {
                  $lm.token($acc, $p, $accEnd)
                  $result = _root_.hearth.kindlings.parser.internal.runtime.Machine.Done
                }
              }
            }
            $result
          }"""
    }
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
        case Right(code) if code.rejectable && !out.flags("ThrowingInRuntime") && !hasErrorChannel =>
          val shown = coll.tpe.asInstanceOf[Type].toString
          val msg =
            "`.as[" + shown + "]`: " + shown + " has a smart constructor that can reject the repeated values, which needs an effect whose error channel reports it (Option, Try, Either[E, *], Future, a cats-effect F, ...) instead of " + effect + " (no ErrorChannel[" + effect + "]). " +
              "To throw the rejection as a ParseError instead, opt in with `enable(ThrowingInRuntime)` in the grammar block."
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
            val prim = primKind(vd.symbol.info.baseType(symbolOf[NonTerminal[?]]).typeArgs.head)
            nonTerminals(vd.symbol) = nonTerminals.size -> NonTerminalDecl(name, pos(vd), prim)
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
      case DslCall(kind @ ("enable" | "disable"), _, List(arg)) =>
        val known = hearth.kindlings.parser.GrammarFlag.all.map(_.name)
        val name = strip(arg) match {
          case t: RefTree if t.symbol != null => t.symbol.name.decodedName.toString.stripSuffix("$")
          case _                              => ""
        }
        if (!known.contains(name))
          fail(arg, s"`$kind` expects one of the grammar flags: ${known.mkString(", ")}")
        statements += FlagSetting(name, kind == "enable", pos(stat))
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

    /** The `Prims` kind of a non-terminal's value type. */
    private def primKind(tpe: Type): Int = {
      import hearth.kindlings.parser.internal.runtime.Prims.*
      val t = tpe.dealias
      if (t =:= definitions.IntTpe) IntKind
      else if (t =:= definitions.LongTpe) LongKind
      else if (t =:= definitions.DoubleTpe) DoubleKind
      else if (t =:= definitions.FloatTpe) FloatKind
      else if (t =:= definitions.BooleanTpe) BooleanKind
      else if (t =:= definitions.CharTpe) CharKind
      else if (t =:= definitions.ShortTpe) ShortKind
      else if (t =:= definitions.ByteTpe) ByteKind
      else Boxed
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
      case DslCall("litSym", _, List(arg))         => IRTerm(LiteralPattern(stringLiteral(arg)), name, pos(tree))
      case DslCall("reSym", _, List(arg))          => IRTerm(RegexPattern(regexLiteral(arg)), name, pos(tree))
      case DslCall("terminal", _, List(arg))       => IRTerm(RegexPattern(stringLiteral(arg)), name, pos(tree))
      case DslCall("mapSlice", terminal, List(fn)) =>
        checkNoGrammarRefs(fn)
        sym(terminal, name) match {
          case t: IRTerm if t.converters.isEmpty && t.slicer.isEmpty && t.pattern.isInstanceOf[RegexPattern] =>
            t.copy(slicer = Some(strip(fn)))
          case _: IRTerm =>
            fail(tree, "`.mapSlice` must be the first conversion of a `terminal(...)` (before any `.map`)")
          case _ => fail(tree, "`.mapSlice` is only available on terminals")
        }
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
