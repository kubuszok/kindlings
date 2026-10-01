package hearth.kindlings.parser
package internal.compiletime

import hearth.MacroCommons

import scala.collection.mutable

import GrammarIR.{Statement as IRStatement, Term as IRTerm, *}

/** Reads the grammar block (`g => { import g.*; val expr = nonTerminal[Int]; ...; expr ::= ...; expr }`) into a
  * [[GrammarIR.Grammar]], with the same code on Scala 2 and Scala 3.
  *
  * The block is destructured with Hearth's `DestructuredExpr`: local `val`s are `ValDefinition`s whose bindings are
  * shared (by identity) with every `LocalReference` to them, so grammar symbols are associated with their declarations
  * by binding, never by name. Actions and conversions are kept as compiler trees (`toUntypedExpr`) for the code
  * generation of the per-compiler bridges.
  */
private[parser] trait GrammarExtractor { this: MacroCommons =>

  /** @param body
    *   the grammar lambda (`Dsl[F] => NonTerminal[R]`)
    * @param allowAs
    *   whether repetitions may choose their collection with `.as[C]` (generated code only)
    */
  def extractGrammar(body: UntypedExpr, allowAs: Boolean): Grammar =
    new Extractor(allowAs).grammar(DestructuredExpr.parseUntyped(body))

  private def untypedOf(tpe: ??): UntypedType = {
    import tpe.Underlying as A
    UntypedType.fromTyped[A]
  }

  private def listOfType[A: Type]: UntypedType = UntypedType.fromTyped(using Type.of[List[A]])

  // the DSL types (`Sym` alone would be `GrammarIR.Sym`)
  private lazy val SymCtor = Type.Ctor1.of[hearth.kindlings.parser.Sym]
  private lazy val RepetitionCtor = Type.Ctor1.of[hearth.kindlings.parser.Repetition]
  private lazy val NonTerminalCtor = Type.Ctor1.of[hearth.kindlings.parser.NonTerminal]

  /** The type argument of `tpe` seen as `Sym[_]`/`Repetition[_]`/`NonTerminal[_]`. */
  private def symArg(tpe: ??): Option[UntypedType] = {
    import tpe.Underlying as A
    SymCtor.unapply(Type[A]).map(untypedOf)
  }
  private def repetitionArg(tpe: ??): Option[UntypedType] = {
    import tpe.Underlying as A
    RepetitionCtor.unapply(Type[A]).map(untypedOf)
  }
  private def nonTerminalArg(tpe: ??): Option[UntypedType] = {
    import tpe.Underlying as A
    NonTerminalCtor.unapply(Type[A]).map(untypedOf)
  }

  final private class Extractor(allowAs: Boolean) {

    private val nonTerminals = mutable.LinkedHashMap.empty[DestructuredExpr.LocalBinding, (Int, NonTerminalDecl)]
    private val terminals = mutable.HashMap.empty[DestructuredExpr.LocalBinding, IRTerm]
    private val statements = Vector.newBuilder[IRStatement]

    /** The grammar's own symbols (the DSL parameter, declared non-terminals and terminals) do not exist at run time in
      * generated code, so actions and conversions must not refer to them.
      */
    private var dslParam: Option[DestructuredExpr.Binding] = None

    private def positionOf(node: DestructuredExpr): Position = node.position.getOrElse(Environment.currentPosition)

    private def pos(node: DestructuredExpr): Pos = {
      val p = positionOf(node)
      Pos(p.fileName.getOrElse("<unknown>"), p.line, p.column)(p)
    }

    private def fail(node: DestructuredExpr, message: String): Nothing =
      Environment.reportErrorAndAbort(message, positionOf(node))

    def grammar(body: DestructuredExpr): Grammar = body match {
      case lambda: DestructuredExpr.Lambda if lambda.params.sizeIs == 1 =>
        dslParam = Some(lambda.params.head)
        val (stats, result) = lambda.body match {
          case block: DestructuredExpr.Block => (block.statements, block.result)
          case expr                          => (Nil, expr)
        }
        stats.foreach(statement)
        val root = result match {
          case NonTerminalRef(id) => id
          case other              => fail(other, "the grammar block must end with the start non-terminal")
        }
        Grammar(nonTerminals.values.map(_._2).toVector, statements.result(), root, pos(result))
      case other => fail(other, "`grammar` expects a lambda literal: grammar[R, F] { g => import g.*; ... }")
    }

    /** A reference to a non-terminal declared in this grammar block: its id. */
    private object NonTerminalRef {
      def unapply(node: DestructuredExpr): Option[Int] = node match {
        case ref: DestructuredExpr.LocalReference => nonTerminals.get(ref.binding).map(_._1)
        case _                                    => None
      }
    }

    /** Whether the call is a method of the DSL (`Dsl`, `NonTerminal`, `Terminal`, `Repetition`, `Alt`, `All*`, ...). */
    private def isDsl(call: DestructuredExpr.MethodCall): Boolean =
      call.method.Instance.plainPrint.startsWith("hearth.kindlings.parser.")

    /** `recv.name[targs](args)(args2)...` on a DSL method: `(name, receiver, all value args)` (vararg elements inlined).
      *
      * The receiver is the instance as it is (not `MethodCall.receiver`): the DSL's implicit conversions (`litAlt`,
      * `symAlt`, ...) carry meaning and are matched as calls themselves.
      */
    private object DslCall {
      def unapply(node: DestructuredExpr): Option[(String, DestructuredExpr, List[DestructuredExpr])] = node match {
        case call: DestructuredExpr.MethodCall if isDsl(call) =>
          val receiver = call.applied
            .collectFirst { case instance: DestructuredExpr.MethodCall.AppliedInstance => instance.value }
            .getOrElse(call)
          val args = call.applied
            .collect { case values: DestructuredExpr.MethodCall.AppliedValues => values.args }
            .flatten
            .flatMap {
              case varargs: DestructuredExpr.Varargs => varargs.elements
              case arg                               => List(arg)
            }
          Some((call.method.name, receiver, args))
        case _ => None
      }
    }

    private def statement(stat: DestructuredExpr): Unit = stat match {
      case _: DestructuredExpr.Import         => ()
      case vd: DestructuredExpr.ValDefinition =>
        val name = vd.binding.name
        vd.rhs match {
          case DslCall("nonTerminal", _, Nil) =>
            val prim = nonTerminalArg(vd.binding.tpe)
              .map(primKind)
              .getOrElse(hearth.kindlings.parser.internal.runtime.Prims.Boxed)
            nonTerminals(vd.binding) = nonTerminals.size -> NonTerminalDecl(name, pos(vd), prim)
          case rhs =>
            sym(rhs, Some(name)) match {
              case t: IRTerm => terminals(vd.binding) = t
              case _         =>
                fail(
                  vd,
                  "only `nonTerminal[A]` and terminal declarations (`terminal(...)`, optionally `.map(...)`) are allowed as vals in a grammar block"
                )
            }
        }
      case DslCall("::=", lhs, List(alts)) =>
        lhs match {
          case NonTerminalRef(id) => statements += Production(id, alternatives(alts), pos(stat))
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
        val name = (arg match {
          case call: DestructuredExpr.MethodCall          => call.method.name
          case singleton: DestructuredExpr.Singleton      => singleton.name.split('.').last
          case nd: DestructuredExpr.NonDestructurable     => nd.description.split('.').last
          case ref: DestructuredExpr.LocalReference       => ref.binding.name
          case _                                          => ""
        }).stripSuffix("$")
        if (!known.contains(name)) fail(arg, s"`$kind` expects one of the grammar flags: ${known.mkString(", ")}")
        statements += FlagSetting(name, kind == "enable", pos(stat))
      case local: DestructuredExpr.LocalDefinition if local.kind == "def" =>
        fail(stat, "helper methods are not supported in grammar blocks yet")
      case other =>
        fail(
          other,
          "grammar blocks may only contain declarations (`val x = nonTerminal[A]`, `val t = terminal(...)`), productions (`x ::= ...`), precedence declarations and `skip(...)`"
        )
    }

    private def checkNoGrammarRefs(fn: DestructuredExpr): Unit = {
      val grammarBindings: List[DestructuredExpr.Binding] = dslParam.toList ++ nonTerminals.keys ++ terminals.keys
      fn.findReferences(grammarBindings).headOption.foreach { ref =>
        Environment.reportErrorAndAbort(
          "grammar symbols (and the grammar DSL) can only be used in productions, not inside actions or `.map` functions",
          ref.position.getOrElse(positionOf(fn))
        )
      }
    }

    private def action(fn: DestructuredExpr): Action = {
      checkNoGrammarRefs(fn)
      val types = UntypedType.typeArguments(UntypedType.dealias(untypedOf(fn.tpe)))
      val paramTypes = types.init
      val used = fn match {
        case lambda: DestructuredExpr.Lambda =>
          val unused = lambda.unusedParams
          lambda.params.map(param => !unused.exists(_ eq param))
        case _ => paramTypes.map(_ => true)
      }
      Action(fn.toUntypedExpr, paramTypes, types.last, used)
    }

    /** The `Prims` kind of a non-terminal's value type. */
    private def primKind(tpe: UntypedType): Int = {
      import hearth.kindlings.parser.internal.runtime.Prims.*
      val t = UntypedType.as_??(UntypedType.dealias(tpe))
      import t.Underlying as T
      if (Type[T] =:= Type.of[Int]) IntKind
      else if (Type[T] =:= Type.of[Long]) LongKind
      else if (Type[T] =:= Type.of[Double]) DoubleKind
      else if (Type[T] =:= Type.of[Float]) FloatKind
      else if (Type[T] =:= Type.of[Boolean]) BooleanKind
      else if (Type[T] =:= Type.of[Char]) CharKind
      else if (Type[T] =:= Type.of[Short]) ShortKind
      else if (Type[T] =:= Type.of[Byte]) ByteKind
      else Boxed
    }

    /** The type argument of the node's type, extracted with `arg` (`symArg`/`repetitionArg`). */
    private def typeArg(node: DestructuredExpr, arg: ?? => Option[UntypedType]): UntypedType =
      arg(node.tpe).getOrElse(
        fail(node, s"unexpected type of a grammar symbol: ${node.tpe.plainPrint}")
      )

    /** The default collection of a repetition: a `List` of its elements. */
    private def listOf(node: DestructuredExpr): Collection = {
      val element = typeArg(node, repetitionArg)
      val elem = UntypedType.as_??(element)
      import elem.Underlying as E
      Collection(listOfType[E], element, pos(node))
    }

    private object StringLiteral {
      def unapply(node: DestructuredExpr): Option[String] = node match {
        case literal: DestructuredExpr.Literal =>
          literal.value match {
            case s: String => Some(s)
            case _         => None
          }
        case _ => None
      }
    }

    private def stringLiteral(node: DestructuredExpr): String = node match {
      case StringLiteral(s) => s
      case other            => fail(other, "expected a string literal (patterns are compiled at compile time)")
    }

    /** The literal of `"...".r` (through the implicit `augmentString`/`StringOps` wrapper). */
    private def regexLiteral(node: DestructuredExpr): String = node match {
      case call: DestructuredExpr.MethodCall if call.method.name == "r" =>
        call.applied
          .collectFirst { case instance: DestructuredExpr.MethodCall.AppliedInstance => instance.value }
          .map(unwrapString)
          .getOrElse(fail(node, "expected an inline regex literal: \"...\".r"))
      case other => fail(other, "expected an inline regex literal: \"...\".r")
    }
    private def unwrapString(node: DestructuredExpr): String = node match {
      case StringLiteral(s)                                              => s
      case call: DestructuredExpr.MethodCall if singleValueArg(call).isDefined => unwrapString(singleValueArg(call).get)
      case other                                                         => fail(other, "regex patterns must be string literals")
    }
    private def singleValueArg(call: DestructuredExpr.MethodCall): Option[DestructuredExpr] =
      call.applied.collect { case values: DestructuredExpr.MethodCall.AppliedValues => values.args } match {
        case List(List(arg)) => Some(arg)
        case _               => None
      }

    private def alternatives(node: DestructuredExpr): List[Alternative] = node match {
      case DslCall("||", left, List(right))                      => alternatives(left) ++ alternatives(right)
      case DslCall(kind @ ("apply" | "pure"), builder, List(fn)) =>
        val (syms, prec) = sequence(builder)
        List(
          Alternative(syms, if (kind == "pure") Kind.Pure else Kind.Effectful, prec, pos(node), Some(action(fn)))
        )
      case DslCall("litAlt", _, List(arg)) =>
        stringLiteral(arg) match {
          case ""   => List(Alternative(Nil, Kind.Empty, None, pos(node)))
          case text => List(Alternative(List(IRTerm(LiteralPattern(text), None, pos(arg))), Kind.Pass, None, pos(node)))
        }
      case DslCall("reAlt", _, List(arg)) =>
        List(Alternative(List(IRTerm(RegexPattern(regexLiteral(arg)), None, pos(arg))), Kind.Pass, None, pos(node)))
      case DslCall("symAlt", _, List(arg)) => List(Alternative(List(sym(arg, None)), Kind.Pass, None, pos(node)))
      case other                           =>
        fail(
          other,
          "expected alternatives: `all(...) { ... }`, `all(...).pure { ... }`, a symbol, or several of them combined with `||`"
        )
    }

    /** `all(s1, ..., sN)` optionally followed by `.prec(op)`. */
    private def sequence(node: DestructuredExpr): (List[Sym], Option[Sym]) = node match {
      case DslCall("prec", inner, List(op)) =>
        val (syms, _) = sequence(inner)
        (syms, Some(sym(op, None)))
      case DslCall("all", _, args) => (args.map(sym(_, None)), None)
      case other                   => fail(other, "expected `all(...)`")
    }

    private def sym(node: DestructuredExpr, name: Option[String]): Sym = node match {
      case NonTerminalRef(id)                                                       => NtRef(id)
      case ref: DestructuredExpr.LocalReference if terminals.contains(ref.binding) => terminals(ref.binding)
      case DslCall("litSym", _, List(arg))   => IRTerm(LiteralPattern(stringLiteral(arg)), name, pos(node))
      case DslCall("reSym", _, List(arg))    => IRTerm(RegexPattern(regexLiteral(arg)), name, pos(node))
      case DslCall("terminal", _, List(arg)) => IRTerm(RegexPattern(stringLiteral(arg)), name, pos(node))
      case DslCall("mapSlice", terminal, List(fn)) =>
        checkNoGrammarRefs(fn)
        sym(terminal, name) match {
          case t: IRTerm if t.converters.isEmpty && t.slicer.isEmpty && t.pattern.isInstanceOf[RegexPattern] =>
            t.copy(slicer = Some(fn.toUntypedExpr))
          case _: IRTerm =>
            fail(node, "`.mapSlice` must be the first conversion of a `terminal(...)` (before any `.map`)")
          case _ => fail(node, "`.mapSlice` is only available on terminals")
        }
      case DslCall("map", terminal, List(fn)) =>
        checkNoGrammarRefs(fn)
        sym(terminal, name) match {
          case t: IRTerm => t.copy(converters = t.converters :+ fn.toUntypedExpr)
          case _         => fail(node, "`.map` is only available on terminals")
        }
      case DslCall("group", _, List(alts))    => Group(alternatives(alts))
      case DslCall("opt", _, List(s))         => Opt(sym(s, None))
      case DslCall("rep", _, List(s))         => Rep(sym(s, None), atLeastOne = false, listOf(node))
      case DslCall("rep1", _, List(s))        => Rep(sym(s, None), atLeastOne = true, listOf(node))
      case DslCall("sepBy", _, List(s, sep))  => SepBy(sym(s, None), sym(sep, None), atLeastOne = false, listOf(node))
      case DslCall("sepBy1", _, List(s, sep)) => SepBy(sym(s, None), sym(sep, None), atLeastOne = true, listOf(node))
      case DslCall("as", inner, Nil)          =>
        if (!allowAs)
          fail(
            node,
            "`.as[C]` is only supported by `Grammar.grammar` (`Grammar.interpreted` collects repetitions into Lists)"
          )
        // aliases (e.g. Iron's `A :| C`) stay as written: Hearth's `Type.CtorN` matching sees through them (hearth#384)
        val target = typeArg(node, symArg)
        sym(inner, name) match {
          case r: Rep   => r.copy(collection = r.collection.copy(tpe = target, pos = pos(node)))
          case s: SepBy => s.copy(collection = s.collection.copy(tpe = target, pos = pos(node)))
          case _        => fail(node, "`.as[C]` is only available on repetitions (rep, rep1, sepBy, sepBy1)")
        }
      case other =>
        fail(
          other,
          "expected a grammar symbol declared in this grammar block, a string literal, an inline \"...\".r regex, or opt/rep/rep1/sepBy/sepBy1(...)"
        )
    }
  }
}
