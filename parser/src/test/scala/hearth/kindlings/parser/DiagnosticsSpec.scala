package hearth.kindlings.parser

import hearth.MacroSuite

final class DiagnosticsSpec extends MacroSuite {

  group("malformed grammars get helpful compile errors") {

    test("shift/reduce conflicts are reported with the production and the LR state") {
      compileErrors(
        """
        import hearth.kindlings.parser.*
        Grammar.grammar[Int, Id] { g =>
          import g.*
          val e = nonTerminal[Int]
          e ::= all(e, "+", e).pure((a, _, b) => a + b) || all("1").pure(_ => 1)
          e
        }
        """
      ).check("shift/reduce conflict on \"+\"", "e ::= e \"+\" e", "left/right/nonassoc")
    }

    test("reduce/reduce conflicts are reported") {
      compileErrors(
        """
        import hearth.kindlings.parser.*
        Grammar.grammar[String, Id] { g =>
          import g.*
          val s = nonTerminal[String]
          val a = nonTerminal[String]
          val b = nonTerminal[String]
          a ::= "x"
          b ::= "x"
          s ::= a || b
          s
        }
        """
      ).check("reduce/reduce conflict on end of input")
    }

    test("non-terminals without productions are reported") {
      compileErrors(
        """
        import hearth.kindlings.parser.*
        Grammar.grammar[String, Id] { g =>
          import g.*
          val s = nonTerminal[String]
          val missing = nonTerminal[String]
          s ::= all("x", missing).pure((_, m) => m)
          s
        }
        """
      ).check("non-terminal `missing` has no productions")
    }

    test("terminals matching the empty string are reported") {
      compileErrors(
        """
        import hearth.kindlings.parser.*
        Grammar.grammar[String, Id] { g =>
          import g.*
          val s = nonTerminal[String]
          val maybe = terminal("(Jr|Sr)?")
          s ::= maybe
          s
        }
        """
      ).check("terminal /(Jr|Sr)?/ matches the empty string")
    }

    test("unsupported regex features are reported") {
      compileErrors(
        """
        import hearth.kindlings.parser.*
        Grammar.grammar[String, Id] { g =>
          import g.*
          val s = nonTerminal[String]
          val word = terminal("\\bword")
          s ::= word
          s
        }
        """
      ).check("escape '\\b' is not supported")
    }

    test("effects without an engine are reported") {
      compileErrors(
        """
        import hearth.kindlings.parser.*
        Grammar.grammar[String, List] { g =>
          import g.*
          val s = nonTerminal[String]
          s ::= "x"
          s
        }
        """
      ).check("No ParserEngine for List")
    }

    test("non-literal patterns are reported") {
      compileErrors(
        """
        import hearth.kindlings.parser.*
        val pattern = "[a-z]+"
        Grammar.grammar[String, Id] { g =>
          import g.*
          val s = nonTerminal[String]
          val word = terminal(pattern)
          s ::= word
          s
        }
        """
      ).check("expected a string literal")
    }

    test("flags that are both enabled and disabled are reported") {
      compileErrors(
        """
        import hearth.kindlings.parser.*
        Grammar.grammar[String, Id] { g =>
          import g.*
          enable(RequireLALR)
          disable(RequireLALR)
          val s = nonTerminal[String]
          s ::= "x"
          s
        }
        """
      ).check("`RequireLALR` is both enabled and disabled")
    }

    test("contradicting parser requirements are reported") {
      compileErrors(
        """
        import hearth.kindlings.parser.*
        Grammar.grammar[String, Id] { g =>
          import g.*
          enable(RequireLL1)
          enable(RequireLALR)
          val s = nonTerminal[String]
          s ::= "x"
          s
        }
        """
      ).check("`RequireLL1` and `RequireLALR` ask for different parsers")
    }

    test("flags must be written in the grammar block") {
      compileErrors(
        """
        import hearth.kindlings.parser.*
        val flag: GrammarFlag = GrammarFlag.RequireLALR
        Grammar.grammar[String, Id] { g =>
          import g.*
          enable(flag)
          val s = nonTerminal[String]
          s ::= "x"
          s
        }
        """
      ).check("`enable` expects one of the grammar flags: ThrowingInRuntime, RequireLL1, RequireLALR")
    }

    test("non-terminals that cannot derive any finite input are reported") {
      compileErrors(
        """
        import hearth.kindlings.parser.*
        Grammar.grammar[String, Id] { g =>
          import g.*
          val s = nonTerminal[String]
          val loop = nonTerminal[String]
          loop ::= all("(", loop, ")").pure((_, l, _) => l)
          s ::= "x" || loop
          s
        }
        """
      ).check("non-terminal `loop` cannot derive any finite input")
    }

    test("skipped patterns used as terminals are reported") {
      compileErrors(
        """
        import hearth.kindlings.parser.*
        Grammar.grammar[String, Id] { g =>
          import g.*
          val s = nonTerminal[String]
          skip(" +")
          s ::= terminal(" +")
          s
        }
        """
      ).check("skipped pattern / +/ is also used as a terminal")
    }

    test("the empty literal inside a sequence is reported") {
      compileErrors(
        """
        import hearth.kindlings.parser.*
        Grammar.grammar[String, Id] { g =>
          import g.*
          val s = nonTerminal[String]
          s ::= all("x", "").pure((x, _) => x)
          s
        }
        """
      ).check("the empty literal \"\" is only allowed as a whole alternative")
    }

    test("malformed regexes are reported") {
      compileErrors(
        """
        import hearth.kindlings.parser.*
        Grammar.grammar[String, Id] { g =>
          import g.*
          val s = nonTerminal[String]
          s ::= terminal("(ab")
          s
        }
        """
      ).check("invalid terminal pattern")
    }

    test("statements other than declarations and productions are reported") {
      compileErrors(
        """
        import hearth.kindlings.parser.*
        Grammar.grammar[String, Id] { g =>
          import g.*
          val s = nonTerminal[String]
          val t = terminal("y")
          s ::= t
          t.map(_.length)
          s
        }
        """
      ).check("grammar blocks may only contain declarations")
    }

    test("helper methods are reported") {
      compileErrors(
        """
        import hearth.kindlings.parser.*
        Grammar.grammar[String, Id] { g =>
          import g.*
          val s = nonTerminal[String]
          def twice(x: Alt[String]) = x
          s ::= "x"
          s
        }
        """
      ).check("helper methods are not supported in grammar blocks")
    }

    test("a grammar block must end with its start symbol") {
      compileErrors(
        """
        import hearth.kindlings.parser.*
        Grammar.grammar[String, Id] { g =>
          import g.*
          val s = nonTerminal[String]
          s ::= "x"
          if (true) s else s
        }
        """
      ).check("the grammar block must end with the start non-terminal")
    }

    test("precedence of non-terminals is reported") {
      compileErrors(
        """
        import hearth.kindlings.parser.*
        Grammar.grammar[String, Id] { g =>
          import g.*
          val s = nonTerminal[String]
          left(s)
          s ::= "x"
          s
        }
        """
      ).check("precedence declarations accept only terminals")
    }
  }
}
