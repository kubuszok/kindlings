package hearth.kindlings.parser

import hearth.MacroSuite

final class DiagnosticsSpec extends MacroSuite {

  group("compile-time grammar diagnostics") {

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
  }
}
