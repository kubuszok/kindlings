package hearth.kindlings.parser

import hearth.MacroSuite

/** `enable(RequireLL1)`: grammars that are LL(1) compile, the others fail with a plain explanation of each problem. */
final class LL1Spec extends MacroSuite {

  import LL1Spec.*

  group("RequireLL1 accepts LL(1) grammars") {

    test("JSON: choices by first token, sepBy lists closed by their own token") {
      json.parse("""{"a": [1, true, null], "b": {}}""") ==> "{a:[1,true,null],b:{}}"
    }
  }

  group("RequireLL1 explains why a grammar is not LL(1)") {

    test("direct left recursion") {
      compileErrors(
        """
        import hearth.kindlings.parser.*
        Grammar.grammar[Int, Id] { g =>
          import g.*
          enable(RequireLL1)
          val e = nonTerminal[Int]
          val t = terminal("[0-9]+").map(_.toInt)
          e ::= all(e, "+", t).pure((a, _, b) => a + b) || all(t).pure(n => n)
          e
        }
        """
      ).check(
        "this grammar is not LL(1)",
        "`e` can start with itself: `e ::= e \"+\" t`",
        "left recursion",
        "`list ::= all(item, rep(all(\",\", item)))`"
      )
    }

    test("indirect left recursion") {
      compileErrors(
        """
        import hearth.kindlings.parser.*
        Grammar.grammar[String, Id] { g =>
          import g.*
          enable(RequireLL1)
          val a = nonTerminal[String]
          val b = nonTerminal[String]
          a ::= all(b, "x").pure((b, _) => b)
          b ::= all(a, "y").pure((a, _) => a) || "z"
          a
        }
        """
      ).check("`a` can start with itself through a -> b -> a")
    }

    test("alternatives with a common beginning") {
      compileErrors(
        """
        import hearth.kindlings.parser.*
        Grammar.grammar[String, Id] { g =>
          import g.*
          enable(RequireLL1)
          val s = nonTerminal[String]
          s ::= all("x", "y").pure((_, _) => "xy") || all("x", "z").pure((_, _) => "xz")
          s
        }
        """
      ).check(
        "When the next token is \"x\", the parser cannot tell which alternative of `s` to use",
        "`s ::= \"x\" \"y\"`",
        "`s ::= \"x\" \"z\"`",
        "can both start with it",
        "Move the common beginning out of the alternatives"
      )
    }

    test("an empty alternative whose next token could also start another one") {
      compileErrors(
        """
        import hearth.kindlings.parser.*
        Grammar.grammar[String, Id] { g =>
          import g.*
          enable(RequireLL1)
          val s = nonTerminal[String]
          val a = nonTerminal[String]
          a ::= "x" || ""
          s ::= all(a, "x").pure((a, x) => a + x)
          s
        }
        """
      ).check(
        "`a ::= \"x\"` (",
        "starts with it, but `a ::= \"\" (nothing)`",
        "can match nothing, and \"x\" can also come right after `a`",
        "drop the empty alternative"
      )
    }

    test("an optional part followed by the same token") {
      compileErrors(
        """
        import hearth.kindlings.parser.*
        Grammar.grammar[String, Id] { g =>
          import g.*
          enable(RequireLL1)
          val s = nonTerminal[String]
          s ::= all(opt("x"), "x").pure((_, x) => x)
          s
        }
        """
      ).check(
        "opt(\"x\") may be skipped: when the next token is \"x\", it could be the start of the optional \"x\" or what comes after it"
      )
    }

    test("a repetition followed by the same token") {
      compileErrors(
        """
        import hearth.kindlings.parser.*
        Grammar.grammar[String, Id] { g =>
          import g.*
          enable(RequireLL1)
          val s = nonTerminal[String]
          s ::= all(rep("x"), "x").pure((_, x) => x)
          s
        }
        """
      ).check("the parser cannot tell whether the repetition continues", "Add a separator or terminator")
    }

    test("a list separator that can also follow the list") {
      compileErrors(
        """
        import hearth.kindlings.parser.*
        Grammar.grammar[String, Id] { g =>
          import g.*
          enable(RequireLL1)
          val s = nonTerminal[String]
          s ::= all(sepBy1("x", ","), ",", "end").pure((_, _, e) => e)
          s
        }
        """
      ).check("when the next token after an element of sepBy1(\"x\", \",\") is \",\", it could be the separator")
    }

    test("a repetition of something that can be empty") {
      compileErrors(
        """
        import hearth.kindlings.parser.*
        Grammar.grammar[String, Id] { g =>
          import g.*
          enable(RequireLL1)
          val s = nonTerminal[String]
          s ::= all(rep(opt("x")), "y").pure((_, y) => y)
          s
        }
        """
      ).check("the repeated part of rep(opt(\"x\")) can match nothing")
    }

    test("precedence declarations get a note") {
      compileErrors(
        """
        import hearth.kindlings.parser.*
        Grammar.grammar[Int, Id] { g =>
          import g.*
          enable(RequireLL1)
          val e = nonTerminal[Int]
          val t = terminal("[0-9]+").map(_.toInt)
          left("+")
          e ::= all(e, "+", e).pure((a, _, b) => a + b) || all(t).pure(n => n)
          e
        }
        """
      ).check("precedence declarations (left/right/nonassoc) only settle choices for the LALR parser")
    }
  }
}
object LL1Spec {

  val json: Parser[Id, String] = Grammar.grammar[String, Id] { g =>
    import g.*
    enable(RequireLL1)
    val value = nonTerminal[String]
    val member = nonTerminal[String]
    val string = terminal("\"[a-z]*\"").map(s => s.substring(1, s.length - 1))
    val number = terminal("-?[0-9]+")
    skip("[ \\n]+")
    value ::= (
      all("{", sepBy(member, ","), "}").pure((_, ms, _) => ms.mkString("{", ",", "}")) ||
        all("[", sepBy(value, ","), "]").pure((_, vs, _) => vs.mkString("[", ",", "]")) ||
        all(string).pure(s => s) || all(number).pure(n => n) ||
        all("true").pure(_ => "true") || all("false").pure(_ => "false") || all("null").pure(_ => "null")
    )
    member ::= all(string, ":", value).pure((k, _, v) => k + ":" + v)
    value
  }
}
