package hearth.kindlings.parser

import hearth.MacroSuite

import scala.util.{Success, Try}

/** Generated grammars keep the values of non-terminals with a primitive type unboxed (a `Long` stack of bits); these
  * tests go through every kind and every way such a value is produced and consumed.
  */
final class PrimsSpec extends MacroSuite {

  import PrimsSpec.*

  group("primitive non-terminals") {

    test("carry every primitive kind") {
      kinds.parse("77777777") ==> "int=7 long=7000000000 double=3.5 float=2.5 boolean=true char=7 short=7 byte=7"
    }

    test("work as the parse result, through pass-through alternatives, opt and repetitions") {
      sum.parse("1 2 3") ==> 6.0
      sum.parse("(1 2) 3") ==> 6.0
      maybe.parse("") ==> None
      maybe.parse("4") ==> Some(4)
      doubles.parse("1 2.5 x") ==> List(1.0, 2.5, -1.0)
    }

    test("take the result of effectful actions") {
      effectful.parse("1 2 3") ==> Success(6L)
    }

    test("give the same results as interpreted grammars") {
      List("1", "1 2", "(1 (2 3)) 4").foreach(input => sum.parse(input) ==> interpretedSum.parse(input))
    }
  }
}
object PrimsSpec {

  val kinds: Parser[Id, String] = Grammar.grammar[String, Id] { g =>
    import g.*
    val all_ = nonTerminal[String]
    val i = nonTerminal[Int]
    val l = nonTerminal[Long]
    val d = nonTerminal[Double]
    val f = nonTerminal[Float]
    val b = nonTerminal[Boolean]
    val c = nonTerminal[Char]
    val s = nonTerminal[Short]
    val y = nonTerminal[Byte]
    val digit = terminal("[0-9]")
    i ::= all(digit).pure(t => t.toInt)
    l ::= all(i).pure(n => n * 1000000000L)
    d ::= all(i).pure(n => n / 2.0)
    f ::= all(d).pure(x => (x - 1.0).toFloat)
    b ::= all(f).pure(x => x > 2.0f)
    c ::= all(i).pure(n => ('0' + n).toChar)
    s ::= all(i).pure(n => n.toShort)
    y ::= all(s).pure(n => n.toByte)
    all_ ::= all(i, l, d, f, b, c, s, y).pure((i, l, d, f, b, c, s, y) =>
      s"int=$i long=$l double=$d float=$f boolean=$b char=$c short=$s byte=$y"
    )
    all_
  }

  val sum: Parser[Id, Double] = Grammar.grammar[Double, Id] { g =>
    import g.*
    val total = nonTerminal[Double]
    val item = nonTerminal[Double]
    val number = nonTerminal[Double]
    skip(" +")
    total ::= all(rep1(item)).pure(xs => xs.sum)
    item ::= number || all("(", total, ")").pure((_, t, _) => t)
    number ::= all(terminal("[0-9]+").map(_.toDouble)).pure(n => n)
    total
  }

  val interpretedSum: Parser[Id, Double] = Grammar.interpreted[Double, Id] { g =>
    import g.*
    val total = nonTerminal[Double]
    val item = nonTerminal[Double]
    val number = nonTerminal[Double]
    skip(" +")
    total ::= all(rep1(item)).pure(xs => xs.sum)
    item ::= number || all("(", total, ")").pure((_, t, _) => t)
    number ::= all(terminal("[0-9]+").map(_.toDouble)).pure(n => n)
    total
  }

  val maybe: Parser[Id, Option[Int]] = Grammar.grammar[Option[Int], Id] { g =>
    import g.*
    val all_ = nonTerminal[Option[Int]]
    val n = nonTerminal[Int]
    n ::= all(terminal("[0-9]+")).pure(_.toInt)
    all_ ::= all(opt(n)).pure(o => o)
    all_
  }

  val doubles: Parser[Id, List[Double]] = Grammar.grammar[List[Double], Id] { g =>
    import g.*
    val all_ = nonTerminal[List[Double]]
    val d = nonTerminal[Double]
    skip(" +")
    d ::= all(terminal("[0-9]+(\\.[0-9]+)?")).pure(_.toDouble) || all("x").pure(_ => -1.0)
    all_ ::= all(rep(d)).pure(ds => ds)
    all_
  }

  val effectful: Parser[Try, Long] = Grammar.grammar[Long, Try] { g =>
    import g.*
    val total = nonTerminal[Long]
    val n = nonTerminal[Long]
    skip(" +")
    n ::= all(terminal("[0-9]+"))(t => Try(t.toLong))
    total ::= all(n).pure(x => x) || all(total, n)((a, b) => Try(a + b))
    total
  }
}
