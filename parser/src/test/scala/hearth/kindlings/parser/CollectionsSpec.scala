package hearth.kindlings.parser

import _root_.cats.data.{Chain, NonEmptyList}
import hearth.MacroSuite

import scala.collection.immutable.ArraySeq
import scala.collection.mutable.ArrayBuffer

final class CollectionsSpec extends MacroSuite {

  import CollectionsSpec.*

  group("repetitions") {

    test("are collected into Lists by default, also when nested or optional") {
      lists.parse("[1 2] [] [3]") ==> List(List(1, 2), Nil, List(3))
      optional.parse("") ==> None
      optional.parse("x x") ==> Some(List("x", "x"))
    }

    test("are collected into the collection chosen with .as[C]") {
      vectors.parse("1, 2, 3") ==> Vector(1, 2, 3)
      vectors.parse("") ==> Vector.empty
      sets.parse("a b a c b") ==> Set("a", "b", "c")
      arrays.parse("4 5 6").toList ==> List(4, 5, 6)
      buffers.parse("1;2") ==> ArrayBuffer(1, 2)
      arraySeqs.parse("[1, 2, 3]") ==> ArraySeq(1, 2, 3)
    }

    test("are collected into a wider element type") {
      widened.parse("1 2") ==> List[Any](1, 2)
    }

    test("are collected into collections nested in each other") {
      nested.parse("1 2; 3; 4 5 6") ==> Vector(List(1, 2), List(3), List(4, 5, 6))
    }

    test("are collected into collections provided by classpath extensions (cats)") {
      nonEmpty.parse("1 2 3") ==> Right(NonEmptyList.of(1, 2, 3))
      chains.parse("1 2") ==> Chain(1, 2)
    }

    test("rejected by a smart constructor are reported through the error channel") {
      val error = nonEmpty.parse("").swap.getOrElse(fail("expected a Left"))
      assert(error.getMessage.startsWith("Invalid cats.data.NonEmptyList[scala.Int]"), error.getMessage)
      assert(nonEmptyTry.parse("").isFailure)
      nonEmptyTry.parse("1").get ==> NonEmptyList.of(1)
    }

    test("rejected by a smart constructor are thrown only with the explicit opt-in") {
      nonEmptyThrowing.parse("4") ==> NonEmptyList.of(4)
      val error = intercept[ParseError](nonEmptyThrowing.parse(""))
      assert(error.getMessage.startsWith("Invalid cats.data.NonEmptyList[scala.Int]"), error.getMessage)
    }

    test("with smart constructors are rejected in effects without an error channel") {
      compileErrors(
        """
        import hearth.kindlings.parser.*
        import cats.data.NonEmptyList
        Grammar.grammar[NonEmptyList[String], Id] { g =>
          import g.*
          val s = nonTerminal[NonEmptyList[String]]
          s ::= all(rep("x").as[NonEmptyList[String]]).pure(l => l)
          s
        }
        """
      ).check("has a smart constructor that can reject the repeated values")
    }

    test("pass the collection through groups and optional symbols") {
      optionalVector.parse("") ==> None
      optionalVector.parse("1 2") ==> Some(Vector(1, 2))
    }

    test("of unsupported types are rejected") {
      compileErrors(
        """
        import hearth.kindlings.parser.*
        Grammar.grammar[Int, Id] { g =>
          import g.*
          val s = nonTerminal[Int]
          s ::= all(rep("x").as[Int]).pure(n => n)
          s
        }
        """
      ).check("Int is not a supported collection")
    }

    test("into collections of other elements are rejected") {
      compileErrors(
        """
        import hearth.kindlings.parser.*
        Grammar.grammar[Vector[Int], Id] { g =>
          import g.*
          val s = nonTerminal[Vector[Int]]
          s ::= all(rep("x").as[Vector[Int]]).pure(v => v)
          s
        }
        """
      ).check("which is not a subtype of the collection's element type")
    }

    test("into maps are rejected") {
      compileErrors(
        """
        import hearth.kindlings.parser.*
        Grammar.grammar[Map[String, Int], Id] { g =>
          import g.*
          val s = nonTerminal[Map[String, Int]]
          s ::= all(rep("x").as[Map[String, Int]]).pure(m => m)
          s
        }
        """
      ).check("repetitions cannot be collected into maps")
    }

    test("with .as[C] are rejected by interpreted grammars") {
      compileErrors(
        """
        import hearth.kindlings.parser.*
        Grammar.interpreted[Vector[String], Id] { g =>
          import g.*
          val s = nonTerminal[Vector[String]]
          s ::= all(rep("x").as[Vector[String]]).pure(v => v)
          s
        }
        """
      ).check("`.as[C]` is only supported by `Grammar.grammar`")
    }
  }
}
object CollectionsSpec {

  val lists: Parser[Id, List[List[Int]]] = Grammar.grammar[List[List[Int]], Id] { g =>
    import g.*
    val groups = nonTerminal[List[List[Int]]]
    val group = nonTerminal[List[Int]]
    val num = terminal("[0-9]+").map(_.toInt)
    skip(" +")
    groups ::= all(rep(group)).pure(gs => gs)
    group ::= all("[", rep(num), "]").pure((_, ns, _) => ns)
    groups
  }

  val optional: Parser[Id, Option[List[String]]] = Grammar.grammar[Option[List[String]], Id] { g =>
    import g.*
    val s = nonTerminal[Option[List[String]]]
    skip(" +")
    s ::= all(opt(rep1("x"))).pure(o => o)
    s
  }

  val vectors: Parser[Id, Vector[Int]] = Grammar.grammar[Vector[Int], Id] { g =>
    import g.*
    val s = nonTerminal[Vector[Int]]
    val num = terminal("[0-9]+").map(_.toInt)
    skip(" +")
    s ::= all(sepBy(num, ",").as[Vector[Int]]).pure(v => v)
    s
  }

  val sets: Parser[Id, Set[String]] = Grammar.grammar[Set[String], Id] { g =>
    import g.*
    val s = nonTerminal[Set[String]]
    skip(" +")
    s ::= all(rep1(terminal("[a-z]+")).as[Set[String]]).pure(v => v)
    s
  }

  val arrays: Parser[Id, Array[Int]] = Grammar.grammar[Array[Int], Id] { g =>
    import g.*
    val s = nonTerminal[Array[Int]]
    val num = terminal("[0-9]+").map(_.toInt)
    skip(" +")
    s ::= all(rep(num).as[Array[Int]]).pure(a => a)
    s
  }

  val buffers: Parser[Id, ArrayBuffer[Int]] = Grammar.grammar[ArrayBuffer[Int], Id] { g =>
    import g.*
    val s = nonTerminal[ArrayBuffer[Int]]
    val num = terminal("[0-9]+").map(_.toInt)
    s ::= all(sepBy1(num, ";").as[ArrayBuffer[Int]]).pure(b => b)
    s
  }

  val arraySeqs: Parser[Id, ArraySeq[Int]] = Grammar.grammar[ArraySeq[Int], Id] { g =>
    import g.*
    val list = nonTerminal[ArraySeq[Int]]
    val num = terminal("[0-9]+").map(_.toInt)
    skip(" +")
    list ::= all("[", sepBy(num, ",").as[ArraySeq[Int]], "]").pure((_, ns, _) => ns)
    list
  }

  val widened: Parser[Id, List[Any]] = Grammar.grammar[List[Any], Id] { g =>
    import g.*
    val s = nonTerminal[List[Any]]
    val num = terminal("[0-9]+").map(_.toInt)
    skip(" +")
    s ::= all(rep(num).as[List[Any]]).pure(l => l)
    s
  }

  val nested: Parser[Id, Vector[List[Int]]] = Grammar.grammar[Vector[List[Int]], Id] { g =>
    import g.*
    val s = nonTerminal[Vector[List[Int]]]
    val num = terminal("[0-9]+").map(_.toInt)
    skip(" +")
    s ::= all(sepBy1(rep1(num), ";").as[Vector[List[Int]]]).pure(v => v)
    s
  }

  type Result[A] = Either[ParseError, A]

  val nonEmpty: Parser[Result, NonEmptyList[Int]] = Grammar.grammar[NonEmptyList[Int], Result] { g =>
    import g.*
    val s = nonTerminal[NonEmptyList[Int]]
    val num = terminal("[0-9]+").map(_.toInt)
    skip(" +")
    s ::= all(rep(num).as[NonEmptyList[Int]]).pure(l => l)
    s
  }

  val nonEmptyTry: Parser[scala.util.Try, NonEmptyList[Int]] = Grammar.grammar[NonEmptyList[Int], scala.util.Try] { g =>
    import g.*
    val s = nonTerminal[NonEmptyList[Int]]
    val num = terminal("[0-9]+").map(_.toInt)
    s ::= all(rep(num).as[NonEmptyList[Int]]).pure(l => l)
    s
  }

  val nonEmptyThrowing: Parser[Id, NonEmptyList[Int]] = {
    import ErrorChannel.throwing.*
    Grammar.grammar[NonEmptyList[Int], Id] { g =>
      import g.*
      val s = nonTerminal[NonEmptyList[Int]]
      val num = terminal("[0-9]+").map(_.toInt)
      s ::= all(rep(num).as[NonEmptyList[Int]]).pure(l => l)
      s
    }
  }

  val chains: Parser[Id, Chain[Int]] = Grammar.grammar[Chain[Int], Id] { g =>
    import g.*
    val s = nonTerminal[Chain[Int]]
    val num = terminal("[0-9]+").map(_.toInt)
    skip(" +")
    s ::= all(rep(num).as[Chain[Int]]).pure(c => c)
    s
  }

  val optionalVector: Parser[Id, Option[Vector[Int]]] = Grammar.grammar[Option[Vector[Int]], Id] { g =>
    import g.*
    val s = nonTerminal[Option[Vector[Int]]]
    val num = terminal("[0-9]+").map(_.toInt)
    skip(" +")
    s ::= all(opt(rep1(num).as[Vector[Int]])).pure(o => o)
    s
  }
}
