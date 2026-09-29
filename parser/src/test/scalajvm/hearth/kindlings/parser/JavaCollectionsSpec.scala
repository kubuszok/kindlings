package hearth.kindlings.parser

import hearth.MacroSuite

/** Hearth registers its Java collection providers (`IsCollection` for `java.util.*`) on the JVM only. */
final class JavaCollectionsSpec extends MacroSuite {

  import JavaCollectionsSpec.*

  group("repetitions") {

    test("are collected into Java collections") {
      javaLists.parse("1 2 3") ==> java.util.Arrays.asList(1, 2, 3)
    }
  }
}
object JavaCollectionsSpec {

  val javaLists: Parser[Id, java.util.List[Int]] = Grammar.grammar[java.util.List[Int], Id] { g =>
    import g.*
    val s = nonTerminal[java.util.List[Int]]
    val num = terminal("[0-9]+").map(_.toInt)
    skip(" +")
    s ::= all(rep(num).as[java.util.List[Int]]).pure(l => l)
    s
  }
}
