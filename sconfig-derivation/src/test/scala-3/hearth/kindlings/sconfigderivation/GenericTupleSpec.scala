package hearth.kindlings.sconfigderivation

import hearth.MacroSuite
import hearth.kindlings.sconfigderivation.ConfigDecodingError.{WrongSizeList, WrongType}
import org.ekrich.config.{ConfigFactory, ConfigObject, ConfigRenderOptions, ConfigValue}

object GenericTupleFixtures {

  /** 23 elements: more than `Tuple22`, a `TupleXXL` at runtime. */
  type T23 = (
      Int,
      String,
      Int,
      String,
      Int,
      String,
      Int,
      String,
      Int,
      String,
      Int,
      String,
      Int,
      String,
      Int,
      String,
      Int,
      String,
      Int,
      String,
      Int,
      String,
      Boolean
  )

  val t23: T23 =
    (
      0,
      "s1",
      2,
      "s3",
      4,
      "s5",
      6,
      "s7",
      8,
      "s9",
      10,
      "s11",
      12,
      "s13",
      14,
      "s15",
      16,
      "s17",
      18,
      "s19",
      20,
      "s21",
      true
    )

  val t23Hocon: String =
    "[0, s1, 2, s3, 4, s5, 6, s7, 8, s9, 10, s11, 12, s13, 14, s15, 16, s17, 18, s19, 20, s21, true]"

  type Cons = Int *: String *: EmptyTuple
}

final case class WithT23(values: GenericTupleFixtures.T23)
final case class WithEmptyTuple(values: EmptyTuple, other: Int)

/** Scala 3 tuples which are not `TupleN` (`EmptyTuple`, `H *: T`, more than 22 elements) are read from / written as
  * lists, like upstream PureConfig's Scala 3 derivation does.
  */
final class GenericTupleSpec extends MacroSuite {
  import GenericTupleFixtures.*

  private def cursor(hocon: String): ConfigValue = ConfigFactory.parseString(hocon).root

  private def value(hocon: String): ConfigValue = cursor(s"v = $hocon").asInstanceOf[ConfigObject].get("v")

  private def failures(result: Either[ConfigDecodingError, Any]): List[ConfigDecodingError] =
    ConfigDecodingError.flatten(result.left.toOption.getOrElse(fail(s"expected a failure, got $result")))

  private def render(v: ConfigValue): String = v.render(ConfigRenderOptions.concise)

  group("tuples with more than 22 elements") {

    test("are read from lists") {
      ConfigReader.derived[T23].from(value(t23Hocon)) ==> Right(t23)
      ConfigReader.derived[WithT23].from(cursor(s"values = $t23Hocon")) ==> Right(WithT23(t23))
    }

    test("are written as lists") {
      render(ConfigWriter.derived[T23].to(t23)) ==>
        """[0,"s1",2,"s3",4,"s5",6,"s7",8,"s9",10,"s11",12,"s13",14,"s15",16,"s17",18,"s19",20,"s21",true]"""
    }

    test("a list of another size is a WrongSizeList") {
      failures(ConfigReader.derived[WithT23].from(cursor("values = [1, a]"))) ==>
        List(WrongSizeList(List("values"), 23, 2))
    }

    test("every failing element is reported, at its index") {
      val input = t23Hocon.replace("[0,", "[x,").replace("true]", "nope]")
      failures(ConfigReader.derived[WithT23].from(cursor(s"values = $input"))) ==> List(
        WrongType(List("values", "0"), "NUMBER", "STRING"),
        WrongType(List("values", "22"), "BOOLEAN", "STRING")
      )
    }

    test("a value that is not a list is a WrongType expecting a list") {
      failures(ConfigReader.derived[T23].from(value("str"))) ==> List(WrongType(Nil, "LIST", "STRING"))
    }

    test("round trip, also through ConfigCodec") {
      ConfigReader.derived[WithT23].from(ConfigWriter.derived[WithT23].to(WithT23(t23))) ==> Right(WithT23(t23))
      val c = ConfigCodec.derived[T23]
      c.from(c.to(t23)) ==> Right(t23)
    }
  }

  group("EmptyTuple") {

    test("is read from an empty list") {
      ConfigReader.derived[EmptyTuple].from(value("[]")) ==> Right(EmptyTuple)
      ConfigReader.derived[WithEmptyTuple].from(cursor("values = [], other = 1")) ==>
        Right(WithEmptyTuple(EmptyTuple, 1))
    }

    test("a non-empty list is a WrongSizeList, a non-list a WrongType") {
      failures(ConfigReader.derived[EmptyTuple].from(value("[1]"))) ==> List(WrongSizeList(Nil, 0, 1))
      failures(ConfigReader.derived[EmptyTuple].from(value("1"))) ==> List(WrongType(Nil, "LIST", "NUMBER"))
    }

    test("is written as an empty list") {
      render(ConfigWriter.derived[EmptyTuple].to(EmptyTuple)) ==> "[]"
      render(ConfigWriter.derived[WithEmptyTuple].to(WithEmptyTuple(EmptyTuple, 1))) ==>
        """{"other":1,"values":[]}"""
    }
  }

  group("tuples written with *:") {

    test("are read from lists and written as lists") {
      val c: Cons = 1 *: "a" *: EmptyTuple
      ConfigReader.derived[Cons].from(value("[1, a]")) ==> Right(c)
      failures(ConfigReader.derived[Cons].from(value("[1]"))) ==> List(WrongSizeList(Nil, 2, 1))
      render(ConfigWriter.derived[Cons].to(c)) ==> """[1,"a"]"""
    }
  }
}
