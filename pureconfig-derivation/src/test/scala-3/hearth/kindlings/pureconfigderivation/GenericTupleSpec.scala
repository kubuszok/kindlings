package hearth.kindlings.pureconfigderivation

import com.typesafe.config.{ConfigFactory, ConfigRenderOptions, ConfigValue, ConfigValueType}
import hearth.MacroSuite
import pureconfig.ConfigCursor
import pureconfig.error.{ConfigReaderFailures, ConvertFailure, WrongSizeList, WrongType}

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
    (0, "s1", 2, "s3", 4, "s5", 6, "s7", 8, "s9", 10, "s11", 12, "s13", 14, "s15", 16, "s17", 18, "s19", 20, "s21", true)

  val t23Hocon: String =
    "[0, s1, 2, s3, 4, s5, 6, s7, 8, s9, 10, s11, 12, s13, 14, s15, 16, s17, 18, s19, 20, s21, true]"

  type Cons = Int *: String *: EmptyTuple
}

final case class WithT23(values: GenericTupleFixtures.T23)
final case class WithEmptyTuple(values: EmptyTuple, other: Int)

/** Scala 3 tuples which are not `TupleN` (`EmptyTuple`, `H *: T`, more than 22 elements) are read from / written as
  * lists, like upstream pureconfig's Scala 3 derivation does.
  */
final class GenericTupleSpec extends MacroSuite {
  import GenericTupleFixtures.*

  private def cursor(hocon: String): ConfigCursor = ConfigCursor(ConfigFactory.parseString(hocon).root(), Nil)

  private def value(hocon: String): ConfigCursor =
    cursor(s"v = $hocon").asObjectCursor.flatMap(_.atKey("v")).toOption.get

  private def reasons(result: Either[ConfigReaderFailures, Any]): List[(String, Any)] =
    result.left.toOption.get.toList.map { case ConvertFailure(reason, _, path) => (path, reason); case f => ("?", f) }

  private def render(v: ConfigValue): String = v.render(ConfigRenderOptions.concise())

  group("tuples with more than 22 elements") {

    test("are read from lists") {
      KindlingsConfigReader.derived[T23].from(value(t23Hocon)) ==> Right(t23)
      KindlingsConfigReader.derived[WithT23].from(cursor(s"values = $t23Hocon")) ==> Right(WithT23(t23))
    }

    test("are written as lists") {
      render(KindlingsConfigWriter.derived[T23].to(t23)) ==>
        """[0,"s1",2,"s3",4,"s5",6,"s7",8,"s9",10,"s11",12,"s13",14,"s15",16,"s17",18,"s19",20,"s21",true]"""
    }

    test("a list of another size is a WrongSizeList") {
      val r = KindlingsConfigReader.derived[WithT23]
      reasons(r.from(cursor("values = [1, a]"))) ==> List("values" -> WrongSizeList(23, 2))
    }

    test("every failing element is reported, at its index") {
      val r = KindlingsConfigReader.derived[WithT23]
      val input = t23Hocon.replace("[0,", "[x,").replace("true]", "nope]")
      reasons(r.from(cursor(s"values = $input"))) ==> List(
        "values.0" -> WrongType(ConfigValueType.STRING, Set(ConfigValueType.NUMBER)),
        "values.22" -> WrongType(ConfigValueType.STRING, Set(ConfigValueType.BOOLEAN))
      )
    }

    test("a value that is not a list is a WrongType expecting a list") {
      reasons(KindlingsConfigReader.derived[T23].from(value("str"))) ==>
        List("v" -> WrongType(ConfigValueType.STRING, Set(ConfigValueType.LIST)))
    }

    test("round trip, also through KindlingsConfigConvert") {
      val r = KindlingsConfigReader.derived[WithT23]
      val w = KindlingsConfigWriter.derived[WithT23]
      r.from(ConfigCursor(w.to(WithT23(t23)), Nil)) ==> Right(WithT23(t23))
      val c = KindlingsConfigConvert.derived[T23]
      c.from(ConfigCursor(c.to(t23), Nil)) ==> Right(t23)
    }
  }

  group("EmptyTuple") {

    test("is read from an empty list") {
      KindlingsConfigReader.derived[EmptyTuple].from(value("[]")) ==> Right(EmptyTuple)
      KindlingsConfigReader.derived[WithEmptyTuple].from(cursor("values = [], other = 1")) ==>
        Right(WithEmptyTuple(EmptyTuple, 1))
    }

    test("a non-empty list is a WrongSizeList, a non-list a WrongType") {
      reasons(KindlingsConfigReader.derived[EmptyTuple].from(value("[1]"))) ==> List("v" -> WrongSizeList(0, 1))
      reasons(KindlingsConfigReader.derived[EmptyTuple].from(value("1"))) ==>
        List("v" -> WrongType(ConfigValueType.NUMBER, Set(ConfigValueType.LIST)))
    }

    test("is written as an empty list") {
      render(KindlingsConfigWriter.derived[EmptyTuple].to(EmptyTuple)) ==> "[]"
      render(KindlingsConfigWriter.derived[WithEmptyTuple].to(WithEmptyTuple(EmptyTuple, 1))) ==>
        """{"other":1,"values":[]}"""
    }
  }

  group("tuples written with *:") {

    test("are read from lists and written as lists") {
      val c: Cons = 1 *: "a" *: EmptyTuple
      KindlingsConfigReader.derived[Cons].from(value("[1, a]")) ==> Right(c)
      reasons(KindlingsConfigReader.derived[Cons].from(value("[1]"))) ==> List("v" -> WrongSizeList(2, 1))
      render(KindlingsConfigWriter.derived[Cons].to(c)) ==> """[1,"a"]"""
    }
  }
}
