package hearth.kindlings.pureconfigderivation

import com.typesafe.config.{ConfigFactory, ConfigRenderOptions}
import hearth.MacroSuite
import pureconfig.ConfigCursor
import pureconfig.error.{ConfigReaderFailures, ConvertFailure, KeyNotFound, UnknownKey, WrongType}
import pureconfig.generic.error.{CoproductHintException, NoValidCoproductOptionFound, UnexpectedValueForFieldCoproductHint}

sealed trait UsSource
final case class UsUrlSource(url: String) extends UsSource
final case class UsFileSource(path: String) extends UsSource
final case class UsAnySource(url: String, path: Option[String]) extends UsSource
case object UsNoSource extends UsSource

final case class UsStrict(name: String, port: Int)

/** Behaviours checked against upstream pureconfig by `PureConfigOracleSpec` (upstream-oracle-tests), tested here
  * without upstream's derivation.
  */
final class UpstreamSemanticsSpec extends MacroSuite {

  private def cursor(hocon: String): ConfigCursor = ConfigCursor(ConfigFactory.parseString(hocon).root(), Nil)

  private def value(hocon: String): ConfigCursor =
    cursor(s"v = $hocon").asObjectCursor.flatMap(_.atKey("v")).toOption.get

  private def reasons(result: Either[ConfigReaderFailures, Any]): List[(String, Any)] =
    result.left.toOption.get.toList.map { case ConvertFailure(reason, _, path) => (path, reason); case f => ("?", f) }

  group("errors are accumulated") {

    test("every failing field and every unknown key is reported") {
      implicit val hint: KindlingsProductHint[UsStrict] = KindlingsProductHint[UsStrict](allowUnknownKeys = false)
      val r = KindlingsConfigReader.derived[UsStrict]
      reasons(r.from(cursor("port = x, extra = 1"))) ==> List(
        "" -> KeyNotFound("name"),
        "port" -> WrongType(com.typesafe.config.ConfigValueType.STRING, Set(com.typesafe.config.ConfigValueType.NUMBER)),
        "extra" -> UnknownKey("extra")
      )
    }

    test("every failing item of a collection is reported") {
      val r = KindlingsConfigReader.derived[TeamWithMembers]
      reasons(r.from(cursor("name = t, members = [{ name = a }, { age = 1 }]"))) ==> List(
        "members.0" -> KeyNotFound("age"),
        "members.1" -> KeyNotFound("name", Set.empty)
      )
    }

    test("a value that is not an object fails once") {
      val r = KindlingsConfigReader.derived[SimplePerson]
      reasons(r.from(value("[1]"))).size ==> 1
    }

    test("missing keys suggest candidates the way upstream does (KeyNotFound.forKeys)") {
      val r = KindlingsConfigReader.derived[MultiWordFields]
      reasons(r.from(cursor("firstName = a, last-name = b, postal-code = 1"))) ==> List(
        "" -> KeyNotFound("first-name", Set("firstName"))
      )
    }
  }

  group("FieldCoproductHint semantics") {

    test("the discriminator key is not passed to the subtype (strict subtypes do not see it as unknown)") {
      implicit val hint: KindlingsProductHint[Circle] = KindlingsProductHint[Circle](allowUnknownKeys = false)
      val r = KindlingsConfigReader.derived[Shape]
      r.from(cursor("type = circle, radius = 1.5")) ==> Right(Circle(1.5))
    }

    test("the discriminator key is an unknown key of a strict case class read on its own") {
      implicit val hint: KindlingsProductHint[UsStrict] = KindlingsProductHint[UsStrict](allowUnknownKeys = false)
      val r = KindlingsConfigReader.derived[UsStrict]
      reasons(r.from(cursor("type = x, name = a, port = 1"))) ==> List("type" -> UnknownKey("type"))
    }

    test("an unknown discriminator value fails at the discriminator") {
      val r = KindlingsConfigReader.derived[Shape]
      val List((path, reason)) = reasons(r.from(cursor("type = square, side = 1"))): @unchecked
      path ==> "type"
      assert(reason.isInstanceOf[UnexpectedValueForFieldCoproductHint], reason)
    }

    test("case objects are objects with a discriminator, not strings") {
      val r = KindlingsConfigReader.derived[CardinalDirection]
      r.from(cursor("type = north")) ==> Right(North)
      reasons(r.from(value("north"))) ==> List(
        "v" -> WrongType(com.typesafe.config.ConfigValueType.STRING, Set(com.typesafe.config.ConfigValueType.OBJECT))
      )
    }

    test("writing a subtype with a field named like the discriminator throws, as upstream") {
      val w = KindlingsConfigWriter.derived[ColorChoiceOrTyped]
      intercept[CoproductHintException](w.to(TypedChoice("x")))
    }
  }

  group("enumerations (no discriminator)") {

    test("case objects are read only from strings") {
      implicit val config: PureConfig = PureConfig.default.withWrappedSubtypes
      val r = KindlingsConfigReader.derived[CardinalDirection]
      r.from(value("north")) ==> Right(North)
      assert(r.from(cursor("north {}")).isLeft)
      assert(r.from(value("northwest")).isLeft)
    }
  }

  group("FirstSuccessCoproductHint") {

    @scala.annotation.nowarn("msg=never used|unused local definition")
    implicit val hint: KindlingsCoproductHint.FirstSuccess[UsSource] = KindlingsCoproductHint.FirstSuccess()

    test("subtypes are attempted in the alphabetical order of their names; the first success wins") {
      val r = KindlingsConfigReader.derived[UsSource]
      r.from(cursor("""url = "http://a"""")) ==> Right(UsAnySource("http://a", None))
      r.from(cursor("path = p")) ==> Right(UsFileSource("p"))
      // the case object reads any object, but `UsAnySource` and `UsFileSource` come first
      r.from(cursor("other = 1")) ==> Right(UsNoSource)
    }

    test("when no subtype can be read, the failures of every attempt are reported") {
      val r = KindlingsConfigReader.derived[UsSource]
      val List((path, reason)) = reasons(r.from(value("text"))): @unchecked
      path ==> "v"
      reason match {
        case NoValidCoproductOptionFound(_, attempts) =>
          attempts.map(_._1) ==> List("UsAnySource", "UsFileSource", "UsNoSource", "UsUrlSource")
        case other => fail(s"unexpected $other")
      }
    }

    test("written without disambiguation information") {
      val w = KindlingsConfigWriter.derived[UsSource]
      w.to(UsFileSource("p")).render(ConfigRenderOptions.concise()) ==> """{"path":"p"}"""
    }
  }
}

sealed trait ColorChoiceOrTyped
final case class TypedChoice(`type`: String) extends ColorChoiceOrTyped
final case class PlainChoice(value: Int) extends ColorChoiceOrTyped
