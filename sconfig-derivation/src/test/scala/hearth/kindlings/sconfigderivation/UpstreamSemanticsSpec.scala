package hearth.kindlings.sconfigderivation

import hearth.MacroSuite
import hearth.kindlings.sconfigderivation.ConfigDecodingError.{
  CannotConvert,
  KeyNotFound,
  NoValidCoproductOptionFound,
  UnexpectedValueForFieldCoproductHint,
  UnknownKey,
  WrongType
}
import org.ekrich.config.{ConfigFactory, ConfigObject, ConfigRenderOptions, ConfigValue}

sealed trait UsSource
final case class UsUrlSource(url: String) extends UsSource
final case class UsFileSource(path: String) extends UsSource
final case class UsAnySource(url: String, path: Option[String]) extends UsSource
case object UsNoSource extends UsSource

final case class UsStrict(name: String, port: Int)

final case class UsMultiWordFields(firstName: String, lastName: String, postalCode: Int)

sealed trait UsColorChoiceOrTyped
final case class UsTypedChoice(`type`: String) extends UsColorChoiceOrTyped
final case class UsPlainChoice(value: Int) extends UsColorChoiceOrTyped

/** sconfig-derivation follows upstream PureConfig's semantics, as pureconfig-derivation does (checked against upstream
  * by `PureConfigOracleSpec` in upstream-oracle-tests). sconfig has no derivation of its own, so there is no upstream
  * oracle: this spec runs the scenarios of pureconfig-derivation's `UpstreamSemanticsSpec` and asserts the same
  * outcomes (values read, failure kinds and paths, written output), with sconfig's own failure types.
  *
  * Paths: sconfig readers read a `ConfigValue` (not a cursor), so failure paths are relative to the value the reader is
  * called with. Where the pureconfig spec reads a value at key `v` of a parsed document (path `v`), the path here is
  * empty.
  */
final class UpstreamSemanticsSpec extends MacroSuite {

  private def cursor(hocon: String): ConfigValue = ConfigFactory.parseString(hocon).root

  private def value(hocon: String): ConfigValue = cursor(s"v = $hocon").asInstanceOf[ConfigObject].get("v")

  private def failures(result: Either[ConfigDecodingError, Any]): List[ConfigDecodingError] =
    ConfigDecodingError.flatten(result.left.toOption.getOrElse(fail(s"expected a failure, got $result")))

  private def render(v: ConfigValue): String = v.render(ConfigRenderOptions.concise)

  group("errors are accumulated") {

    test("every failing field and every unknown key is reported") {
      implicit val hint: ProductHint[UsStrict] = ProductHint[UsStrict](allowUnknownKeys = false)
      val r = ConfigReader.derived[UsStrict]
      failures(r.from(cursor("port = x, extra = 1"))) ==> List(
        KeyNotFound(Nil, "name"),
        WrongType(List("port"), "NUMBER", "STRING"),
        UnknownKey(List("extra"), "extra")
      )
    }

    test("every failing item of a collection is reported") {
      val r = ConfigReader.derived[TeamWithMembers]
      failures(r.from(cursor("name = t, members = [{ name = a }, { age = 1 }]"))) ==> List(
        KeyNotFound(List("members", "0"), "age"),
        KeyNotFound(List("members", "1"), "name", Set.empty)
      )
    }

    test("every failing value of a map is reported") {
      val r = ConfigReader.derived[WithMap]
      failures(r.from(cursor("scores { a = x, b = 1, c = y }"))).toSet ==> Set[ConfigDecodingError](
        WrongType(List("scores", "a"), "NUMBER", "STRING"),
        WrongType(List("scores", "c"), "NUMBER", "STRING")
      )
    }

    test("a value that is not an object fails once") {
      val r = ConfigReader.derived[SimplePerson]
      failures(r.from(value("[1]"))) ==> List(WrongType(Nil, "OBJECT", "LIST"))
    }

    test("missing keys suggest candidates the way upstream does (KeyNotFound.forKeys)") {
      val r = ConfigReader.derived[UsMultiWordFields]
      failures(r.from(cursor("firstName = a, last-name = b, postal-code = 1"))) ==> List(
        KeyNotFound(Nil, "first-name", Set("firstName"))
      )
    }

    test("a missing key is passed to readers of missing keys (Option fields)") {
      val r = ConfigReader.derived[WithOption]
      r.from(cursor("name = a")) ==> Right(WithOption("a", None))
    }

    test("a missing key is passed to custom ReadsMissingKeys readers") {
      implicit val defaulting: ConfigReader[Int] = new ConfigReader[Int] with ReadsMissingKeys {
        def from(value: ConfigValue): Either[ConfigDecodingError, Int] =
          if (value == null) Right(-1) else ConfigReader.intReader.from(value)
      }
      val r = ConfigReader.derived[SingleField]
      r.from(cursor("{}")) ==> Right(SingleField(-1))
      r.from(cursor("value = 3")) ==> Right(SingleField(3))
    }
  }

  group("FieldCoproductHint semantics") {

    test("the discriminator key is not passed to the subtype (strict subtypes do not see it as unknown)") {
      implicit val hint: ProductHint[Circle] = ProductHint[Circle](allowUnknownKeys = false)
      val r = ConfigReader.derived[Shape]
      r.from(cursor("type = circle, radius = 1.5")) ==> Right(Circle(1.5))
    }

    test("the discriminator key is an unknown key of a strict case class read on its own") {
      implicit val hint: ProductHint[UsStrict] = ProductHint[UsStrict](allowUnknownKeys = false)
      val r = ConfigReader.derived[UsStrict]
      failures(r.from(cursor("type = x, name = a, port = 1"))) ==> List(UnknownKey(List("type"), "type"))
    }

    test("an unknown discriminator value fails at the discriminator") {
      val r = ConfigReader.derived[Shape]
      failures(r.from(cursor("type = square, side = 1"))) match {
        case List(UnexpectedValueForFieldCoproductHint(path, v)) =>
          path ==> List("type")
          render(v) ==> "\"square\""
        case other => fail(s"unexpected $other")
      }
    }

    test("a missing discriminator is a missing key") {
      val r = ConfigReader.derived[Shape]
      failures(r.from(cursor("radius = 1"))) ==> List(KeyNotFound(Nil, "type"))
    }

    test("the subtype's failures are reported at the subtype's paths") {
      val r = ConfigReader.derived[Shape]
      failures(r.from(cursor("type = rectangle, width = x"))) ==> List(
        WrongType(List("width"), "NUMBER", "STRING"),
        KeyNotFound(Nil, "height")
      )
    }

    test("case objects are objects with a discriminator, not strings") {
      val r = ConfigReader.derived[CardinalDirection]
      r.from(cursor("type = north")) ==> Right(North)
      failures(r.from(value("north"))) ==> List(WrongType(Nil, "OBJECT", "STRING"))
    }

    test("writing a subtype with a field named like the discriminator throws, as upstream") {
      val w = ConfigWriter.derived[UsColorChoiceOrTyped]
      val e = intercept[CoproductHintException](w.to(UsTypedChoice("x")))
      assert(e.failure.isInstanceOf[ConfigDecodingError.CollidingKeys], e.failure)
      render(w.to(UsPlainChoice(1))) ==> """{"type":"us-plain-choice","value":1}"""
    }
  }

  group("enumerations (no discriminator)") {

    test("case objects are read only from strings") {
      implicit val config: SConfig = SConfig.default.withWrappedSubtypes
      val r = ConfigReader.derived[CardinalDirection]
      r.from(value("north")) ==> Right(North)
      assert(r.from(cursor("north {}")).isLeft)
      failures(r.from(value("northwest"))) match {
        case List(CannotConvert(Nil, message, None)) =>
          message ==> "Cannot convert 'northwest' to CardinalDirection: The value is not a valid enum option."
        case other => fail(s"unexpected $other")
      }
    }
  }

  group("FirstSuccessCoproductHint") {

    @scala.annotation.nowarn("msg=never used|unused local definition")
    implicit val hint: CoproductHint.FirstSuccess[UsSource] = CoproductHint.FirstSuccess()

    test("subtypes are attempted in the alphabetical order of their names; the first success wins") {
      val r = ConfigReader.derived[UsSource]
      r.from(cursor("""url = "http://a"""")) ==> Right(UsAnySource("http://a", None))
      r.from(cursor("path = p")) ==> Right(UsFileSource("p"))
      // the case object reads any object, but `UsAnySource` and `UsFileSource` come first
      r.from(cursor("other = 1")) ==> Right(UsNoSource)
    }

    test("when no subtype can be read, the failures of every attempt are reported") {
      val r = ConfigReader.derived[UsSource]
      failures(r.from(value("text"))) match {
        case List(NoValidCoproductOptionFound(path, v, attempts)) =>
          path ==> Nil
          render(v) ==> "\"text\""
          attempts.map(_._1) ==> List("UsAnySource", "UsFileSource", "UsNoSource", "UsUrlSource")
          attempts.map(a => ConfigDecodingError.flatten(a._2)).foreach { attemptFailures =>
            attemptFailures ==> List(WrongType(Nil, "OBJECT", "STRING"))
          }
        case other => fail(s"unexpected $other")
      }
    }

    test("written without disambiguation information") {
      val w = ConfigWriter.derived[UsSource]
      render(w.to(UsFileSource("p"))) ==> """{"path":"p"}"""
      render(w.to(UsAnySource("u", None))) ==> """{"url":"u"}"""
    }
  }

  group("writing") {

    test("an empty Option field is omitted (as upstream's WritesMissingKeys)") {
      val w = ConfigWriter.derived[WithOption]
      render(w.to(WithOption("a", None))) ==> """{"name":"a"}"""
      render(w.to(WithOption("a", Some("b")))) ==> """{"name":"a","nickname":"b"}"""
    }
  }

  group("failure messages mirror upstream's descriptions") {

    test("KeyNotFound") {
      KeyNotFound(Nil, "a").message ==> "Key not found: 'a'."
      KeyNotFound(Nil, "first-name", Set("firstName")).message ==>
        "Key not found: 'first-name'. You might have a misconfigured ProductHint, since the following similar keys were found:\n  - 'firstName'"
    }

    test("WrongType") {
      WrongType(List("port"), "NUMBER", "STRING").getMessage ==> "Expected type NUMBER. Found STRING instead. (at port)"
    }
  }
}
