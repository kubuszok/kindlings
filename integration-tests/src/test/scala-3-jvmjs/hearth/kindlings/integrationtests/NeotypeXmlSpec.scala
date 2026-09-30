package hearth.kindlings.integrationtests

import hearth.MacroSuite
import hearth.kindlings.xmlderivation.{KindlingsXmlDecoder, KindlingsXmlEncoder}

final class NeotypeXmlSpec extends MacroSuite {

  private val person = NeotypePerson(NeotypeEmail("alice@example.com"), NeotypeAge(30), NeotypeNickname("ally"))

  group("Neotype + XML") {

    test("case class with neotype fields encodes correctly") {
      val result = KindlingsXmlEncoder.derived[NeotypePerson].encode(person, "person")
      assert((result \ "email").text == "alice@example.com")
      assert((result \ "age").text == "30")
    }

    test("neotype validation rejects invalid value") {
      val decoder = KindlingsXmlDecoder.derived[NeotypePerson]
      val elem = scala.xml.XML.loadString(
        "<person><email>not-an-email</email><age>30</age><nickname>ally</nickname></person>"
      )
      val result = decoder.decode(elem)
      assert(result.isLeft, s"Expected Left but got $result")
    }

    test("encode then decode preserves value") {
      val encoder = KindlingsXmlEncoder.derived[NeotypePerson]
      val decoder = KindlingsXmlDecoder.derived[NeotypePerson]
      decoder.decode(encoder.encode(person, "person")) ==> Right(person)
    }
  }
}
