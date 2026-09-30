package hearth.kindlings.integrationtests

import hearth.MacroSuite
import hearth.kindlings.xmlderivation.{KindlingsXmlDecoder, KindlingsXmlEncoder}

final class NewtypeXmlSpec extends MacroSuite {

  group("scala-newtype + XML") {

    test("case class with newtype fields encodes correctly") {
      val result = KindlingsXmlEncoder.derived[NewtypeUser].encode(NewtypeUser.example, "user")
      assert((result \ "id").text == "1")
      assert((result \ "name").text == "alice")
    }

    test("encode then decode preserves value") {
      val encoder = KindlingsXmlEncoder.derived[NewtypeUser]
      val decoder = KindlingsXmlDecoder.derived[NewtypeUser]
      decoder.decode(encoder.encode(NewtypeUser.example, "user")) ==> Right(NewtypeUser.example)
    }
  }
}
