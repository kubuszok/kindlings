package hearth.kindlings.integrationtests

import eu.timepit.refined.refineV
import eu.timepit.refined.collection.NonEmpty
import eu.timepit.refined.numeric.Positive
import hearth.MacroSuite
import hearth.kindlings.xmlderivation.{KindlingsXmlDecoder, KindlingsXmlEncoder}

final class RefinedXmlSpec extends MacroSuite {

  private val alice = refineV[NonEmpty]("Alice").toOption.get
  private val thirty = refineV[Positive](30).toOption.get

  private def parseXml(xml: String): scala.xml.Elem =
    scala.xml.XML.loadString(xml)

  group("Refined + XML") {

    group("encoding") {

      test("case class with refined fields encodes correctly") {
        val encoder = KindlingsXmlEncoder.derive[RefinedPerson]
        val person = RefinedPerson(alice, thirty)
        val result = encoder.encode(person, "person")
        assert((result \ "name").text == "Alice")
        assert((result \ "age").text == "30")
        assert(result.label == "person")
      }
    }

    group("decoding valid") {

      test("case class with refined fields decodes valid XML") {
        val decoder = KindlingsXmlDecoder.derive[RefinedPerson]
        val elem = parseXml("<person><name>Alice</name><age>30</age></person>")
        val result = decoder.decode(elem)
        assert(result.isRight, s"Expected Right but got $result")
        result.foreach { p =>
          assertEquals(p.name.value, "Alice")
          assertEquals(p.age.value, 30)
        }
      }
    }

    group("decoding invalid") {

      test("refined positive rejects negative") {
        val decoder = KindlingsXmlDecoder.derive[RefinedPerson]
        val elem = parseXml("<person><name>Alice</name><age>-1</age></person>")
        val result = decoder.decode(elem)
        assert(result.isLeft, s"Expected Left but got $result")
      }

      test("refined non-empty rejects empty string") {
        val decoder = KindlingsXmlDecoder.derive[RefinedPerson]
        val elem = parseXml("<person><name></name><age>30</age></person>")
        val result = decoder.decode(elem)
        assert(result.isLeft, s"Expected Left but got $result")
      }
    }

    group("round-trip") {

      test("encode then decode preserves value") {
        val encoder = KindlingsXmlEncoder.derive[RefinedPerson]
        val decoder = KindlingsXmlDecoder.derive[RefinedPerson]
        val person = RefinedPerson(alice, thirty)
        val elem = encoder.encode(person, "person")
        val decoded = decoder.decode(elem)
        assert(decoded.isRight, s"Expected Right but got $decoded")
        decoded.foreach { p =>
          assertEquals(p.name.value, "Alice")
          assertEquals(p.age.value, 30)
        }
      }
    }
  }
}
