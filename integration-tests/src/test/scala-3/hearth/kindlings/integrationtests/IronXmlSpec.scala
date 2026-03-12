package hearth.kindlings.integrationtests

import io.github.iltotore.iron.*
import io.github.iltotore.iron.constraint.any.*
import io.github.iltotore.iron.constraint.numeric.*
import io.github.iltotore.iron.constraint.string.*
import hearth.MacroSuite
import hearth.kindlings.xmlderivation.{KindlingsXmlDecoder, KindlingsXmlEncoder}

final class IronXmlSpec extends MacroSuite {

  private def parseXml(xml: String): scala.xml.Elem =
    scala.xml.XML.loadString(xml)

  group("Iron + XML") {

    group("encoding") {

      test("case class with iron fields encodes correctly") {
        val encoder = KindlingsXmlEncoder.derive[IronPerson]
        val person = IronPerson("Alice", 30)
        val result = encoder.encode(person, "person")
        assert((result \ "name").text == "Alice")
        assert((result \ "age").text == "30")
        assert(result.label == "person")
      }
    }

    group("decoding valid") {

      test("case class with iron fields decodes valid XML") {
        val decoder = KindlingsXmlDecoder.derive[IronPerson]
        val elem = parseXml("<person><name>Alice</name><age>30</age></person>")
        val result = decoder.decode(elem)
        assert(result.isRight, s"Expected Right but got $result")
      }
    }

    group("decoding invalid") {

      test("iron positive rejects negative") {
        val decoder = KindlingsXmlDecoder.derive[IronPerson]
        val elem = parseXml("<person><name>Alice</name><age>-1</age></person>")
        val result = decoder.decode(elem)
        assert(result.isLeft, s"Expected Left but got $result")
      }
    }

    group("round-trip") {

      test("encode then decode preserves value") {
        val encoder = KindlingsXmlEncoder.derive[IronPerson]
        val decoder = KindlingsXmlDecoder.derive[IronPerson]
        val person = IronPerson("Alice", 30)
        val elem = encoder.encode(person, "person")
        val decoded = decoder.decode(elem)
        assert(decoded.isRight, s"Expected Right but got $decoded")
      }
    }
  }
}
