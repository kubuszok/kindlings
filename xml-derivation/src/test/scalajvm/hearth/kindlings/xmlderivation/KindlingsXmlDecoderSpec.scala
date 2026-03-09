package hearth.kindlings.xmlderivation

import hearth.MacroSuite

final class KindlingsXmlDecoderSpec extends MacroSuite {

  private def parseXml(xml: String): scala.xml.Elem =
    scala.xml.XML.loadString(xml)

  group("KindlingsXmlDecoder") {

    group("case classes") {

      test("simple case class") {
        val decoder = KindlingsXmlDecoder.derive[SimplePerson]
        val elem = parseXml("<person><name>Alice</name><age>30</age></person>")
        val result = decoder.decode(elem)
        assert(result == Right(SimplePerson("Alice", 30)))
      }

      test("empty case class") {
        val decoder = KindlingsXmlDecoder.derive[EmptyClass]
        val elem = parseXml("<empty/>")
        val result = decoder.decode(elem)
        assert(result == Right(EmptyClass()))
      }

      test("single field case class") {
        val decoder = KindlingsXmlDecoder.derive[SingleField]
        val elem = parseXml("<field><value>42</value></field>")
        val result = decoder.decode(elem)
        assert(result == Right(SingleField(42)))
      }

      test("nested case class") {
        val decoder = KindlingsXmlDecoder.derive[PersonWithAddress]
        val elem = parseXml(
          "<person><name>Bob</name><age>25</age><address><street>123 Main St</street><city>Springfield</city></address></person>"
        )
        val result = decoder.decode(elem)
        assert(result == Right(PersonWithAddress("Bob", 25, Address("123 Main St", "Springfield"))))
      }
    }

    group("annotations") {

      test("@xmlName renames field") {
        val decoder = KindlingsXmlDecoder.derive[XmlWithFieldName]
        val elem = parseXml("<user><user_name>John</user_name><age>30</age></user>")
        val result = decoder.decode(elem)
        assert(result == Right(XmlWithFieldName("John", 30)))
      }

      test("@transientField uses default") {
        val decoder = KindlingsXmlDecoder.derive[XmlWithTransient]
        val elem = parseXml("<item><name>Alice</name></item>")
        val result = decoder.decode(elem)
        assert(result == Right(XmlWithTransient("Alice", None)))
      }
    }

    group("sealed traits") {

      test("decode with discriminator attribute") {
        val decoder = KindlingsXmlDecoder.derive[Shape]
        val elem = parseXml("""<shape type="Circle"><radius>5.0</radius></shape>""")
        val result = decoder.decode(elem)
        assert(result == Right(Circle(5.0)))
      }

      test("unknown discriminator returns error") {
        val decoder = KindlingsXmlDecoder.derive[Shape]
        val elem = parseXml("""<shape type="Triangle"><sides>3</sides></shape>""")
        val result = decoder.decode(elem)
        assert(result.isLeft)
      }
    }

    group("error handling") {

      test("missing required field") {
        val decoder = KindlingsXmlDecoder.derive[SimplePerson]
        val elem = parseXml("<person><name>Alice</name></person>")
        val result = decoder.decode(elem)
        assert(result.isLeft)
      }

      test("invalid value for Int field") {
        val decoder = KindlingsXmlDecoder.derive[SingleField]
        val elem = parseXml("<field><value>not-a-number</value></field>")
        val result = decoder.decode(elem)
        assert(result.isLeft)
      }
    }

    group("generic types") {

      test("Pair[String, Int]") {
        val decoder = KindlingsXmlDecoder.derive[Pair[String, Int]]
        val elem = parseXml("<pair><first>hello</first><second>42</second></pair>")
        val result = decoder.decode(elem)
        assert(result == Right(Pair("hello", 42)))
      }
    }

    group("deeply nested") {

      test("3 levels deep") {
        val decoder = KindlingsXmlDecoder.derive[PersonFull]
        val elem = parseXml(
          "<person><name>Alice</name><address><street>123 Main</street><city>NY</city><geo><lat>40.7</lat><lon>-74.0</lon></geo></address></person>"
        )
        val result = decoder.decode(elem)
        assert(
          result == Right(PersonFull("Alice", FullAddress("123 Main", "NY", GeoCoordinates(40.7, -74.0))))
        )
      }
    }
  }
}
