package hearth.kindlings.xmlderivation

import hearth.MacroSuite
import hearth.kindlings.xmlderivation.annotations.xmlAttribute

case class XmlAllBuiltIns(
    s: String,
    i: Int,
    l: Long,
    d: Double,
    f: Float,
    bool: Boolean,
    b: Byte,
    sh: Short,
    c: Char,
    bd: BigDecimal,
    bi: BigInt
)
case class XmlAllBuiltInAttributes(
    @xmlAttribute s: String,
    @xmlAttribute i: Int,
    @xmlAttribute l: Long,
    @xmlAttribute d: Double,
    @xmlAttribute f: Float,
    @xmlAttribute bool: Boolean,
    @xmlAttribute b: Byte,
    @xmlAttribute sh: Short,
    @xmlAttribute c: Char,
    @xmlAttribute bd: BigDecimal,
    @xmlAttribute bi: BigInt
)
case class XmlMaps(byName: Map[String, Int], nested: Map[String, Address])
case class XmlWithUserInstance(version: XmlVersion, versions: List[XmlVersion], maybe: Option[XmlVersion])

/** Documented features of the user guide that the other specs do not exercise. */
final class XmlDocumentedFeaturesSpec extends MacroSuite {

  private def roundTripped[A](codec: KindlingsXmlCodec[A], value: A): scala.xml.Elem = {
    val elem = codec.encode(value, "x")
    assertEquals(codec.decode(elem), Right(value))
    assertEquals(codec.decode(scala.xml.XML.loadString(elem.toString)), Right(value))
    elem
  }

  group("syntax extensions") {

    test("toXmlString / fromXmlString") {
      import hearth.kindlings.xmlderivation.syntax.*
      val xml = SimplePerson("Alice", 30).toXmlString("person")
      assertEquals(xml.replaceAll("\\s+", ""), "<person><name>Alice</name><age>30</age></person>")
      assertEquals(xml.fromXmlString[SimplePerson], Right(SimplePerson("Alice", 30)))
    }

    test("with a configuration in scope") {
      import hearth.kindlings.xmlderivation.syntax.*
      implicit val config: XmlConfig = XmlConfig().withAttributesByDefault.withSnakeCaseFieldNames
      val xml = CamelCasePerson("A", "B").toXmlString("p")
      assertEquals(xml, """<p first_name="A" last_name="B"/>""")
      assertEquals(xml.fromXmlString[CamelCasePerson], Right(CamelCasePerson("A", "B")))
    }

    test("text that is not XML is an error") {
      import hearth.kindlings.xmlderivation.syntax.*
      "<person><name>".fromXmlString[SimplePerson] match {
        case Left(XmlDecodingError.General(message)) => assert(message.startsWith("Failed to parse XML"), message)
        case other                                   => fail(s"unexpected: $other")
      }
      assert(KindlingsXmlDecoder.fromXmlString[SimplePerson]("not xml").isLeft)
    }
  }

  group("supported types") {

    val builtIns =
      XmlAllBuiltIns("s", 1, 2L, 3.5, 4.5f, true, 5.toByte, 6.toShort, 'c', BigDecimal("7.25"), BigInt("8"))

    test("every built-in type, as elements") {
      val codec = KindlingsXmlCodec.derived[XmlAllBuiltIns]
      assertEquals(
        roundTripped(codec, builtIns).toString,
        "<x><s>s</s><i>1</i><l>2</l><d>3.5</d><f>4.5</f><bool>true</bool><b>5</b><sh>6</sh><c>c</c>" +
          "<bd>7.25</bd><bi>8</bi></x>"
      )
    }

    test("every built-in type, as attributes") {
      val codec = KindlingsXmlCodec.derived[XmlAllBuiltInAttributes]
      val value =
        XmlAllBuiltInAttributes("s", 1, 2L, 3.5, 4.5f, true, 5.toByte, 6.toShort, 'c', BigDecimal("7.25"), BigInt("8"))
      assertEquals(
        roundTripped(codec, value).toString,
        """<x s="s" i="1" l="2" d="3.5" f="4.5" bool="true" b="5" sh="6" c="c" bd="7.25" bi="8"/>"""
      )
    }

    test("invalid values are InvalidValue errors naming the type") {
      val codec = KindlingsXmlCodec.derived[XmlAllBuiltIns]
      def withField(name: String, text: String): scala.xml.Elem = {
        val elem = codec.encode(builtIns, "x")
        elem.copy(child = elem.child.map {
          case e: scala.xml.Elem if e.label == name => e.copy(child = Seq(scala.xml.Text(text)))
          case other                                => other
        })
      }
      List(
        "i" -> "Int",
        "l" -> "Long",
        "d" -> "Double",
        "f" -> "Float",
        "bool" -> "Boolean",
        "b" -> "Byte",
        "sh" -> "Short",
        "c" -> "Char",
        "bd" -> "BigDecimal",
        "bi" -> "BigInt"
      ).foreach { case (field, tpe) =>
        assertEquals(codec.decode(withField(field, "zz")), Left(XmlDecodingError.InvalidValue("zz", tpe, field)))
      }
    }

    test("maps: entries with a key attribute and a value element") {
      val codec = KindlingsXmlCodec.derived[XmlMaps]
      assertEquals(
        roundTripped(codec, XmlMaps(Map("a" -> 1), Map("home" -> Address("s", "c")))).toString,
        """<x><byName><entry key="a"><value>1</value></entry></byName>""" +
          """<nested><entry key="home"><value><street>s</street><city>c</city></value></entry></nested></x>"""
      )
      assertEquals(roundTripped(codec, XmlMaps(Map.empty, Map.empty)).toString, "<x><byName/><nested/></x>")
    }

    test("maps: withTransientEmpty skips empty maps, an entry without a value is an error") {
      implicit val config: XmlConfig = XmlConfig().withTransientEmpty
      val codec = KindlingsXmlCodec.derived[XmlMaps]
      assertEquals(roundTripped(codec, XmlMaps(Map.empty, Map.empty)).toString, "<x/>")
      assertEquals(
        codec.decode(<x><byName><entry key="a"/></byName></x>),
        Left(XmlDecodingError.Multiple(List(XmlDecodingError.MissingElement("value", "entry"))))
      )
    }

    test("user-provided instances are used for fields, items and Options") {
      val codec = KindlingsXmlCodec.derived[XmlWithUserInstance]
      assertEquals(
        roundTripped(
          codec,
          XmlWithUserInstance(XmlVersion(1, 2), List(XmlVersion(3, 4)), Some(XmlVersion(5, 6)))
        ).toString,
        "<x><version>1.2</version><versions><item>3.4</item></versions><maybe>5.6</maybe></x>"
      )
    }

    test("a top-level collection and Option") {
      assertEquals(KindlingsXmlEncoder.encode(List(1, 2), "xs").toString, "<xs><item>1</item><item>2</item></xs>")
      assertEquals(KindlingsXmlDecoder.decode[List[Int]](<xs><item>1</item><item>2</item></xs>), Right(List(1, 2)))
      assertEquals(KindlingsXmlDecoder.decode[Option[Int]](<x>1</x>), Right(Some(1)))
      assertEquals(KindlingsXmlDecoder.decode[Option[Int]](<x/>), Right(None))
    }

    test("unknown child elements and attributes are ignored") {
      assertEquals(
        KindlingsXmlDecoder.decode[SimplePerson](<p extra="1"><name>A</name><other>x</other><age>3</age></p>),
        Right(SimplePerson("A", 3))
      )
    }
  }
}
