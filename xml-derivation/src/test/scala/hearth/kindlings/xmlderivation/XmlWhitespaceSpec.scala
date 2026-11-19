package hearth.kindlings.xmlderivation

import hearth.MacroSuite
import hearth.kindlings.xmlderivation.annotations.{xmlAttribute, xmlContent}
import hearth.kindlings.xmlderivation.internal.runtime.XmlDerivationUtils

final case class WrappedStr(value: String) extends AnyVal
case class XmlWsStrings(
    name: String,
    @xmlAttribute label: String,
    opt: Option[String],
    items: List[String],
    wrapped: WrappedStr,
    c: Char
)
case class XmlWsContent(@xmlAttribute lang: String, @xmlContent text: String)
case class XmlWsNumbers(
    i: Int,
    l: Long,
    d: Double,
    b: Boolean,
    bd: BigDecimal,
    w: WrappedInt,
    c: Char,
    @xmlAttribute a: Int,
    shape: Shape,
    nested: List[Address]
)

/** String values keep their whitespace exactly; values parsed from text (numbers, booleans...) are trimmed. */
final class XmlWhitespaceSpec extends MacroSuite {

  private val spaced = XmlWsStrings(" a ", " b  c ", Some("  "), List(" ", " d "), WrappedStr(" w "), ' ')

  group("String values keep their whitespace") {

    test("in memory and through elem.toString") {
      val codec = KindlingsXmlCodec.derived[XmlWsStrings]
      val elem = codec.encode(spaced, "x")
      assertEquals(codec.decode(elem), Right(spaced), elem.toString)
      assertEquals(XmlDerivationUtils.parseAndDecode(elem.toString, codec.decode), Right(spaced), elem.toString)
      assertEquals(
        codec.decode(<x label=" l "><name> n </name><opt> o </opt><items/><wrapped> </wrapped><c> </c></x>),
        Right(XmlWsStrings(" n ", " l ", Some(" o "), Nil, WrappedStr(" "), ' '))
      )
    }

    test("through toXmlString / fromXmlString, also for long and multi-line strings") {
      val long = "word " * 40
      List(spaced, spaced.copy(name = long, opt = Some("x  y\n  z"), items = List(long, "\n"))).foreach { value =>
        val xml = KindlingsXmlEncoder.toXmlString(value, "x")
        assertEquals(KindlingsXmlDecoder.fromXmlString[XmlWsStrings](xml), Right(value), xml)
      }
    }

    test("toXmlString still indents nested elements") {
      val xml = KindlingsXmlEncoder.toXmlString(PersonWithAddress("A", 1, Address("s", "c")), "p")
      assertEquals(
        xml,
        """<p>
          |  <name>A</name>
          |  <age>1</age>
          |  <address>
          |    <street>s</street>
          |    <city>c</city>
          |  </address>
          |</p>""".stripMargin
      )
    }

    test("@xmlContent text") {
      val codec = KindlingsXmlCodec.derived[XmlWsContent]
      val value = XmlWsContent("en", "  Hello,\n  World ")
      val elem = codec.encode(value, "x")
      assertEquals(codec.decode(elem), Right(value))
      assertEquals(XmlDerivationUtils.parseAndDecode(elem.toString, codec.decode), Right(value))
      val xml = KindlingsXmlEncoder.toXmlString(value, "x")
      assertEquals(KindlingsXmlDecoder.fromXmlString[XmlWsContent](xml), Right(value), xml)
    }

    test("an Option[String] of blanks is Some (only an empty element is None)") {
      val codec = KindlingsXmlCodec.derived[Box[Option[String]]]
      assertEquals(codec.decode(<box><value> </value></box>), Right(Box(Option(" "))))
      assertEquals(codec.decode(<box><value/></box>), Right(Box(Option.empty[String])))
    }
  }

  group("values parsed from text are trimmed") {

    test("surrounding whitespace in elements and attributes") {
      val codec = KindlingsXmlCodec.derived[XmlWsNumbers]
      val decoded = codec.decode(
        <x a=" 3 ">
          <i> 1 </i>
          <l>
            2
          </l>
          <d> 1.5 </d>
          <b> true </b>
          <bd> 2.25 </bd>
          <w> 4 </w>
          <c> z </c>
          <shape type="Circle"><radius> 1.0 </radius></shape>
          <nested><item><street>s</street><city>c</city></item></nested>
        </x>
      )
      assertEquals(
        decoded,
        Right(
          XmlWsNumbers(
            1,
            2L,
            1.5,
            b = true,
            BigDecimal("2.25"),
            WrappedInt(4),
            'z',
            3,
            Circle(1.0),
            List(Address("s", "c"))
          )
        )
      )
    }

    test("the output of scala.xml.PrettyPrinter decodes") {
      val codec = KindlingsXmlCodec.derived[XmlWsNumbers]
      val value = XmlWsNumbers(
        1,
        2L,
        1.5,
        b = false,
        BigDecimal("2.25"),
        WrappedInt(4),
        'z',
        3,
        Rectangle(1.0, 2.0),
        List(Address("s", "c"), Address("t", "d"))
      )
      val pretty = new scala.xml.PrettyPrinter(20, 4).format(codec.encode(value, "x"))
      assertEquals(XmlDerivationUtils.parseAndDecode(pretty, codec.decode), Right(value), pretty)
    }
  }
}
