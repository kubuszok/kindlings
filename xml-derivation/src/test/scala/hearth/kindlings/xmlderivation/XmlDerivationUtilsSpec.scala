package hearth.kindlings.xmlderivation

import hearth.MacroSuite
import hearth.kindlings.xmlderivation.internal.runtime.XmlDerivationUtils

/** The runtime helpers that read text, check emptiness and print elements. */
final class XmlDerivationUtilsSpec extends MacroSuite {

  private def text(t: String): scala.xml.Elem = XmlDerivationUtils.makeTextElem("e", t)

  group("getStringContent") {

    test("keeps whitespace") {
      assertEquals(XmlDerivationUtils.getStringContent(text(" a  b \n")), Right(" a  b \n"))
      assertEquals(XmlDerivationUtils.getStringContent(text("   ")), Right("   "))
      assertEquals(XmlDerivationUtils.getStringContent(<e/>), Right(""))
    }
  }

  group("getTextContent") {

    test("trims, and blank is missing content") {
      assertEquals(XmlDerivationUtils.getTextContent(text(" 42 ")), Right("42"))
      assertEquals(XmlDerivationUtils.getTextContent(text("  ")), Left(XmlDecodingError.MissingContent("e")))
    }
  }

  group("getCharContent") {

    test("a single character exactly, otherwise the trimmed text") {
      assertEquals(XmlDerivationUtils.getCharContent(text(" ")), Right(' '))
      assertEquals(XmlDerivationUtils.getCharContent(text("a")), Right('a'))
      assertEquals(XmlDerivationUtils.getCharContent(text(" a\n")), Right('a'))
      assertEquals(XmlDerivationUtils.getCharContent(<e/>), Left(XmlDecodingError.MissingContent("e")))
      assertEquals(
        XmlDerivationUtils.getCharContent(text("ab")),
        Left(XmlDecodingError.InvalidValue("ab", "Char", "e"))
      )
    }
  }

  group("isEmptyStringElement / isEmptyElement") {

    test("blank text is empty only for isEmptyElement") {
      assertEquals(XmlDerivationUtils.isEmptyStringElement(<e/>), true)
      assertEquals(XmlDerivationUtils.isEmptyStringElement(text("")), true)
      assertEquals(XmlDerivationUtils.isEmptyStringElement(text(" ")), false)
      assertEquals(XmlDerivationUtils.isEmptyStringElement(<e a="1"/>), false)
      assertEquals(XmlDerivationUtils.isEmptyStringElement(<e><c/></e>), false)
      assertEquals(XmlDerivationUtils.isEmptyElement(text(" ")), true)
      assertEquals(XmlDerivationUtils.isEmptyElement(<e><c/></e>), false)
    }
  }

  group("elemToString") {

    test("indents nested elements and writes text-only elements exactly") {
      val long = "word " * 40
      val elem = XmlDerivationUtils.makeElem(
        "p",
        List("a" -> "x  y"),
        List(text(" a "), text("x  y\nz"), text(long), XmlDerivationUtils.makeElem("q", Nil, List(text("1"))))
      )
      assertEquals(
        XmlDerivationUtils.elemToString(elem),
        s"""<p a="x  y">
           |  <e> a </e>
           |  <e>x  y
           |z</e>
           |  <e>$long</e>
           |  <q>
           |    <e>1</e>
           |  </q>
           |</p>""".stripMargin
      )
    }

    test("writes elements without children and with mixed content exactly") {
      assertEquals(XmlDerivationUtils.elemToString(<e a="x  y"/>), """<e a="x  y"/>""")
      assertEquals(XmlDerivationUtils.elemToString(<e> t <c/> u </e>), "<e> t <c/> u </e>")
    }
  }
}
