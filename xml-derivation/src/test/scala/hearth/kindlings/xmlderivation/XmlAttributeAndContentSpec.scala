package hearth.kindlings.xmlderivation

import hearth.MacroSuite

object XmlAttributeAndContentRuntimeConfigs {
  @volatile var default: XmlConfig = XmlConfig()
}

/** What `@xmlAttribute` and `@xmlContent` accept (single text values), and what they reject at compile time. */
final class XmlAttributeAndContentSpec extends MacroSuite {

  private def roundTripped[A](codec: KindlingsXmlCodec[A], value: A): scala.xml.Elem = {
    val elem = codec.encode(value, "x")
    assertEquals(codec.decode(elem), Right(value))
    assertEquals(codec.decode(scala.xml.XML.loadString(elem.toString)), Right(value))
    elem
  }

  group("@xmlAttribute") {

    test("a type with user-provided instances: the text of its element") {
      val codec = KindlingsXmlCodec.derived[XmlRelease]
      assertEquals(
        roundTripped(codec, XmlRelease(XmlVersion(1, 2), Some(XmlVersion(1, 1)), "n")).toString,
        """<x version="1.2" previous="1.1"><name>n</name></x>"""
      )
      assertEquals(
        roundTripped(codec, XmlRelease(XmlVersion(1, 2), None, "n")).toString,
        """<x version="1.2"><name>n</name></x>"""
      )
      assertEquals(
        codec.decode(<x version="one"><name>n</name></x>),
        Left(XmlDecodingError.InvalidValue("one", "XmlVersion", "attr"))
      )
    }

    test("a type with user-provided instances stays a child element under withAttributesByDefault") {
      implicit val config: XmlConfig = XmlConfig().withAttributesByDefault
      val codec = KindlingsXmlCodec.derived[XmlReleaseElement]
      assertEquals(
        roundTripped(codec, XmlReleaseElement(XmlVersion(1, 2), "n")).toString,
        """<x name="n"><version>1.2</version></x>"""
      )
    }

    test("a case class, a collection, a sealed trait (or an Option of one) is a compile-time error") {
      compileErrors(
        "hearth.kindlings.xmlderivation.KindlingsXmlEncoder.derived[hearth.kindlings.xmlderivation.XmlBadAttributeCaseClass]"
      ).check(
        "Field 'address' of hearth.kindlings.xmlderivation.XmlBadAttributeCaseClass",
        "@xmlAttribute needs a single text value, hearth.kindlings.xmlderivation.Address is not one",
        "Remove @xmlAttribute to write the field as a child element"
      )
      compileErrors(
        "hearth.kindlings.xmlderivation.KindlingsXmlDecoder.derived[hearth.kindlings.xmlderivation.XmlBadAttributeCaseClass]"
      ).check("@xmlAttribute needs a single text value")
      compileErrors(
        "hearth.kindlings.xmlderivation.KindlingsXmlCodec.derived[hearth.kindlings.xmlderivation.XmlBadAttributeList]"
      ).check("@xmlAttribute needs a single text value")
      compileErrors(
        "hearth.kindlings.xmlderivation.KindlingsXmlEncoder.encode(hearth.kindlings.xmlderivation.XmlBadAttributeOptionCaseClass(None), \"x\")"
      ).check("@xmlAttribute needs a single text value")
      compileErrors(
        "hearth.kindlings.xmlderivation.KindlingsXmlDecoder.decode[hearth.kindlings.xmlderivation.XmlBadAttributeSealed](<x/>)"
      ).check("@xmlAttribute needs a single text value")
    }

    test("@xmlAttribute together with @xmlElement is a compile-time error") {
      compileErrors(
        "hearth.kindlings.xmlderivation.KindlingsXmlEncoder.derived[hearth.kindlings.xmlderivation.XmlBadAttributeElement]"
      ).check("@xmlAttribute and @xmlElement cannot be combined")
    }
  }

  group("@xmlContent") {

    test("the other fields are attributes, whatever the default field mode is") {
      val codec = KindlingsXmlCodec.derived[XmlContentPlain]
      assertEquals(
        roundTripped(codec, XmlContentPlain(1, Some("en"), "Hello")).toString,
        """<x id="1" lang="en">Hello</x>"""
      )
      assertEquals(roundTripped(codec, XmlContentPlain(1, None, "Hello")).toString, """<x id="1">Hello</x>""")
      val runtime = {
        implicit val config: XmlConfig = XmlAttributeAndContentRuntimeConfigs.default
        KindlingsXmlCodec.derived[XmlContentPlain]
      }
      assertEquals(
        roundTripped(runtime, XmlContentPlain(1, Some("en"), "Hello")).toString,
        """<x id="1" lang="en">Hello</x>"""
      )
    }

    test("an Option: None is no text") {
      val codec = KindlingsXmlCodec.derived[XmlContentOption]
      assertEquals(roundTripped(codec, XmlContentOption(1, Some("z"))).toString, """<x id="1">z</x>""")
      assertEquals(roundTripped(codec, XmlContentOption(1, Some(" "))).toString, """<x id="1"> </x>""")
      assertEquals(roundTripped(codec, XmlContentOption(1, None)).toString, """<x id="1"/>""")
      val ints = KindlingsXmlCodec.derived[XmlContentOptionInt]
      assertEquals(roundTripped(ints, XmlContentOptionInt(1, Some(5))).toString, """<x id="1">5</x>""")
      assertEquals(roundTripped(ints, XmlContentOptionInt(1, None)).toString, """<x id="1"/>""")
      assertEquals(ints.decode(<x id="1"> </x>), Right(XmlContentOptionInt(1, None)))
    }

    test("a number") {
      val codec = KindlingsXmlCodec.derived[XmlContentInt]
      assertEquals(roundTripped(codec, XmlContentInt(1, 5)).toString, """<x id="1">5</x>""")
      assertEquals(codec.decode(<x id="1"/>), Left(XmlDecodingError.MissingContent("x")))
    }

    test("a type with user-provided instances: what its encoder writes into the element") {
      val codec = KindlingsXmlCodec.derived[XmlReleaseContent]
      assertEquals(roundTripped(codec, XmlReleaseContent("n", XmlVersion(1, 2))).toString, """<x name="n">1.2</x>""")
    }

    test("a case class as content, two contents, or a child element next to it is a compile-time error") {
      compileErrors(
        "hearth.kindlings.xmlderivation.KindlingsXmlEncoder.derived[hearth.kindlings.xmlderivation.XmlBadContentCaseClass]"
      ).check("@xmlContent needs a single text value, hearth.kindlings.xmlderivation.Address is not one")
      compileErrors(
        "hearth.kindlings.xmlderivation.KindlingsXmlDecoder.derived[hearth.kindlings.xmlderivation.XmlBadContentTwice]"
      ).check("more than one @xmlContent field")
      compileErrors(
        "hearth.kindlings.xmlderivation.KindlingsXmlCodec.derived[hearth.kindlings.xmlderivation.XmlBadContentWithElement]"
      ).check("Field 'title'", "next to @xmlContent the other fields have to be attributes")
      compileErrors(
        "hearth.kindlings.xmlderivation.KindlingsXmlEncoder.derived[hearth.kindlings.xmlderivation.XmlBadContentWithChild]"
      ).check("Field 'tags'", "next to @xmlContent the other fields have to be attributes")
    }
  }
}
