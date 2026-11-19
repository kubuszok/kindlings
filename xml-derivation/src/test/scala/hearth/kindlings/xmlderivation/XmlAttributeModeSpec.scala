package hearth.kindlings.xmlderivation

import hearth.MacroSuite
import hearth.kindlings.xmlderivation.annotations.{xmlAttribute, xmlContent, xmlElement, xmlName}
import hearth.kindlings.xmlderivation.internal.runtime.XmlDerivationUtils

case class XmlAttrModePerson(name: String, age: Int, score: Double, active: Boolean)
case class XmlAttrModeOverrides(@xmlElement title: String, @xmlName("n") count: Int, other: String)
case class XmlAttrModeOptions(label: Option[String], count: Option[Int], wrapped: Option[WrappedInt])
case class XmlAttrModeStructured(
    id: Int,
    tags: List[String],
    lookup: Map[String, Int],
    address: Address,
    shape: Shape,
    maybeAddress: Option[Address],
    wrapped: WrappedInt
)
case class XmlAttrModeDefaults(name: String, age: Int = 7, nick: Option[String] = Some("n"))
case class XmlAttrModeContent(lang: String, @xmlContent text: String)
case class XmlAttrAnnotatedDefault(@xmlAttribute n: Int = 7)
case class XmlAttrAnnotatedValueType(@xmlAttribute wrapped: WrappedInt, @xmlAttribute opt: Option[WrappedInt])

// configs only known at runtime (the macro cannot evaluate a `var`)
object XmlAttrModeRuntimeConfigs {
  @volatile var attributes: XmlConfig = XmlConfig().withAttributesByDefault
  @volatile var elements: XmlConfig = XmlConfig()
  @volatile var attributesWithDefaults: XmlConfig = XmlConfig().withAttributesByDefault.withUseDefaults
}

/** `withAttributesByDefault`: what the encoder writes as an attribute, the decoder reads from an attribute. */
final class XmlAttributeModeSpec extends MacroSuite {

  // in memory, and through the XML text (which is what a user reads back)
  private def roundTrips[A](codec: KindlingsXmlCodec[A], value: A): Unit = {
    val _ = roundTripped(codec, value)
  }

  private def roundTripped[A](codec: KindlingsXmlCodec[A], value: A): scala.xml.Elem = {
    val elem = codec.encode(value, "x")
    assertEquals(codec.decode(elem), Right(value), s"encoded as $elem")
    assertEquals(XmlDerivationUtils.parseAndDecode(elem.toString, codec.decode), Right(value), s"encoded as $elem")
    elem
  }

  private def attr(elem: scala.xml.Elem, name: String): Option[String] = elem.attribute(name).map(_.text)
  private def childNames(elem: scala.xml.Elem): List[String] =
    elem.child.collect { case e: scala.xml.Elem => e.label }.toList

  group("withAttributesByDefault known at compile time") {

    test("fields are written and read as attributes") {
      implicit val config: XmlConfig = XmlConfig().withAttributesByDefault
      val codec = KindlingsXmlCodec.derived[XmlAttrModePerson]
      val elem = roundTripped(codec, XmlAttrModePerson("Alice", 30, 1.5, active = true))
      assertEquals(attr(elem, "name"), Some("Alice"))
      assertEquals(attr(elem, "age"), Some("30"))
      assertEquals(childNames(elem), Nil)
      assertEquals(
        codec.decode(<x name="Bob" age="1" score="2.0" active="false"/>),
        Right(XmlAttrModePerson("Bob", 1, 2.0, active = false))
      )
      assertEquals(
        codec.decode(<x age="1" score="2.0" active="false"/>),
        Left(XmlDecodingError.MissingAttribute("name", "x"))
      )
    }

    test("the inline decoder reads attributes too") {
      implicit val config: XmlConfig = XmlConfig().withAttributesByDefault
      val value = XmlAttrModePerson("Alice", 30, 1.5, active = true)
      val elem = KindlingsXmlEncoder.encode(value, "x")
      assertEquals(KindlingsXmlDecoder.decode[XmlAttrModePerson](elem), Right(value))
      assertEquals(KindlingsXmlDecoder.fromXmlString[XmlAttrModePerson](elem.toString), Right(value))
    }

    test("with a field name mapper") {
      implicit val config: XmlConfig = XmlConfig().withAttributesByDefault.withSnakeCaseFieldNames
      val codec = KindlingsXmlCodec.derived[CamelCasePerson]
      val elem = roundTripped(codec, CamelCasePerson("A", "B"))
      assertEquals(attr(elem, "first_name"), Some("A"))
    }

    test("@xmlElement still makes a child element, @xmlName still renames") {
      implicit val config: XmlConfig = XmlConfig().withAttributesByDefault
      val codec = KindlingsXmlCodec.derived[XmlAttrModeOverrides]
      val elem = roundTripped(codec, XmlAttrModeOverrides("T", 3, "o"))
      assertEquals(childNames(elem), List("title"))
      assertEquals(attr(elem, "n"), Some("3"))
      assertEquals(attr(elem, "other"), Some("o"))
    }

    test("Option fields: Some is the value, None is an absent attribute") {
      implicit val config: XmlConfig = XmlConfig().withAttributesByDefault
      val codec = KindlingsXmlCodec.derived[XmlAttrModeOptions]
      val some = roundTripped(codec, XmlAttrModeOptions(Some("a"), Some(1), Some(WrappedInt(2))))
      assertEquals(attr(some, "label"), Some("a"))
      assertEquals(attr(some, "wrapped"), Some("2"))
      val none = roundTripped(codec, XmlAttrModeOptions(None, None, None))
      assertEquals(none.attributes.isEmpty, true, none.toString)
      roundTrips(codec, XmlAttrModeOptions(Some(""), None, None))
    }

    test("collections, maps, case classes, sealed traits stay child elements; value types become attributes") {
      implicit val config: XmlConfig = XmlConfig().withAttributesByDefault
      val codec = KindlingsXmlCodec.derived[XmlAttrModeStructured]
      val value = XmlAttrModeStructured(
        1,
        List("a", "b"),
        Map("k" -> 2),
        Address("s", "c"),
        Circle(1.0),
        Some(Address("s2", "c2")),
        WrappedInt(5)
      )
      val elem = roundTripped(codec, value)
      assertEquals(attr(elem, "id"), Some("1"))
      assertEquals(attr(elem, "wrapped"), Some("5"))
      assertEquals(childNames(elem), List("tags", "lookup", "address", "shape", "maybeAddress"))
      // nested case classes use the same configuration: their fields are attributes
      assertEquals((elem \ "address").head.asInstanceOf[scala.xml.Elem].attribute("street").map(_.text), Some("s"))
      roundTrips(codec, value.copy(tags = Nil, lookup = Map.empty, maybeAddress = None))
    }

    test("withUseDefaults: an absent attribute is the default value") {
      implicit val config: XmlConfig = XmlConfig().withAttributesByDefault.withUseDefaults
      val codec = KindlingsXmlCodec.derived[XmlAttrModeDefaults]
      roundTrips(codec, XmlAttrModeDefaults("a", 1, Some("x")))
      assertEquals(codec.decode(<x name="a"/>), Right(XmlAttrModeDefaults("a")))
    }

    test("without withUseDefaults: an absent attribute is an error (an absent Option is None)") {
      implicit val config: XmlConfig = XmlConfig().withAttributesByDefault
      val codec = KindlingsXmlCodec.derived[XmlAttrModeDefaults]
      assertEquals(codec.decode(<x name="a"/>), Left(XmlDecodingError.MissingAttribute("age", "x")))
      assertEquals(codec.decode(<x name="a" age="1"/>), Right(XmlAttrModeDefaults("a", 1, None)))
    }

    test("@xmlContent with the other fields as attributes") {
      implicit val config: XmlConfig = XmlConfig().withAttributesByDefault
      val codec = KindlingsXmlCodec.derived[XmlAttrModeContent]
      val elem = roundTripped(codec, XmlAttrModeContent("en", "Hello"))
      assertEquals(elem.toString, """<x lang="en">Hello</x>""")
    }
  }

  group("withAttributesByDefault known only at runtime") {

    test("fields are written and read as attributes") {
      implicit val config: XmlConfig = XmlAttrModeRuntimeConfigs.attributes
      val codec = KindlingsXmlCodec.derived[XmlAttrModePerson]
      val elem = roundTripped(codec, XmlAttrModePerson("Alice", 30, 1.5, active = true))
      assertEquals(attr(elem, "name"), Some("Alice"))
      assertEquals(childNames(elem), Nil)
    }

    test("the same instance reads elements with an elements config") {
      implicit val config: XmlConfig = XmlAttrModeRuntimeConfigs.elements
      val codec = KindlingsXmlCodec.derived[XmlAttrModePerson]
      val elem = roundTripped(codec, XmlAttrModePerson("Alice", 30, 1.5, active = true))
      assertEquals(childNames(elem), List("name", "age", "score", "active"))
    }

    test("@xmlElement still makes a child element") {
      implicit val config: XmlConfig = XmlAttrModeRuntimeConfigs.attributes
      val codec = KindlingsXmlCodec.derived[XmlAttrModeOverrides]
      val elem = roundTripped(codec, XmlAttrModeOverrides("T", 3, "o"))
      assertEquals(childNames(elem), List("title"))
      assertEquals(attr(elem, "n"), Some("3"))
    }

    test("Option fields: Some is the value (not Some(...)), None is an absent attribute") {
      implicit val config: XmlConfig = XmlAttrModeRuntimeConfigs.attributes
      val codec = KindlingsXmlCodec.derived[XmlAttrModeOptions]
      val some = roundTripped(codec, XmlAttrModeOptions(Some("a"), Some(1), Some(WrappedInt(2))))
      assertEquals(attr(some, "label"), Some("a"))
      assertEquals(attr(some, "count"), Some("1"))
      assertEquals(attr(some, "wrapped"), Some("2"))
      val none = roundTripped(codec, XmlAttrModeOptions(None, None, None))
      assertEquals(none.attributes.isEmpty, true, none.toString)
      roundTrips(codec, XmlAttrModeOptions(Some(""), None, None))
    }

    test("collections, maps, case classes, sealed traits stay child elements") {
      implicit val config: XmlConfig = XmlAttrModeRuntimeConfigs.attributes
      val codec = KindlingsXmlCodec.derived[XmlAttrModeStructured]
      val value = XmlAttrModeStructured(
        1,
        List("a", "b"),
        Map("k" -> 2),
        Address("s", "c"),
        Rectangle(1.0, 2.0),
        None,
        WrappedInt(5)
      )
      val elem = roundTripped(codec, value)
      assertEquals(attr(elem, "wrapped"), Some("5"))
      assertEquals(childNames(elem), List("tags", "lookup", "address", "shape", "maybeAddress"))
    }

    test("withUseDefaults: an absent attribute is the default value") {
      implicit val config: XmlConfig = XmlAttrModeRuntimeConfigs.attributesWithDefaults
      val codec = KindlingsXmlCodec.derived[XmlAttrModeDefaults]
      roundTrips(codec, XmlAttrModeDefaults("a", 1, Some("x")))
      assertEquals(codec.decode(<x name="a"/>), Right(XmlAttrModeDefaults("a")))
    }

    test("@xmlContent with the other fields as attributes") {
      implicit val config: XmlConfig = XmlAttrModeRuntimeConfigs.attributes
      val codec = KindlingsXmlCodec.derived[XmlAttrModeContent]
      val elem = roundTripped(codec, XmlAttrModeContent("en", "Hello"))
      assertEquals(elem.toString, """<x lang="en">Hello</x>""")
    }
  }

  group("compile-time and runtime configurations") {

    test("write the same XML (fields in field order)") {
      val value =
        XmlAttrModeStructured(1, List("a"), Map("k" -> 2), Address("s", "c"), Circle(1.0), None, WrappedInt(5))
      val compileTimeAttributes = {
        implicit val config: XmlConfig = XmlConfig.default.withAttributesByDefault
        KindlingsXmlCodec.derived[XmlAttrModeStructured]
      }
      val compileTimeElements = KindlingsXmlCodec.derived[XmlAttrModeStructured]
      val runtimeAttributes = {
        implicit val config: XmlConfig = XmlAttrModeRuntimeConfigs.attributes
        KindlingsXmlCodec.derived[XmlAttrModeStructured]
      }
      val runtimeElements = {
        implicit val config: XmlConfig = XmlAttrModeRuntimeConfigs.elements
        KindlingsXmlCodec.derived[XmlAttrModeStructured]
      }
      assertEquals(runtimeAttributes.encode(value, "x").toString, compileTimeAttributes.encode(value, "x").toString)
      assertEquals(runtimeElements.encode(value, "x").toString, compileTimeElements.encode(value, "x").toString)
      assertEquals(
        childNames(runtimeElements.encode(value, "x")),
        List("id", "tags", "lookup", "address", "shape", "maybeAddress", "wrapped")
      )
    }
  }

  group("@xmlAttribute") {

    test("withUseDefaults: an absent attribute is the default value") {
      implicit val config: XmlConfig = XmlConfig.default.withUseDefaults
      val codec = KindlingsXmlCodec.derived[XmlAttrAnnotatedDefault]
      assertEquals(codec.decode(<x/>), Right(XmlAttrAnnotatedDefault()))
      roundTrips(codec, XmlAttrAnnotatedDefault(3))
      locally {
        implicit val config: XmlConfig = XmlAttrModeRuntimeConfigs.attributesWithDefaults
        assertEquals(KindlingsXmlCodec.derived[XmlAttrAnnotatedDefault].decode(<x/>), Right(XmlAttrAnnotatedDefault()))
      }
      locally {
        implicit val config: XmlConfig = XmlConfig.default
        assertEquals(
          KindlingsXmlCodec.derived[XmlAttrAnnotatedDefault].decode(<x/>),
          Left(XmlDecodingError.MissingAttribute("n", "x"))
        )
      }
    }

    test("a value type attribute holds the unwrapped value") {
      val codec = KindlingsXmlCodec.derived[XmlAttrAnnotatedValueType]
      val elem = roundTripped(codec, XmlAttrAnnotatedValueType(WrappedInt(1), Some(WrappedInt(2))))
      assertEquals(attr(elem, "wrapped"), Some("1"))
      assertEquals(attr(elem, "opt"), Some("2"))
      roundTrips(codec, XmlAttrAnnotatedValueType(WrappedInt(1), None))
    }
  }
}
