package hearth.kindlings.xmlderivation

import hearth.MacroSuite
import hearth.kindlings.xmlderivation.annotations.{xmlAttribute, xmlContent, xmlName}

enum XmlPlanet {
  case Mercury
  @xmlName("terra") case Earth
  case Mars
}

enum XmlFigure {
  case Dot
  case Line(length: Int)
}

case class XmlTrip(from: XmlPlanet, @xmlAttribute to: XmlPlanet, via: List[XmlPlanet], back: Option[XmlPlanet])
case class XmlPlanetLabel(@xmlAttribute id: Int, @xmlContent planet: XmlPlanet)
case class XmlDerivedTrip(planet: XmlPlanet) derives KindlingsXmlCodec

object XmlScala3EnumRuntimeConfigs {
  @volatile var default: XmlConfig = XmlConfig()
  @volatile var strings: XmlConfig = XmlConfig().withEnumAsStrings
}

/** Scala 3 enums: parameterless cases are an enum of singletons, cases with parameters a sealed hierarchy. */
final class XmlScala3EnumSpec extends MacroSuite {

  private def roundTripped[A](codec: KindlingsXmlCodec[A], value: A): scala.xml.Elem = {
    val elem = codec.encode(value, "x")
    assertEquals(codec.decode(elem), Right(value))
    assertEquals(codec.decode(scala.xml.XML.loadString(elem.toString)), Right(value))
    elem
  }

  private val trip = XmlTrip(XmlPlanet.Mercury, XmlPlanet.Mars, List(XmlPlanet.Earth), Some(XmlPlanet.Earth))

  test("default: the case name in the discriminator attribute (@xmlName renames a case)") {
    val codec = KindlingsXmlCodec.derived[XmlTrip]
    assertEquals(
      roundTripped(codec, trip).toString,
      """<x to="Mars"><from type="Mercury"/><via><item type="terra"/></via><back type="terra"/></x>"""
    )
    assertEquals(codec.decode(codec.encode(trip.copy(back = None), "x")), Right(trip.copy(back = None)))
  }

  test("withEnumAsStrings, known at compile time and only at runtime") {
    val expected = """<x to="Mars"><from>Mercury</from><via><item>terra</item></via><back>terra</back></x>"""
    val static = {
      given XmlConfig = XmlConfig().withEnumAsStrings
      KindlingsXmlCodec.derived[XmlTrip]
    }
    assertEquals(roundTripped(static, trip).toString, expected)
    val runtime = {
      given XmlConfig = XmlScala3EnumRuntimeConfigs.strings
      KindlingsXmlCodec.derived[XmlTrip]
    }
    assertEquals(roundTripped(runtime, trip).toString, expected)
    val runtimeDefault = {
      given XmlConfig = XmlScala3EnumRuntimeConfigs.default
      KindlingsXmlCodec.derived[XmlTrip]
    }
    assertEquals(roundTripped(runtimeDefault, trip), KindlingsXmlCodec.derived[XmlTrip].encode(trip, "x"))
  }

  test("withAttributesByDefault and withEnumAsStrings: attributes") {
    given XmlConfig = XmlConfig().withEnumAsStrings.withAttributesByDefault
    val codec = KindlingsXmlCodec.derived[XmlTrip]
    assertEquals(
      roundTripped(codec, trip).toString,
      """<x from="Mercury" to="Mars" back="terra"><via><item>terra</item></via></x>"""
    )
  }

  test("@xmlContent and the top level") {
    val codec = KindlingsXmlCodec.derived[XmlPlanetLabel]
    assertEquals(roundTripped(codec, XmlPlanetLabel(1, XmlPlanet.Earth)).toString, """<x id="1">terra</x>""")
    assertEquals(KindlingsXmlEncoder.encode(XmlPlanet.Mars, "p").toString, """<p type="Mars"/>""")
    assertEquals(KindlingsXmlDecoder.decode[XmlPlanet](<p type="terra"/>), Right(XmlPlanet.Earth))
    locally {
      given XmlConfig = XmlConfig().withEnumAsStrings
      assertEquals(KindlingsXmlEncoder.encode(XmlPlanet.Mars, "p").toString, "<p>Mars</p>")
      assertEquals(KindlingsXmlDecoder.decode[XmlPlanet](<p>terra</p>), Right(XmlPlanet.Earth))
    }
  }

  test("an enum with a case with parameters keeps the discriminator with withEnumAsStrings") {
    given XmlConfig = XmlConfig().withEnumAsStrings
    val codec = KindlingsXmlCodec.derived[XmlFigure]
    assertEquals(roundTripped(codec, XmlFigure.Dot).toString, """<x type="Dot"/>""")
    assertEquals(roundTripped(codec, XmlFigure.Line(2)).toString, """<x type="Line"><length>2</length></x>""")
  }

  test("derives") {
    val codec = summon[KindlingsXmlCodec[XmlDerivedTrip]]
    assertEquals(codec.decode(codec.encode(XmlDerivedTrip(XmlPlanet.Mars), "x")), Right(XmlDerivedTrip(XmlPlanet.Mars)))
  }
}
