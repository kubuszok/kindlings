package hearth.kindlings.xmlderivation

import hearth.MacroSuite
import hearth.kindlings.xmlderivation.annotations.xmlAttribute

case class XmlJavaPaint(color: XmlJavaColor, @xmlAttribute accent: XmlJavaColor, more: List[XmlJavaColor])

case class XmlJavaPaintElements(color: XmlJavaColor, more: List[XmlJavaColor])

object XmlJavaEnumRuntimeConfigs {
  @volatile var strings: XmlConfig = XmlConfig().withEnumAsStrings
}

/** Java enums (JVM only): the same encodings as an enum of case objects. */
final class XmlJavaEnumSpec extends MacroSuite {

  private def roundTripped[A](codec: KindlingsXmlCodec[A], value: A): scala.xml.Elem = {
    val elem = codec.encode(value, "x")
    assertEquals(codec.decode(elem), Right(value))
    assertEquals(codec.decode(scala.xml.XML.loadString(elem.toString)), Right(value))
    elem
  }

  private val paint = XmlJavaPaint(XmlJavaColor.RED, XmlJavaColor.BLUE, List(XmlJavaColor.GREEN))

  test("default: the value name in the discriminator attribute") {
    val codec = KindlingsXmlCodec.derived[XmlJavaPaintElements]
    assertEquals(
      roundTripped(codec, XmlJavaPaintElements(XmlJavaColor.RED, List(XmlJavaColor.GREEN))).toString,
      """<x><color type="RED"/><more><item type="GREEN"/></more></x>"""
    )
    // as an attribute it needs withEnumAsStrings
    compileErrors(
      "hearth.kindlings.xmlderivation.KindlingsXmlCodec.derived[hearth.kindlings.xmlderivation.XmlJavaPaint]"
    ).check("@xmlAttribute on the enum hearth.kindlings.xmlderivation.XmlJavaColor writes the case name")
  }

  test("withEnumAsStrings, known at compile time and only at runtime") {
    val expected = """<x accent="BLUE"><color>RED</color><more><item>GREEN</item></more></x>"""
    val static = {
      implicit val config: XmlConfig = XmlConfig().withEnumAsStrings
      KindlingsXmlCodec.derived[XmlJavaPaint]
    }
    assertEquals(roundTripped(static, paint).toString, expected)
    val runtime = {
      implicit val config: XmlConfig = XmlJavaEnumRuntimeConfigs.strings
      KindlingsXmlCodec.derived[XmlJavaPaint]
    }
    assertEquals(roundTripped(runtime, paint).toString, expected)
    assert(static.decode(<x accent="BLUE"><color>PINK</color></x>).isLeft)
  }

  test("withAttributesByDefault and withEnumAsStrings: an attribute") {
    implicit val config: XmlConfig = XmlConfig().withEnumAsStrings.withAttributesByDefault
    val codec = KindlingsXmlCodec.derived[XmlJavaPaint]
    assertEquals(
      roundTripped(codec, paint).toString,
      """<x color="RED" accent="BLUE"><more><item>GREEN</item></more></x>"""
    )
  }
}
