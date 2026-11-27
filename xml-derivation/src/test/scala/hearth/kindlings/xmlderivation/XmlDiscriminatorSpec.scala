package hearth.kindlings.xmlderivation

import hearth.MacroSuite
import hearth.kindlings.xmlderivation.annotations.xmlDiscriminator

@xmlDiscriminator("kind") sealed trait XmlKindShape
case class XmlKindCircle(r: Int) extends XmlKindShape
case object XmlKindDot extends XmlKindShape

@xmlDiscriminator("lvl") sealed trait XmlLevel
case object XmlLow extends XmlLevel
case object XmlHigh extends XmlLevel

// the annotation of the type a value is encoded as counts: `inner` for an XmlInnerNode, not for an XmlOuterNode
sealed trait XmlOuterNode
@xmlDiscriminator("inner") sealed trait XmlInnerNode extends XmlOuterNode
case class XmlInnerLeaf(n: Int) extends XmlInnerNode
case class XmlOuterLeaf(s: String) extends XmlOuterNode

case class XmlKindHolder(
    shape: XmlKindShape,
    plain: Shape,
    level: XmlLevel,
    inner: XmlInnerNode,
    outer: XmlOuterNode,
    shapes: List[XmlKindShape]
)

// configs only known at runtime (the macro cannot evaluate a `var`)
object XmlDiscriminatorRuntimeConfigs {
  @volatile var default: XmlConfig = XmlConfig()
  @volatile var renamed: XmlConfig = XmlConfig().withDiscriminator("t")
  @volatile var none: XmlConfig = XmlConfig().withNoDiscriminator
}

/** `@xmlDiscriminator(name)` on a sealed type: its discriminator attribute, whatever the configuration says. */
final class XmlDiscriminatorSpec extends MacroSuite {

  private def roundTripped[A](codec: KindlingsXmlCodec[A], value: A): scala.xml.Elem = {
    val elem = codec.encode(value, "x")
    assertEquals(codec.decode(elem), Right(value))
    assertEquals(codec.decode(scala.xml.XML.loadString(elem.toString)), Right(value))
    elem
  }

  private val holder = XmlKindHolder(
    XmlKindCircle(1),
    Circle(1.5),
    XmlLow,
    XmlInnerLeaf(2),
    XmlInnerLeaf(3),
    List(XmlKindDot, XmlKindCircle(4))
  )

  private val annotated =
    """<shape kind="XmlKindCircle"><r>1</r></shape>"""
  private val annotatedRest =
    """<level lvl="XmlLow"/><inner inner="XmlInnerLeaf"><n>2</n></inner>"""
  private val annotatedItems =
    """<shapes><item kind="XmlKindDot"/><item kind="XmlKindCircle"><r>4</r></item></shapes>"""

  private val withDefault = s"<x>$annotated" + """<plain type="Circle"><radius>1.5</radius></plain>""" +
    annotatedRest + """<outer type="XmlInnerLeaf"><n>3</n></outer>""" + s"$annotatedItems</x>"
  private val withRenamed = s"<x>$annotated" + """<plain t="Circle"><radius>1.5</radius></plain>""" +
    annotatedRest + """<outer t="XmlInnerLeaf"><n>3</n></outer>""" + s"$annotatedItems</x>"
  private val withNone = s"<x>$annotated" + "<plain><Circle><radius>1.5</radius></Circle></plain>" +
    annotatedRest + "<outer><XmlInnerLeaf><n>3</n></XmlInnerLeaf></outer>" + s"$annotatedItems</x>"

  group("with a configuration known at compile time") {

    test("the annotated types use their attribute, the others the configured one") {
      val codec = KindlingsXmlCodec.derived[XmlKindHolder]
      assertEquals(roundTripped(codec, holder).toString, withDefault)
    }

    test("withDiscriminator(name) does not change the annotated types") {
      val codec = KindlingsXmlCodec.derived[XmlKindHolder](using XmlConfig().withDiscriminator("t"))
      assertEquals(roundTripped(codec, holder).toString, withRenamed)
    }

    test("withNoDiscriminator: the annotated types keep their attribute") {
      val codec = KindlingsXmlCodec.derived[XmlKindHolder](using XmlConfig().withNoDiscriminator)
      assertEquals(roundTripped(codec, holder).toString, withNone)
    }

    test("at the top level, and through the inline methods") {
      assertEquals(
        KindlingsXmlEncoder.encode[XmlKindShape](XmlKindCircle(1), "s").toString,
        """<s kind="XmlKindCircle"><r>1</r></s>"""
      )
      assertEquals(KindlingsXmlDecoder.decode[XmlKindShape](<s kind="XmlKindDot"/>), Right(XmlKindDot))
      assertEquals(
        KindlingsXmlEncoder.encode[XmlKindShape](XmlKindDot, "s")(using XmlConfig().withNoDiscriminator).toString,
        """<s kind="XmlKindDot"/>"""
      )
    }

    test("errors name the attribute that is used") {
      assertEquals(
        KindlingsXmlDecoder.decode[XmlKindShape](<s type="XmlKindDot"/>),
        Left(XmlDecodingError.MissingDiscriminator("kind", "s"))
      )
      assertEquals(
        KindlingsXmlDecoder.decode[XmlLevel](<l type="XmlLow"/>),
        Left(XmlDecodingError.MissingDiscriminator("lvl", "l"))
      )
      KindlingsXmlDecoder.decode[XmlKindShape](<s kind="Square"/>) match {
        case Left(XmlDecodingError.UnknownDiscriminator("Square", known)) =>
          assertEquals(known.sorted, List("XmlKindCircle", "XmlKindDot"))
        case other => fail(s"unexpected: $other")
      }
    }

    test("an enum of singletons with withEnumAsStrings is its name (no discriminator to rename)") {
      assertEquals(
        KindlingsXmlEncoder.encode[XmlLevel](XmlHigh, "l")(using XmlConfig().withEnumAsStrings).toString,
        "<l>XmlHigh</l>"
      )
      assertEquals(
        KindlingsXmlDecoder.decode[XmlLevel](<l>XmlHigh</l>)(using XmlConfig().withEnumAsStrings),
        Right(XmlHigh)
      )
    }
  }

  group("with a configuration only known at runtime") {

    test("writes and reads the same XML") {
      val default = {
        implicit val config: XmlConfig = XmlDiscriminatorRuntimeConfigs.default
        KindlingsXmlCodec.derived[XmlKindHolder]
      }
      assertEquals(roundTripped(default, holder).toString, withDefault)
      val renamed = {
        implicit val config: XmlConfig = XmlDiscriminatorRuntimeConfigs.renamed
        KindlingsXmlCodec.derived[XmlKindHolder]
      }
      assertEquals(roundTripped(renamed, holder).toString, withRenamed)
      val none = {
        implicit val config: XmlConfig = XmlDiscriminatorRuntimeConfigs.none
        KindlingsXmlCodec.derived[XmlKindHolder]
      }
      assertEquals(roundTripped(none, holder).toString, withNone)
    }

    test("errors name the attribute that is used") {
      val codec = {
        implicit val config: XmlConfig = XmlDiscriminatorRuntimeConfigs.renamed
        KindlingsXmlCodec.derived[XmlKindShape]
      }
      assertEquals(codec.decode(<s t="XmlKindDot"/>), Left(XmlDecodingError.MissingDiscriminator("kind", "s")))
    }
  }
}
