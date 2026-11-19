package hearth.kindlings.xmlderivation

import hearth.MacroSuite

/** Instances for a 100-field case class with defaults, derived in a class of their own so their bytecode can be
  * measured.
  */
object XmlWideInstances {
  // configurations evaluated at compile time: each field is read from an element / from an attribute
  val elements: KindlingsXmlCodec[XmlWideDefaults] = KindlingsXmlCodec.derived[XmlWideDefaults]
  val attributes: KindlingsXmlCodec[XmlWideDefaults] = {
    implicit val config: XmlConfig = XmlConfig().withAttributesByDefault.withUseDefaults
    KindlingsXmlCodec.derived[XmlWideDefaults]
  }

  // `None` is skipped (a skip condition per `Option` field)
  val transientNone: KindlingsXmlCodec[XmlWideDefaults] = {
    implicit val config: XmlConfig = XmlConfig.default.withTransientNone
    KindlingsXmlCodec.derived[XmlWideDefaults]
  }

  // `config` is a parameter: the macro cannot evaluate it and reads it (incl. `defaultFieldMode`) at runtime
  def codecWith(config: XmlConfig): KindlingsXmlCodec[XmlWideDefaults] = {
    implicit val runtimeConfig: XmlConfig = config
    KindlingsXmlCodec.derived[XmlWideDefaults]
  }
}

/** Guards against generated methods too large to be JIT-compiled (see `docs/research/generated-code-audit-2026-11.md`).
  * JVM only: it reads the class files.
  */
final class XmlBytecodeSizeSpec extends MacroSuite {

  test("no method of the instances derived for a 100-field case class exceeds HotSpot's JIT limit") {
    val sizes = BytecodeSizes.methodSizes(XmlWideInstances.getClass)
    assert(sizes.map(_._3).sum > 5000, s"expected the derived instances' code, got: $sizes")
    val huge = sizes.filter(_._3 > BytecodeSizes.HugeMethodLimit)
    assert(huge.isEmpty, s"methods larger than ${BytecodeSizes.HugeMethodLimit} B: ${huge.mkString(", ")}")
  }

  test("the instances round-trip, with compile-time and runtime configurations") {
    val value = XmlWideDefaults(f0 = Some("x"), f1 = 1, f2 = " s ", f99 = 5L)
    List(
      XmlWideInstances.elements -> XmlConfig(),
      XmlWideInstances.attributes -> XmlConfig().withAttributesByDefault.withUseDefaults,
      XmlWideInstances.transientNone -> XmlConfig().withTransientNone
    ).foreach { case (codec, config) =>
      val runtime = XmlWideInstances.codecWith(config)
      val elem = codec.encode(value, "x")
      assertEquals(runtime.encode(value, "x"), elem)
      assertEquals(codec.decode(elem), Right(value))
      assertEquals(runtime.decode(elem), Right(value))
    }
    assertEquals(XmlWideInstances.attributes.decode(<x/>), Right(XmlWideDefaults()))
    assertEquals(XmlWideInstances.attributes.encode(value, "x").child.size, 0)
  }
}
