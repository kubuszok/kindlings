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

/** Instances for a 100-field case class with `@xmlWrapper` / `@xmlUnwrapped` fields and fields of a 100-case enum (as
  * elements and as attributes), derived in a class of their own so their bytecode can be measured.
  */
object XmlWideAnnotatedInstances {
  val default: KindlingsXmlCodec[XmlWideAnnotated] = KindlingsXmlCodec.derived[XmlWideAnnotated]

  // the enum as text, absent wrappers as defaults, empty collections and `None` skipped
  val strings: KindlingsXmlCodec[XmlWideAnnotated] = {
    implicit val config: XmlConfig =
      XmlConfig().withEnumAsStrings.withAttributesByDefault.withUseDefaults.withTransientEmpty.withTransientNone
    KindlingsXmlCodec.derived[XmlWideAnnotated]
  }

  // `config` is a parameter: every choice (`enumAsStrings`, `useDefaults`, ...) is made at runtime
  def codecWith(config: XmlConfig): KindlingsXmlCodec[XmlWideAnnotated] = {
    implicit val runtimeConfig: XmlConfig = config
    KindlingsXmlCodec.derived[XmlWideAnnotated]
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

  test("no method of the instances with wrappers, unwrapped collections and a 100-case enum exceeds the JIT limit") {
    val sizes = BytecodeSizes.methodSizes(XmlWideAnnotatedInstances.getClass)
    assert(sizes.map(_._3).sum > 5000, s"expected the derived instances' code, got: $sizes")
    val huge = sizes.filter(_._3 > BytecodeSizes.HugeMethodLimit)
    assert(huge.isEmpty, s"methods larger than ${BytecodeSizes.HugeMethodLimit} B: ${huge.mkString(", ")}")
  }

  test("the instances with wrappers and enums round-trip, with compile-time and runtime configurations") {
    val value = XmlWideAnnotated(
      f0 = List(1, 2),
      f1 = List("a", "b"),
      f2 = XmlWideCase99,
      f3 = Some(XmlWideCase50),
      f4 = Some("x"),
      f97 = XmlWideCase0,
      f98 = Some(XmlWideCase1)
    )
    val stringsConfig =
      XmlConfig().withEnumAsStrings.withAttributesByDefault.withUseDefaults.withTransientEmpty.withTransientNone
    List(
      XmlWideAnnotatedInstances.default -> XmlConfig(),
      XmlWideAnnotatedInstances.strings -> stringsConfig
    ).foreach { case (codec, config) =>
      val runtime = XmlWideAnnotatedInstances.codecWith(config)
      List(value, XmlWideAnnotated()).foreach { v =>
        val elem = codec.encode(v, "x")
        assertEquals(runtime.encode(v, "x"), elem)
        assertEquals(codec.decode(elem), Right(v))
        assertEquals(runtime.decode(elem), Right(v))
      }
    }
    assertEquals(XmlWideAnnotatedInstances.strings.decode(<x/>), Right(XmlWideAnnotated()))
    assertEquals(
      XmlWideAnnotatedInstances.strings.encode(value, "x").attribute("f2").map(_.text),
      Some("XmlWideCase99")
    )
  }
}
