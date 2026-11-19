package hearth.kindlings.xmlderivation

import hearth.MacroSuite
import hearth.kindlings.xmlderivation.annotations.xmlAttribute

case class XmlEmptyStr(name: String, n: Int)
case class XmlEmptyStrAttr(@xmlAttribute label: String, n: Int)
case class XmlEmptyOptStr(name: Option[String], n: Int)
case class XmlEmptyOptStrAttr(@xmlAttribute label: Option[String], n: Int)
case class XmlEmptyStrList(items: List[String])

// a config only known at runtime (the macro cannot evaluate a `var`)
object XmlEmptyStringRuntimeConfigs {
  @volatile var transientNone: XmlConfig = XmlConfig().withTransientNone
}

/** An empty `String` is present-but-empty (an empty element or attribute value), distinct from an absent one. */
final class XmlEmptyStringSpec extends MacroSuite {

  // in memory, and through the XML text (which is what a user reads back)
  private def roundTrips[A](codec: KindlingsXmlCodec[A], value: A): Unit = {
    val elem = codec.encode(value, "x")
    assertEquals(codec.decode(elem), Right(value), s"encoded as $elem")
    assertEquals(
      hearth.kindlings.xmlderivation.internal.runtime.XmlDerivationUtils
        .parseAndDecode(elem.toString, codec.decode),
      Right(value),
      s"encoded as $elem"
    )
  }

  group("an empty String") {

    test("element field round-trips") {
      val codec = KindlingsXmlCodec.derived[XmlEmptyStr]
      roundTrips(codec, XmlEmptyStr("", 1))
      roundTrips(codec, XmlEmptyStr("a", 1))
      assertEquals(codec.decode(<x><name/><n>1</n></x>), Right(XmlEmptyStr("", 1)))
      // absent is still an error
      assertEquals(codec.decode(<x><n>1</n></x>), Left(XmlDecodingError.MissingElement("name", "x")))
    }

    test("attribute field round-trips") {
      val codec = KindlingsXmlCodec.derived[XmlEmptyStrAttr]
      roundTrips(codec, XmlEmptyStrAttr("", 1))
      roundTrips(codec, XmlEmptyStrAttr("a", 1))
      roundTrips(codec, XmlEmptyStrAttr("a & <b>", 1)) // (entity references: several nodes in the attribute value)
      assertEquals(codec.decode(<x label=""><n>1</n></x>), Right(XmlEmptyStrAttr("", 1)))
      assertEquals(codec.decode(<x><n>1</n></x>), Left(XmlDecodingError.MissingAttribute("label", "x")))
    }

    test("withTransientEmpty still writes an empty String (only empty collections are skipped)") {
      implicit val config: XmlConfig = XmlConfig().withTransientEmpty
      val codec = KindlingsXmlCodec.derived[XmlEmptyStr]
      roundTrips(codec, XmlEmptyStr("", 1))
      val listCodec = KindlingsXmlCodec.derived[XmlEmptyStrList]
      roundTrips(listCodec, XmlEmptyStrList(List("")))
      roundTrips(listCodec, XmlEmptyStrList(Nil))
    }

    test("String items of a collection round-trip") {
      val codec = KindlingsXmlCodec.derived[XmlEmptyStrList]
      roundTrips(codec, XmlEmptyStrList(List("a", "", "b")))
      roundTrips(codec, XmlEmptyStrList(List("")))
      roundTrips(codec, XmlEmptyStrList(Nil))
    }

    test("Some(\"\") in an Option attribute round-trips (None is an absent attribute)") {
      val codec = KindlingsXmlCodec.derived[XmlEmptyOptStrAttr]
      roundTrips(codec, XmlEmptyOptStrAttr(Some(""), 1))
      roundTrips(codec, XmlEmptyOptStrAttr(Some("a"), 1))
      roundTrips(codec, XmlEmptyOptStrAttr(None, 1))
      assertEquals(codec.encode(XmlEmptyOptStrAttr(None, 1), "x").attribute("label"), None)
      locally {
        implicit val config: XmlConfig = XmlConfig().withTransientNone
        val transientNone = KindlingsXmlCodec.derived[XmlEmptyOptStrAttr]
        roundTrips(transientNone, XmlEmptyOptStrAttr(Some(""), 1))
        roundTrips(transientNone, XmlEmptyOptStrAttr(None, 1))
      }
    }

    test("Some(\"\") in an Option element round-trips with withTransientNone") {
      implicit val config: XmlConfig = XmlConfig().withTransientNone
      val codec = KindlingsXmlCodec.derived[XmlEmptyOptStr]
      roundTrips(codec, XmlEmptyOptStr(Some(""), 1))
      roundTrips(codec, XmlEmptyOptStr(Some("a"), 1))
      roundTrips(codec, XmlEmptyOptStr(None, 1))
      // an empty element of an Option[Int]: still None (no value to decode)
      val intCodec = KindlingsXmlCodec.derived[Box[Option[Int]]]
      assertEquals(intCodec.decode(<box><value/></box>), Right(Box(Option.empty[Int])))
    }

    test("Some(\"\") in an Option element round-trips with withTransientNone known only at runtime") {
      implicit val config: XmlConfig = XmlEmptyStringRuntimeConfigs.transientNone
      val codec = KindlingsXmlCodec.derived[XmlEmptyOptStr]
      roundTrips(codec, XmlEmptyOptStr(Some(""), 1))
      roundTrips(codec, XmlEmptyOptStr(None, 1))
      val intCodec = KindlingsXmlCodec.derived[Box[Option[Int]]]
      assertEquals(intCodec.decode(<box><value/></box>), Right(Box(Option.empty[Int])))
    }

    test("an Option element without withTransientNone: an empty element is the encoding of None") {
      val codec = KindlingsXmlCodec.derived[XmlEmptyOptStr]
      roundTrips(codec, XmlEmptyOptStr(None, 1))
      roundTrips(codec, XmlEmptyOptStr(Some("a"), 1))
      // None and Some("") are both written as an empty element: it decodes as None
      assertEquals(codec.decode(codec.encode(XmlEmptyOptStr(Some(""), 1), "x")), Right(XmlEmptyOptStr(None, 1)))
    }
  }
}
