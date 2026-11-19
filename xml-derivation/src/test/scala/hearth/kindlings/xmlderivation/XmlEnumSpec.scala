package hearth.kindlings.xmlderivation

import hearth.MacroSuite

// configs only known at runtime (the macro cannot evaluate a `var`)
object XmlEnumRuntimeConfigs {
  @volatile var default: XmlConfig = XmlConfig()
  @volatile var strings: XmlConfig = XmlConfig().withEnumAsStrings
  @volatile var stringsSnake: XmlConfig = XmlConfig().withEnumAsStrings.withSnakeCaseConstructorNames
  @volatile var stringsAttributes: XmlConfig = XmlConfig().withEnumAsStrings.withAttributesByDefault
  @volatile var attributes: XmlConfig = XmlConfig().withAttributesByDefault
  @volatile var noDiscriminator: XmlConfig = XmlConfig().withNoDiscriminator.withKebabCaseConstructorNames
}

/** Enums (sealed traits of case objects, `Enumeration`s), `withEnumAsStrings`, subtype names and sealed hierarchies. */
final class XmlEnumSpec extends MacroSuite {

  // in memory, and through the XML text (which is what a user reads back)
  private def roundTripped[A](codec: KindlingsXmlCodec[A], value: A): scala.xml.Elem = {
    val elem = codec.encode(value, "x")
    assertEquals(codec.decode(elem), Right(value))
    assertEquals(codec.decode(scala.xml.XML.loadString(elem.toString)), Right(value))
    elem
  }

  private val paint = XmlPaint("a", XmlGreen, Some(XmlDarkBlue))
  private val plain = XmlPaint("a", XmlRed, None)

  group("enums of case objects") {

    test("a case object, at the top level") {
      assertEquals(KindlingsXmlEncoder.encode(XmlRed, "x").toString, "<x/>")
      assertEquals(KindlingsXmlDecoder.decode[XmlRed.type](<x/>), Right(XmlRed))
    }

    test("default: the case name in the discriminator attribute (@xmlName renames a case)") {
      val codec = KindlingsXmlCodec.derived[XmlPaint]
      assertEquals(
        roundTripped(codec, paint).toString,
        """<x><name>a</name><color type="verde"/><accent type="XmlDarkBlue"/></x>"""
      )
      assertEquals(roundTripped(codec, plain).toString, """<x><name>a</name><color type="XmlRed"/><accent/></x>""")
      assertEquals(KindlingsXmlEncoder.encode[XmlColor](XmlRed, "c").toString, """<c type="XmlRed"/>""")
      assertEquals(KindlingsXmlDecoder.decode[XmlColor](<c type="verde"/>), Right(XmlGreen))
    }

    test("default: an unknown or a missing discriminator is an error") {
      val names = KindlingsXmlDecoder.decode[XmlColor](<c type="Pink"/>) match {
        case Left(XmlDecodingError.UnknownDiscriminator("Pink", known)) => known
        case other                                                      => fail(s"unexpected: $other")
      }
      assertEquals(names.sorted, List("XmlDarkBlue", "XmlRed", "verde"))
      assertEquals(
        KindlingsXmlDecoder.decode[XmlColor](<c/>),
        Left(XmlDecodingError.MissingDiscriminator("type", "c"))
      )
    }

    test("withNoDiscriminator: an element named after the case") {
      implicit val config: XmlConfig = XmlConfig().withNoDiscriminator
      val codec = KindlingsXmlCodec.derived[XmlPaint]
      assertEquals(
        roundTripped(codec, paint).toString,
        "<x><name>a</name><color><verde/></color><accent><XmlDarkBlue/></accent></x>"
      )
      assertEquals(roundTripped(codec, plain).toString, "<x><name>a</name><color><XmlRed/></color><accent/></x>")
    }

    test("withEnumAsStrings: the case name as the text of the element") {
      implicit val config: XmlConfig = XmlConfig().withEnumAsStrings
      val codec = KindlingsXmlCodec.derived[XmlPaint]
      assertEquals(
        roundTripped(codec, paint).toString,
        "<x><name>a</name><color>verde</color><accent>XmlDarkBlue</accent></x>"
      )
      assertEquals(roundTripped(codec, plain).toString, "<x><name>a</name><color>XmlRed</color><accent/></x>")
      assertEquals(KindlingsXmlEncoder.encode[XmlColor](XmlRed, "c").toString, "<c>XmlRed</c>")
      assertEquals(KindlingsXmlDecoder.decode[XmlColor](<c> verde </c>), Right(XmlGreen))
      assertEquals(
        KindlingsXmlDecoder.fromXmlString[XmlPaint](KindlingsXmlEncoder.toXmlString(paint, "x")),
        Right(paint)
      )
    }

    test("withEnumAsStrings: an unknown name is an error") {
      implicit val config: XmlConfig = XmlConfig().withEnumAsStrings
      val names = KindlingsXmlDecoder.decode[XmlColor](<c>Pink</c>) match {
        case Left(XmlDecodingError.UnknownDiscriminator("Pink", known)) => known
        case other                                                      => fail(s"unexpected: $other")
      }
      assertEquals(names.sorted, List("XmlDarkBlue", "XmlRed", "verde"))
      assert(KindlingsXmlDecoder.decode[XmlColor](<c type="XmlRed"/>).isLeft)
    }

    test("withEnumAsStrings: names transformed by constructorNameMapper (not the @xmlName ones)") {
      implicit val config: XmlConfig = XmlConfig().withEnumAsStrings.withSnakeCaseConstructorNames
      val codec = KindlingsXmlCodec.derived[XmlPaint]
      assertEquals(
        roundTripped(codec, paint).toString,
        "<x><name>a</name><color>verde</color><accent>xml_dark_blue</accent></x>"
      )
    }

    test("withEnumAsStrings: in collections, maps and wrappers") {
      implicit val config: XmlConfig = XmlConfig().withEnumAsStrings
      val codec = KindlingsXmlCodec.derived[XmlPalette]
      assertEquals(
        roundTripped(codec, XmlPalette(List(XmlRed, XmlGreen), Map("k" -> XmlDarkBlue), List(XmlRed))).toString,
        "<x><colors><item>XmlRed</item><item>verde</item></colors>" +
          """<byName><entry key="k"><value>XmlDarkBlue</value></entry></byName><extra><more>XmlRed</more></extra></x>"""
      )
    }

    test("withEnumAsStrings does not change sealed traits with case classes") {
      implicit val config: XmlConfig = XmlConfig().withEnumAsStrings
      val codec = KindlingsXmlCodec.derived[MixedPet]
      assertEquals(roundTripped(codec, Goldfish: MixedPet).toString, """<x type="Goldfish"/>""")
      assertEquals(
        roundTripped(codec, Budgie("b", true): MixedPet).toString,
        """<x type="Budgie"><name>b</name><canTalk>true</canTalk></x>"""
      )
      assertEquals(KindlingsXmlDecoder.decode[MixedPet](<x type="Turtle"/>), Right(Turtle))
    }

    test("@xmlAttribute: the case name as the attribute value, whatever enumAsStrings is") {
      val expected = """<x color="verde" accent="XmlDarkBlue"><name>n</name></x>"""
      val default = KindlingsXmlCodec.derived[XmlPaintAttr]
      assertEquals(roundTripped(default, XmlPaintAttr(XmlGreen, Some(XmlDarkBlue), "n")).toString, expected)
      assertEquals(
        roundTripped(default, XmlPaintAttr(XmlRed, None, "n")).toString,
        """<x color="XmlRed"><name>n</name></x>"""
      )
      val strings = {
        implicit val config: XmlConfig = XmlConfig().withEnumAsStrings
        KindlingsXmlCodec.derived[XmlPaintAttr]
      }
      assertEquals(roundTripped(strings, XmlPaintAttr(XmlGreen, Some(XmlDarkBlue), "n")).toString, expected)
      assert(default.decode(<x color="Pink"><name>n</name></x>).isLeft)
      assertEquals(
        default.decode(<x><name>n</name></x>),
        Left(XmlDecodingError.MissingAttribute("color", "x"))
      )
    }

    test("@xmlContent: the case name as the text") {
      val codec = KindlingsXmlCodec.derived[XmlPaintContent]
      assertEquals(roundTripped(codec, XmlPaintContent(1, XmlGreen)).toString, """<x id="1">verde</x>""")
    }

    test("withAttributesByDefault: an attribute with withEnumAsStrings, a child element without it") {
      val strings = {
        implicit val config: XmlConfig = XmlConfig().withAttributesByDefault.withEnumAsStrings
        KindlingsXmlCodec.derived[XmlPaint]
      }
      assertEquals(roundTripped(strings, paint).toString, """<x name="a" color="verde" accent="XmlDarkBlue"/>""")
      assertEquals(roundTripped(strings, plain).toString, """<x name="a" color="XmlRed"/>""")
      val elements = {
        implicit val config: XmlConfig = XmlConfig().withAttributesByDefault
        KindlingsXmlCodec.derived[XmlPaint]
      }
      assertEquals(
        roundTripped(elements, paint).toString,
        """<x name="a"><color type="verde"/><accent type="XmlDarkBlue"/></x>"""
      )
    }

    test("a configuration only known at runtime writes and reads the same XML") {
      def static(config: XmlConfig): KindlingsXmlCodec[XmlPaint] = {
        implicit val c: XmlConfig = config
        KindlingsXmlCodec.derived[XmlPaint]
      }
      val default = { implicit val c: XmlConfig = XmlEnumRuntimeConfigs.default; KindlingsXmlCodec.derived[XmlPaint] }
      val strings = { implicit val c: XmlConfig = XmlEnumRuntimeConfigs.strings; KindlingsXmlCodec.derived[XmlPaint] }
      val stringsSnake = {
        implicit val c: XmlConfig = XmlEnumRuntimeConfigs.stringsSnake; KindlingsXmlCodec.derived[XmlPaint]
      }
      val stringsAttributes = {
        implicit val c: XmlConfig = XmlEnumRuntimeConfigs.stringsAttributes; KindlingsXmlCodec.derived[XmlPaint]
      }
      val attributes = {
        implicit val c: XmlConfig = XmlEnumRuntimeConfigs.attributes; KindlingsXmlCodec.derived[XmlPaint]
      }
      val noDiscriminator = {
        implicit val c: XmlConfig = XmlEnumRuntimeConfigs.noDiscriminator; KindlingsXmlCodec.derived[XmlPaint]
      }
      List(paint, plain).foreach { value =>
        assertEquals(roundTripped(default, value), static(XmlConfig()).encode(value, "x"))
        assertEquals(roundTripped(strings, value), static(XmlConfig().withEnumAsStrings).encode(value, "x"))
        assertEquals(
          roundTripped(stringsSnake, value),
          static(XmlConfig().withEnumAsStrings.withSnakeCaseConstructorNames).encode(value, "x")
        )
        assertEquals(
          roundTripped(stringsAttributes, value),
          static(XmlConfig().withEnumAsStrings.withAttributesByDefault).encode(value, "x")
        )
        assertEquals(roundTripped(attributes, value), static(XmlConfig().withAttributesByDefault).encode(value, "x"))
        assertEquals(
          roundTripped(noDiscriminator, value),
          static(XmlConfig().withNoDiscriminator.withKebabCaseConstructorNames).encode(value, "x")
        )
      }
      assertEquals(
        noDiscriminator.encode(paint, "x").toString,
        "<x><name>a</name><color><verde/></color><accent><xml-dark-blue/></accent></x>"
      )
    }

    test("@xmlAttribute / @xmlContent with a configuration only known at runtime") {
      implicit val config: XmlConfig = XmlEnumRuntimeConfigs.stringsSnake
      val attr = KindlingsXmlCodec.derived[XmlPaintAttr]
      assertEquals(
        roundTripped(attr, XmlPaintAttr(XmlGreen, Some(XmlDarkBlue), "n")).toString,
        """<x color="verde" accent="xml_dark_blue"><name>n</name></x>"""
      )
      val content = KindlingsXmlCodec.derived[XmlPaintContent]
      assertEquals(roundTripped(content, XmlPaintContent(1, XmlDarkBlue)).toString, """<x id="1">xml_dark_blue</x>""")
    }
  }

  group("scala.Enumeration") {

    val schedule = XmlSchedule(XmlWeekday.Tue, XmlWeekday.Mon, List(XmlWeekday.Mon, XmlWeekday.Tue))

    test("default: the value name in the discriminator attribute") {
      val codec = KindlingsXmlCodec.derived[XmlSchedule]
      assertEquals(
        roundTripped(codec, schedule).toString,
        """<x first="Mon"><day type="Tue"/><others><item type="Mon"/><item type="Tue"/></others></x>"""
      )
      assertEquals(KindlingsXmlDecoder.decode[XmlWeekday.Value](<d type="Tue"/>), Right(XmlWeekday.Tue))
    }

    test("withEnumAsStrings, known at compile time and only at runtime") {
      val expected = """<x first="Mon"><day>Tue</day><others><item>Mon</item><item>Tue</item></others></x>"""
      val static = {
        implicit val config: XmlConfig = XmlConfig().withEnumAsStrings
        KindlingsXmlCodec.derived[XmlSchedule]
      }
      assertEquals(roundTripped(static, schedule).toString, expected)
      val runtime = {
        implicit val config: XmlConfig = XmlEnumRuntimeConfigs.strings
        KindlingsXmlCodec.derived[XmlSchedule]
      }
      assertEquals(roundTripped(runtime, schedule).toString, expected)
    }
  }

  group("sealed hierarchies") {

    test("@xmlName renames a subtype (and is not transformed by constructorNameMapper)") {
      val codec = KindlingsXmlCodec.derived[XmlRenamedShape]
      assertEquals(
        roundTripped(codec, XmlRenamedCircle(1): XmlRenamedShape).toString,
        """<x type="circ"><r>1</r></x>"""
      )
      val snake = {
        implicit val config: XmlConfig = XmlConfig().withSnakeCaseConstructorNames
        KindlingsXmlCodec.derived[XmlRenamedShape]
      }
      assertEquals(
        roundTripped(snake, XmlRenamedCircle(1): XmlRenamedShape).toString,
        """<x type="circ"><r>1</r></x>"""
      )
      assertEquals(
        roundTripped(snake, XmlRenamedSquare(2): XmlRenamedShape).toString,
        """<x type="xml_renamed_square"><side>2</side></x>"""
      )
      val runtime = {
        implicit val config: XmlConfig = XmlEnumRuntimeConfigs.noDiscriminator
        KindlingsXmlCodec.derived[XmlRenamedShape]
      }
      assertEquals(
        roundTripped(runtime, XmlRenamedCircle(1): XmlRenamedShape).toString,
        "<x><circ><r>1</r></circ></x>"
      )
      assertEquals(
        roundTripped(runtime, XmlRenamedSquare(2): XmlRenamedShape).toString,
        "<x><xml-renamed-square><side>2</side></xml-renamed-square></x>"
      )
    }

    test("withNoDiscriminator: the subtype's element inside the element, also as a field") {
      implicit val config: XmlConfig = XmlConfig().withNoDiscriminator
      val codec = KindlingsXmlCodec.derived[Shape]
      assertEquals(
        roundTripped(codec, Circle(5.0): Shape).toString,
        "<x><Circle><radius>5.0</radius></Circle></x>"
      )
      val shapes = KindlingsXmlCodec.derived[ShapeCollection]
      assertEquals(
        roundTripped(shapes, ShapeCollection(List(Circle(1.0), Rectangle(2.0, 3.0)))).toString,
        "<x><shapes><item><Circle><radius>1.0</radius></Circle></item>" +
          "<item><Rectangle><width>2.0</width><height>3.0</height></Rectangle></item></shapes></x>"
      )
    }

    test("nested sealed traits: the leaves are written under their own names") {
      val codec = KindlingsXmlCodec.derived[XmlFleet]
      assertEquals(
        roundTripped(codec, XmlFleet(XmlTransportCar("b"), List(XmlTransportTank, XmlTransportBike(3)))).toString,
        """<x><lead type="XmlTransportCar"><brand>b</brand></lead>""" +
          """<rest><item type="XmlTransportTank"/><item type="XmlTransportBike"><gears>3</gears></item></rest></x>"""
      )
      val vehicle = KindlingsXmlCodec.derived[XmlVehicle]
      assertEquals(roundTripped(vehicle, XmlCar("b"): XmlVehicle).toString, """<x type="XmlCar"><brand>b</brand></x>""")
      assertEquals(
        roundTripped(vehicle, XmlBicycle(3): XmlVehicle).toString,
        """<x type="XmlBicycle"><gears>3</gears></x>"""
      )
    }

    test("nested sealed traits without a discriminator, with a configuration only known at runtime") {
      implicit val config: XmlConfig = XmlEnumRuntimeConfigs.noDiscriminator
      val codec = KindlingsXmlCodec.derived[XmlFleet]
      assertEquals(
        roundTripped(codec, XmlFleet(XmlTransportCar("b"), List(XmlTransportTank))).toString,
        "<x><lead><xml-transport-car><brand>b</brand></xml-transport-car></lead>" +
          "<rest><item><xml-transport-tank/></item></rest></x>"
      )
    }
  }
}
