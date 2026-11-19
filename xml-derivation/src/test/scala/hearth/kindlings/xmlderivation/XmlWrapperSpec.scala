package hearth.kindlings.xmlderivation

import hearth.MacroSuite
import hearth.kindlings.xmlderivation.annotations.{xmlAttribute, xmlElement, xmlName, xmlUnwrapped, xmlWrapper}

case class XmlWrapItem(@xmlAttribute id: Int, name: String)
case class XmlWrapOrder(title: String, @xmlWrapper("items") item: List[XmlWrapItem])
case class XmlWrapMany(
    @xmlWrapper("tags") tag: List[String],
    @xmlWrapper("scores") @xmlName("score") values: Vector[Int],
    @xmlWrapper("props") prop: Map[String, Int],
    @xmlWrapper("meta") note: Option[String],
    @xmlWrapper("owner") person: SimplePerson,
    @xmlWrapper("shapes") shape: List[Shape]
)
case class XmlWrapDefaults(
    @xmlWrapper("tags") tag: List[String] = List("d"),
    @xmlWrapper("meta") note: Option[String] = Some("n"),
    @xmlWrapper("n") count: Int = 3
)
case class XmlWrapRequired(@xmlWrapper("n") count: Int)
case class XmlWrapElement(@xmlElement @xmlWrapper("labels") label: List[String], other: Int)
case class XmlWrapCamel(@xmlWrapper("allItems") lineItem: List[Int])
case class XmlUnwrapOrder(title: String, @xmlUnwrapped item: List[XmlWrapItem], @xmlUnwrapped prop: Map[String, Int])
case class XmlUnwrapDefaults(@xmlUnwrapped tag: List[String] = List("d"), last: Int = 1)
case class XmlUnwrapNested(@xmlUnwrapped row: List[XmlUnwrapOrder])

// configs only known at runtime (the macro cannot evaluate a `var`)
object XmlWrapperRuntimeConfigs {
  @volatile var default: XmlConfig = XmlConfig()
  @volatile var attributes: XmlConfig = XmlConfig().withAttributesByDefault
  @volatile var transient: XmlConfig = XmlConfig().withTransientEmpty.withTransientNone
  @volatile var defaults: XmlConfig = XmlConfig().withUseDefaults
  @volatile var snake: XmlConfig = XmlConfig().withSnakeCaseFieldNames
}

/** `@xmlWrapper` (the field inside an outer element) and `@xmlUnwrapped` (one element per item, no outer element). */
final class XmlWrapperSpec extends MacroSuite {

  // in memory, and through the XML text (which is what a user reads back)
  private def roundTripped[A](codec: KindlingsXmlCodec[A], value: A): scala.xml.Elem = {
    val elem = codec.encode(value, "x")
    assertEquals(codec.decode(elem), Right(value))
    assertEquals(codec.decode(scala.xml.XML.loadString(elem.toString)), Right(value))
    elem
  }
  private def roundTrips[A](codec: KindlingsXmlCodec[A], value: A): Unit = {
    val _ = roundTripped(codec, value)
  }

  private val order = XmlWrapOrder("t", List(XmlWrapItem(1, "a"), XmlWrapItem(2, "b")))
  private val many = XmlWrapMany(
    List("x", "y"),
    Vector(1, 2),
    Map("k" -> 1),
    Some("z"),
    SimplePerson("p", 3),
    List(Circle(1.5), Rectangle(2.5, 3.5))
  )
  private val manyEmpty = XmlWrapMany(Nil, Vector.empty, Map.empty, None, SimplePerson("p", 3), Nil)

  group("@xmlWrapper with a configuration known at compile time") {

    test("a collection: one element per item, named after the field, inside the wrapper element") {
      val codec = KindlingsXmlCodec.derived[XmlWrapOrder]
      assertEquals(
        roundTripped(codec, order).toString,
        """<x><title>t</title><items><item id="1"><name>a</name></item><item id="2"><name>b</name></item></items></x>"""
      )
    }

    test("the inline methods") {
      val elem = KindlingsXmlEncoder.encode(order, "x")
      assertEquals(elem, KindlingsXmlCodec.derived[XmlWrapOrder].encode(order, "x"))
      assertEquals(KindlingsXmlDecoder.decode[XmlWrapOrder](elem), Right(order))
      assertEquals(
        KindlingsXmlDecoder.fromXmlString[XmlWrapOrder](KindlingsXmlEncoder.toXmlString(order, "x")),
        Right(order)
      )
    }

    test("collections, @xmlName, a map, an Option, a case class and a sealed trait") {
      val codec = KindlingsXmlCodec.derived[XmlWrapMany]
      assertEquals(
        roundTripped(codec, many).toString,
        "<x><tags><tag>x</tag><tag>y</tag></tags><scores><score>1</score><score>2</score></scores>" +
          """<props><prop key="k"><value>1</value></prop></props><meta><note>z</note></meta>""" +
          "<owner><person><name>p</name><age>3</age></person></owner>" +
          """<shapes><shape type="Circle"><radius>1.5</radius></shape>""" +
          """<shape type="Rectangle"><width>2.5</width><height>3.5</height></shape></shapes></x>"""
      )
    }

    test("an empty collection is an empty wrapper, None is an empty element inside the wrapper") {
      val codec = KindlingsXmlCodec.derived[XmlWrapMany]
      assertEquals(
        roundTripped(codec, manyEmpty).toString,
        "<x><tags/><scores/><props/><meta><note/></meta>" +
          "<owner><person><name>p</name><age>3</age></person></owner><shapes/></x>"
      )
    }

    test("an absent wrapper is an empty collection / None; an absent required field is an error") {
      val codec = KindlingsXmlCodec.derived[XmlWrapMany]
      assertEquals(
        codec.decode(<x><owner><person><name>p</name><age>3</age></person></owner></x>),
        Right(manyEmpty)
      )
      assertEquals(codec.decode(<x/>), Left(XmlDecodingError.MissingElement("person", "owner")))
      assertEquals(
        KindlingsXmlDecoder.decode[XmlWrapRequired](<x><count>1</count></x>),
        Left(XmlDecodingError.MissingElement("count", "n"))
      )
    }

    test("only the elements named after the field are items; an invalid item is an error") {
      val codec = KindlingsXmlCodec.derived[XmlWrapCamel]
      assertEquals(
        codec.decode(<x><allItems><lineItem>1</lineItem><other>9</other><lineItem>2</lineItem></allItems></x>),
        Right(XmlWrapCamel(List(1, 2)))
      )
      assertEquals(
        codec.decode(<x><allItems><lineItem>1</lineItem><lineItem>z</lineItem></allItems></x>),
        Left(XmlDecodingError.Multiple(List(XmlDecodingError.InvalidValue("z", "Int", "lineItem"))))
      )
    }

    test("withTransientEmpty / withTransientNone skip the wrapper") {
      implicit val config: XmlConfig = XmlConfig().withTransientEmpty.withTransientNone
      val codec = KindlingsXmlCodec.derived[XmlWrapMany]
      assertEquals(
        roundTripped(codec, manyEmpty).toString,
        "<x><owner><person><name>p</name><age>3</age></person></owner></x>"
      )
      roundTrips(codec, many)
    }

    test("withAttributesByDefault: the wrapped field stays an element, the items use attributes") {
      implicit val config: XmlConfig = XmlConfig().withAttributesByDefault
      val codec = KindlingsXmlCodec.derived[XmlWrapOrder]
      assertEquals(
        roundTripped(codec, order).toString,
        """<x title="t"><items><item id="1" name="a"/><item id="2" name="b"/></items></x>"""
      )
      val defaults = KindlingsXmlCodec.derived[XmlWrapDefaults]
      assertEquals(
        roundTripped(defaults, XmlWrapDefaults()).toString,
        "<x><tags><tag>d</tag></tags><meta><note>n</note></meta><n><count>3</count></n></x>"
      )
    }

    test("withUseDefaults: an absent wrapper is the default value, a present one is what it holds") {
      implicit val config: XmlConfig = XmlConfig().withUseDefaults
      val codec = KindlingsXmlCodec.derived[XmlWrapDefaults]
      assertEquals(codec.decode(<x/>), Right(XmlWrapDefaults()))
      // an empty collection is written as an empty wrapper, so it is not replaced by the default
      roundTrips(codec, XmlWrapDefaults(Nil, None, 1))
      assertEquals(codec.decode(<x><tags/></x>), Right(XmlWrapDefaults(Nil)))
      roundTrips(codec, XmlWrapDefaults(List("a"), Some("b"), 2))
    }

    test("without withUseDefaults the defaults are not used") {
      val codec = KindlingsXmlCodec.derived[XmlWrapDefaults]
      assertEquals(codec.decode(<x><n><count>1</count></n></x>), Right(XmlWrapDefaults(Nil, None, 1)))
      assertEquals(codec.decode(<x/>), Left(XmlDecodingError.MissingElement("count", "n")))
    }

    test("field names are transformed, wrapper names are used as they are") {
      implicit val config: XmlConfig = XmlConfig().withSnakeCaseFieldNames
      val codec = KindlingsXmlCodec.derived[XmlWrapCamel]
      assertEquals(
        roundTripped(codec, XmlWrapCamel(List(1, 2))).toString,
        "<x><allItems><line_item>1</line_item><line_item>2</line_item></allItems></x>"
      )
    }

    test("@xmlElement together with @xmlWrapper") {
      val codec = KindlingsXmlCodec.derived[XmlWrapElement]
      assertEquals(
        roundTripped(codec, XmlWrapElement(List("a"), 1)).toString,
        "<x><labels><label>a</label></labels><other>1</other></x>"
      )
    }
  }

  group("@xmlWrapper with a configuration only known at runtime") {

    test("writes and reads the same XML as with the configuration known at compile time") {
      val static = { implicit val c: XmlConfig = XmlConfig(); KindlingsXmlCodec.derived[XmlWrapMany] }
      val codec = {
        implicit val config: XmlConfig = XmlWrapperRuntimeConfigs.default
        KindlingsXmlCodec.derived[XmlWrapMany]
      }
      assertEquals(roundTripped(codec, many), static.encode(many, "x"))
      assertEquals(roundTripped(codec, manyEmpty), static.encode(manyEmpty, "x"))
    }

    test("withAttributesByDefault") {
      implicit val config: XmlConfig = XmlWrapperRuntimeConfigs.attributes
      val codec = KindlingsXmlCodec.derived[XmlWrapOrder]
      assertEquals(
        roundTripped(codec, order).toString,
        """<x title="t"><items><item id="1" name="a"/><item id="2" name="b"/></items></x>"""
      )
    }

    test("withTransientEmpty / withTransientNone") {
      implicit val config: XmlConfig = XmlWrapperRuntimeConfigs.transient
      val codec = KindlingsXmlCodec.derived[XmlWrapMany]
      assertEquals(
        roundTripped(codec, manyEmpty).toString,
        "<x><owner><person><name>p</name><age>3</age></person></owner></x>"
      )
      roundTrips(codec, many)
    }

    test("withUseDefaults, and without it") {
      val withDefaults = {
        implicit val config: XmlConfig = XmlWrapperRuntimeConfigs.defaults
        KindlingsXmlCodec.derived[XmlWrapDefaults]
      }
      assertEquals(withDefaults.decode(<x/>), Right(XmlWrapDefaults()))
      roundTrips(withDefaults, XmlWrapDefaults(Nil, None, 1))
      val without = {
        implicit val config: XmlConfig = XmlWrapperRuntimeConfigs.default
        KindlingsXmlCodec.derived[XmlWrapDefaults]
      }
      assertEquals(without.decode(<x><n><count>1</count></n></x>), Right(XmlWrapDefaults(Nil, None, 1)))
      assertEquals(without.decode(<x/>), Left(XmlDecodingError.MissingElement("count", "n")))
    }

    test("field names are transformed") {
      implicit val config: XmlConfig = XmlWrapperRuntimeConfigs.snake
      val codec = KindlingsXmlCodec.derived[XmlWrapCamel]
      assertEquals(
        roundTripped(codec, XmlWrapCamel(List(1, 2))).toString,
        "<x><allItems><line_item>1</line_item><line_item>2</line_item></allItems></x>"
      )
    }
  }

  group("@xmlUnwrapped") {

    val unwrapped = XmlUnwrapOrder("t", List(XmlWrapItem(1, "a"), XmlWrapItem(2, "b")), Map("k" -> 1))

    test("one element per item (per map entry), named after the field, directly in the element") {
      val codec = KindlingsXmlCodec.derived[XmlUnwrapOrder]
      assertEquals(
        roundTripped(codec, unwrapped).toString,
        """<x><title>t</title><item id="1"><name>a</name></item><item id="2"><name>b</name></item>""" +
          """<prop key="k"><value>1</value></prop></x>"""
      )
      assertEquals(roundTripped(codec, XmlUnwrapOrder("t", Nil, Map.empty)).toString, "<x><title>t</title></x>")
    }

    test("items between other elements are read in document order") {
      val codec = KindlingsXmlCodec.derived[XmlUnwrapOrder]
      assertEquals(
        codec.decode(
          <x><item id="1"><name>a</name></item><title>t</title><item id="2"><name>b</name></item></x>
        ),
        Right(unwrapped.copy(prop = Map.empty))
      )
    }

    test("nested, with a configuration only known at runtime") {
      implicit val config: XmlConfig = XmlWrapperRuntimeConfigs.attributes
      val codec = KindlingsXmlCodec.derived[XmlUnwrapNested]
      assertEquals(
        roundTripped(codec, XmlUnwrapNested(List(unwrapped, unwrapped.copy(title = "u")))).toString,
        """<x><row title="t"><item id="1" name="a"/><item id="2" name="b"/><prop key="k"><value>1</value></prop></row>""" +
          """<row title="u"><item id="1" name="a"/><item id="2" name="b"/><prop key="k"><value>1</value></prop></row></x>"""
      )
    }

    test("withUseDefaults: no item at all is the default value") {
      implicit val config: XmlConfig = XmlConfig().withUseDefaults
      val codec = KindlingsXmlCodec.derived[XmlUnwrapDefaults]
      assertEquals(codec.decode(<x/>), Right(XmlUnwrapDefaults()))
      roundTrips(codec, XmlUnwrapDefaults(List("a", "b"), 2))
      val runtime = {
        implicit val config: XmlConfig = XmlWrapperRuntimeConfigs.defaults
        KindlingsXmlCodec.derived[XmlUnwrapDefaults]
      }
      assertEquals(runtime.decode(<x/>), Right(XmlUnwrapDefaults()))
      roundTrips(runtime, XmlUnwrapDefaults(List("a", "b"), 2))
    }

    test("without withUseDefaults: no item is an empty collection") {
      val codec = KindlingsXmlCodec.derived[XmlUnwrapDefaults]
      assertEquals(codec.decode(<x><last>2</last></x>), Right(XmlUnwrapDefaults(Nil, 2)))
      val runtime = {
        implicit val config: XmlConfig = XmlWrapperRuntimeConfigs.default
        KindlingsXmlCodec.derived[XmlUnwrapDefaults]
      }
      assertEquals(runtime.decode(<x><last>2</last></x>), Right(XmlUnwrapDefaults(Nil, 2)))
    }
  }

  group("invalid annotations are compile-time errors") {

    test("@xmlUnwrapped on a field that is not a collection") {
      compileErrors(
        "hearth.kindlings.xmlderivation.KindlingsXmlEncoder.derived[hearth.kindlings.xmlderivation.XmlBadUnwrappedString]"
      )
        .check("Field 'name' of", "@xmlUnwrapped needs a collection or a map")
      compileErrors(
        "hearth.kindlings.xmlderivation.KindlingsXmlDecoder.derived[hearth.kindlings.xmlderivation.XmlBadUnwrappedOption]"
      )
        .check("@xmlUnwrapped needs a collection or a map")
    }

    test("@xmlWrapper together with @xmlAttribute / @xmlUnwrapped, and a wrapper name used twice") {
      compileErrors(
        "hearth.kindlings.xmlderivation.KindlingsXmlEncoder.derived[hearth.kindlings.xmlderivation.XmlBadWrappedAttribute]"
      )
        .check("@xmlAttribute and @xmlWrapper cannot be combined")
      compileErrors(
        "hearth.kindlings.xmlderivation.KindlingsXmlDecoder.derived[hearth.kindlings.xmlderivation.XmlBadWrappedUnwrapped]"
      )
        .check("@xmlUnwrapped and @xmlWrapper cannot be combined")
      compileErrors(
        "hearth.kindlings.xmlderivation.KindlingsXmlCodec.derived[hearth.kindlings.xmlderivation.XmlBadWrapperTwice]"
      )
        .check("more than one field with @xmlWrapper(\"w\")")
    }

    test("a map whose keys are not Strings") {
      compileErrors(
        "hearth.kindlings.xmlderivation.KindlingsXmlEncoder.derived[hearth.kindlings.xmlderivation.XmlBadWrappedMap]"
      )
        .check("a map with scala.Int keys is not supported")
    }
  }
}
