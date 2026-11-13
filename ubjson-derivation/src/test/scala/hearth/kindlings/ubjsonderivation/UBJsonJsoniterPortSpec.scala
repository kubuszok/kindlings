package hearth.kindlings.ubjsonderivation

import hearth.MacroSuite

/** jsoniter-derivation features ported to UBJson: plain classes and multiple parameter lists, `scalaTransientSupport`,
  * `useScalaEnumValueId`, `transientNull`, nested options (`skipNestedOptionValues`), nested sealed hierarchies.
  * Config-dependent scenarios run with a config evaluated at compile time ([[UbPortOptimizedCodecs]]: optimized
  * codegen) and with one only known at runtime ([[UbPortRuntimeConfigs]]: `@volatile var`s).
  */
final class UBJsonJsoniterPortSpec extends MacroSuite {

  private def bytes(fields: (String, UBJsonWriter => Unit)*): Array[Byte] = {
    val w = new UBJsonWriter()
    w.writeObjectStart()
    fields.foreach { case (k, v) => w.writeFieldName(k); v(w) }
    w.writeObjectEnd()
    w.toByteArray
  }
  private def decode[A](codec: UBJsonValueCodec[A])(fields: (String, UBJsonWriter => Unit)*): A =
    codec.decode(new UBJsonReader(bytes(fields*)))
  private def decodeError[A](codec: UBJsonValueCodec[A], encoded: Array[Byte]): String =
    intercept[UBJsonReaderException](codec.decode(new UBJsonReader(encoded))).getMessage

  group("plain classes and multiple parameter lists") {

    test("plain (non-case) classes are objects of their primary-constructor vals/vars") {
      val value = UbHasPlain(new UbPlainClass(1, "a"), List(new UbPlainClass(2, "b")))
      val codec = UBJsonValueCodec.derived[UbHasPlain]
      UbRender.encode(value)(codec) ==> """{"inner":{"id":1,"name":"a"},"list":[{"id":2,"name":"b"}]}"""
      UbRender.roundTrip(value)(codec) ==> value
      val withDefaults = UBJsonValueCodec.derived[UbPlainWithDefaults]
      decode(withDefaults)("id" -> (_.writeInt(1))) ==> new UbPlainWithDefaults(1)
    }

    test("a constructor parameter without a val/var fails the derivation") {
      compileErrors(
        "hearth.kindlings.ubjsonderivation.UBJsonValueCodec.derived[hearth.kindlings.ubjsonderivation.UbNoAccessor]"
      ).check("'b' parameter of")
    }

    test("plain classes and objects as ADT leaves") {
      val values = List[UbPlainAdt](new UbPlainLeaf(), new UbPlainLeafWithFields(1, "a"), UbPlainObject)
      val runtime = {
        implicit val c: UBJsonConfig = UbPortRuntimeConfigs.default
        UBJsonValueCodec.derived[List[UbPlainAdt]]
      }
      List(UbPortOptimizedCodecs.plainAdt, runtime).foreach { codec =>
        UbRender.encode(values)(codec) ==>
          """[{"UbPlainLeaf":{}},{"UbPlainLeafWithFields":{"x":1,"y":"a"}},"UbPlainObject"]"""
        UbRender.roundTrip(values)(codec) ==> values
      }
      val runtimeDiscriminator = {
        implicit val c: UBJsonConfig = UbPortRuntimeConfigs.discriminator
        UBJsonValueCodec.derived[List[UbPlainAdt]]
      }
      List(UbPortOptimizedCodecs.plainAdtDiscriminator, runtimeDiscriminator).foreach { codec =>
        UbRender.encode(values)(codec) ==>
          """[{"type":"UbPlainLeaf"},{"type":"UbPlainLeafWithFields","x":1,"y":"a"},{"type":"UbPlainObject"}]"""
        UbRender.roundTrip(values)(codec) ==> values
      }
    }

    test("a non-abstract sealed base is one of its leaves") {
      val values = List[UbSealedBase](new UbSealedBase, UbSealedB(1), UbSealedC("c"))
      val runtime = {
        implicit val c: UBJsonConfig = UbPortRuntimeConfigs.discriminator
        UBJsonValueCodec.derived[List[UbSealedBase]]
      }
      List(UbPortOptimizedCodecs.sealedBase, runtime).foreach { codec =>
        UbRender.encode(values)(
          codec
        ) ==> """[{"type":"UbSealedBase"},{"type":"UbSealedB","n":1},{"type":"UbSealedC","s":"c"}]"""
        val decoded = UbRender.roundTrip(values)(codec)
        decoded.map(_.getClass) ==> values.map(_.getClass)
        decoded.tail ==> values.tail
      }
    }

    test("all parameter lists are encoded") {
      val codec = UBJsonValueCodec.derived[UbMultiList]
      val value = UbMultiList(1)(2L)("s")
      UbRender.encode(value)(codec) ==> """{"i":1,"l":2,"s":"s"}"""
      UbRender.roundTrip(value)(codec).toString ==> value.toString
    }

    test("defaults of non-first parameter lists may depend on the preceding parameters") {
      val runtime = {
        implicit val c: UBJsonConfig = UbPortRuntimeConfigs.transientDefault
        UBJsonValueCodec.derived[UbMultiListDependent]
      }
      List(UbPortOptimizedCodecs.multiList, runtime).foreach { codec =>
        decode(codec)().toString ==> "UbMultiListDependent(1)(0)(1-0)"
        decode(codec)("i" -> (_.writeInt(5))).toString ==> "UbMultiListDependent(5)(4)(5-4)"
        UbRender.encode(UbMultiListDependent()()())(codec) ==> "{}"
        UbRender.encode(UbMultiListDependent(2)()())(codec) ==> """{"i":2}"""
        UbRender.encode(UbMultiListDependent(2)(7L)())(codec) ==> """{"i":2,"l":7}"""
        UbRender.encode(UbMultiListDependent(2)(7L)("x"))(codec) ==> """{"i":2,"l":7,"s":"x"}"""
      }
      val generic = UBJsonValueCodec.derived[UbGenericMultiList[Int]]
      UbRender.encode(UbGenericMultiList(3)()()())(generic) ==> """{"a":3,"as":[3,3],"m":4}"""
      decode(generic)("a" -> (_.writeInt(3))).toString ==> "UbGenericMultiList(3)(2)(List(3, 3))(4)"
    }

    test("ADT leaves with multiple parameter lists") {
      val runtime = {
        implicit val c: UBJsonConfig = UbPortRuntimeConfigs.discriminator
        UBJsonValueCodec.derived[List[UbMultiListAdt]]
      }
      List(UbPortOptimizedCodecs.multiListAdt, runtime).foreach { codec =>
        val values = List[UbMultiListAdt](UbMultiData1(1)(2), UbMultiData2(3)(4))
        UbRender.encode(values)(
          codec
        ) ==> """[{"type":"UbMultiData1","i":1,"pos":2},{"type":"UbMultiData2","i":3,"pos":4}]"""
        UbRender.roundTrip(values)(codec).map {
          case d: UbMultiData1 => (d.i, d.pos)
          case d: UbMultiData2 => (d.i, d.pos)
        } ==> List((1, 2), (3, 4))
      }
    }
  }

  group("fields with default values") {

    test("null (reference types) and an empty collection decode as the default value") {
      val runtime = {
        implicit val c: UBJsonConfig = UbPortRuntimeConfigs.default
        UBJsonValueCodec.derived[UbFieldDefaults]
      }
      List(UBJsonValueCodec.derived[UbFieldDefaults], runtime).foreach { codec =>
        decode(codec)(
          "l" -> { w => w.writeArrayStart(); w.writeArrayEnd() },
          "o" -> (_.writeNull()),
          "s" -> (_.writeNull())
        ) ==> UbFieldDefaults()
        decode(codec)("l" -> (_.writeNull()), "n" -> (_.writeInt(3))) ==> UbFieldDefaults(n = 3)
        UbRender
          .roundTrip(UbFieldDefaults(List(1), Some(5), "x", 4))(codec) ==> UbFieldDefaults(List(1), Some(5), "x", 4)
        // as in jsoniter-scala: `None` is written as null, which decodes as the default
        UbRender.roundTrip(UbFieldDefaults(o = None))(codec) ==> UbFieldDefaults()
        assert(decodeError(codec, bytes("n" -> (_.writeNull()))).nonEmpty)
      }
    }
  }

  group("scalaTransientSupport") {

    test("scala.transient fields are transient with scalaTransientSupport") {
      val runtime = {
        implicit val c: UBJsonConfig = UbPortRuntimeConfigs.scalaTransient
        UBJsonValueCodec.derived[UbScalaTransient]
      }
      List(UbPortOptimizedCodecs.scalaTransient, runtime).foreach { codec =>
        UbRender.encode(UbScalaTransient("VVV", "non-default"))(codec) ==> """{"required":"VVV"}"""
        decode(codec)("transient" -> (_.writeString("XXX")), "required" -> (_.writeString("VVV"))) ==>
          UbScalaTransient("VVV")
      }
    }

    test("without it scala.transient fields are regular fields") {
      val runtime = {
        implicit val c: UBJsonConfig = UbPortRuntimeConfigs.default
        UBJsonValueCodec.derived[UbScalaTransient]
      }
      List(UBJsonValueCodec.derived[UbScalaTransient], runtime).foreach { codec =>
        UbRender.encode(UbScalaTransient("VVV"))(codec) ==> """{"required":"VVV","transient":"default"}"""
        decode(codec)("transient" -> (_.writeString("XXX")), "required" -> (_.writeString("VVV"))) ==>
          UbScalaTransient("VVV", "XXX")
      }
    }
  }

  group("useScalaEnumValueId") {

    test("Enumeration values are their ids with useScalaEnumValueId, names otherwise") {
      val value = UbEnums(UbLocation.GPS, List(UbLocation.IP, UbLocation.Named))
      val runtime = {
        implicit val c: UBJsonConfig = UbPortRuntimeConfigs.enumIds
        UBJsonValueCodec.derived[UbEnums]
      }
      List(UbPortOptimizedCodecs.enumIds, runtime).foreach { codec =>
        UbRender.encode(value)(codec) ==> """{"lt":1,"l":[0,2]}"""
        UbRender.roundTrip(value)(codec) ==> value
        assert(
          decodeError(codec, bytes("lt" -> (_.writeInt(7)), "l" -> { w => w.writeArrayStart(); w.writeArrayEnd() }))
            .contains("illegal enum value")
        )
      }
      val runtimeDefault = {
        implicit val c: UBJsonConfig = UbPortRuntimeConfigs.default
        UBJsonValueCodec.derived[UbEnums]
      }
      List(UBJsonValueCodec.derived[UbEnums], runtimeDefault).foreach { codec =>
        UbRender.encode(value)(codec) ==> """{"lt":"GPS","l":["IP","named-value"]}"""
        UbRender.roundTrip(value)(codec) ==> value
      }
    }
  }

  group("transientNull") {

    test("transientNull does not affect fields which are not nullable unions") {
      val runtime = {
        implicit val c: UBJsonConfig = UbPortRuntimeConfigs.transientNull
        UBJsonValueCodec.derived[UbNotNullable]
      }
      List(UbPortOptimizedCodecs.transientNull, runtime).foreach { codec =>
        UbRender.encode(UbNotNullable("a", 1))(codec) ==> """{"s":"a","n":1}"""
      }
    }
  }

  group("nested options") {

    test("skipNestedOptionValues: Some(None) is null, None is omitted, and both read back") {
      val runtime = {
        implicit val c: UBJsonConfig = UbPortRuntimeConfigs.skipNestedOptions
        UBJsonValueCodec.derived[List[UbNestedOptions]]
      }
      val value = List(
        UbNestedOptions(Some("VVV"), Some(Some("WWW"))),
        UbNestedOptions(None),
        UbNestedOptions(None, Some(None))
      )
      List(UbPortOptimizedCodecs.skipNestedOptions, runtime).foreach { codec =>
        UbRender.encode(value)(codec) ==> """[{"f1":"VVV","f2":"WWW"},{},{"f2":null}]"""
        UbRender.roundTrip(value)(codec) ==> value
      }
    }

    val none = UbNestedOptions3(None, None)
    val someSomeSome = UbNestedOptions3(None, Some(Some(Some("VVV"))))
    val someNone = UbNestedOptions3(None, Some(None))
    val someSomeNone = UbNestedOptions3(None, Some(Some(None)))

    test("inner options are encoded as the ADT Option, with a discriminator") {
      val runtime = {
        implicit val c: UBJsonConfig = UbPortRuntimeConfigs.discriminatorTransientNone
        UBJsonValueCodec.derived[UbNestedOptions3]
      }
      List(UbPortOptimizedCodecs.nestedOptions3Discriminator, runtime).foreach { codec =>
        UbRender.encode(UbNestedOptions3(Some("VVV"), None))(codec) ==> """{"y":"VVV"}"""
        UbRender.encode(someSomeSome)(codec) ==> """{"x":{"type":"Some","value":{"type":"Some","value":"VVV"}}}"""
        UbRender.encode(someNone)(codec) ==> """{"x":{"type":"None"}}"""
        UbRender.encode(someSomeNone)(codec) ==> """{"x":{"type":"Some","value":{"type":"None"}}}"""
        List(none, someSomeSome, someNone, someSomeNone).foreach(v => UbRender.roundTrip(v)(codec) ==> v)
        val missing = bytes("x" -> { w =>
          w.writeObjectStart(); w.writeFieldName("type"); w.writeString("Some"); w.writeObjectEnd()
        })
        assert(decodeError(codec, missing).contains("missing required field \"value\""))
        val unknown = bytes("x" -> { w =>
          w.writeObjectStart(); w.writeFieldName("type"); w.writeString("X"); w.writeObjectEnd()
        })
        assert(decodeError(codec, unknown).contains("illegal value of discriminator field \"type\""))
      }
    }

    test("inner options are encoded as the ADT Option, wrapper-style") {
      val runtime = {
        implicit val c: UBJsonConfig = UbPortRuntimeConfigs.default
        UBJsonValueCodec.derived[UbNestedOptions3]
      }
      List(UbPortOptimizedCodecs.nestedOptions3, runtime).foreach { codec =>
        UbRender.encode(someSomeSome)(codec) ==> """{"y":null,"x":{"Some":{"value":{"Some":{"value":"VVV"}}}}}"""
        UbRender.encode(someNone)(codec) ==> """{"y":null,"x":"None"}"""
        UbRender.encode(someSomeNone)(codec) ==> """{"y":null,"x":{"Some":{"value":"None"}}}"""
        List(none, someSomeSome, someNone, someSomeNone).foreach(v => UbRender.roundTrip(v)(codec) ==> v)
      }
      val circeRuntime = {
        implicit val c: UBJsonConfig = UbPortRuntimeConfigs.circeLike
        UBJsonValueCodec.derived[UbNestedOptions3]
      }
      List(UbPortOptimizedCodecs.nestedOptions3CirceLike, circeRuntime).foreach { codec =>
        UbRender.encode(someNone)(codec) ==> """{"y":null,"x":{"None":{}}}"""
        List(none, someSomeSome, someNone, someSomeNone).foreach(v => UbRender.roundTrip(v)(codec) ==> v)
      }
    }
  }

  group("nested sealed hierarchies") {

    val vehicles = List[UbVehicle](UbTruck(1), UbMotorcycle(2), UbScooter, UbBicycle(3))

    test("are flattened to their leaves (wrapper mode)") {
      val runtime = {
        implicit val c: UBJsonConfig = UbPortRuntimeConfigs.default
        UBJsonValueCodec.derived[List[UbVehicle]]
      }
      List(UbPortOptimizedCodecs.vehicles, runtime).foreach { codec =>
        UbRender.encode(vehicles)(codec) ==>
          """[{"UbTruck":{"payload":1}},{"UbMotorcycle":{"cc":2}},"UbScooter",{"UbBicycle":{"gears":3}}]"""
        UbRender.roundTrip(vehicles)(codec) ==> vehicles
      }
    }

    test("are flattened to their leaves (discriminator mode)") {
      val runtime = {
        implicit val c: UBJsonConfig = UbPortRuntimeConfigs.discriminator
        UBJsonValueCodec.derived[List[UbVehicle]]
      }
      val anywhere = {
        implicit val c: UBJsonConfig = UbPortRuntimeConfigs.discriminatorAnywhere
        UBJsonValueCodec.derived[List[UbVehicle]]
      }
      List(UbPortOptimizedCodecs.vehiclesDiscriminator, runtime, anywhere).foreach { codec =>
        UbRender.encode(vehicles)(codec) ==>
          """[{"type":"UbTruck","payload":1},{"type":"UbMotorcycle","cc":2},{"type":"UbScooter"},{"type":"UbBicycle","gears":3}]"""
        UbRender.roundTrip(vehicles)(codec) ==> vehicles
      }
      val motor = UBJsonValueCodec.derived[List[UbMotorVehicle]]
      UbRender.roundTrip(List[UbMotorVehicle](UbTruck(1), UbScooter))(motor) ==> List[UbMotorVehicle](
        UbTruck(1),
        UbScooter
      )
    }
  }
}
