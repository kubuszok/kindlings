package hearth.kindlings.ubjsonderivation

import hearth.MacroSuite

/** Behaviours ported from jsoniter-derivation (aligned with jsoniter-scala). Config-dependent scenarios run with a
  * config known at compile time (optimized codegen) and with one only known at runtime (`@volatile var`s).
  */
final class UBJsonJsoniterParitySpec extends MacroSuite {

  private val mixed: List[UbMixed] = List(UbMixed.Origin, UbMixed.Point(1, 2), UbMixed.Renamed("r"))

  group("ADTs") {

    test("wrapper mode: case objects are strings, case classes are wrapped, @named leaves") {
      val runtime = {
        implicit val c: UBJsonConfig = UbJsoniterParityRuntimeConfigs.default
        UBJsonValueCodec.derived[List[UbMixed]]
      }
      List(UBJsonValueCodec.derived[List[UbMixed]], runtime).foreach { codec =>
        UbRender.encode(mixed)(codec) ==> """["Origin",{"Point":{"x":1,"y":2}},{"renamed-leaf":{"v":"r"}}]"""
        UbRender.roundTrip(mixed)(codec) ==> mixed
      }
    }

    test("circeLikeObjectEncoding: case objects are wrapped empty objects") {
      val static = {
        implicit val c: UBJsonConfig = UBJsonConfig().withCirceLikeObjectEncoding
        UBJsonValueCodec.derived[List[UbMixed]]
      }
      val runtime = {
        implicit val c: UBJsonConfig = UbJsoniterParityRuntimeConfigs.circeLike
        UBJsonValueCodec.derived[List[UbMixed]]
      }
      List(static, runtime).foreach { codec =>
        UbRender.encode(mixed)(codec) ==> """[{"Origin":{}},{"Point":{"x":1,"y":2}},{"renamed-leaf":{"v":"r"}}]"""
        UbRender.roundTrip(mixed)(codec) ==> mixed
      }
    }

    test("discriminator: case objects have just the discriminator") {
      val static = {
        implicit val c: UBJsonConfig = UBJsonConfig().withDiscriminator("type")
        UBJsonValueCodec.derived[List[UbMixed]]
      }
      val runtime = {
        implicit val c: UBJsonConfig = UbJsoniterParityRuntimeConfigs.discriminator
        UBJsonValueCodec.derived[List[UbMixed]]
      }
      List(static, runtime).foreach { codec =>
        UbRender.encode(mixed)(codec) ==>
          """[{"type":"Origin"},{"type":"Point","x":1,"y":2},{"type":"renamed-leaf","v":"r"}]"""
        UbRender.roundTrip(mixed)(codec) ==> mixed
      }
    }

    test("adtLeafClassNameMapper receives fully-qualified names") {
      val runtime = {
        implicit val c: UBJsonConfig = UbJsoniterParityRuntimeConfigs.fullNames
        UBJsonValueCodec.derived[List[UbMixed]]
      }
      UbRender.encode(mixed)(runtime) ==>
        """[{"type":"hearth.kindlings.ubjsonderivation.UbMixed.Origin"},""" +
        """{"type":"hearth.kindlings.ubjsonderivation.UbMixed.Point","x":1,"y":2},{"type":"renamed-leaf","v":"r"}]"""
      UbRender.roundTrip(mixed)(runtime) ==> mixed
    }

    test("enumAsStrings: ADTs of case objects are strings also with a discriminator") {
      implicit val c: UBJsonConfig = UBJsonConfig().withDiscriminator("type").withEnumAsStrings
      val codec = UBJsonValueCodec.derived[List[CardinalDirection]]
      UbRender.encode(List[CardinalDirection](North, West))(codec) ==> """["North","West"]"""
    }
  }

  group("requireDiscriminatorFirst") {

    def bytes(fields: (String, UBJsonWriter => Unit)*): Array[Byte] = {
      val w = new UBJsonWriter()
      w.writeObjectStart()
      fields.foreach { case (k, v) => w.writeFieldName(k); v(w) }
      w.writeObjectEnd()
      w.toByteArray
    }
    val pointLast = bytes("x" -> (_.writeInt(1)), "y" -> (_.writeInt(2)), "type" -> (_.writeString("Point")))
    val originLast = bytes("extra" -> (_.writeInt(1)), "type" -> (_.writeString("Origin")))

    test("by default the discriminator must be the first field") {
      implicit val c: UBJsonConfig = UBJsonConfig().withDiscriminator("type")
      val codec = UBJsonValueCodec.derived[UbMixed]
      val error = intercept[UBJsonReaderException](codec.decode(new UBJsonReader(pointLast)))
      assert(error.getMessage.contains("expected discriminator field 'type' as first field"), error.getMessage)
    }

    test("with requireDiscriminatorFirst = false the discriminator may be anywhere") {
      val static = {
        implicit val c: UBJsonConfig = UBJsonConfig().withDiscriminator("type").withRequireDiscriminatorFirst(false)
        UBJsonValueCodec.derived[UbMixed]
      }
      val runtime = {
        implicit val c: UBJsonConfig = UbDiscriminatorRuntimeConfigs.anywhere
        UBJsonValueCodec.derived[UbMixed]
      }
      val strict = {
        implicit val c: UBJsonConfig = UbDiscriminatorRuntimeConfigs.anywhereStrict
        UBJsonValueCodec.derived[UbMixed]
      }
      List(static, runtime, strict).foreach { codec =>
        codec.decode(new UBJsonReader(pointLast)) ==> (UbMixed.Point(1, 2): UbMixed)
      }
      List(static, runtime).foreach { codec =>
        codec.decode(new UBJsonReader(originLast)) ==> (UbMixed.Origin: UbMixed)
      }
      val missing = intercept[UBJsonReaderException](runtime.decode(new UBJsonReader(bytes("x" -> (_.writeInt(1))))))
      assert(missing.getMessage.contains("missing discriminator field 'type'"), missing.getMessage)
    }
  }

  group("fields, tuples and enumerations") {

    test("@named renames a field, tuples are arrays, Enumeration values are strings") {
      val value = UbTuples((1, "a"), List((1, 2)), UbSize.Big)
      val runtime = {
        implicit val c: UBJsonConfig = UbJsoniterParityRuntimeConfigs.discriminator
        UBJsonValueCodec.derived[UbTuples]
      }
      List(UBJsonValueCodec.derived[UbTuples], runtime).foreach { codec =>
        UbRender.encode(value)(codec) ==> """{"t":[1,"a"],"l":[[1,2]],"size":"BIG"}"""
        UbRender.roundTrip(value)(codec) ==> value
      }
      UbRender.encode(UbNamedField(1))(UBJsonValueCodec.derived[UbNamedField]) ==> """{"b":1}"""
    }
  }

  group("config") {

    test("name mappers accept partial functions, names they are not defined at stay unchanged") {
      implicit val c: UBJsonConfig = UBJsonConfig().withFieldNameMapper { case "x" => "X" }
      UbRender.encode(UbMixed.Point(1, 2))(UBJsonValueCodec.derived[UbMixed.Point]) ==> """{"X":1,"y":2}"""
    }

    test("name-mapper helpers as in jsoniter-scala") {
      UBJsonConfig.enforce_snake_case("HTTPServer") ==> "http_server"
      UBJsonConfig.enforce_snake_case("field1") ==> "field_1"
      UBJsonConfig.enforce_snake_case2("field1") ==> "field1"
      UBJsonConfig.`enforce-kebab-case`("fooBar") ==> "foo-bar"
      UBJsonConfig.enforceCamelCase("foo_bar") ==> "fooBar"
      UBJsonConfig.EnforcePascalCase("foo_bar") ==> "FooBar"
      UBJsonConfig.simpleClassName("a.b.C") ==> "C"
    }

    test("bigIntDigitsLimit") {
      val static = {
        implicit val c: UBJsonConfig = UBJsonConfig().withBigIntDigitsLimit(3)
        UBJsonValueCodec.derived[UbBigInt]
      }
      val runtime = {
        implicit val c: UBJsonConfig = UbJsoniterParityRuntimeConfigs.bigIntDigits3
        UBJsonValueCodec.derived[UbBigInt]
      }
      List(static, runtime).foreach { codec =>
        UbRender.roundTrip(UbBigInt(BigInt(12)))(codec) ==> UbBigInt(BigInt(12))
        val error = intercept[UBJsonReaderException](UbRender.roundTrip(UbBigInt(BigInt("123456")))(codec))
        assert(error.getMessage.contains("digits"), error.getMessage)
      }
    }
  }

  group("codegen paths and validation") {

    // A contradictory config known at compile time fails the derivation (the macro evaluated it: optimized codegen),
    // one known only at runtime fails when the codec is created (generic codegen), with jsoniter-scala's messages.
    test("contradictory configs: compile-time error (optimized) or IllegalArgumentException (runtime)") {
      // (the compile-time half is UBJsonCompileTimeValidationSpec, per Scala version: explicit-argument syntax differs)
      val error = intercept[IllegalArgumentException] {
        implicit val c: UBJsonConfig = UbJsoniterParityRuntimeConfigs.contradictory
        UBJsonValueCodec.derived[UbBigInt]
      }
      error.getMessage ==> "'requireDefaultFields' and 'transientDefault' cannot be 'true' simultaneously"
      intercept[IllegalArgumentException](
        UBJsonConfig.validate(UBJsonConfig().withCirceLikeObjectEncoding.withDiscriminator("t"))
      )
    }

    test("duplicated keys fail the derivation") {
      compileErrors(
        "hearth.kindlings.ubjsonderivation.UBJsonValueCodec.derived[hearth.kindlings.ubjsonderivation.UbDuplicatedKeys]"
      )
        .check("Duplicated key(s) defined for")
    }
  }
}
