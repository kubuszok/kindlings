package hearth.kindlings.jsoniterderivation

import com.github.plokhotnyuk.jsoniter_scala.core.{readFromString, writeToString, JsonReaderException, JsonValueCodec}
import hearth.MacroSuite

/** Behaviours aligned with jsoniter-scala's `JsonCodecMaker.make` (v2.42.3). The `Spec ~N` references point at
  * upstream's `JsonCodecMakerSpec.scala`.
  *
  * Config-dependent scenarios run twice: on the optimized codegen (`JsoniterConfig.default` or `OptimizedCodecs`) and on
  * the generic one (configs from `ParityRuntimeConfigs`, unknown at compile time). See `AbsentFieldsSpec` "codegen
  * paths" for the proof of which path each kind of codec takes.
  */
final class JsoniterScalaParitySpec extends MacroSuite {

  private def decodeError(fragment: String)(thunk: => Any): Unit = {
    val error = intercept[JsonReaderException](thunk)
    assert(error.getMessage.contains(fragment), s"expected an error containing '$fragment', got: ${error.getMessage}")
  }

  private def runtime[A](config: JsoniterConfig)(derive: JsoniterConfig => A): A = derive(config)

  group("jsoniter-scala parity") {

    group("setMaxInsertNumber limits only sets (Spec ~1952)") {
      val oneTooMany = (1 to 11).mkString("[", ",", "]")
      val big = (1 to 1025).mkString("[", ",", "]")

      test("lists are not limited") {
        val listRuntime = runtime(ParityRuntimeConfigs.setLimit10) { implicit c => KindlingsJsonValueCodec.derived[List[Int]] }
        List[JsonValueCodec[List[Int]]](OptimizedCodecs.setLimit10List, listRuntime).foreach { c =>
          readFromString[List[Int]](oneTooMany)(c) ==> (1 to 11).toList
        }
        val defaultsRuntime =
          runtime(ParityRuntimeConfigs.jsoniterScalaDefaults) { implicit c => KindlingsJsonValueCodec.derived[List[Int]] }
        List[JsonValueCodec[List[Int]]](OptimizedCodecs.jsoniterScalaDefaultsList, defaultsRuntime).foreach { c =>
          readFromString[List[Int]](big)(c).size ==> 1025
        }
      }

      test("sets are limited") {
        val setRuntime = runtime(ParityRuntimeConfigs.setLimit10) { implicit c => KindlingsJsonValueCodec.derived[Set[Int]] }
        List[JsonValueCodec[Set[Int]]](OptimizedCodecs.setLimit10Set, setRuntime).foreach { c =>
          readFromString[Set[Int]]((1 to 10).mkString("[", ",", "]"))(c).size ==> 10
          decodeError("too many set inserts")(readFromString[Set[Int]](oneTooMany)(c))
        }
        val defaultsRuntime =
          runtime(ParityRuntimeConfigs.jsoniterScalaDefaults) { implicit c => KindlingsJsonValueCodec.derived[Set[Int]] }
        List[JsonValueCodec[Set[Int]]](OptimizedCodecs.jsoniterScalaDefaultsSet, defaultsRuntime).foreach { c =>
          decodeError("too many set inserts")(readFromString[Set[Int]](big)(c))
        }
      }
    }

    group("objects without fields (Spec ~2621, ~2742)") {
      val withUnknown = """{"x":1,"y":[1,2],"z":{"a":3}}"""

      test("zero-field case class skips unknown fields by default") {
        val cRuntime = runtime(ParityRuntimeConfigs.default) { implicit c => KindlingsJsonValueCodec.derived[ParityZeroFields] }
        List[JsonValueCodec[ParityZeroFields]](KindlingsJsonValueCodec.derived[ParityZeroFields], cRuntime).foreach { c =>
          readFromString[ParityZeroFields](withUnknown)(c) ==> ParityZeroFields()
          readFromString[ParityZeroFields]("{}")(c) ==> ParityZeroFields()
        }
      }

      test("all-@transientField case class skips unknown fields by default") {
        val cRuntime =
          runtime(ParityRuntimeConfigs.default) { implicit c => KindlingsJsonValueCodec.derived[ParityAllTransient] }
        List[JsonValueCodec[ParityAllTransient]](KindlingsJsonValueCodec.derived[ParityAllTransient], cRuntime).foreach {
          c => readFromString[ParityAllTransient](withUnknown)(c) ==> ParityAllTransient()
        }
      }

      test("zero-field case classes reject unknown fields when skipUnexpectedFields = false") {
        val zeroRuntime =
          runtime(ParityRuntimeConfigs.strict) { implicit c => KindlingsJsonValueCodec.derived[ParityZeroFields] }
        List[JsonValueCodec[ParityZeroFields]](OptimizedCodecs.strictZero, zeroRuntime).foreach { c =>
          decodeError("unexpected field")(readFromString[ParityZeroFields](withUnknown)(c))
          readFromString[ParityZeroFields]("{}")(c) ==> ParityZeroFields()
        }
        val transientRuntime =
          runtime(ParityRuntimeConfigs.strict) { implicit c => KindlingsJsonValueCodec.derived[ParityAllTransient] }
        List[JsonValueCodec[ParityAllTransient]](OptimizedCodecs.strictAllTransient, transientRuntime).foreach { c =>
          decodeError("unexpected field")(readFromString[ParityAllTransient](withUnknown)(c))
        }
      }

      test("zero-field ADT leaf with a discriminator skips unknown fields by default") {
        val cRuntime =
          runtime(ParityRuntimeConfigs.discriminator) { implicit c => KindlingsJsonValueCodec.derived[ParityZeroAdt] }
        List[JsonValueCodec[ParityZeroAdt]](OptimizedCodecs.discriminatorZeroAdt, cRuntime).foreach { c =>
          readFromString[ParityZeroAdt]("""{"type":"ParityZeroLeaf","extra":"ignored"}""")(c) ==> ParityZeroLeaf()
          readFromString[ParityZeroAdt]("""{"type":"ParityZeroLeaf"}""")(c) ==> ParityZeroLeaf()
        }
      }

      test("zero-field ADT leaf with a discriminator rejects unknown fields when skipUnexpectedFields = false") {
        val cRuntime =
          runtime(ParityRuntimeConfigs.discriminatorStrict) { implicit c => KindlingsJsonValueCodec.derived[ParityZeroAdt] }
        List[JsonValueCodec[ParityZeroAdt]](OptimizedCodecs.discriminatorStrictZeroAdt, cRuntime).foreach { c =>
          decodeError("unexpected field")(readFromString[ParityZeroAdt]("""{"type":"ParityZeroLeaf","extra":1}""")(c))
          readFromString[ParityZeroAdt]("""{"type":"ParityZeroLeaf"}""")(c) ==> ParityZeroLeaf()
        }
      }

      test("zero-field ADT leaf with a discriminator rejects a malformed object end") {
        val cRuntime =
          runtime(ParityRuntimeConfigs.discriminator) { implicit c => KindlingsJsonValueCodec.derived[ParityZeroAdt] }
        List[JsonValueCodec[ParityZeroAdt]](OptimizedCodecs.discriminatorZeroAdt, cRuntime).foreach { c =>
          val _ = intercept[JsonReaderException](readFromString[ParityZeroAdt]("""{"type":"ParityZeroLeaf" "x":1}""")(c))
          val _ = intercept[JsonReaderException](readFromString[ParityZeroAdt]("""{"type":"ParityZeroLeaf","x":1]""")(c))
        }
      }
    }

    group("transientEmpty (Spec ~2617, ~3731)") {
      val value = ParityEmptyCapable(Array.empty[Int], "", Map.empty, Nil)

      test("omits empty arrays, maps and collections but not empty strings") {
        val cRuntime =
          runtime(ParityRuntimeConfigs.transientEmpty) { implicit c => KindlingsJsonValueCodec.derived[ParityEmptyCapable] }
        List[JsonValueCodec[ParityEmptyCapable]](OptimizedCodecs.transientEmpty, cRuntime).foreach { c =>
          writeToString(value)(c) ==> """{"s":""}"""
          writeToString(ParityEmptyCapable(Array(1), "x", Map("k" -> 1), List(2)))(c) ==>
            """{"a":[1],"s":"x","m":{"k":1},"l":[2]}"""
        }
      }
    }

    group("transientDefault with array defaults (Spec ~2501)") {

      test("compares array contents with the default") {
        val cRuntime =
          runtime(ParityRuntimeConfigs.transientDefault) { implicit c => KindlingsJsonValueCodec.derived[ParityArrayDefault] }
        List[JsonValueCodec[ParityArrayDefault]](OptimizedCodecs.transientDefault, cRuntime).foreach { c =>
          writeToString(ParityArrayDefault())(c) ==> "{}"
          writeToString(ParityArrayDefault(Array(3)))(c) ==> """{"a":[3]}"""
        }
      }
    }

    group("JSON null for collections and defaulted fields (Spec ~1932, ~1995, ~2530)") {
      val nulls = """{"l":null,"m":null,"v":null}"""

      test("null decodes as an empty collection or map") {
        val cRuntime =
          runtime(ParityRuntimeConfigs.default) { implicit c => KindlingsJsonValueCodec.derived[ParityNullCollections] }
        List[JsonValueCodec[ParityNullCollections]](KindlingsJsonValueCodec.derived[ParityNullCollections], cRuntime)
          .foreach { c =>
            readFromString[ParityNullCollections](nulls)(c) ==> ParityNullCollections(Nil, Map.empty, Vector.empty)
          }
      }

      test("null decodes as an empty collection or map with requireCollectionFields") {
        val cRuntime = runtime(ParityRuntimeConfigs.requireCollection) { implicit c =>
          KindlingsJsonValueCodec.derived[ParityNullCollections]
        }
        List[JsonValueCodec[ParityNullCollections]](OptimizedCodecs.requireCollectionNulls, cRuntime).foreach { c =>
          readFromString[ParityNullCollections](nulls)(c) ==> ParityNullCollections(Nil, Map.empty, Vector.empty)
        }
      }

      test("null decodes as the default value of a field") {
        val cRuntime =
          runtime(ParityRuntimeConfigs.default) { implicit c => KindlingsJsonValueCodec.derived[ParityNullDefaults] }
        List[JsonValueCodec[ParityNullDefaults]](KindlingsJsonValueCodec.derived[ParityNullDefaults], cRuntime).foreach {
          c =>
            readFromString[ParityNullDefaults]("""{"s":null,"bi":null,"l":null,"m":null,"inner":null}""")(c) ==>
              ParityNullDefaults()
            readFromString[ParityNullDefaults]("{}")(c) ==> ParityNullDefaults()
        }
      }

      test("null is still rejected for a primitive field with a default") {
        val cRuntime =
          runtime(ParityRuntimeConfigs.default) { implicit c => KindlingsJsonValueCodec.derived[ParityPrimitiveDefault] }
        List[JsonValueCodec[ParityPrimitiveDefault]](KindlingsJsonValueCodec.derived[ParityPrimitiveDefault], cRuntime)
          .foreach { c =>
            val _ = intercept[JsonReaderException](readFromString[ParityPrimitiveDefault]("""{"n":null}""")(c))
          }
      }

      test("top-level null decodes as an empty collection (codec nullValue)") {
        readFromString[List[Int]]("null")(KindlingsJsonValueCodec.derived[List[Int]]) ==> Nil
        readFromString[Map[String, Int]]("null")(KindlingsJsonValueCodec.derived[Map[String, Int]]) ==> Map.empty
      }
    }

    group("BigDecimal / BigInt limits (JsonCodecMaker ~3147)") {

      test("BigDecimal is rounded to bigDecimalPrecision") {
        val cRuntime = runtime(ParityRuntimeConfigs.precision5) { implicit c => KindlingsJsonValueCodec.derived[ParityBigDecimal] }
        List[JsonValueCodec[ParityBigDecimal]](OptimizedCodecs.precision5, cRuntime).foreach { c =>
          val decoded = readFromString[ParityBigDecimal]("""{"value":123456.789}""")(c).value
          decoded ==> BigDecimal("1.2346E+5")
          decoded.mc.getPrecision ==> 5
        }
      }

      test("BigDecimal scale is limited by bigDecimalScaleLimit") {
        val cRuntime = runtime(ParityRuntimeConfigs.scaleLimit2) { implicit c => KindlingsJsonValueCodec.derived[ParityBigDecimal] }
        List[JsonValueCodec[ParityBigDecimal]](OptimizedCodecs.scaleLimit2, cRuntime).foreach { c =>
          // the reader's limits are exclusive: a scale of 2 already exceeds a limit of 2
          readFromString[ParityBigDecimal]("""{"value":1.2}""")(c) ==> ParityBigDecimal(BigDecimal("1.2"))
          decodeError("value exceeds limit for scale")(readFromString[ParityBigDecimal]("""{"value":1.25}""")(c))
        }
      }

      test("BigDecimal digits are limited by bigDecimalDigitsLimit") {
        val cRuntime =
          runtime(ParityRuntimeConfigs.digitsLimit3) { implicit c => KindlingsJsonValueCodec.derived[ParityBigDecimal] }
        List[JsonValueCodec[ParityBigDecimal]](OptimizedCodecs.digitsLimit3, cRuntime).foreach { c =>
          readFromString[ParityBigDecimal]("""{"value":1.2}""")(c) ==> ParityBigDecimal(BigDecimal("1.2"))
          decodeError("value exceeds limit for number of digits")(
            readFromString[ParityBigDecimal]("""{"value":1.23}""")(c)
          )
        }
      }

      test("BigInt digits are limited by bigIntDigitsLimit, not bigDecimalDigitsLimit") {
        val cRuntime =
          runtime(ParityRuntimeConfigs.bigIntDigitsLimit3) { implicit c => KindlingsJsonValueCodec.derived[ParityBigInt] }
        List[JsonValueCodec[ParityBigInt]](OptimizedCodecs.bigIntDigitsLimit3, cRuntime).foreach { c =>
          readFromString[ParityBigInt]("""{"value":12}""")(c) ==> ParityBigInt(BigInt(12))
          decodeError("value exceeds limit for number of digits")(readFromString[ParityBigInt]("""{"value":1234}""")(c))
        }
        val decimalRuntime =
          runtime(ParityRuntimeConfigs.digitsLimit3) { implicit c => KindlingsJsonValueCodec.derived[ParityBigInt] }
        List[JsonValueCodec[ParityBigInt]](OptimizedCodecs.bigDecimalDigitsLimit3BigInt, decimalRuntime).foreach { c =>
          readFromString[ParityBigInt]("""{"value":12345}""")(c) ==> ParityBigInt(BigInt(12345))
        }
      }
    }

    group("transientNull and scalaTransientSupport (Spec ~2590)") {

      test("transientNull only affects nullable (Scala 3 `T | Null`) fields, not every null reference") {
        val cRuntime =
          runtime(ParityRuntimeConfigs.transientNull) { implicit c => KindlingsJsonValueCodec.derived[ParityNullable] }
        List[JsonValueCodec[ParityNullable]](OptimizedCodecs.transientNull, cRuntime).foreach { c =>
          // as upstream: a `null` String is not skipped, and cannot be serialized
          val _ = intercept[NullPointerException](writeToString(ParityNullable(null, 1))(c))
          writeToString(ParityNullable("a", 1))(c) ==> """{"s":"a","n":1}"""
        }
      }

      test("scalaTransientSupport makes scala.transient fields transient (compile-time config)") {
        val c = OptimizedCodecs.scalaTransient
        writeToString(ParityScalaTransient("VVV", "non-default"))(c) ==> """{"required":"VVV"}"""
        readFromString[ParityScalaTransient]("""{"transient":"XXX","required":"VVV"}""")(c) ==> ParityScalaTransient("VVV")
        val off = KindlingsJsonValueCodec.derived[ParityScalaTransient]
        writeToString(ParityScalaTransient("VVV"))(off) ==> """{"required":"VVV","transient":"default"}"""
      }

      test("scalaTransientSupport also works with a runtime config and on the inline entry points") {
        val cRuntime =
          runtime(ParityRuntimeConfigs.scalaTransient) { implicit c => KindlingsJsonValueCodec.derived[ParityScalaTransient] }
        writeToString(ParityScalaTransient("VVV", "non-default"))(cRuntime) ==> """{"required":"VVV"}"""
        readFromString[ParityScalaTransient]("""{"transient":"XXX","required":"VVV"}""")(cRuntime) ==>
          ParityScalaTransient("VVV")
        val offRuntime =
          runtime(ParityRuntimeConfigs.default) { implicit c => KindlingsJsonValueCodec.derived[ParityScalaTransient] }
        readFromString[ParityScalaTransient]("""{"transient":"XXX","required":"VVV"}""")(offRuntime) ==>
          ParityScalaTransient("VVV", "XXX")
        locally {
          implicit val c: JsoniterConfig = ParityRuntimeConfigs.scalaTransient
          KindlingsJsonValueCodec.writeToString(ParityScalaTransient("VVV", "non-default")) ==> """{"required":"VVV"}"""
          KindlingsJsonValueCodec.readFromString[ParityScalaTransient]("""{"transient":"XXX","required":"VVV"}""") ==>
            Right(ParityScalaTransient("VVV"))
        }
      }
    }

    group("map keys (Spec ~523, ~636, ~1580)") {

      test("UUID keys") {
        val codec = KindlingsJsonValueCodec.derived[Map[java.util.UUID, Int]]
        val value = Map(new java.util.UUID(0, 0) -> 0)
        val json = """{"00000000-0000-0000-0000-000000000000":0}"""
        writeToString(value)(codec) ==> json
        readFromString[Map[java.util.UUID, Int]](json)(codec) ==> value
      }

      test("Char, Byte and java.lang.Character keys") {
        val chars = KindlingsJsonValueCodec.derived[Map[Char, Int]]
        writeToString(Map('V' -> 1))(chars) ==> """{"V":1}"""
        readFromString[Map[Char, Int]]("""{"V":1}""")(chars) ==> Map('V' -> 1)
        val bytes = KindlingsJsonValueCodec.derived[Map[Byte, Int]]
        writeToString(Map(4.toByte -> 1))(bytes) ==> """{"4":1}"""
        readFromString[Map[Byte, Int]]("""{"4":1}""")(bytes) ==> Map(4.toByte -> 1)
        val boxed = KindlingsJsonValueCodec.derived[Map[java.lang.Character, Int]]
        val boxedValue = Map(java.lang.Character.valueOf('X') -> 2)
        writeToString(boxedValue)(boxed) ==> """{"X":2}"""
        readFromString[Map[java.lang.Character, Int]]("""{"X":2}""")(boxed) ==> boxedValue
      }

      test("value class over String keys") {
        val codec = KindlingsJsonValueCodec.derived[Map[ParityUserId, Int]]
        val value = Map(ParityUserId("123abc") -> 123123)
        writeToString(value)(codec) ==> """{"123abc":123123}"""
        readFromString[Map[ParityUserId, Int]]("""{"123abc":123123}""")(codec) ==> value
      }

      test("a key type without a key codec is a compile-time error when mapAsArray is statically off") {
        compileErrors(
          """
          hearth.kindlings.jsoniterderivation.KindlingsJsonValueCodec.derived[Map[hearth.kindlings.jsoniterderivation.ParityNoKeyCodec, Int]]
          """
        ).check(
          "cannot be used as a JSON object key"
        )
      }
    }

    group("mapAsArray (round 2, item C)") {

      test("decoding reads the nested pairs the encoder writes") {
        val stringKeysRuntime =
          runtime(ParityRuntimeConfigs.mapAsArray) { implicit c => KindlingsJsonValueCodec.derived[Map[String, Int]] }
        List[JsonValueCodec[Map[String, Int]]](OptimizedCodecs.mapAsArray, stringKeysRuntime).foreach { c =>
          val json = writeToString(Map("a" -> 1, "b" -> 2))(c)
          json ==> """[["a",1],["b",2]]"""
          readFromString[Map[String, Int]](json)(c) ==> Map("a" -> 1, "b" -> 2)
        }
        val intKeysRuntime =
          runtime(ParityRuntimeConfigs.mapAsArray) { implicit c => KindlingsJsonValueCodec.derived[Map[Int, String]] }
        List[JsonValueCodec[Map[Int, String]]](OptimizedCodecs.mapAsArrayIntKeys, intKeysRuntime).foreach { c =>
          val json = writeToString(Map(1 -> "a"))(c)
          json ==> """[[1,"a"]]"""
          readFromString[Map[Int, String]](json)(c) ==> Map(1 -> "a")
        }
      }
    }

    group("alwaysEmitDiscriminator (Spec ~2715)") {

      test("a leaf class codec writes the discriminator and reads with or without it") {
        val cRuntime = runtime(ParityRuntimeConfigs.alwaysEmit) { implicit c => KindlingsJsonValueCodec.derived[ParityAAA] }
        val strictRuntime =
          runtime(ParityRuntimeConfigs.alwaysEmitStrict) { implicit c => KindlingsJsonValueCodec.derived[ParityAAA] }
        List[JsonValueCodec[ParityAAA]](OptimizedCodecs.alwaysEmitLeaf, cRuntime, OptimizedCodecs.alwaysEmitStrictLeaf, strictRuntime)
          .foreach { c =>
            writeToString(ParityAAA(1))(c) ==> """{"type":"ParityAAA","a":1}"""
            readFromString[ParityAAA]("""{"type":"ParityAAA","a":1}""")(c) ==> ParityAAA(1)
            readFromString[ParityAAA]("""{"a":1}""")(c) ==> ParityAAA(1)
          }
      }

      test("a leaf case object codec writes the discriminator") {
        val cRuntime =
          runtime(ParityRuntimeConfigs.alwaysEmit) { implicit c => KindlingsJsonValueCodec.derived[ParityDDD.type] }
        List[JsonValueCodec[ParityDDD.type]](OptimizedCodecs.alwaysEmitObject, cRuntime).foreach { c =>
          writeToString[ParityDDD.type](ParityDDD)(c) ==> """{"type":"ParityDDD"}"""
          readFromString[ParityDDD.type]("""{"type":"ParityDDD"}""")(c) ==> ParityDDD
        }
      }

      test("the ADT base codec writes the discriminator once") {
        val cRuntime = runtime(ParityRuntimeConfigs.alwaysEmit) { implicit c => KindlingsJsonValueCodec.derived[ParityAdt] }
        List[JsonValueCodec[ParityAdt]](OptimizedCodecs.alwaysEmitBase, cRuntime).foreach { c =>
          writeToString[ParityAdt](ParityAAA(1))(c) ==> """{"type":"ParityAAA","a":1}"""
          writeToString[ParityAdt](ParityDDD)(c) ==> """{"type":"ParityDDD"}"""
          readFromString[ParityAdt]("""{"type":"ParityAAA","a":1}""")(c) ==> ParityAAA(1)
        }
      }

      test("a duplicated discriminator in a leaf is rejected with checkFieldDuplication") {
        val cRuntime = runtime(ParityRuntimeConfigs.alwaysEmitCheckDuplication) { implicit c =>
          KindlingsJsonValueCodec.derived[ParityAAA]
        }
        List[JsonValueCodec[ParityAAA]](OptimizedCodecs.alwaysEmitCheckDuplicationLeaf, cRuntime).foreach { c =>
          decodeError("duplicate")(readFromString[ParityAAA]("""{"type":"ParityAAA","type":"ParityAAA","a":1}""")(c))
        }
      }
    }

    group("duplicated discriminator (Spec ~3092, round 2 item H)") {

      test("is rejected inside a leaf object with checkFieldDuplication") {
        val cRuntime = runtime(ParityRuntimeConfigs.discriminatorCheckDuplication) { implicit c =>
          KindlingsJsonValueCodec.derived[ParityAdt]
        }
        List[JsonValueCodec[ParityAdt]](OptimizedCodecs.discriminatorCheckDuplication, cRuntime).foreach { c =>
          decodeError("duplicate")(readFromString[ParityAdt]("""{"type":"ParityAAA","a":1,"type":"ParityAAA"}""")(c))
        }
      }
    }

    group("skipNestedOptionValues (Spec ~1362)") {

      test("Some(None) is written as null, None is omitted, and both read back") {
        val cRuntime = runtime(ParityRuntimeConfigs.skipNestedOptions) { implicit c =>
          KindlingsJsonValueCodec.derived[List[ParityNestedOptions]]
        }
        val value = List(
          ParityNestedOptions(Some("VVV"), Some(Some("WWW"))),
          ParityNestedOptions(None),
          ParityNestedOptions(None, Some(None))
        )
        val json = """[{"f1":"VVV","f2":"WWW"},{},{"f2":null}]"""
        List[JsonValueCodec[List[ParityNestedOptions]]](OptimizedCodecs.skipNestedOptions, cRuntime).foreach { c =>
          writeToString(value)(c) ==> json
          readFromString[List[ParityNestedOptions]](json)(c) ==> value
        }
      }
    }

    group("Unit (round 2, item B)") {

      test("Unit is written as {} and read back") {
        val codec = KindlingsJsonValueCodec.derived[ParityWithUnit]
        writeToString(ParityWithUnit((), 1))(codec) ==> """{"u":{},"i":1}"""
        readFromString[ParityWithUnit]("""{"u":{},"i":1}""")(codec) ==> ParityWithUnit((), 1)
        readFromString[ParityWithUnit]("""{"u":null,"i":1}""")(codec) ==> ParityWithUnit((), 1)
        val _ = intercept[JsonReaderException](readFromString[ParityWithUnit]("""{"u":1,"i":1}""")(codec))
        writeToString(())(KindlingsJsonValueCodec.derived[Unit]) ==> "{}"
      }
    }

    group("inlineOneValueClasses (round 2, item F)") {

      test("works with both a compile-time and a runtime config") {
        val cRuntime = runtime(ParityRuntimeConfigs.inlineOneValueClasses) { implicit c =>
          KindlingsJsonValueCodec.derived[ParityHasWrapper]
        }
        val value = ParityHasWrapper(Wrapper(1), StringWrapper("s"))
        List[JsonValueCodec[ParityHasWrapper]](OptimizedCodecs.inlineOneValueClasses, cRuntime).foreach { c =>
          writeToString(value)(c) ==> """{"w":1,"s":"s"}"""
          readFromString[ParityHasWrapper]("""{"w":1,"s":"s"}""")(c) ==> value
        }
        val notInlined = KindlingsJsonValueCodec.derived[ParityHasWrapper]
        writeToString(value)(notInlined) ==> """{"w":{"inner":1},"s":{"value":"s"}}"""
      }
    }

    group("constructor parameters without accessors (round 2, item G)") {

      test("are a compile-time error") {
        compileErrors(
          """
          hearth.kindlings.jsoniterderivation.KindlingsJsonValueCodec.derived[hearth.kindlings.jsoniterderivation.ParityPrivateParam]
          """
        ).check(
          "'token' parameter of 'hearth.kindlings.jsoniterderivation.ParityPrivateParam' should be defined as 'val' or 'var' in the primary constructor."
        )
      }
    }
  }
}
