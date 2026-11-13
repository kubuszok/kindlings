package hearth.kindlings.jsoniterderivation

import com.github.plokhotnyuk.jsoniter_scala.core.{readFromString, writeToString, JsonValueCodec}
import hearth.MacroSuite

/** Scala 3-only behaviours aligned with jsoniter-scala's `JsonCodecMaker.make` (v2.42.3): generic tuples
  * (`JsonCodecMakerGenericTuplesSpec`) and named tuples (`JsonCodecMakerNamedTupleSpec`).
  */
final class JsoniterScala3ParitySpec extends MacroSuite {

  private def roundTrip[A](codec: JsonValueCodec[A])(value: A, json: String): Unit = {
    writeToString(value)(codec) ==> json
    readFromString[A](json)(codec) ==> value
  }

  group("jsoniter-scala Scala 3 parity") {

    group("nullable (T | Null) fields (Spec ~2590, NewTypeSpec ~404)") {

      test("are optional when absent and written as null") {
        val cRuntime = {
          implicit val c: JsoniterConfig = NullableRuntimeConfigs.default
          KindlingsJsonValueCodec.derived[ParityNullableFields]
        }
        List[JsonValueCodec[ParityNullableFields]](KindlingsJsonValueCodec.derived[ParityNullableFields], cRuntime)
          .foreach { c =>
            readFromString[ParityNullableFields]("""{"n":1}""")(c) ==> ParityNullableFields(null, null, 1)
            readFromString[ParityNullableFields]("""{"s":null,"l":null,"n":1}""")(c) ==> ParityNullableFields(
              null,
              null,
              1
            )
            roundTrip(c)(ParityNullableFields("a", 1, 2), """{"s":"a","l":1,"n":2,"ls":null}""")
            writeToString(ParityNullableFields(null, null, 1))(c) ==> """{"s":null,"l":null,"n":1,"ls":null}"""
            roundTrip(c)(ParityNullableFields("a", 1, 2, List(3)), """{"s":"a","l":1,"n":2,"ls":[3]}""")
          }
      }

      test("transientNull skips them when null, and the codec reads its own output") {
        val cRuntime = {
          implicit val c: JsoniterConfig = NullableRuntimeConfigs.transientNull
          KindlingsJsonValueCodec.derived[ParityNullableFields]
        }
        List[JsonValueCodec[ParityNullableFields]](OptimizedCodecs.nullableTransientNull, cRuntime).foreach { c =>
          writeToString(ParityNullableFields(null, null, 1))(c) ==> """{"n":1}"""
          readFromString[ParityNullableFields]("""{"n":1}""")(c) ==> ParityNullableFields(null, null, 1)
          roundTrip(c)(ParityNullableFields("a", 1, 2), """{"s":"a","l":1,"n":2}""")
        }
      }
    }

    group("generic tuples (GenericTuplesSpec)") {

      test("tuples with arities greater than 22") {
        type T24 = (
            Int,
            Int,
            Int,
            Int,
            Int,
            Int,
            Int,
            Int,
            Int,
            Int,
            Int,
            Int,
            Int,
            Int,
            Int,
            Int,
            Int,
            Int,
            Int,
            Int,
            Int,
            Int,
            Int,
            String
        )
        roundTrip(KindlingsJsonValueCodec.derived[T24])(
          (1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, "24"),
          """[1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,"24"]"""
        )
      }

      test("generic tuples") {
        type B = Byte
        type I = Int
        type L = Long
        type S = Short
        roundTrip(KindlingsJsonValueCodec.derived[EmptyTuple])(EmptyTuple, "[]")
        roundTrip(KindlingsJsonValueCodec.derived[Byte *: Short *: Int *: Long *: EmptyTuple])(
          (1: Byte, 2: Short, 3, 4L),
          "[1,2,3,4]"
        )
        roundTrip(KindlingsJsonValueCodec.derived[B *: S *: I *: L *: EmptyTuple])(
          (1: B) *: (2: S) *: 3 *: 4L *: EmptyTuple,
          "[1,2,3,4]"
        )
        roundTrip(KindlingsJsonValueCodec.derived[Byte *: Short *: Tuple2[Int, Long]])(
          (1: Byte, 2: Short, 3, 4L),
          "[1,2,3,4]"
        )
        roundTrip(KindlingsJsonValueCodec.derived[Tuple.Concat[(Byte, Short), (Int, Long)]])(
          (1: Byte, 2: Short, 3, 4L),
          "[1,2,3,4]"
        )
        roundTrip(KindlingsJsonValueCodec.derived[Tuple.Append[(Byte, Short), Int]])((1: Byte, 2: Short, 3), "[1,2,3]")
        roundTrip(KindlingsJsonValueCodec.derived[Tuple.Drop[(Long, Int, String), 1]])((1, "VVV"), """[1,"VVV"]""")
        roundTrip(KindlingsJsonValueCodec.derived[Tuple.Drop[(Long, Int, String), 3]])(EmptyTuple, "[]")
        roundTrip(KindlingsJsonValueCodec.derived[Tuple.Take[(Int, String, Long), 2]])(
          1 *: "VVV" *: EmptyTuple,
          """[1,"VVV"]"""
        )
        roundTrip(KindlingsJsonValueCodec.derived[Tuple.Zip[Tuple1[Int], Tuple1[String]]])(
          (1, "VVV") *: EmptyTuple,
          """[[1,"VVV"]]"""
        )
        roundTrip(KindlingsJsonValueCodec.derived[Tuple.Map[(Int, String), Option]])(
          (Some(1), Some("VVV")),
          """[1,"VVV"]"""
        )
        roundTrip(KindlingsJsonValueCodec.derived[Tuple.InverseMap[(Option[Int], Option[String]), Option]])(
          (1, "VVV"),
          """[1,"VVV"]"""
        )
      }
    }

    group("named tuples are JSON objects (NamedTupleSpec)") {

      test("named tuples") {
        roundTrip(KindlingsJsonValueCodec.derived[(i: Int)])(Tuple1(1), """{"i":1}""")
        roundTrip(KindlingsJsonValueCodec.derived[(i: Int, s: String)])((1, "VVV"), """{"i":1,"s":"VVV"}""")
      }

      test("nested named tuples, generic named tuples and field name mapping") {
        roundTrip(KindlingsJsonValueCodec.derived[(i: Int, t: (d: Double, s: String))])(
          (1, (2.0, "VVV")),
          """{"i":1,"t":{"d":2.0,"s":"VVV"}}"""
        )
        roundTrip(
          KindlingsJsonValueCodec.derived[(intField: Int, strField: String)](using
            JsoniterConfig.default.withSnakeCaseFieldNames
          )
        )(
          (1, "VVV"),
          """{"int_field":1,"str_field":"VVV"}"""
        )
        roundTrip(KindlingsJsonValueCodec.derived[List[(i: Int, s: String)]])(
          List((1, "VVV"), (2, "WWW")),
          """[{"i":1,"s":"VVV"},{"i":2,"s":"WWW"}]"""
        )
      }

      test("named tuples with more than 22 fields") {
        roundTrip(
          KindlingsJsonValueCodec.derived[
            (
                i1: Int,
                i2: Int,
                i3: Int,
                i4: Int,
                i5: Int,
                i6: Int,
                i7: Int,
                i8: Int,
                i9: Int,
                i10: Int,
                i11: Int,
                i12: Int,
                i13: Int,
                i14: Int,
                i15: Int,
                i16: Int,
                i17: Int,
                i18: Int,
                i19: Int,
                i20: Int,
                i21: Int,
                i22: Int,
                i23: Int
            )
          ]
        )(
          (1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23),
          """{"i1":1,"i2":2,"i3":3,"i4":4,"i5":5,"i6":6,"i7":7,"i8":8,"i9":9,"i10":10,"i11":11,"i12":12,"i13":13,"i14":14,"i15":15,"i16":16,"i17":17,"i18":18,"i19":19,"i20":20,"i21":21,"i22":22,"i23":23}"""
        )
      }

      test("complex generic and named tuples") {
        roundTrip(KindlingsJsonValueCodec.derived[NamedTuple.DropNames[(i: Int, s: String)]])(
          (1, "VVV"),
          """[1,"VVV"]"""
        )
        roundTrip(KindlingsJsonValueCodec.derived[Tuple.Reverse[(String, Int)]])((1, "VVV"), """[1,"VVV"]""")
      }
    }

    group("named tuples behind type aliases (Hearth gap, docs/research/hearth-gap-named-tuple-aliases.md)") {
      // Upstream supports these (NamedTupleSpec). Hearth's `Type.isNamedTuple` does not dealias, so a named tuple
      // reached through an alias is not recognized. When Hearth fixes it, these checks fail: replace them with
      // round-trips (`{}` for NamedTuple.Empty, `{"a":1,"b":["VVV"]}` for the generic alias).
      test("are not derivable yet") {
        compileErrors("hearth.kindlings.jsoniterderivation.KindlingsJsonValueCodec.derived[NamedTuple.Empty]")
          .check("is not a named tuple")
        compileErrors(
          """
          type GenericNamedTuple[A, B] = (a: A, b: B)
          hearth.kindlings.jsoniterderivation.KindlingsJsonValueCodec.derived[GenericNamedTuple[Option[Int], List[String]]]
          """
        ).check("is not a named tuple")
      }
    }
    group("Scala 3 enums (NewEnumSpec)") {

      test("parameterless cases work like case objects") {
        val lights = List(ParityTrafficLight.Red, ParityTrafficLight.Green)
        roundTrip(KindlingsJsonValueCodec.derived[List[ParityTrafficLight]])(lights, """["Red","Green"]""")
        roundTrip(
          KindlingsJsonValueCodec.derived[List[ParityTrafficLight]](using
            JsoniterConfig.default.withDiscriminator("type")
          )
        )(
          lights,
          """[{"type":"Red"},{"type":"Green"}]"""
        )
        roundTrip(KindlingsJsonValueCodec.derived[List[ParityRgb]])(
          List(ParityRgb.Red, ParityRgb.Green),
          """["Red","Green"]"""
        )
      }

      test("@named on enum cases (NewEnumSpec Side)") {
        roundTrip(KindlingsJsonValueCodec.derived[List[ParitySide]])(
          List(ParitySide.Buy, ParitySide.Sell),
          """["0","1"]"""
        )
      }

      test("mixed enum ADTs") {
        val values = List(ParityColorAdt.Red, ParityColorAdt.Mix(0))
        roundTrip(KindlingsJsonValueCodec.derived[List[ParityColorAdt]])(values, """["Red",{"Mix":{"mix":0}}]""")
        roundTrip(
          KindlingsJsonValueCodec.derived[List[ParityColorAdt]](using JsoniterConfig.default.withDiscriminator("type"))
        )(
          values,
          """[{"type":"Red"},{"type":"Mix","mix":0}]"""
        )
      }

      test("enums extending java.lang.Enum are strings named by javaEnumValueNameMapper") {
        val values = List(ParityJavaLike.`JDK-17`, ParityJavaLike.`JDK-11`)
        roundTrip(
          KindlingsJsonValueCodec.derived[List[ParityJavaLike]](using JsoniterConfig.default.withDiscriminator("type"))
        )(
          values,
          """["JDK-17","JDK-11"]"""
        )
        roundTrip(
          KindlingsJsonValueCodec.derived[List[ParityJavaLike]](using
            JsoniterConfig.default.withJavaEnumValueNameMapper(_.toLowerCase)
          )
        )(
          values,
          """["jdk-17","jdk-11"]"""
        )
      }
    }
    group("literal types in case classes (LiteralTypesSpec)") {
      test("case class with literal types") {
        roundTrip(KindlingsJsonValueCodec.derived[ParityLiteralTypes])(
          ParityLiteralTypes("VVV", true, '1', 2, 3L, 4.0f, 5.0),
          """{"s":"VVV","b":true,"c":"1","i":2,"l":3,"f":4.0,"d":5.0}"""
        )
      }
    }
  }
}
