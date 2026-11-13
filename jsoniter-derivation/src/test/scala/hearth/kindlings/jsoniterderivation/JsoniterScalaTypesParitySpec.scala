package hearth.kindlings.jsoniterderivation

import com.github.plokhotnyuk.jsoniter_scala.core.{readFromString, writeToString, JsonReaderException, JsonValueCodec}
import hearth.MacroSuite

/** Type-support behaviours aligned with jsoniter-scala's `JsonCodecMaker.make` (v2.42.3). The `Spec ~N` references
  * point at upstream's `JsonCodecMakerSpec.scala`.
  */
final class JsoniterScalaTypesParitySpec extends MacroSuite {

  private def decodeError(fragment: String)(thunk: => Any): Unit = {
    val error = intercept[JsonReaderException](thunk)
    assert(error.getMessage.contains(fragment), s"expected an error containing '$fragment', got: ${error.getMessage}")
  }

  private def runtime[A](config: JsoniterConfig)(derive: JsoniterConfig => A): A = derive(config)

  group("jsoniter-scala type parity") {

    group("tuples are JSON arrays (Spec ~1720-1746)") {

      test("case class with tuple fields") {
        val codec = KindlingsJsonValueCodec.derived[TupleFields]
        val value = TupleFields((1, 2.2, List('V')), ("VVV", BigInt(3), Some(North)))
        val json = """{"t1":[1,2.2,["V"]],"t2":["VVV",3,"North"]}"""
        writeToString(value)(codec) ==> json
        readFromString[TupleFields](json)(codec) ==> value
      }

      test("unexpected number of array values") {
        val codec = KindlingsJsonValueCodec.derived[TupleFields]
        decodeError("expected ','")(readFromString[TupleFields]("""{"t1":[1,2.2],"t2":["VVV",3,"North"]}""")(codec))
        decodeError("expected ']'")(
          readFromString[TupleFields]("""{"t1":[1,2.2,["V"]],"t2":["VVV",3,"North","XXX"]}""")(codec)
        )
      }

      test("top-level tuples, tuples with type aliases and nested tuples") {
        val codec = KindlingsJsonValueCodec.derived[(String, Int)]
        writeToString(("VVV", 1))(codec) ==> """["VVV",1]"""
        readFromString[(String, Int)]("""["VVV",1]""")(codec) ==> (("VVV", 1))
        val aliased = KindlingsJsonValueCodec.derived[(JsoniterAliases.Name, Int)]
        writeToString[(JsoniterAliases.Name, Int)](("VVV", 1))(aliased) ==> """["VVV",1]"""
        val nested = KindlingsJsonValueCodec.derived[List[(Int, (String, Boolean))]]
        val value = List((1, ("a", true)), (2, ("b", false)))
        val json = """[[1,["a",true]],[2,["b",false]]]"""
        writeToString(value)(nested) ==> json
        readFromString[List[(Int, (String, Boolean))]](json)(nested) ==> value
        val single = KindlingsJsonValueCodec.derived[Tuple1[Int]]
        writeToString(Tuple1(1))(single) ==> "[1]"
        readFromString[Tuple1[Int]]("[1]")(single) ==> Tuple1(1)
      }

      test("JSON null and objects are rejected for a tuple (null default, as upstream)") {
        val codec = KindlingsJsonValueCodec.derived[(String, Int)]
        decodeError("expected '['")(readFromString[(String, Int)]("null")(codec))
        decodeError("expected '['")(readFromString[(String, Int)]("""{"_1":"VVV","_2":1}""")(codec))
      }

      test("tuples with 22 elements") {
        type T22 = (
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
        val codec = KindlingsJsonValueCodec.derived[T22]
        val value: T22 = (1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, "22")
        val json = """[1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,"22"]"""
        writeToString(value)(codec) ==> json
        readFromString[T22](json)(codec) ==> value
      }
    }
    group("stringified (Spec ~224, ~454, ~1712, ~1732, ~1942, ~2383)") {

      test("@stringified on Int, BigInt, Option and List fields stringifies their numbers") {
        val codec = KindlingsJsonValueCodec.derived[ParityStringified]
        val value = ParityStringified(1, BigInt(2), Some(1), List(2))
        val json = """{"i":"1","bi":"2","o":"1","l":["2"]}"""
        writeToString(value)(codec) ==> json
        readFromString[ParityStringified](json)(codec) ==> value
        decodeError("expected '\"'")(readFromString[ParityStringified]("""{"i":1,"bi":"2","o":"1","l":["2"]}""")(codec))
        decodeError("expected '\"'")(readFromString[ParityStringified]("""{"i":"1","bi":"2","o":1,"l":["2"]}""")(codec))
        decodeError("expected '\"'")(readFromString[ParityStringified]("""{"i":"1","bi":"2","o":"1","l":[2]}""")(codec))
      }

      test("@stringified on Boolean, and it does not leak into other fields or nested case classes") {
        val codec = KindlingsJsonValueCodec.derived[ParityStringifiedMixed]
        val value = ParityStringifiedMixed(true, 1, ParityInner(2))
        val json = """{"b":"true","n":1,"inner":{"x":2}}"""
        writeToString(value)(codec) ==> json
        readFromString[ParityStringifiedMixed](json)(codec) ==> value
      }

      test("isStringified applies down to the first case class only, never to its fields (Spec ~2150)") {
        val value = ParityAllNumbers(
          1,
          2,
          3,
          4L,
          1.5f,
          2.5,
          true,
          java.lang.Long.valueOf(5L),
          BigDecimal("1.1"),
          Some(6),
          Map("k" -> 7L),
          (8, 9.5),
          'c',
          "s"
        )
        val json =
          """{"b":1,"s":2,"i":3,"l":4,"f":1.5,"d":2.5,"bool":true,"jl":5,"bd":1.1,"os":6,"m":{"k":7},"t":[8,9.5],"c":"c","str":"s"}"""
        val cRuntime =
          runtime(StringifiedRuntimeConfigs.stringified) { implicit c =>
            KindlingsJsonValueCodec.derived[ParityAllNumbers]
          }
        List[JsonValueCodec[ParityAllNumbers]](OptimizedCodecs.stringifiedAll, cRuntime).foreach { c =>
          writeToString(value)(c) ==> json
          readFromString[ParityAllNumbers](json)(c) ==> value
        }
      }

      test("isStringified applies to top-level values, options and nested collections") {
        val intRuntime =
          runtime(StringifiedRuntimeConfigs.stringified)(implicit c => KindlingsJsonValueCodec.derived[Int])
        List[JsonValueCodec[Int]](OptimizedCodecs.stringifiedInt, intRuntime).foreach { c =>
          writeToString(3)(c) ==> "\"3\""
          readFromString[Int]("\"3\"")(c) ==> 3
        }
        val optRuntime =
          runtime(StringifiedRuntimeConfigs.stringified)(implicit c => KindlingsJsonValueCodec.derived[Option[BigInt]])
        List[JsonValueCodec[Option[BigInt]]](OptimizedCodecs.stringifiedOption, optRuntime).foreach { c =>
          writeToString[Option[BigInt]](Some(BigInt(123)))(c) ==> "\"123\""
          writeToString[Option[BigInt]](None)(c) ==> "null"
          readFromString[Option[BigInt]]("\"123\"")(c) ==> Some(BigInt(123))
        }
        type S = scala.collection.mutable.Set[List[BigDecimal]]
        val setRuntime =
          runtime(StringifiedRuntimeConfigs.stringified)(implicit c => KindlingsJsonValueCodec.derived[S])
        List[JsonValueCodec[S]](OptimizedCodecs.stringifiedSet, setRuntime).foreach { c =>
          val value: S = scala.collection.mutable.Set(List(BigDecimal("1.1"), BigDecimal("2.2")))
          writeToString(value)(c) ==> "[[\"1.1\",\"2.2\"]]"
          readFromString[S]("[[\"1.1\",\"2.2\"]]")(c) ==> value
        }
      }

      test("stringified BigDecimal honours the config limits") {
        val cRuntime = runtime(StringifiedRuntimeConfigs.stringifiedDigits3) { implicit c =>
          KindlingsJsonValueCodec.derived[List[BigDecimal]]
        }
        List[JsonValueCodec[List[BigDecimal]]](OptimizedCodecs.stringifiedDigits3, cRuntime).foreach { c =>
          readFromString[List[BigDecimal]]("[\"1.2\"]")(c) ==> List(BigDecimal("1.2"))
          decodeError("value exceeds limit for number of digits")(readFromString[List[BigDecimal]]("[\"1.234\"]")(c))
        }
      }

      test("stringified Enumeration ids (Spec ~861)") {
        val cRuntime = runtime(StringifiedRuntimeConfigs.stringifiedIds) { implicit c =>
          KindlingsJsonValueCodec.derived[ParityLocation.ParityLocation]
        }
        List[JsonValueCodec[ParityLocation.ParityLocation]](OptimizedCodecs.stringifiedIds, cRuntime).foreach { c =>
          writeToString(ParityLocation.GPS)(c) ==> "\"1\""
          readFromString[ParityLocation.ParityLocation]("\"1\"")(c) ==> ParityLocation.GPS
        }
      }
    }
  }
}
