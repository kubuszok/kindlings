package hearth.kindlings.jsoniterderivation

import com.github.plokhotnyuk.jsoniter_scala.core.{readFromString, writeToString, JsonValueCodec}
import hearth.MacroSuite

/** An explicit empty JSON array/object (`[]`/`{}`) for a collection/map field with a non-empty default value:
  *
  *   - by default, as in jsoniter-scala's `JsonCodecMaker.make`, it decodes as the field's default value,
  *   - with `withEmptyCollectionsAsEmpty` (a Kindlings superset) it decodes as an empty collection/map, so that with
  *     Kindlings' write-everything defaults (`transientDefault`/`transientEmpty` off) such values round-trip.
  *
  * Every scenario is checked with a config known at compile time (optimized codegen, `OptimizedCodecs`) and with one
  * only known at runtime (generic codegen, `EmptyCollectionsRuntimeConfigs`).
  */
final class EmptyCollectionsSpec extends MacroSuite {

  private val runtimeAsDefault: JsonValueCodec[EmptyCollDefaults] = {
    implicit val config: JsoniterConfig = EmptyCollectionsRuntimeConfigs.asDefault
    KindlingsJsonValueCodec.derived[EmptyCollDefaults]
  }
  private val runtimeAsEmpty: JsonValueCodec[EmptyCollDefaults] = {
    implicit val config: JsoniterConfig = EmptyCollectionsRuntimeConfigs.asEmpty
    KindlingsJsonValueCodec.derived[EmptyCollDefaults]
  }

  private val asDefaultCodecs = List("optimized" -> OptimizedCodecs.emptyCollAsDefault, "runtime" -> runtimeAsDefault)
  private val asEmptyCodecs = List("optimized" -> OptimizedCodecs.emptyCollAsEmpty, "runtime" -> runtimeAsEmpty)

  private val allEmpty = EmptyCollDefaults(Nil, Map.empty, Vector.empty, Set.empty, Nil)
  private val emptyJson = """{"xs":[],"m":{},"v":[],"s":[],"noDefault":[]}"""

  test("the flag defaults to jsoniter-scala's behaviour") {
    assertEquals(JsoniterConfig.default.emptyCollectionsAsDefault, true)
    assertEquals(JsoniterConfig.default.withEmptyCollectionsAsEmpty.emptyCollectionsAsDefault, false)
    assertEquals(
      JsoniterConfig.default.withEmptyCollectionsAsEmpty.withEmptyCollectionsAsDefault(true).emptyCollectionsAsDefault,
      true
    )
  }

  group("default (jsoniter-scala parity): [] / {} decode as the field's default") {
    asDefaultCodecs.foreach { case (path, codec) =>
      test(s"$path codegen") {
        readFromString[EmptyCollDefaults](emptyJson)(codec) ==> EmptyCollDefaults(noDefault = Nil)
        // so a value with empty collections is written as [] / {} but not read back
        writeToString(allEmpty)(codec) ==> emptyJson
      }
    }
  }

  group("withEmptyCollectionsAsEmpty: [] / {} decode as empty collections/maps") {
    asEmptyCodecs.foreach { case (path, codec) =>
      test(s"$path codegen: explicit empty values are empty and round-trip") {
        readFromString[EmptyCollDefaults](emptyJson)(codec) ==> allEmpty
        readFromString[EmptyCollDefaults](writeToString(allEmpty)(codec))(codec) ==> allEmpty
      }

      test(s"$path codegen: absent fields and null still decode as the default") {
        readFromString[EmptyCollDefaults]("""{"noDefault":[]}""")(codec) ==> EmptyCollDefaults(noDefault = Nil)
        readFromString[EmptyCollDefaults]("""{"xs":null,"m":null,"v":null,"s":null,"noDefault":[2]}""")(codec) ==>
          EmptyCollDefaults(noDefault = List(2))
      }

      test(s"$path codegen: non-empty values are decoded as usual") {
        val value = EmptyCollDefaults(List(2, 3), Map("b" -> 2), Vector("x"), Set(5), List(4))
        readFromString[EmptyCollDefaults](writeToString(value)(codec))(codec) ==> value
      }
    }
  }
}
