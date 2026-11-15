package hearth.kindlings.jsoniterderivation

import com.github.plokhotnyuk.jsoniter_scala.core.{
  readFromString,
  writeToString,
  JsonReaderException,
  JsonValueCodec,
  ReaderConfig
}
import hearth.MacroSuite

final case class UofNonEmpty(head: ::[Int], withDefault: ::[String] = ::("d", Nil))
final case class UofName(value: String)
final case class UofNameMap(byName: Map[UofName, Int])
final case class UofPerson(name: String, age: Int)
sealed trait UofShape
final case class UofCircle(radius: Double) extends UofShape
case object UofEmpty extends UofShape
sealed trait UofColour
case object UofRed extends UofColour
case object UofGreen extends UofColour
object UofLevel extends Enumeration {
  val Low, High = Value
}
final case class UofLevelHolder(level: UofLevel.Value)

object UofRuntimeConfigs {
  @volatile var default: JsoniterConfig = JsoniterConfig.default
  @volatile var mapLimit0: JsoniterConfig = JsoniterConfig.default.withMapMaxInsertNumber(0)
  @volatile var mapLimit1: JsoniterConfig = JsoniterConfig.default.withMapMaxInsertNumber(1)
  @volatile var inlineOneValue: JsoniterConfig = JsoniterConfig.default.withInlineOneValueClasses
  @volatile var encodingOnly: JsoniterConfig = JsoniterConfig.default.withEncodingOnly
  @volatile var discriminator: JsoniterConfig = JsoniterConfig.default.withDiscriminator("type")
  @volatile var discriminatorAnywhere: JsoniterConfig =
    JsoniterConfig.default.withDiscriminator("type").withRequireDiscriminatorFirst(false)
  @volatile var circeLike: JsoniterConfig = JsoniterConfig.default.withCirceLikeObjectEncoding
  @volatile var scalaEnumValueId: JsoniterConfig = JsoniterConfig.default.withUseScalaEnumValueId
}

/** Behaviours aligned with jsoniter-scala's `JsonCodecMaker.make` after the differential tests of the
  * upstream-oracle-tests module (`JsoniterOracleSpec`) found them. Every scenario runs on the optimized codegen (a
  * config known at compile time, `OptimizedCodecs`) and on the generic one (`UofRuntimeConfigs`).
  */
final class UpstreamOracleFindingsSpec extends MacroSuite {

  private val noHexDump = ReaderConfig.withAppendHexDumpToParseException(false)

  private def decodeError[A](codec: JsonValueCodec[A], json: String): String =
    intercept[JsonReaderException](readFromString[A](json, noHexDump)(codec)).getMessage

  private def runtime[A](config: JsoniterConfig)(derive: JsoniterConfig => A): A = derive(config)

  group("upstream oracle findings") {

    test("scala.collection.immutable.:: is a non-empty JSON array (required, no empty value)") {
      val codecs = List[JsonValueCodec[UofNonEmpty]](
        KindlingsJsonValueCodec.derived[UofNonEmpty],
        runtime(UofRuntimeConfigs.default)(implicit c => KindlingsJsonValueCodec.derived[UofNonEmpty])
      )
      codecs.foreach { c =>
        writeToString(UofNonEmpty(::(1, List(2))))(c) ==> """{"head":[1,2],"withDefault":["d"]}"""
        readFromString[UofNonEmpty]("""{"head":[1,2],"withDefault":["x"]}""")(c) ==>
          UofNonEmpty(::(1, List(2)), ::("x", Nil))
        // `[]` and `null` of a field with a default value are the default
        readFromString[UofNonEmpty]("""{"head":[1],"withDefault":[]}""")(c) ==> UofNonEmpty(::(1, Nil))
        readFromString[UofNonEmpty]("""{"head":[1],"withDefault":null}""")(c) ==> UofNonEmpty(::(1, Nil))
        decodeError(c, """{"head":[1],"withDefault":1}""") ==> "expected '[' or null, offset: 0x0000001a"
        decodeError(c, """{"head":[]}""") ==> "expected non-empty JSON array, offset: 0x00000009"
        decodeError(c, """{"head":null}""") ==> "expected non-empty JSON array, offset: 0x00000008"
        decodeError(c, """{"withDefault":["x"]}""") ==> "missing required field \"head\", offset: 0x00000014"
        c.nullValue ==> null
      }
      val topLevel = KindlingsJsonValueCodec.derived[::[Int]]
      readFromString[::[Int]]("[1]")(topLevel) ==> ::(1, Nil)
      decodeError(topLevel, "[]") ==> "expected non-empty JSON array, offset: 0x00000001"
    }

    test("mapMaxInsertNumber is checked after every insert (a limit of 0 rejects the first entry)") {
      val limit0 = List[JsonValueCodec[Map[String, Int]]](
        OptimizedCodecs.uofMapLimit0,
        runtime(UofRuntimeConfigs.mapLimit0)(implicit c => KindlingsJsonValueCodec.derived[Map[String, Int]])
      )
      limit0.foreach { c =>
        readFromString[Map[String, Int]]("{}")(c) ==> Map.empty
        decodeError(c, """{"a":1}""") ==> "too many map inserts, offset: 0x00000005"
      }
      val limit1 = List[JsonValueCodec[Map[String, Int]]](
        OptimizedCodecs.uofMapLimit1,
        runtime(UofRuntimeConfigs.mapLimit1)(implicit c => KindlingsJsonValueCodec.derived[Map[String, Int]])
      )
      limit1.foreach { c =>
        readFromString[Map[String, Int]]("""{"a":1}""")(c) ==> Map("a" -> 1)
        decodeError(c, """{"a":1,"b":22}""") ==> "too many map inserts, offset: 0x0000000c"
      }
    }

    test("with inlineOneValueClasses, nullValue of a one-value class wraps its field's nullValue") {
      List[JsonValueCodec[UofNameMap]](
        OptimizedCodecs.uofInlineNameMap,
        runtime(UofRuntimeConfigs.inlineOneValue)(implicit c => KindlingsJsonValueCodec.derived[UofNameMap])
      ).foreach(c => c.nullValue ==> UofNameMap(Map.empty))
    }

    test("with encodingOnly, nullValue throws NotImplementedError (it is a part of decoding)") {
      List[JsonValueCodec[UofPerson]](
        OptimizedCodecs.uofEncodingOnly,
        runtime(UofRuntimeConfigs.encodingOnly)(implicit c => KindlingsJsonValueCodec.derived[UofPerson])
      ).foreach { c =>
        intercept[NotImplementedError](c.nullValue).getMessage ==> "an implementation is missing"
        writeToString(UofPerson("a", 1))(c) ==> """{"name":"a","age":1}"""
      }
    }

    test("ADT errors for JSON values of another type, as jsoniter-scala reports them") {
      List[JsonValueCodec[UofShape]](
        OptimizedCodecs.uofShapeDiscriminator,
        runtime(UofRuntimeConfigs.discriminator)(implicit c => KindlingsJsonValueCodec.derived[UofShape])
      ).foreach { c =>
        decodeError(c, "\"UofEmpty\"") ==> "expected '{', offset: 0x00000000"
        decodeError(c, "1") ==> "expected '{', offset: 0x00000000"
      }
      List[JsonValueCodec[UofShape]](
        KindlingsJsonValueCodec.derived[UofShape],
        runtime(UofRuntimeConfigs.default)(implicit c => KindlingsJsonValueCodec.derived[UofShape])
      ).foreach(c => decodeError(c, "1") ==> "expected '\"' or '{', offset: 0x00000000")
      List[JsonValueCodec[UofColour]](
        KindlingsJsonValueCodec.derived[UofColour],
        runtime(UofRuntimeConfigs.default)(implicit c => KindlingsJsonValueCodec.derived[UofColour])
      ).foreach(c => decodeError(c, "1") ==> "expected '\"', offset: 0x00000000")
    }

    test("with circeLikeObjectEncoding, case objects are not read from strings") {
      List[JsonValueCodec[UofShape]](
        OptimizedCodecs.uofShapeCirceLike,
        runtime(UofRuntimeConfigs.circeLike)(implicit c => KindlingsJsonValueCodec.derived[UofShape])
      ).foreach { c =>
        readFromString[UofShape]("""{"UofEmpty":{}}""")(c) ==> UofEmpty
        assert(decodeError(c, "\"UofEmpty\"").endsWith(", offset: 0x00000000"))
      }
    }

    test("an unknown discriminator value is reported where it was read, also when it is not the first key") {
      List[JsonValueCodec[UofShape]](
        OptimizedCodecs.uofShapeDiscriminatorAnywhere,
        runtime(UofRuntimeConfigs.discriminatorAnywhere)(implicit c => KindlingsJsonValueCodec.derived[UofShape])
      ).foreach { c =>
        readFromString[UofShape]("""{"radius":1,"type":"UofCircle"}""")(c) ==> UofCircle(1)
        decodeError(c, """{"radius":1,"type":"Unknown"}""") ==>
          "illegal value of discriminator field \"type\", offset: 0x0000001b"
      }
    }

    test("useScalaEnumValueId: a value that is not a number fails with 'expected digit'") {
      List[JsonValueCodec[UofLevelHolder]](
        OptimizedCodecs.uofScalaEnumValueId,
        runtime(UofRuntimeConfigs.scalaEnumValueId)(implicit c => KindlingsJsonValueCodec.derived[UofLevelHolder])
      ).foreach { c =>
        readFromString[UofLevelHolder]("""{"level":1}""")(c) ==> UofLevelHolder(UofLevel.High)
        decodeError(c, """{"level":"High"}""") ==> "expected digit, offset: 0x00000009"
        decodeError(c, """{"level":7}""") ==> "illegal enum value 7, offset: 0x00000009"
      }
    }
  }
}
