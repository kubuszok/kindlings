package hearth.kindlings.jsoniterderivation

import com.github.plokhotnyuk.jsoniter_scala.core.{
  readFromString,
  writeToString,
  JsonReaderException,
  JsonValueCodec,
  ReaderConfig
}
import hearth.MacroSuite
import hearth.kindlings.jsoniterderivation.annotations.transientField

object SpsBody extends Enumeration {
  type Body = Value
  val Sun, Moon: Body = Value
  private val Vesta: Body = Value("Vesta")
  lazy val Halley: Body = Value("Halley")
  def hidden: Body = Vesta
  def extra(name: String): Body = Value(nextId, name)
}
object SpsTemperature extends Enumeration {
  val Cold = Value(-1)
  val Hot = Value(1)
}
final case class SpsBodies(main: SpsBody.Body, all: List[SpsBody.Body], byBody: Map[SpsBody.Body, Int])

sealed trait SpsLeafBase
case object SpsLeaf extends SpsLeafBase

final case class SpsDependent(value: String)(@transientField val length: Int = value.length)

sealed trait SpsZone
case object `Sps/Alaska` extends SpsZone
case object `Sps/Paris` extends SpsZone

object SpsRuntimeConfigs {
  @volatile var default: JsoniterConfig = JsoniterConfig.default
  @volatile var ids: JsoniterConfig = JsoniterConfig.default.withUseScalaEnumValueId
  @volatile var inline: JsoniterConfig = JsoniterConfig.default.withInlineOneValueClasses
}

/** Behaviour found by running jsoniter-scala's own `JsonCodecMakerSpec` through both libraries
  * (`JsoniterUpstreamSpecOracleSpec` in upstream-oracle-tests compares them; these are the regression tests of the
  * fixes, with a config known at compile time and only at runtime).
  */
final class JsoniterScalaSpecParitySpec extends MacroSuite {

  private def runtime[A](config: JsoniterConfig)(derive: JsoniterConfig => JsonValueCodec[A]): JsonValueCodec[A] =
    derive(config)

  private def decodeError[A](codec: JsonValueCodec[A], json: String): String =
    intercept[JsonReaderException](
      readFromString[A](json, ReaderConfig.withAppendHexDumpToParseException(false))(codec)
    ).getMessage

  group("jsoniter-scala spec parity") {

    group("scala.Enumeration values are looked up at runtime (Spec: 'enumerations with values that are added ...')") {
      val byName = List[JsonValueCodec[SpsBodies]](
        KindlingsJsonValueCodec.derived[SpsBodies],
        runtime(SpsRuntimeConfigs.default)(implicit c => KindlingsJsonValueCodec.derived[SpsBodies])
      )
      val byId = List[JsonValueCodec[SpsBodies]](
        OptimizedCodecs.spsBodiesById,
        runtime(SpsRuntimeConfigs.ids)(implicit c => KindlingsJsonValueCodec.derived[SpsBodies])
      )

      test("private, lazy and later-added values, as values and as map keys") {
        byName.foreach { c =>
          val stable =
            SpsBodies(SpsBody.Sun, List(SpsBody.Moon, SpsBody.hidden, SpsBody.Halley), Map(SpsBody.hidden -> 1))
          val json = """{"main":"Sun","all":["Moon","Vesta","Halley"],"byBody":{"Vesta":1}}"""
          writeToString(stable)(c) ==> json
          readFromString[SpsBodies](json)(c) ==> stable
          decodeError(
            c,
            """{"main":"Ceres","all":[],"byBody":{}}"""
          ) ==> "illegal enum value \"Ceres\", offset: 0x0000000e"
          val ceres = SpsBody.extra("Ceres" + System.nanoTime())
          val added = SpsBodies(ceres, List(ceres), Map(ceres -> 2))
          readFromString[SpsBodies](writeToString(added)(c))(c) ==> added
          decodeError(c, """{"main":"Sun","all":[],"byBody":{"Eris":1}}""") ==>
            "illegal enum value \"Eris\", offset: 0x00000027"
          decodeError(c, """{"main":null,"all":[],"byBody":{}}""") ==> "expected '\"', offset: 0x00000008"
        }
      }

      test("ids, also of later-added values") {
        byId.foreach { c =>
          val value = SpsBodies(SpsBody.Moon, List(SpsBody.Sun), Map(SpsBody.Moon -> 1))
          writeToString(value)(c) ==> """{"main":1,"all":[0],"byBody":{"1":1}}"""
          readFromString[SpsBodies]("""{"main":1,"all":[0],"byBody":{"1":1}}""")(c) ==> value
          val pluto = SpsBody.extra("Pluto" + System.nanoTime())
          val added = SpsBodies(pluto, List(pluto), Map(pluto -> 2))
          readFromString[SpsBodies](writeToString(added)(c))(c) ==> added
          decodeError(c, """{"main":1000,"all":[],"byBody":{}}""") ==> "illegal enum value 1000, offset: 0x0000000b"
          decodeError(c, """{"main":1,"all":[],"byBody":{"1000":1}}""") ==>
            "illegal enum value \"1000\", offset: 0x00000023"
          decodeError(c, """{"main":null,"all":[],"byBody":{}}""") ==> "expected digit, offset: 0x00000008"
        }
      }

      test("negative ids") {
        val c = OptimizedCodecs.spsTemperaturesById
        writeToString(List(SpsTemperature.Cold, SpsTemperature.Hot))(c) ==> "[-1,1]"
        readFromString[List[SpsTemperature.Value]]("[-1,1]")(c) ==> List(SpsTemperature.Cold, SpsTemperature.Hot)
        decodeError(c, "[0]") ==> "illegal enum value 0, offset: 0x00000001"
      }
    }

    group("null at the top level (Spec: 'top-level Java primitives', 'top-level options')") {
      test("the boxes of primitives are read like primitives") {
        decodeError(KindlingsJsonValueCodec.derived[java.lang.Integer], "null") ==> "illegal number, offset: 0x00000000"
        decodeError(
          KindlingsJsonValueCodec.derived[java.lang.Boolean],
          "null"
        ) ==> "illegal boolean, offset: 0x00000000"
        readFromString[java.lang.Integer]("3")(KindlingsJsonValueCodec.derived[java.lang.Integer]) ==>
          java.lang.Integer.valueOf(3)
      }
      test("options report what they expect") {
        val c = KindlingsJsonValueCodec.derived[Option[Int]]
        readFromString[Option[Int]]("null")(c) ==> None
        decodeError(c, "no!!!") ==> "expected value or null, offset: 0x00000001"
        decodeError(KindlingsJsonValueCodec.derived[List[Option[Int]]], "[1,nul]") ==>
          "expected value or null, offset: 0x00000006"
      }
      test("a one-value class is read like its field with inlineOneValueClasses") {
        List[JsonValueCodec[UofName]](
          OptimizedCodecs.spsNameInlined,
          runtime(SpsRuntimeConfigs.inline)(implicit c => KindlingsJsonValueCodec.derived[UofName])
        ).foreach { c =>
          readFromString[UofName]("\"a\"")(c) ==> UofName("a")
          decodeError(c, "null") ==> "expected '\"', offset: 0x00000000"
        }
      }
    }

    group("codecs of case objects (Spec: 'product types without discriminators ...')") {
      test("any JSON object and null are the object, which is the nullValue") {
        val c = KindlingsJsonValueCodec.derived[SpsLeaf.type]
        writeToString[SpsLeaf.type](SpsLeaf)(c) ==> "{}"
        readFromString[SpsLeaf.type]("{}")(c) ==> SpsLeaf
        readFromString[SpsLeaf.type]("""{"a":[1,{"b":null}]}""")(c) ==> SpsLeaf
        readFromString[SpsLeaf.type]("null")(c) ==> SpsLeaf
        (c.nullValue: Any) ==> SpsLeaf
        decodeError(c, "1") ==> "expected '{' or null, offset: 0x00000000"
      }
    }

    group("bit sets (Spec: 'mutable & immutable bitsets')") {
      test("null is an empty bit set, also for scala.collection.BitSet fields") {
        val c = KindlingsJsonValueCodec.derived[SpsBitSets]
        readFromString[SpsBitSets]("""{"bs":null,"ibs":null,"mbs":null}""")(c) ==>
          SpsBitSets(
            scala.collection.BitSet.empty,
            scala.collection.immutable.BitSet.empty,
            scala.collection.mutable.BitSet.empty
          )
        decodeError(c, """{"bs":1}""") ==> "expected '[' or null, offset: 0x00000006"
      }
    }

    group("inlineOneValueClasses (Spec: 'one value classes ... inlining')") {
      test("a transient parameter whose default reads the field: the field is read once") {
        List[JsonValueCodec[List[SpsDependent]]](
          OptimizedCodecs.spsDependentInlined,
          runtime(SpsRuntimeConfigs.inline)(implicit c => KindlingsJsonValueCodec.derived[List[SpsDependent]])
        ).foreach { c =>
          val read = readFromString[List[SpsDependent]]("""["1234","ab"]""")(c)
          read.map(_.value) ==> List("1234", "ab")
          read.map(_.length) ==> List(4, 2)
          writeToString(read)(c) ==> """["1234","ab"]"""
        }
      }
    }

    group("names with operators (Spec: 'ADTs with Scala operators in names')") {
      test("ADT leaf names are the names written in the source") {
        val c = OptimizedCodecs.spsZoneWithDiscriminator
        writeToString[SpsZone](`Sps/Alaska`)(c) ==> """{"zoneId":"Sps/Alaska"}"""
        readFromString[SpsZone]("""{"zoneId":"Sps/Paris"}""")(c) ==> `Sps/Paris`
      }
    }
  }
}

final case class SpsBitSets(
    bs: scala.collection.BitSet,
    ibs: scala.collection.immutable.BitSet,
    mbs: scala.collection.mutable.BitSet
)
