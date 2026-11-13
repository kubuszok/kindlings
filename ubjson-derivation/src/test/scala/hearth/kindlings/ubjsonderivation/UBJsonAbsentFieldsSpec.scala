package hearth.kindlings.ubjsonderivation

import hearth.MacroSuite
import hearth.kindlings.ubjsonderivation.internal.runtime.UBJsonDerivationUtils

import scala.collection.immutable.ListMap

/** Absent fields decode the same way as in the jsoniter derivation: `None` for `Option`, the default value, an empty
  * collection, otherwise `missing required field`; the transient* flags only affect encoding.
  */
final class UBJsonAbsentFieldsSpec extends MacroSuite {

  private val objectCodec: UBJsonValueCodec[ListMap[String, String]] =
    UBJsonValueCodec.derived[ListMap[String, String]]

  private def read[A](fields: (String, String)*)(codec: UBJsonValueCodec[A]): A =
    UBJsonDerivationUtils.readFromBytes[A](
      UBJsonDerivationUtils.writeToBytes[ListMap[String, String]](ListMap.from(fields))(objectCodec)
    )(codec)

  private def missingField(name: String)(thunk: => Any): Unit = {
    val error = intercept[UBJsonReaderException](thunk)
    assert(error.getMessage.contains(s"""missing required field "$name""""), error.getMessage)
  }

  group("UBJson absent fields") {
    val codec = UBJsonValueCodec.derived[UbAbsentRecord]
    val codecRuntime = {
      implicit val config: UBJsonConfig = UbAbsentRuntimeConfigs.default
      UBJsonValueCodec.derived[UbAbsentRecord]
    }

    test("Option, defaulted and collection fields absent") {
      List(codec, codecRuntime).foreach { c =>
        read("name" -> "a")(c) ==> UbAbsentRecord("a", None, 7, Nil, Vector.empty, Map.empty)
      }
    }

    test("missing required field fails") {
      List(codec, codecRuntime).foreach { c =>
        missingField("name")(read("opt" -> "x")(c))
      }
    }

    test("transient flags do not change decoding") {
      implicit val config: UBJsonConfig = UBJsonConfig().withTransientNone.withTransientDefault.withTransientEmpty
      val c = UBJsonValueCodec.derived[UbAbsentRecord]
      read("name" -> "a")(c) ==> UbAbsentRecord("a", None, 7, Nil, Vector.empty, Map.empty)
      missingField("name")(read("opt" -> "x")(c))
    }

    test("requireDefaultFields") {
      val c = {
        implicit val config: UBJsonConfig = UBJsonConfig().withRequireDefaultFields
        UBJsonValueCodec.derived[UbAbsentRecord]
      }
      val cRuntime = {
        implicit val config: UBJsonConfig = UbAbsentRuntimeConfigs.requireDefault
        UBJsonValueCodec.derived[UbAbsentRecord]
      }
      List(c, cRuntime).foreach { codec =>
        missingField("n")(read("name" -> "a")(codec))
      }
      val cOpt = {
        implicit val config: UBJsonConfig = UBJsonConfig().withRequireDefaultFields
        UBJsonValueCodec.derived[UbAbsentOptionWithNoneDefault]
      }
      read()(cOpt) ==> UbAbsentOptionWithNoneDefault(None)
    }

    test("requireCollectionFields") {
      implicit val config: UBJsonConfig = UBJsonConfig().withRequireCollectionFields
      val c = UBJsonValueCodec.derived[UbAbsentRecord]
      missingField("ys")(read("name" -> "a")(c))
    }

    test("discriminated leaf") {
      implicit val config: UBJsonConfig = UBJsonConfig().withDiscriminator("type")
      val c = UBJsonValueCodec.derived[UbAbsentAdt]
      read("type" -> "UbAbsentLeaf", "name" -> "a")(c) ==> (UbAbsentLeaf("a", None, 3, Nil): UbAbsentAdt)
      missingField("name")(read("type" -> "UbAbsentLeaf")(c))
    }
  }
}
