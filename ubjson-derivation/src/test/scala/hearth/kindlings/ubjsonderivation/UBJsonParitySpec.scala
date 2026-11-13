package hearth.kindlings.ubjsonderivation

import hearth.MacroSuite
import hearth.kindlings.ubjsonderivation.internal.runtime.UBJsonDerivationUtils

import scala.collection.immutable.ListMap

/** Behaviours shared with the jsoniter derivation, aligned with jsoniter-scala (see `JsoniterScalaParitySpec`). Each
  * config-dependent scenario runs with a config known at compile time and with one only known at runtime.
  */
final class UBJsonParitySpec extends MacroSuite {

  private val objectCodec: UBJsonValueCodec[ListMap[String, String]] =
    UBJsonValueCodec.derived[ListMap[String, String]]

  private def objectBytes(fields: (String, String)*): Array[Byte] =
    UBJsonDerivationUtils.writeToBytes[ListMap[String, String]](ListMap.from(fields))(objectCodec)

  private def writtenFields[A](value: A)(codec: UBJsonValueCodec[A]): ListMap[String, String] =
    UBJsonDerivationUtils.readFromBytes[ListMap[String, String]](UBJsonDerivationUtils.writeToBytes(value)(codec))(
      objectCodec
    )

  group("UBJson jsoniter-scala parity") {

    test("objects without fields skip unknown fields by default") {
      val zeroRuntime = {
        implicit val config: UBJsonConfig = UbParityRuntimeConfigs.default
        UBJsonValueCodec.derived[UbZeroFields]
      }
      List(UBJsonValueCodec.derived[UbZeroFields], zeroRuntime).foreach { c =>
        UBJsonDerivationUtils.readFromBytes[UbZeroFields](objectBytes("x" -> "1"))(c) ==> UbZeroFields()
      }
      val transientRuntime = {
        implicit val config: UBJsonConfig = UbParityRuntimeConfigs.default
        UBJsonValueCodec.derived[UbAllTransient]
      }
      List(UBJsonValueCodec.derived[UbAllTransient], transientRuntime).foreach { c =>
        UBJsonDerivationUtils.readFromBytes[UbAllTransient](objectBytes("x" -> "5"))(c) ==> UbAllTransient()
      }
    }

    test("objects without fields reject unknown fields when skipUnexpectedFields = false") {
      val static = {
        implicit val config: UBJsonConfig = UBJsonConfig().withSkipUnexpectedFields(false)
        UBJsonValueCodec.derived[UbZeroFields]
      }
      val runtime = {
        implicit val config: UBJsonConfig = UbParityRuntimeConfigs.strict
        UBJsonValueCodec.derived[UbZeroFields]
      }
      List(static, runtime).foreach { c =>
        val error =
          intercept[UBJsonReaderException](UBJsonDerivationUtils.readFromBytes[UbZeroFields](objectBytes("x" -> "1"))(c))
        assert(error.getMessage.contains("unexpected field"), error.getMessage)
      }
    }

    test("setMaxInsertNumber limits only sets") {
      val listBytes = UBJsonDerivationUtils.writeToBytes(List(1, 2, 3))(UBJsonValueCodec.derived[List[Int]])
      val static = {
        implicit val config: UBJsonConfig = UBJsonConfig().withSetMaxInsertNumber(2)
        (UBJsonValueCodec.derived[List[Int]], UBJsonValueCodec.derived[Set[Int]])
      }
      val runtime = {
        implicit val config: UBJsonConfig = UbParityRuntimeConfigs.setLimit2
        (UBJsonValueCodec.derived[List[Int]], UBJsonValueCodec.derived[Set[Int]])
      }
      List(static, runtime).foreach { case (lists, sets) =>
        UBJsonDerivationUtils.readFromBytes[List[Int]](listBytes)(lists) ==> List(1, 2, 3)
        val error = intercept[UBJsonReaderException](UBJsonDerivationUtils.readFromBytes[Set[Int]](listBytes)(sets))
        assert(error.getMessage.contains("too many set inserts"), error.getMessage)
      }
    }

    test("transientEmpty omits empty arrays, maps and collections but not empty strings") {
      val static = {
        implicit val config: UBJsonConfig = UBJsonConfig().withTransientEmpty
        UBJsonValueCodec.derived[UbEmptyCapable]
      }
      val runtime = {
        implicit val config: UBJsonConfig = UbParityRuntimeConfigs.transientEmpty
        UBJsonValueCodec.derived[UbEmptyCapable]
      }
      List(static, runtime).foreach { c =>
        writtenFields(UbEmptyCapable(Array.empty[Int], "", Map.empty, Nil))(c) ==> ListMap("s" -> "")
      }
    }

    test("transientDefault compares array contents with the default") {
      val static = {
        implicit val config: UBJsonConfig = UBJsonConfig().withTransientDefault
        UBJsonValueCodec.derived[UbArrayDefault]
      }
      val runtime = {
        implicit val config: UBJsonConfig = UbParityRuntimeConfigs.transientDefault
        UBJsonValueCodec.derived[UbArrayDefault]
      }
      List(static, runtime).foreach { c =>
        writtenFields(UbArrayDefault())(c) ==> ListMap.empty[String, String]
      }
    }
  }
}
