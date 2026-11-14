package hearth.kindlings.ubjsonderivation

import hearth.MacroSuite
import hearth.kindlings.ubjsonderivation.internal.runtime.UBJsonDerivationUtils

/** An empty array/object for a collection/map field with a non-empty default value: the default (jsoniter-scala parity)
  * unless `withEmptyCollectionsAsEmpty`, as in the jsoniter derivation. Checked with a config known at compile time and
  * with one only known at runtime.
  */
final class UBJsonEmptyCollectionsSpec extends MacroSuite {

  private def roundTrip[A](value: A)(codec: UBJsonValueCodec[A]): A =
    UBJsonDerivationUtils.readFromBytes[A](UBJsonDerivationUtils.writeToBytes[A](value)(codec))(codec)

  private val allEmpty = UbEmptyCollDefaults(Nil, Map.empty, Vector.empty, Nil)

  private val asDefault = List(
    "compile-time" -> UBJsonValueCodec.derived[UbEmptyCollDefaults],
    "runtime" -> {
      implicit val config: UBJsonConfig = UbAbsentRuntimeConfigs.default
      UBJsonValueCodec.derived[UbEmptyCollDefaults]
    }
  )
  private val asEmpty = List(
    "compile-time" -> {
      implicit val config: UBJsonConfig = UBJsonConfig().withEmptyCollectionsAsEmpty
      UBJsonValueCodec.derived[UbEmptyCollDefaults]
    },
    "runtime" -> {
      implicit val config: UBJsonConfig = UbAbsentRuntimeConfigs.emptyAsEmpty
      UBJsonValueCodec.derived[UbEmptyCollDefaults]
    }
  )

  test("the flag defaults to jsoniter-scala's behaviour") {
    assertEquals(UBJsonConfig().emptyCollectionsAsDefault, true)
    assertEquals(UBJsonConfig().withEmptyCollectionsAsEmpty.emptyCollectionsAsDefault, false)
    assertEquals(
      UBJsonConfig().withEmptyCollectionsAsEmpty.withEmptyCollectionsAsDefault(true).emptyCollectionsAsDefault,
      true
    )
  }

  asDefault.foreach { case (path, codec) =>
    test(s"default ($path config): empty collections are read back as the defaults") {
      roundTrip(allEmpty)(codec) ==> UbEmptyCollDefaults(noDefault = Nil)
    }
  }

  asEmpty.foreach { case (path, codec) =>
    test(s"withEmptyCollectionsAsEmpty ($path config): empty collections round-trip") {
      roundTrip(allEmpty)(codec) ==> allEmpty
      val value = UbEmptyCollDefaults(List(2), Map("b" -> 2), Vector("x"), List(3))
      roundTrip(value)(codec) ==> value
    }
  }
}
