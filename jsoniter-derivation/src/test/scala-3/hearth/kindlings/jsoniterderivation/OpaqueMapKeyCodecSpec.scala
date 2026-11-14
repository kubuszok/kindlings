package hearth.kindlings.jsoniterderivation

import com.github.plokhotnyuk.jsoniter_scala.core.{readFromString, writeToString, JsonValueCodec}
import hearth.MacroSuite

// Opaque types are defined in their own object, so that outside of it they are not dealiased.
object JsoniterOpaqueMapKeys {
  opaque type Name = String
  object Name {
    def apply(value: String): Name = value
  }

  opaque type Counter = Int
  object Counter {
    def apply(value: Int): Counter = value
  }

  opaque type Big = Long
  object Big {
    def apply(value: Long): Big = value
  }
}
import JsoniterOpaqueMapKeys.*

final case class ByOpaqueName(byName: Map[Name, Int])
final case class ByOpaqueCounter(byCounter: Map[Counter, String])
final case class ByOpaqueLong(byBig: Map[Big, Boolean])

/** Maps keyed by opaque types are written as JSON objects keyed by the underlying value's string form. */
final class OpaqueMapKeyCodecSpec extends MacroSuite {

  private def roundTrip[A](value: A, expectedJson: String)(using codec: JsonValueCodec[A]): Unit = {
    val json = writeToString(value)
    json ==> expectedJson
    readFromString[A](json) ==> value
  }

  group("maps keyed by opaque types") {

    test("opaque type over String") {
      given JsonValueCodec[ByOpaqueName] = KindlingsJsonValueCodec.derived
      roundTrip(ByOpaqueName(Map(Name("a") -> 1, Name("b") -> 2)), """{"byName":{"a":1,"b":2}}""")
    }

    test("opaque type over Int") {
      given JsonValueCodec[ByOpaqueCounter] = KindlingsJsonValueCodec.derived
      roundTrip(ByOpaqueCounter(Map(Counter(7) -> "x", Counter(-1) -> "y")), """{"byCounter":{"7":"x","-1":"y"}}""")
    }

    test("opaque type over Long") {
      given JsonValueCodec[ByOpaqueLong] = KindlingsJsonValueCodec.derived
      roundTrip(ByOpaqueLong(Map(Big(10000000000L) -> true)), """{"byBig":{"10000000000":true}}""")
    }

    test("a non-numeric key of an opaque type over Int is rejected") {
      given JsonValueCodec[ByOpaqueCounter] = KindlingsJsonValueCodec.derived
      assert(scala.util.Try(readFromString[ByOpaqueCounter]("""{"byCounter":{"x":"y"}}""")).isFailure)
    }
  }
}
