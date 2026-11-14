package hearth.kindlings.circederivation

import hearth.MacroSuite
import io.circe.Json

// Opaque types are defined in their own object, so that outside of it they are not dealiased.
object CirceOpaqueMapKeys {
  opaque type Name = String
  object Name {
    def apply(value: String): Name = value
  }

  opaque type Counter = Int
  object Counter {
    def apply(value: Int): Counter = value
  }
}
import CirceOpaqueMapKeys.*

final case class CirceByOpaqueName(byName: Map[Name, Int])
final case class CirceByOpaqueCounter(byCounter: Map[Counter, String])

/** Maps keyed by opaque types are encoded as JSON objects keyed by the underlying value's string form. */
final class OpaqueMapKeyCodecSpec extends MacroSuite {

  group("maps keyed by opaque types") {

    test("opaque type over String") {
      val value = CirceByOpaqueName(Map(Name("a") -> 1))
      val json = Json.obj("byName" -> Json.obj("a" -> Json.fromInt(1)))
      KindlingsEncoder.encode(value) ==> json
      KindlingsDecoder.decode[CirceByOpaqueName](json) ==> Right(value)
    }

    test("opaque type over Int") {
      val value = CirceByOpaqueCounter(Map(Counter(7) -> "x"))
      val json = Json.obj("byCounter" -> Json.obj("7" -> Json.fromString("x")))
      KindlingsEncoder.encode(value) ==> json
      KindlingsDecoder.decode[CirceByOpaqueCounter](json) ==> Right(value)
    }

    test("a non-numeric key of an opaque type over Int is rejected") {
      val json = Json.obj("byCounter" -> Json.obj("x" -> Json.fromString("y")))
      assert(KindlingsDecoder.decode[CirceByOpaqueCounter](json).isLeft)
    }
  }
}
