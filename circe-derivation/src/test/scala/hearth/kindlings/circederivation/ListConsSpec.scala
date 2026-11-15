package hearth.kindlings.circederivation

import hearth.MacroSuite
import io.circe.Json

final case class CiConsBox(xs: ::[Int])

/** `::` (a non-empty `List`) is a collection (Hearth's `IsCollection` provider): a JSON array, rejected when empty. */
final class ListConsSpec extends MacroSuite {

  group("::") {

    test("is encoded as a JSON array and decoded back") {
      val value = CiConsBox(::(1, List(2, 3)))
      val json = KindlingsEncoder.encode(value)
      json ==> Json.obj("xs" -> Json.arr(Json.fromInt(1), Json.fromInt(2), Json.fromInt(3)))
      KindlingsDecoder.decode[CiConsBox](json) ==> Right(value)
    }

    test("an empty JSON array is rejected") {
      assert(KindlingsDecoder.decode[CiConsBox](Json.obj("xs" -> Json.arr())).isLeft)
    }
  }
}
