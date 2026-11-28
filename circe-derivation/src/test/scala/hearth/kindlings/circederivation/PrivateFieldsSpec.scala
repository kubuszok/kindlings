package hearth.kindlings.circederivation

import hearth.MacroSuite
import io.circe.Json

final case class CircePrivate(a: Int, private val b: String, c: Boolean) {
  def bValue: String = b
}

// Hearth's `caseFieldValuesAt` has no `private val` constructor parameters (they have no accessor the generated code could
// call): derivations which read fields include them anyway (through `Product.productElement`), matched by name, so the
// fields after them are not shifted.
final class PrivateFieldsSpec extends MacroSuite {

  test("private val constructor fields are encoded and decoded, as circe-generic does") {
    val value = CircePrivate(1, "two", c = true)
    val json = Json.obj("a" -> Json.fromInt(1), "b" -> Json.fromString("two"), "c" -> Json.True)
    KindlingsEncoder.encode(value) ==> json
    KindlingsDecoder.decode[CircePrivate](json) ==> Right(value)
    KindlingsDecoder.decode[CircePrivate](json).map(_.bValue) ==> Right("two")
  }
}
