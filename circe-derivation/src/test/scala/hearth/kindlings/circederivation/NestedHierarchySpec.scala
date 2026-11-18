package hearth.kindlings.circederivation

import hearth.MacroSuite
import io.circe.{Decoder, Encoder, Json}

/** A sealed hierarchy nested in an object, with a case object in the companion of the sealed trait.
  *
  * The same hierarchy declared inside a CLASS (as in a test suite or a `.sc` script) crashes the Scala 3 compiler with
  * "missing outer accessor" because Hearth types the case object as `Shape.this.Empty$` - see
  * `docs/research/hearth-bug-enum-child-this-type-prefix.md`. That variant cannot be kept here as a failing test (a
  * compiler crash fails every test of the module); when Hearth is fixed, move `NestedHierarchyFixtures`' members into
  * this class.
  */
object NestedHierarchyFixtures {
  sealed trait Shape
  object Shape {
    case object Empty extends Shape
  }
  final case class Circle(r: Double) extends Shape
}

final class NestedHierarchySpec extends MacroSuite {
  import NestedHierarchyFixtures.*

  group("sealed hierarchy nested in an object, case object in the companion") {

    test("encodes and decodes") {
      val encoder: Encoder[Shape] = KindlingsEncoder.derived[Shape]
      val decoder: Decoder[Shape] = KindlingsDecoder.derived[Shape]
      val empty: Shape = Shape.Empty
      val circle: Shape = Circle(1.0)
      encoder(empty) ==> Json.obj("Empty" -> Json.obj())
      decoder.decodeJson(encoder(empty)) ==> Right(empty)
      decoder.decodeJson(encoder(circle)) ==> Right(circle)
    }
  }
}
