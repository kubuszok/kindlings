package hearth.kindlings.circederivation

import hearth.MacroSuite
import io.circe.{Decoder, Encoder, Json}

/** A sealed hierarchy with a case object in the companion of the sealed trait, nested in an object and in a class.
  *
  * The class-nested variant (as in a test suite or a `.sc` script) crashed the Scala 3 compiler with "missing outer
  * accessor" on Hearth 0.4.5, which typed the case object as `Shape.this.Empty$` - see
  * `docs/research/hearth-bug-enum-child-this-type-prefix.md` (fixed in Hearth 985e5e6).
  */
object NestedHierarchyFixtures {
  sealed trait Shape
  object Shape {
    case object Empty extends Shape
  }
  final case class Circle(r: Double) extends Shape
}

final class NestedHierarchySpec extends MacroSuite {

  sealed trait InnerShape
  object InnerShape {
    case object Empty extends InnerShape
  }
  final case class InnerCircle(r: Double) extends InnerShape

  group("sealed hierarchy nested in an object, case object in the companion") {

    test("encodes and decodes") {
      import NestedHierarchyFixtures.*
      val encoder: Encoder[Shape] = KindlingsEncoder.derived[Shape]
      val decoder: Decoder[Shape] = KindlingsDecoder.derived[Shape]
      val empty: Shape = Shape.Empty
      val circle: Shape = Circle(1.0)
      encoder(empty) ==> Json.obj("Empty" -> Json.obj())
      decoder.decodeJson(encoder(empty)) ==> Right(empty)
      decoder.decodeJson(encoder(circle)) ==> Right(circle)
    }
  }

  group("sealed hierarchy nested in a class, case object in the companion") {

    test("encodes and decodes") {
      val encoder: Encoder[InnerShape] = KindlingsEncoder.derived[InnerShape]
      val decoder: Decoder[InnerShape] = KindlingsDecoder.derived[InnerShape]
      val empty: InnerShape = InnerShape.Empty
      val circle: InnerShape = InnerCircle(1.0)
      encoder(empty) ==> Json.obj("Empty" -> Json.obj())
      decoder.decodeJson(encoder(empty)) ==> Right(empty)
      decoder.decodeJson(encoder(circle)) ==> Right(circle)
    }
  }
}
