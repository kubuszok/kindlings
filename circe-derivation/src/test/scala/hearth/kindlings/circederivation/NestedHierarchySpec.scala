package hearth.kindlings.circederivation

import hearth.MacroSuite
import io.circe.{Decoder, Encoder, Json}

/** A sealed hierarchy with a case object in the companion of the sealed trait, nested in an object and in a class.
  *
  * The class-nested variant (as in a test suite or a `.sc` script) crashed the Scala 3 compiler with "missing outer
  * accessor" on Hearth 0.4.5, which typed the case object as `Shape.this.Empty$` (fixed in Hearth 985e5e6).
  *
  * Only the ENCODER is derived for the class-nested variant: on Scala 2 a decoder (which refers to the case object's
  * value) still fails with "unable to find the outer accessor symbol", because Hearth's `Expr.singletonOf` refers to
  * the case object as `InnerShape.this.Empty` - see `docs/research/hearth-bug-scala2-singleton-this-prefix.md`. When
  * that is fixed, derive and test the decoder here too.
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

    test("encodes") {
      val encoder: Encoder[InnerShape] = KindlingsEncoder.derived[InnerShape]
      encoder(InnerShape.Empty) ==> Json.obj("Empty" -> Json.obj())
      encoder(InnerCircle(1.0)) ==> Json.obj("InnerCircle" -> Json.obj("r" -> Json.fromDoubleOrNull(1.0)))
    }
  }
}
