package hearth.kindlings.circederivation

import hearth.MacroSuite
import hearth.kindlings.circederivation.internal.runtime.CirceDerivationUtils
import io.circe.{Decoder, Encoder, Json}

import scala.annotation.nowarn

/** The members kept (deprecated) for binary compatibility with Kindlings 0.3.x keep their 0.3.2 behaviour. */
@nowarn("cat=deprecation")
final class BinaryCompatSpec extends MacroSuite {

  private case class Point(x: Int, y: Int)
  private implicit val pointDecoder: Decoder[Point] = Decoder.forProduct2("x", "y")(Point.apply)
  private implicit val pointEncoder: Encoder[Point] = Encoder.forProduct2("x", "y")((p: Point) => (p.x, p.y))

  group("Configuration of 0.3.2") {

    test("the old-arity constructor and apply fill the new fields with their defaults") {
      val viaApply = Configuration(identity, identity, true, Some("t"), true, true)
      val viaNew = new Configuration(identity, identity, true, Some("t"), true, true)
      for (c <- List(viaApply, viaNew)) {
        c.useDefaults ==> true
        c.discriminator ==> Some("t")
        c.strictDecoding ==> true
        c.enumAsStrings ==> true
        c.unwrapValueClasses ==> true
      }
    }

    test("the old-arity copy keeps the new fields") {
      val c = Configuration.default.withoutValueClassUnwrapping.copy(identity, identity, true, None, false, false)
      c.useDefaults ==> true
      c.unwrapValueClasses ==> false
    }

    test("named copy still uses the full-arity copy") {
      Configuration.default.copy(unwrapValueClasses = false).unwrapValueClasses ==> false
      Configuration.default.copy(useDefaults = true) ==> Configuration(useDefaults = true)
    }
  }

  group("KindlingsDecoder.patch") {

    test("KindlingsDecoder.patch[A] selects the macro (a wrong type fails at decode time)") {
      val patch = KindlingsDecoder.patch[Point]
      assert(patch.decodeJson(Json.obj("x" -> Json.fromString("a"))).isLeft)
    }

    test("the 0.3.x patch (implicit decoder and encoder) deep-merges and keeps the original on failure") {
      // Its overload is not selectable by a `KindlingsDecoder.patch[A]` call, only through the parent trait.
      val patch = (KindlingsDecoder: KindlingsDecoderBinaryCompat).patch[Point]
      patch.decodeJson(Json.obj("y" -> Json.fromInt(5))).map(_(Point(1, 2))) ==> Right(Point(1, 5))
      patch.decodeJson(Json.obj("x" -> Json.fromString("a"))).map(_(Point(1, 2))) ==> Right(Point(1, 2))
      patch.decodeJson(Json.Null).map(_(Point(1, 2))) ==> Right(Point(1, 2))
    }
  }

  group("CirceDerivationUtils of 0.3.2") {

    test("strict decoding helpers") {
      val c = Json.obj("a" -> Json.fromInt(1), "b" -> Json.fromInt(2)).hcursor
      CirceDerivationUtils.checkStrictDecoding(c, Set("a", "b")) ==> Right(())
      CirceDerivationUtils.checkStrictDecoding(c, Set("a")).left.map(_.message) ==> Left("Unexpected field(s): b")
      CirceDerivationUtils.checkStrictDecodingWithDiscriminator(c, Set("a"), "b") ==> Right(())
      assert(CirceDerivationUtils.checkStrictDecodingAccumulating(c, Set("b")).isInvalid)
    }

    test("sum helpers") {
      val wrapped = Json.obj("A" -> Json.obj()).hcursor
      CirceDerivationUtils.decodeWrapped(wrapped).map(_._1) ==> Right("A")
      assert(CirceDerivationUtils.decodeWrapped(Json.obj().hcursor).isLeft)
      val disc = Json.obj("t" -> Json.fromString("B")).hcursor
      CirceDerivationUtils.decodeDiscriminator(disc, "t").map(_._1) ==> Right("B")
      CirceDerivationUtils.failedToMatchSubtype("C", disc, List("A", "B")).message ==>
        "Unknown type discriminator: C. Expected one of: A, B"
      CirceDerivationUtils.decodeEnumFromString(Json.fromString("A").hcursor, List("A"))(s => Right(s)) ==> Right("A")
      assert(CirceDerivationUtils.decodeEnumFromString(Json.fromInt(1).hcursor, List("A"))(s => Right(s)).isLeft)
    }
  }
}
