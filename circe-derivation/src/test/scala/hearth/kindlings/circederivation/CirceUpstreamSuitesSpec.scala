package hearth.kindlings.circederivation

import cats.data.Validated
import hearth.MacroSuite
import io.circe.{Codec, Decoder, DecodingFailure, Encoder, Json}
import io.circe.CursorOp.DownField

// Fixtures of upstream's own test suites (prefixed `Up`); the cases the upstream oracle cannot express (call counts,
// compile-time errors, instances in scope, exact results upstream's tests assert).

sealed trait UpConfigExampleBase
final case class UpConfigExampleFoo(thisIsAField: String, a: Int = 0, b: Double) extends UpConfigExampleBase
case object UpConfigExampleBar extends UpConfigExampleBase

final case class UpQux[A](i: Int, a: A, j: Int)

sealed trait UpExtendedCardinalDirection
case object UpNorth2 extends UpExtendedCardinalDirection
case object UpSouth2 extends UpExtendedCardinalDirection
final case class UpNotACardinalDirectionAtAll(x: String) extends UpExtendedCardinalDirection

/** circe-generic `AutoDerivedSuite`: members of tagged (intersection) types, with instances for the tagged types. */
object UpTagged {
  trait Tag1
  trait Tag2
  type @@[A, T] = A with T
  def tag[T]: Tagger[T] = new Tagger[T]
  final class Tagger[T] { def apply[A](a: A): A @@ T = a.asInstanceOf[A @@ T] }

  final case class WithTaggedMembers(i: List[Int] @@ Tag1, s: String @@ Tag2)
  final case class WithSeqOfTagged(s: Vector[String @@ Tag2])

  implicit val encodeIntTag1: Encoder[List[Int] @@ Tag1] = Encoder[List[Int]].contramap(x => x)
  implicit val encodeStringTag2: Encoder[String @@ Tag2] = Encoder[String].contramap(x => x)
  implicit val decodeIntTag1: Decoder[List[Int] @@ Tag1] = Decoder[List[Int]].map(tag[Tag1](_))
  implicit val decodeStringTag2: Decoder[String @@ Tag2] = Decoder[String].map(tag[Tag2](_))
}

/** circe-generic `AutoDerivedSuite` "Generic decoders/encoders should not interfere with defined decoders/encoders". */
final case class UpInnerCaseClassExample(a: String, b: String, c: String, d: String)
final case class UpOuterCaseClassExample(a: String, inner: UpInnerCaseClassExample)

final class CirceUpstreamSuitesSpec extends MacroSuite {

  group("upstream test suites") {

    // generic-extras ConfiguredSemiautoDerivedSuite "it should call field modification times equal to field count":
    // the name transformations run once per instance, not once per encoded/decoded value.
    test("name transformations of a runtime configuration run a fixed number of times, not per call") {
      var transformMemberNamesCallCount, transformConstructorCallCount = 0
      implicit val customConfig: Configuration =
        Configuration.default.copy(
          transformMemberNames = v => {
            transformMemberNamesCallCount = transformMemberNamesCallCount + 1
            renaming.snakeCase(v)
          },
          transformConstructorNames = v => {
            transformConstructorCallCount = transformConstructorCallCount + 1
            renaming.snakeCase(v)
          }
        )
      val encoder: Encoder[UpConfigExampleBase] = KindlingsEncoder.derived[UpConfigExampleBase]
      val decoder: Decoder[UpConfigExampleBase] = KindlingsDecoder.derived[UpConfigExampleBase]
      val foo: UpConfigExampleBase = UpConfigExampleFoo("field_value", 0, 100)
      val first = encoder(foo)
      first ==> Json.obj(
        "up_config_example_foo" -> Json.obj(
          "this_is_a_field" -> Json.fromString("field_value"),
          "a" -> Json.fromInt(0),
          "b" -> Json.fromDoubleOrNull(100)
        )
      )
      decoder.decodeJson(first) ==> Right(foo)
      decoder.decodeJson(encoder(UpConfigExampleBar)) ==> Right(UpConfigExampleBar)
      val memberCalls = transformMemberNamesCallCount
      val constructorCalls = transformConstructorCallCount
      // upstream counts 3 fields on each side (6) and 2 constructors to decode + 1 written; Kindlings builds its name
      // tables per instance too (how many times each name is transformed while doing so is not part of the contract)
      assert(memberCalls <= 6, s"member names transformed $memberCalls times")
      assert(constructorCalls <= 6, s"constructor names transformed $constructorCalls times")
      for (_ <- 1 until 100) {
        decoder.decodeJson(encoder(foo)) ==> Right(foo)
        decoder.decodeJson(encoder(UpConfigExampleBar)) ==> Right(UpConfigExampleBar)
      }
      transformMemberNamesCallCount ==> memberCalls
      transformConstructorCallCount ==> constructorCalls
    }

    // generic-extras ConfiguredSemiautoDerivedSuite "it should support configured strict decoding with decodeStrict"
    // (also "... on a configured codec"): the extraneous fields include `null`-valued ones, in JSON order.
    test("decodeStrict reports the failure and every extraneous field") {
      implicit val customConfig: Configuration =
        Configuration.default.withSnakeCaseMemberNames.withDefaults
          .withDiscriminator("type_field")
          .withSnakeCaseConstructorNames
          .withStrictDecoding
      val json = Json.obj(
        "type_field" -> Json.fromString("config_example_foo"),
        "this_is_a_field" -> Json.fromString("f"),
        "stowaway_field" -> Json.fromString("I should not be here"),
        "b" -> Json.fromDoubleOrNull(2.5),
        "also_bad_but_null_valued" -> Json.Null
      )
      val expectedExtraneous = List("stowaway_field", "also_bad_but_null_valued")
      val decoder: ExtrasDecoder[UpConfigExampleFoo] = KindlingsDecoder.derived[UpConfigExampleFoo]
      val Left((failure, extraneous)) = decoder.decodeStrict(json.hcursor): @unchecked
      extraneous ==> expectedExtraneous
      // upstream (Scala 3 core wording; generic-extras on Scala 2 says "Unexpected field: [stowaway_field]; valid
      // fields: this_is_a_field, a, b, type_field" — documented difference in wording)
      assert(failure.message.contains("stowaway_field"), failure.message)
      assert(failure.message.contains("valid fields: this_is_a_field, a, b, type_field"), failure.message)
      val codec: ExtrasDecoder[UpConfigExampleFoo] = KindlingsCodecAsObject.derived[UpConfigExampleFoo]
      codec.decodeStrict(json.hcursor).left.map(_._2) ==> Left(expectedExtraneous)
    }

    // circe-generic SemiautoDerivedSuite "Decoder[Int => Qux[String]] should return as many errors as invalid elements
    // in a partial case class"
    test("an incomplete decoder accumulates one error per missing field") {
      implicit val config: Configuration = Configuration.default
      val decoded = KindlingsDecoder.incomplete[Int => UpQux[String]].decodeAccumulating(Json.obj().hcursor)
      decoded.fold(_.tail.size + 1, _ => 0) ==> 2
    }

    // generic-extras EnumerationSemiautoDerivedSuite "deriveEnumerationDecoder/Encoder should not compile on an ADT
    // with case classes" (Scala 3: `Enum "X" contains non singleton case "Y"`).
    test("an enumeration codec for an ADT with case classes does not compile") {
      // (the configuration is written inline so that it is known at compile time on Scala 2 too; one only known at
      // runtime fails at runtime, see CirceParityBacklogSpec)
      compileErrors(
        """
        import hearth.kindlings.circederivation.{Configuration, KindlingsDecoder, UpExtendedCardinalDirection}
        KindlingsDecoder.derived[UpExtendedCardinalDirection](using Configuration.default.withEnumAsStrings)
        """
      ).check("enumAsStrings is enabled but", "UpNotACardinalDirectionAtAll")
      compileErrors(
        """
        import hearth.kindlings.circederivation.{Configuration, KindlingsEncoder, UpExtendedCardinalDirection}
        KindlingsEncoder.derived[UpExtendedCardinalDirection](using Configuration.default.withEnumAsStrings)
        """
      ).check("enumAsStrings is enabled but", "UpNotACardinalDirectionAtAll")
    }

    // circe-generic AutoDerivedSuite `WithTaggedMembers` / `WithSeqOfTagged`, circe-core DerivesSuite
    // `ProductWithTaggedMember` (#2135): the instances of the tagged types are used.
    test("members of tagged (intersection) types use the instances provided for them") {
      import UpTagged.*
      implicit val config: Configuration = Configuration.default
      val value = WithTaggedMembers(tag[Tag1](List(1, 2)), tag[Tag2]("s"))
      val json = Json.obj("i" -> Json.arr(Json.fromInt(1), Json.fromInt(2)), "s" -> Json.fromString("s"))
      KindlingsEncoder.derived[WithTaggedMembers].apply(value) ==> json
      KindlingsDecoder.derived[WithTaggedMembers].decodeJson(json) ==> Right(value)
      val seq = WithSeqOfTagged(Vector(tag[Tag2]("a"), tag[Tag2]("b")))
      val seqJson = Json.obj("s" -> Json.arr(Json.fromString("a"), Json.fromString("b")))
      KindlingsEncoder.derived[WithSeqOfTagged].apply(seq) ==> seqJson
      KindlingsDecoder.derived[WithSeqOfTagged].decodeJson(seqJson) ==> Right(seq)
    }

    // circe-generic AutoDerivedSuite "Generic decoders/encoders should not interfere with defined decoders/encoders"
    test("instances in scope are used instead of derived ones") {
      implicit val config: Configuration = Configuration.default
      implicit val decodeInner: Decoder[UpInnerCaseClassExample] =
        Decoder.instance(_.as[String].map(s => UpInnerCaseClassExample(s, "b", "c", "d")))
      implicit val encodeInner: Encoder[UpInnerCaseClassExample] = Encoder.instance(v => Json.fromString(v.a))
      val json = Json.obj("a" -> Json.fromString("x"), "inner" -> Json.fromString("i"))
      val value = UpOuterCaseClassExample("x", UpInnerCaseClassExample("i", "b", "c", "d"))
      KindlingsDecoder.derived[UpOuterCaseClassExample].decodeJson(json) ==> Right(value)
      KindlingsEncoder.derived[UpOuterCaseClassExample].apply(value) ==> json
    }

    // circe-generic AutoDerivedSuite "not be derived for Object / AnyRef"
    test("no instance is derived for Object / AnyRef") {
      assert(compileErrors("hearth.kindlings.circederivation.KindlingsDecoder.derived[Object]").nonEmpty)
      assert(compileErrors("hearth.kindlings.circederivation.KindlingsEncoder.derived[AnyRef]").nonEmpty)
    }

    // circe-core SemiautoDerivationSuite `testLocalCaseClasses` / `testLocalAdts` (circe/circe#2278): local types
    // whose instances are strict `implicit val`s of their companions.
    test("local case classes and ADTs with strict implicit vals in their companions") {
      implicit val config: Configuration = Configuration.default
      case class LocalCaseClass1(int: Int)
      object LocalCaseClass1 {
        implicit val decoder: Decoder[LocalCaseClass1] = KindlingsDecoder.derived[LocalCaseClass1]
        implicit val encoder: Encoder.AsObject[LocalCaseClass1] = KindlingsEncoder.deriveAsObject[LocalCaseClass1]
      }
      case class LocalCaseClass3(int: Int)
      object LocalCaseClass3 {
        implicit val codec: Codec.AsObject[LocalCaseClass3] = KindlingsCodecAsObject.derived[LocalCaseClass3]
      }
      sealed trait LocalAdt1
      object LocalAdt1 {
        case class Class(int: Int) extends LocalAdt1
        case object Object extends LocalAdt1
        implicit val decoder: Decoder[LocalAdt1] = KindlingsDecoder.derived[LocalAdt1]
        implicit val encoder: Encoder.AsObject[LocalAdt1] = KindlingsEncoder.deriveAsObject[LocalAdt1]
      }
      sealed trait LocalAdt3
      object LocalAdt3 {
        case class Class(int: Int) extends LocalAdt3
        case object Object extends LocalAdt3
        implicit val codec: Codec.AsObject[LocalAdt3] = KindlingsCodecAsObject.derived[LocalAdt3]
      }
      Decoder[LocalCaseClass1].decodeJson(Encoder[LocalCaseClass1].apply(LocalCaseClass1(1))) ==> Right(
        LocalCaseClass1(1)
      )
      Decoder[LocalCaseClass3].decodeJson(Encoder[LocalCaseClass3].apply(LocalCaseClass3(3))) ==> Right(
        LocalCaseClass3(3)
      )
      List[LocalAdt1](LocalAdt1.Class(1), LocalAdt1.Object).foreach { v =>
        Decoder[LocalAdt1].decodeJson(Encoder[LocalAdt1].apply(v)) ==> Right(v)
      }
      Encoder[LocalAdt1].apply(LocalAdt1.Object) ==> Json.obj("Object" -> Json.obj())
      List[LocalAdt3](LocalAdt3.Class(1), LocalAdt3.Object).foreach { v =>
        Decoder[LocalAdt3].decodeJson(Encoder[LocalAdt3].apply(v)) ==> Right(v)
      }
    }

    // circe-core ConfiguredDerivesSuite "Option[T] with default should fail to decode if type in json is not correct"
    // and "Field with default should fail to decode it type in json is not correct" (exact failures)
    test("a field with a default still fails on a value of the wrong type, with the field in the history") {
      implicit val config: Configuration = Configuration.default.withDefaults
      final case class FooWithDefault(a: Option[Int] = Some(0), b: String = "b")
      val decoder = KindlingsDecoder.derived[FooWithDefault]
      val wrongA = Json.obj("a" -> Json.fromString("NotAnInt"))
      val failureA = DecodingFailure("Int", List(DownField("a")))
      decoder.decodeJson(wrongA).left.map(f => (f.message, f.history)) ==> Left((failureA.message, failureA.history))
      decoder.decodeAccumulating(wrongA.hcursor).leftMap(_.map(f => (f.message, f.history)).toList) ==>
        Validated.invalid(List((failureA.message, failureA.history)))
      val wrongB = Json.obj("b" -> Json.fromInt(25))
      val failureB =
        DecodingFailure(DecodingFailure.Reason.WrongTypeExpectation("string", Json.fromInt(25)), List(DownField("b")))
      decoder.decodeJson(wrongB) ==> Left(failureB)
      decoder.decodeAccumulating(wrongB.hcursor) ==> Validated.invalidNel(failureB)
    }
  }
}
