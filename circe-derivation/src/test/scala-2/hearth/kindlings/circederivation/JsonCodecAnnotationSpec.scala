package hearth.kindlings.circederivation

import hearth.MacroSuite
import io.circe.{Decoder, Encoder, Json}

// Scala 2 only: macro annotations (-Ymacro-annotations); Scala 3 uses `derives`.
@JsonCodec case class AnnPerson(firstName: String, age: Int)
@JsonCodec sealed trait AnnShape
@JsonCodec case class AnnCircle(r: Double) extends AnnShape
@JsonCodec(encodeOnly = true) case class AnnEncOnly(a: Int)
@JsonCodec(decodeOnly = true) case class AnnDecOnly(a: Int)
@JsonCodec case class AnnBox[A](value: A)
@JsonCodec case class AnnWithCompanion(a: Int)
object AnnWithCompanion { val extra: Int = 1 }

object AnnConfigured {
  implicit val config: Configuration = Configuration.default.withSnakeCaseMemberNames
  @ConfiguredJsonCodec case class AnnSnake(firstName: String)
  @JsonCodec case class AnnIgnoresConfig(firstName: String)
}

// Fixtures of circe-generic's JsonCodecMacrosSuite and generic-extras' ConfiguredJsonCodecSuite /
// ConfiguredJsonCodecWithKeySuite.
@JsonCodec final case class AnnSingle(i: Int)
@JsonCodec final case class AnnTyped2[A, B](i: Int, a: A, b: B, j: Int)
@JsonCodec final private[circederivation] case class AnnAccessModifier(a: Int)
@JsonCodec sealed trait AnnHierarchy
final case class AnnHierarchy1(i: Int, s: String) extends AnnHierarchy
final case class AnnHierarchy2(xs: List[String]) extends AnnHierarchy
final case class AnnHierarchy3(s: AnnSingle, d: Double) extends AnnHierarchy
@JsonCodec sealed trait AnnRecursiveHierarchy
final case class AnnBaseRecursiveHierarchy(a: String) extends AnnRecursiveHierarchy
final case class AnnNestedRecursiveHierarchy(r: AnnRecursiveHierarchy) extends AnnRecursiveHierarchy
@JsonCodec final case class AnnSelfRecursiveWithOption(o: Option[AnnSelfRecursiveWithOption])

object AnnUpstreamConfigured {
  implicit val customConfig: Configuration =
    Configuration.default.withSnakeCaseMemberNames.withDefaults.withDiscriminator("type").withSnakeCaseConstructorNames
  @ConfiguredJsonCodec sealed trait ConfigExampleBase
  case class ConfigExampleFoo(thisIsAField: String, a: Int = 0, b: Double) extends ConfigExampleBase
  // (generic-extras: `@JsonKey("myField")`)
  @ConfiguredJsonCodec sealed trait ConfigExampleWithKeyBase
  case class ConfigExampleWithKeyFoo(thisIsAField: String, a: Int = 0, @annotations.fieldName("myField") b: Double)
      extends ConfigExampleWithKeyBase
  @ConfiguredJsonCodec final private[circederivation] case class AccessModifier(a: Int)
  @ConfiguredJsonCodec case class GenericExample[A](a: A, b: Int)
}

final class JsonCodecAnnotationSpec extends MacroSuite {

  group("@JsonCodec / @ConfiguredJsonCodec (Scala 2 macro annotations)") {

    test("codec for a case class and a sealed trait") {
      val json = Json.obj("firstName" -> Json.fromString("a"), "age" -> Json.fromInt(1))
      Encoder[AnnPerson].apply(AnnPerson("a", 1)) ==> json
      Decoder[AnnPerson].decodeJson(json) ==> Right(AnnPerson("a", 1))
      val shape: AnnShape = AnnCircle(1.0)
      Decoder[AnnShape].decodeJson(Encoder[AnnShape].apply(shape)) ==> Right(shape)
      assert(AnnWithCompanion.extra == 1)
      Encoder[AnnWithCompanion].apply(AnnWithCompanion(2)) ==> Json.obj("a" -> Json.fromInt(2))
    }

    test("encodeOnly / decodeOnly") {
      Encoder[AnnEncOnly].apply(AnnEncOnly(1)) ==> Json.obj("a" -> Json.fromInt(1))
      Decoder[AnnDecOnly].decodeJson(Json.obj("a" -> Json.fromInt(1))) ==> Right(AnnDecOnly(1))
      compileErrors("io.circe.Decoder[hearth.kindlings.circederivation.AnnEncOnly]").check("could not find implicit")
    }

    test("type parameters") {
      Encoder[AnnBox[Int]].apply(AnnBox(1)) ==> Json.obj("value" -> Json.fromInt(1))
      Decoder[AnnBox[String]].decodeJson(Json.obj("value" -> Json.fromString("x"))) ==> Right(AnnBox("x"))
    }

    test("@ConfiguredJsonCodec uses the configuration in scope, @JsonCodec does not") {
      import AnnConfigured.*
      Encoder[AnnSnake].apply(AnnSnake("a")) ==> Json.obj("first_name" -> Json.fromString("a"))
      Encoder[AnnIgnoresConfig].apply(AnnIgnoresConfig("a")) ==> Json.obj("firstName" -> Json.fromString("a"))
    }

    // circe-generic JsonCodecMacrosSuite "@JsonCodec should provide Encoder.AsObject instances"
    test("Encoder.AsObject instances, also for type parameters, hierarchies and recursive types") {
      Encoder.AsObject[AnnSingle]
      Encoder.AsObject[AnnTyped2[Int, Long]]
      Encoder.AsObject[AnnAccessModifier]
      Encoder.AsObject[AnnHierarchy]
      Encoder.AsObject[AnnRecursiveHierarchy]
      Encoder.AsObject[AnnSelfRecursiveWithOption]
      val typed = AnnTyped2(1, "a", 2L, 3)
      Decoder[AnnTyped2[String, Long]].decodeJson(Encoder[AnnTyped2[String, Long]].apply(typed)) ==> Right(typed)
      val hierarchy: List[AnnHierarchy] =
        List(AnnHierarchy1(1, "s"), AnnHierarchy2(List("a")), AnnHierarchy3(AnnSingle(1), 1.5))
      hierarchy.foreach(h => Decoder[AnnHierarchy].decodeJson(Encoder[AnnHierarchy].apply(h)) ==> Right(h))
      Encoder[AnnHierarchy].apply(AnnHierarchy3(AnnSingle(1), 1.5)) ==> Json.obj(
        "AnnHierarchy3" -> Json.obj("s" -> Json.obj("i" -> Json.fromInt(1)), "d" -> Json.fromDoubleOrNull(1.5))
      )
      val recursive: AnnRecursiveHierarchy = AnnNestedRecursiveHierarchy(AnnBaseRecursiveHierarchy("a"))
      Decoder[AnnRecursiveHierarchy].decodeJson(Encoder[AnnRecursiveHierarchy].apply(recursive)) ==> Right(recursive)
      val self = AnnSelfRecursiveWithOption(Some(AnnSelfRecursiveWithOption(None)))
      Decoder[AnnSelfRecursiveWithOption].decodeJson(Encoder[AnnSelfRecursiveWithOption].apply(self)) ==> Right(self)
    }

    // circe-generic JsonCodecMacrosSuite "@JsonCodec should only require necessary element instances for generic case
    // classes"
    test("generic case classes only require the element instance of the side that is used") {
      @JsonCodec case class GenericCaseClass[A](a: A)
      trait OnlyEncoder
      trait OnlyDecoder
      implicit def encodeOnlyEncoder: Encoder[OnlyEncoder] = Encoder[Unit].contramap(_ => ())
      implicit def decodeOnlyDecoder: Decoder[OnlyDecoder] = Decoder[Unit].map(_ => new OnlyDecoder {})
      Encoder.AsObject[GenericCaseClass[OnlyEncoder]]
      Decoder[GenericCaseClass[OnlyDecoder]]
    }

    // circe-generic JsonCodecMacrosSuite / generic-extras ConfiguredJsonCodecSuite "(encodeOnly = true) should only
    // provide Encoder instances [for generic case classes]", "(decodeOnly = true) should provide Decoder instances"
    test("local encodeOnly / decodeOnly classes, also generic ones") {
      implicit val config: Configuration = Configuration.default
      @JsonCodec(encodeOnly = true) case class CaseClassEncodeOnly(foo: String, bar: Int)
      Encoder[CaseClassEncodeOnly]
      Encoder.AsObject[CaseClassEncodeOnly]
      compileErrors("Decoder[CaseClassEncodeOnly]").check("could not find implicit")
      @JsonCodec(decodeOnly = true) case class CaseClassDecodeOnly(foo: String, bar: Int)
      Decoder[CaseClassDecodeOnly]
      compileErrors("Encoder[CaseClassDecodeOnly]").check("could not find implicit")
      @ConfiguredJsonCodec(encodeOnly = true) case class GenericEncodeOnly[A](foo: A, bar: Int)
      Encoder[GenericEncodeOnly[Int]]
      Encoder.AsObject[GenericEncodeOnly[Int]]
      compileErrors("Decoder[GenericEncodeOnly[Int]]").check("could not find implicit")
      @ConfiguredJsonCodec(decodeOnly = true) case class GenericDecodeOnly[A](foo: A, bar: Int)
      Decoder[GenericDecodeOnly[Int]]
      compileErrors("Encoder[GenericDecodeOnly[Int]]").check("could not find implicit")
    }

    // "should allow only one, named argument set to true": both set is rejected (upstream also rejects positional
    // arguments and `= false`, which Kindlings accepts)
    test("encodeOnly and decodeOnly together are rejected") {
      assert(
        compileErrors(
          "@hearth.kindlings.circederivation.JsonCodec(encodeOnly = true, decodeOnly = true) case class X(a: Int)"
        ).nonEmpty
      )
      assert(
        compileErrors(
          "@hearth.kindlings.circederivation.ConfiguredJsonCodec(encodeOnly = true, decodeOnly = true) case class X(a: Int)"
        ).nonEmpty
      )
    }

    // generic-extras ConfiguredJsonCodecSuite "ConfiguredJsonCodec should support configuration",
    // ConfiguredJsonCodecWithKeySuite "should support key annotation and configuration"
    test("@ConfiguredJsonCodec: upstream's configuration, a renamed key, access modifiers, type parameters") {
      import AnnUpstreamConfigured.*
      val foo: ConfigExampleBase = ConfigExampleFoo("f", 0, 2.5)
      val json = Json.obj(
        "type" -> Json.fromString("config_example_foo"),
        "this_is_a_field" -> Json.fromString("f"),
        "b" -> Json.fromDoubleOrNull(2.5)
      )
      val expected = Json.obj(
        "this_is_a_field" -> Json.fromString("f"),
        "a" -> Json.fromInt(0),
        "b" -> Json.fromDoubleOrNull(2.5),
        "type" -> Json.fromString("config_example_foo")
      )
      Encoder[ConfigExampleBase].apply(foo) ==> expected
      Decoder[ConfigExampleBase].decodeJson(json) ==> Right(foo)
      val withKey: ConfigExampleWithKeyBase = ConfigExampleWithKeyFoo("f", 0, 2.5)
      val keyJson = Json.obj(
        "type" -> Json.fromString("config_example_with_key_foo"),
        "this_is_a_field" -> Json.fromString("f"),
        "myField" -> Json.fromDoubleOrNull(2.5)
      )
      Decoder[ConfigExampleWithKeyBase].decodeJson(keyJson) ==> Right(withKey)
      Encoder[ConfigExampleWithKeyBase].apply(withKey) ==> keyJson.deepMerge(Json.obj("a" -> Json.fromInt(0)))
      Decoder[AccessModifier].decodeJson(Encoder[AccessModifier].apply(AccessModifier(1))) ==> Right(AccessModifier(1))
      Decoder[GenericExample[Int]].decodeJson(Encoder[GenericExample[Int]].apply(GenericExample(1, 2))) ==> Right(
        GenericExample(1, 2)
      )
    }
  }
}
