package hearth.kindlings.circederivation

import cats.data.Validated
import hearth.MacroSuite
import io.circe.{Decoder, Json}

/** Decoders for a 100-field case class with defaults, derived in a class of their own so their bytecode can be
  * measured.
  */
object CirceWideInstances {
  // the default configuration (evaluated at compile time): defaults are not used
  val decoder: KindlingsDecoder[WideDefaults] = KindlingsDecoder.derived[WideDefaults]

  // `config` is a parameter: the macro cannot evaluate it and reads it (incl. `useDefaults`) at runtime
  def decoderWith(config: Configuration): KindlingsDecoder[WideDefaults] = {
    implicit val runtimeConfig: Configuration = config
    KindlingsDecoder.derived[WideDefaults]
  }

  val encoder: KindlingsEncoder[WideDefaults] = KindlingsEncoder.derived[WideDefaults]

  // `config` is a parameter: the transformed field names are computed once per instance
  def encoderWith(config: Configuration): KindlingsEncoder[WideDefaults] = {
    implicit val runtimeConfig: Configuration = config
    KindlingsEncoder.derived[WideDefaults]
  }
}

case class InlineNested(x: Int, label: Option[String])
case class InlineDecoded(name: String, tags: List[String], nested: Option[InlineNested], more: List[InlineNested])

/** The inline `KindlingsDecoder.decode`, in a class of its own so its bytecode can be inspected. */
object CirceInlineDecodes {
  def decode(json: Json): Either[io.circe.DecodingFailure, InlineDecoded] = KindlingsDecoder.decode[InlineDecoded](json)
}

/** A field type whose encoder is an `implicit def` (a lazy val of a derived instance). */
final case class InlineField(value: Int)
object InlineField {
  implicit def inlineFieldEncoder: io.circe.Encoder[InlineField] = io.circe.Encoder.encodeInt.contramap(_.value)
}
final case class InlineEncoded(counted: InlineField, other: Int)

/** The inline `KindlingsEncoder.encode` of a type with a summoned field instance, in a class of its own so its bytecode
  * can be inspected.
  */
object CirceInlineEncodes {
  def encode(value: InlineEncoded): Json = KindlingsEncoder.encode[InlineEncoded](value)
  def encodeWith(value: InlineEncoded, config: Configuration): Json = {
    implicit val runtimeConfig: Configuration = config
    KindlingsEncoder.encode[InlineEncoded](value)
  }
}

/** Guards against generated methods too large to be JIT-compiled (see `docs/research/generated-code-audit-2026-11.md`).
  * JVM only: it reads the class files.
  */
final class CirceBytecodeSizeSpec extends MacroSuite {

  test("no method of the decoders derived for a 100-field case class exceeds HotSpot's JIT limit") {
    val sizes = BytecodeSizes.methodSizes(CirceWideInstances.getClass)
    assert(sizes.map(_._3).sum > 5000, s"expected the derived decoders' code, got: $sizes")
    val huge = sizes.filter(_._3 > BytecodeSizes.HugeMethodLimit)
    assert(huge.isEmpty, s"methods larger than ${BytecodeSizes.HugeMethodLimit} B: ${huge.mkString(", ")}")
  }

  test("the inline decode allocates no lazy cell per call") {
    // the instance's cached field decoders are lazy vals (positive control: the check sees them)
    assert(BytecodeSizes.refersTo(CirceWideInstances.getClass, "scala/runtime/LazyRef"))
    // the inline decode runs its definitions once per call: field decoders are defs there
    assert(!BytecodeSizes.refersTo(CirceInlineDecodes.getClass, "scala/runtime/LazyRef"))
    val value = InlineDecoded("n", List("a"), Some(InlineNested(1, None)), List(InlineNested(2, Some("l"))))
    CirceInlineDecodes.decode(KindlingsEncoder.encode[InlineDecoded](value)) ==> Right(value)
  }

  test("the inline encode allocates no lazy cell per call") {
    // the instances' summoned field encoders and runtime-transformed names are lazy vals (positive control above)
    assert(!BytecodeSizes.refersTo(CirceInlineEncodes.getClass, "scala/runtime/LazyRef"))
    val value = InlineEncoded(InlineField(1), 2)
    CirceInlineEncodes.encode(value) ==> Json.obj("counted" -> Json.fromInt(1), "other" -> Json.fromInt(2))
    CirceInlineEncodes.encodeWith(value, Configuration.default.withTransformMemberNames(_.toUpperCase)) ==>
      Json.obj("COUNTED" -> Json.fromInt(1), "OTHER" -> Json.fromInt(2))
  }

  test("the encoders work, with a compile-time and a runtime configuration") {
    val value = WideDefaults(f0 = Some("x"), f1 = 1, f99 = 5L)
    val json = KindlingsEncoder.encode[WideDefaults](value)
    CirceWideInstances.encoder(value) ==> json
    val upper = CirceWideInstances.encoderWith(Configuration.default.withTransformMemberNames(_.toUpperCase))(value)
    upper.asObject.map(_.keys.toList) ==> json.asObject.map(_.keys.toList.map(_.toUpperCase))
  }

  test("the decoders work, fail-fast and accumulating") {
    val value = WideDefaults(f0 = Some("x"), f1 = 1, f99 = 5L)
    val json = KindlingsEncoder.encode[WideDefaults](value)
    val withDefaults: Decoder[WideDefaults] = CirceWideInstances.decoderWith(Configuration(useDefaults = true))
    CirceWideInstances.decoder.decodeJson(json) ==> Right(value)
    CirceWideInstances.decoder.decodeAccumulating(json.hcursor) ==> Validated.valid(value)
    withDefaults.decodeJson(json) ==> Right(value)
    withDefaults.decodeAccumulating(json.hcursor) ==> Validated.valid(value)
    withDefaults.decodeJson(Json.obj()) ==> Right(WideDefaults())
    withDefaults.decodeAccumulating(Json.obj().hcursor) ==> Validated.valid(WideDefaults())

    val broken = Json.obj("f1" -> Json.fromString("x"), "f3" -> Json.fromString("y"))
    // fail-fast: the first failing field
    val first = withDefaults.decodeJson(broken)
    assert(first.left.toOption.flatMap(_.pathToRootString) == Some(".f1"), first.toString)
    // accumulating: every failing field, in field order
    val failures = withDefaults.decodeAccumulating(broken.hcursor)
    assert(
      failures.fold(_.toList.map(_.pathToRootString), _ => Nil) == List(Some(".f1"), Some(".f3")),
      failures.toString
    )
    // without defaults every absent non-Option field fails
    val missing = CirceWideInstances.decoder.decodeAccumulating(Json.obj().hcursor)
    assert(missing.fold(_.size, _ => 0) == 67, missing.toString)
  }
}
