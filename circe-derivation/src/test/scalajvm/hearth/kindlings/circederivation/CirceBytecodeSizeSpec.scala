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
    assert(failures.fold(_.toList.map(_.pathToRootString), _ => Nil) == List(Some(".f1"), Some(".f3")), failures.toString)
    // without defaults every absent non-Option field fails
    val missing = CirceWideInstances.decoder.decodeAccumulating(Json.obj().hcursor)
    assert(missing.fold(_.size, _ => 0) == 67, missing.toString)
  }
}
