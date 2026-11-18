package hearth.kindlings.yamlderivation

import hearth.MacroSuite
import org.virtuslab.yaml.YamlDecoder

/** Decoders for a 100-field case class with defaults, derived in a class of their own so their bytecode can be
  * measured.
  */
object YamlWideInstances {
  // the default configuration (evaluated at compile time): defaults are used
  val decoder: KindlingsYamlDecoder[WideDefaults] = KindlingsYamlDecoder.derived[WideDefaults]

  // `config` is a parameter: the macro cannot evaluate it and reads it (incl. `useDefaults`) at runtime
  def decoderWith(config: YamlConfig): KindlingsYamlDecoder[WideDefaults] = {
    implicit val runtimeConfig: YamlConfig = config
    KindlingsYamlDecoder.derived[WideDefaults]
  }
}

/** Guards against generated methods too large to be JIT-compiled (see `docs/research/generated-code-audit-2026-11.md`).
  * JVM only: it reads the class files.
  */
final class YamlBytecodeSizeSpec extends MacroSuite {

  test("no method of the decoders derived for a 100-field case class exceeds HotSpot's JIT limit") {
    val sizes = BytecodeSizes.methodSizes(YamlWideInstances.getClass)
    assert(sizes.map(_._3).sum > 5000, s"expected the derived decoders' code, got: $sizes")
    val huge = sizes.filter(_._3 > BytecodeSizes.HugeMethodLimit)
    assert(huge.isEmpty, s"methods larger than ${BytecodeSizes.HugeMethodLimit} B: ${huge.mkString(", ")}")
  }

  test("the decoders work") {
    val value = WideDefaults(f0 = Some("x"), f1 = 1, f99 = 5L)
    val node = KindlingsYamlEncoder.encode[WideDefaults](value)
    val runtime: YamlDecoder[WideDefaults] = YamlWideInstances.decoderWith(YamlConfig())
    YamlWideInstances.decoder.construct(node) ==> Right(value)
    runtime.construct(node) ==> Right(value)
    val empty = KindlingsYamlEncoder.encode[Map[String, Int]](Map.empty)
    YamlWideInstances.decoder.construct(empty) ==> Right(WideDefaults())
    runtime.construct(empty) ==> Right(WideDefaults())
    // the first failing field is reported
    val broken = KindlingsYamlEncoder.encode[Map[String, String]](Map("f1" -> "x", "f3" -> "y"))
    val failure = YamlWideInstances.decoder.construct(broken)
    val runtimeFailure = runtime.construct(broken)
    assert(failure.isLeft, failure.toString)
    runtimeFailure ==> failure
  }
}
