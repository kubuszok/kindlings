package hearth.kindlings.ubjsonderivation

import hearth.MacroSuite
import hearth.kindlings.ubjsonderivation.internal.runtime.UBJsonDerivationUtils

/** A codec for a 100-field case class with defaults, derived in a class of its own so its bytecode can be measured. */
object UBJsonWideInstances {
  // the default configuration (evaluated at compile time)
  val codec: UBJsonValueCodec[WideDefaults] = UBJsonValueCodec.derived[WideDefaults]
}

/** Not size-checked: with a runtime config the field-matching lambda of a 100-field decoder is ~9.4 KB (it predates the
  * 0.4.0 work, like jsoniter's runtime-config decoder; tracked in `docs/research/generated-code-audit-2026-11.md`).
  */
object UBJsonWideRuntimeInstances {
  // `config` is a parameter: the macro cannot evaluate it and reads it at runtime
  def codecWith(config: UBJsonConfig): UBJsonValueCodec[WideDefaults] = {
    implicit val runtimeConfig: UBJsonConfig = config
    UBJsonValueCodec.derived[WideDefaults]
  }
}

/** Guards against generated methods too large to be JIT-compiled (see `docs/research/generated-code-audit-2026-11.md`).
  * JVM only: it reads the class files.
  */
final class UBJsonBytecodeSizeSpec extends MacroSuite {

  test(
    "no method of the codec derived (config known at compile time) for a 100-field case class exceeds HotSpot's JIT limit"
  ) {
    val sizes = BytecodeSizes.methodSizes(UBJsonWideInstances.getClass)
    assert(sizes.map(_._3).sum > 5000, s"expected the derived codecs' code, got: $sizes")
    val huge = sizes.filter(_._3 > BytecodeSizes.HugeMethodLimit)
    assert(huge.isEmpty, s"methods larger than ${BytecodeSizes.HugeMethodLimit} B: ${huge.mkString(", ")}")
  }

  test("the codecs round-trip and fill absent fields with their defaults") {
    val value = WideDefaults(f0 = Some("x"), f1 = 1, f99 = 5L)
    val runtime = UBJsonWideRuntimeInstances.codecWith(UBJsonConfig())
    List(UBJsonWideInstances.codec, runtime).foreach { codec =>
      val bytes = UBJsonDerivationUtils.writeToBytes(value)(codec)
      UBJsonDerivationUtils.readFromBytes(bytes)(codec) ==> value
    }
    // an empty object: every field absent
    val empty =
      UBJsonDerivationUtils.writeToBytes(Map.empty[String, String])(UBJsonValueCodec.derived[Map[String, String]])
    List(UBJsonWideInstances.codec, runtime).foreach { codec =>
      UBJsonDerivationUtils.readFromBytes(empty)(codec) ==> WideDefaults()
    }
  }
}
