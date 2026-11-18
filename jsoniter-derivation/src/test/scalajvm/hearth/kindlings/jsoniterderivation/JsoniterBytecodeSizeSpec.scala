package hearth.kindlings.jsoniterderivation

import com.github.plokhotnyuk.jsoniter_scala.core.{readFromString, writeToString}
import hearth.MacroSuite

/** A codec for a 100-field case class with defaults, derived in a class of its own so its bytecode can be measured. */
object JsoniterWideInstances {
  val codec: KindlingsJsonValueCodec[WideDefaults] = KindlingsJsonValueCodec.derived[WideDefaults]
}

/** The same codec with a config known only at runtime: not guarded yet. Its decoder matches keys against
  * `config.fieldNameMapper(name)` calls per field per key and is ~9.4 KB (see the pre-existing problems in
  * `docs/research/generated-code-audit-2026-11.md`).
  */
object JsoniterWideRuntimeConfigInstances {
  // `config` is a parameter: the macro cannot evaluate it and reads it at runtime
  def codecWith(config: JsoniterConfig): KindlingsJsonValueCodec[WideDefaults] = {
    implicit val runtimeConfig: JsoniterConfig = config
    KindlingsJsonValueCodec.derived[WideDefaults]
  }
}

/** Guards against generated methods too large to be JIT-compiled (see `docs/research/generated-code-audit-2026-11.md`).
  * JVM only: it reads the class files.
  */
final class JsoniterBytecodeSizeSpec extends MacroSuite {

  test("no method of the codec derived for a 100-field case class exceeds HotSpot's JIT limit") {
    val sizes = BytecodeSizes.methodSizes(JsoniterWideInstances.getClass)
    assert(sizes.map(_._3).sum > 5000, s"expected the derived codec's code, got: $sizes")
    val huge = sizes.filter(_._3 > BytecodeSizes.HugeMethodLimit)
    assert(huge.isEmpty, s"methods larger than ${BytecodeSizes.HugeMethodLimit} B: ${huge.mkString(", ")}")
  }

  test("the codecs work") {
    val value = WideDefaults(f0 = Some("x"), f1 = 1, f99 = 5L)
    readFromString(writeToString(value)(JsoniterWideInstances.codec))(JsoniterWideInstances.codec) ==> value
    val runtimeCodec = JsoniterWideRuntimeConfigInstances.codecWith(JsoniterConfig())
    readFromString(writeToString(value)(runtimeCodec))(runtimeCodec) ==> value
    readFromString("{}")(JsoniterWideInstances.codec) ==> WideDefaults()
  }
}
