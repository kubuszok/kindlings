package hearth.kindlings.jsoniterderivation

import com.github.plokhotnyuk.jsoniter_scala.core.{readFromString, writeToString}
import hearth.MacroSuite

/** A codec for a 100-field case class with defaults, derived in a class of its own so its bytecode can be measured. */
object JsoniterWideInstances {
  val codec: KindlingsJsonValueCodec[WideDefaults] = KindlingsJsonValueCodec.derived[WideDefaults]
  val codec64: KindlingsJsonValueCodec[Wide64Defaults] = KindlingsJsonValueCodec.derived[Wide64Defaults]
}

/** The same codec with a config known only at runtime (a different, generic code path). */
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

  private def assertNoHugeMethods(cls: Class[?]): Unit = {
    val sizes = BytecodeSizes.methodSizes(cls)
    assert(sizes.map(_._3).sum > 5000, s"expected the derived codec's code, got: $sizes")
    val huge = sizes.filter(_._3 > BytecodeSizes.HugeMethodLimit)
    assert(huge.isEmpty, s"methods larger than ${BytecodeSizes.HugeMethodLimit} B: ${huge.mkString(", ")}")
  }

  test("no method of the codec derived for a 100-field case class exceeds HotSpot's JIT limit") {
    assertNoHugeMethods(JsoniterWideInstances.getClass)
  }

  test("no method of the codec derived for a 100-field case class with a runtime config exceeds the JIT limit") {
    assertNoHugeMethods(JsoniterWideRuntimeConfigInstances.getClass)
  }

  test("the codecs work") {
    val value = WideDefaults(f0 = Some("x"), f1 = 1, f99 = 5L)
    readFromString(writeToString(value)(JsoniterWideInstances.codec))(JsoniterWideInstances.codec) ==> value
    val runtimeCodec = JsoniterWideRuntimeConfigInstances.codecWith(JsoniterConfig())
    readFromString(writeToString(value)(runtimeCodec))(runtimeCodec) ==> value
    readFromString("{}")(JsoniterWideInstances.codec) ==> WideDefaults()
    readFromString("{}")(runtimeCodec) ==> WideDefaults()
    // more than 8 keys: a key is found by its hash (compile-time config) or in a per-instance index (runtime config)
    val shuffled = """{"f99":5,"x":1,"f0":"x","f1":1,"f50":"e","f2":null}"""
    val expected = WideDefaults(f0 = Some("x"), f1 = 1, f50 = "e", f99 = 5L)
    readFromString(shuffled)(JsoniterWideInstances.codec) ==> expected
    readFromString(shuffled)(runtimeCodec) ==> expected
    readFromString("""{"g63":5,"x":1,"g0":"x","g1":1}""")(JsoniterWideInstances.codec64) ==>
      Wide64Defaults(g0 = Some("x"), g1 = 1, g63 = 5L)
  }
}
