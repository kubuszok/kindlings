package hearth.kindlings.pureconfigderivation

import com.typesafe.config.ConfigFactory
import hearth.MacroSuite
import pureconfig.ConfigCursor

/** Instances for a 100-field case class with defaults, derived in a class of their own so their bytecode can be
  * measured.
  */
object PureConfigWideInstances {
  val reader: KindlingsConfigReader[WideDefaults] = KindlingsConfigReader.derived[WideDefaults]
  val writer: KindlingsConfigWriter[WideDefaults] = KindlingsConfigWriter.derived[WideDefaults]

  // `config` is a parameter: the macros cannot evaluate it and read it at runtime
  def readerWith(config: PureConfig): KindlingsConfigReader[WideDefaults] = {
    implicit val runtimeConfig: PureConfig = config
    KindlingsConfigReader.derived[WideDefaults]
  }
  def writerWith(config: PureConfig): KindlingsConfigWriter[WideDefaults] = {
    implicit val runtimeConfig: PureConfig = config
    KindlingsConfigWriter.derived[WideDefaults]
  }
}

/** Guards against generated methods too large to be JIT-compiled (see `docs/research/generated-code-audit-2026-11.md`).
  */
final class PureConfigBytecodeSizeSpec extends MacroSuite {

  test("no method of the instances derived for a 100-field case class exceeds HotSpot's JIT limit") {
    val sizes = BytecodeSizes.methodSizes(PureConfigWideInstances.getClass)
    assert(sizes.map(_._3).sum > 10000, s"expected the derived instances' code, got: $sizes")
    val huge = sizes.filter(_._3 > BytecodeSizes.HugeMethodLimit)
    assert(huge.isEmpty, s"methods larger than ${BytecodeSizes.HugeMethodLimit} B: ${huge.mkString(", ")}")
  }

  test("the instances work") {
    val value = WideDefaults(f0 = Some("x"), f1 = 1, f99 = 5L)
    val cursor = ConfigCursor(PureConfigWideInstances.writer.to(value), Nil)
    PureConfigWideInstances.reader.from(cursor) ==> Right(value)
    val runtimeConfig = PureConfig()
    val runtimeCursor = ConfigCursor(PureConfigWideInstances.writerWith(runtimeConfig).to(value), Nil)
    PureConfigWideInstances.readerWith(runtimeConfig).from(runtimeCursor) ==> Right(value)
    PureConfigWideInstances.reader.from(ConfigCursor(ConfigFactory.parseString("{}").root(), Nil)) ==>
      Right(WideDefaults())
    val failures = PureConfigWideInstances.reader.from(
      ConfigCursor(ConfigFactory.parseString("{ f-1 = x, f-3 = y }").root(), Nil)
    )
    assert(failures.left.toOption.exists(_.toList.size == 2), failures.toString)
  }
}
