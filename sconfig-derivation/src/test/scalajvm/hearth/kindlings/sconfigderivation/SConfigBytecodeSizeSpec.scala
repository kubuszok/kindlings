package hearth.kindlings.sconfigderivation

import hearth.MacroSuite
import org.ekrich.config.ConfigFactory

/** Instances for a 100-field case class with defaults, derived in a class of their own so their bytecode can be
  * measured.
  */
object SConfigWideInstances {
  val reader: ConfigReader[WideDefaults] = ConfigReader.derived[WideDefaults]
  val writer: ConfigWriter[WideDefaults] = ConfigWriter.derived[WideDefaults]

  // `config` is a parameter: the macros cannot evaluate it and read it at runtime
  def readerWith(config: SConfig): ConfigReader[WideDefaults] = {
    implicit val runtimeConfig: SConfig = config
    ConfigReader.derived[WideDefaults]
  }
  def writerWith(config: SConfig): ConfigWriter[WideDefaults] = {
    implicit val runtimeConfig: SConfig = config
    ConfigWriter.derived[WideDefaults]
  }
}

/** Guards against generated methods too large to be JIT-compiled (see `docs/research/generated-code-audit-2026-11.md`).
  * JVM only: it reads the class files.
  */
final class SConfigBytecodeSizeSpec extends MacroSuite {

  test("no method of the instances derived for a 100-field case class exceeds HotSpot's JIT limit") {
    val sizes = BytecodeSizes.methodSizes(SConfigWideInstances.getClass)
    assert(sizes.map(_._3).sum > 10000, s"expected the derived instances' code, got: $sizes")
    val huge = sizes.filter(_._3 > BytecodeSizes.HugeMethodLimit)
    assert(huge.isEmpty, s"methods larger than ${BytecodeSizes.HugeMethodLimit} B: ${huge.mkString(", ")}")
  }

  test("the instances work") {
    val value = WideDefaults(f0 = Some("x"), f1 = 1, f99 = 5L)
    SConfigWideInstances.reader.from(SConfigWideInstances.writer.to(value)) ==> Right(value)
    val runtimeConfig = SConfig()
    SConfigWideInstances.readerWith(runtimeConfig).from(SConfigWideInstances.writerWith(runtimeConfig).to(value)) ==>
      Right(value)
    SConfigWideInstances.reader.from(ConfigFactory.parseString("{}").root) ==> Right(WideDefaults())
    val failures = SConfigWideInstances.reader.from(ConfigFactory.parseString("{ f-1 = x, f-3 = y }").root)
    assert(failures.isLeft, failures.toString)
  }
}
