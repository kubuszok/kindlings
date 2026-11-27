package hearth.kindlings.avroderivation

import hearth.MacroSuite

/** Instances for a 100-field record with defaults, derived in a class of their own so their bytecode can be measured.
  */
object AvroWideInstances {
  val schemaFor: AvroSchemaFor[WideDefaults] = AvroSchemaFor.derived[WideDefaults]
  val encoder: AvroEncoder[WideDefaults] = AvroEncoder.derived[WideDefaults]
  val decoder: AvroDecoder[WideDefaults] = AvroDecoder.derived[WideDefaults]

  // `config` is a parameter: the macros cannot evaluate it and read it at runtime
  def encoderWith(config: AvroConfig): AvroEncoder[WideDefaults] = {
    implicit val runtimeConfig: AvroConfig = config
    AvroEncoder.derived[WideDefaults]
  }
  def decoderWith(config: AvroConfig): AvroDecoder[WideDefaults] = {
    implicit val runtimeConfig: AvroConfig = config
    AvroDecoder.derived[WideDefaults]
  }
}

/** Instances exercising the paths added for avro4s parity (see `AvroAvro4sParitySpec`): renamed ENUM symbols, fields
  * moved to another namespace, FIXED strings and value classes, fields read through `Product`.
  */
object AvroParityInstances {
  val shipEncoder: AvroEncoder[PaShip] = AvroEncoder.derived[PaShip]
  val shipDecoder: AvroDecoder[PaShip] = AvroDecoder.derived[PaShip]
  val namespacedEncoder: AvroEncoder[PaNamespaced] = AvroEncoder.derived[PaNamespaced]
  val namespacedDecoder: AvroDecoder[PaNamespaced] = AvroDecoder.derived[PaNamespaced]
  val fixedEncoder: AvroEncoder[PaFixedValues] = AvroEncoder.derived[PaFixedValues]
  val fixedDecoder: AvroDecoder[PaFixedValues] = AvroDecoder.derived[PaFixedValues]
  val privateEncoder: AvroEncoder[PaPrivate] = AvroEncoder.derived[PaPrivate]
  val privateDecoder: AvroDecoder[PaPrivate] = AvroDecoder.derived[PaPrivate]
}

/** Guards against generated methods too large to be JIT-compiled (see `docs/research/generated-code-audit-2026-11.md`).
  */
final class AvroBytecodeSizeSpec extends MacroSuite {

  test("no method of the instances derived for a 100-field record exceeds HotSpot's JIT limit") {
    val sizes = BytecodeSizes.methodSizes(AvroWideInstances.getClass)
    assert(sizes.map(_._3).sum > 10000, s"expected the derived instances' code, got: $sizes")
    val huge = sizes.filter(_._3 > BytecodeSizes.HugeMethodLimit)
    assert(huge.isEmpty, s"methods larger than ${BytecodeSizes.HugeMethodLimit} B: ${huge.mkString(", ")}")
  }

  test("no method of the instances using the avro4s-parity paths exceeds HotSpot's JIT limit") {
    val sizes = BytecodeSizes.methodSizes(AvroParityInstances.getClass)
    assert(sizes.map(_._3).sum > 1000, s"expected the derived instances' code, got: $sizes")
    val huge = sizes.filter(_._3 > BytecodeSizes.HugeMethodLimit)
    assert(huge.isEmpty, s"methods larger than ${BytecodeSizes.HugeMethodLimit} B: ${huge.mkString(", ")}")
  }

  test("the instances work") {
    val value = WideDefaults(f0 = Some("x"), f1 = 1, f99 = 5L)
    val runtimeConfig = AvroConfig.default
    AvroIO.fromBinary(AvroIO.toBinary(value)(AvroWideInstances.encoder))(AvroWideInstances.decoder) ==> value
    val bytes = AvroIO.toBinary(value)(AvroWideInstances.encoderWith(runtimeConfig))
    AvroIO.fromBinary(bytes)(AvroWideInstances.decoderWith(runtimeConfig)) ==> value
    AvroWideInstances.schemaFor.schema ==> AvroWideInstances.encoder.schema
  }
}
