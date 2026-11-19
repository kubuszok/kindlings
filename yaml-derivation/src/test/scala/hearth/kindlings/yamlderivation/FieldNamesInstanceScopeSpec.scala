package hearth.kindlings.yamlderivation

import hearth.MacroSuite

final case class YamlScopedFields(firstField: Int, secondField: String, optionalField: Option[Int] = None)

sealed trait YamlScopedFieldsSum
object YamlScopedFieldsSum {
  final case class WithFields(innerField: Int, otherField: String) extends YamlScopedFieldsSum
}

/** Instances derived with a configuration only known at runtime (a parameter the macro cannot evaluate). */
object YamlRuntimeConfigFieldInstances {
  def encoder(config: YamlConfig): KindlingsYamlEncoder[YamlScopedFields] = {
    implicit val runtimeConfig: YamlConfig = config
    KindlingsYamlEncoder.derived[YamlScopedFields]
  }
  def decoder(config: YamlConfig): KindlingsYamlDecoder[YamlScopedFields] = {
    implicit val runtimeConfig: YamlConfig = config
    KindlingsYamlDecoder.derived[YamlScopedFields]
  }
  def sumEncoder(config: YamlConfig): KindlingsYamlEncoder[YamlScopedFieldsSum] = {
    implicit val runtimeConfig: YamlConfig = config
    KindlingsYamlEncoder.derived[YamlScopedFieldsSum]
  }
  def sumDecoder(config: YamlConfig): KindlingsYamlDecoder[YamlScopedFieldsSum] = {
    implicit val runtimeConfig: YamlConfig = config
    KindlingsYamlDecoder.derived[YamlScopedFieldsSum]
  }
}

/** With a configuration only known at runtime, derived encoders and decoders compute the field names once per instance,
  * not on every call.
  */
final class FieldNamesInstanceScopeSpec extends MacroSuite {

  final private class Counting(base: YamlConfig) {
    var calls: Int = 0
    val config: YamlConfig = base.withTransformMemberNames { name => calls += 1; name.toUpperCase }
  }

  // Runs `op` once (an instance may compute its names on first use), then twice more, and returns how many name
  // transformations the 2nd and 3rd run made.
  private def transformsOfRepeatedCalls(counting: Counting)(op: () => Any): Int = {
    op()
    val before = counting.calls
    op()
    op()
    counting.calls - before
  }

  private val value = YamlScopedFields(1, "s", Some(2))

  test("a case class: the encoder and the decoder") {
    val counting = new Counting(YamlConfig())
    val encoder = YamlRuntimeConfigFieldInstances.encoder(counting.config)
    val decoder = YamlRuntimeConfigFieldInstances.decoder(counting.config)
    val node = encoder.asNode(value)
    decoder.construct(node) ==> Right(value)
    transformsOfRepeatedCalls(counting)(() => encoder.asNode(value)) ==> 0
    transformsOfRepeatedCalls(counting)(() => decoder.construct(node)) ==> 0
    // the field names are the transformed ones
    implicit val upperCase: YamlConfig = YamlConfig().withTransformMemberNames(_.toUpperCase)
    KindlingsYamlDecoder.decode[YamlScopedFields](node) ==> Right(value)
  }

  test("the fields of a subtype") {
    val counting = new Counting(YamlConfig().withDiscriminator("type"))
    val encoder = YamlRuntimeConfigFieldInstances.sumEncoder(counting.config)
    val decoder = YamlRuntimeConfigFieldInstances.sumDecoder(counting.config)
    val sumValue: YamlScopedFieldsSum = YamlScopedFieldsSum.WithFields(1, "o")
    val node = encoder.asNode(sumValue)
    decoder.construct(node) ==> Right(sumValue)
    transformsOfRepeatedCalls(counting)(() => encoder.asNode(sumValue)) ==> 0
    transformsOfRepeatedCalls(counting)(() => decoder.construct(node)) ==> 0
  }
}
