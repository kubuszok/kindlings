package hearth.kindlings.yamlderivation

import hearth.MacroSuite

sealed trait YamlScopedSum
object YamlScopedSum {
  final case class First(firstValue: Int) extends YamlScopedSum
  final case class Second(secondValue: String) extends YamlScopedSum
  case object Third extends YamlScopedSum
}

sealed trait YamlScopedEnum
object YamlScopedEnum {
  case object Alpha extends YamlScopedEnum
  case object Beta extends YamlScopedEnum
}

/** Instances derived with a configuration only known at runtime (a parameter the macro cannot evaluate). */
object YamlRuntimeConfigSumInstances {
  def encoder(config: YamlConfig): KindlingsYamlEncoder[YamlScopedSum] = {
    implicit val runtimeConfig: YamlConfig = config
    KindlingsYamlEncoder.derived[YamlScopedSum]
  }
  def decoder(config: YamlConfig): KindlingsYamlDecoder[YamlScopedSum] = {
    implicit val runtimeConfig: YamlConfig = config
    KindlingsYamlDecoder.derived[YamlScopedSum]
  }
  def enumEncoder(config: YamlConfig): KindlingsYamlEncoder[YamlScopedEnum] = {
    implicit val runtimeConfig: YamlConfig = config
    KindlingsYamlEncoder.derived[YamlScopedEnum]
  }
  def enumDecoder(config: YamlConfig): KindlingsYamlDecoder[YamlScopedEnum] = {
    implicit val runtimeConfig: YamlConfig = config
    KindlingsYamlDecoder.derived[YamlScopedEnum]
  }
}

/** With a configuration only known at runtime, derived sum encoders and decoders compute the subtype names once per
  * instance, not on every call.
  */
final class SumInstanceScopeSpec extends MacroSuite {

  private def countingConfig(base: YamlConfig): (YamlConfig, () => Int) = {
    var transformed = 0
    val config = base.withTransformConstructorNames { name =>
      transformed += 1
      name.toLowerCase
    }
    (config, () => transformed)
  }

  private val values: List[YamlScopedSum] =
    List(YamlScopedSum.First(1), YamlScopedSum.Second("x"), YamlScopedSum.Third)

  List(
    "discriminator" -> YamlConfig().withDiscriminator("type"),
    "wrapped subtypes" -> YamlConfig().withWrappedSubtypes
  ).foreach { case (mode, base) =>
    test(s"$mode: a runtime configuration transforms the subtype names once per encoder/decoder") {
      val (config, transformed) = countingConfig(base)
      val encoder = YamlRuntimeConfigSumInstances.encoder(config)
      val decoder = YamlRuntimeConfigSumInstances.decoder(config)
      val nodes = values.map(encoder.asNode)
      nodes.map(decoder.construct(_)) ==> values.map(Right(_))
      val before = transformed()
      values.foreach(v => decoder.construct(encoder.asNode(v)) ==> Right(v))
      transformed() ==> before
    }
  }

  private val enumValues: List[YamlScopedEnum] = List(YamlScopedEnum.Alpha, YamlScopedEnum.Beta)

  List(
    ("untagged", YamlConfig(), YamlStaticConfigSumInstances.Untagged),
    (
      "discriminator",
      YamlConfig(discriminator = Some("type")),
      YamlStaticConfigSumInstances.Discriminator
    ),
    ("wrapped subtypes", YamlConfig(wrappedSubtypes = true), YamlStaticConfigSumInstances.Wrapped),
    ("enumAsStrings", YamlConfig(enumAsStrings = true), YamlStaticConfigSumInstances.EnumAsStrings)
  ).foreach { case (mode, config, static) =>
    test(s"$mode: a configuration known at compile time encodes and decodes as the same one known at runtime") {
      val (encoder, decoder) =
        (YamlRuntimeConfigSumInstances.encoder(config), YamlRuntimeConfigSumInstances.decoder(config))
      val (enumEncoder, enumDecoder) =
        (YamlRuntimeConfigSumInstances.enumEncoder(config), YamlRuntimeConfigSumInstances.enumDecoder(config))
      import static.{decoder as staticDecoder, encoder as staticEncoder}
      import static.{enumDecoder as staticEnumDecoder, enumEncoder as staticEnumEncoder}
      values.foreach { v =>
        val node = encoder.asNode(v)
        staticEncoder.asNode(v) ==> node
        staticDecoder.construct(node) ==> decoder.construct(node)
      }
      enumValues.foreach { v =>
        val node = enumEncoder.asNode(v)
        staticEnumEncoder.asNode(v) ==> node
        staticEnumDecoder.construct(node) ==> enumDecoder.construct(node)
        // (untagged, every case object is `{}` and decodes as the first one, as in scala-yaml)
        if (mode != "untagged") enumDecoder.construct(node) ==> Right(v)
      }
    }
  }
}
