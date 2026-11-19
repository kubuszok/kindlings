package hearth.kindlings.yamlderivation

// Instances derived with configurations known at compile time (passed explicitly: a Scala 2 macro cannot read the
// definition of an implicit val), so that only the applicable sum dispatch is generated. The Scala 2 and Scala 3
// versions differ only in how the configuration is passed.

/** Instances of `YamlScopedSum`/`YamlScopedEnum` with configurations known at compile time. */
object YamlStaticConfigSumInstances {
  sealed trait StaticInstances {
    val encoder: KindlingsYamlEncoder[YamlScopedSum]
    val decoder: KindlingsYamlDecoder[YamlScopedSum]
    val enumEncoder: KindlingsYamlEncoder[YamlScopedEnum]
    val enumDecoder: KindlingsYamlDecoder[YamlScopedEnum]
  }
  object Untagged extends StaticInstances {
    val encoder: KindlingsYamlEncoder[YamlScopedSum] = KindlingsYamlEncoder.derived[YamlScopedSum]
    val decoder: KindlingsYamlDecoder[YamlScopedSum] = KindlingsYamlDecoder.derived[YamlScopedSum]
    val enumEncoder: KindlingsYamlEncoder[YamlScopedEnum] =
      KindlingsYamlEncoder.derived[YamlScopedEnum]
    val enumDecoder: KindlingsYamlDecoder[YamlScopedEnum] =
      KindlingsYamlDecoder.derived[YamlScopedEnum]
  }
  object Discriminator extends StaticInstances {
    val encoder: KindlingsYamlEncoder[YamlScopedSum] = KindlingsYamlEncoder.derived[YamlScopedSum](using YamlConfig(discriminator = Some("type")))
    val decoder: KindlingsYamlDecoder[YamlScopedSum] = KindlingsYamlDecoder.derived[YamlScopedSum](using YamlConfig(discriminator = Some("type")))
    val enumEncoder: KindlingsYamlEncoder[YamlScopedEnum] =
      KindlingsYamlEncoder.derived[YamlScopedEnum](using YamlConfig(discriminator = Some("type")))
    val enumDecoder: KindlingsYamlDecoder[YamlScopedEnum] =
      KindlingsYamlDecoder.derived[YamlScopedEnum](using YamlConfig(discriminator = Some("type")))
  }
  object Wrapped extends StaticInstances {
    val encoder: KindlingsYamlEncoder[YamlScopedSum] = KindlingsYamlEncoder.derived[YamlScopedSum](using YamlConfig(wrappedSubtypes = true))
    val decoder: KindlingsYamlDecoder[YamlScopedSum] = KindlingsYamlDecoder.derived[YamlScopedSum](using YamlConfig(wrappedSubtypes = true))
    val enumEncoder: KindlingsYamlEncoder[YamlScopedEnum] =
      KindlingsYamlEncoder.derived[YamlScopedEnum](using YamlConfig(wrappedSubtypes = true))
    val enumDecoder: KindlingsYamlDecoder[YamlScopedEnum] =
      KindlingsYamlDecoder.derived[YamlScopedEnum](using YamlConfig(wrappedSubtypes = true))
  }
  object EnumAsStrings extends StaticInstances {
    val encoder: KindlingsYamlEncoder[YamlScopedSum] = KindlingsYamlEncoder.derived[YamlScopedSum](using YamlConfig(enumAsStrings = true))
    val decoder: KindlingsYamlDecoder[YamlScopedSum] = KindlingsYamlDecoder.derived[YamlScopedSum](using YamlConfig(enumAsStrings = true))
    val enumEncoder: KindlingsYamlEncoder[YamlScopedEnum] =
      KindlingsYamlEncoder.derived[YamlScopedEnum](using YamlConfig(enumAsStrings = true))
    val enumDecoder: KindlingsYamlDecoder[YamlScopedEnum] =
      KindlingsYamlDecoder.derived[YamlScopedEnum](using YamlConfig(enumAsStrings = true))
  }
}

/** Instances of a sealed trait with 50 subtypes with configurations known at compile time. */
object YamlWideSumStaticInstances {
  object Discriminator {
    val encoder: KindlingsYamlEncoder[WideSum] = KindlingsYamlEncoder.derived[WideSum](using YamlConfig(discriminator = Some("type")))
    val decoder: KindlingsYamlDecoder[WideSum] = KindlingsYamlDecoder.derived[WideSum](using YamlConfig(discriminator = Some("type")))
  }
  object Wrapped {
    val encoder: KindlingsYamlEncoder[WideSum] = KindlingsYamlEncoder.derived[WideSum](using YamlConfig(wrappedSubtypes = true))
    val decoder: KindlingsYamlDecoder[WideSum] = KindlingsYamlDecoder.derived[WideSum](using YamlConfig(wrappedSubtypes = true))
  }
  object Untagged {
    val encoder: KindlingsYamlEncoder[WideSum] = KindlingsYamlEncoder.derived[WideSum]
    val decoder: KindlingsYamlDecoder[WideSum] = KindlingsYamlDecoder.derived[WideSum]
  }
}
