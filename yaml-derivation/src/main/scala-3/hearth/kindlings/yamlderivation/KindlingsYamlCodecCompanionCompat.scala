package hearth.kindlings.yamlderivation

private[yamlderivation] trait KindlingsYamlCodecCompanionCompat { this: KindlingsYamlCodec.type =>

  inline def derive[A](using config: YamlConfig): KindlingsYamlCodec[A] =
    internal.runtime.YamlDerivationUtils.yamlCodec[A](
      KindlingsYamlEncoder.derive[A],
      KindlingsYamlDecoder.derive[A]
    )

  inline given derived[A](using config: YamlConfig): KindlingsYamlCodec[A] =
    internal.runtime.YamlDerivationUtils.yamlCodec[A](
      KindlingsYamlEncoder.derive[A],
      KindlingsYamlDecoder.derive[A]
    )
}
