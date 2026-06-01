package hearth.kindlings.yamlderivation

private[yamlderivation] trait KindlingsYamlCodecCompanionCompat { this: KindlingsYamlCodec.type =>

  inline given derived[A](using config: YamlConfig): KindlingsYamlCodec[A] =
    internal.runtime.YamlDerivationUtils.yamlCodec[A](
      KindlingsYamlEncoder.derived[A],
      KindlingsYamlDecoder.derived[A]
    )
}
