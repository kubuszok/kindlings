package hearth.kindlings.yamlderivation

import org.virtuslab.yaml.{ConstructError, Node, YamlDecoder, YamlError}

private[yamlderivation] trait KindlingsYamlDecoderCompanionCompat { this: KindlingsYamlDecoder.type =>

  inline def derive[A](using config: YamlConfig): YamlDecoder[A] = ${
    internal.compiletime.DecoderMacros.deriveDecoderImpl[A]('config)
  }

  inline def decode[A](node: Node)(using config: YamlConfig): Either[ConstructError, A] = ${
    internal.compiletime.DecoderMacros.deriveInlineDecodeImpl[A]('node, 'config)
  }

  inline def fromYamlString[A](yaml: String)(using config: YamlConfig): Either[YamlError, A] = ${
    internal.compiletime.DecoderMacros.deriveInlineFromYamlStringImpl[A]('yaml, 'config)
  }

  inline given derived[A](using config: YamlConfig): KindlingsYamlDecoder[A] = ${
    internal.compiletime.DecoderMacros.deriveKindlingsDecoderImpl[A]('config)
  }
}
