package hearth.kindlings.yamlderivation

import org.virtuslab.yaml.{Node, YamlEncoder}

private[yamlderivation] trait KindlingsYamlEncoderCompanionCompat { this: KindlingsYamlEncoder.type =>

  inline def derive[A](using config: YamlConfig): YamlEncoder[A] = ${
    internal.compiletime.EncoderMacros.deriveEncoderImpl[A]('config)
  }

  inline def encode[A](inline value: A)(using config: YamlConfig): Node = ${
    internal.compiletime.EncoderMacros.deriveInlineEncodeImpl[A]('value, 'config)
  }

  inline def toYamlString[A](inline value: A)(using config: YamlConfig): String = ${
    internal.compiletime.EncoderMacros.deriveInlineToYamlStringImpl[A]('value, 'config)
  }

  inline given derived[A](using config: YamlConfig): KindlingsYamlEncoder[A] = ${
    internal.compiletime.EncoderMacros.deriveKindlingsEncoderImpl[A]('config)
  }
}
