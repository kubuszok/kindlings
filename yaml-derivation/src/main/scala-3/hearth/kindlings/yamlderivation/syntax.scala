package hearth.kindlings.yamlderivation

import org.virtuslab.yaml.YamlError

object syntax {

  extension [A](value: A) {
    inline def toYamlString(using config: YamlConfig): String = ${
      internal.compiletime.EncoderMacros.deriveInlineToYamlStringImpl[A]('value, 'config)
    }
  }

  extension (yaml: String) {
    inline def fromYamlString[A](using config: YamlConfig): Either[YamlError, A] = ${
      internal.compiletime.DecoderMacros.deriveInlineFromYamlStringImpl[A]('yaml, 'config)
    }
  }
}
