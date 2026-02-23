package hearth.kindlings.yamlderivation

import org.virtuslab.yaml.YamlError
import scala.language.experimental.macros

object syntax {

  implicit class YamlWriteOps[A](private val value: A) extends AnyVal {
    def toYamlString(implicit config: YamlConfig): String =
      macro internal.compiletime.EncoderMacros.deriveInlineToYamlStringOpsImpl[A]
  }

  implicit class YamlReadOps(private val yaml: String) extends AnyVal {
    def fromYamlString[A](implicit config: YamlConfig): Either[YamlError, A] =
      macro internal.compiletime.DecoderMacros.deriveInlineFromYamlStringOpsImpl[A]
  }
}
