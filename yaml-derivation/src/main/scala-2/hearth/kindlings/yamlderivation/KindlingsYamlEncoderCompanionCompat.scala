package hearth.kindlings.yamlderivation

import org.virtuslab.yaml.{Node, YamlEncoder}
import scala.language.experimental.macros

private[yamlderivation] trait KindlingsYamlEncoderCompanionCompat { this: KindlingsYamlEncoder.type =>

  def derive[A](implicit config: YamlConfig): YamlEncoder[A] =
    macro internal.compiletime.EncoderMacros.deriveEncoderImpl[A]

  def encode[A](value: A)(implicit config: YamlConfig): Node =
    macro internal.compiletime.EncoderMacros.deriveInlineEncodeImpl[A]

  def toYamlString[A](value: A)(implicit config: YamlConfig): String =
    macro internal.compiletime.EncoderMacros.deriveInlineToYamlStringImpl[A]

  implicit def derived[A](implicit config: YamlConfig): KindlingsYamlEncoder[A] =
    macro internal.compiletime.EncoderMacros.deriveKindlingsEncoderImpl[A]
}
