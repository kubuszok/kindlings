package hearth.kindlings.yamlderivation

import org.virtuslab.yaml.{ConstructError, Node, YamlDecoder, YamlError}
import scala.language.experimental.macros

private[yamlderivation] trait KindlingsYamlDecoderCompanionCompat { this: KindlingsYamlDecoder.type =>

  def derive[A](implicit config: YamlConfig): YamlDecoder[A] =
    macro internal.compiletime.DecoderMacros.deriveDecoderImpl[A]

  def decode[A](node: Node)(implicit config: YamlConfig): Either[ConstructError, A] =
    macro internal.compiletime.DecoderMacros.deriveInlineDecodeImpl[A]

  def fromYamlString[A](yaml: String)(implicit config: YamlConfig): Either[YamlError, A] =
    macro internal.compiletime.DecoderMacros.deriveInlineFromYamlStringImpl[A]

  implicit def derived[A](implicit config: YamlConfig): KindlingsYamlDecoder[A] =
    macro internal.compiletime.DecoderMacros.deriveKindlingsDecoderImpl[A]
}
