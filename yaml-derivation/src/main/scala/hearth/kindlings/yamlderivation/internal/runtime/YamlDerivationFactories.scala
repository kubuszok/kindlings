package hearth.kindlings.yamlderivation.internal.runtime

import hearth.kindlings.yamlderivation.{KindlingsYamlCodec, KindlingsYamlDecoder, KindlingsYamlEncoder}
import org.virtuslab.yaml.{ConstructError, LoadSettings, Node}

object YamlDerivationFactories {

  def encoderInstance[A](asNodeFn: A => Node): KindlingsYamlEncoder[A] =
    new KindlingsYamlEncoder[A] {
      def asNode(obj: A): Node = asNodeFn(obj)
    }

  def decoderInstance[A](constructFn: Node => Either[ConstructError, A]): KindlingsYamlDecoder[A] =
    new KindlingsYamlDecoder[A] {
      def construct(node: Node)(implicit settings: LoadSettings = LoadSettings.empty): Either[ConstructError, A] =
        constructFn(node)
    }

  def codecInstance[A](
      asNodeFn: A => Node,
      constructFn: Node => Either[ConstructError, A]
  ): KindlingsYamlCodec[A] =
    new KindlingsYamlCodec[A] {
      def asNode(obj: A): Node = asNodeFn(obj)
      def construct(node: Node)(implicit settings: LoadSettings = LoadSettings.empty): Either[ConstructError, A] =
        constructFn(node)
    }
}
