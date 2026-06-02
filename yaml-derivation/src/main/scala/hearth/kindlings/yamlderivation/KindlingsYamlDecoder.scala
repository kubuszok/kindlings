package hearth.kindlings.yamlderivation

import org.virtuslab.yaml.{ConstructError, LoadSettings, Node, YamlDecoder}

trait KindlingsYamlDecoder[A] extends YamlDecoder[A] {
  def construct(node: Node)(implicit settings: LoadSettings = LoadSettings.empty): Either[ConstructError, A]

  def orElse(other: KindlingsYamlDecoder[A]): KindlingsYamlDecoder[A] = {
    val self = this
    new KindlingsYamlDecoder[A] {
      def construct(node: Node)(implicit settings: LoadSettings = LoadSettings.empty): Either[ConstructError, A] =
        self.construct(node) match {
          case right @ Right(_) => right
          case Left(_)          => other.construct(node)
        }
    }
  }
}
object KindlingsYamlDecoder extends KindlingsYamlDecoderCompanionCompat {

  /** Special type - if its implicit is in scope then macros will log the derivation process.
    *
    * @see
    *   [[hearth.kindlings.yamlderivation.debug.logDerivationForKindlingsYamlDecoder]] for details
    */
  sealed trait LogDerivation
  object LogDerivation extends LogDerivation
}
