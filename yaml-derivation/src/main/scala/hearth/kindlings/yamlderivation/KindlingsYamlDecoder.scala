package hearth.kindlings.yamlderivation

import org.virtuslab.yaml.{ConstructError, LoadSettings, Node, YamlDecoder}

trait KindlingsYamlDecoder[A] extends YamlDecoder[A] {
  def construct(node: Node)(implicit settings: LoadSettings = LoadSettings.empty): Either[ConstructError, A]
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
