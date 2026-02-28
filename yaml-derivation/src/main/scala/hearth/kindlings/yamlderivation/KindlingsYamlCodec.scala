package hearth.kindlings.yamlderivation

import org.virtuslab.yaml.{YamlDecoder, YamlEncoder}

trait KindlingsYamlCodec[A] extends YamlEncoder[A] with YamlDecoder[A]

object KindlingsYamlCodec extends KindlingsYamlCodecCompanionCompat {

  /** Special type - if its implicit is in scope then macros will log the derivation process.
    *
    * @see
    *   [[hearth.kindlings.yamlderivation.debug.logDerivationForKindlingsYamlEncoder]] and
    *   [[hearth.kindlings.yamlderivation.debug.logDerivationForKindlingsYamlDecoder]] for details
    */
  sealed trait LogDerivation
  object LogDerivation extends LogDerivation
}
