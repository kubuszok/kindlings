package hearth.kindlings.yamlderivation

import org.virtuslab.yaml.{Node, YamlEncoder}

trait KindlingsYamlEncoder[A] extends YamlEncoder[A] {
  def asNode(obj: A): Node
}
object KindlingsYamlEncoder extends KindlingsYamlEncoderCompanionCompat {

  /** Special type - if its implicit is in scope then macros will log the derivation process.
    *
    * @see
    *   [[hearth.kindlings.yamlderivation.debug.logDerivationForKindlingsYamlEncoder]] for details
    */
  sealed trait LogDerivation
  object LogDerivation extends LogDerivation
}
