package hearth.kindlings.oracle.circe

import io.circe.Codec
import io.circe.generic.extras.Configuration as ExtrasConfiguration

/** Scala 3: upstream cannot derive value classes at all (the compiler synthesizes no `Mirror` for them), so users write
  * these instances by hand and there is no upstream derivation to compare with.
  */
object CiVersionSpecific {

  val isScala3: Boolean = true

  def extras(implicit config: ExtrasConfiguration): Option[Codec[CiWithId]] = None

  def generic: Option[Codec[CiWithId]] = None
}
