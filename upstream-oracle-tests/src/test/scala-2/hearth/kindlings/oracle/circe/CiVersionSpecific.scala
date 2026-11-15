package hearth.kindlings.oracle.circe

import io.circe.Codec
import io.circe.generic.extras.Configuration as ExtrasConfiguration

/** Scala 2: circe-generic and generic-extras derive value classes as products (`{"value":1}`). */
object CiVersionSpecific {

  val isScala3: Boolean = false

  def extras(implicit config: ExtrasConfiguration): Option[Codec[CiWithId]] = {
    import io.circe.generic.extras.semiauto.deriveConfiguredCodec
    implicit val userId: Codec.AsObject[CiUserId] = deriveConfiguredCodec[CiUserId]
    Some(deriveConfiguredCodec[CiWithId])
  }

  def generic: Option[Codec[CiWithId]] = {
    import io.circe.generic.semiauto.deriveCodec
    implicit val userId: Codec.AsObject[CiUserId] = deriveCodec[CiUserId]
    Some(deriveCodec[CiWithId])
  }
}
