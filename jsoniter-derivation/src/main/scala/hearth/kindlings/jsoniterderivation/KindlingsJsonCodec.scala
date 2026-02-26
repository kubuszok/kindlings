package hearth.kindlings.jsoniterderivation

import com.github.plokhotnyuk.jsoniter_scala.core.JsonCodec

trait KindlingsJsonCodec[A] extends JsonCodec[A]
object KindlingsJsonCodec extends KindlingsJsonCodecCompanionCompat {

  sealed trait LogDerivation
  object LogDerivation extends LogDerivation
}
