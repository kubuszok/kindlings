package hearth.kindlings.sconfigderivation

/** Combined reader + writer in a single instance. The hierarchy is what the user explicitly
  * asked for: `Codec extends Reader with Writer`, no wrapping or subtype gymnastics
  * (since we own all three traits in this module).
  */
trait ConfigCodec[A] extends ConfigReader[A] with ConfigWriter[A]
object ConfigCodec extends ConfigCodecCompanionCompat {

  def apply[A](implicit c: ConfigCodec[A]): ConfigCodec[A] = c

  /** Special type — if its implicit is in scope then macros will log the derivation process. */
  sealed trait LogDerivation
  object LogDerivation extends LogDerivation
}
