package hearth.kindlings.sconfigderivation

import scala.language.experimental.macros

private[sconfigderivation] trait ConfigWriterCompanionCompat { this: ConfigWriter.type =>

  @deprecated("Use .derived instead", "next")
  def derive[A](implicit config: SConfig): ConfigWriter[A] =
    macro internal.compiletime.WriterMacros.deriveWriterImpl[A]

  def derived[A](implicit config: SConfig): ConfigWriter[A] =
    macro internal.compiletime.WriterMacros.deriveWriterImpl[A]
}
