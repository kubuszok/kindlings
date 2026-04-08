package hearth.kindlings.pureconfigderivation

import pureconfig.ConfigWriter
import scala.language.experimental.macros

private[pureconfigderivation] trait KindlingsConfigWriterCompanionCompat { this: KindlingsConfigWriter.type =>

  def derive[A](implicit config: PureConfig): ConfigWriter[A] =
    macro internal.compiletime.WriterMacros.deriveWriterImpl[A]

  implicit def derived[A](implicit config: PureConfig): KindlingsConfigWriter[A] =
    macro internal.compiletime.WriterMacros.deriveKindlingsWriterImpl[A]
}
