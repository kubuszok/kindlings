package hearth.kindlings.pureconfigderivation

import scala.language.experimental.macros

private[pureconfigderivation] trait KindlingsConfigWriterCompanionCompat { this: KindlingsConfigWriter.type =>

  implicit def derived[A](implicit config: PureConfig): KindlingsConfigWriter[A] =
    macro internal.compiletime.WriterMacros.deriveKindlingsWriterImpl[A]
}
