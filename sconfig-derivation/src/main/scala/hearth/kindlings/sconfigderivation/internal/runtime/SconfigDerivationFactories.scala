package hearth.kindlings.sconfigderivation.internal.runtime

import hearth.kindlings.sconfigderivation.{ConfigDecodingError, ConfigReader, ConfigWriter}
import org.ekrich.config.ConfigValue

object SconfigDerivationFactories {

  def readerInstance[A](fromFn: ConfigValue => Either[ConfigDecodingError, A]): ConfigReader[A] =
    new ConfigReader[A] {
      def from(value: ConfigValue): Either[ConfigDecodingError, A] = fromFn(value)
    }

  def writerInstance[A](toFn: A => ConfigValue): ConfigWriter[A] =
    new ConfigWriter[A] {
      def to(value: A): ConfigValue = toFn(value)
    }
}
