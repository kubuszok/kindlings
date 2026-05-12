package hearth.kindlings.pureconfigderivation.internal.runtime

import com.typesafe.config.ConfigValue
import hearth.kindlings.pureconfigderivation.{KindlingsConfigConvert, KindlingsConfigReader, KindlingsConfigWriter}
import pureconfig.ConfigCursor
import pureconfig.ConfigReader.Result

object PureconfigDerivationFactories {

  def readerInstance[A](fromFn: ConfigCursor => Result[A]): KindlingsConfigReader[A] =
    new KindlingsConfigReader[A] {
      def from(cur: ConfigCursor): Result[A] = fromFn(cur)
    }

  def writerInstance[A](toFn: A => ConfigValue): KindlingsConfigWriter[A] =
    new KindlingsConfigWriter[A] {
      def to(a: A): ConfigValue = toFn(a)
    }

  def convertInstance[A](
      fromFn: ConfigCursor => Result[A],
      toFn: A => ConfigValue
  ): KindlingsConfigConvert[A] =
    new KindlingsConfigConvert[A] {
      def from(cur: ConfigCursor): Result[A] = fromFn(cur)
      def to(a: A): ConfigValue = toFn(a)
    }
}
