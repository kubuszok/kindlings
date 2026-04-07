package hearth.kindlings.sconfigderivation

import scala.language.experimental.macros

private[sconfigderivation] trait ConfigReaderCompanionCompat { this: ConfigReader.type =>

  def derive[A](implicit config: SConfig): ConfigReader[A] =
    macro internal.compiletime.ReaderMacros.deriveReaderImpl[A]

  // Note: not implicit on Scala 2 to avoid clashing with the built-in instances
  // (intReader, stringReader, etc.). Users either summon a derived instance
  // explicitly via `ConfigReader.derive[Foo]` and assign it to an implicit val, or
  // use `derives` syntax on Scala 3.
  def derived[A](implicit config: SConfig): ConfigReader[A] =
    macro internal.compiletime.ReaderMacros.deriveReaderImpl[A]
}
