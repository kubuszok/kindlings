package hearth.kindlings.sconfigderivation

import org.ekrich.config.{ConfigValue, ConfigValueType}

/** Type class for decoding a [[org.ekrich.config.ConfigValue]] into a value of type `A`.
  *
  * Unlike [[hearth.kindlings.pureconfigderivation.KindlingsConfigReader]], this trait
  * is owned by the kindlings sconfig module — it does not extend any third-party type,
  * and the `ConfigCodec[A]` companion forms a clean three-trait hierarchy
  * (`Reader`, `Writer`, `Codec extends Reader with Writer`).
  */
trait ConfigReader[A] {
  def from(value: ConfigValue): Either[ConfigDecodingError, A]

  def map[B](f: A => B): ConfigReader[B] = new ConfigReader[B] {
    def from(value: ConfigValue): Either[ConfigDecodingError, B] = ConfigReader.this.from(value).map(f)
  }

  def emap[B](f: A => Either[String, B]): ConfigReader[B] = new ConfigReader[B] {
    def from(value: ConfigValue): Either[ConfigDecodingError, B] = ConfigReader.this.from(value).flatMap { a =>
      f(a).left.map(msg => ConfigDecodingError.CannotConvert(Nil, msg))
    }
  }
}

/** Companion. Built-in instances live here directly (not in mixed-in traits) because
  * Scala 2's macro `Implicits.search` is unreliable about picking up `implicit val`s
  * declared in inherited traits — defining them in the companion body itself works on
  * both Scala 2 and Scala 3.
  */
object ConfigReader extends ConfigReaderCompanionCompat {

  def apply[A](implicit r: ConfigReader[A]): ConfigReader[A] = r

  /** Special type — if its implicit is in scope then macros will log the derivation process. */
  sealed trait LogDerivation
  object LogDerivation extends LogDerivation

  // --- Built-in primitive instances ---

  private def wrongType(expected: ConfigValueType, value: ConfigValue): ConfigDecodingError =
    ConfigDecodingError.WrongType(Nil, expected.toString, value.valueType.toString)

  implicit val stringReader: ConfigReader[String] = new ConfigReader[String] {
    def from(value: ConfigValue): Either[ConfigDecodingError, String] =
      value.valueType match {
        case ConfigValueType.STRING  => Right(value.unwrapped.asInstanceOf[String])
        case ConfigValueType.NUMBER  => Right(value.unwrapped.toString)
        case ConfigValueType.BOOLEAN => Right(value.unwrapped.toString)
        case _                       => Left(wrongType(ConfigValueType.STRING, value))
      }
  }

  implicit val booleanReader: ConfigReader[Boolean] = new ConfigReader[Boolean] {
    def from(value: ConfigValue): Either[ConfigDecodingError, Boolean] =
      value.valueType match {
        case ConfigValueType.BOOLEAN => Right(value.unwrapped.asInstanceOf[java.lang.Boolean].booleanValue())
        case _                       => Left(wrongType(ConfigValueType.BOOLEAN, value))
      }
  }

  implicit val intReader: ConfigReader[Int] = new ConfigReader[Int] {
    def from(value: ConfigValue): Either[ConfigDecodingError, Int] =
      value.valueType match {
        case ConfigValueType.NUMBER => Right(value.unwrapped.asInstanceOf[Number].intValue())
        case _                      => Left(wrongType(ConfigValueType.NUMBER, value))
      }
  }

  implicit val longReader: ConfigReader[Long] = new ConfigReader[Long] {
    def from(value: ConfigValue): Either[ConfigDecodingError, Long] =
      value.valueType match {
        case ConfigValueType.NUMBER => Right(value.unwrapped.asInstanceOf[Number].longValue())
        case _                      => Left(wrongType(ConfigValueType.NUMBER, value))
      }
  }

  implicit val shortReader: ConfigReader[Short] = new ConfigReader[Short] {
    def from(value: ConfigValue): Either[ConfigDecodingError, Short] =
      value.valueType match {
        case ConfigValueType.NUMBER => Right(value.unwrapped.asInstanceOf[Number].shortValue())
        case _                      => Left(wrongType(ConfigValueType.NUMBER, value))
      }
  }

  implicit val byteReader: ConfigReader[Byte] = new ConfigReader[Byte] {
    def from(value: ConfigValue): Either[ConfigDecodingError, Byte] =
      value.valueType match {
        case ConfigValueType.NUMBER => Right(value.unwrapped.asInstanceOf[Number].byteValue())
        case _                      => Left(wrongType(ConfigValueType.NUMBER, value))
      }
  }

  implicit val floatReader: ConfigReader[Float] = new ConfigReader[Float] {
    def from(value: ConfigValue): Either[ConfigDecodingError, Float] =
      value.valueType match {
        case ConfigValueType.NUMBER => Right(value.unwrapped.asInstanceOf[Number].floatValue())
        case _                      => Left(wrongType(ConfigValueType.NUMBER, value))
      }
  }

  implicit val doubleReader: ConfigReader[Double] = new ConfigReader[Double] {
    def from(value: ConfigValue): Either[ConfigDecodingError, Double] =
      value.valueType match {
        case ConfigValueType.NUMBER => Right(value.unwrapped.asInstanceOf[Number].doubleValue())
        case _                      => Left(wrongType(ConfigValueType.NUMBER, value))
      }
  }

  implicit val charReader: ConfigReader[Char] = new ConfigReader[Char] {
    def from(value: ConfigValue): Either[ConfigDecodingError, Char] =
      value.valueType match {
        case ConfigValueType.STRING =>
          val s = value.unwrapped.asInstanceOf[String]
          if (s.length == 1) Right(s.charAt(0))
          else Left(ConfigDecodingError.CannotConvert(Nil, s"Expected single character but got '$s'"))
        case _ => Left(wrongType(ConfigValueType.STRING, value))
      }
  }

  implicit val bigDecimalReader: ConfigReader[BigDecimal] = new ConfigReader[BigDecimal] {
    def from(value: ConfigValue): Either[ConfigDecodingError, BigDecimal] =
      value.valueType match {
        case ConfigValueType.NUMBER => Right(BigDecimal(value.unwrapped.toString))
        case ConfigValueType.STRING =>
          try Right(BigDecimal(value.unwrapped.asInstanceOf[String]))
          catch { case e: NumberFormatException => Left(ConfigDecodingError.CannotConvert(Nil, e.getMessage)) }
        case _ => Left(wrongType(ConfigValueType.NUMBER, value))
      }
  }

  implicit val bigIntReader: ConfigReader[BigInt] = new ConfigReader[BigInt] {
    def from(value: ConfigValue): Either[ConfigDecodingError, BigInt] =
      value.valueType match {
        case ConfigValueType.NUMBER => Right(BigInt(value.unwrapped.toString))
        case ConfigValueType.STRING =>
          try Right(BigInt(value.unwrapped.asInstanceOf[String]))
          catch { case e: NumberFormatException => Left(ConfigDecodingError.CannotConvert(Nil, e.getMessage)) }
        case _ => Left(wrongType(ConfigValueType.NUMBER, value))
      }
  }

  implicit def optionReader[A](implicit inner: ConfigReader[A]): ConfigReader[Option[A]] =
    new ConfigReader[Option[A]] {
      def from(value: ConfigValue): Either[ConfigDecodingError, Option[A]] =
        if (value == null || value.valueType == ConfigValueType.NULL) Right(None)
        else inner.from(value).map(Some(_))
    }

  // Note: collection (List, Vector, Seq, Set) and Map[String, A] readers are NOT exposed
  // as implicit instances on the companion. Such generic implicits would cause Scala 2's
  // implicit search to diverge ("starting with method mapReader") whenever it tries to
  // resolve any unrelated `ConfigReader[T]`. Instead, the macro's `HandleAsCollectionRule`
  // and `HandleAsMapRule` synthesise collection readers directly from the structural
  // pattern, which works for all standard collection types without needing companion-level
  // implicits.
}
