package hearth.kindlings.oracle.pureconfig

import hearth.MacroSuite
import hearth.kindlings.pureconfigderivation.{KindlingsConfigReader, KindlingsConfigWriter, PureConfig}
import pureconfig.{ConfigFieldMapping, ConfigReader, ConfigWriter, PascalCase, SnakeCase}

import scala.concurrent.duration.*

enum Pc3Color {
  case RainyBlue, SunnyYellow
}

enum Pc3Animal {
  case DogConfig(age: Int)
  case CatConfig(age: Int)
  case BirdConfig(canFly: Boolean)
}
final case class Pc3AnimalCage(animal: Pc3Animal)

final case class Pc3AnimalWrapper(value: Pc3Animal) extends AnyVal
final case class Pc3AnimalDoubleWrapper(value: Pc3AnimalWrapper)

final case class Pc3SocketConfig(
    connectTimeout: FiniteDuration = 5.seconds,
    readTimeout: FiniteDuration = 12.seconds,
    keepAlive: Option[Boolean] = None,
    reuseAddress: Option[Boolean] = None,
    soLinger: Option[Int] = None,
    tcpNoDelay: Option[Boolean] = Some(true),
    receiveBufferSize: Option[Int] = None,
    sendBufferSize: Option[Int] = None
)

/** The Scala 3-only cases of upstream's suites (pureconfig `modules/generic-scala3/src/test`: EnumDerivationSuite,
  * CoproductConvertDerivationSuite, ValueClassSuite, ProductConvertDerivationSuite), through Kindlings and through
  * upstream's `pureconfig.generic.semiauto`.
  */
final class PureConfigScala3OracleSpec extends MacroSuite with PureConfigOracleSupport {

  private val snake: ConfigFieldMapping = ConfigFieldMapping(PascalCase, SnakeCase)

  /** Upstream's `deriveEnumerationReader` / `Writer` is Kindlings' derivation without a discriminator. */
  private object EnumK {
    given PureConfig = PureConfig.default.withWrappedSubtypes
    val colorR: ConfigReader[Pc3Color] = KindlingsConfigReader.derived[Pc3Color]
    val colorW: ConfigWriter[Pc3Color] = KindlingsConfigWriter.derived[Pc3Color]
  }
  private object EnumU {
    import pureconfig.generic.semiauto.*
    val colorR: ConfigReader[Pc3Color] = deriveEnumerationReader[Pc3Color]
    val colorW: ConfigWriter[Pc3Color] = deriveEnumerationWriter[Pc3Color]
  }
  private object SnakeEnumK {
    given PureConfig = PureConfig.default.withWrappedSubtypes.withTransformConstructorNames(snake)
    val colorR: ConfigReader[Pc3Color] = KindlingsConfigReader.derived[Pc3Color]
    val colorW: ConfigWriter[Pc3Color] = KindlingsConfigWriter.derived[Pc3Color]
  }
  private object SnakeEnumU {
    import pureconfig.generic.semiauto.*
    val colorR: ConfigReader[Pc3Color] = deriveEnumerationReader[Pc3Color](snake)
    val colorW: ConfigWriter[Pc3Color] = deriveEnumerationWriter[Pc3Color](snake)
  }

  private object K {
    val animalR: ConfigReader[Pc3Animal] = KindlingsConfigReader.derived[Pc3Animal]
    val animalW: ConfigWriter[Pc3Animal] = KindlingsConfigWriter.derived[Pc3Animal]
    val cageR: ConfigReader[Pc3AnimalCage] = KindlingsConfigReader.derived[Pc3AnimalCage]
    val wrapperR: ConfigReader[Pc3AnimalWrapper] = KindlingsConfigReader.derived[Pc3AnimalWrapper]
    val wrapperW: ConfigWriter[Pc3AnimalWrapper] = KindlingsConfigWriter.derived[Pc3AnimalWrapper]
    val doubleWrapperR: ConfigReader[Pc3AnimalDoubleWrapper] = KindlingsConfigReader.derived[Pc3AnimalDoubleWrapper]
    val doubleWrapperW: ConfigWriter[Pc3AnimalDoubleWrapper] = KindlingsConfigWriter.derived[Pc3AnimalDoubleWrapper]
    val socketR: ConfigReader[Pc3SocketConfig] = KindlingsConfigReader.derived[Pc3SocketConfig]
    val lazyAR: ConfigReader[PcLazyA] = KindlingsConfigReader.derived[PcLazyA]
    val lazyBR: ConfigReader[PcLazyB] = KindlingsConfigReader.derived[PcLazyB]
    val lazyCR: ConfigReader[PcLazyC] = KindlingsConfigReader.derived[PcLazyC]
  }
  private object U {
    import pureconfig.generic.semiauto.*
    given animalR: ConfigReader[Pc3Animal] = deriveReader[Pc3Animal]
    given animalW: ConfigWriter[Pc3Animal] = deriveWriter[Pc3Animal]
    val cageR: ConfigReader[Pc3AnimalCage] = deriveReader[Pc3AnimalCage]
    given wrapperR: ConfigReader[Pc3AnimalWrapper] = deriveReader[Pc3AnimalWrapper]
    given wrapperW: ConfigWriter[Pc3AnimalWrapper] = deriveWriter[Pc3AnimalWrapper]
    val doubleWrapperR: ConfigReader[Pc3AnimalDoubleWrapper] = deriveReader[Pc3AnimalDoubleWrapper]
    val doubleWrapperW: ConfigWriter[Pc3AnimalDoubleWrapper] = deriveWriter[Pc3AnimalDoubleWrapper]
    val socketR: ConfigReader[Pc3SocketConfig] = deriveReader[Pc3SocketConfig]
    given lazyAR: ConfigReader[PcLazyA] = deriveReader[PcLazyA]
    val lazyBR: ConfigReader[PcLazyB] = deriveReader[PcLazyB]
    val lazyCR: ConfigReader[PcLazyC] = deriveReader[PcLazyC]
  }

  group("upstream EnumDerivationSuite (Scala 3 enums)") {

    test("deriveEnumerationReader / deriveEnumerationWriter") {
      compareReads(EnumK.colorR, EnumU.colorR)(
        value("rainy-blue"),
        value("sunny-yellow"),
        value("blue"),
        value("RainyBlue"),
        root("type = person, name = John, surname = Doe"),
        value("[rainy-blue]")
      )()
      compareWrites(EnumK.colorW, EnumU.colorW)(Pc3Color.RainyBlue, Pc3Color.SunnyYellow)()
    }

    test("customizable name mapping") {
      compareReads(SnakeEnumK.colorR, SnakeEnumU.colorR)(
        value("rainy_blue"),
        value("sunny_yellow"),
        value("rainy-blue")
      )()
      compareWrites(SnakeEnumK.colorW, SnakeEnumU.colorW)(Pc3Color.RainyBlue, Pc3Color.SunnyYellow)()
    }
  }

  group("upstream CoproductConvertDerivationSuite (Scala 3 enums with fields)") {

    test("read / write disambiguation information by default") {
      compareReads(K.animalR, U.animalR)(
        root("type = dog-config, age = 2"),
        root("can-fly = true"),
        root("can-fly = true, type = car-config"),
        root("type = bird-config, age = 2"),
        value("dog-config")
      )()
      compareWrites(K.animalW, U.animalW)(Pc3Animal.DogConfig(2), Pc3Animal.CatConfig(3), Pc3Animal.BirdConfig(true))()
    }

    test("a proper failure when a coproduct config is missing") {
      compareReads(K.cageR, U.cageR)(root(""), root("animal { type = dog-config }"))()
    }
  }

  group("upstream ValueClassSuite (Scala 3)") {

    test("a value class of an enum, and a case class of it") {
      compareReads(K.wrapperR, U.wrapperR)(
        root("type = dog-config, age = 1"),
        root("value { type = dog-config, age = 1 }")
      )()
      compareWrites(K.wrapperW, U.wrapperW)(Pc3AnimalWrapper(Pc3Animal.DogConfig(1)))()
      compareReads(K.doubleWrapperR, U.doubleWrapperR)(
        root("value { type = dog-config, age = 1 }"),
        root("type = dog-config, age = 1")
      )()
      compareWrites(K.doubleWrapperW, U.doubleWrapperW)(
        Pc3AnimalDoubleWrapper(Pc3AnimalWrapper(Pc3Animal.DogConfig(1)))
      )()
    }
  }

  group("upstream ProductConvertDerivationSuite (Scala 3)") {

    test("invoke defaults when a key is not in the configuration") {
      compareReads(K.socketR, U.socketR)(
        root(""),
        root("connect-timeout = 1s, keep-alive = true, tcp-no-delay = null"),
        root("connect-timeout = soon, so-linger = x")
      )()
    }

    test("evaluate defaults lazily") {
      compareReads(K.lazyAR, U.lazyAR)(root("foo = bar"))()
      compareReads(K.lazyBR, U.lazyBR)(root(""), root("bar { foo = x }"))()
      compareReads(K.lazyCR, U.lazyCR)(root("baz { foo = bar }, foo = bar"))()
    }
  }
}
