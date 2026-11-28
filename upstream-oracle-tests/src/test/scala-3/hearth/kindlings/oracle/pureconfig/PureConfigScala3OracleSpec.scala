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

object Pc3Tuples {

  /** 23 elements: more than `Tuple22`, a `TupleXXL` at runtime. */
  type T23 = (
      Int,
      String,
      Int,
      String,
      Int,
      String,
      Int,
      String,
      Int,
      String,
      Int,
      String,
      Int,
      String,
      Int,
      String,
      Int,
      String,
      Int,
      String,
      Int,
      String,
      Boolean
  )

  val t23: T23 =
    (
      0,
      "s1",
      2,
      "s3",
      4,
      "s5",
      6,
      "s7",
      8,
      "s9",
      10,
      "s11",
      12,
      "s13",
      14,
      "s15",
      16,
      "s17",
      18,
      "s19",
      20,
      "s21",
      true
    )

  val t23Hocon: String =
    "[0, s1, 2, s3, 4, s5, 6, s7, 8, s9, 10, s11, 12, s13, 14, s15, 16, s17, 18, s19, 20, s21, true]"

  type Cons = Int *: String *: EmptyTuple
}
final case class Pc3WithT23(values: Pc3Tuples.T23)
final case class Pc3WithEmptyTuple(values: EmptyTuple, other: Int)

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

  private object TuplesK {
    import Pc3Tuples.*
    val t23R: ConfigReader[T23] = KindlingsConfigReader.derived[T23]
    val t23W: ConfigWriter[T23] = KindlingsConfigWriter.derived[T23]
    val withT23R: ConfigReader[Pc3WithT23] = KindlingsConfigReader.derived[Pc3WithT23]
    val withT23W: ConfigWriter[Pc3WithT23] = KindlingsConfigWriter.derived[Pc3WithT23]
    val emptyR: ConfigReader[EmptyTuple] = KindlingsConfigReader.derived[EmptyTuple]
    val emptyW: ConfigWriter[EmptyTuple] = KindlingsConfigWriter.derived[EmptyTuple]
    val withEmptyR: ConfigReader[Pc3WithEmptyTuple] = KindlingsConfigReader.derived[Pc3WithEmptyTuple]
    val withEmptyW: ConfigWriter[Pc3WithEmptyTuple] = KindlingsConfigWriter.derived[Pc3WithEmptyTuple]
    val consR: ConfigReader[Cons] = KindlingsConfigReader.derived[Cons]
    val consW: ConfigWriter[Cons] = KindlingsConfigWriter.derived[Cons]
  }
  private object TuplesU {
    import Pc3Tuples.*
    import pureconfig.generic.semiauto.*
    given t23R: ConfigReader[T23] = deriveReader[T23]
    given t23W: ConfigWriter[T23] = deriveWriter[T23]
    val withT23R: ConfigReader[Pc3WithT23] = deriveReader[Pc3WithT23]
    val withT23W: ConfigWriter[Pc3WithT23] = deriveWriter[Pc3WithT23]
    given emptyR: ConfigReader[EmptyTuple] = deriveReader[EmptyTuple]
    given emptyW: ConfigWriter[EmptyTuple] = deriveWriter[EmptyTuple]
    val withEmptyR: ConfigReader[Pc3WithEmptyTuple] = deriveReader[Pc3WithEmptyTuple]
    val withEmptyW: ConfigWriter[Pc3WithEmptyTuple] = deriveWriter[Pc3WithEmptyTuple]
    val consR: ConfigReader[Cons] = deriveReader[Cons]
    val consW: ConfigWriter[Cons] = deriveWriter[Cons]
  }

  group("tuples which are not TupleN (Scala 3: more than 22 elements, EmptyTuple, *:)") {
    import Pc3Tuples.*

    val broken = t23Hocon.replace("[0,", "[x,").replace("true]", "nope]")

    test("a 23-element tuple is read from and written as a list") {
      compareReads(TuplesK.t23R, TuplesU.t23R)(
        value(t23Hocon),
        value(broken),
        value("[1, a]"),
        value("str"),
        root("_1 = 0"),
        root(""""0" = 0, "1" = s1""")
      )()
      compareWrites(TuplesK.t23W, TuplesU.t23W)(t23)()
      compareReads(TuplesK.withT23R, TuplesU.withT23R)(
        root(s"values = $t23Hocon"),
        root(s"values = $broken"),
        root("values = []"),
        root("")
      )()
      compareWrites(TuplesK.withT23W, TuplesU.withT23W)(Pc3WithT23(t23))()
    }

    test("EmptyTuple is read from and written as an empty list") {
      compareReads(TuplesK.emptyR, TuplesU.emptyR)(value("[]"), value("[1]"), value("1"), root(""))()
      compareWrites(TuplesK.emptyW, TuplesU.emptyW)(EmptyTuple)()
      compareReads(TuplesK.withEmptyR, TuplesU.withEmptyR)(
        root("values = [], other = 1"),
        root("values = [1], other = 1"),
        root("other = 1")
      )()
      compareWrites(TuplesK.withEmptyW, TuplesU.withEmptyW)(Pc3WithEmptyTuple(EmptyTuple, 1))()
    }

    test("a tuple written with *: is read from and written as a list") {
      compareReads(TuplesK.consR, TuplesU.consR)(
        value("[1, a]"),
        value("[1]"),
        value("[x, a]"),
        value("str"),
        root("_1 = 1, _2 = a")
      )()
      compareWrites(TuplesK.consW, TuplesU.consW)(1 *: "a" *: EmptyTuple)()
    }
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
