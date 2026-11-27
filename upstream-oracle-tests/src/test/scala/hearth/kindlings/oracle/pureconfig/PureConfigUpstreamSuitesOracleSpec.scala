package hearth.kindlings.oracle.pureconfig

import com.typesafe.config.{ConfigRenderOptions, ConfigValue, ConfigValueFactory}
import hearth.MacroSuite
import hearth.kindlings.pureconfigderivation.{
  KindlingsConfigReader,
  KindlingsConfigWriter,
  KindlingsCoproductHint,
  KindlingsProductHint,
  PureConfig
}
import pureconfig.{
  CamelCase,
  ConfigConvert,
  ConfigCursor,
  ConfigFieldMapping,
  ConfigReader,
  ConfigWriter,
  PascalCase,
  ReadsMissingKeys,
  WritesMissingKeys
}
import pureconfig.ConfigConvert.catchReadError
import pureconfig.generic.{FieldCoproductHint, FirstSuccessCoproductHint, ProductHint}

/** The cases of upstream's own derivation suites (pureconfig `modules/generic/src/test` on Scala 2,
  * `modules/generic-scala3/src/test` on Scala 3), run through Kindlings and through upstream's `semiauto` and compared.
  * Each test names the upstream suite and case it mirrors.
  */
final class PureConfigUpstreamSuitesOracleSpec extends MacroSuite with PureConfigOracleSupport {

  // --- instances shared by both sides --------------------------------------------------------------------------------

  /** ProductConvertersSuite "allow custom ConfigReaders to handle missing keys". */
  private val defaultingIntReader: ConfigReader[Int] = new ConfigReader[Int] with ReadsMissingKeys {
    def from(cur: ConfigCursor): ConfigReader.Result[Int] =
      cur.asConfigValue.fold(
        _ => Right(42),
        v => cur.scopeFailure(catchReadError(_.toInt).apply(v.render(ConfigRenderOptions.concise)))
      )
  }

  /** ProductConvertersSuite "allow custom ConfigWriters to handle missing keys". */
  private val nonZeroIntWriter: ConfigWriter[Int] = new ConfigWriter[Int] with WritesMissingKeys[Int] {
    def to(v: Int): ConfigValue = ConfigValueFactory.fromAnyRef(v)
    def toOpt(a: Int): Option[ConfigValue] = if (a == 0) None else Some(to(a))
  }

  /** ProductConvertersSuite "return a KeyNotFound when a custom convert is used and when a key is not in the
    * configuration".
    */
  private val constantInnerConvert: ConfigConvert[PcInner] = new ConfigConvert[PcInner] {
    def from(cur: ConfigCursor): ConfigReader.Result[PcInner] = Right(PcInner(42, 42))
    def to(conf: PcInner): ConfigValue = ConfigValueFactory.fromAnyRef(s"inner ${conf.e}")
  }

  private val upperCase: ConfigFieldMapping = ConfigFieldMapping(_.toUpperCase)

  /** CoproductHintSuite: `name.dropRight("Config".length)` (and the fixtures' `Pc` prefix). */
  private val animalName: String => String = _.drop(2).dropRight("Config".length)

  // --- derivations: defaults -----------------------------------------------------------------------------------------

  private object K {
    val emptyR: ConfigReader[PcEmptyConf] = KindlingsConfigReader.derived[PcEmptyConf]
    val emptyW: ConfigWriter[PcEmptyConf] = KindlingsConfigWriter.derived[PcEmptyConf]
    val objR: ConfigReader[PcObjConf.type] = KindlingsConfigReader.derived[PcObjConf.type]
    val objW: ConfigWriter[PcObjConf.type] = KindlingsConfigWriter.derived[PcObjConf.type]
    val manyDefaultsR: ConfigReader[PcManyDefaults] = KindlingsConfigReader.derived[PcManyDefaults]
    val twoIntsR: ConfigReader[PcTwoInts] = KindlingsConfigReader.derived[PcTwoInts]
    val twoIntsW: ConfigWriter[PcTwoInts] = KindlingsConfigWriter.derived[PcTwoInts]
    val optIntW: ConfigWriter[PcOptInt] = KindlingsConfigWriter.derived[PcOptInt]
    val flatR: ConfigReader[PcFlat] = KindlingsConfigReader.derived[PcFlat]
    val flatW: ConfigWriter[PcFlat] = KindlingsConfigWriter.derived[PcFlat]
    val tupledR: ConfigReader[PcTupled] = KindlingsConfigReader.derived[PcTupled]
    val tupledW: ConfigWriter[PcTupled] = KindlingsConfigWriter.derived[PcTupled]
    val tupled3R: ConfigReader[PcTupled3] = KindlingsConfigReader.derived[PcTupled3]
    val tupled3W: ConfigWriter[PcTupled3] = KindlingsConfigWriter.derived[PcTupled3]
    val pairR: ConfigReader[(String, Int)] = KindlingsConfigReader.derived[(String, Int)]
    val pairW: ConfigWriter[(String, Int)] = KindlingsConfigWriter.derived[(String, Int)]
    val tripleR: ConfigReader[(Int, Int, Int)] = KindlingsConfigReader.derived[(Int, Int, Int)]
    val tuple1R: ConfigReader[Tuple1[Int]] = KindlingsConfigReader.derived[Tuple1[Int]]
    val tuple1W: ConfigWriter[Tuple1[Int]] = KindlingsConfigWriter.derived[Tuple1[Int]]
    val recR: ConfigReader[PcRec] = KindlingsConfigReader.derived[PcRec]
    val recW: ConfigWriter[PcRec] = KindlingsConfigWriter.derived[PcRec]
    val genericDefaultsR: ConfigReader[PcGenericDefaults[Long]] = KindlingsConfigReader.derived[PcGenericDefaults[Long]]
    val animalR: ConfigReader[PcAnimalConfig] = KindlingsConfigReader.derived[PcAnimalConfig]
    val animalW: ConfigWriter[PcAnimalConfig] = KindlingsConfigWriter.derived[PcAnimalConfig]
    val cageR: ConfigReader[PcAnimalCage] = KindlingsConfigReader.derived[PcAnimalCage]
    val enclosingAR: ConfigReader[PcEnclosingA] = KindlingsConfigReader.derived[PcEnclosingA]
    val camelR: ConfigReader[PcCamelOuter] = KindlingsConfigReader.derived[PcCamelOuter]
    val camelW: ConfigWriter[PcCamelOuter] = KindlingsConfigWriter.derived[PcCamelOuter]
    val intWrapperR: ConfigReader[PcIntWrapper] = KindlingsConfigReader.derived[PcIntWrapper]
    val intWrapperW: ConfigWriter[PcIntWrapper] = KindlingsConfigWriter.derived[PcIntWrapper]
    val shapeWrapperR: ConfigReader[PcShapeWrapper] = KindlingsConfigReader.derived[PcShapeWrapper]
    val shapeWrapperW: ConfigWriter[PcShapeWrapper] = KindlingsConfigWriter.derived[PcShapeWrapper]
    val doubleWrapperR: ConfigReader[PcShapeDoubleWrapper] = KindlingsConfigReader.derived[PcShapeDoubleWrapper]
    val doubleWrapperW: ConfigWriter[PcShapeDoubleWrapper] = KindlingsConfigWriter.derived[PcShapeDoubleWrapper]
    val customR: ConfigReader[PcCustomCaseClass] = KindlingsConfigReader.derived[PcCustomCaseClass]
    val customW: ConfigWriter[PcCustomCaseClass] = KindlingsConfigWriter.derived[PcCustomCaseClass]
  }

  private object U {
    import pureconfig.generic.semiauto.*
    lazy val emptyR: ConfigReader[PcEmptyConf] = deriveReader[PcEmptyConf]
    lazy val emptyW: ConfigWriter[PcEmptyConf] = deriveWriter[PcEmptyConf]
    lazy val objR: ConfigReader[PcObjConf.type] = deriveReader[PcObjConf.type]
    lazy val objW: ConfigWriter[PcObjConf.type] = deriveWriter[PcObjConf.type]
    implicit lazy val innerR: ConfigReader[PcInner] = deriveReader[PcInner]
    lazy val manyDefaultsR: ConfigReader[PcManyDefaults] = deriveReader[PcManyDefaults]
    lazy val twoIntsR: ConfigReader[PcTwoInts] = deriveReader[PcTwoInts]
    lazy val twoIntsW: ConfigWriter[PcTwoInts] = deriveWriter[PcTwoInts]
    lazy val optIntW: ConfigWriter[PcOptInt] = deriveWriter[PcOptInt]
    lazy val flatR: ConfigReader[PcFlat] = deriveReader[PcFlat]
    lazy val flatW: ConfigWriter[PcFlat] = deriveWriter[PcFlat]
    implicit lazy val boolIntR: ConfigReader[(Boolean, Int)] = deriveReader[(Boolean, Int)]
    implicit lazy val boolIntW: ConfigWriter[(Boolean, Int)] = deriveWriter[(Boolean, Int)]
    lazy val tupledR: ConfigReader[PcTupled] = deriveReader[PcTupled]
    lazy val tupledW: ConfigWriter[PcTupled] = deriveWriter[PcTupled]
    implicit lazy val longStringR: ConfigReader[(Long, String)] = deriveReader[(Long, String)]
    implicit lazy val longStringW: ConfigWriter[(Long, String)] = deriveWriter[(Long, String)]
    implicit lazy val nestedTupleR: ConfigReader[(Int, (Long, String), Boolean)] =
      deriveReader[(Int, (Long, String), Boolean)]
    implicit lazy val nestedTupleW: ConfigWriter[(Int, (Long, String), Boolean)] =
      deriveWriter[(Int, (Long, String), Boolean)]
    lazy val tupled3R: ConfigReader[PcTupled3] = deriveReader[PcTupled3]
    lazy val tupled3W: ConfigWriter[PcTupled3] = deriveWriter[PcTupled3]
    lazy val pairR: ConfigReader[(String, Int)] = deriveReader[(String, Int)]
    lazy val pairW: ConfigWriter[(String, Int)] = deriveWriter[(String, Int)]
    lazy val tripleR: ConfigReader[(Int, Int, Int)] = deriveReader[(Int, Int, Int)]
    lazy val tuple1R: ConfigReader[Tuple1[Int]] = deriveReader[Tuple1[Int]]
    lazy val tuple1W: ConfigWriter[Tuple1[Int]] = deriveWriter[Tuple1[Int]]
    implicit lazy val recR: ConfigReader[PcRec] = deriveReader[PcRec]
    implicit lazy val recW: ConfigWriter[PcRec] = deriveWriter[PcRec]
    lazy val genericDefaultsR: ConfigReader[PcGenericDefaults[Long]] = deriveReader[PcGenericDefaults[Long]]
    implicit lazy val dogR: ConfigReader[PcDogConfig] = deriveReader[PcDogConfig]
    implicit lazy val dogW: ConfigWriter[PcDogConfig] = deriveWriter[PcDogConfig]
    implicit lazy val catR: ConfigReader[PcCatConfig] = deriveReader[PcCatConfig]
    implicit lazy val catW: ConfigWriter[PcCatConfig] = deriveWriter[PcCatConfig]
    implicit lazy val birdR: ConfigReader[PcBirdConfig] = deriveReader[PcBirdConfig]
    implicit lazy val birdW: ConfigWriter[PcBirdConfig] = deriveWriter[PcBirdConfig]
    implicit lazy val animalR: ConfigReader[PcAnimalConfig] = deriveReader[PcAnimalConfig]
    implicit lazy val animalW: ConfigWriter[PcAnimalConfig] = deriveWriter[PcAnimalConfig]
    lazy val cageR: ConfigReader[PcAnimalCage] = deriveReader[PcAnimalCage]
    implicit lazy val aa1R: ConfigReader[PcAA1] = deriveReader[PcAA1]
    implicit lazy val aa2R: ConfigReader[PcAA2] = deriveReader[PcAA2]
    implicit lazy val aR: ConfigReader[PcA] = deriveReader[PcA]
    lazy val enclosingAR: ConfigReader[PcEnclosingA] = deriveReader[PcEnclosingA]
    implicit lazy val camelInnerR: ConfigReader[PcCamelInner] = deriveReader[PcCamelInner]
    implicit lazy val camelInnerW: ConfigWriter[PcCamelInner] = deriveWriter[PcCamelInner]
    lazy val camelR: ConfigReader[PcCamelOuter] = deriveReader[PcCamelOuter]
    lazy val camelW: ConfigWriter[PcCamelOuter] = deriveWriter[PcCamelOuter]
    lazy val intWrapperR: ConfigReader[PcIntWrapper] = deriveReader[PcIntWrapper]
    lazy val intWrapperW: ConfigWriter[PcIntWrapper] = deriveWriter[PcIntWrapper]
    implicit lazy val circleR: ConfigReader[PcCircle] = deriveReader[PcCircle]
    implicit lazy val circleW: ConfigWriter[PcCircle] = deriveWriter[PcCircle]
    implicit lazy val rectangleR: ConfigReader[PcRectangle] = deriveReader[PcRectangle]
    implicit lazy val rectangleW: ConfigWriter[PcRectangle] = deriveWriter[PcRectangle]
    implicit lazy val pcEmptyR: ConfigReader[PcEmpty.type] = deriveReader[PcEmpty.type]
    implicit lazy val pcEmptyW: ConfigWriter[PcEmpty.type] = deriveWriter[PcEmpty.type]
    implicit lazy val shapeR: ConfigReader[PcShape] = deriveReader[PcShape]
    implicit lazy val shapeW: ConfigWriter[PcShape] = deriveWriter[PcShape]
    implicit lazy val shapeWrapperR: ConfigReader[PcShapeWrapper] = deriveReader[PcShapeWrapper]
    implicit lazy val shapeWrapperW: ConfigWriter[PcShapeWrapper] = deriveWriter[PcShapeWrapper]
    lazy val doubleWrapperR: ConfigReader[PcShapeDoubleWrapper] = deriveReader[PcShapeDoubleWrapper]
    lazy val doubleWrapperW: ConfigWriter[PcShapeDoubleWrapper] = deriveWriter[PcShapeDoubleWrapper]
    lazy val customR: ConfigReader[PcCustomCaseClass] = deriveReader[PcCustomCaseClass]
    lazy val customW: ConfigWriter[PcCustomCaseClass] = deriveWriter[PcCustomCaseClass]
  }

  private object CompanionK {
    val hintedR: ConfigReader[PcCompanionHinted] = KindlingsConfigReader.derived[PcCompanionHinted]
    val hintedW: ConfigWriter[PcCompanionHinted] = KindlingsConfigWriter.derived[PcCompanionHinted]
    val familyR: ConfigReader[PcCompanionFamily] = KindlingsConfigReader.derived[PcCompanionFamily]
    val familyW: ConfigWriter[PcCompanionFamily] = KindlingsConfigWriter.derived[PcCompanionFamily]
  }

  private object CompanionU {
    import pureconfig.generic.semiauto.*
    lazy val hintedR: ConfigReader[PcCompanionHinted] = deriveReader[PcCompanionHinted]
    lazy val hintedW: ConfigWriter[PcCompanionHinted] = deriveWriter[PcCompanionHinted]
    implicit lazy val leafR: ConfigReader[PcCompanionLeaf] = deriveReader[PcCompanionLeaf]
    implicit lazy val leafW: ConfigWriter[PcCompanionLeaf] = deriveWriter[PcCompanionLeaf]
    implicit lazy val objectR: ConfigReader[PcCompanionObject.type] = deriveReader[PcCompanionObject.type]
    implicit lazy val objectW: ConfigWriter[PcCompanionObject.type] = deriveWriter[PcCompanionObject.type]
    lazy val familyR: ConfigReader[PcCompanionFamily] = deriveReader[PcCompanionFamily]
    lazy val familyW: ConfigWriter[PcCompanionFamily] = deriveWriter[PcCompanionFamily]
  }

  // --- derivations: hints --------------------------------------------------------------------------------------------

  private object HintsK {
    implicit val emptyHint: KindlingsProductHint[PcEmptyConf] =
      KindlingsProductHint[PcEmptyConf](allowUnknownKeys = false)
    implicit val objHint: KindlingsProductHint[PcObjConf.type] =
      KindlingsProductHint[PcObjConf.type](allowUnknownKeys = false)
    implicit val noDefaultsHint: KindlingsProductHint[PcManyDefaults] =
      KindlingsProductHint[PcManyDefaults](useDefaults = false)
    implicit val upperHint: KindlingsProductHint[PcTwoInts] = KindlingsProductHint[PcTwoInts](upperCase)
    implicit val outerOnlyHint: KindlingsProductHint[PcCamelOuter] =
      KindlingsProductHint[PcCamelOuter](ConfigFieldMapping(CamelCase, CamelCase))
    implicit val animalHint: KindlingsCoproductHint.Field[PcAnimalConfig] =
      KindlingsCoproductHint.Field[PcAnimalConfig]("which-animal", animalName)
    implicit val ambiguousHint: KindlingsCoproductHint.Field[PcAmbiguous] =
      KindlingsCoproductHint.Field[PcAmbiguous]("typ")
    val emptyR: ConfigReader[PcEmptyConf] = KindlingsConfigReader.derived[PcEmptyConf]
    val objR: ConfigReader[PcObjConf.type] = KindlingsConfigReader.derived[PcObjConf.type]
    val manyDefaultsR: ConfigReader[PcManyDefaults] = KindlingsConfigReader.derived[PcManyDefaults]
    val twoIntsR: ConfigReader[PcTwoInts] = KindlingsConfigReader.derived[PcTwoInts]
    val twoIntsW: ConfigWriter[PcTwoInts] = KindlingsConfigWriter.derived[PcTwoInts]
    val camelR: ConfigReader[PcCamelOuter] = KindlingsConfigReader.derived[PcCamelOuter]
    val camelW: ConfigWriter[PcCamelOuter] = KindlingsConfigWriter.derived[PcCamelOuter]
    val animalR: ConfigReader[PcAnimalConfig] = KindlingsConfigReader.derived[PcAnimalConfig]
    val animalW: ConfigWriter[PcAnimalConfig] = KindlingsConfigWriter.derived[PcAnimalConfig]
    val ambiguousR: ConfigReader[PcAmbiguous] = KindlingsConfigReader.derived[PcAmbiguous]
    val ambiguousW: ConfigWriter[PcAmbiguous] = KindlingsConfigWriter.derived[PcAmbiguous]
  }

  private object HintsU {
    import pureconfig.generic.semiauto.*
    implicit val emptyHint: ProductHint[PcEmptyConf] = ProductHint[PcEmptyConf](allowUnknownKeys = false)
    implicit val objHint: ProductHint[PcObjConf.type] = ProductHint[PcObjConf.type](allowUnknownKeys = false)
    implicit val noDefaultsHint: ProductHint[PcManyDefaults] = ProductHint[PcManyDefaults](useDefaultArgs = false)
    implicit val upperHint: ProductHint[PcTwoInts] = ProductHint[PcTwoInts](upperCase)
    implicit val outerOnlyHint: ProductHint[PcCamelOuter] =
      ProductHint[PcCamelOuter](ConfigFieldMapping(CamelCase, CamelCase))
    implicit val animalHint: FieldCoproductHint[PcAnimalConfig] =
      new FieldCoproductHint[PcAnimalConfig]("which-animal") {
        override def fieldValue(name: String): String = animalName(name)
      }
    implicit val ambiguousHint: FieldCoproductHint[PcAmbiguous] = new FieldCoproductHint[PcAmbiguous]("typ")
    lazy val emptyR: ConfigReader[PcEmptyConf] = deriveReader[PcEmptyConf]
    lazy val objR: ConfigReader[PcObjConf.type] = deriveReader[PcObjConf.type]
    implicit lazy val innerR: ConfigReader[PcInner] = deriveReader[PcInner]
    lazy val manyDefaultsR: ConfigReader[PcManyDefaults] = deriveReader[PcManyDefaults]
    lazy val twoIntsR: ConfigReader[PcTwoInts] = deriveReader[PcTwoInts]
    lazy val twoIntsW: ConfigWriter[PcTwoInts] = deriveWriter[PcTwoInts]
    implicit lazy val camelInnerR: ConfigReader[PcCamelInner] = deriveReader[PcCamelInner]
    implicit lazy val camelInnerW: ConfigWriter[PcCamelInner] = deriveWriter[PcCamelInner]
    lazy val camelR: ConfigReader[PcCamelOuter] = deriveReader[PcCamelOuter]
    lazy val camelW: ConfigWriter[PcCamelOuter] = deriveWriter[PcCamelOuter]
    implicit lazy val dogR: ConfigReader[PcDogConfig] = deriveReader[PcDogConfig]
    implicit lazy val dogW: ConfigWriter[PcDogConfig] = deriveWriter[PcDogConfig]
    implicit lazy val catR: ConfigReader[PcCatConfig] = deriveReader[PcCatConfig]
    implicit lazy val catW: ConfigWriter[PcCatConfig] = deriveWriter[PcCatConfig]
    implicit lazy val birdR: ConfigReader[PcBirdConfig] = deriveReader[PcBirdConfig]
    implicit lazy val birdW: ConfigWriter[PcBirdConfig] = deriveWriter[PcBirdConfig]
    lazy val animalR: ConfigReader[PcAnimalConfig] = deriveReader[PcAnimalConfig]
    lazy val animalW: ConfigWriter[PcAnimalConfig] = deriveWriter[PcAnimalConfig]
    implicit lazy val ambiguousConfR: ConfigReader[PcAmbiguousConf] = deriveReader[PcAmbiguousConf]
    implicit lazy val ambiguousConfW: ConfigWriter[PcAmbiguousConf] = deriveWriter[PcAmbiguousConf]
    lazy val ambiguousR: ConfigReader[PcAmbiguous] = deriveReader[PcAmbiguous]
    lazy val ambiguousW: ConfigWriter[PcAmbiguous] = deriveWriter[PcAmbiguous]
  }

  /** ProductHintSuite: `implicit def productHint[A] = ProductHint[A](ConfigFieldMapping(CamelCase, PascalCase))` — a
    * mapping for every type, which is Kindlings' global `PureConfig`.
    */
  private object PascalK {
    implicit val config: PureConfig = PureConfig.default.withPascalCaseMemberNames
    val camelR: ConfigReader[PcCamelOuter] = KindlingsConfigReader.derived[PcCamelOuter]
    val camelW: ConfigWriter[PcCamelOuter] = KindlingsConfigWriter.derived[PcCamelOuter]
  }

  private object PascalU {
    import pureconfig.generic.semiauto.*
    implicit def productHint[A]: ProductHint[A] = ProductHint[A](ConfigFieldMapping(CamelCase, PascalCase))
    implicit lazy val camelInnerR: ConfigReader[PcCamelInner] = deriveReader[PcCamelInner]
    implicit lazy val camelInnerW: ConfigWriter[PcCamelInner] = deriveWriter[PcCamelInner]
    lazy val camelR: ConfigReader[PcCamelOuter] = deriveReader[PcCamelOuter]
    lazy val camelW: ConfigWriter[PcCamelOuter] = deriveWriter[PcCamelOuter]
  }

  private object FirstSuccessK {
    implicit val animalHint: KindlingsCoproductHint.FirstSuccess[PcAnimalConfig] = KindlingsCoproductHint.FirstSuccess()
    val animalR: ConfigReader[PcAnimalConfig] = KindlingsConfigReader.derived[PcAnimalConfig]
    val animalW: ConfigWriter[PcAnimalConfig] = KindlingsConfigWriter.derived[PcAnimalConfig]
  }

  private object FirstSuccessU {
    import pureconfig.generic.semiauto.*
    implicit val animalHint: FirstSuccessCoproductHint[PcAnimalConfig] = new FirstSuccessCoproductHint[PcAnimalConfig]
    implicit lazy val dogR: ConfigReader[PcDogConfig] = deriveReader[PcDogConfig]
    implicit lazy val dogW: ConfigWriter[PcDogConfig] = deriveWriter[PcDogConfig]
    implicit lazy val catR: ConfigReader[PcCatConfig] = deriveReader[PcCatConfig]
    implicit lazy val catW: ConfigWriter[PcCatConfig] = deriveWriter[PcCatConfig]
    implicit lazy val birdR: ConfigReader[PcBirdConfig] = deriveReader[PcBirdConfig]
    implicit lazy val birdW: ConfigWriter[PcBirdConfig] = deriveWriter[PcBirdConfig]
    lazy val animalR: ConfigReader[PcAnimalConfig] = deriveReader[PcAnimalConfig]
    lazy val animalW: ConfigWriter[PcAnimalConfig] = deriveWriter[PcAnimalConfig]
  }

  // --- derivations: user-provided instances of the fields' types -----------------------------------------------------

  private object MissingKeysK {
    implicit val intReader: ConfigReader[Int] = defaultingIntReader
    implicit val intWriter: ConfigWriter[Int] = nonZeroIntWriter
    val twoIntsR: ConfigReader[PcTwoInts] = KindlingsConfigReader.derived[PcTwoInts]
    val twoIntsW: ConfigWriter[PcTwoInts] = KindlingsConfigWriter.derived[PcTwoInts]
  }

  private object MissingKeysU {
    import pureconfig.generic.semiauto.*
    implicit val intReader: ConfigReader[Int] = defaultingIntReader
    implicit val intWriter: ConfigWriter[Int] = nonZeroIntWriter
    lazy val twoIntsR: ConfigReader[PcTwoInts] = deriveReader[PcTwoInts]
    lazy val twoIntsW: ConfigWriter[PcTwoInts] = deriveWriter[PcTwoInts]
  }

  private trait Overrides {
    implicit val readBoolean: ConfigReader[Boolean] = ConfigReader.fromString[Boolean](catchReadError(_ => false))
    implicit val readDouble: ConfigReader[Double] = ConfigReader.fromString[Double](catchReadError(_ => 1d))
    implicit val readFloat: ConfigReader[Float] = ConfigReader.fromString[Float](catchReadError(_ => 2f))
    implicit val readInt: ConfigReader[Int] = ConfigReader.fromString[Int](catchReadError(_ => 3))
    implicit val readLong: ConfigReader[Long] = ConfigReader.fromString[Long](catchReadError(_ => 4L))
    implicit val readString: ConfigReader[String] = ConfigReader.fromString[String](catchReadError(_ => "foobar"))
    implicit val convertOption: ConfigConvert[Option[String]] =
      ConfigConvert.viaString[Option[String]](catchReadError(_ => None), _ => " ")
    implicit val convertInner: ConfigConvert[PcInner] = constantInnerConvert
  }

  private object OverridesK extends Overrides {
    val flatR: ConfigReader[PcFlat] = KindlingsConfigReader.derived[PcFlat]
    val flatW: ConfigWriter[PcFlat] = KindlingsConfigWriter.derived[PcFlat]
    val manyDefaultsR: ConfigReader[PcManyDefaults] = KindlingsConfigReader.derived[PcManyDefaults]
  }

  private object OverridesU extends Overrides {
    import pureconfig.generic.semiauto.*
    lazy val flatR: ConfigReader[PcFlat] = deriveReader[PcFlat]
    lazy val flatW: ConfigWriter[PcFlat] = deriveWriter[PcFlat]
    lazy val manyDefaultsR: ConfigReader[PcManyDefaults] = deriveReader[PcManyDefaults]
  }

  // --- ProductHintSuite ----------------------------------------------------------------------------------------------

  group("upstream ProductHintSuite") {

    test("read / write kebab case config keys from camel case fields by default") {
      compareReads(K.camelR, U.camelR)(
        root(
          "camel-case-int = 1, camel-case-string = bar, camel-case-conf { this-is-an-int = 3, this-is-another-int = 10 }"
        ),
        root("camelCaseInt = 1, camelCaseString = bar, camelCaseConf { thisIsAnInt = 3, thisIsAnotherInt = 10 }")
      )()
      compareWrites(K.camelW, U.camelW)(PcCamelOuter(1, "foobar", PcCamelInner(2, 3)))()
    }

    test("allow customizing the field mapping through a product hint") {
      compareReads(HintsK.twoIntsR, HintsU.twoIntsR)(root("A = 2, B = 3"), root("a = 2, b = 3"))()
      compareReads(K.twoIntsR, U.twoIntsR)(root("A = 2, B = 3"))()
      compareWrites(HintsK.twoIntsW, HintsU.twoIntsW)(PcTwoInts(2, 3))()
    }

    test("read / write pascal case config keys when configured to do so (for every type)") {
      compareReads(PascalK.camelR, PascalU.camelR)(
        root("CamelCaseInt = 1, CamelCaseString = bar, CamelCaseConf { ThisIsAnInt = 3, ThisIsAnotherInt = 10 }"),
        root(
          "camel-case-int = 1, camel-case-string = bar, camel-case-conf { this-is-an-int = 3, this-is-another-int = 10 }"
        )
      )()
      compareWrites(PascalK.camelW, PascalU.camelW)(PcCamelOuter(1, "foobar", PcCamelInner(2, 3)))()
    }

    test("allow customizing the field mapping only for specific types") {
      compareReads(HintsK.camelR, HintsU.camelR)(
        root("camelCaseInt = 1, camelCaseString = bar, camelCaseConf { this-is-an-int = 3, this-is-another-int = 10 }"),
        root("camelCaseInt = 1, camelCaseString = bar, camelCaseConf { thisIsAnInt = 3, thisIsAnotherInt = 10 }")
      )()
      compareWrites(HintsK.camelW, HintsU.camelW)(PcCamelOuter(1, "foobar", PcCamelInner(2, 3)))()
    }

    test("disallow unknown keys even for types with no fields") {
      compareReads(K.emptyR, U.emptyR)(root("a = 1"), root(""), value("hello"), value("[1]"))()
      compareReads(K.objR, U.objR)(root("a = 1"), root(""), value("hello"), value("[1]"))()
      compareReads(HintsK.emptyR, HintsU.emptyR)(root("a = 1"), root("a = 1, b { c = 2 }"), root(""), value("hello"))()
      compareReads(HintsK.objR, HintsU.objR)(root("a = 1"), root("a = 1, b { c = 2 }"), root(""), value("hello"))()
      compareWrites(K.emptyW, U.emptyW)(PcEmptyConf())()
      compareWrites(K.objW, U.objW)(PcObjConf)()
    }

    test("not use default arguments if specified through a product hint") {
      compareReads(HintsK.manyDefaultsR, HintsU.manyDefaultsR)(
        root("a = 2"),
        root("a = 2, b = x, c = 1, d { e = 1, g = 2 }"),
        root("a = 2, b = x, c = 1, d { e = 1, g = 2 }, e = 3")
      )()
    }

    test("include candidate keys in failure reasons in case of a suspected misconfigured ProductHint") {
      compareReads(K.camelR, U.camelR)(
        root(
          "camel-case-int = 1, camel-case-string = bar, camel-case-conf { thisIsAnInt = 3, this_is_another_int = 10 }"
        ),
        root(
          "camel_case_int = 1, camel-case-string = bar, camel-case-conf { this-is-an-int = 3, this-is-another-int = 10 }"
        )
      )()
    }
  }

  group("upstream docs: hints in companion objects") {

    test("a ProductHint in the companion of the case class") {
      compareReads(CompanionK.hintedR, CompanionU.hintedR)(
        root("some_field = 1"),
        root("some_field = 1, other_field = y"),
        root("some-field = 1"),
        root("some_field = 1, extra = 2")
      )()
      compareWrites(CompanionK.hintedW, CompanionU.hintedW)(PcCompanionHinted(1, "y"))()
    }

    test("a CoproductHint in the companion of the sealed trait") {
      compareReads(CompanionK.familyR, CompanionU.familyR)(
        root("kind = pc-companion-leaf, some-value = 1"),
        root("kind = pc-companion-object"),
        root("type = pc-companion-leaf, some-value = 1"),
        root("kind = nope")
      )()
      compareWrites(CompanionK.familyW, CompanionU.familyW)(PcCompanionLeaf(1), PcCompanionObject)()
    }
  }

  // --- ProductConvertersSuite (Scala 2) / ProductConvertDerivationSuite (Scala 3) ------------------------------------

  group("upstream ProductConvertersSuite") {

    test("flat case class of simple values and an Option") {
      compareReads(K.flatR, U.flatR)(
        root("b = true, d = 2.5, f = 4.5, i = 6, l = 8, s = barfoo, o = foobar"),
        root("b = true, d = 2.5, f = 4.5, i = 6, l = 8, s = barfoo"),
        root("b = yes, d = x, f = y, i = 6.5, l = z, s = [1], o { a = 1 }")
      )()
      compareWrites(K.flatW, U.flatW)(
        PcFlat(true, 2.5, 4.5f, 6, 8L, "barfoo", Some("foobar")),
        PcFlat(false, 0d, 0f, 0, 0L, "", None)
      )()
    }

    test("be able to override all of the instances used to parse the product elements") {
      compareReads(OverridesK.flatR, OverridesU.flatR)(
        root("b = true, d = 2.5, f = 4.5, i = 6, l = 8, s = barfoo, o = foobar"),
        root("b = true, d = 2.5, f = 4.5, i = 6, l = 8, s = barfoo")
      )()
      // The user's `ConfigConvert[Option[String]]` is not a `WritesMissingKeys`: an empty `Option` is written with it.
      compareWrites(OverridesK.flatW, OverridesU.flatW)(
        PcFlat(true, 2.5, 4.5f, 6, 8L, "barfoo", Some("foobar")),
        PcFlat(false, 0d, 0f, 0, 0L, "", None)
      )()
    }

    test("return a KeyNotFound when a custom convert is used and when a key is not in the configuration") {
      compareReads(OverridesK.manyDefaultsR, OverridesU.manyDefaultsR)(
        root("a = 1"),
        root("a = 1, d = whatever"),
        root("")
      )()
    }

    test("allow custom ConfigReaders to handle missing keys") {
      compareReads(K.twoIntsR, U.twoIntsR)(root("a = 1"))()
      compareReads(MissingKeysK.twoIntsR, MissingKeysU.twoIntsR)(
        root("a = 1"),
        root("a = 1, b = 2"),
        root("b = x"),
        root("")
      )()
    }

    test("allow custom ConfigWriters to handle missing keys") {
      compareWrites(K.twoIntsW, U.twoIntsW)(PcTwoInts(0, 3))()
      compareWrites(MissingKeysK.twoIntsW, MissingKeysU.twoIntsW)(PcTwoInts(0, 3), PcTwoInts(0, 0), PcTwoInts(1, 2))()
    }

    test("not write empty option fields") {
      compareWrites(K.optIntW, U.optIntW)(PcOptInt(42, Some(1)), PcOptInt(42, None))()
    }

    test("consider default arguments by default") {
      compareReads(K.manyDefaultsR, U.manyDefaultsR)(
        root("a = 2"),
        root("a = 2, c = 50"),
        root("c = 50"),
        root("a = 2, d { e = 5 }"),
        root("a = 2, d { e = 5, g = 6 }"),
        root("a = 2, d = notAnInnerConf"),
        root("a = 2, c = 50, e = 1"),
        root("a = 2, c = 50, e = null"),
        root("a = 2, b = null, c = null, d = null")
      )()
    }

    test("use default arguments for generic case classes when a key is not in the configuration") {
      compareReads(K.genericDefaultsR, U.genericDefaultsR)(root(""), root("i = 2, l = [1, 2]"), root("l = [x]"))()
    }

    test("work properly with recursively defined product types") {
      compareReads(K.recR, U.recR)(
        root("ls = [{ ls = [] }, { ls = [{ ls = [] }] }]"),
        root("ls = [{ ls = [] }, { ls = [{ sl = [] }, 1] }]")
      )()
      compareWrites(K.recW, U.recW)(PcRec(List(PcRec(Nil), PcRec(List(PcRec(Nil))))))()
    }

    test("use existing reader and writer instances when they exist (DerivationModesSuite)") {
      compareReads(K.customR, U.customR)(
        root(
          """custom-object = eaaxacaca, map-custom-object { a = eaaxacaca, b = a }, map-list-custom-object { x = [a, eaaxacaca, cvbc] }"""
        )
      )()
      compareWrites(K.customW, U.customW)(
        PcCustomCaseClass(
          PcCustomObject(453),
          Map("a" -> PcCustomObject(453), "b" -> PcCustomObject(45)),
          Map("x" -> List(PcCustomObject(45), PcCustomObject(453), PcCustomObject(1)))
        )
      )()
    }
  }

  // --- TupleConvertersSuite (Scala 2) / ProductConvertDerivationSuite "read lists as tuples" (Scala 3) ---------------

  group("upstream TupleConvertersSuite") {

    test("tuple fields are read from and written as lists") {
      compareReads(K.tupledR, U.tupledR)(
        root("values = [true, 5]"),
        root("values = [true, value]"),
        root("values = [true]"),
        root("values = [true, 5, value]"),
        root("values = []"),
        root("values = text"),
        root("""values { "0" = true, "1" = 5 }"""),
        root("")
      )()
      compareWrites(K.tupledW, U.tupledW)(PcTupled((true, 5)))()
    }

    test("nested tuples") {
      compareReads(K.tupled3R, U.tupled3R)(
        root("values = [1, [2, three], false]"),
        root("values = [x, [y, three, 4], z]"),
        root("values = [1, 2, false]")
      )()
      compareWrites(K.tupled3W, U.tupled3W)(PcTupled3((1, (2L, "three"), false)))()
    }

    test("tuples derived directly") {
      compareReads(K.pairR, U.pairR)(
        value("[one, 2]"),
        value("[one, two]"),
        value("str"),
        root(""""0" = one, "1" = 2""")
      )()
      compareReads(K.tripleR, U.tripleR)(value("[1, one]"), value("[1, 2, 3]"))()
      compareReads(K.tuple1R, U.tuple1R)(value("[1]"), value("1"), value("[]"))()
      compareWrites(K.pairW, U.pairW)(("one", 2))()
      compareWrites(K.tuple1W, U.tuple1W)(Tuple1(1))()
    }
  }

  group("upstream TupleConvertersSuite: the product representation") {

    // Kindlings reads a value that is not a list but is an object from the tuple's product representation on both
    // Scala versions, as upstream's Scala 2 derivation does. Upstream's Scala 3 derivation reads only lists, so there
    // Kindlings accepts more (documented in feature-parity.md, pureconfig-derivation, "Tuples").
    def onlyListsUpstream(differences: String*): Seq[String] =
      if (PcVersion.upstreamReadsTuplesFromObjects) Nil else differences

    val notAList = "WrongType: Expected type LIST. Found OBJECT instead.)"

    test("tuple fields as objects with _1, _2, ... keys") {
      compareReads(K.tupledR, U.tupledR)(
        root("values { _1 = true, _2 = 5 }"),
        root("values { _1 = true }"),
        root("values { _1 = x, _2 = y }")
      )(
        onlyListsUpstream(
          s"""{"values":{"_1":true,"_2":5}}: kindlings=Right(PcTupled((true,5))) upstream=Left(at 'values' (String: 1): $notAList""",
          s"""{"values":{"_1":true}}: kindlings=Left(at 'values' (String: 1): KeyNotFound: Key not found: '_2'.) upstream=Left(at 'values' (String: 1): $notAList""",
          s"""{"values":{"_1":"x","_2":"y"}}: kindlings=Left(at 'values._1' (String: 1): WrongType: Expected type BOOLEAN. Found STRING instead. | at 'values._2' (String: 1): WrongType: Expected type NUMBER. Found STRING instead.) upstream=Left(at 'values' (String: 1): $notAList"""
        )*
      )
    }

    test("tuples derived directly, as objects") {
      compareReads(K.pairR, U.pairR)(root("_1 = one, _2 = 2"), root("_1 = one, _2 = two"))(
        onlyListsUpstream(
          s"""{"_1":"one","_2":2}: kindlings=Right((one,2)) upstream=Left(at '' (String: 1): $notAList""",
          s"""{"_1":"one","_2":"two"}: kindlings=Left(at '_2' (String: 1): WrongType: Expected type NUMBER. Found STRING instead.) upstream=Left(at '' (String: 1): $notAList"""
        )*
      )
      compareReads(K.tripleR, U.tripleR)(root("_1 = one, _2 = 2"))(
        onlyListsUpstream(
          s"""{"_1":"one","_2":2}: kindlings=Left(at '_1' (String: 1): WrongType: Expected type NUMBER. Found STRING instead. | at '' (String: 1): KeyNotFound: Key not found: '_3'.) upstream=Left(at '' (String: 1): $notAList"""
        )*
      )
    }
  }

  // --- CoproductConvertersSuite / CoproductHintSuite -----------------------------------------------------------------

  group("upstream CoproductConvertersSuite") {

    test("read / write disambiguation information on sealed families by default") {
      compareReads(K.animalR, U.animalR)(
        root("type = pc-dog-config, age = 2"),
        root("can-fly = true"),
        root("can-fly = true, type = pc-car-config"),
        root("type = pc-bird-config, age = 2")
      )()
      compareWrites(K.animalW, U.animalW)(PcDogConfig(2), PcCatConfig(3), PcBirdConfig(true))()
    }

    test("return a proper ConfigReaderFailure when a coproduct config is missing") {
      compareReads(K.cageR, U.cageR)(root(""), root("animal = dog"), root("animal { type = pc-dog-config }"))()
    }
  }

  group("upstream CoproductHintSuite") {

    test("FieldCoproductHint with a custom field and fieldValue") {
      compareReads(HintsK.animalR, HintsU.animalR)(
        root("which-animal = Dog, age = 2"),
        value("Dog"),
        root("which-animal { type = Dog }, age = 2"),
        root("which-animal = unexpected, age = 2"),
        root("type = pc-dog-config, age = 2")
      )()
      compareWrites(HintsK.animalW, HintsU.animalW)(PcDogConfig(2), PcBirdConfig(false))()
    }

    test("the hint field conflicts with a field of an option") {
      compareReads(HintsK.ambiguousR, HintsU.ambiguousR)(root("typ = pc-ambiguous-conf"))()
      compareWrites(HintsK.ambiguousW, HintsU.ambiguousW)(PcAmbiguousConf("ambiguous-conf"))()
    }

    test("FirstSuccessCoproductHint") {
      compareReads(FirstSuccessK.animalR, FirstSuccessU.animalR)(
        root("can-fly = true"),
        root("age = 2"),
        root("age = 2, can-fly = true"),
        root("nothing = 1"),
        value("text")
      )()
      compareWrites(FirstSuccessK.animalW, FirstSuccessU.animalW)(PcDogConfig(2), PcBirdConfig(true))()
    }

    test("errors relevant for coproduct derivation when using the default CoproductHint") {
      compareReads(K.enclosingAR, U.enclosingAR)(
        root("""values { v1 { type = unexpected, a = 2 }, v2 { type = "pc-aa-2", a = val }, v3 { a = 5 } }""")
      )()
    }
  }

  // --- ValueClassSuite -----------------------------------------------------------------------------------------------

  group("upstream ValueClassSuite") {

    test("value classes are read from and written as their value") {
      compareReads(K.intWrapperR, U.intWrapperR)(value("1"), value("x"), root("inner = 1"))()
      compareWrites(K.intWrapperW, U.intWrapperW)(new PcIntWrapper(1))()
    }

    test("a value class of a sealed family, and a case class of it") {
      compareReads(K.shapeWrapperR, U.shapeWrapperR)(
        root("type = pc-circle, radius = 1"),
        root("value { type = pc-circle, radius = 1 }")
      )()
      compareWrites(K.shapeWrapperW, U.shapeWrapperW)(PcShapeWrapper(PcCircle(1)))()
      compareReads(K.doubleWrapperR, U.doubleWrapperR)(
        root("value { type = pc-circle, radius = 1 }"),
        root("type = pc-circle, radius = 1")
      )()
      compareWrites(K.doubleWrapperW, U.doubleWrapperW)(PcShapeDoubleWrapper(PcShapeWrapper(PcCircle(1))))()
    }
  }
}
