package hearth.kindlings.pureconfigderivation

import com.typesafe.config.{ConfigFactory, ConfigRenderOptions, ConfigValue, ConfigValueFactory, ConfigValueType}
import hearth.MacroSuite
import pureconfig.{ConfigConvert, ConfigCursor, ConfigReader, ConfigWriter, ReadsMissingKeys, WritesMissingKeys}
import pureconfig.ConfigConvert.catchReadError
import pureconfig.error.{ConfigReaderFailures, ConvertFailure, KeyNotFound, UnknownKey, WrongSizeList, WrongType}

final case class UsTupled(values: (Boolean, Int))
final case class UsNestedTupled(values: (Int, (Long, String), Boolean))
final case class UsTwoInts(a: Int, b: Int)
final case class UsOptString(a: Int, o: Option[String])
final case class UsIntWithDefault(a: Int, b: Int = 7)
final case class UsEmptyConf()
case object UsObjConf

sealed trait UsStrictFamily
final case class UsStrictLeaf(a: Int) extends UsStrictFamily
case object UsStrictObject extends UsStrictFamily

object UsLazy {
  def boom: String = throw new RuntimeException("Should not be evaluated")
}
final case class UsLazyA(foo: String = UsLazy.boom)
final case class UsLazyB(bar: Option[UsLazyA])
final case class UsLazyC(baz: UsLazyA = UsLazyA(), foo: String = UsLazy.boom)

/** Cases of upstream pureconfig's own suites (`modules/generic/src/test`, `modules/generic-scala3/src/test`) that
  * `PureConfigUpstreamSuitesOracleSpec` (upstream-oracle-tests) compares with upstream's derivation, tested here
  * without it.
  */
final class UpstreamSuitesSpec extends MacroSuite {

  private def cursor(hocon: String): ConfigCursor = ConfigCursor(ConfigFactory.parseString(hocon).root(), Nil)

  private def value(hocon: String): ConfigCursor =
    cursor(s"v = $hocon").asObjectCursor.flatMap(_.atKey("v")).toOption.get

  private def reasons(result: Either[ConfigReaderFailures, Any]): List[(String, Any)] =
    result.left.toOption.get.toList.map { case ConvertFailure(reason, _, path) => (path, reason); case f => ("?", f) }

  private def render(v: ConfigValue): String = v.render(ConfigRenderOptions.concise())

  // upstream: TupleConvertersSuite (Scala 2), ProductConvertDerivationSuite "be able to read lists as tuples" (Scala 3)
  group("tuples") {

    test("are read from lists") {
      val r = KindlingsConfigReader.derived[UsTupled]
      r.from(cursor("values = [true, 5]")) ==> Right(UsTupled((true, 5)))
      // HOCON turns an object with numeric keys into a list
      r.from(cursor("""values { "0" = true, "1" = 5 }""")) ==> Right(UsTupled((true, 5)))
    }

    test("are written as lists") {
      render(KindlingsConfigWriter.derived[UsTupled].to(UsTupled((true, 5)))) ==> """{"values":[true,5]}"""
      render(KindlingsConfigWriter.derived[UsNestedTupled].to(UsNestedTupled((1, (2L, "three"), false)))) ==>
        """{"values":[1,[2,"three"],false]}"""
      render(KindlingsConfigWriter.derived[Tuple1[Int]].to(Tuple1(1))) ==> "[1]"
    }

    test("a list of another size is a WrongSizeList") {
      val r = KindlingsConfigReader.derived[UsTupled]
      reasons(r.from(cursor("values = [true]"))) ==> List("values" -> WrongSizeList(2, 1))
      reasons(r.from(cursor("values = [true, 5, value]"))) ==> List("values" -> WrongSizeList(2, 3))
    }

    test("every failing element is reported, at its index") {
      val r = KindlingsConfigReader.derived[UsNestedTupled]
      reasons(r.from(cursor("values = [x, [y, three, 4], z]"))) ==> List(
        "values.0" -> WrongType(ConfigValueType.STRING, Set(ConfigValueType.NUMBER)),
        "values.1" -> WrongSizeList(2, 3),
        "values.2" -> WrongType(ConfigValueType.STRING, Set(ConfigValueType.BOOLEAN))
      )
    }

    test("a value that is neither a list nor an object is a WrongType expecting a list") {
      val r = KindlingsConfigReader.derived[(String, Int)]
      reasons(r.from(value("str"))) ==> List("v" -> WrongType(ConfigValueType.STRING, Set(ConfigValueType.LIST)))
    }

    test("are also read from their product representation (_1, _2, ...)") {
      val r = KindlingsConfigReader.derived[(String, Int)]
      r.from(cursor("_1 = one, _2 = 2")) ==> Right(("one", 2))
      reasons(r.from(cursor("_1 = one"))) ==> List("" -> KeyNotFound("_2"))
    }

    test("round trip") {
      val r = KindlingsConfigReader.derived[UsNestedTupled]
      val w = KindlingsConfigWriter.derived[UsNestedTupled]
      val v = UsNestedTupled((1, (2L, "three"), false))
      r.from(ConfigCursor(w.to(v), Nil)) ==> Right(v)
    }
  }

  // upstream: ProductConvertersSuite / ProductConvertDerivationSuite
  group("readers and writers of missing keys") {

    test("without custom instances a missing key is a KeyNotFound and every key is written") {
      reasons(KindlingsConfigReader.derived[UsTwoInts].from(cursor("a = 1"))) ==> List("" -> KeyNotFound("b"))
      render(KindlingsConfigWriter.derived[UsTwoInts].to(UsTwoInts(0, 3))) ==> """{"a":0,"b":3}"""
    }

    test("a custom ReadsMissingKeys reader reads a missing key") {
      implicit val defaulting: ConfigReader[Int] = new ConfigReader[Int] with ReadsMissingKeys {
        def from(cur: ConfigCursor): ConfigReader.Result[Int] =
          cur.asConfigValue.fold(_ => Right(42), v => cur.scopeFailure(catchReadError(_.toInt).apply(render(v))))
      }
      KindlingsConfigReader.derived[UsTwoInts].from(cursor("a = 1")) ==> Right(UsTwoInts(1, 42))
    }

    test("a custom ReadsMissingKeys reader reads the missing key of a field whose default is not used") {
      implicit val hint: KindlingsProductHint[UsIntWithDefault] =
        KindlingsProductHint[UsIntWithDefault](useDefaults = false)
      implicit val defaulting: ConfigReader[Int] = new ConfigReader[Int] with ReadsMissingKeys {
        def from(cur: ConfigCursor): ConfigReader.Result[Int] =
          cur.asConfigValue.fold(_ => Right(42), v => cur.scopeFailure(catchReadError(_.toInt).apply(render(v))))
      }
      KindlingsConfigReader.derived[UsIntWithDefault].from(cursor("a = 1")) ==> Right(UsIntWithDefault(1, 42))
    }

    test("a custom WritesMissingKeys writer decides whether a key is written") {
      implicit val nonZero: ConfigWriter[Int] = new ConfigWriter[Int] with WritesMissingKeys[Int] {
        def to(v: Int): ConfigValue = ConfigValueFactory.fromAnyRef(v)
        def toOpt(a: Int): Option[ConfigValue] = if (a == 0) None else Some(to(a))
      }
      render(KindlingsConfigWriter.derived[UsTwoInts].to(UsTwoInts(0, 3))) ==> """{"b":3}"""
    }

    test("a custom instance of an Option type that does not handle missing keys is used as is") {
      implicit val convert: ConfigConvert[Option[String]] =
        ConfigConvert.viaString[Option[String]](catchReadError(_ => None), _ => " ")
      // reading: the key is required (upstream: KeyNotFound at the object)
      reasons(KindlingsConfigReader.derived[UsOptString].from(cursor("a = 1"))) ==> List("" -> KeyNotFound("o"))
      KindlingsConfigReader.derived[UsOptString].from(cursor("a = 1, o = x")) ==> Right(UsOptString(1, None))
      // writing: an empty Option is written with the user's writer, not omitted
      render(KindlingsConfigWriter.derived[UsOptString].to(UsOptString(1, None))) ==> """{"a":1,"o":" "}"""
    }

    test("an empty Option field is omitted and a missing one is None by default") {
      render(KindlingsConfigWriter.derived[UsOptString].to(UsOptString(1, None))) ==> """{"a":1}"""
      KindlingsConfigReader.derived[UsOptString].from(cursor("a = 1")) ==> Right(UsOptString(1, None))
      KindlingsConfigReader.derived[UsLazyB].from(cursor("")) ==> Right(UsLazyB(None))
    }
  }

  // upstream: ProductHintSuite "disallow unknown keys even for types with no fields"
  group("types with no fields") {

    test("are read only from objects") {
      KindlingsConfigReader.derived[UsEmptyConf].from(cursor("a = 1")) ==> Right(UsEmptyConf())
      KindlingsConfigReader.derived[UsObjConf.type].from(cursor("a = 1")) ==> Right(UsObjConf)
      reasons(KindlingsConfigReader.derived[UsEmptyConf].from(value("hello"))) ==>
        List("v" -> WrongType(ConfigValueType.STRING, Set(ConfigValueType.OBJECT)))
      reasons(KindlingsConfigReader.derived[UsObjConf.type].from(value("hello"))) ==>
        List("v" -> WrongType(ConfigValueType.STRING, Set(ConfigValueType.OBJECT)))
    }

    test("disallow unknown keys with a strict hint") {
      implicit val emptyHint: KindlingsProductHint[UsEmptyConf] =
        KindlingsProductHint[UsEmptyConf](allowUnknownKeys = false)
      implicit val objHint: KindlingsProductHint[UsObjConf.type] =
        KindlingsProductHint[UsObjConf.type](allowUnknownKeys = false)
      reasons(KindlingsConfigReader.derived[UsEmptyConf].from(cursor("a = 1"))) ==> List("a" -> UnknownKey("a"))
      reasons(KindlingsConfigReader.derived[UsObjConf.type].from(cursor("a = 1"))) ==> List("a" -> UnknownKey("a"))
      KindlingsConfigReader.derived[UsEmptyConf].from(cursor("")) ==> Right(UsEmptyConf())
      KindlingsConfigReader.derived[UsObjConf.type].from(cursor("")) ==> Right(UsObjConf)
    }

    test("a case object of a sealed family disallows unknown keys in strict mode (the discriminator is not one)") {
      implicit val config: PureConfig = PureConfig.default.withStrictDecoding
      val r = KindlingsConfigReader.derived[UsStrictFamily]
      r.from(cursor("type = us-strict-object")) ==> Right(UsStrictObject)
      reasons(r.from(cursor("type = us-strict-object, extra = 1"))) ==> List("extra" -> UnknownKey("extra"))
      reasons(r.from(cursor("type = us-strict-leaf, a = 1, extra = 1"))) ==> List("extra" -> UnknownKey("extra"))
    }
  }

  // upstream: ProductConvertDerivationSuite "evaluate defaults lazily"
  group("defaults") {

    test("are evaluated only when the key is missing") {
      KindlingsConfigReader.derived[UsLazyA].from(cursor("foo = bar")) ==> Right(UsLazyA("bar"))
      KindlingsConfigReader.derived[UsLazyC].from(cursor("baz { foo = bar }, foo = bar")) ==>
        Right(UsLazyC(UsLazyA("bar"), "bar"))
    }
  }
}
