package hearth.kindlings.sconfigderivation

import hearth.MacroSuite
import hearth.kindlings.sconfigderivation.ConfigDecodingError.{KeyNotFound, UnknownKey, WrongSizeList, WrongType}
import org.ekrich.config.{ConfigFactory, ConfigObject, ConfigRenderOptions, ConfigValue, ConfigValueFactory}

final case class UsTupled(values: (Boolean, Int))
final case class UsNestedTupled(values: (Int, (Long, String), Boolean))
final case class UsTwoInts(a: Int, b: Int)
final case class UsIntWithDefault(a: Int, b: Int = 7)
final case class UsOptString(a: Int, o: Option[String])
final case class UsOptInner(o: Option[UsTwoInts])
final case class UsEmptyConf()
case object UsObjConf

sealed trait UsStrictFamily
final case class UsStrictLeaf(a: Int) extends UsStrictFamily
case object UsStrictObject extends UsStrictFamily

object UsLazy {
  def boom: String = throw new RuntimeException("Should not be evaluated")
}
final case class UsLazyA(foo: String = UsLazy.boom)
final case class UsLazyC(baz: UsLazyA = UsLazyA(), foo: String = UsLazy.boom)

/** The cases of upstream PureConfig's own derivation suites that pureconfig-derivation's `UpstreamSuitesSpec` asserts
  * (and `PureConfigUpstreamSuitesOracleSpec` compares with upstream), with the same outcomes for sconfig: tuples as
  * lists, readers / writers of missing keys, types without fields, lazily evaluated defaults.
  *
  * Paths are relative to the value the reader is called with (see `UpstreamSemanticsSpec`).
  */
final class UpstreamSuitesSpec extends MacroSuite {

  private def cursor(hocon: String): ConfigValue = ConfigFactory.parseString(hocon).root

  private def value(hocon: String): ConfigValue = cursor(s"v = $hocon").asInstanceOf[ConfigObject].get("v")

  private def failures(result: Either[ConfigDecodingError, Any]): List[ConfigDecodingError] =
    ConfigDecodingError.flatten(result.left.toOption.getOrElse(fail(s"expected a failure, got $result")))

  private def render(v: ConfigValue): String = v.render(ConfigRenderOptions.concise)

  // upstream: TupleConvertersSuite (Scala 2), ProductConvertDerivationSuite "be able to read lists as tuples" (Scala 3)
  group("tuples") {

    test("are read from lists") {
      ConfigReader.derived[UsTupled].from(cursor("values = [true, 5]")) ==> Right(UsTupled((true, 5)))
    }

    test("are written as lists") {
      render(ConfigWriter.derived[UsTupled].to(UsTupled((true, 5)))) ==> """{"values":[true,5]}"""
      render(ConfigWriter.derived[UsNestedTupled].to(UsNestedTupled((1, (2L, "three"), false)))) ==>
        """{"values":[1,[2,"three"],false]}"""
      render(ConfigWriter.derived[Tuple1[Int]].to(Tuple1(1))) ==> "[1]"
    }

    test("a list of another size is a WrongSizeList") {
      val r = ConfigReader.derived[UsTupled]
      failures(r.from(cursor("values = [true]"))) ==> List(WrongSizeList(List("values"), 2, 1))
      failures(r.from(cursor("values = [true, 5, value]"))) ==> List(WrongSizeList(List("values"), 2, 3))
    }

    test("every failing element is reported, at its index") {
      val r = ConfigReader.derived[UsNestedTupled]
      failures(r.from(cursor("values = [x, [y, three, 4], z]"))) ==> List(
        WrongType(List("values", "0"), "NUMBER", "STRING"),
        WrongSizeList(List("values", "1"), 2, 3),
        WrongType(List("values", "2"), "BOOLEAN", "STRING")
      )
    }

    test("a value that is neither a list nor an object is a WrongType expecting a list") {
      failures(ConfigReader.derived[(String, Int)].from(value("str"))) ==> List(WrongType(Nil, "LIST", "STRING"))
    }

    test("are also read from their product representation (_1, _2, ...)") {
      val r = ConfigReader.derived[(String, Int)]
      r.from(cursor("_1 = one, _2 = 2")) ==> Right(("one", 2))
      failures(r.from(cursor("_1 = one"))) ==> List(KeyNotFound(Nil, "_2"))
    }

    test("round trip") {
      val v = UsNestedTupled((1, (2L, "three"), false))
      ConfigReader.derived[UsNestedTupled].from(ConfigWriter.derived[UsNestedTupled].to(v)) ==> Right(v)
    }
  }

  // upstream: ProductConvertersSuite / ProductConvertDerivationSuite
  group("readers and writers of missing keys") {

    test("without custom instances a missing key is a KeyNotFound and every key is written") {
      failures(ConfigReader.derived[UsTwoInts].from(cursor("a = 1"))) ==> List(KeyNotFound(Nil, "b"))
      render(ConfigWriter.derived[UsTwoInts].to(UsTwoInts(0, 3))) ==> """{"a":0,"b":3}"""
    }

    test("a custom ReadsMissingKeys reader reads the missing key of a field whose default is not used") {
      implicit val hint: ProductHint[UsIntWithDefault] = ProductHint[UsIntWithDefault](useDefaults = false)
      implicit val defaulting: ConfigReader[Int] = new ConfigReader[Int] with ReadsMissingKeys {
        def from(value: ConfigValue): Either[ConfigDecodingError, Int] =
          if (value == null) Right(42) else ConfigReader.intReader.from(value)
      }
      ConfigReader.derived[UsIntWithDefault].from(cursor("a = 1")) ==> Right(UsIntWithDefault(1, 42))
    }

    test("a custom WritesMissingKeys writer decides whether a key is written") {
      implicit val nonZero: ConfigWriter[Int] = new ConfigWriter[Int] with WritesMissingKeys[Int] {
        def to(v: Int): ConfigValue = ConfigValueFactory.fromAnyRef(java.lang.Integer.valueOf(v))
        def toOpt(a: Int): Option[ConfigValue] = if (a == 0) None else Some(to(a))
      }
      render(ConfigWriter.derived[UsTwoInts].to(UsTwoInts(0, 3))) ==> """{"b":3}"""
    }

    test("a custom instance of an Option type that does not handle missing keys is used as is") {
      implicit val reader: ConfigReader[Option[String]] = ConfigReader.stringReader.map(_ => None)
      implicit val writer: ConfigWriter[Option[String]] = ConfigWriter.stringWriter.contramap(_ => " ")
      // reading: the key is required (upstream: KeyNotFound at the object)
      failures(ConfigReader.derived[UsOptString].from(cursor("a = 1"))) ==> List(KeyNotFound(Nil, "o"))
      ConfigReader.derived[UsOptString].from(cursor("a = 1, o = x")) ==> Right(UsOptString(1, None))
      // writing: an empty Option is written with the user's writer, not omitted
      render(ConfigWriter.derived[UsOptString].to(UsOptString(1, None))) ==> """{"a":1,"o":" "}"""
    }

    test("an empty Option field is omitted and a missing one is None by default") {
      render(ConfigWriter.derived[UsOptString].to(UsOptString(1, None))) ==> """{"a":1}"""
      render(ConfigWriter.derived[UsOptString].to(UsOptString(1, Some("x")))) ==> """{"a":1,"o":"x"}"""
      ConfigReader.derived[UsOptString].from(cursor("a = 1")) ==> Right(UsOptString(1, None))
      // an Option of a type whose instances are derived here
      ConfigReader.derived[UsOptInner].from(cursor("")) ==> Right(UsOptInner(None))
      render(ConfigWriter.derived[UsOptInner].to(UsOptInner(None))) ==> "{}"
      render(ConfigWriter.derived[UsOptInner].to(UsOptInner(Some(UsTwoInts(1, 2))))) ==> """{"o":{"a":1,"b":2}}"""
    }
  }

  // upstream: ProductHintSuite "disallow unknown keys even for types with no fields"
  group("types with no fields") {

    test("are read only from objects") {
      ConfigReader.derived[UsEmptyConf].from(cursor("a = 1")) ==> Right(UsEmptyConf())
      ConfigReader.derived[UsObjConf.type].from(cursor("a = 1")) ==> Right(UsObjConf)
      failures(ConfigReader.derived[UsEmptyConf].from(value("hello"))) ==> List(WrongType(Nil, "OBJECT", "STRING"))
      failures(ConfigReader.derived[UsObjConf.type].from(value("hello"))) ==> List(WrongType(Nil, "OBJECT", "STRING"))
    }

    test("disallow unknown keys with a strict hint") {
      implicit val emptyHint: ProductHint[UsEmptyConf] = ProductHint[UsEmptyConf](allowUnknownKeys = false)
      implicit val objHint: ProductHint[UsObjConf.type] = ProductHint[UsObjConf.type](allowUnknownKeys = false)
      failures(ConfigReader.derived[UsEmptyConf].from(cursor("a = 1"))) ==> List(UnknownKey(List("a"), "a"))
      failures(ConfigReader.derived[UsObjConf.type].from(cursor("a = 1"))) ==> List(UnknownKey(List("a"), "a"))
      ConfigReader.derived[UsEmptyConf].from(cursor("")) ==> Right(UsEmptyConf())
      ConfigReader.derived[UsObjConf.type].from(cursor("")) ==> Right(UsObjConf)
    }

    test("a case object of a sealed family disallows unknown keys in strict mode (the discriminator is not one)") {
      implicit val config: SConfig = SConfig.default.withStrictDecoding
      val r = ConfigReader.derived[UsStrictFamily]
      r.from(cursor("type = us-strict-object")) ==> Right(UsStrictObject)
      failures(r.from(cursor("type = us-strict-object, extra = 1"))) ==> List(UnknownKey(List("extra"), "extra"))
      failures(r.from(cursor("type = us-strict-leaf, a = 1, extra = 1"))) ==> List(UnknownKey(List("extra"), "extra"))
    }
  }

  // upstream: ProductConvertDerivationSuite "evaluate defaults lazily"
  group("defaults") {

    test("are evaluated only when the key is missing") {
      ConfigReader.derived[UsLazyA].from(cursor("foo = bar")) ==> Right(UsLazyA("bar"))
      ConfigReader.derived[UsLazyC].from(cursor("baz { foo = bar }, foo = bar")) ==>
        Right(UsLazyC(UsLazyA("bar"), "bar"))
    }
  }
}
