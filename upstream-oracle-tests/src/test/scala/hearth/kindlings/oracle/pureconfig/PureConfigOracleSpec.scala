package hearth.kindlings.oracle.pureconfig

import com.typesafe.config.{ConfigFactory, ConfigList, ConfigObject, ConfigRenderOptions, ConfigValue}
import hearth.MacroSuite
import hearth.kindlings.oracle.Oracle
import hearth.kindlings.pureconfigderivation.{
  KindlingsConfigReader,
  KindlingsConfigWriter,
  KindlingsCoproductHint,
  KindlingsProductHint,
  PureConfig
}
import pureconfig.{CamelCase, ConfigCursor, ConfigFieldMapping, ConfigReader, ConfigWriter, PascalCase, SnakeCase}
import pureconfig.error.{ConfigReaderFailures, ConvertFailure}
import pureconfig.generic.{FieldCoproductHint, FirstSuccessCoproductHint, ProductHint}

import scala.jdk.CollectionConverters.*

/** Kindlings' derived `ConfigReader`s / `ConfigWriter`s vs pureconfig's own derivation (`pureconfig.generic.semiauto`
  * with the equivalent `ProductHint` / `CoproductHint`), compared on the values read from HOCON (including every
  * failure: reason and path) and on the config values written.
  *
  * Each case lists the *exact* differences it expects; each one must be a documented, intentional difference (see
  * `docs/user-guide/feature-parity.md`). A difference that appears or disappears fails the test.
  */
final class PureConfigOracleSpec extends MacroSuite {

  // --- helpers -------------------------------------------------------------------------------------------------------

  /** A cursor at the root of a HOCON document (an object). */
  private def root(hocon: String): ConfigCursor = ConfigCursor(ConfigFactory.parseString(hocon).root(), Nil)

  /** A cursor at `v` of `v = <hocon>`, to read scalars and lists. */
  private def value(hocon: String): ConfigCursor =
    root(s"v = $hocon").asObjectCursor.flatMap(_.atKey("v")).fold(f => sys.error(f.prettyPrint()), identity)

  private def failures(f: ConfigReaderFailures): List[String] = f.toList.map {
    case ConvertFailure(reason, _, path) => s"at '$path': ${reason.getClass.getSimpleName}: ${reason.description}"
    case other                           => other.description
  }

  private def show(result: Either[ConfigReaderFailures, Any]): String =
    result.fold(f => failures(f).mkString("Left(", " | ", ")"), v => s"Right($v)")

  /** Reads every input with both readers; returns `input: kindlings=... upstream=...` for each one they disagree on. */
  private def compareReads[A](kindlings: ConfigReader[A], upstream: ConfigReader[A])(
      inputs: ConfigCursor*
  )(expectedDifferences: String*): Unit = {
    val differences = inputs.toList.flatMap { cur =>
      val k = show(kindlings.from(cur))
      val u = show(upstream.from(cur))
      if (k == u) Nil
      else List(s"${cur.valueOpt.fold("<undefined>")(_.render(ConfigRenderOptions.concise()))}: kindlings=$k upstream=$u")
    }
    assertEquals(differences, expectedDifferences.toList, s"Kindlings vs pureconfig reads:\n${differences.mkString("\n")}")
  }

  /** Every input with every result each library produced, for assertions on the results themselves. */
  private def read[A](reader: ConfigReader[A], cur: ConfigCursor): String = show(reader.from(cur))

  private def toScala(v: ConfigValue): Any = v match {
    case o: ConfigObject => scala.collection.immutable.TreeMap(o.asScala.toSeq.map { case (k, x) => k -> toScala(x) }*)
    case l: ConfigList   => l.asScala.toList.map(toScala)
    case other           => other.unwrapped()
  }

  private def write[A](writer: ConfigWriter[A], a: A): Either[String, Any] =
    try Right(toScala(writer.to(a)))
    catch { case e: Exception => Left(s"${e.getClass.getSimpleName}: ${e.getMessage}") }

  /** Writes every value with both writers; returns the paths at which the written config values differ. */
  private def compareWrites[A](kindlings: ConfigWriter[A], upstream: ConfigWriter[A])(values: A*)(
      expectedDifferences: String*
  ): Unit = {
    val differences = values.toList.flatMap { a =>
      Oracle.diff(write(kindlings, a), write(upstream, a)).map(d => s"$a: $d")
    }
    assertEquals(differences, expectedDifferences.toList, s"Kindlings vs pureconfig writes:\n${differences.mkString("\n")}")
  }

  // --- derivations ---------------------------------------------------------------------------------------------------

  private object DefaultKindlings {
    implicit val config: PureConfig = PureConfig.default
    val personR: ConfigReader[PcPerson] = KindlingsConfigReader.derived[PcPerson]
    val personW: ConfigWriter[PcPerson] = KindlingsConfigWriter.derived[PcPerson]
    val camelR: ConfigReader[PcCamel] = KindlingsConfigReader.derived[PcCamel]
    val camelW: ConfigWriter[PcCamel] = KindlingsConfigWriter.derived[PcCamel]
    val defaultsR: ConfigReader[PcDefaults] = KindlingsConfigReader.derived[PcDefaults]
    val defaultsW: ConfigWriter[PcDefaults] = KindlingsConfigWriter.derived[PcDefaults]
    val collectionsR: ConfigReader[PcCollections] = KindlingsConfigReader.derived[PcCollections]
    val collectionsW: ConfigWriter[PcCollections] = KindlingsConfigWriter.derived[PcCollections]
    val nestedR: ConfigReader[PcNested] = KindlingsConfigReader.derived[PcNested]
    val nestedW: ConfigWriter[PcNested] = KindlingsConfigWriter.derived[PcNested]
    val usesWrapperR: ConfigReader[PcUsesWrapper] = KindlingsConfigReader.derived[PcUsesWrapper]
    val usesWrapperW: ConfigWriter[PcUsesWrapper] = KindlingsConfigWriter.derived[PcUsesWrapper]
    val strictR: ConfigReader[PcStrict] = KindlingsConfigReader.derived[PcStrict]
    val shapeR: ConfigReader[PcShape] = KindlingsConfigReader.derived[PcShape]
    val shapeW: ConfigWriter[PcShape] = KindlingsConfigWriter.derived[PcShape]
    val drawingR: ConfigReader[PcDrawing] = KindlingsConfigReader.derived[PcDrawing]
    val drawingW: ConfigWriter[PcDrawing] = KindlingsConfigWriter.derived[PcDrawing]
    val colorR: ConfigReader[PcColor] = KindlingsConfigReader.derived[PcColor]
    val colorW: ConfigWriter[PcColor] = KindlingsConfigWriter.derived[PcColor]
    val taggedR: ConfigReader[PcTagged] = KindlingsConfigReader.derived[PcTagged]
    val taggedW: ConfigWriter[PcTagged] = KindlingsConfigWriter.derived[PcTagged]
  }

  private object DefaultUpstream {
    import pureconfig.generic.semiauto.*
    // `semiauto` derives one type at a time: the instances of nested types are declared explicitly, as pureconfig's
    // docs say for semi-automatic derivation.
    implicit lazy val personR: ConfigReader[PcPerson] = deriveReader[PcPerson]
    implicit lazy val personW: ConfigWriter[PcPerson] = deriveWriter[PcPerson]
    lazy val camelR: ConfigReader[PcCamel] = deriveReader[PcCamel]
    lazy val camelW: ConfigWriter[PcCamel] = deriveWriter[PcCamel]
    lazy val defaultsR: ConfigReader[PcDefaults] = deriveReader[PcDefaults]
    lazy val defaultsW: ConfigWriter[PcDefaults] = deriveWriter[PcDefaults]
    lazy val collectionsR: ConfigReader[PcCollections] = deriveReader[PcCollections]
    lazy val collectionsW: ConfigWriter[PcCollections] = deriveWriter[PcCollections]
    lazy val nestedR: ConfigReader[PcNested] = deriveReader[PcNested]
    lazy val nestedW: ConfigWriter[PcNested] = deriveWriter[PcNested]
    implicit lazy val wrapperR: ConfigReader[PcWrapper] = deriveReader[PcWrapper]
    implicit lazy val wrapperW: ConfigWriter[PcWrapper] = deriveWriter[PcWrapper]
    lazy val usesWrapperR: ConfigReader[PcUsesWrapper] = deriveReader[PcUsesWrapper]
    lazy val usesWrapperW: ConfigWriter[PcUsesWrapper] = deriveWriter[PcUsesWrapper]
    lazy val strictR: ConfigReader[PcStrict] = deriveReader[PcStrict]
    implicit lazy val circleR: ConfigReader[PcCircle] = deriveReader[PcCircle]
    implicit lazy val circleW: ConfigWriter[PcCircle] = deriveWriter[PcCircle]
    implicit lazy val rectangleR: ConfigReader[PcRectangle] = deriveReader[PcRectangle]
    implicit lazy val rectangleW: ConfigWriter[PcRectangle] = deriveWriter[PcRectangle]
    implicit lazy val emptyR: ConfigReader[PcEmpty.type] = deriveReader[PcEmpty.type]
    implicit lazy val emptyW: ConfigWriter[PcEmpty.type] = deriveWriter[PcEmpty.type]
    implicit lazy val shapeR: ConfigReader[PcShape] = deriveReader[PcShape]
    implicit lazy val shapeW: ConfigWriter[PcShape] = deriveWriter[PcShape]
    lazy val drawingR: ConfigReader[PcDrawing] = deriveReader[PcDrawing]
    lazy val drawingW: ConfigWriter[PcDrawing] = deriveWriter[PcDrawing]
    implicit lazy val redR: ConfigReader[PcRed.type] = deriveReader[PcRed.type]
    implicit lazy val redW: ConfigWriter[PcRed.type] = deriveWriter[PcRed.type]
    implicit lazy val greenR: ConfigReader[PcGreen.type] = deriveReader[PcGreen.type]
    implicit lazy val greenW: ConfigWriter[PcGreen.type] = deriveWriter[PcGreen.type]
    implicit lazy val darkBlueR: ConfigReader[PcDarkBlue.type] = deriveReader[PcDarkBlue.type]
    implicit lazy val darkBlueW: ConfigWriter[PcDarkBlue.type] = deriveWriter[PcDarkBlue.type]
    lazy val colorR: ConfigReader[PcColor] = deriveReader[PcColor]
    lazy val colorW: ConfigWriter[PcColor] = deriveWriter[PcColor]
    implicit lazy val withTypeR: ConfigReader[PcWithType] = deriveReader[PcWithType]
    implicit lazy val withTypeW: ConfigWriter[PcWithType] = deriveWriter[PcWithType]
    implicit lazy val plainR: ConfigReader[PcPlain] = deriveReader[PcPlain]
    implicit lazy val plainW: ConfigWriter[PcPlain] = deriveWriter[PcPlain]
    lazy val taggedR: ConfigReader[PcTagged] = deriveReader[PcTagged]
    lazy val taggedW: ConfigWriter[PcTagged] = deriveWriter[PcTagged]
  }

  private object HintsKindlings {
    implicit val strictHint: KindlingsProductHint[PcStrict] = KindlingsProductHint[PcStrict](allowUnknownKeys = false)
    implicit val noDefaultsHint: KindlingsProductHint[PcNoDefaults] =
      KindlingsProductHint[PcNoDefaults](useDefaults = false)
    implicit val camelHint: KindlingsProductHint[PcCamel] =
      KindlingsProductHint[PcCamel](transformMemberNames = ConfigFieldMapping(CamelCase, SnakeCase))
    implicit val eventHint: KindlingsCoproductHint.Field[PcEvent] =
      KindlingsCoproductHint.Field[PcEvent](
        fieldName = "kind",
        transformConstructorNames = ConfigFieldMapping(PascalCase, SnakeCase)
      )
    implicit val sourceHint: KindlingsCoproductHint.FirstSuccess[PcSource] = KindlingsCoproductHint.FirstSuccess()
    val strictR: ConfigReader[PcStrict] = KindlingsConfigReader.derived[PcStrict]
    val noDefaultsR: ConfigReader[PcNoDefaults] = KindlingsConfigReader.derived[PcNoDefaults]
    val camelR: ConfigReader[PcCamel] = KindlingsConfigReader.derived[PcCamel]
    val camelW: ConfigWriter[PcCamel] = KindlingsConfigWriter.derived[PcCamel]
    val eventR: ConfigReader[PcEvent] = KindlingsConfigReader.derived[PcEvent]
    val eventW: ConfigWriter[PcEvent] = KindlingsConfigWriter.derived[PcEvent]
    val sourceR: ConfigReader[PcSource] = KindlingsConfigReader.derived[PcSource]
    val sourceW: ConfigWriter[PcSource] = KindlingsConfigWriter.derived[PcSource]
  }

  private object HintsUpstream {
    import pureconfig.generic.semiauto.*
    implicit val strictHint: ProductHint[PcStrict] = ProductHint[PcStrict](allowUnknownKeys = false)
    implicit val noDefaultsHint: ProductHint[PcNoDefaults] = ProductHint[PcNoDefaults](useDefaultArgs = false)
    implicit val camelHint: ProductHint[PcCamel] = ProductHint[PcCamel](ConfigFieldMapping(CamelCase, SnakeCase))
    implicit val eventHint: FieldCoproductHint[PcEvent] = new FieldCoproductHint[PcEvent]("kind") {
      override def fieldValue(name: String): String = ConfigFieldMapping(PascalCase, SnakeCase)(name)
    }
    implicit val sourceHint: FirstSuccessCoproductHint[PcSource] = new FirstSuccessCoproductHint[PcSource]
    lazy val strictR: ConfigReader[PcStrict] = deriveReader[PcStrict]
    lazy val noDefaultsR: ConfigReader[PcNoDefaults] = deriveReader[PcNoDefaults]
    lazy val camelR: ConfigReader[PcCamel] = deriveReader[PcCamel]
    lazy val camelW: ConfigWriter[PcCamel] = deriveWriter[PcCamel]
    implicit lazy val clickedR: ConfigReader[PcClicked] = deriveReader[PcClicked]
    implicit lazy val clickedW: ConfigWriter[PcClicked] = deriveWriter[PcClicked]
    implicit lazy val typedR: ConfigReader[PcTyped] = deriveReader[PcTyped]
    implicit lazy val typedW: ConfigWriter[PcTyped] = deriveWriter[PcTyped]
    lazy val eventR: ConfigReader[PcEvent] = deriveReader[PcEvent]
    lazy val eventW: ConfigWriter[PcEvent] = deriveWriter[PcEvent]
    implicit lazy val urlR: ConfigReader[PcUrlSource] = deriveReader[PcUrlSource]
    implicit lazy val urlW: ConfigWriter[PcUrlSource] = deriveWriter[PcUrlSource]
    implicit lazy val fileR: ConfigReader[PcFileSource] = deriveReader[PcFileSource]
    implicit lazy val fileW: ConfigWriter[PcFileSource] = deriveWriter[PcFileSource]
    implicit lazy val anyR: ConfigReader[PcAnySource] = deriveReader[PcAnySource]
    implicit lazy val anyW: ConfigWriter[PcAnySource] = deriveWriter[PcAnySource]
    implicit lazy val inlineR: ConfigReader[PcInlineSource] = deriveReader[PcInlineSource]
    implicit lazy val inlineW: ConfigWriter[PcInlineSource] = deriveWriter[PcInlineSource]
    lazy val sourceR: ConfigReader[PcSource] = deriveReader[PcSource]
    lazy val sourceW: ConfigWriter[PcSource] = deriveWriter[PcSource]
  }

  /** pureconfig's `deriveEnumerationReader`/`Writer` (a sealed family of case objects as strings) corresponds to
    * Kindlings' derivation without a discriminator.
    */
  private object EnumKindlings {
    implicit val config: PureConfig = PureConfig.default.withWrappedSubtypes
    val colorR: ConfigReader[PcColor] = KindlingsConfigReader.derived[PcColor]
    val colorW: ConfigWriter[PcColor] = KindlingsConfigWriter.derived[PcColor]
    val paletteR: ConfigReader[PcPalette] = KindlingsConfigReader.derived[PcPalette]
    val paletteW: ConfigWriter[PcPalette] = KindlingsConfigWriter.derived[PcPalette]
  }

  private object EnumUpstream {
    import pureconfig.generic.semiauto.*
    implicit lazy val colorR: ConfigReader[PcColor] = deriveEnumerationReader[PcColor]
    implicit lazy val colorW: ConfigWriter[PcColor] = deriveEnumerationWriter[PcColor]
    lazy val paletteR: ConfigReader[PcPalette] = deriveReader[PcPalette]
    lazy val paletteW: ConfigWriter[PcPalette] = deriveWriter[PcPalette]
  }

  private val K = DefaultKindlings
  private val U = DefaultUpstream

  // --- products ------------------------------------------------------------------------------------------------------

  group("pureconfig oracle: products") {

    test("case class with an Option field") {
      compareReads(K.personR, U.personR)(
        root("name = Alice, age = 30, nickname = Al"),
        root("name = Alice, age = 30"),
        root("name = Alice, age = 30, nickname = null"),
        root("name = Alice, age = 30, unknown = 1"),
        root("name = Alice"),
        root("nmae = Alice, age = 30"),
        root("age = thirty"),
        root("name = Alice, age = 30, nickname { a = 1 }"),
        root("name = [1], age = 30.5"),
        value("[1, 2]"),
        value("hello")
      )()
      compareWrites(K.personW, U.personW)(PcPerson("Alice", 30, Some("Al")), PcPerson("Bob", 1, None))()
    }

    test("default field name mapping (camelCase -> kebab-case)") {
      compareReads(K.camelR, U.camelR)(
        root("first-name = a, max-retry-count = 1, http-url-value = u, a-1-b-2 = 2"),
        root("first-name = a, max-retry-count = 1, http-url-value = u, a1b2 = 2"),
        root("firstName = a, maxRetryCount = 1, httpURLValue = u, a1b2 = 2")
      )()
      compareWrites(K.camelW, U.camelW)(PcCamel("a", 1, "u", 2))()
    }

    test("default values") {
      compareReads(K.defaultsR, U.defaultsR)(
        root(""),
        root("host = example.com"),
        root("port = 1, tags = [a, b]"),
        root("port = null"),
        root("port = nope")
      )()
      compareWrites(K.defaultsW, U.defaultsW)(PcDefaults(), PcDefaults("h", 1, List("a")))()
    }

    test("collections and maps") {
      compareReads(K.collectionsR, U.collectionsR)(
        root("list = [1, 2], set = [a, b, a], vector = [1.5], map { a = 1, b = 2 }"),
        root("list = [1, 2], set = [a], vector = [], map {}, maybe-list = [3]"),
        root("list = [1, x], set = [], vector = [], map {}"),
        root("list = 1, set = [], vector = [], map { a = x }"),
        root("""list { "0" = 1, "1" = 2 }, set = [], vector = [], map {}"""),
        root("set = [], vector = [], map {}")
      )()
      compareWrites(K.collectionsW, U.collectionsW)(
        PcCollections(List(1, 2), Set("a"), Vector(1.5), Map("a" -> 1), Some(List(3))),
        PcCollections(Nil, Set.empty, Vector.empty, Map.empty, None)
      )()
    }

    test("nested case classes") {
      compareReads(K.nestedR, U.nestedR)(
        root("owner { name = A, age = 1 }, others = [{ name = B, age = 2 }], by-name { c { name = C, age = 3 } }"),
        root("owner { name = A }, others = [{ age = 2 }, { name = B, age = x }], by-name { c = 1 }")
      )()
      compareWrites(K.nestedW, U.nestedW)(
        PcNested(PcPerson("A", 1, None), List(PcPerson("B", 2, Some("b"))), Map("c" -> PcPerson("C", 3, None)))
      )()
    }

    test("value classes") {
      compareReads(K.usesWrapperR, U.usesWrapperR)(
        root("id = abc, maybe-id = def"),
        root("id = abc"),
        root("id { value = abc }"),
        root("maybe-id = 1")
      )()
      compareWrites(K.usesWrapperW, U.usesWrapperW)(PcUsesWrapper(PcWrapper("a"), Some(PcWrapper("b"))))()
    }

    test("unknown keys are allowed by default") {
      compareReads(K.strictR, U.strictR)(root("name = a, port = 1, extra = 2, type = x"))()
    }
  }

  group("pureconfig oracle: ProductHint") {

    test("allowUnknownKeys = false") {
      compareReads(HintsKindlings.strictR, HintsUpstream.strictR)(
        root("name = a, port = 1"),
        root("name = a, port = 1, extra = 2"),
        root("name = a, port = 1, extra = 2, other { x = 1 }"),
        root("name = a, port = 1, type = x"),
        root("name = a, extra = 2"),
        root("port = x, extra = 2")
      )()
    }

    test("useDefaultArgs = false") {
      compareReads(HintsKindlings.noDefaultsR, HintsUpstream.noDefaultsR)(
        root("host = h, port = 1"),
        root("host = h"),
        root("")
      )()
    }

    test("custom field mapping (camelCase -> snake_case)") {
      compareReads(HintsKindlings.camelR, HintsUpstream.camelR)(
        root("first_name = a, max_retry_count = 1, http_url_value = u, a_1_b_2 = 2"),
        root("first-name = a, max-retry-count = 1, http-url-value = u, a-1-b-2 = 2")
      )()
      compareWrites(HintsKindlings.camelW, HintsUpstream.camelW)(PcCamel("a", 1, "u", 2))()
    }
  }

  // --- sealed families -----------------------------------------------------------------------------------------------

  group("pureconfig oracle: sealed families (FieldCoproductHint)") {

    test("default discriminator field 'type' with kebab-case values") {
      compareReads(K.shapeR, U.shapeR)(
        root("type = pc-circle, radius = 1.5"),
        root("type = pc-rectangle, width = 1, height = 2"),
        root("type = pc-empty"),
        root("type = pc-empty, extra = 1"),
        root("type = PcCircle, radius = 1.5"),
        root("type = pc-square, side = 1"),
        root("radius = 1.5"),
        root("type = 1, radius = 1.5"),
        root("type = [a], radius = 1.5"),
        root("type = pc-circle"),
        root("type = pc-circle, radius = big"),
        root("pc-circle { radius = 1.5 }"),
        value("pc-empty"),
        value("[1]")
      )()
      compareWrites(K.shapeW, U.shapeW)(PcCircle(1.5), PcRectangle(1, 2), PcEmpty)()
    }

    test("sealed family nested in a case class") {
      compareReads(K.drawingR, U.drawingR)(
        root("name = d, shapes = [{ type = pc-circle, radius = 1 }, { type = pc-empty }]"),
        root("name = d, shapes = [{ type = pc-circle }, { type = nope }]")
      )()
      compareWrites(K.drawingW, U.drawingW)(PcDrawing("d", List(PcCircle(1), PcEmpty)))()
    }

    test("case objects only (still a FieldCoproductHint by default)") {
      compareReads(K.colorR, U.colorR)(
        root("type = pc-red"),
        root("type = pc-dark-blue"),
        root("type = pc-blue"),
        value("pc-red"),
        value("PcRed")
      )()
      compareWrites(K.colorW, U.colorW)(PcRed, PcDarkBlue)()
    }

    test("a subtype with a field named like the discriminator") {
      compareReads(K.taggedR, U.taggedR)(
        root("type = pc-plain, value = 1"),
        root("type = pc-with-type, value = 1")
      )()
      compareWrites(K.taggedW, U.taggedW)(PcPlain(1), PcWithType("x", 1))()
    }

    test("custom discriminator field and value mapping") {
      compareReads(HintsKindlings.eventR, HintsUpstream.eventR)(
        root("kind = pc_clicked, x = 1, y = 2"),
        root("kind = pc_typed, text = hi"),
        root("type = pc-typed, text = hi"),
        root("kind = pc-typed, text = hi")
      )()
      compareWrites(HintsKindlings.eventW, HintsUpstream.eventW)(PcClicked(1, 2), PcTyped("hi"))()
    }
  }

  group("pureconfig oracle: sealed families (FirstSuccessCoproductHint)") {

    test("the first subtype (in the alphabetical order of names) that reads successfully wins") {
      compareReads(HintsKindlings.sourceR, HintsUpstream.sourceR)(
        root("""url = "http://a""""),
        root("""url = "http://a", path = p"""),
        root("path = p"),
        root("content = c"),
        root("content = c, encoding = latin1"),
        root("""type = pc-url-source, url = "http://a""""),
        root(""),
        root("url = 1"),
        value("[1]"),
        value("text")
      )()
      assertEquals(read(HintsKindlings.sourceR, root("""url = "http://a"""")), "Right(PcAnySource(http://a,None))")
      assertEquals(read(HintsKindlings.sourceR, root("path = p")), "Right(PcFileSource(p))")
    }

    test("written without disambiguation information") {
      compareWrites(HintsKindlings.sourceW, HintsUpstream.sourceW)(
        PcUrlSource("u"),
        PcFileSource("p"),
        PcAnySource("u", Some("p")),
        PcInlineSource("c")
      )()
    }
  }

  group("pureconfig oracle: enumerations (deriveEnumerationReader / deriveEnumerationWriter)") {

    test("case objects as kebab-case strings") {
      compareReads(EnumKindlings.colorR, EnumUpstream.colorR)(
        value("pc-red"),
        value("pc-dark-blue"),
        value("PcRed"),
        value("pc-blue"),
        value("1"),
        root("type = pc-red"),
        root("pc-red {}")
      )()
      compareWrites(EnumKindlings.colorW, EnumUpstream.colorW)(PcRed, PcGreen, PcDarkBlue)()
    }

    test("enumerations nested in a case class") {
      compareReads(EnumKindlings.paletteR, EnumUpstream.paletteR)(
        root("primary = pc-green, all = [pc-red, pc-dark-blue]"),
        root("primary = green, all = [pc-red, 3]")
      )()
      compareWrites(EnumKindlings.paletteW, EnumUpstream.paletteW)(PcPalette(PcGreen, List(PcRed, PcDarkBlue)))()
    }
  }
}
