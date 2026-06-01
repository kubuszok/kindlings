package hearth.kindlings.pureconfigderivation

import com.typesafe.config.{ConfigFactory, ConfigRenderOptions}
import hearth.MacroSuite
import pureconfig.ConfigCursor

/** Tests for [[PureConfig]] convenience methods.
  *
  * Each group exercises a specific `with*` builder on [[PureConfig]] by deriving a reader and/or writer under the
  * customized config and verifying the resulting HOCON field/discriminator names and behaviors.
  */
final class PureConfigSpec extends MacroSuite {

  private def cursor(hocon: String): ConfigCursor =
    ConfigCursor(ConfigFactory.parseString(hocon).root(), Nil)

  private def renderConcise(value: com.typesafe.config.ConfigValue): String =
    value.render(ConfigRenderOptions.concise())

  // ----- Member name transforms -----

  group("withSnakeCaseMemberNames") {

    test("reader maps camelCase fields to snake_case keys") {
      implicit val cfg: PureConfig = PureConfig().withSnakeCaseMemberNames
      val r = KindlingsConfigReader.derived[MultiWordFields]
      r.from(cursor("{ first_name = Alice, last_name = Smith, postal_code = 12345 }")) ==>
        Right(MultiWordFields("Alice", "Smith", 12345))
    }

    test("writer emits snake_case keys") {
      implicit val cfg: PureConfig = PureConfig().withSnakeCaseMemberNames
      val w = KindlingsConfigWriter.derived[MultiWordFields]
      val rendered = renderConcise(w.to(MultiWordFields("Alice", "Smith", 12345)))
      assert(rendered.contains("\"first_name\":\"Alice\""))
      assert(rendered.contains("\"last_name\":\"Smith\""))
      assert(rendered.contains("\"postal_code\":12345"))
    }

    test("round-trip") {
      implicit val cfg: PureConfig = PureConfig().withSnakeCaseMemberNames
      val r = KindlingsConfigReader.derived[MultiWordFields]
      val w = KindlingsConfigWriter.derived[MultiWordFields]
      val original = MultiWordFields("Bob", "Jones", 99999)
      r.from(ConfigCursor(w.to(original), Nil)) ==> Right(original)
    }
  }

  group("withKebabCaseMemberNames") {

    test("reader maps camelCase fields to kebab-case keys (default behavior)") {
      implicit val cfg: PureConfig = PureConfig().withKebabCaseMemberNames
      val r = KindlingsConfigReader.derived[MultiWordFields]
      r.from(cursor("{ first-name = Alice, last-name = Smith, postal-code = 12345 }")) ==>
        Right(MultiWordFields("Alice", "Smith", 12345))
    }

    test("writer emits kebab-case keys") {
      implicit val cfg: PureConfig = PureConfig().withKebabCaseMemberNames
      val w = KindlingsConfigWriter.derived[MultiWordFields]
      val rendered = renderConcise(w.to(MultiWordFields("Alice", "Smith", 12345)))
      assert(rendered.contains("\"first-name\":\"Alice\""))
      assert(rendered.contains("\"last-name\":\"Smith\""))
      assert(rendered.contains("\"postal-code\":12345"))
    }

    test("round-trip") {
      implicit val cfg: PureConfig = PureConfig().withKebabCaseMemberNames
      val r = KindlingsConfigReader.derived[MultiWordFields]
      val w = KindlingsConfigWriter.derived[MultiWordFields]
      val original = MultiWordFields("Charlie", "Brown", 11111)
      r.from(ConfigCursor(w.to(original), Nil)) ==> Right(original)
    }
  }

  group("withPascalCaseMemberNames") {

    test("reader maps camelCase fields to PascalCase keys") {
      implicit val cfg: PureConfig = PureConfig().withPascalCaseMemberNames
      val r = KindlingsConfigReader.derived[MultiWordFields]
      r.from(cursor("{ FirstName = Alice, LastName = Smith, PostalCode = 12345 }")) ==>
        Right(MultiWordFields("Alice", "Smith", 12345))
    }

    test("writer emits PascalCase keys") {
      implicit val cfg: PureConfig = PureConfig().withPascalCaseMemberNames
      val w = KindlingsConfigWriter.derived[MultiWordFields]
      val rendered = renderConcise(w.to(MultiWordFields("Alice", "Smith", 12345)))
      assert(rendered.contains("\"FirstName\":\"Alice\""))
      assert(rendered.contains("\"LastName\":\"Smith\""))
      assert(rendered.contains("\"PostalCode\":12345"))
    }

    test("round-trip") {
      implicit val cfg: PureConfig = PureConfig().withPascalCaseMemberNames
      val r = KindlingsConfigReader.derived[MultiWordFields]
      val w = KindlingsConfigWriter.derived[MultiWordFields]
      val original = MultiWordFields("Diana", "Prince", 22222)
      r.from(ConfigCursor(w.to(original), Nil)) ==> Right(original)
    }
  }

  group("withScreamingSnakeCaseMemberNames") {

    test("reader maps camelCase fields to SCREAMING_SNAKE_CASE keys") {
      implicit val cfg: PureConfig = PureConfig().withScreamingSnakeCaseMemberNames
      val r = KindlingsConfigReader.derived[MultiWordFields]
      r.from(cursor("{ FIRST_NAME = Alice, LAST_NAME = Smith, POSTAL_CODE = 12345 }")) ==>
        Right(MultiWordFields("Alice", "Smith", 12345))
    }

    test("writer emits SCREAMING_SNAKE_CASE keys") {
      implicit val cfg: PureConfig = PureConfig().withScreamingSnakeCaseMemberNames
      val w = KindlingsConfigWriter.derived[MultiWordFields]
      val rendered = renderConcise(w.to(MultiWordFields("Alice", "Smith", 12345)))
      assert(rendered.contains("\"FIRST_NAME\":\"Alice\""))
      assert(rendered.contains("\"LAST_NAME\":\"Smith\""))
      assert(rendered.contains("\"POSTAL_CODE\":12345"))
    }

    test("round-trip") {
      implicit val cfg: PureConfig = PureConfig().withScreamingSnakeCaseMemberNames
      val r = KindlingsConfigReader.derived[MultiWordFields]
      val w = KindlingsConfigWriter.derived[MultiWordFields]
      val original = MultiWordFields("Eve", "Adams", 33333)
      r.from(ConfigCursor(w.to(original), Nil)) ==> Right(original)
    }
  }

  group("withCamelCaseMemberNames") {

    test("reader maps camelCase fields to camelCase keys (identity)") {
      implicit val cfg: PureConfig = PureConfig().withCamelCaseMemberNames
      val r = KindlingsConfigReader.derived[MultiWordFields]
      r.from(cursor("{ firstName = Alice, lastName = Smith, postalCode = 12345 }")) ==>
        Right(MultiWordFields("Alice", "Smith", 12345))
    }

    test("writer emits camelCase keys") {
      implicit val cfg: PureConfig = PureConfig().withCamelCaseMemberNames
      val w = KindlingsConfigWriter.derived[MultiWordFields]
      val rendered = renderConcise(w.to(MultiWordFields("Alice", "Smith", 12345)))
      assert(rendered.contains("\"firstName\":\"Alice\""))
      assert(rendered.contains("\"lastName\":\"Smith\""))
      assert(rendered.contains("\"postalCode\":12345"))
    }

    test("round-trip") {
      implicit val cfg: PureConfig = PureConfig().withCamelCaseMemberNames
      val r = KindlingsConfigReader.derived[MultiWordFields]
      val w = KindlingsConfigWriter.derived[MultiWordFields]
      val original = MultiWordFields("Frank", "Castle", 44444)
      r.from(ConfigCursor(w.to(original), Nil)) ==> Right(original)
    }
  }

  // ----- Strict decoding / allow unknown keys -----

  group("withStrictDecoding") {

    test("rejects unknown keys in HOCON") {
      implicit val cfg: PureConfig = PureConfig().withStrictDecoding
      val r = KindlingsConfigReader.derived[SimplePerson]
      val result = r.from(cursor("{ name = Alice, age = 30, extra = oops }"))
      assert(result.isLeft)
    }

    test("accepts valid HOCON without extra keys") {
      implicit val cfg: PureConfig = PureConfig().withStrictDecoding
      val r = KindlingsConfigReader.derived[SimplePerson]
      r.from(cursor("{ name = Alice, age = 30 }")) ==> Right(SimplePerson("Alice", 30))
    }

    test("rejects unknown keys in sealed trait subtype") {
      implicit val cfg: PureConfig = PureConfig().withStrictDecoding
      val r = KindlingsConfigReader.derived[Shape]
      val result = r.from(cursor("{ type = circle, radius = 1.5, extra = oops }"))
      assert(result.isLeft)
    }

    test("accepts valid sealed trait subtype without extra keys") {
      implicit val cfg: PureConfig = PureConfig().withStrictDecoding
      val r = KindlingsConfigReader.derived[Shape]
      r.from(cursor("{ type = circle, radius = 1.5 }")) ==> Right(Circle(1.5))
    }
  }

  group("withAllowUnknownKeys") {

    test("accepts unknown keys in HOCON") {
      implicit val cfg: PureConfig = PureConfig().withAllowUnknownKeys
      val r = KindlingsConfigReader.derived[SimplePerson]
      r.from(cursor("{ name = Alice, age = 30, extra = ignored }")) ==> Right(SimplePerson("Alice", 30))
    }

    test("restores permissive mode after strict") {
      implicit val cfg: PureConfig = PureConfig().withStrictDecoding.withAllowUnknownKeys
      val r = KindlingsConfigReader.derived[SimplePerson]
      r.from(cursor("{ name = Alice, age = 30, extra = ok }")) ==> Right(SimplePerson("Alice", 30))
    }
  }

  // ----- Use defaults / without defaults -----

  group("withUseDefaults") {

    test("missing fields fall back to defaults") {
      implicit val cfg: PureConfig = PureConfig().withUseDefaults
      val r = KindlingsConfigReader.derived[WithDefaults]
      r.from(cursor("{ name = Alice }")) ==> Right(WithDefaults("Alice"))
    }

    test("provided fields override defaults") {
      implicit val cfg: PureConfig = PureConfig().withUseDefaults
      val r = KindlingsConfigReader.derived[WithDefaults]
      r.from(cursor("{ name = Alice, age = 50, active = false }")) ==> Right(WithDefaults("Alice", 50, false))
    }
  }

  group("withoutUseDefaults") {

    test("missing fields fail even when defaults exist") {
      implicit val cfg: PureConfig = PureConfig().withoutUseDefaults
      val r = KindlingsConfigReader.derived[WithDefaults]
      val result = r.from(cursor("{ name = Bob }"))
      assert(result.isLeft)
    }

    test("all fields present succeeds") {
      implicit val cfg: PureConfig = PureConfig().withoutUseDefaults
      val r = KindlingsConfigReader.derived[WithDefaults]
      r.from(cursor("{ name = Bob, age = 40, active = false }")) ==> Right(WithDefaults("Bob", 40, false))
    }
  }

  // ----- Discriminator -----

  group("withDiscriminator") {

    test("reader uses custom discriminator field") {
      implicit val cfg: PureConfig = PureConfig().withDiscriminator("kind")
      val r = KindlingsConfigReader.derived[Shape]
      r.from(cursor("{ kind = circle, radius = 1.5 }")) ==> Right(Circle(1.5))
      r.from(cursor("{ kind = rectangle, width = 2.0, height = 3.0 }")) ==> Right(Rectangle(2.0, 3.0))
    }

    test("writer uses custom discriminator field") {
      implicit val cfg: PureConfig = PureConfig().withDiscriminator("kind")
      val w = KindlingsConfigWriter.derived[Shape]
      val rendered = renderConcise(w.to(Circle(1.5)))
      assert(rendered.contains("\"kind\":\"circle\""))
      assert(!rendered.contains("\"type\":"))
    }

    test("round-trip with custom discriminator") {
      implicit val cfg: PureConfig = PureConfig().withDiscriminator("kind")
      val r = KindlingsConfigReader.derived[Shape]
      val w = KindlingsConfigWriter.derived[Shape]
      val original: Shape = Rectangle(4.0, 5.0)
      r.from(ConfigCursor(w.to(original), Nil)) ==> Right(original)
    }
  }

  // ----- Wrapped subtypes -----

  group("withWrappedSubtypes") {

    test("reader uses single-key wrapping for sealed traits") {
      implicit val cfg: PureConfig = PureConfig().withWrappedSubtypes
      val r = KindlingsConfigReader.derived[Shape]
      r.from(cursor("{ circle = { radius = 1.5 } }")) ==> Right(Circle(1.5))
      r.from(cursor("{ rectangle = { width = 2.0, height = 3.0 } }")) ==> Right(Rectangle(2.0, 3.0))
    }

    test("writer uses single-key wrapping for sealed traits") {
      implicit val cfg: PureConfig = PureConfig().withWrappedSubtypes
      val w = KindlingsConfigWriter.derived[Shape]
      val rendered = renderConcise(w.to(Circle(1.5)))
      assert(rendered.contains("\"circle\""))
      assert(!rendered.contains("\"type\":"))
    }

    test("round-trip with wrapped subtypes") {
      implicit val cfg: PureConfig = PureConfig().withWrappedSubtypes
      val r = KindlingsConfigReader.derived[Shape]
      val w = KindlingsConfigWriter.derived[Shape]
      val original: Shape = Rectangle(4.0, 5.0)
      r.from(ConfigCursor(w.to(original), Nil)) ==> Right(original)
    }

    test("case-object enum encodes as string") {
      implicit val cfg: PureConfig = PureConfig().withWrappedSubtypes
      val w = KindlingsConfigWriter.derived[CardinalDirection]
      renderConcise(w.to(North)) ==> "\"north\""
    }
  }

  // ----- Constructor name transforms -----

  group("withSnakeCaseConstructorNames") {

    test("reader uses snake_case constructor names") {
      implicit val cfg: PureConfig = PureConfig().withSnakeCaseConstructorNames
      val r = KindlingsConfigReader.derived[ColorChoice]
      r.from(cursor("{ type = bright_red, intensity = 10 }")) ==> Right(BrightRed(10))
      r.from(cursor("{ type = dark_blue, intensity = 5 }")) ==> Right(DarkBlue(5))
    }

    test("writer uses snake_case constructor names") {
      implicit val cfg: PureConfig = PureConfig().withSnakeCaseConstructorNames
      val w = KindlingsConfigWriter.derived[ColorChoice]
      val rendered = renderConcise(w.to(BrightRed(10)))
      assert(rendered.contains("\"type\":\"bright_red\""))
    }

    test("round-trip") {
      implicit val cfg: PureConfig = PureConfig().withSnakeCaseConstructorNames
      val r = KindlingsConfigReader.derived[ColorChoice]
      val w = KindlingsConfigWriter.derived[ColorChoice]
      val original: ColorChoice = DarkBlue(7)
      r.from(ConfigCursor(w.to(original), Nil)) ==> Right(original)
    }
  }

  group("withKebabCaseConstructorNames") {

    test("reader uses kebab-case constructor names (default)") {
      implicit val cfg: PureConfig = PureConfig().withKebabCaseConstructorNames
      val r = KindlingsConfigReader.derived[ColorChoice]
      r.from(cursor("{ type = bright-red, intensity = 10 }")) ==> Right(BrightRed(10))
      r.from(cursor("{ type = dark-blue, intensity = 5 }")) ==> Right(DarkBlue(5))
    }

    test("writer uses kebab-case constructor names") {
      implicit val cfg: PureConfig = PureConfig().withKebabCaseConstructorNames
      val w = KindlingsConfigWriter.derived[ColorChoice]
      val rendered = renderConcise(w.to(BrightRed(10)))
      assert(rendered.contains("\"type\":\"bright-red\""))
    }

    test("round-trip") {
      implicit val cfg: PureConfig = PureConfig().withKebabCaseConstructorNames
      val r = KindlingsConfigReader.derived[ColorChoice]
      val w = KindlingsConfigWriter.derived[ColorChoice]
      val original: ColorChoice = BrightRed(3)
      r.from(ConfigCursor(w.to(original), Nil)) ==> Right(original)
    }
  }

  // ----- Composed convenience methods -----

  group("combined convenience methods") {

    test("snake_case members + custom discriminator") {
      implicit val cfg: PureConfig = PureConfig().withSnakeCaseMemberNames
        .withDiscriminator("kind")
      val r = KindlingsConfigReader.derived[Shape]
      r.from(cursor("{ kind = circle, radius = 1.5 }")) ==> Right(Circle(1.5))
    }

    test("snake_case members + strict decoding rejects extras") {
      implicit val cfg: PureConfig = PureConfig().withSnakeCaseMemberNames.withStrictDecoding
      val r = KindlingsConfigReader.derived[MultiWordFields]
      val result = r.from(cursor("{ first_name = Alice, last_name = Smith, postal_code = 123, extra = fail }"))
      assert(result.isLeft)
    }

    test("camelCase members + snake_case constructors + wrapped subtypes") {
      implicit val cfg: PureConfig =
        PureConfig().withCamelCaseMemberNames.withSnakeCaseConstructorNames.withWrappedSubtypes
      val r = KindlingsConfigReader.derived[ColorChoice]
      r.from(cursor("{ bright_red = { intensity = 10 } }")) ==> Right(BrightRed(10))
    }

    test("withoutUseDefaults + PascalCase members") {
      implicit val cfg: PureConfig = PureConfig().withoutUseDefaults.withPascalCaseMemberNames
      val r = KindlingsConfigReader.derived[WithDefaultsMultiWord]
      // Must provide all fields, including those with defaults
      r.from(cursor("{ FirstName = Alice, MiddleName = Marie, Age = 30 }")) ==>
        Right(WithDefaultsMultiWord("Alice", "Marie", 30))
    }

    test("withoutUseDefaults + PascalCase members fails on missing defaulted field") {
      implicit val cfg: PureConfig = PureConfig().withoutUseDefaults.withPascalCaseMemberNames
      val r = KindlingsConfigReader.derived[WithDefaultsMultiWord]
      val result = r.from(cursor("{ FirstName = Alice }"))
      assert(result.isLeft)
    }

    test("full round-trip: SCREAMING_SNAKE members + snake_case constructors + custom discriminator") {
      implicit val cfg: PureConfig = PureConfig().withScreamingSnakeCaseMemberNames.withSnakeCaseConstructorNames
        .withDiscriminator("variant")
      val r = KindlingsConfigReader.derived[ColorChoice]
      val w = KindlingsConfigWriter.derived[ColorChoice]
      val original: ColorChoice = DarkBlue(42)
      val written = w.to(original)
      val rendered = renderConcise(written)
      assert(rendered.contains("\"variant\":\"dark_blue\""))
      assert(rendered.contains("\"INTENSITY\":42"))
      r.from(ConfigCursor(written, Nil)) ==> Right(original)
    }
  }
}
