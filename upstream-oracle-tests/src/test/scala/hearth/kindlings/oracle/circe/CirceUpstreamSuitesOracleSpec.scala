package hearth.kindlings.oracle.circe

import hearth.MacroSuite
import hearth.kindlings.circederivation.Configuration
import io.circe.generic.extras.Configuration as ExtrasConfiguration

/** The fixtures and the configurations of upstream's own derivation test suites, run through Kindlings and upstream:
  *   - circe-generic `SemiautoDerivedSuite` (Scala 2) / circe-core `DerivesSuite`, `SemiautoDerivationSuite` (Scala 3)
  *   - circe-generic-extras `ConfiguredSemiautoDerivedSuite`, `ConfiguredAutoDerivedSuite`,
  *     `SharedConfiguredSemiautoDerivedSuite`
  *   - circe-core `ConfiguredDerivesSuite` (Scala 3; generic-extras 0.14.5 delegates to it)
  *
  * Each test names the upstream tests whose configuration it uses; the inputs of those tests (absent keys, `null`s,
  * wrong types, unknown keys) are among the mutations `CirceOracle.compare` generates, the rest is listed explicitly.
  */
final class CirceUpstreamSuitesOracleSpec extends MacroSuite {

  import CiExpected.{onScala2, onScala3}

  private def check(differences: List[CiDiff], expected: List[CiExpected]): Unit = {
    val problems = CirceOracle.unexplained(differences, expected)
    assert(problems.isEmpty, s"Kindlings vs upstream circe differences:\n${problems.mkString("\n")}")
  }

  private def configured(
      label: String,
      expected: List[CiExpected],
      fooWrapperInputs: List[String] = Nil
  )(k: Configuration, u: ExtrasConfiguration): Unit =
    check(CuSamples.all(label, CuCodecs.kindlings(k), CuCodecs.extras(u), fooWrapperInputs), expected)

  // Documented (feature-parity.md circe "Discriminator with a non-object child"): `CuBaz`'s hand-written instance
  // writes a JSON array, which cannot carry the discriminator; Kindlings (like generic-extras on Scala 2) writes it
  // wrapped (`{"CuBaz":[..]}`) and reads it back, Scala 3 core drops the value (`{"type":"CuBaz"}`).
  private val nonObjectChildWithDiscriminator =
    CiExpected("Scala 3 core: a non-object child is written as the discriminator alone") { d =>
      d.label.contains("CuFoo") && d.operation == "encode" && d.subject.contains("CuBaz")
    }

  // Documented (same row, "Improvement (decoding)"): generic-extras on Scala 2 writes the wrapped form but cannot read
  // it back; Kindlings can.
  private val nonObjectChildReadBack =
    CiExpected("Scala 2 generic-extras: a non-object child written wrapped next to a discriminator is not read back") {
      d =>
        d.operation.startsWith("decode") && d.kindlings.startsWith("ok") && d.upstream.startsWith("fail") &&
        d.subject.toLowerCase.replace("_", "").replace("-", "").contains("\"cubaz\":[")
    }
  private val discriminated = onScala3(nonObjectChildWithDiscriminator) ++ onScala2(nonObjectChildReadBack)

  /** An object with at least `minConstructors` keys naming constructors (they all start with `Cu`) and 2+ keys. */
  private def multiKeyWrapper(d: CiDiff, minConstructors: Int): Boolean =
    d.input.exists(CirceOracle.objects(_).exists { o =>
      o.size >= 2 && o.keys.count(_.toLowerCase.startsWith("cu")) >= minConstructors
    })

  // Documented (feature-parity.md circe "Wrapper object with superfluous keys"): see CirceGenericOracleSpec.
  private val wrapperKeyChoice = onScala2(
    CiExpected("Scala 2 upstream: a wrapper naming several constructors decodes the first one in coproduct order") { d =>
      d.operation.startsWith("decode") && d.kindlings.startsWith("ok") && d.upstream.startsWith("ok") &&
      multiKeyWrapper(d, minConstructors = 2)
    }
  )
  private val strictWrapper = onScala2(
    CiExpected("Scala 2 generic-extras: strictDecoding accepts wrapper objects with superfluous keys") { d =>
      d.operation.startsWith("decode") && d.kindlings.startsWith("fail") && d.upstream.startsWith("ok") &&
      multiKeyWrapper(d, minConstructors = 1)
    }
  )

  // Documented (feature-parity.md circe "Non-object input for a case class whose fields all have defaults"): with
  // `useDefaults`, generic-extras on Scala 2 reads every field through a failed cursor and so decodes ANY non-object
  // JSON (`null`, `1`, `"wrong"`, `[]`) to the all-defaults value; Scala 3 core rejects non-objects first, and so does
  // Kindlings on both versions.
  private val allDefaultsFromNonObject = onScala2(
    CiExpected(
      "Scala 2 generic-extras: useDefaults decodes a non-object to a case class whose fields all have defaults"
    ) { d =>
      (d.label.endsWith("CuFooWithDefault") || d.label.endsWith("CuGenericFoo")) &&
      d.operation.startsWith("decode") && d.input.exists(!_.isObject) &&
      d.kindlings.startsWith("fail") && d.upstream.startsWith("ok")
    }
  )

  /** Wrapper objects of `CuFoo` with several keys. */
  private val fooWrapperInputs = List(
    """{"CuBar":{"i":1,"s":"s"},"CuBaz":["a"]}""",
    """{"CuBaz":["a"],"CuBar":{"i":1,"s":"s"}}""",
    """{"extraField":true,"CuBar":{"i":1,"s":"s"}}""",
    """{"extraField":true,"extraField2":15,"CuBar":{"i":1,"s":"s"}}""",
    """{"CuBar":{"i":1,"s":"s"},"extraField":true}""",
    """{"invalid-name":{"i":1,"s":"s"}}"""
  )

  test("circe-generic deriveCodec (SemiautoDerivedSuite / DerivesSuite / SemiautoDerivationSuite)") {
    check(
      CuSamples.all("generic", CuCodecs.kindlings(Configuration.default), CuCodecs.generic, fooWrapperInputs),
      wrapperKeyChoice
    )
  }

  test("Configuration.default (ConfiguredAutoDerivedSuite 'default configuration')") {
    configured("default", wrapperKeyChoice, fooWrapperInputs)(Configuration.default, ExtrasConfiguration.default)
  }

  test("Configuration#transformMemberNames: snake_case, SCREAMING_SNAKE_CASE, kebab-case, PascalCase") {
    configured("snake members", Nil)(
      Configuration.default.withSnakeCaseMemberNames,
      ExtrasConfiguration.default.withSnakeCaseMemberNames
    )
    configured("screaming members", Nil)(
      Configuration.default.withScreamingSnakeCaseMemberNames,
      ExtrasConfiguration.default.withScreamingSnakeCaseMemberNames
    )
    configured("kebab members", Nil)(
      Configuration.default.withKebabCaseMemberNames,
      ExtrasConfiguration.default.withKebabCaseMemberNames
    )
    configured("pascal members", Nil)(
      Configuration.default.withPascalCaseMemberNames,
      ExtrasConfiguration.default.withPascalCaseMemberNames
    )
  }

  test(
    "Configuration#useDefaults (also 'Option[T] with/without default', 'withDefaults should support generic classes')"
  ) {
    configured("defaults", allDefaultsFromNonObject)(
      Configuration.default.withDefaults,
      ExtrasConfiguration.default.withDefaults
    )
  }

  test("Configuration#discriminator should support a field indicating constructor") {
    configured("discriminator", discriminated)(
      Configuration.default.withDiscriminator("type"),
      ExtrasConfiguration.default.withDiscriminator("type")
    )
  }

  test(
    "Configuration#transformConstructorNames with a discriminator: snake_case, SCREAMING_SNAKE_CASE, kebab, Pascal"
  ) {
    configured("snake constructors", discriminated)(
      Configuration.default.withDiscriminator("type").withSnakeCaseConstructorNames,
      ExtrasConfiguration.default.withDiscriminator("type").withSnakeCaseConstructorNames
    )
    configured("screaming constructors", discriminated)(
      Configuration.default.withDiscriminator("type").withScreamingSnakeCaseConstructorNames,
      ExtrasConfiguration.default.withDiscriminator("type").withScreamingSnakeCaseConstructorNames
    )
    configured("kebab constructors", discriminated)(
      Configuration.default.withDiscriminator("type").withKebabCaseConstructorNames,
      ExtrasConfiguration.default.withDiscriminator("type").withKebabCaseConstructorNames
    )
    configured("pascal constructors", discriminated)(
      Configuration.default.withDiscriminator("type").withPascalCaseConstructorNames,
      ExtrasConfiguration.default.withDiscriminator("type").withPascalCaseConstructorNames
    )
  }

  test("transformConstructorNames without a discriminator (wrapper keys)") {
    configured("snake wrapper", Nil)(
      Configuration.default.withSnakeCaseConstructorNames,
      ExtrasConfiguration.default.withSnakeCaseConstructorNames
    )
  }

  test("Configuration options should work together / Semi-automatic derivation should support configuration") {
    configured("together", discriminated ++ allDefaultsFromNonObject)(
      Configuration.default.withSnakeCaseMemberNames.withDefaults.withDiscriminator("type"),
      ExtrasConfiguration.default.withSnakeCaseMemberNames.withDefaults.withDiscriminator("type")
    )
    configured("custom", discriminated ++ allDefaultsFromNonObject)(
      Configuration.default.withSnakeCaseMemberNames.withDefaults
        .withDiscriminator("type")
        .withSnakeCaseConstructorNames,
      ExtrasConfiguration.default.withSnakeCaseMemberNames.withDefaults
        .withDiscriminator("type")
        .withSnakeCaseConstructorNames
    )
  }

  test("it should support configured strict decoding / not transform discriminator for strict decoding checks") {
    configured("strict type_field", discriminated ++ allDefaultsFromNonObject)(
      Configuration.default.withSnakeCaseMemberNames.withDefaults
        .withDiscriminator("type_field")
        .withSnakeCaseConstructorNames
        .withStrictDecoding,
      ExtrasConfiguration.default.withSnakeCaseMemberNames.withDefaults
        .withDiscriminator("type_field")
        .withSnakeCaseConstructorNames
        .withStrictDecoding
    )
    configured("strict typeField", discriminated ++ allDefaultsFromNonObject)(
      Configuration.default.withSnakeCaseMemberNames.withDefaults
        .withDiscriminator("typeField")
        .withSnakeCaseConstructorNames
        .withStrictDecoding,
      ExtrasConfiguration.default.withSnakeCaseMemberNames.withDefaults
        .withDiscriminator("typeField")
        .withSnakeCaseConstructorNames
        .withStrictDecoding
    )
  }

  test("Configuration#strictDecoding for sum types (wrapper objects) and product types") {
    configured("strict wrapper", strictWrapper, fooWrapperInputs)(
      Configuration.default.withStrictDecoding,
      ExtrasConfiguration.default.withStrictDecoding
    )
  }
}
