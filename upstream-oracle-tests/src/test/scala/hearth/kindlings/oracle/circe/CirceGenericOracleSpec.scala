package hearth.kindlings.oracle.circe

import hearth.MacroSuite
import hearth.kindlings.circederivation.{Configuration, KindlingsDecoder, KindlingsEncoder}
import io.circe.generic.extras.Configuration as ExtrasConfiguration

/** Kindlings' circe derivation vs circe-generic (`deriveCodec`) and circe-generic-extras (`deriveConfiguredCodec`,
  * `deriveEnumerationCodec`, `deriveUnwrappedCodec`) with the equivalent configurations. On Scala 2 upstream is
  * shapeless-based; on Scala 3 both delegate to circe-core's `Configured*` derivation.
  */
final class CirceGenericOracleSpec extends MacroSuite {

  import CiSamples.*

  private def check(differences: List[CiDiff], expected: List[CiExpected]): Unit = {
    val problems = CirceOracle.unexplained(differences, expected)
    assert(problems.isEmpty, s"Kindlings vs upstream circe differences:\n${problems.mkString("\n")}")
  }

  import CiExpected.{onScala2, onScala3}

  // Documented (feature-parity.md circe "Nested sealed hierarchies (hand-written intermediate instances)"): Kindlings
  // (like Scala 3 core) uses the hand-written instance of an intermediate sealed trait under the intermediate's name;
  // Scala 2's shapeless-based derivations flatten the hierarchy to its leaves and ignore it.
  private val intermediateInstanceIgnored =
    CiExpected("Scala 2 upstream: the hand-written instance of an intermediate sealed trait is ignored") { d =>
      d.label.endsWith(" CiMedia") && d.subject.toLowerCase.contains("novel")
    }

  // Documented (feature-parity.md circe "Discriminator with a non-object child"): a child whose instance does not
  // write a JSON object cannot carry the discriminator; Kindlings (like generic-extras) writes it wrapped
  // (`{"Name":..}`), Scala 3 core drops the value (`{"type":"Name"}`).
  private val nonObjectChildWithDiscriminator =
    CiExpected("Scala 3 core: a non-object child is written as the discriminator alone") { d =>
      d.label.endsWith(" CiMedia") && d.operation == "encode" && d.subject == "CiNovel(Dune)" &&
      !d.upstream.contains("Dune")
    }

  private val wrapped = onScala2(intermediateInstanceIgnored)
  private val discriminated = onScala2(intermediateInstanceIgnored) ++ onScala3(nonObjectChildWithDiscriminator)

  /** An object with several keys naming constructors (all constructors of the fixtures start with `Ci`). */
  private def multiKeyWrapper(d: CiDiff, minConstructors: Int): Boolean =
    d.input.exists(CirceOracle.objects(_).exists { o =>
      o.size >= 2 && o.keys.count(_.startsWith("Ci")) >= minConstructors
    })

  // Documented (feature-parity.md circe "Wrapper object with superfluous keys", circe guide "Behaviour matching
  // upstream circe"): Kindlings follows Scala 3 core on both versions and uses the first key (JSON order) naming a
  // constructor; Scala 2's shapeless-based derivations try the constructors in (alphabetical) coproduct order.
  private val wrapperKeyChoice =
    CiExpected("Scala 2 upstream: a wrapper naming several constructors decodes the first one in coproduct order") { d =>
      d.operation.startsWith("decode") && d.kindlings.startsWith("ok") && d.upstream.startsWith("ok") &&
      multiKeyWrapper(d, minConstructors = 2)
    }

  // Documented (feature-parity.md circe "Wrapper object with superfluous keys"): with strictDecoding, Kindlings (like
  // Scala 3 core) rejects a wrapper object with other keys besides the constructor's; generic-extras on Scala 2 is
  // strict only inside case classes.
  private val strictWrapper =
    CiExpected("Scala 2 generic-extras: strictDecoding accepts wrapper objects with superfluous keys") { d =>
      d.operation.startsWith("decode") && d.kindlings.startsWith("fail") && d.upstream.startsWith("ok") &&
      multiKeyWrapper(d, minConstructors = 1)
    }

  /** Upstream codes `AnyVal` case classes as products; Kindlings' upstream-compatible setting. */
  private val upstreamValueClasses = Configuration.default.withoutValueClassUnwrapping

  test("default configuration vs circe-generic deriveCodec") {
    check(
      all("generic", new CiKindlingsSide()(upstreamValueClasses), generic, wrapperInputs),
      wrapped ++ onScala2(wrapperKeyChoice)
    )
  }

  test("default configuration vs circe-generic-extras Configuration.default") {
    val k = new CiKindlingsSide()(upstreamValueClasses)
    val u = new CiExtrasSide()(ExtrasConfiguration.default)
    check(all("extras-default", k, extras(u), wrapperInputs), wrapped ++ onScala2(wrapperKeyChoice))
  }

  test("snake_case members, kebab-case constructors, defaults") {
    val k =
      new CiKindlingsSide()(upstreamValueClasses.withSnakeCaseMemberNames.withKebabCaseConstructorNames.withDefaults)
    val u = new CiExtrasSide()(
      ExtrasConfiguration.default.withSnakeCaseMemberNames.withKebabCaseConstructorNames.withDefaults
    )
    check(all("snake-kebab-defaults", k, extras(u), Nil), wrapped)
  }

  test("discriminator, SCREAMING_SNAKE_CASE constructors, kebab-case members") {
    val k = new CiKindlingsSide()(
      upstreamValueClasses.withDiscriminator("type").withScreamingSnakeCaseConstructorNames.withKebabCaseMemberNames
    )
    val u = new CiExtrasSide()(
      ExtrasConfiguration.default
        .withDiscriminator("type")
        .withScreamingSnakeCaseConstructorNames
        .withKebabCaseMemberNames
    )
    check(
      all("discriminator", k, extras(u), Nil) ++
        CirceOracle.compare(
          "discriminator CiShape edge cases",
          pair(k.shape, u.shape),
          Nil,
          discriminatorInputs("CI_CIRCLE", "CI_NO_SHAPE")
        ),
      discriminated
    )
  }

  test("strict decoding with defaults") {
    val k = new CiKindlingsSide()(upstreamValueClasses.withStrictDecoding.withDefaults)
    val u = new CiExtrasSide()(ExtrasConfiguration.default.withStrictDecoding.withDefaults)
    check(
      all("strict", k, extras(u), wrapperInputs),
      wrapped ++ onScala2(strictWrapper)
    )
  }

  test("strict decoding, discriminator, PascalCase members, snake_case constructors") {
    val k = new CiKindlingsSide()(
      upstreamValueClasses.withStrictDecoding
        .withDiscriminator("kind")
        .withPascalCaseMemberNames
        .withSnakeCaseConstructorNames
    )
    val u = new CiExtrasSide()(
      ExtrasConfiguration.default.withStrictDecoding
        .withDiscriminator("kind")
        .withPascalCaseMemberNames
        .withSnakeCaseConstructorNames
    )
    check(
      all("strict-discriminator", k, extras(u), Nil) ++
        CirceOracle.compare(
          "strict-discriminator CiShape edge cases",
          pair(k.shape, u.shape),
          Nil,
          discriminatorInputs("ci_circle", "ci_no_shape").map(_.replace("\"type\"", "\"kind\""))
        ),
      discriminated
    )
  }

  test("enumerations: enumAsStrings vs deriveEnumerationCodec") {
    import io.circe.generic.extras.semiauto.deriveEnumerationCodec
    val inputs = List("\"CiRed\"", "\"ci_dark_blue\"", "\"CiBlack\"", "null", "1", "{\"CiRed\":{}}", "[]")
    val plain = {
      implicit val k: Configuration = Configuration.default.withEnumAsStrings
      implicit val u: ExtrasConfiguration = ExtrasConfiguration.default
      CirceOracle.compare(
        "enum",
        pair(
          KindlingsEncoder.derived[CiColor],
          KindlingsDecoder.derived[CiColor],
          deriveEnumerationCodec[CiColor],
          deriveEnumerationCodec[CiColor]
        ),
        colors,
        inputs
      )
    }
    val snake = {
      implicit val k: Configuration = Configuration.default.withEnumAsStrings.withSnakeCaseConstructorNames
      implicit val u: ExtrasConfiguration = ExtrasConfiguration.default.withSnakeCaseConstructorNames
      CirceOracle.compare(
        "enum snake",
        pair(
          KindlingsEncoder.derived[CiColor],
          KindlingsDecoder.derived[CiColor],
          deriveEnumerationCodec[CiColor],
          deriveEnumerationCodec[CiColor]
        ),
        colors,
        inputs
      )
    }
    check(plain ++ snake, Nil)
  }

  test("unwrapped codecs: deriveUnwrapped vs deriveUnwrappedCodec") {
    import io.circe.generic.extras.semiauto.deriveUnwrappedCodec
    implicit val k: Configuration = Configuration.default
    val inputs = List("\"a\"", "null", "1", "{\"inner\":\"a\"}")
    check(
      CirceOracle.compare(
        "unwrapped",
        pair(
          KindlingsEncoder.deriveUnwrapped[CiWrapper],
          KindlingsDecoder.deriveUnwrapped[CiWrapper],
          deriveUnwrappedCodec[CiWrapper],
          deriveUnwrappedCodec[CiWrapper]
        ),
        List(CiWrapper("a")),
        inputs
      ) ++ CirceOracle.compare(
        "unwrapped value class",
        pair(
          KindlingsEncoder.deriveUnwrapped[CiUserId],
          KindlingsDecoder.deriveUnwrapped[CiUserId],
          deriveUnwrappedCodec[CiUserId],
          deriveUnwrappedCodec[CiUserId]
        ),
        List(CiUserId(5)),
        List("5", "\"5\"", "null", "{\"value\":5}")
      ),
      Nil
    )
  }

  test("Kindlings' default value-class unwrapping (documented difference)") {
    val k = new CiKindlingsSide()(Configuration.default)
    val differences = CiGenericSide.withId.toList.flatMap { upstream =>
      CirceOracle.compare("value classes", pair(k.withId, upstream), withIds).filter(_.operation == "encode")
    }
    assertEquals(
      differences.map(_.toString),
      // On Scala 3 upstream has no derivation of value classes (no `Mirror`), so there is nothing to compare.
      if (CiGenericSide.withId.isEmpty) Nil
      else
        // Documented (feature-parity.md "Value classes", circe guide "Value classes: Kindlings default vs upstream"):
        // Kindlings unwraps `AnyVal` case classes by default; `withoutValueClassUnwrapping` gives upstream's format
        // (all other tests of this suite use it).
        List(
          "value classes: encode CiWithId(CiUserId(1),List(CiUserId(2), CiUserId(3)),Some(CiUserId(4))): " +
            """kindlings={"id":1,"ids":[2,3],"maybe":4} upstream={"id":{"value":1},"ids":[{"value":2},{"value":3}],"maybe":{"value":4}}"""
        )
    )
  }
}
