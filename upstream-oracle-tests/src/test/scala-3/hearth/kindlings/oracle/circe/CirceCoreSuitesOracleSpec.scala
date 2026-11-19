package hearth.kindlings.oracle.circe

import hearth.MacroSuite
import hearth.kindlings.circederivation.{renaming as kRenaming, Configuration, KindlingsCodecAsObject}
import io.circe.Codec
import io.circe.derivation.{Configuration as CoreConfiguration, ConfiguredCodec, ConfiguredEnumCodec}

/** Upstream needs the instance of a recursive type in scope while deriving it (`derives Codec` in its own suite). */
private object CuCoreRecursive {
  given codec: Codec.AsObject[CuRecursiveEnumAdt] = Codec.AsObject.derived[CuRecursiveEnumAdt]
}

/** The Scala 3 fixtures of circe-core's own test suites (`DerivesSuite`, `ConfiguredDerivesSuite`,
  * `ConfiguredEnumDerivesSuites`), derived by Kindlings and by circe-core.
  */
final class CirceCoreSuitesOracleSpec extends MacroSuite {

  private def check(differences: List[CiDiff], expected: List[CiExpected] = Nil): Unit = {
    val problems = CirceOracle.unexplained(differences, expected)
    assert(problems.isEmpty, s"Kindlings vs circe-core differences:\n${problems.mkString("\n")}")
  }

  private val vegetables =
    List(CuVegetable.Potato("russet"), CuVegetable.Carrot(1.5), CuVegetable.Onion(3), CuVegetable.Turnip)
  private val recursiveEnums = List(
    CuRecursiveEnumAdt.BaseAdtExample("a"),
    CuRecursiveEnumAdt.NestedAdtExample(CuRecursiveEnumAdt.NestedAdtExample(CuRecursiveEnumAdt.BaseAdtExample("b")))
  )
  private val longSums = List(CuLongSum.v1("a"), CuLongSum.v17("b"), CuLongSum.v33("c"))
  private val longEnums = List(CuLongEnum.v1, CuLongEnum.v17, CuLongEnum.v33)

  // DerivesSuite: `enum Vegetable derives Codec`, `enum RecursiveEnumAdt derives Codec`, `enum LongSum`
  test("DerivesSuite: enums with parameterised cases (Vegetable, RecursiveEnumAdt, LongSum)") {
    given Configuration = Configuration.default
    check(
      CirceOracle.compare(
        "Vegetable",
        CiSamples.pair(KindlingsCodecAsObject.derived[CuVegetable], Codec.AsObject.derived[CuVegetable]),
        vegetables,
        List("""{"Turnip":null}""", """{"Turnip":{"x":1}}""", """{"Leek":{}}""", """{"x":1,"Onion":{"layers":1}}""")
      ) ++ CirceOracle.compare(
        "RecursiveEnumAdt",
        CiSamples.pair(KindlingsCodecAsObject.derived[CuRecursiveEnumAdt], CuCoreRecursive.codec),
        recursiveEnums
      ) ++ CirceOracle.compare(
        "LongSum",
        CiSamples.pair(KindlingsCodecAsObject.derived[CuLongSum], Codec.AsObject.derived[CuLongSum]),
        longSums,
        List("""{"v34":{"str":"a"}}""")
      )
    )
  }

  // DerivesSuite: `enum LongEnum derives Encoder, Decoder` — a parameterless enum derived with the plain (object)
  // encoding, and the same enum as strings (`ConfiguredEnumCodec`).
  test("DerivesSuite: a parameterless enum (LongEnum) with the object encoding and as strings") {
    val objects = {
      given Configuration = Configuration.default
      CirceOracle.compare(
        "LongEnum objects",
        CiSamples.pair(KindlingsCodecAsObject.derived[CuLongEnum], Codec.AsObject.derived[CuLongEnum]),
        longEnums,
        List("\"v1\"", """{"v34":{}}""", """{"v1":null}""")
      )
    }
    val strings = {
      given CoreConfiguration = CoreConfiguration.default
      CirceOracle.compare(
        "LongEnum strings",
        CiSamples.pair(KindlingsCodecAsObject.deriveEnum[CuLongEnum](), ConfiguredEnumCodec.derived[CuLongEnum]),
        longEnums,
        List("\"v34\"", """{"v1":{}}""")
      )
    }
    check(objects ++ strings)
  }

  // DerivesSuite "Codec[ProductWithTaggedMember] (#2135)"
  test("DerivesSuite: a member of an intersection type uses the instance of that type (#2135)") {
    given Configuration = Configuration.default
    check(
      CirceOracle.compare(
        "ProductWithTaggedMember",
        CiSamples.pair(
          KindlingsCodecAsObject.derived[CuProductWithTaggedMember],
          Codec.AsObject.derived[CuProductWithTaggedMember]
        ),
        List(CuProductWithTaggedMember.fromUntagged("tagged"))
      )
    )
  }

  // ConfiguredDerivesSuite "Codec for hierarchy of more than 2 level with discriminator should encode and decode
  // correctly, even if a parent's sibling has a field with the same name as a Child type"
  test("ConfiguredDerivesSuite: three levels, a sibling's field named like a leaf") {
    val values: List[CuGreatGrandParent] = List(CuChild(1, "a"), CuUncle(2))
    def compare(label: String, k: Configuration, u: CoreConfiguration): List[CiDiff] = {
      given Configuration = k
      given CoreConfiguration = u
      CirceOracle.compare(
        label,
        CiSamples.pair(KindlingsCodecAsObject.derived[CuGreatGrandParent], ConfiguredCodec.derived[CuGreatGrandParent]),
        values,
        List(
          """{"type":"CuUncle","CuChild":{"a":1,"b":"a"}}""",
          """{"CuUncle":{"CuChild":1},"CuChild":{"a":1,"b":"a"}}"""
        )
      )
    }
    check(
      compare(
        "discriminator",
        Configuration.default.withDiscriminator("type"),
        CoreConfiguration.default.withDiscriminator("type")
      ) ++
        compare("wrapper", Configuration.default, CoreConfiguration.default) ++
        compare(
          "strict discriminator",
          Configuration.default.withDiscriminator("type").withStrictDecoding,
          CoreConfiguration.default.withDiscriminator("type").withStrictDecoding
        )
    )
  }

  // ConfiguredEnumDerivesSuites: IntercardinalDirections with every constructor name transformation,
  // "Fail to decode if case name does not exist", "Should work with nested hierarchies" (with a diamond)
  test("ConfiguredEnumDerivesSuites: constructor name transformations, unknown cases, nested hierarchies") {
    val directions = CuIntercardinalDirections.values.toList
    val inputs = List("\"NorthNorth\"", "\"north_east\"", "\"SOUTH_EAST\"", "\"south-west\"", "null", "{}")
    def compare(label: String, k: String => String, u: CoreConfiguration): List[CiDiff] = {
      given CoreConfiguration = u
      CirceOracle.compare(
        label,
        CiSamples.pair(
          KindlingsCodecAsObject.deriveEnum[CuIntercardinalDirections](k),
          ConfiguredEnumCodec.derived[CuIntercardinalDirections]
        ),
        directions,
        inputs
      )
    }
    val hierarchical = {
      given CoreConfiguration = CoreConfiguration.default
      val values: List[CuHierarchicalEnum] =
        List(CuHierarchicalEnum.A, CuHierarchicalEnum.B, CuHierarchicalEnum.C, CuHierarchicalEnum.D)
      CirceOracle.compare(
        "HierarchicalEnum",
        CiSamples.pair(
          KindlingsCodecAsObject.deriveEnum[CuHierarchicalEnum](),
          ConfiguredEnumCodec.derived[CuHierarchicalEnum]
        ),
        values,
        List("\"NestedA\"", "\"E\"")
      )
    }
    check(
      compare("default", identity, CoreConfiguration.default) ++
        compare("snake", kRenaming.snakeCase, CoreConfiguration.default.withSnakeCaseConstructorNames) ++
        compare(
          "screaming",
          kRenaming.screamingSnakeCase,
          CoreConfiguration.default.withScreamingSnakeCaseConstructorNames
        ) ++
        compare("kebab", kRenaming.kebabCase, CoreConfiguration.default.withKebabCaseConstructorNames) ++
        compare("pascal", kRenaming.pascalCase, CoreConfiguration.default.withPascalCaseConstructorNames) ++
        hierarchical
    )
  }
}
