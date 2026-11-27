package hearth.kindlings.oracle.tapirparity

import hearth.MacroSuite
import hearth.kindlings.circederivation.Configuration as CirceConfig
import hearth.kindlings.jsoniterderivation.JsoniterConfig
import hearth.kindlings.oracle.{Oracle, TapirSchemaDescription}
import hearth.kindlings.tapirschemaderivation.{KindlingsSchema, PreferSchemaConfig}
import sttp.tapir.{Schema, Validator}
import sttp.tapir.generic.Configuration as TapirConfig

/** Kindlings' tapir schemas vs tapir's own derivation (`generic.auto` + `implicitly`, with the equivalent tapir
  * `Configuration`), compared as everything a `Schema` contains ([[TapirSchemaDescription]]: names with type
  * parameters, `FieldName`s, attributes, `hidden`, defaults, examples, validators), plus field getters, subtype
  * selection and `applyValidation` on sample values. Cases mirror tapir's own tests (v1.13.33,
  * `core/src/test/scala/sttp/tapir/generic/SchemaGenericAutoTest.scala` and the others cited per test).
  *
  * Each comparison lists the *exact* differences it expects, each a documented difference (see
  * `docs/user-guide/feature-parity.md`, section tapir-schema-derivation). A difference that appears or disappears
  * fails.
  */
final class TapirParityOracleSpec extends MacroSuite {

  // The subtypes are compared sorted by name: their order is a documented difference, checked by
  // `subtypes are in declaration order` below.
  private def differences(kindlings: Schema[?], upstream: Schema[?]): List[String] =
    Oracle.diff(
      TapirSchemaDescription.describe(kindlings, sortSubtypes = true),
      TapirSchemaDescription.describe(upstream, sortSubtypes = true)
    )

  private def compare(kindlings: Schema[?], upstream: Schema[?], expectedDifferences: String*): Unit = {
    val found = differences(kindlings, upstream)
    assertEquals(found, expectedDifferences.toList, s"Kindlings vs tapir Schema differences:\n${found.mkString("\n")}")
  }

  private def sameOn[T, R](kindlings: Schema[T], upstream: Schema[T], samples: T*)(f: (Schema[T], T) => R): Unit =
    samples.foreach { sample =>
      assertEquals(f(kindlings, sample), f(upstream, sample), s"for $sample")
    }

  // Documented difference ("Required and optional fields"): a field is optional exactly when the decoder derived with
  // the same config accepts it missing; circe's decoders reject a missing collection, tapir marks collections optional.
  private def requiredUnderCirce(path: String): String = s"$path.isOptional: kindlings=false upstream=true"

  // ---------------------------------------------------------------------------------------------------------------
  // Configurations: Kindlings reads the JSON library's config, tapir its own `Configuration`.

  private object CirceDefault {
    implicit val circe: CirceConfig = CirceConfig.default
    def a: Schema[PtA] = KindlingsSchema.derived[PtA].schema
    def b: Schema[PtB] = KindlingsSchema.derived[PtB].schema
    def c: Schema[PtC] = KindlingsSchema.derived[PtC].schema
    def listOfA: Schema[List[PtA]] = KindlingsSchema.derived[List[PtA]].schema
    def hOfA: Schema[PtH[PtA]] = KindlingsSchema.derived[PtH[PtA]].schema
    def mapOfInt: Schema[Map[String, Int]] = KindlingsSchema.derived[Map[String, Int]].schema
    def mapOfD: Schema[Map[String, PtD]] = KindlingsSchema.derived[Map[String, PtD]].schema
    def mapOfHD: Schema[Map[String, PtH[PtD]]] = KindlingsSchema.derived[Map[String, PtH[PtD]]].schema
    def usesMaps: Schema[PtUsesMaps] = KindlingsSchema.derived[PtUsesMaps].schema
    def i: Schema[PtI] = KindlingsSchema.derived[PtI].schema
    def j: Schema[PtJ] = KindlingsSchema.derived[PtJ].schema
    def m: Schema[PtM] = KindlingsSchema.derived[PtM].schema
    def multiCustomised: Schema[PtMultiCustomised] = KindlingsSchema.derived[PtMultiCustomised].schema
    def f: Schema[PtF] = KindlingsSchema.derived[PtF].schema
    def iOpt: Schema[PtIOpt] = KindlingsSchema.derived[PtIOpt].schema
    def jOpt: Schema[PtJOpt] = KindlingsSchema.derived[PtJOpt].schema
    def iList: Schema[PtIList] = KindlingsSchema.derived[PtIList].schema
    def jList: Schema[PtJList] = KindlingsSchema.derived[PtJList].schema
    def validateEach: Schema[PtValidateEach] = KindlingsSchema.derived[PtValidateEach].schema
    def privateFields: Schema[PtPrivate] = KindlingsSchema.derived[PtPrivate].schema
    def serFamily: Schema[PtSerFamily] = KindlingsSchema.derived[PtSerFamily].schema
    def serPerson: Schema[PtSerPerson] = KindlingsSchema.derived[PtSerPerson].schema
    def usesCountry: Schema[PtUsesCountry] = KindlingsSchema.derived[PtUsesCountry].schema
    def lettersEnumeration: Schema[PtLetters] = KindlingsSchema.derivedEnumeration[PtLetters].schema
    def simpleDog: Schema[PtSimpleDog] = KindlingsSchema.derived[PtSimpleDog].schema
    def usesValueClasses: Schema[PtUsesValueClasses] = KindlingsSchema.derived[PtUsesValueClasses].schema
    def usesAnnotatedTypes: Schema[PtUsesAnnotatedTypes] = KindlingsSchema.derived[PtUsesAnnotatedTypes].schema
    def builtIns: Schema[PtBuiltIns] = KindlingsSchema.derived[PtBuiltIns].schema
    def optionOfString: Schema[Option[String]] = KindlingsSchema.derived[Option[String]].schema
    def listOfString: Schema[List[String]] = KindlingsSchema.derived[List[String]].schema
    def setOfString: Schema[Set[String]] = KindlingsSchema.derived[Set[String]].schema
    def arrayOfString: Schema[Array[String]] = KindlingsSchema.derived[Array[String]].schema
    object withCustom {
      implicit val customSchema: Schema[PtCustom] = Schema.string[PtCustom]
      def g: Schema[PtG] = KindlingsSchema.derived[PtG].schema
    }
  }

  private object UpstreamDefault {
    implicit val tapir: TapirConfig = TapirConfig.default
    import sttp.tapir.generic.auto.*
    import TpVersion.Recursive.*
    def a: Schema[PtA] = implicitly[Schema[PtA]]
    def b: Schema[PtB] = implicitly[Schema[PtB]]
    def c: Schema[PtC] = implicitly[Schema[PtC]]
    def listOfA: Schema[List[PtA]] = implicitly[Schema[List[PtA]]]
    def hOfA: Schema[PtH[PtA]] = implicitly[Schema[PtH[PtA]]]
    def mapOfInt: Schema[Map[String, Int]] = implicitly[Schema[Map[String, Int]]]
    def mapOfD: Schema[Map[String, PtD]] = implicitly[Schema[Map[String, PtD]]]
    def mapOfHD: Schema[Map[String, PtH[PtD]]] = implicitly[Schema[Map[String, PtH[PtD]]]]
    def usesMaps: Schema[PtUsesMaps] = implicitly[Schema[PtUsesMaps]]
    def i: Schema[PtI] = implicitly[Schema[PtI]]
    def j: Schema[PtJ] = implicitly[Schema[PtJ]]
    def m: Schema[PtM] = implicitly[Schema[PtM]]
    def multiCustomised: Schema[PtMultiCustomised] = implicitly[Schema[PtMultiCustomised]]
    def f: Schema[PtF] = implicitly[Schema[PtF]]
    def iOpt: Schema[PtIOpt] = implicitly[Schema[PtIOpt]]
    def jOpt: Schema[PtJOpt] = implicitly[Schema[PtJOpt]]
    def iList: Schema[PtIList] = implicitly[Schema[PtIList]]
    def jList: Schema[PtJList] = implicitly[Schema[PtJList]]
    def validateEach: Schema[PtValidateEach] = implicitly[Schema[PtValidateEach]]
    def privateFields: Schema[PtPrivate] = implicitly[Schema[PtPrivate]]
    def serFamily: Schema[PtSerFamily] = implicitly[Schema[PtSerFamily]]
    def serPerson: Schema[PtSerPerson] = implicitly[Schema[PtSerPerson]]
    def usesCountry: Schema[PtUsesCountry] = implicitly[Schema[PtUsesCountry]]
    def lettersEnumeration: Schema[PtLetters] = Schema.derivedEnumeration[PtLetters].defaultStringBased
    def simpleDog: Schema[PtSimpleDog] = implicitly[Schema[PtSimpleDog]]
    def usesAnnotatedTypes: Schema[PtUsesAnnotatedTypes] = implicitly[Schema[PtUsesAnnotatedTypes]]
    def builtIns: Schema[PtBuiltIns] = implicitly[Schema[PtBuiltIns]]
    def optionOfString: Schema[Option[String]] = implicitly[Schema[Option[String]]]
    def listOfString: Schema[List[String]] = implicitly[Schema[List[String]]]
    def setOfString: Schema[Set[String]] = implicitly[Schema[Set[String]]]
    def arrayOfString: Schema[Array[String]] = implicitly[Schema[Array[String]]]
    object withCustom {
      implicit val customSchema: Schema[PtCustom] = Schema.string[PtCustom]
      def g: Schema[PtG] = implicitly[Schema[PtG]]
    }
  }

  /** Member-name transforms: circe's `transformMemberNames` vs tapir's `toEncodedName`. */
  private object MemberNames {
    def snake: Schema[PtD] = {
      implicit val circe: CirceConfig = CirceConfig.default.withSnakeCaseMemberNames
      KindlingsSchema.derived[PtD].schema
    }
    def screamingSnake: Schema[PtD] = {
      implicit val circe: CirceConfig = CirceConfig.default.withScreamingSnakeCaseMemberNames
      KindlingsSchema.derived[PtD].schema
    }
    def kebab: Schema[PtD] = {
      implicit val circe: CirceConfig = CirceConfig.default.withKebabCaseMemberNames
      KindlingsSchema.derived[PtD].schema
    }
    def encodedNameNotTransformed: Schema[PtL] = {
      implicit val circe: CirceConfig = CirceConfig.default.withSnakeCaseMemberNames
      KindlingsSchema.derived[PtL].schema
    }
  }

  private object MemberNamesUpstream {
    import sttp.tapir.generic.auto.*
    def snake: Schema[PtD] = {
      implicit val tapir: TapirConfig = TapirConfig.default.withSnakeCaseMemberNames
      implicitly[Schema[PtD]]
    }
    def screamingSnake: Schema[PtD] = {
      implicit val tapir: TapirConfig = TapirConfig.default.withScreamingSnakeCaseMemberNames
      implicitly[Schema[PtD]]
    }
    def kebab: Schema[PtD] = {
      implicit val tapir: TapirConfig = TapirConfig.default.withKebabCaseMemberNames
      implicitly[Schema[PtD]]
    }
    def encodedNameNotTransformed: Schema[PtL] = {
      implicit val tapir: TapirConfig = TapirConfig.default.withSnakeCaseMemberNames
      implicitly[Schema[PtL]]
    }
  }

  // tapir's case transformations (sttp.tapir.generic.Configuration), for jsoniter's `adtLeafClassNameMapper`, which
  // receives the fully-qualified name of the leaf
  private def tapirCase(separator: String)(s: String): String =
    s.replaceAll("([A-Z]+)([A-Z][a-z])", "$1" + separator + "$2")
      .replaceAll("([a-z\\d])([A-Z])", "$1" + separator + "$2")

  /** Discriminator values: circe's `transformConstructorNames` (short names) and jsoniter's `adtLeafClassNameMapper`
    * (full names) vs tapir's `toDiscriminatorValue`.
    */
  private object Discriminators {
    def short: Schema[PtEntity] = {
      implicit val circe: CirceConfig = CirceConfig.default.withDiscriminator("who_am_i")
      KindlingsSchema.derived[PtEntity].schema
    }
    def kebab: Schema[PtEntity] = {
      implicit val circe: CirceConfig = CirceConfig.default.withDiscriminator("who_am_i").withKebabCaseConstructorNames
      KindlingsSchema.derived[PtEntity].schema
    }
    def snake: Schema[PtEntity] = {
      implicit val circe: CirceConfig = CirceConfig.default.withDiscriminator("who_am_i").withSnakeCaseConstructorNames
      KindlingsSchema.derived[PtEntity].schema
    }
    def screamingSnake: Schema[PtEntity] = {
      implicit val circe: CirceConfig =
        CirceConfig.default.withDiscriminator("who_am_i").withScreamingSnakeCaseConstructorNames
      KindlingsSchema.derived[PtEntity].schema
    }
    def pet: Schema[PtPet] = {
      implicit val circe: CirceConfig = CirceConfig.default.withDiscriminator("type")
      KindlingsSchema.derived[PtPet].schema
    }
    def hericium: Schema[PtHericium] = {
      implicit val circe: CirceConfig = CirceConfig.default.withDiscriminator("type")
      KindlingsSchema.derived[PtHericium].schema
    }
    def erinaceus: Schema[PtHericium.Erinaceus] = {
      implicit val circe: CirceConfig = CirceConfig.default.withDiscriminator("type")
      KindlingsSchema.derived[PtHericium.Erinaceus].schema
    }
    def botryoides: Schema[PtHericium.Botryoides] = {
      implicit val circe: CirceConfig = CirceConfig.default.withDiscriminator("type")
      KindlingsSchema.derived[PtHericium.Botryoides].schema
    }
    def node: Schema[PtNode] = {
      implicit val circe: CirceConfig = CirceConfig.default.withDiscriminator("type")
      KindlingsSchema.derived[PtNode].schema
    }
    def annotatedShape: Schema[PtAnnotatedShape] = {
      implicit val circe: CirceConfig = CirceConfig.default.withDiscriminator("type")
      KindlingsSchema.derived[PtAnnotatedShape].schema
    }
  }

  /** jsoniter's codecs write `scala.Enumeration` values as strings (circe-derivation as the objects of a sealed
    * hierarchy, which the schema then describes).
    */
  private object JsoniterEnums {
    implicit val preferJsoniter: PreferSchemaConfig[JsoniterConfig] = PreferSchemaConfig[JsoniterConfig]
    implicit val jsoniter: JsoniterConfig = JsoniterConfig.default.withDiscriminator("type")
    def usesEnums: Schema[PtUsesEnums] = KindlingsSchema.derived[PtUsesEnums].schema
    def usesCountry: Schema[PtUsesCountry] = KindlingsSchema.derived[PtUsesCountry].schema
  }

  /** Full-name discriminator values: jsoniter's `adtLeafClassNameMapper` receives the fully-qualified leaf name. */
  private object FullNameDiscriminators {
    implicit val preferJsoniter: PreferSchemaConfig[JsoniterConfig] = PreferSchemaConfig[JsoniterConfig]
    def full: Schema[PtEntity] = {
      implicit val jsoniter: JsoniterConfig =
        JsoniterConfig.default.withDiscriminator("who_am_i").withAdtLeafClassNameMapper((n: String) => n)
      KindlingsSchema.derived[PtEntity].schema
    }
    def fullKebab: Schema[PtEntity] = {
      implicit val jsoniter: JsoniterConfig = JsoniterConfig.default
        .withDiscriminator("who_am_i")
        .withAdtLeafClassNameMapper((n: String) => tapirCase("-")(n).toLowerCase)
      KindlingsSchema.derived[PtEntity].schema
    }
    def fullSnake: Schema[PtEntity] = {
      implicit val jsoniter: JsoniterConfig = JsoniterConfig.default
        .withDiscriminator("who_am_i")
        .withAdtLeafClassNameMapper((n: String) => tapirCase("_")(n).toLowerCase)
      KindlingsSchema.derived[PtEntity].schema
    }
  }

  private object DiscriminatorsUpstream {
    import sttp.tapir.generic.auto.*
    private val base = TapirConfig.default.withDiscriminator("who_am_i")
    def short: Schema[PtEntity] = {
      implicit val tapir: TapirConfig = base
      implicitly[Schema[PtEntity]]
    }
    def kebab: Schema[PtEntity] = {
      implicit val tapir: TapirConfig = base.withKebabCaseDiscriminatorValues
      implicitly[Schema[PtEntity]]
    }
    def snake: Schema[PtEntity] = {
      implicit val tapir: TapirConfig = base.withSnakeCaseDiscriminatorValues
      implicitly[Schema[PtEntity]]
    }
    def screamingSnake: Schema[PtEntity] = {
      implicit val tapir: TapirConfig = base.withScreamingSnakeCaseDiscriminatorValues
      implicitly[Schema[PtEntity]]
    }
    def full: Schema[PtEntity] = {
      implicit val tapir: TapirConfig = base.withFullDiscriminatorValues
      implicitly[Schema[PtEntity]]
    }
    def fullKebab: Schema[PtEntity] = {
      implicit val tapir: TapirConfig = base.withFullKebabCaseDiscriminatorValues
      implicitly[Schema[PtEntity]]
    }
    def fullSnake: Schema[PtEntity] = {
      implicit val tapir: TapirConfig = base.withFullSnakeCaseDiscriminatorValues
      implicitly[Schema[PtEntity]]
    }
    def pet: Schema[PtPet] = {
      implicit val tapir: TapirConfig = TapirConfig.default.withDiscriminator("type")
      implicitly[Schema[PtPet]]
    }
    def hericium: Schema[PtHericium] = {
      implicit val tapir: TapirConfig = TapirConfig.default.withDiscriminator("type")
      Schema.derived[PtHericium]
    }
    def erinaceus: Schema[PtHericium.Erinaceus] = {
      implicit val tapir: TapirConfig = TapirConfig.default.withDiscriminator("type")
      Schema.derived[PtHericium.Erinaceus]
    }
    def botryoides: Schema[PtHericium.Botryoides] = {
      implicit val tapir: TapirConfig = TapirConfig.default.withDiscriminator("type")
      Schema.derived[PtHericium.Botryoides]
    }
    def node: Schema[PtNode] = {
      import TpVersion.RecursiveWithDiscriminator.nodeSchema
      implicitly[Schema[PtNode]]
    }
    def usesEnums: Schema[PtUsesEnums] = {
      implicit val tapir: TapirConfig = TapirConfig.default.withDiscriminator("type")
      implicitly[Schema[PtUsesEnums]]
    }
    def annotatedShape: Schema[PtAnnotatedShape] = {
      implicit val tapir: TapirConfig = TapirConfig.default.withDiscriminator("type")
      implicitly[Schema[PtAnnotatedShape]]
    }
  }

  /** Schemas the user builds with tapir's builders, in implicit scope of both derivations. */
  private object Provided {
    import sttp.tapir.generic.auto.*
    implicit val entitySchema: Schema[PtEntity] = Schema.oneOfWrapped[PtEntity]
    implicit val kindedSchema: Schema[PtKinded] =
      Schema.oneOfUsingField[PtKinded, String](_.kind, _.toString)(
        "user" -> Schema.derived[PtUser],
        "org" -> Schema.derived[PtOrg]
      )
    implicit val lettersSchema: Schema[PtLetters] = Schema.derivedEnumeration[PtLetters].defaultStringBased
    implicit val customSchema: Schema[PtCustom] = Schema.string[PtCustom].description("custom")
  }

  private object ProvidedKindlings {
    implicit val circe: CirceConfig = CirceConfig.default
    import Provided.*
    def usesProvided: Schema[PtUsesProvided] = KindlingsSchema.derived[PtUsesProvided].schema
  }

  private object ProvidedUpstream {
    implicit val tapir: TapirConfig = TapirConfig.default
    import sttp.tapir.generic.auto.*
    import Provided.*
    def usesProvided: Schema[PtUsesProvided] = implicitly[Schema[PtUsesProvided]]
  }

  // ---------------------------------------------------------------------------------------------------------------

  group("tapir parity (SchemaGenericAutoTest): products") {
    val k = CirceDefault
    val u = UpstreamDefault

    test("simple case class") {
      compare(k.a, u.a)
    }

    test("nested case class") {
      compare(k.b, u.b)
    }

    test("case class with collections") {
      compare(k.c, u.c, requiredUnderCirce("$.fields[0].schema"))
    }

    test("collection of case classes") {
      compare(k.listOfA, u.listOfA)
    }

    test("custom schema for a custom type") {
      compare(k.withCustom.g, u.withCustom.g)
    }

    test("parametrised type: SName with the type parameters") {
      compare(k.hOfA, u.hOfA)
      assertEquals(k.hOfA.name, Some(Schema.SName(classOf[PtH[?]].getName, List(classOf[PtA].getName))))
    }

    test("maps (of products, of generic products)") {
      compare(k.mapOfInt, u.mapOfInt)
      compare(k.mapOfD, u.mapOfD)
      compare(k.mapOfHD, u.mapOfHD)
      compare(k.usesMaps, u.usesMaps)
    }

    test("field getters read every constructor field, private ones too") {
      compare(k.privateFields, u.privateFields)
      sameOn(k.privateFields, u.privateFields, PtPrivate(1, "two", c = true))(TapirSchemaDescription.fieldValues)
    }
  }

  group("tapir parity (SchemaGenericAutoTest): built-in types") {
    val k = CirceDefault
    val u = UpstreamDefault

    test("simple types, optional types and collections (as fields)") {
      compare(
        k.builtIns,
        u.builtIns,
        requiredUnderCirce("$.fields[8].schema"),
        requiredUnderCirce("$.fields[9].schema"),
        requiredUnderCirce("$.fields[10].schema"),
        requiredUnderCirce("$.fields[11].schema")
      )
    }

    test("optional types and collections (on their own)") {
      compare(k.optionOfString, u.optionOfString)
      compare(k.listOfString, u.listOfString)
      compare(k.setOfString, u.setOfString)
      compare(k.arrayOfString, u.arrayOfString)
    }
  }

  group("tapir parity (SchemaAnnotationsTest): annotations of types") {
    val k = CirceDefault
    val u = UpstreamDefault

    test("metadata annotations on a type, @customise applied after them") {
      compare(k.usesAnnotatedTypes, u.usesAnnotatedTypes)
      assertEquals(
        k.usesAnnotatedTypes.schemaType match {
          case p: sttp.tapir.SchemaType.SProduct[?] => p.fields.map(_.schema.format)
          case _                                    => Nil
        },
        List(Some("utf8"), Some("c1-c2"))
      )
    }
  }

  group("tapir parity (SchemaGenericAutoTest): annotations") {
    val k = CirceDefault
    val u = UpstreamDefault

    test("metadata from annotations (I, K)") {
      compare(k.i, u.i)
    }

    test("@customise on the class") {
      compare(k.m, u.m)
      assertEquals(k.m.attribute(PtM.testAttributeKey), Some("test"))
    }

    test("several @customise applied in declaration order") {
      compare(k.multiCustomised, u.multiCustomised)
      assertEquals(k.multiCustomised.format, Some("c1-c2"))
    }

    test("@title, @hidden, @deprecated, @encodedExample, @validate and @default (on a type and its fields)") {
      compare(
        k.j,
        u.j,
        // Documented difference: tapir's derivation ignores `@hidden` on fields (it only reads it for enumerations and
        // endpoint inputs), Kindlings hides the field (tapir's docs interpreter then leaves it out)
        "$.fields[1].schema.hidden: kindlings=true upstream=false"
      )
    }
  }

  group("tapir parity (SchemaGenericAutoTest): configuration") {

    test("snake_case member names") {
      compare(MemberNames.snake, MemberNamesUpstream.snake)
    }

    test("SCREAMING_SNAKE_CASE member names") {
      compare(MemberNames.screamingSnake, MemberNamesUpstream.screamingSnake)
    }

    test("kebab-case member names") {
      compare(MemberNames.kebab, MemberNamesUpstream.kebab)
    }

    test("@encodedName is not transformed") {
      compare(MemberNames.encodedNameNotTransformed, MemberNamesUpstream.encodedNameNotTransformed)
    }

    test("discriminator (case object child included)") {
      compare(Discriminators.short, DiscriminatorsUpstream.short)
      sameOn(
        Discriminators.short,
        DiscriminatorsUpstream.short,
        PtPerson("a", 1),
        PtOrganization("o"),
        PtUnknownEntity
      )(
        TapirSchemaDescription.subtypeOf
      )
    }

    test("discriminator values: kebab-case") {
      compare(Discriminators.kebab, DiscriminatorsUpstream.kebab)
    }

    test("discriminator values: snake_case") {
      compare(Discriminators.snake, DiscriminatorsUpstream.snake)
    }

    test("discriminator values: SCREAMING_SNAKE_CASE") {
      compare(Discriminators.screamingSnake, DiscriminatorsUpstream.screamingSnake)
    }

    test("discriminator values: full names (jsoniter adtLeafClassNameMapper)") {
      compare(FullNameDiscriminators.full, DiscriminatorsUpstream.full)
    }

    test("discriminator values: full kebab-case names (jsoniter adtLeafClassNameMapper)") {
      compare(FullNameDiscriminators.fullKebab, DiscriminatorsUpstream.fullKebab)
    }

    test("discriminator values: full snake_case names (jsoniter adtLeafClassNameMapper)") {
      compare(FullNameDiscriminators.fullSnake, DiscriminatorsUpstream.fullSnake)
    }

    test("subtypes inherit annotations of the parent's members (Pet, Cat, Dog, Hamster)") {
      compare(Discriminators.pet, DiscriminatorsUpstream.pet)
      sameOn(Discriminators.pet, DiscriminatorsUpstream.pet, PtCat("c", "f"), PtDog("d", "f"), PtHamster("h", true))(
        TapirSchemaDescription.subtypeOf
      )
    }

    test("subtypes inherit annotations of the parent type") {
      compare(
        Discriminators.annotatedShape,
        DiscriminatorsUpstream.annotatedShape,
        // Documented difference: tapir's derivation does not apply the annotations of a sealed trait to its own
        // (coproduct) schema, only to the subtypes; Kindlings applies them to both
        "$.description: kindlings=Some(a shape) upstream=None",
        "$.format: kindlings=Some(shape) upstream=None"
      )
    }

    // Documented difference: Kindlings lists subtypes in declaration order on Scala 3 and by name on Scala 2
    // (StableChildrenOrder: Scala 2 class files keep no declaration order), tapir sorts them by name.
    test("subtypes are in declaration order on Scala 3 (tapir: sorted by name)") {
      val kindlings = TapirSchemaDescription.subtypeNames(Discriminators.short).flatten.map(_.split('.').last)
      val upstream = TapirSchemaDescription.subtypeNames(DiscriminatorsUpstream.short).flatten.map(_.split('.').last)
      assertEquals(
        kindlings,
        if (TpVersion.isScala3) List("PtPerson", "PtOrganization", "PtUnknownEntity")
        else List("PtOrganization", "PtPerson", "PtUnknownEntity")
      )
      assertEquals(upstream, List("PtOrganization", "PtPerson", "PtUnknownEntity"))
    }

    test("@encodedName on types names the schema, on a parent's member renames the field (Hericium)") {
      compare(
        Discriminators.hericium,
        DiscriminatorsUpstream.hericium,
        // Documented difference: the discriminator value is the one the JSON codec writes (circe: the constructor
        // name), tapir derives it from the schema name, i.e. from `@encodedName`
        "$.subtypes[0].fields[2].schema.attributes[sttp.tapir.Schema.EncodedDiscriminatorValue]: " +
          "kindlings=EncodedDiscriminatorValue(Erinaceus) upstream=EncodedDiscriminatorValue(CustomErinaceus)",
        "$.discriminator.value: kindlings=type/type -> Abietis=hearth.kindlings.oracle.tapirparity.PtHericium.Abietis, " +
          "Botryoides=hearth.kindlings.oracle.tapirparity.PtHericium.Botryoides, Erinaceus=CustomErinaceus " +
          "upstream=type/type -> Abietis=hearth.kindlings.oracle.tapirparity.PtHericium.Abietis, " +
          "Botryoides=hearth.kindlings.oracle.tapirparity.PtHericium.Botryoides, CustomErinaceus=CustomErinaceus"
      )
      compare(Discriminators.erinaceus, DiscriminatorsUpstream.erinaceus)
      compare(Discriminators.botryoides, DiscriminatorsUpstream.botryoides)
      sameOn(
        Discriminators.hericium,
        DiscriminatorsUpstream.hericium,
        PtHericium.Erinaceus(1, "e"),
        PtHericium.Abietis(1, 2),
        PtHericium.Botryoides(1, b = true)
      )(TapirSchemaDescription.subtypeOf)
    }

    test("enumerations and sealed traits as fields") {
      compare(JsoniterEnums.usesEnums, DiscriminatorsUpstream.usesEnums)
    }
  }

  group("tapir parity (LegacySchemaGenericAutoTest): recursive types") {
    val k = CirceDefault
    val u = UpstreamDefault

    test("recursive product") {
      compare(k.f, u.f, requiredUnderCirce("$.fields[0].schema"))
    }

    test("recursive coproduct") {
      compare(Discriminators.node, DiscriminatorsUpstream.node)
    }

    test("recursive schemas wrapped with an Option") {
      compare(k.iOpt, u.iOpt)
      compare(k.jOpt, u.jOpt)
    }

    test("recursive schemas wrapped with a collection") {
      compare(k.iList, u.iList, requiredUnderCirce("$.fields[0].schema"))
      compare(
        k.jList,
        u.jList,
        requiredUnderCirce("$.fields[0].schema"),
        requiredUnderCirce("$.fields[0].schema.element.value.fields[0].schema")
      )
    }
  }

  group("tapir parity (SchemaMacroTest, EnumsCodecDerivationTest): enumerations") {
    val k = CirceDefault
    val u = UpstreamDefault

    test("Schema.derivedEnumeration of an annotated sealed trait of case objects") {
      compare(k.lettersEnumeration, u.lettersEnumeration)
    }

    test("scala.Enumeration with annotations on the object") {
      compare(JsoniterEnums.usesCountry, u.usesCountry)
    }
  }

  group("tapir parity: schemas the user provides with tapir's builders") {

    test("Schema.oneOfWrapped, Schema.oneOfUsingField, Schema.derivedEnumeration and Schema.string are picked up") {
      compare(ProvidedKindlings.usesProvided, ProvidedUpstream.usesProvided)
    }
  }

  group("tapir parity (SchemaApplyValidationTest, SchemaGenericAutoTest): validation") {
    val k = CirceDefault
    val u = UpstreamDefault

    test("@validateEach on collection and Option elements") {
      compare(k.validateEach, u.validateEach, requiredUnderCirce("$.fields[0].schema"))
      sameOn(
        k.validateEach,
        u.validateEach,
        PtValidateEach(Nil, None),
        PtValidateEach(List(6, 10), Some("1234")),
        PtValidateEach(List(6, 0, 10), Some("1234")),
        PtValidateEach(List(6, 10), Some("12"))
      )(TapirSchemaDescription.validationErrors)
    }

    test("validators of implicit field schemas") {
      implicit val nameSchema: Schema[String] = Schema.schemaForString.validate(Validator.pattern("^[A-Z].*"))
      val kindlings = {
        implicit val circe: CirceConfig = CirceConfig.default
        KindlingsSchema.derived[PtSimpleDog].schema
      }
      val upstream = Schema.derived[PtSimpleDog]
      compare(kindlings, upstream)
      sameOn(kindlings, upstream, PtSimpleDog("rex"), PtSimpleDog("Rex"))(TapirSchemaDescription.validationErrors)
    }

    test("recursive values") {
      implicit val stringSchema: Schema[String] = Schema.schemaForString.validate(Validator.minLength(1))
      val kindlings = {
        implicit val circe: CirceConfig = CirceConfig.default
        KindlingsSchema.derived[PtRecursiveName].schema
      }
      implicit lazy val upstream: Schema[PtRecursiveName] = Schema.derived[PtRecursiveName]
      compare(kindlings, upstream)
      sameOn(
        kindlings,
        upstream,
        PtRecursiveName("x", None),
        PtRecursiveName("", None),
        PtRecursiveName("x", Some(Vector(PtRecursiveName("", None)))),
        PtRecursiveName("x", Some(Vector(PtRecursiveName("x", Some(Vector(PtRecursiveName("", None)))))))
      )(TapirSchemaDescription.validationErrors)
      assertEquals(kindlings.showValidators, upstream.showValidators)
    }

    // SchemaApplyValidationTest "validate either": Kindlings describes `Either` as the JSON codec writes it (circe:
    // `{"Left": ...}` / `{"Right": ...}`), tapir as `oneOf` the two schemas; validation reaches the same element.
    test("Either values validate their side") {
      implicit val intSchema: Schema[Int] = Schema.schemaForInt.validate(Validator.min(1))
      implicit val stringSchema: Schema[String] = Schema.schemaForString.validate(Validator.minLength(1))
      val kindlings = {
        implicit val circe: CirceConfig = CirceConfig.default
        KindlingsSchema.derived[Either[Int, String]].schema
      }
      val upstream = Schema.schemaForEither(intSchema, stringSchema)
      // the path of the error includes the wrapper field (`Left.value`) only on Kindlings' side
      sameOn(kindlings, upstream, Left(10), Right("x"), Left(0), Right(""))((schema, value) =>
        schema.applyValidation(value).map(error => (error.validator, error.invalidValue))
      )
      assertEquals(TapirSchemaDescription.validationErrors(kindlings, Left(0)), List("Left.value: Min(1,false) on 0"))
    }

    test("modified schemas (Schema.modify) validate like upstream") {
      val kindlings = k.simpleDog.modify(_.name)(_.validate(Validator.minLength(3)))
      val upstream = u.simpleDog.modify(_.name)(_.validate(Validator.minLength(3)))
      sameOn(kindlings, upstream, PtSimpleDog("a"), PtSimpleDog("abc"))(TapirSchemaDescription.validationErrors)
    }
  }

  group("tapir parity (SchemaSerialisationTest): serialisation") {
    val k = CirceDefault
    val u = UpstreamDefault

    def roundTrip[T](schema: Schema[T]): Schema[T] = {
      val bytes = new java.io.ByteArrayOutputStream()
      val out = new java.io.ObjectOutputStream(bytes)
      out.writeObject(schema)
      out.close()
      val in = new java.io.ObjectInputStream(new java.io.ByteArrayInputStream(bytes.toByteArray)) {
        override def resolveClass(desc: java.io.ObjectStreamClass): Class[?] =
          try Class.forName(desc.getName, false, getClass.getClassLoader)
          catch { case _: ClassNotFoundException => super.resolveClass(desc) }
      }
      in.readObject().asInstanceOf[Schema[T]]
    }

    test("derived schemas serialise and deserialise, and validate the same afterwards") {
      compare(k.serFamily, u.serFamily, requiredUnderCirce("$.fields[2].schema"))
      assertEquals(roundTrip(k.serFamily), k.serFamily)
      assertEquals(roundTrip(k.serPerson), k.serPerson)
      val p = PtSerPerson("x", Some(10))
      assertEquals(roundTrip(k.serPerson).applyValidation(p), k.serPerson.applyValidation(p))
      sameOn(k.serPerson, u.serPerson, p, PtSerPerson("xyz", Some(20)))(TapirSchemaDescription.validationErrors)
    }
  }

  group("tapir parity (SchemaMacroTest2, LegacySchemaGenericAutoTest): value classes") {
    val k = CirceDefault

    // tapir derives them on Scala 2 only; on Scala 3 its users write `Schema.schemaForX.as[V]` (TpValueClasses)
    test("value classes are their underlying type's schema") {
      compare(
        k.usesValueClasses,
        TpValueClasses.upstream,
        requiredUnderCirce("$.fields[3].schema")
      )
    }
  }
}
