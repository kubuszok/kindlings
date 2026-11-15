package hearth.kindlings.oracle

import hearth.MacroSuite
import hearth.kindlings.circederivation.Configuration as CirceConfig
import hearth.kindlings.tapirschemaderivation.KindlingsSchema
import sttp.tapir.Schema
import sttp.tapir.generic.Configuration as TapirConfig

/** Kindlings' tapir schemas (derived with a circe `Configuration`) vs tapir's own derivation (`generic.auto` with the
  * equivalent tapir `Configuration`), compared as the JSON Schema tapir's docs interpreter renders for them.
  *
  * Each case lists the *exact* differences it expects; each one must be a documented, intentional difference (see
  * `docs/user-guide/feature-parity.md`). A difference that appears or disappears fails the test.
  */
final class TapirSchemaOracleSpec extends MacroSuite {

  // Documented difference ("Required and optional fields" in the tapir guide): a field is optional exactly when the
  // decoder derived with the same config accepts it missing. circe's decoders reject a missing collection, tapir
  // marks every collection optional.
  private def collectionsRequiredUnderCirce(at: String, kindlings: String, upstream: String): String =
    s"$at.required: kindlings=List($kindlings) upstream=List($upstream)"

  private def compare(kindlings: Schema[?], upstream: Schema[?], expectedDifferences: String*): Unit = {
    val differences = Oracle.diff(Oracle.jsonSchema(kindlings), Oracle.jsonSchema(upstream))
    assertEquals(
      differences,
      expectedDifferences.toList,
      s"Kindlings vs tapir JSON Schema differences:\n${differences.mkString("\n")}"
    )
  }

  private object Default {
    implicit val circe: CirceConfig = CirceConfig.default
    object kindlings {
      def person: Schema[OPerson] = KindlingsSchema.derived[OPerson].schema
      def primitives: Schema[OPrimitives] = KindlingsSchema.derived[OPrimitives].schema
      def collections: Schema[OCollections] = KindlingsSchema.derived[OCollections].schema
      def defaults: Schema[ODefaults] = KindlingsSchema.derived[ODefaults].schema
      def usesWrapper: Schema[OUsesWrapper] = KindlingsSchema.derived[OUsesWrapper].schema
      def tree: Schema[OTree] = KindlingsSchema.derived[OTree].schema
      def annotated: Schema[OAnnotated] = KindlingsSchema.derived[OAnnotated].schema
      def usesBox: Schema[OUsesBox] = KindlingsSchema.derived[OUsesBox].schema
    }
  }

  private object DefaultUpstream {
    implicit val tapir: TapirConfig = TapirConfig.default
    import sttp.tapir.generic.auto.*
    // tapir's Scala 3 derivation has no value-class support; this is the schema tapir users write by hand.
    implicit val wrapperSchema: Schema[OWrapper] = Schema.schemaForString.as[OWrapper]
    // Recursive schemas are declared explicitly, as tapir's docs say (auto-derivation does not recurse into itself).
    import TapirUpstreamRecursive.treeSchema
    object upstream {
      def person: Schema[OPerson] = implicitly[Schema[OPerson]]
      def primitives: Schema[OPrimitives] = implicitly[Schema[OPrimitives]]
      def collections: Schema[OCollections] = implicitly[Schema[OCollections]]
      def defaults: Schema[ODefaults] = implicitly[Schema[ODefaults]]
      def usesWrapper: Schema[OUsesWrapper] = implicitly[Schema[OUsesWrapper]]
      def tree: Schema[OTree] = implicitly[Schema[OTree]]
      def annotated: Schema[OAnnotated] = implicitly[Schema[OAnnotated]]
      def usesBox: Schema[OUsesBox] = implicitly[Schema[OUsesBox]]
    }
  }

  private object Discriminator {
    implicit val circe: CirceConfig = CirceConfig.default.withDiscriminator("type")
    def shape: Schema[OShape] = KindlingsSchema.derived[OShape].schema
    def drawing: Schema[ODrawing] = KindlingsSchema.derived[ODrawing].schema
  }

  private object DiscriminatorUpstream {
    implicit val tapir: TapirConfig = TapirConfig.default.withDiscriminator("type")
    import sttp.tapir.generic.auto.*
    def shape: Schema[OShape] = implicitly[Schema[OShape]]
    def drawing: Schema[ODrawing] = implicitly[Schema[ODrawing]]
  }

  private object SnakeCase {
    implicit val circe: CirceConfig =
      CirceConfig.default.withSnakeCaseMemberNames.withSnakeCaseConstructorNames.withDiscriminator("kind")
    def camelCased: Schema[OCamelCased] = KindlingsSchema.derived[OCamelCased].schema
    def event: Schema[OEvent] = KindlingsSchema.derived[OEvent].schema
  }

  private object SnakeCaseUpstream {
    implicit val tapir: TapirConfig =
      TapirConfig.default.withSnakeCaseMemberNames.withSnakeCaseDiscriminatorValues.withDiscriminator("kind")
    import sttp.tapir.generic.auto.*
    def camelCased: Schema[OCamelCased] = implicitly[Schema[OCamelCased]]
    def event: Schema[OEvent] = implicitly[Schema[OEvent]]
  }

  group("tapir Schema.derived oracle: products") {
    import Default.kindlings
    import DefaultUpstream.upstream

    test("simple case class with an Option field") {
      compare(kindlings.person, upstream.person)
    }

    test("built-in primitive and library types") {
      compare(kindlings.primitives, upstream.primitives)
    }

    test("collections and maps") {
      compare(
        kindlings.collections,
        upstream.collections,
        collectionsRequiredUnderCirce("$", "tags, ids, scores, matrix", "scores")
      )
    }

    test("default values") {
      compare(kindlings.defaults, upstream.defaults)
    }

    test("value classes") {
      compare(kindlings.usesWrapper, upstream.usesWrapper)
    }

    test("recursive types") {
      compare(
        kindlings.tree,
        upstream.tree,
        collectionsRequiredUnderCirce("$.$defs.value[OTree]", "value, children", "value")
      )
    }

    test("tapir annotations") {
      compare(kindlings.annotated, upstream.annotated)
    }

    test("generic case classes") {
      compare(
        kindlings.usesBox,
        upstream.usesBox,
        collectionsRequiredUnderCirce("$.$defs.value[OBox_OPerson]", "value, items", "value"),
        collectionsRequiredUnderCirce("$.$defs.value[OBox_Int]", "value, items", "value")
      )
    }
  }

  group("tapir Schema.derived oracle: configuration") {

    test("sealed trait with a discriminator") {
      compare(Discriminator.shape, DiscriminatorUpstream.shape)
    }

    test("sealed trait with a discriminator nested in a product") {
      compare(
        Discriminator.drawing,
        DiscriminatorUpstream.drawing,
        collectionsRequiredUnderCirce("$", "title, shapes", "title")
      )
    }

    test("snake_case member names") {
      compare(SnakeCase.camelCased, SnakeCaseUpstream.camelCased)
    }

    test("snake_case discriminator values") {
      compare(SnakeCase.event, SnakeCaseUpstream.event)
    }
  }
}
