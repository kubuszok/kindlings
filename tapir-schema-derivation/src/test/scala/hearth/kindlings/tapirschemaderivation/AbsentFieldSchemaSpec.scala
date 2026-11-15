package hearth.kindlings.tapirschemaderivation

import com.github.plokhotnyuk.jsoniter_scala.core.{readFromString, writeToString}
import hearth.MacroSuite
import io.circe.Json
import sttp.tapir.{Schema, SchemaType}

import scala.util.Try

/** A field is optional in the tapir Schema derived with a JSON library's config exactly when the decoder derived with
  * the same config accepts it missing from the JSON object. Checked against the actual decoders: every field of
  * [[AbsAll]] is removed from a complete JSON object in turn, and the decoder must accept the result iff the schema
  * marks the field optional.
  */
final class AbsentFieldSchemaSpec extends MacroSuite {

  private val jsoniterAll: List[AbsJsoniterDerived] = List(
    AbsJsoniterDefault.derived,
    AbsJsoniterRequireDefaults.derived,
    AbsJsoniterRequireCollections.derived,
    AbsJsoniterRequireBoth.derived,
    AbsJsoniterTransient.derived,
    AbsJsoniterRuntime.derived
  )

  private val circeAll: List[AbsCirceDerived] = List(AbsCirceDefault.derived, AbsCirceWithDefaults.derived)

  private def productFields(schema: Schema[?]): List[(String, Boolean)] = schema.schemaType match {
    case p: SchemaType.SProduct[?] => p.fields.map(f => f.name.encodedName -> f.schema.isOptional)
    case other                     => fail(s"Expected SProduct, got: $other")
  }

  private def optionalFields(schema: Schema[?]): Set[String] = productFields(schema).collect { case (n, true) =>
    n
  }.toSet

  private def required(schema: Schema[?]): Set[String] = schema.schemaType match {
    case p: SchemaType.SProduct[?] => p.required.map(_.encodedName).toSet
    case other                     => fail(s"Expected SProduct, got: $other")
  }

  private def parse(json: String): Json =
    io.circe.parser.parse(json).fold(e => fail(s"Invalid JSON $json: $e"), identity)

  private def withoutField(json: Json, field: String): Json =
    json.asObject.fold(fail(s"Expected an object, got $json"))(obj => Json.fromJsonObject(obj.remove(field)))

  /** The fields the decoder accepts missing, found by removing each of them from the complete JSON in turn. */
  private def acceptedMissing(fields: List[String], complete: Json)(decodes: Json => Boolean): Set[String] = {
    assert(decodes(complete), s"The complete JSON ${complete.noSpaces} does not decode")
    fields.filter(f => decodes(withoutField(complete, f))).toSet
  }

  private val allFields = List(
    "plain",
    "text",
    "name",
    "size",
    "tags",
    "counts",
    "vec",
    "set",
    "listWithDefault",
    "opt",
    "optWithDefault",
    "wrappedTags",
    "wrappedTagsWithDefault"
  )

  group("jsoniter: schema optionality matches the decoder") {
    jsoniterAll.foreach { derived =>
      test(s"config: ${derived.name}") {
        assertEquals(productFields(derived.schema).map(_._1), allFields)
        val complete = parse(writeToString(AbsAll.sample)(derived.codec))
        val accepted = acceptedMissing(allFields, complete) { json =>
          Try(readFromString[AbsAll](json.noSpaces)(derived.codec)).isSuccess
        }
        assertEquals(optionalFields(derived.schema), accepted)
        assertEquals(required(derived.schema), allFields.toSet -- accepted)
      }
    }
  }

  group("jsoniter: expected optional fields") {

    val always = Set("opt", "optWithDefault")
    // a value type wrapping a collection is required (decoders do not fill it in), unless it has a default value
    val defaults = Set("name", "size", "listWithDefault", "wrappedTagsWithDefault")
    val emptiableCollections = Set("tags", "counts", "vec", "set")

    test("default config: defaults, Options and collections (not String)") {
      assertEquals(optionalFields(AbsJsoniterDefault.derived.schema), always ++ defaults ++ emptiableCollections)
    }

    test("requireDefaultFields: fields with defaults are required (not Options, nor collections that can be empty)") {
      // an absent `listWithDefault` is still decoded as an empty list
      assertEquals(
        optionalFields(AbsJsoniterRequireDefaults.derived.schema),
        always ++ emptiableCollections + "listWithDefault"
      )
    }

    test("requireCollectionFields: collections without defaults are required") {
      assertEquals(optionalFields(AbsJsoniterRequireCollections.derived.schema), always ++ defaults)
    }

    test("requireCollectionFields + requireDefaultFields: only Options are optional") {
      assertEquals(optionalFields(AbsJsoniterRequireBoth.derived.schema), always)
    }

    test("transient* settings only affect encoding, not which fields are optional") {
      assertEquals(optionalFields(AbsJsoniterTransient.derived.schema), always ++ defaults ++ emptiableCollections)
    }

    test("a config known only at runtime") {
      assertEquals(optionalFields(AbsJsoniterRuntime.derived.schema), always ++ defaults)
    }
  }

  group("circe: schema optionality matches the decoder") {
    circeAll.foreach { derived =>
      test(s"config: ${derived.name}") {
        assertEquals(productFields(derived.schema).map(_._1), allFields)
        val complete = derived.encoder(AbsAll.sample)
        val accepted = acceptedMissing(allFields, complete)(json => derived.decoder.decodeJson(json).isRight)
        assertEquals(optionalFields(derived.schema), accepted)
        assertEquals(required(derived.schema), allFields.toSet -- accepted)
      }
    }
  }
}
