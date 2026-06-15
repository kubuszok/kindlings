package hearth.kindlings.openapijsoniter

import com.github.plokhotnyuk.jsoniter_scala.core.{readFromString, writeToString}
import hearth.MacroSuite
import sttp.apispec._

import scala.collection.immutable.ListMap

final class SchemaCodecSpec extends MacroSuite {

  import OpenApiJsoniter.circe._

  private def enc(s: Schema): String = writeToString(s)(schemaCodec)
  private def dec(json: String): Schema = readFromString[Schema](json)(schemaCodec)

  group("Schema codec (OpenAPI 3.1)") {

    test("simple object schema with properties") {
      val s = Schema(
        `type` = Some(List(SchemaType.Object)),
        properties = ListMap(
          "name" -> Schema(`type` = Some(List(SchemaType.String))),
          "age" -> Schema(`type` = Some(List(SchemaType.Integer)))
        ),
        required = List("name")
      )
      val json = enc(s)
      json ==> """{"type":"object","required":["name"],"properties":{"name":{"type":"string"},"age":{"type":"integer"}}}"""
      dec(json) ==> s
    }

    test("drops empty collections and None fields") {
      val s = Schema(`type` = Some(List(SchemaType.String)))
      enc(s) ==> """{"type":"string"}"""
    }

    test("$ref schema") {
      val s = Schema($ref = Some("#/components/schemas/Foo"))
      enc(s) ==> """{"$ref":"#/components/schemas/Foo"}"""
      dec(enc(s)) ==> s
    }

    test("enum schema") {
      val s = Schema(
        `type` = Some(List(SchemaType.String)),
        `enum` = Some(List(ExampleSingleValue("a"), ExampleSingleValue("b")))
      )
      enc(s) ==> """{"type":"string","enum":["a","b"]}"""
      dec(enc(s)) ==> s
    }

    test("const schema (3.1 keeps const)") {
      val s = Schema(const = Some(ExampleSingleValue("x")))
      enc(s) ==> """{"const":"x"}"""
      dec(enc(s)) ==> s
    }

    test("validators: numeric bounds") {
      val s = Schema(
        `type` = Some(List(SchemaType.Integer)),
        minimum = Some(BigDecimal(0)),
        exclusiveMaximum = Some(BigDecimal(100))
      )
      enc(s) ==> """{"type":"integer","minimum":0,"exclusiveMaximum":100}"""
      dec(enc(s)) ==> s
    }

    test("oneOf / discriminator") {
      val s = Schema(
        oneOf = List(
          Schema($ref = Some("#/components/schemas/Cat")),
          Schema($ref = Some("#/components/schemas/Dog"))
        ),
        discriminator = Some(Discriminator("petType", Some(ListMap("cat" -> "#/c/Cat"))))
      )
      val json = enc(s)
      dec(json) ==> s
    }

    test("extensions hoisted to object level") {
      val s = Schema(
        `type` = Some(List(SchemaType.String)),
        extensions = ListMap("x-foo" -> ExtensionValue("42"))
      )
      enc(s) ==> """{"type":"string","x-foo":42}"""
      dec(enc(s)) ==> s
    }
  }

  group("AnySchema") {
    test("Anything encodes as true (boolean encoding)") {
      writeToString(AnySchema.Anything: SchemaLike)(schemaLikeCodec) ==> "true"
    }
    test("Nothing encodes as false") {
      writeToString(AnySchema.Nothing: SchemaLike)(schemaLikeCodec) ==> "false"
    }
  }
}
