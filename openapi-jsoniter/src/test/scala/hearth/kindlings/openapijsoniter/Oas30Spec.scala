package hearth.kindlings.openapijsoniter

import com.github.plokhotnyuk.jsoniter_scala.core.{readFromString, writeToString}
import hearth.MacroSuite
import sttp.apispec._

/** Cross-platform (no resource IO) tests for the OAS 3.0.3 Schema translations and their decode inverses. */
final class Oas30Spec extends MacroSuite {

  import OpenApiJsoniter.circe_openapi_3_0_3.{schemaCodec => codec30}
  import OpenApiJsoniter.circe.{schemaCodec => codec31}

  private def enc30(s: Schema): String = writeToString(s)(codec30)
  private def dec30(s: String): Schema = readFromString[Schema](s)(codec30)
  private def enc31(s: Schema): String = writeToString(s)(codec31)

  group("OAS 3.0 const -> single-element enum") {
    test("const becomes enum on encode") {
      enc30(Schema(const = Some(ExampleSingleValue("x")))) ==> """{"enum":["x"]}"""
    }
    test("3.1 keeps const") {
      enc31(Schema(const = Some(ExampleSingleValue("x")))) ==> """{"const":"x"}"""
    }
  }

  group("OAS 3.0 type:[t,null] -> nullable") {
    test("nullable string encodes with nullable:true") {
      enc30(Schema(`type` = Some(List(SchemaType.String, SchemaType.Null)))) ==>
        """{"type":"string","nullable":true}"""
    }
    test("decode reverses nullable into type array") {
      val decoded = dec30("""{"type":"string","nullable":true}""")
      decoded.`type` ==> Some(List(SchemaType.String, SchemaType.Null))
    }
  }

  group("OAS 3.0 exclusive bounds as booleans") {
    test("exclusiveMinimum numeric -> minimum + exclusiveMinimum:true") {
      enc30(Schema(exclusiveMinimum = Some(BigDecimal(10)))) ==>
        """{"minimum":10,"exclusiveMinimum":true}"""
    }
    test("decode reverses boolean exclusiveMinimum") {
      val decoded = dec30("""{"minimum":10,"exclusiveMinimum":true}""")
      decoded.exclusiveMinimum ==> Some(BigDecimal(10))
      decoded.minimum ==> None
    }
    test("exclusiveMaximum numeric -> maximum + exclusiveMaximum:true") {
      enc30(Schema(exclusiveMaximum = Some(BigDecimal(20)))) ==>
        """{"maximum":20,"exclusiveMaximum":true}"""
    }
  }

  group("OAS 3.0 examples -> example") {
    test("single example list -> example") {
      enc30(Schema(`type` = Some(List(SchemaType.String)), examples = Some(List(ExampleSingleValue("v"))))) ==>
        """{"example":"v","type":"string"}"""
    }
    test("decode reverses example into examples list") {
      val decoded = dec30("""{"type":"string","example":"v"}""")
      decoded.examples.map(_.size) ==> Some(1)
    }
  }

  group("OAS 3.0 nullable $ref -> allOf + nullable") {
    test("anyOf[ref, null] encodes as allOf[ref] + nullable:true") {
      val s = Schema(anyOf =
        List(Schema($ref = Some("#/components/schemas/Foo")), Schema(`type` = Some(List(SchemaType.Null))))
      )
      enc30(s) ==> """{"nullable":true,"allOf":[{"$ref":"#/components/schemas/Foo"}]}"""
    }
  }
}
