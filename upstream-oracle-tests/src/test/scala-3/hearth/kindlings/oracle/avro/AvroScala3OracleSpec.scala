package hearth.kindlings.oracle.avro

import com.sksamuel.avro4s.{AvroError, AvroName, AvroNamespace, AvroProp, AvroScalePrecision}
import hearth.kindlings.avroderivation.{AvroConfig, AvroDecoder, AvroEncoder}
import hearth.kindlings.avroderivation.annotations.{avroError, avroName, avroNamespace, avroProp, avroScalePrecision}
import hearth.kindlings.oracle.Oracle

// Annotations only avro4s 5 has (Kindlings has them on both Scala versions).

@AvroError()
@avroError
case class AvFailure(code: Int, message: String)

// avro4s 5: `(scale, precision)`; Kindlings: `(precision, scale)`
case class AvScaled(@AvroScalePrecision(3, 9) @avroScalePrecision(9, 3) amount: BigDecimal)

// avro4s: schema/AvroErrorSchemaTest ("@AvroError should set schema type to Error when used on a field")
case class AuFlibble(a: String)
case class AuVeryCross(@AvroError @avroError flibble: AuFlibble)

// avro4s: schema/AvroPropSchemaTest ("support json prop annotation on field"): avro4s 5 takes a JsonNode, Kindlings
// parses a value that looks like JSON
object AuJson {
  val terms: com.fasterxml.jackson.databind.JsonNode =
    new com.fasterxml.jackson.databind.ObjectMapper().createArrayNode().add("foo").add("bar")
}
case class AuJsonProp(@AvroProp("terms", AuJson.terms) @avroProp("terms", """["foo","bar"]""") str: String)

// avro4s: record/encoder/Scala3EnumEncoderTest, record/decoder/Scala3EnumDecoderTest
enum AuSimple {
  case ONE, TWO, THREE
}
enum AuExtending {
  case ONE extends AuExtending
  case TWO extends AuExtending
  case THREE extends AuExtending
}
enum AuParametrized(val param: Int) {
  case ONE extends AuParametrized(1)
  case TWO extends AuParametrized(2)
  case THREE extends AuParametrized(3)
}
@AvroNamespace("com.test.namespace")
@avroNamespace("com.test.namespace")
@AvroName("CustomSimple")
@avroName("CustomSimple")
enum AuSimpleAnnotated {
  case ONE, TWO, THREE
}
@AvroNamespace("com.test.namespace")
@avroNamespace("com.test.namespace")
@AvroName("CustomParametrizedWithInt")
@avroName("CustomParametrizedWithInt")
enum AuParametrizedAnnotated(val param: Int) {
  case ONE extends AuParametrizedAnnotated(1)
  case TWO extends AuParametrizedAnnotated(2)
  case THREE extends AuParametrizedAnnotated(3)
}
final case class AuTestProduct(a: String, b: Int)
enum AuWithProduct(val product: AuTestProduct) {
  case One extends AuWithProduct(AuTestProduct("a", 1))
  case Two extends AuWithProduct(AuTestProduct("b", 2))
}
case class AuScala3Enums(simple: AuSimple, parametrized: AuParametrized, opt: Option[AuSimpleAnnotated])

/** avro4s 5-only features vs Kindlings' equivalents (see [[AvroOracleSpec]] for the shared suite). */
final class AvroScala3OracleSpec extends AvroOracleSuite {

  private given AvroConfig = AvroConfig.default

  private def k[A](using AvroEncoder[A], AvroDecoder[A]): AvroSide[A] = AvroSide.kindlings[A]

  private def assertSameSchemaAndBytes[A](k: AvroSide[A], u: AvroSide[A], value: A): Unit = {
    assertEquals(Oracle.diff(AvroOracle.schemaTree(k.schema), AvroOracle.schemaTree(u.schema)), Nil)
    val kb = AvroOracle.encodeBinary(k, value).get
    val ub = AvroOracle.encodeBinary(u, value).get
    assertEquals(AvroOracle.hex(kb), AvroOracle.hex(ub))
    assertEquals(AvroOracle.decodeBinary(k, u.schema, ub).get, value)
    assertEquals(AvroOracle.decodeBinary(u, k.schema, kb).get, value)
  }

  test("@avroError vs @AvroError") {
    val k = AvroSide.kindlings(AvroEncoder.derived[AvFailure], AvroDecoder.derived[AvFailure])
    val u = AvroUpstream.side[AvFailure]
    assert(k.schema.isError)
    assertSameSchemaAndBytes(k, u, AvFailure(404, "not found"))
  }

  test("@avroError on a field (AvroErrorSchemaTest)") {
    val kindlings = k[AuVeryCross]
    assert(kindlings.schema.getField("flibble").schema.isError)
    compare(kindlings, u[AuVeryCross], AuVeryCross(AuFlibble("a")))()
  }

  test("JSON @avroProp (AvroPropSchemaTest): avro4s 5 only") {
    // avro4s 5's @AvroProp also takes a JsonNode; Kindlings' @avroProp takes Strings, as avro4s 4's
    // (feature-parity.md, "Custom properties")
    val kindlings = k[AuJsonProp]
    val upstream = u[AuJsonProp]
    assertEquals(kindlings.schema.getField("str").getObjectProp("terms"), """["foo","bar"]""": AnyRef)
    assertEquals(upstream.schema.getField("str").getObjectProp("terms"), java.util.Arrays.asList("foo", "bar"): AnyRef)
  }

  test("@avroScalePrecision vs @AvroScalePrecision") {
    val k = AvroSide.kindlings(AvroEncoder.derived[AvScaled], AvroDecoder.derived[AvScaled])
    val u = AvroUpstream.side[AvScaled]
    // avro4s 5 declares @AvroScalePrecision but never reads it (its schema keeps the implicit ScalePrecision, 8/2);
    // Kindlings honours the annotation (feature-parity.md, avro annotations: an extension, not a difference to fix).
    assertEquals(
      Oracle.diff(AvroOracle.schemaTree(k.schema), AvroOracle.schemaTree(u.schema)),
      List("$[fields][0][type][precision]: kindlings=9 upstream=8", "$[fields][0][type][scale]: kindlings=3 upstream=2")
    )
    val value = AvScaled(BigDecimal("12.345"))
    assertEquals(AvroOracle.decodeBinary(k, k.schema, AvroOracle.encodeBinary(k, value).get).get, value)
  }

  group("Scala 3 enums (Scala3EnumEncoderTest, Scala3EnumDecoderTest)") {
    test("non-parametrized")(compare(k[AuSimple], u[AuSimple], AuSimple.ONE, AuSimple.THREE)())
    test("extending")(compare(k[AuExtending], u[AuExtending], AuExtending.TWO)())
    test("parametrized")(compare(k[AuParametrized], u[AuParametrized], AuParametrized.THREE)())
    test("annotated")(compare(k[AuSimpleAnnotated], u[AuSimpleAnnotated], AuSimpleAnnotated.ONE)())
    test("parametrized, annotated")(
      compare(k[AuParametrizedAnnotated], u[AuParametrizedAnnotated], AuParametrizedAnnotated.THREE)()
    )
    test("parametrized with a product")(compare(k[AuWithProduct], u[AuWithProduct], AuWithProduct.One)())
    test("in a record")(
      compare(
        k[AuScala3Enums],
        u[AuScala3Enums],
        AuScala3Enums(AuSimple.TWO, AuParametrized.ONE, Some(AuSimpleAnnotated.THREE)),
        AuScala3Enums(AuSimple.ONE, AuParametrized.TWO, None)
      )()
    )
  }
}
