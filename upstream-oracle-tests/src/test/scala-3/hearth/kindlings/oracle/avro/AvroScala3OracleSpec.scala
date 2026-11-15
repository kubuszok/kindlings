package hearth.kindlings.oracle.avro

import com.sksamuel.avro4s.{AvroError, AvroScalePrecision}
import hearth.MacroSuite
import hearth.kindlings.avroderivation.{AvroConfig, AvroDecoder, AvroEncoder}
import hearth.kindlings.avroderivation.annotations.{avroError, avroScalePrecision}
import hearth.kindlings.oracle.Oracle

// Annotations only avro4s 5 has (Kindlings has them on both Scala versions).

@AvroError()
@avroError
case class AvFailure(code: Int, message: String)

// avro4s 5: `(scale, precision)`; Kindlings: `(precision, scale)`
case class AvScaled(@AvroScalePrecision(3, 9) @avroScalePrecision(9, 3) amount: BigDecimal)

/** avro4s 5-only annotations vs Kindlings' equivalents (see [[AvroOracleSpec]] for the shared suite). */
final class AvroScala3OracleSpec extends MacroSuite {

  private given AvroConfig = AvroConfig.default

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
}
