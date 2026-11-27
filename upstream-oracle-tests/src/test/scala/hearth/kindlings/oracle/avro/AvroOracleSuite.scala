package hearth.kindlings.oracle.avro

import com.sksamuel.avro4s.{Decoder as UDecoder, Encoder as UEncoder, SchemaFor as USchemaFor}
import hearth.MacroSuite
import hearth.kindlings.oracle.Oracle
import org.apache.avro.Schema
import org.apache.avro.generic.{GenericData, GenericRecord}

import scala.util.{Failure, Success, Try}

/** The comparisons shared by the avro oracle suites: schemas (as JSON trees), binary encodings, each side decoding the
  * other's bytes, and the decoding of hand-built inputs. Every comparison takes the *exact* list of expected
  * differences.
  */
abstract class AvroOracleSuite extends MacroSuite {

  protected val isAvro4s5 = AvroUpstream.majorVersion == 5

  protected def schemaDiff(k: AvroSide[?], u: AvroSide[?]): List[String] =
    Oracle.diff(AvroOracle.schemaTree(k.schema), AvroOracle.schemaTree(u.schema)).map("schema " + _)

  /** Encodes `value` with both sides, compares the bytes, and decodes each side's bytes with both sides. */
  protected def valueDiff[A](k: AvroSide[A], u: AvroSide[A], value: A, eq: (A, A) => Boolean): List[String] = {
    val kb = AvroOracle.encodeBinary(k, value)
    val ub = AvroOracle.encodeBinary(u, value)
    val bytes = (kb, ub) match {
      case (Success(a), Success(b)) =>
        if (java.util.Arrays.equals(a, b)) Nil
        else List(s"bytes: kindlings=${AvroOracle.hex(a)} upstream=${AvroOracle.hex(b)}")
      case _ => List(s"bytes: kindlings=$kb upstream=$ub")
    }
    def check(label: String, decoded: Try[A]): List[String] = decoded match {
      case Success(d) if eq(d, value) => Nil
      case other                      => List(s"$label: $other")
    }
    val cross = (kb.toOption, ub.toOption) match {
      case (Some(a), Some(b)) =>
        check("kindlings decodes upstream bytes", AvroOracle.decodeBinary(k, u.schema, b)) ++
          check("upstream decodes kindlings bytes", AvroOracle.decodeBinary(u, k.schema, a)) ++
          check("kindlings round trip", AvroOracle.decodeBinary(k, k.schema, a)) ++
          check("upstream round trip", AvroOracle.decodeBinary(u, u.schema, b))
      case _ => Nil
    }
    bytes ++ cross
  }

  protected def compareWith[A](k: AvroSide[A], u: AvroSide[A], values: List[A], eq: (A, A) => Boolean)(
      expectedDifferences: String*
  ): Unit = {
    val differences = schemaDiff(k, u) ++ values.flatMap(valueDiff(k, u, _, eq)).distinct
    assertEquals(
      differences,
      expectedDifferences.toList,
      s"Kindlings vs avro4s ${AvroUpstream.majorVersion} differences:\n${differences.mkString("\n")}\n" +
        s"kindlings schema: ${k.schema}\nupstream schema:  ${u.schema}"
    )
  }

  protected def compare[A](k: AvroSide[A], u: AvroSide[A], values: A*)(expectedDifferences: String*): Unit =
    compareWith(k, u, values.toList, (a: A, b: A) => a == b)(expectedDifferences*)

  /** For documented differences of the default config: only the schemas are compared (the encodings then differ by
    * construction); the same fixture is compared in full under `AvroConfig.avro4sDefaults`.
    */
  protected def compareSchemas(k: AvroSide[?], u: AvroSide[?])(expectedDifferences: String*): Unit = {
    val differences = schemaDiff(k, u)
    assertEquals(
      differences,
      expectedDifferences.toList,
      s"Kindlings vs avro4s ${AvroUpstream.majorVersion} schema differences:\n${differences.mkString("\n")}"
    )
  }

  protected def u[A](implicit s: USchemaFor[A], e: UEncoder[A], d: UDecoder[A]): AvroSide[A] = AvroUpstream.side[A]

  /** Decodes the same Avro value with both decoders and compares the outcomes (the value, or a failure). */
  protected def assertDecodes[A](k: AvroSide[A], u: AvroSide[A], input: Any)(expectedDifferences: String*): Unit = {
    val kd = Try(k.decode(input))
    val ud = Try(u.decode(input))
    val differences = (kd, ud) match {
      case (Success(a), Success(b)) if a == b => Nil
      case (Failure(_), Failure(_))           => Nil
      case _                                  => List(s"decoded: kindlings=$kd upstream=$ud")
    }
    assertEquals(differences, expectedDifferences.toList, differences.mkString("\n"))
  }

  protected def record(schemaJson: String)(fields: (String, Any)*): GenericRecord = {
    val r = new GenericData.Record(new Schema.Parser().parse(schemaJson))
    fields.foreach { case (n, v) => r.put(n, v) }
    r
  }

  protected def recordSchema(name: String, fields: String*): String =
    s"""{"type":"record","name":"$name","namespace":"hearth.kindlings.oracle.avro","fields":[${fields.mkString(
        ","
      )}]}"""
}
