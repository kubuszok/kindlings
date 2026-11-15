package hearth.kindlings.oracle.avro

import com.fasterxml.jackson.databind.{JsonNode, ObjectMapper}
import hearth.kindlings.avroderivation.{AvroDecoder, AvroEncoder}
import org.apache.avro.Schema
import org.apache.avro.generic.{GenericDatumReader, GenericDatumWriter}
import org.apache.avro.io.{DecoderFactory, EncoderFactory}

import java.io.ByteArrayOutputStream
import scala.collection.immutable.ListMap
import scala.jdk.CollectionConverters.*
import scala.util.Try

/** One side of an avro oracle comparison: the schema, the encoder (Scala value -> Avro generic value) and the decoder
  * (Avro generic value -> Scala value) derived for `A` by Kindlings or by avro4s.
  */
final class AvroSide[A](val schema: Schema, val encode: A => Any, val decode: Any => A)
object AvroSide {

  def kindlings[A](implicit encoder: AvroEncoder[A], decoder: AvroDecoder[A]): AvroSide[A] =
    new AvroSide[A](encoder.schema, encoder.encode, decoder.decode)
}

object AvroOracle {

  private val mapper = new ObjectMapper()

  /** The schema as a Scala structure (ordered maps, lists, scalars), so `Oracle.diff` reports differing paths. */
  def schemaTree(schema: Schema): Any = jsonTree(mapper.readTree(schema.toString))

  private def jsonTree(node: JsonNode): Any =
    if (node.isObject) ListMap(node.properties().asScala.toList.map(e => e.getKey -> jsonTree(e.getValue))*)
    else if (node.isArray) node.elements().asScala.toList.map(jsonTree)
    else if (node.isTextual) node.textValue()
    else if (node.isNull) null
    else node.toString

  def toBinary(schema: Schema, value: Any): Array[Byte] = {
    val writer = new GenericDatumWriter[Any](schema)
    val out = new ByteArrayOutputStream()
    val encoder = EncoderFactory.get().binaryEncoder(out, null)
    writer.write(value, encoder)
    encoder.flush()
    out.toByteArray
  }

  /** Reads `bytes` written with `writer` as `reader` (Avro schema resolution), producing Avro generic values. */
  def fromBinary(writer: Schema, reader: Schema, bytes: Array[Byte]): Any =
    new GenericDatumReader[Any](writer, reader).read(null, DecoderFactory.get().binaryDecoder(bytes, null))

  /** Encodes `value` to Avro binary with `side`'s schema and encoder. */
  def encodeBinary[A](side: AvroSide[A], value: A): Try[Array[Byte]] =
    Try(toBinary(side.schema, side.encode(value)))

  /** Decodes Avro binary written with `writer` using `side` (its schema as the reader schema). */
  def decodeBinary[A](side: AvroSide[A], writer: Schema, bytes: Array[Byte]): Try[A] =
    Try(side.decode(fromBinary(writer, side.schema, bytes)))

  def hex(bytes: Array[Byte]): String = bytes.map(b => f"$b%02x").mkString
}
