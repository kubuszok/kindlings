package hearth.kindlings.avroderivation

import org.apache.avro.generic.{GenericDatumReader, GenericDatumWriter}
import org.apache.avro.io.{DecoderFactory, EncoderFactory}

import java.io.ByteArrayOutputStream

object AvroIO {

  def toBinary[A](value: A)(implicit encoder: AvroEncoder[A]): Array[Byte] = {
    val schema = encoder.schema
    val avroValue = encoder.encode(value)
    val writer = new GenericDatumWriter[Any](schema)
    val baos = new ByteArrayOutputStream()
    val avroEncoder = EncoderFactory.get().binaryEncoder(baos, null)
    writer.write(avroValue, avroEncoder)
    avroEncoder.flush()
    baos.toByteArray
  }

  def fromBinary[A](bytes: Array[Byte])(implicit decoder: AvroDecoder[A]): A = {
    val schema = decoder.schema
    val reader = new GenericDatumReader[Any](schema)
    val avroDecoder = DecoderFactory.get().binaryDecoder(bytes, null)
    val avroValue = reader.read(null, avroDecoder)
    decoder.decode(avroValue)
  }

  def toJson[A](value: A)(implicit encoder: AvroEncoder[A]): String = {
    val schema = encoder.schema
    val avroValue = encoder.encode(value)
    val writer = new GenericDatumWriter[Any](schema)
    val baos = new ByteArrayOutputStream()
    val avroEncoder = EncoderFactory.get().jsonEncoder(schema, baos)
    writer.write(avroValue, avroEncoder)
    avroEncoder.flush()
    baos.toString("UTF-8")
  }

  def fromJson[A](json: String)(implicit decoder: AvroDecoder[A]): A = {
    val schema = decoder.schema
    val reader = new GenericDatumReader[Any](schema)
    val avroDecoder = DecoderFactory.get().jsonDecoder(schema, json)
    val avroValue = reader.read(null, avroDecoder)
    decoder.decode(avroValue)
  }
}
