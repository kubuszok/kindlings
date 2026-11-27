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

  /** Writes `values` as an Avro object container file (the "data" format: the schema in the header, then the records),
    * optionally compressed with `codec` (e.g. `CodecFactory.deflateCodec(6)`).
    */
  def toDataFile[A](
      values: Iterable[A],
      codec: org.apache.avro.file.CodecFactory = org.apache.avro.file.CodecFactory.nullCodec()
  )(implicit encoder: AvroEncoder[A]): Array[Byte] = {
    val schema = encoder.schema
    val baos = new ByteArrayOutputStream()
    val writer = new org.apache.avro.file.DataFileWriter[Any](new GenericDatumWriter[Any](schema))
    writer.setCodec(codec)
    writer.create(schema, baos)
    try values.foreach(value => writer.append(encoder.encode(value)))
    finally writer.close()
    baos.toByteArray
  }

  /** Reads every record of an Avro object container file: the data is read with the schema it was written with (from
    * the file's header) and resolved to the decoder's schema.
    */
  def fromDataFile[A](bytes: Array[Byte])(implicit decoder: AvroDecoder[A]): List[A] = {
    val reader = new GenericDatumReader[Any]()
    reader.setExpected(decoder.schema)
    val stream = new org.apache.avro.file.DataFileStream[Any](new java.io.ByteArrayInputStream(bytes), reader)
    try {
      val builder = List.newBuilder[A]
      while (stream.hasNext) builder += decoder.decode(stream.next())
      builder.result()
    } finally stream.close()
  }
}
