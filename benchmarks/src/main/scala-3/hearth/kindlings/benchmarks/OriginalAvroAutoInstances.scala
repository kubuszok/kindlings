package hearth.kindlings.benchmarks

import com.sksamuel.avro4s.{Decoder as AvroDecoder, Encoder as AvroEncoder, SchemaFor}
import org.apache.avro.Schema
import org.apache.avro.generic.GenericRecord

final class Avro4sFormat[A](schema: Schema, encoder: AvroEncoder[A], decoder: AvroDecoder[A]) {
  private val encodeFn = encoder.encode(schema)
  private val decodeFn = decoder.decode(schema)
  def to(value: A): GenericRecord = encodeFn(value).asInstanceOf[GenericRecord]
  def from(record: GenericRecord): A = decodeFn(record)
}

object OriginalAvroAutoInstances {
  val simpleCCFormat: Avro4sFormat[SimpleCC] = {
    given SchemaFor[SimpleCC] = SchemaFor.derived
    given AvroEncoder[SimpleCC] = AvroEncoder.derived
    given AvroDecoder[SimpleCC] = AvroDecoder.derived
    new Avro4sFormat(summon[SchemaFor[SimpleCC]].schema, summon[AvroEncoder[SimpleCC]], summon[AvroDecoder[SimpleCC]])
  }
  val personFormat: Avro4sFormat[Person] = {
    given SchemaFor[Address] = SchemaFor.derived
    given AvroEncoder[Address] = AvroEncoder.derived
    given AvroDecoder[Address] = AvroDecoder.derived
    given SchemaFor[Person] = SchemaFor.derived
    given AvroEncoder[Person] = AvroEncoder.derived
    given AvroDecoder[Person] = AvroDecoder.derived
    new Avro4sFormat(summon[SchemaFor[Person]].schema, summon[AvroEncoder[Person]], summon[AvroDecoder[Person]])
  }
}
