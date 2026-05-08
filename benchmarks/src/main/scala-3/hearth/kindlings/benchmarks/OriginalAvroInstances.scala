package hearth.kindlings.benchmarks

import org.apache.avro.generic.GenericRecord

// avro4s 5.x (Scala 3) has an incompatible API with avro4s 4.x (Scala 2.13).
// On Scala 3 the "original" avro benchmarks delegate to kindlings, so the
// comparison is only meaningful on Scala 2.13 where avro4s 4.x is used.
object OriginalAvroInstances {

  val simpleCCFormat: Avro4sCompat[SimpleCC] = Avro4sCompat(
    KindlingsAvroInstances.simpleCCEncoder,
    KindlingsAvroInstances.simpleCCDecoder
  )
  val personFormat: Avro4sCompat[Person] = Avro4sCompat(
    KindlingsAvroInstances.personEncoder,
    KindlingsAvroInstances.personDecoder
  )
}

final class Avro4sCompat[A](
    private val encoder: hearth.kindlings.avroderivation.AvroEncoder[A],
    private val decoder: hearth.kindlings.avroderivation.AvroDecoder[A]
) {
  def to(value: A): GenericRecord = encoder.encode(value).asInstanceOf[GenericRecord]
  def from(record: GenericRecord): A = decoder.decode(record)
}
object Avro4sCompat {
  def apply[A](
      encoder: hearth.kindlings.avroderivation.AvroEncoder[A],
      decoder: hearth.kindlings.avroderivation.AvroDecoder[A]
  ): Avro4sCompat[A] = new Avro4sCompat(encoder, decoder)
}
