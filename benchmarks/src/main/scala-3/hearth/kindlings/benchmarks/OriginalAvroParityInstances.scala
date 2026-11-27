package hearth.kindlings.benchmarks

import com.sksamuel.avro4s.{Decoder as AvroDecoder, Encoder as AvroEncoder, SchemaFor}

object OriginalAvroParityInstances {
  val voyageFormat: Avro4sFormat[AvroVoyage] =
    new Avro4sFormat(
      summon[SchemaFor[AvroVoyage]].schema,
      summon[AvroEncoder[AvroVoyage]],
      summon[AvroDecoder[AvroVoyage]]
    )
}
