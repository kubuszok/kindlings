package hearth.kindlings.benchmarks

import com.sksamuel.avro4s.RecordFormat

object OriginalAvroParityInstances {
  val voyageFormat: RecordFormat[AvroVoyage] = RecordFormat[AvroVoyage]
  val priceFormat: RecordFormat[AvroPrice] = {
    implicit val roundingMode: scala.math.BigDecimal.RoundingMode.RoundingMode =
      scala.math.BigDecimal.RoundingMode.HALF_UP
    RecordFormat[AvroPrice]
  }
}
