package hearth.kindlings.benchmarks

import io.circe.{Decoder, Encoder}
import io.circe.generic.auto.*

object OriginalCirceAutoInstances {
  val simpleCCEncoder: Encoder[SimpleCC] = implicitly
  val simpleCCDecoder: Decoder[SimpleCC] = implicitly
  val personEncoder: Encoder[Person] = implicitly
  val personDecoder: Decoder[Person] = implicitly
  val eventEncoder: Encoder[Event] = implicitly
  val eventDecoder: Decoder[Event] = implicitly
  val simpleADTEncoder: Encoder[SimpleADT] = implicitly
  val simpleADTDecoder: Decoder[SimpleADT] = implicitly
}
