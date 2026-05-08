package hearth.kindlings.benchmarks

import io.circe.{Decoder, Encoder}
import io.circe.generic.auto.given

object OriginalCirceAutoInstances {
  val simpleCCEncoder: Encoder[SimpleCC] = summon
  val simpleCCDecoder: Decoder[SimpleCC] = summon
  val simpleADTEncoder: Encoder[SimpleADT] = summon
  val simpleADTDecoder: Decoder[SimpleADT] = summon
  val personEncoder: Encoder[Person] = summon
  val personDecoder: Decoder[Person] = summon
  val (eventEncoder: Encoder[Event], eventDecoder: Decoder[Event]) = {
    implicit val pe: Encoder[Person] = personEncoder
    implicit val pd: Decoder[Person] = personDecoder
    (summon[Encoder[Event]], summon[Decoder[Event]])
  }
}
