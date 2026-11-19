package hearth.kindlings.benchmarks

import hearth.kindlings.circederivation.{KindlingsDecoder, KindlingsEncoder}
import hearth.kindlings.jsoniterderivation.KindlingsJsonValueCodec
import hearth.kindlings.fastshowpretty.FastShowPretty
import io.circe.{Decoder, Encoder}
import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec

object KindlingsCirceInstances {

  val simpleCCSemiAutoEncoder: Encoder[SimpleCC] = KindlingsEncoder.derived[SimpleCC]
  val simpleCCSemiAutoDecoder: Decoder[SimpleCC] = KindlingsDecoder.derived[SimpleCC]
  val personSemiAutoEncoder: Encoder[Person] = KindlingsEncoder.derived[Person]
  val personSemiAutoDecoder: Decoder[Person] = KindlingsDecoder.derived[Person]
  val eventSemiAutoEncoder: Encoder[Event] = KindlingsEncoder.derived[Event]
  val eventSemiAutoDecoder: Decoder[Event] = KindlingsDecoder.derived[Event]
  val simpleADTSemiAutoEncoder: Encoder[SimpleADT] = KindlingsEncoder.derived[SimpleADT]
  val simpleADTSemiAutoDecoder: Decoder[SimpleADT] = KindlingsDecoder.derived[SimpleADT]

  val simpleCCAutoEncoder: KindlingsEncoder[SimpleCC] = KindlingsEncoder.derived[SimpleCC]
  val simpleCCAutoDecoder: KindlingsDecoder[SimpleCC] = KindlingsDecoder.derived[SimpleCC]
  val personAutoEncoder: KindlingsEncoder[Person] = KindlingsEncoder.derived[Person]
  val personAutoDecoder: KindlingsDecoder[Person] = KindlingsDecoder.derived[Person]
  val eventAutoEncoder: KindlingsEncoder[Event] = KindlingsEncoder.derived[Event]
  val eventAutoDecoder: KindlingsDecoder[Event] = KindlingsDecoder.derived[Event]
  val simpleADTAutoEncoder: KindlingsEncoder[SimpleADT] = KindlingsEncoder.derived[SimpleADT]
  val simpleADTAutoDecoder: KindlingsDecoder[SimpleADT] = KindlingsDecoder.derived[SimpleADT]
}

/** Instances derived with a configuration only known at runtime (a parameter the macro cannot evaluate). */
object KindlingsCirceRuntimeConfigInstances {
  import hearth.kindlings.circederivation.Configuration

  private def personDecoderWith(config: Configuration): Decoder[Person] = {
    implicit val runtimeConfig: Configuration = config
    KindlingsDecoder.derived[Person]
  }
  private def simpleADTDecoderWith(config: Configuration): Decoder[SimpleADT] = {
    implicit val runtimeConfig: Configuration = config
    KindlingsDecoder.derived[SimpleADT]
  }
  private def eventDecoderWith(config: Configuration): Decoder[Event] = {
    implicit val runtimeConfig: Configuration = config
    KindlingsDecoder.derived[Event]
  }

  // snake_case leaves the names of Person as they are, but the transformation runs (and allocates)
  val personDecoder: Decoder[Person] = personDecoderWith(Configuration.default.withSnakeCaseMemberNames)
  val simpleADTDecoder: Decoder[SimpleADT] = simpleADTDecoderWith(Configuration.default)
  val eventDecoder: Decoder[Event] = eventDecoderWith(Configuration.default)
}

object KindlingsJsoniterInstances {

  val simpleCCSemiAutoCodec: JsonValueCodec[SimpleCC] = KindlingsJsonValueCodec.derived[SimpleCC]
  val personSemiAutoCodec: JsonValueCodec[Person] = KindlingsJsonValueCodec.derived[Person]
  val eventSemiAutoCodec: JsonValueCodec[Event] = KindlingsJsonValueCodec.derived[Event]
  val simpleADTSemiAutoCodec: JsonValueCodec[SimpleADT] = KindlingsJsonValueCodec.derived[SimpleADT]

  val simpleCCAutoCodec: KindlingsJsonValueCodec[SimpleCC] = KindlingsJsonValueCodec.derived[SimpleCC]
  val personAutoCodec: KindlingsJsonValueCodec[Person] = KindlingsJsonValueCodec.derived[Person]
  val eventAutoCodec: KindlingsJsonValueCodec[Event] = KindlingsJsonValueCodec.derived[Event]
  val simpleADTAutoCodec: KindlingsJsonValueCodec[SimpleADT] = KindlingsJsonValueCodec.derived[SimpleADT]
}

object KindlingsFastShowPrettyInstances {
  val simpleCCInstance: FastShowPretty[SimpleCC] = FastShowPretty.derived[SimpleCC]
  val personInstance: FastShowPretty[Person] = FastShowPretty.derived[Person]
  val eventInstance: FastShowPretty[Event] = FastShowPretty.derived[Event]
  val simpleADTInstance: FastShowPretty[SimpleADT] = FastShowPretty.derived[SimpleADT]
}
