package hearth.kindlings.benchmarks

import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker

object OriginalJsoniterInstances {

  val simpleCCCodec: JsonValueCodec[SimpleCC] = JsonCodecMaker.make[SimpleCC]
  val personCodec: JsonValueCodec[Person] = JsonCodecMaker.make[Person]
  val eventCodec: JsonValueCodec[Event] = JsonCodecMaker.make[Event]
  val simpleADTCodec: JsonValueCodec[SimpleADT] = JsonCodecMaker.make[SimpleADT]
}
