package hearth.kindlings.oracle.jsoniter

import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker

/** jsoniter-scala codecs of `JuUpstream` whose source differs between its Scala 2 and Scala 3 macros: the Scala 2 one needs the type of the
  * partial function literal. */
object JuUpstreamVersioned {
  lazy val openapiHintMapper: JsonValueCodec[JUOpenapi.Enum] = JsonCodecMaker.makeOpenapiLike[JUOpenapi.Enum](
    "hint",
    ({ case "EnumValue1" => "enum_value_1"; case "EnumValue2" => "enum_value_2" }: PartialFunction[String, String])
  )
}
