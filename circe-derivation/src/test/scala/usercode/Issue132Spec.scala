package usercode

import hearth.MacroSuite
import hearth.kindlings.circederivation.KindlingsCodecAsObject
import io.circe.Codec

final class Issue132Spec extends MacroSuite {

  test("derives Codec.AsObject for external sealed trait containing map of case classes") {
    val codec = Issue132RootConfig.rootCodec
    val original = Issue132RootConfig(
      Issue132NestedConfig.WithCollection(
        Map("item" -> Issue132ItemConfig("value"))
      )
    )

    codec.decodeJson(codec(original)) ==> Right(original)
  }
}

sealed trait Issue132NestedConfig extends Product with Serializable

object Issue132NestedConfig {
  final case class WithCollection(items: Map[String, Issue132ItemConfig]) extends Issue132NestedConfig
  final case class Disabled(reason: String) extends Issue132NestedConfig
}

final case class Issue132ItemConfig(value: String)

final case class Issue132RootConfig(nested: Issue132NestedConfig)

object Issue132RootConfig {
  implicit val rootCodec: Codec.AsObject[Issue132RootConfig] =
    KindlingsCodecAsObject.derived[Issue132RootConfig]
}
