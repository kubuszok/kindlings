package usercode

import hearth.MacroSuite
import hearth.kindlings.circederivation.KindlingsCodecAsObject
import io.circe.Codec

/** Regression test for [[https://github.com/kubuszok/kindlings/issues/132 kindlings#132]]. */
final class Issue132Spec extends MacroSuite {
  import Issue132Spec.*

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

object Issue132Spec {

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
}
