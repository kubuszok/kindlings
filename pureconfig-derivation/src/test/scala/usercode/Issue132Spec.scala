package usercode

import hearth.MacroSuite
import hearth.kindlings.pureconfigderivation.KindlingsConfigConvert
import pureconfig.ConfigConvert

/** Regression test for [[https://github.com/kubuszok/kindlings/issues/132 kindlings#132]]. */
final class Issue132Spec extends MacroSuite {
  import Issue132Spec.*

  test("derives ConfigConvert for external sealed trait containing map of case classes") {
    val convert = Issue132RootConfig.rootConfigConvert
    val original = Issue132RootConfig(
      Issue132NestedConfig.WithCollection(
        Map("item" -> Issue132ItemConfig("value"))
      )
    )
    convert.from(pureconfig.ConfigCursor(convert.to(original), Nil)) ==> Right(original)
  }
}

object Issue132Spec {

  sealed trait Issue132NestedConfig extends Product with Serializable

  object Issue132NestedConfig {
    final case class WithCollection(items: Map[String, Issue132ItemConfig]) extends Issue132NestedConfig
    case object Disabled extends Issue132NestedConfig
  }

  final case class Issue132ItemConfig(value: String)

  final case class Issue132RootConfig(nested: Issue132NestedConfig)

  object Issue132RootConfig {
    implicit val rootConfigConvert: ConfigConvert[Issue132RootConfig] =
      KindlingsConfigConvert.derived[Issue132RootConfig]
  }
}
