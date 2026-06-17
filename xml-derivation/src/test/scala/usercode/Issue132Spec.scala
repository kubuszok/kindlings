package usercode

import hearth.MacroSuite
import hearth.kindlings.xmlderivation.KindlingsXmlCodec

/** Regression test for [[https://github.com/kubuszok/kindlings/issues/132 kindlings#132]]. */
final class Issue132Spec extends MacroSuite {
  import Issue132Spec.*

  test("derives XmlCodec for external sealed trait containing map of case classes") {
    val codec = Issue132RootConfig.rootCodec
    val original = Issue132RootConfig(
      Issue132NestedConfig.WithCollection(
        Map("item" -> Issue132ItemConfig("value"))
      )
    )

    codec.decode(codec.encode(original, "root")) ==> Right(original)
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
    implicit val rootCodec: KindlingsXmlCodec[Issue132RootConfig] =
      KindlingsXmlCodec.derived[Issue132RootConfig]
  }
}
