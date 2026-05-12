package hearth.kindlings.fastshowpretty.internal.runtime

import hearth.kindlings.fastshowpretty.{FastShowPretty, RenderConfig}

object FastShowPrettyFactories {

  def instance[A](renderFn: (StringBuilder, RenderConfig, Int, A) => StringBuilder): FastShowPretty[A] =
    new FastShowPretty[A] {
      def render(sb: StringBuilder, config: RenderConfig, level: Int)(value: A): StringBuilder =
        renderFn(sb, config, level, value)
    }
}
