package hearth.kindlings.integrationtests

import hearth.MacroSuite
import hearth.kindlings.fastshowpretty.{FastShowPretty, RenderConfig}

final class NeotypeFastShowPrettySpec extends MacroSuite {

  group("Neotype + FastShowPretty") {

    test("neotype value renders as underlying") {
      FastShowPretty.render(NeotypeAge(30), RenderConfig.Default) ==> "30"
    }

    test("case class with neotype fields renders normally") {
      val person = NeotypePerson(NeotypeEmail("alice@example.com"), NeotypeAge(30), NeotypeNickname("ally"))
      val result = FastShowPretty.render(person, RenderConfig.Default)
      assert(result.contains("alice@example.com"), s"Expected 'alice@example.com' in: $result")
      assert(result.contains("30"), s"Expected '30' in: $result")
    }
  }
}
