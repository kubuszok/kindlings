package hearth.kindlings.integrationtests

import hearth.MacroSuite
import hearth.kindlings.fastshowpretty.{FastShowPretty, RenderConfig}
import hearth.kindlings.integrationtests.newtypeExamples.*

final class NewtypeFastShowPrettySpec extends MacroSuite {

  group("scala-newtype + FastShowPretty") {

    test("newtype value renders as underlying") {
      FastShowPretty.render(NewtypeUserId(1), RenderConfig.Default) ==> "1"
    }

    test("case class with newtype fields renders normally") {
      val result = FastShowPretty.render(NewtypeUser.example, RenderConfig.Default)
      assert(result.contains("alice"), s"Expected 'alice' in: $result")
      assert(result.contains("42"), s"Expected '42' in: $result")
    }
  }
}
