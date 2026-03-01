package hearth.kindlings.integrationtests

import io.github.iltotore.iron.*
import io.github.iltotore.iron.constraint.any.*
import io.github.iltotore.iron.constraint.numeric.*
import io.github.iltotore.iron.constraint.string.*
import hearth.MacroSuite
import hearth.kindlings.fastshowpretty.{FastShowPretty, RenderConfig}

final class IronFastShowPrettySpec extends MacroSuite {

  group("Iron + FastShowPretty") {

    test("iron value renders as underlying") {
      val age: Int :| Positive = 30
      FastShowPretty.render(age, RenderConfig.Default) ==> "30"
    }

    test("case class with iron fields renders normally") {
      val person = IronPerson("Alice", 30)
      val result = FastShowPretty.render(person, RenderConfig.Default)
      assert(result.contains("Alice"), s"Expected 'Alice' in: $result")
      assert(result.contains("30"), s"Expected '30' in: $result")
    }
  }
}
