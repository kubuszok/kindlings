package hearth.kindlings.integrationtests

import eu.timepit.refined.refineV
import eu.timepit.refined.collection.NonEmpty
import eu.timepit.refined.numeric.Positive
import hearth.MacroSuite
import hearth.kindlings.fastshowpretty.{FastShowPretty, RenderConfig}

final class RefinedFastShowPrettySpec extends MacroSuite {

  private val alice = refineV[NonEmpty]("Alice").toOption.get
  private val thirty = refineV[Positive](30).toOption.get

  group("Refined + FastShowPretty") {

    test("case class with refined fields renders normally") {
      val person = RefinedPerson(alice, thirty)
      val result = FastShowPretty.render(person, RenderConfig.Default)
      assert(result.contains("Alice"), s"Expected 'Alice' in: $result")
      assert(result.contains("30"), s"Expected '30' in: $result")
    }
  }
}
