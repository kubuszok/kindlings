package hearth.kindlings.integrationtests

import cats.data.{Chain, Const, NonEmptyChain, NonEmptyList, NonEmptyMap, NonEmptySet, NonEmptyVector, Validated}
import hearth.MacroSuite
import hearth.kindlings.fastshowpretty.{FastShowPretty, RenderConfig}

final class CatsFastShowPrettySpec extends MacroSuite {

  group("Cats + FastShowPretty") {

    test("NonEmptyList renders") {
      val v = WithNEL(NonEmptyList.of(1, 2, 3))
      val result = FastShowPretty.render(v, RenderConfig.Default)
      assert(result.contains("1"), s"Expected '1' in: $result")
      assert(result.contains("2"), s"Expected '2' in: $result")
      assert(result.contains("3"), s"Expected '3' in: $result")
    }

    test("NonEmptyVector renders") {
      val v = WithNEV(NonEmptyVector.of(10, 20))
      val result = FastShowPretty.render(v, RenderConfig.Default)
      assert(result.contains("10"), s"Expected '10' in: $result")
      assert(result.contains("20"), s"Expected '20' in: $result")
    }

    test("NonEmptyChain renders") {
      val v = WithNEC(NonEmptyChain.of(7, 8, 9))
      val result = FastShowPretty.render(v, RenderConfig.Default)
      assert(result.contains("7"), s"Expected '7' in: $result")
      assert(result.contains("9"), s"Expected '9' in: $result")
    }

    test("Chain renders") {
      val v = WithChain(Chain(4, 5))
      val result = FastShowPretty.render(v, RenderConfig.Default)
      assert(result.contains("4"), s"Expected '4' in: $result")
      assert(result.contains("5"), s"Expected '5' in: $result")
    }

    test("Chain empty renders") {
      val v = WithChain(Chain.empty)
      val result = FastShowPretty.render(v, RenderConfig.Default)
      assert(result.contains("WithChain"), s"Expected 'WithChain' in: $result")
    }

    test("NonEmptyMap renders") {
      val v = WithNEM(NonEmptyMap.of("a" -> 1, "b" -> 2))
      val result = FastShowPretty.render(v, RenderConfig.Default)
      assert(result.contains("a"), s"Expected 'a' in: $result")
      assert(result.contains("1"), s"Expected '1' in: $result")
    }

    test("NonEmptySet renders") {
      val v = WithNES(NonEmptySet.of(3, 1, 2))
      val result = FastShowPretty.render(v, RenderConfig.Default)
      assert(result.contains("1"), s"Expected '1' in: $result")
      assert(result.contains("2"), s"Expected '2' in: $result")
      assert(result.contains("3"), s"Expected '3' in: $result")
    }

    test("Const renders") {
      val v = WithConst(Const("hello"))
      val result = FastShowPretty.render(v, RenderConfig.Default)
      assert(result.contains("hello"), s"Expected 'hello' in: $result")
    }

    test("Validated Valid renders") {
      val v = WithValidated(Validated.valid(42))
      val result = FastShowPretty.render(v, RenderConfig.Default)
      assert(result.contains("42"), s"Expected '42' in: $result")
    }

    test("Validated Invalid renders") {
      val v = WithValidated(Validated.invalid("error"))
      val result = FastShowPretty.render(v, RenderConfig.Default)
      assert(result.contains("error"), s"Expected 'error' in: $result")
    }
  }
}
