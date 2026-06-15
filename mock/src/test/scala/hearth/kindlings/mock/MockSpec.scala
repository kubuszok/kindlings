package hearth.kindlings.mock

import hearth.MacroSuite

final class MockSpec extends MacroSuite {

  group("Mock.mock") {

    test("mocks a single-method trait and matches arguments") {
      implicit val ctx: MockContext = new MockContext
      val greeter = Mock.mock[MockSpec.Greeter]
      val _ = ctx.expecting("greet", "world").returning("hello, world")

      greeter.greet("world") ==> "hello, world"
      ctx.verifyExpectations()
    }

    test("mocks a multi-method, multi-arg trait") {
      implicit val ctx: MockContext = new MockContext
      val calc = Mock.mock[MockSpec.Calculator]
      val _ = ctx.expecting("add", 2, 3).returning(5)
      val _ = ctx.expecting("negate", 7).returning(-7)

      calc.add(2, 3) ==> 5
      calc.negate(7) ==> -7
      ctx.verifyExpectations()
    }

    test("an unsatisfied expectation fails verification") {
      implicit val ctx: MockContext = new MockContext
      val greeter = Mock.mock[MockSpec.Greeter]
      val _ = greeter // constructed but never called
      val _ = ctx.expecting("greet", "nobody").returning("x")

      val _ = intercept[MockExpectationException] {
        ctx.verifyExpectations()
      }
    }
  }

  group("argument matchers") {

    test("epsilon matches a numeric argument within tolerance") {
      implicit val ctx: MockContext = new MockContext
      val sink = Mock.mock[MockSpec.Sink]
      val _ = ctx.expecting("accept", ArgMatcher.epsilon(1.0, 0.05)).returning("near-one")

      sink.accept(1.02) ==> "near-one"
      ctx.verifyExpectations()
    }

    test("argThat matches an argument satisfying a predicate") {
      implicit val ctx: MockContext = new MockContext
      val sink = Mock.mock[MockSpec.Sink]
      val _ = ctx.expecting("accept", ArgMatcher.argThat[Double](_ > 100.0)).returning("big")

      sink.accept(250.0) ==> "big"
      ctx.verifyExpectations()
    }

    test("capture records the argument the expectation fired with") {
      implicit val ctx: MockContext = new MockContext
      val greeter = Mock.mock[MockSpec.Greeter]
      val name = new Capture[String]
      val _ = ctx.expecting("greet", ArgMatcher.capture(name)).returning("hi")

      greeter.greet("captured-name") ==> "hi"
      name.value ==> "captured-name"
      ctx.verifyExpectations()
    }
  }

  group("faithful `.expects` DSL") {
    import syntax._

    test("(m.method _).expects(...).returning(...) registers and matches like the name-keyed API") {
      implicit val ctx: MockContext = new MockContext
      val greeter = Mock.mock[MockSpec.Greeter]
      val _ = (greeter.greet _).expects("world").returning("hello, world")

      greeter.greet("world") ==> "hello, world"
      ctx.verifyExpectations()
    }

    test("faithful DSL resolves the right method on a multi-method mock") {
      implicit val ctx: MockContext = new MockContext
      val calc = Mock.mock[MockSpec.Calculator]
      val _ = (calc.add _).expects(2, 3).returning(5)
      val _ = (calc.negate _).expects(7).returning(-7)

      calc.add(2, 3) ==> 5
      calc.negate(7) ==> -7
      ctx.verifyExpectations()
    }
  }

  group("Mock.stub") {

    test("a stub returns type-appropriate defaults for un-preset methods") {
      implicit val ctx: MockContext = new MockContext
      val calc = Mock.stub[MockSpec.Calculator]

      calc.add(1, 2) ==> 0
      calc.negate(5) ==> 0
    }

    test("when presets a stub's behaviour; un-preset calls still return defaults (name-keyed)") {
      implicit val ctx: MockContext = new MockContext
      val calc = Mock.stub[MockSpec.Calculator]
      val _ = ctx.when("add", 2, 3).returning(5)

      calc.add(2, 3) ==> 5
      calc.add(9, 9) ==> 0
      ctx.callCountFor("add", Seq(2, 3)) ==> 1
    }

    test("faithful stub DSL: (s.method _).when(...).returning(...) and (s.method _).verify(...)") {
      import syntax._
      implicit val ctx: MockContext = new MockContext
      val greeter = Mock.stub[MockSpec.Greeter]
      val _ = (greeter.greet _).when("world").returning("hi")

      greeter.greet("world") ==> "hi"
      greeter.greet("nobody") ==> ""

      (greeter.greet _).verify("world").once()
      (greeter.greet _).verify("nobody").once()
    }

    test("verify fails when the recorded call count does not match") {
      import syntax._
      implicit val ctx: MockContext = new MockContext
      val greeter = Mock.stub[MockSpec.Greeter]
      val _ = greeter.greet("x")

      val _ = intercept[MockExpectationException] {
        (greeter.greet _).verify("x").twice()
      }
    }
  }

  group("call ordering") {

    test("inSequence enforces the declared call order") {
      implicit val ctx: MockContext = new MockContext
      val calc = Mock.mock[MockSpec.Calculator]
      ctx.inSequence {
        val _ = ctx.expecting("add", 1, 1).returning(2)
        val _ = ctx.expecting("negate", 2).returning(-2)
      }

      calc.add(1, 1) ==> 2
      calc.negate(2) ==> -2
      ctx.verifyExpectations()
    }

    test("inSequence rejects an out-of-order call") {
      implicit val ctx: MockContext = new MockContext
      val calc = Mock.mock[MockSpec.Calculator]
      ctx.inSequence {
        val _ = ctx.expecting("add", 1, 1).returning(2)
        val _ = ctx.expecting("negate", 2).returning(-2)
      }

      val _ = intercept[MockExpectationException] {
        calc.negate(2) // calling the second expectation before the first is unexpected
      }
    }

    test("inAnyOrder nested in inSequence relaxes ordering for its block") {
      implicit val ctx: MockContext = new MockContext
      val calc = Mock.mock[MockSpec.Calculator]
      ctx.inSequence {
        val _ = ctx.expecting("add", 0, 0).returning(0)
        ctx.inAnyOrder {
          val _ = ctx.expecting("negate", 1).returning(-1)
          val _ = ctx.expecting("negate", 2).returning(-2)
        }
      }

      calc.add(0, 0) ==> 0
      // the two negate expectations may arrive in either order
      calc.negate(2) ==> -2
      calc.negate(1) ==> -1
      ctx.verifyExpectations()
    }
  }
}

object MockSpec {

  trait Greeter {
    def greet(name: String): String
  }

  trait Calculator {
    def add(a: Int, b: Int): Int
    def negate(a: Int): Int
  }

  trait Sink {
    def accept(value: Double): String
  }
}
