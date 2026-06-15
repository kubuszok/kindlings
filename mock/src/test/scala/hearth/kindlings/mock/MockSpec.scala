package hearth.kindlings.mock

import hearth.MacroSuite

final class MockSpec extends MacroSuite {

  /** Invoke a mock method purely for its side effect, discarding the (possibly non-Unit) result. Keeps Scala 3's
    * `-Werror` happy about discarded values without cluttering every call site with `val _ = ...`.
    */
  private def call(a: => Any): Unit = { val _ = a }

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

  group("strict-mock defaults (P0.1)") {

    test("a strict expectation with no `returning` yields the return type's default, not a ClassCastException") {
      implicit val ctx: MockContext = new MockContext
      val m = Mock.mock[MockSpec.Defaults]
      val _ = ctx.expecting("anInt", 1) // no .returning
      val _ = ctx.expecting("aString", 1)
      val _ = ctx.expecting("aBoolean", 1)
      val _ = ctx.expecting("aDouble", 1)

      m.anInt(1) ==> 0
      m.aString(1) ==> ""
      m.aBoolean(1) ==> false
      m.aDouble(1) ==> 0.0d
      ctx.verifyExpectations()
    }

    test("a strict expectation for a reference return type with no `returning` yields null") {
      implicit val ctx: MockContext = new MockContext
      val m = Mock.mock[MockSpec.Defaults]
      val _ = ctx.expecting("aRef", 1)

      (m.aRef(1): Any) ==> null
      ctx.verifyExpectations()
    }

    test("the faithful DSL with no `returning` also yields the default") {
      import syntax.*
      implicit val ctx: MockContext = new MockContext
      val m = Mock.mock[MockSpec.Defaults]
      val _ = (m.anInt _).expects(7)

      m.anInt(7) ==> 0
      ctx.verifyExpectations()
    }
  }

  group("overloaded methods (P0.2)") {

    test("overloads disambiguated by arity") {
      implicit val ctx: MockContext = new MockContext
      val m = Mock.mock[MockSpec.Overloaded]
      val _ = ctx.expecting("foo", 1).returning("one-arg")
      val _ = ctx.expecting("foo", 1, 2).returning("two-arg")

      m.foo(1) ==> "one-arg"
      m.foo(1, 2) ==> "two-arg"
      ctx.verifyExpectations()
    }

    test("overloads disambiguated by argument type (same arity)") {
      implicit val ctx: MockContext = new MockContext
      val m = Mock.mock[MockSpec.Overloaded]
      val _ = ctx.expecting("bar", 42).returning("int")
      val _ = ctx.expecting("bar", "x").returning("string")

      m.bar(42) ==> "int"
      m.bar("x") ==> "string"
      ctx.verifyExpectations()
    }

    test("faithful DSL pins the overload arity") {
      import syntax.*
      implicit val ctx: MockContext = new MockContext
      val m = Mock.mock[MockSpec.Overloaded]
      // expects with no argument literals still targets the right overload via the method reference's arity.
      val _ = (m.foo(_: Int)).expects(ArgMatcher.any).returning("one-arg")
      val _ = (m.foo(_: Int, _: Int)).expects(ArgMatcher.any, ArgMatcher.any).returning("two-arg")

      m.foo(9) ==> "one-arg"
      m.foo(9, 9) ==> "two-arg"
      ctx.verifyExpectations()
    }
  }

  group("argument matchers") {

    test("any matches any argument including null") {
      implicit val ctx: MockContext = new MockContext
      val greeter = Mock.mock[MockSpec.Greeter]
      val _ = ctx.expecting("greet", ArgMatcher.any).returning("hi").twice()

      greeter.greet("anything") ==> "hi"
      greeter.greet(null) ==> "hi"
      ctx.verifyExpectations()
    }

    test("epsilon matches a numeric argument within tolerance") {
      implicit val ctx: MockContext = new MockContext
      val sink = Mock.mock[MockSpec.Sink]
      val _ = ctx.expecting("accept", ArgMatcher.epsilon(1.0, 0.05)).returning("near-one")

      sink.accept(1.02) ==> "near-one"
      ctx.verifyExpectations()
    }

    test("epsilon over Float/Int/Long and non-match/non-number") {
      ArgMatcher.epsilon(1.0).matches(1.0f) ==> true
      ArgMatcher.epsilon(1.0).matches(1) ==> true
      ArgMatcher.epsilon(1.0).matches(1L) ==> true
      ArgMatcher.epsilon(1.0).matches(1.0001) ==> true
      ArgMatcher.epsilon(1.0).matches(1.1) ==> false
      ArgMatcher.epsilon(1.0).matches("foo") ==> false
    }

    test("argThat matches an argument satisfying a predicate") {
      implicit val ctx: MockContext = new MockContext
      val sink = Mock.mock[MockSpec.Sink]
      val _ = ctx.expecting("accept", ArgMatcher.argThat[Double](_ > 100.0)).returning("big")

      sink.accept(250.0) ==> "big"
      ctx.verifyExpectations()
    }

    test("argThatDescribed carries a description in toString") {
      val m = ArgMatcher.argThatDescribed[Int]("positive")(_ > 0)
      m.matches(5) ==> true
      m.matches(-1) ==> false
      m.toString ==> "positive"
    }

    test("where2 matches over two arguments") {
      implicit val ctx: MockContext = new MockContext
      val calc = Mock.mock[MockSpec.Calculator]
      val _ = ctx.expectingSeq("add", ArgMatcher.where2[Int, Int]((x, y) => x + y > 100)).returning(999)

      calc.add(60, 50) ==> 999
      ctx.verifyExpectations()
    }

    test("custom Matcher via ArgMatcher.from") {
      implicit val ctx: MockContext = new MockContext
      val greeter = Mock.mock[MockSpec.Greeter]
      val startsWithA = new Matcher[String] { def matches(a: String): Boolean = a.startsWith("A") }
      val _ = ctx.expecting("greet", ArgMatcher.from(startsWithA)).returning("matched")

      greeter.greet("Alan") ==> "matched"
      ctx.verifyExpectations()
    }

    test("mixing matchers and literals") {
      implicit val ctx: MockContext = new MockContext
      val calc = Mock.mock[MockSpec.Calculator]
      val _ = ctx.expecting("add", 1, ArgMatcher.any).returning(7)

      calc.add(1, 999) ==> 7
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

    test("CaptureAll accumulates values across calls") {
      implicit val ctx: MockContext = new MockContext
      val greeter = Mock.mock[MockSpec.Greeter]
      val names = new Capture[String]
      val _ = ctx.expecting("greet", ArgMatcher.capture(names)).returning("hi").repeat(3)

      call(greeter.greet("a")); call(greeter.greet("b")); call(greeter.greet("c"))
      names.values ==> Seq("a", "b", "c")
      names.value ==> "c"
      ctx.verifyExpectations()
    }

    test("Capture.value on empty throws NoSuchElementException") {
      val c = new Capture[String]
      c.hasValue ==> false
      val _ = intercept[NoSuchElementException](c.value)
    }
  }

  group("call counts") {

    test("once: too few and too many both fail") {
      implicit val ctx: MockContext = new MockContext
      val g = Mock.mock[MockSpec.Greeter]
      val _ = ctx.expecting("greet", "x").returning("y").once()
      val _ = intercept[MockExpectationException](ctx.verifyExpectations()) // too few (0)
      g.greet("x") ==> "y"
      ctx.verifyExpectations() // exactly one ok
      val _ = intercept[MockExpectationException](g.greet("x")) // too many
    }

    test("twice") {
      implicit val ctx: MockContext = new MockContext
      val g = Mock.mock[MockSpec.Greeter]
      val _ = ctx.expecting("greet", "a").returning("r").twice()
      call(g.greet("a")); call(g.greet("a"))
      ctx.verifyExpectations()
    }

    test("never") {
      implicit val ctx: MockContext = new MockContext
      val g2 = Mock.mock[MockSpec.Greeter]
      val _ = ctx.expecting("greet", "b").returning("r").never()
      ctx.verifyExpectations()
      val _ = intercept[MockExpectationException](g2.greet("b"))
    }

    test("anyNumberOfTimes accepts zero or many") {
      implicit val ctx: MockContext = new MockContext
      val g = Mock.mock[MockSpec.Greeter]
      val _ = ctx.expecting("greet", "any").returning("r").anyNumberOfTimes()
      ctx.verifyExpectations() // zero ok
      call(g.greet("any")); call(g.greet("any")); call(g.greet("any"))
      ctx.verifyExpectations()
    }

    test("atLeastOnce requires at least one") {
      implicit val ctx: MockContext = new MockContext
      val g2 = Mock.mock[MockSpec.Greeter]
      val _ = ctx.expecting("greet", "a").returning("r").atLeastOnce()
      val _ = intercept[MockExpectationException](ctx.verifyExpectations())
      call(g2.greet("a"))
      ctx.verifyExpectations()
    }

    test("noMoreThanOnce") {
      implicit val ctx: MockContext = new MockContext
      val g = Mock.mock[MockSpec.Greeter]
      val _ = ctx.expecting("greet", "x").returning("r").noMoreThanOnce()
      ctx.verifyExpectations() // zero ok
      call(g.greet("x"))
      ctx.verifyExpectations() // one ok
      val _ = intercept[MockExpectationException](g.greet("x")) // two too many
    }

    test("noMoreThanTwice") {
      implicit val ctx: MockContext = new MockContext
      val g2 = Mock.mock[MockSpec.Greeter]
      val _ = ctx.expecting("greet", "y").returning("r").noMoreThanTwice()
      call(g2.greet("y")); call(g2.greet("y"))
      ctx.verifyExpectations()
      val _ = intercept[MockExpectationException](g2.greet("y"))
    }

    test("repeat(n)") {
      implicit val ctx: MockContext = new MockContext
      val g = Mock.mock[MockSpec.Greeter]
      val _ = ctx.expecting("greet", "x").returning("r").repeat(3)
      call(g.greet("x")); call(g.greet("x")); call(g.greet("x"))
      ctx.verifyExpectations()
    }

    test("repeat(range)") {
      implicit val ctx: MockContext = new MockContext
      val g2 = Mock.mock[MockSpec.Greeter]
      val _ = ctx.expecting("greet", "y").returning("r").repeat(1 to 2)
      val _ = intercept[MockExpectationException](ctx.verifyExpectations()) // 0 < 1
      call(g2.greet("y"))
      ctx.verifyExpectations()
      call(g2.greet("y"))
      ctx.verifyExpectations()
      val _ = intercept[MockExpectationException](g2.greet("y")) // 3 > 2
    }

    test("atLeast on a handler") {
      implicit val ctx: MockContext = new MockContext
      val g = Mock.mock[MockSpec.Greeter]
      val _ = ctx.expecting("greet", "x").returning("r").atLeast(2)
      call(g.greet("x"))
      val _ = intercept[MockExpectationException](ctx.verifyExpectations())
      call(g.greet("x"))
      ctx.verifyExpectations()
    }

    test("atMost on a handler") {
      implicit val ctx: MockContext = new MockContext
      val g2 = Mock.mock[MockSpec.Greeter]
      val _ = ctx.expecting("greet", "y").returning("r").atMost(2)
      ctx.verifyExpectations()
      call(g2.greet("y")); call(g2.greet("y"))
      ctx.verifyExpectations()
      val _ = intercept[MockExpectationException](g2.greet("y"))
    }
  }

  group("return / onCall / throw") {

    test("returns alias") {
      implicit val ctx: MockContext = new MockContext
      val g = Mock.mock[MockSpec.Greeter]
      val _ = ctx.expecting("greet", "x").returns("aliased")
      g.greet("x") ==> "aliased"
      ctx.verifyExpectations()
    }

    test("throws alias") {
      implicit val ctx: MockContext = new MockContext
      val g2 = Mock.mock[MockSpec.Greeter]
      val _ = ctx.expecting("greet", "boom").throws(new RuntimeException("kaboom"))
      val ex = intercept[RuntimeException](g2.greet("boom"))
      ex.getMessage ==> "kaboom"
    }

    test("throwing throws the given exception") {
      implicit val ctx: MockContext = new MockContext
      val g = Mock.mock[MockSpec.Greeter]
      val _ = ctx.expecting("greet", "x").throwing(new IllegalStateException("no"))
      val _ = intercept[IllegalStateException](g.greet("x"))
    }

    test("onCall computes the result from the arguments") {
      implicit val ctx: MockContext = new MockContext
      val calc = Mock.mock[MockSpec.Calculator]
      val _ = ctx.expecting("add", ArgMatcher.any, ArgMatcher.any).onCall { args =>
        args(0).asInstanceOf[Int] + args(1).asInstanceOf[Int]
      }
      calc.add(40, 2) ==> 42
      ctx.verifyExpectations()
    }

    test("typed onCall1 / onCall2") {
      implicit val ctx: MockContext = new MockContext
      val calc = Mock.mock[MockSpec.Calculator]
      val _ = ctx.expecting("negate", ArgMatcher.any).onCall1[Int](x => -x)
      val _ = ctx.expecting("add", ArgMatcher.any, ArgMatcher.any).onCall2[Int, Int]((x, y) => x * y)
      calc.negate(8) ==> -8
      calc.add(6, 7) ==> 42
      ctx.verifyExpectations()
    }

    test("stacked same-name expectations are consumed FIFO with counts") {
      implicit val ctx: MockContext = new MockContext
      val g = Mock.mock[MockSpec.Greeter]
      val _ = ctx.expecting("greet", ArgMatcher.any).returning("first").once()
      val _ = ctx.expecting("greet", ArgMatcher.any).returning("second").twice()

      g.greet("a") ==> "first"
      g.greet("b") ==> "second"
      g.greet("c") ==> "second"
      ctx.verifyExpectations()
    }
  }

  group("faithful `.expects` DSL") {
    import syntax.*

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

    test("chained when with counts and fallback (returns alias)") {
      implicit val ctx: MockContext = new MockContext
      val g = Mock.stub[MockSpec.Greeter]
      val _ = ctx.when("greet", ArgMatcher.any).returns("1").twice()
      val _ = ctx.when("greet", ArgMatcher.any).returns("2").once()

      g.greet("a") ==> "1"
      g.greet("b") ==> "1"
      g.greet("c") ==> "2"
      g.greet("d") ==> "" // fallback default after presets exhausted
    }

    test("faithful stub DSL: (s.method _).when(...).returning(...) and (s.method _).verify(...)") {
      import syntax.*
      implicit val ctx: MockContext = new MockContext
      val greeter = Mock.stub[MockSpec.Greeter]
      val _ = (greeter.greet _).when("world").returning("hi")

      greeter.greet("world") ==> "hi"
      greeter.greet("nobody") ==> ""

      (greeter.greet _).verify("world").once()
      (greeter.greet _).verify("nobody").once()
    }

    test("verify fails on param mismatch and wrong count; atLeast/atMost/never") {
      import syntax.*
      implicit val ctx: MockContext = new MockContext
      val greeter = Mock.stub[MockSpec.Greeter]
      val _ = greeter.greet("x")
      val _ = greeter.greet("x")

      (greeter.greet _).verify("x").twice()
      (greeter.greet _).verify("y").never() // never called with "y"
      (greeter.greet _).verify("x").atLeast(1)
      (greeter.greet _).verify("x").atMost(2)
      val _ = intercept[MockExpectationException]((greeter.greet _).verify("x").once())
      val _ = intercept[MockExpectationException]((greeter.greet _).verify("x").atMost(1))
    }

    test("verify-in-sequence asserts recorded call order") {
      implicit val ctx: MockContext = new MockContext
      val calc = Mock.stub[MockSpec.Calculator]
      call(calc.add(1, 1)); call(calc.negate(2)); call(calc.add(3, 3))

      ctx.verifyInSequence("add" -> Seq(1, 1), "negate" -> Seq(2), "add" -> Seq(3, 3))
      // intervening/extra calls are allowed; only relative order of the listed calls matters
      ctx.verifyInSequence("add" -> Seq(1, 1), "add" -> Seq(3, 3))
      // wrong order fails
      val _ = intercept[MockExpectationException](
        ctx.verifyInSequence("negate" -> Seq(2), "add" -> Seq(1, 1))
      )
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

    test("multi-mock sequence under one shared MockContext") {
      implicit val ctx: MockContext = new MockContext
      val a = Mock.mock[MockSpec.Greeter]
      val b = Mock.mock[MockSpec.Calculator]
      ctx.inSequence {
        val _ = ctx.expecting("greet", "x").returning("hi")
        val _ = ctx.expecting("add", 1, 1).returning(2)
      }
      a.greet("x") ==> "hi"
      b.add(1, 1) ==> 2
      ctx.verifyExpectations()
    }

    test("deeply nested inSequence/inAnyOrder: a valid interleaving passes") {
      implicit val ctx: MockContext = new MockContext
      val m = Mock.mock[MockSpec.Logger]
      MockSpec.setupNested(ctx)

      // valid order: 2.1, 1, 2.2.3, 2.2.2.1, 2.2.2.2, 2.2.1, 3, 2.2.3, 2.3
      m.log("2.1"); m.log("1"); m.log("2.2.3"); m.log("2.2.2.1"); m.log("2.2.2.2")
      m.log("2.2.1"); m.log("3"); m.log("2.2.3"); m.log("2.3")
      ctx.verifyExpectations()
    }

    test("deeply nested ordering: an invalid interleaving fails") {
      implicit val ctx: MockContext = new MockContext
      val m = Mock.mock[MockSpec.Logger]
      MockSpec.setupNested(ctx)

      // invalid: 2.3 fired before the inAnyOrder block (which requires 2.2.2.1->2.2.2.2 and 2.2.1) is complete
      m.log("2.1"); m.log("1"); m.log("2.2.1"); m.log("2.2.2.1")
      val _ = intercept[MockExpectationException](m.log("2.3"))
    }

    test("nested ordering: out-of-order within a nested inSequence fails") {
      implicit val ctx: MockContext = new MockContext
      val m = Mock.mock[MockSpec.Logger]
      ctx.inSequence {
        ctx.inAnyOrder {
          val _ = ctx.expecting("log", "1.1").returning(())
          val _ = ctx.expecting("log", "1.2").returning(())
        }
        val _ = ctx.expecting("log", "2").returning(())
      }
      m.log("1.1")
      val _ = intercept[MockExpectationException](m.log("2")) // 2 before 1.2
    }
  }

  group("method shapes (P1)") {

    test("nullary (no parens) and parameterless (empty parens)") {
      implicit val ctx: MockContext = new MockContext
      val m = Mock.mock[MockSpec.Shapes]
      val _ = ctx.expecting("nullary").returning("n")
      val _ = ctx.expecting("noParams").returning("p")
      m.nullary ==> "n"
      m.noParams() ==> "p"
      ctx.verifyExpectations()
    }

    test("curried multi-parameter-list method") {
      implicit val ctx: MockContext = new MockContext
      val m = Mock.mock[MockSpec.Shapes]
      val _ = ctx.expecting("curried", 10, 1.5).returning("c")
      m.curried(10)(1.5) ==> "c"
      ctx.verifyExpectations()
    }

    test("varargs arrive as a Seq") {
      implicit val ctx: MockContext = new MockContext
      val m = Mock.mock[MockSpec.Shapes]
      val _ = ctx.expecting("repeatedParam", 42, Seq("foo", "bar")).returning("v")
      m.repeatedParam(42, "foo", "bar") ==> "v"
      ctx.verifyExpectations()
    }

    test("by-name argument is forced exactly once") {
      implicit val ctx: MockContext = new MockContext
      val m = Mock.mock[MockSpec.Shapes]
      var forced = 0
      val _ = ctx.expecting("byName", 99).returning("b")
      m.byName { forced += 1; 99 } ==> "b"
      forced ==> 1
      ctx.verifyExpectations()
    }

    // deferred: AnonymousInstance does not emit an override for operator/symbolic-named abstract methods (`def +(x)`),
    // so the synthesized subtype reports "Missing implementation for member ... def +". Probed in isolation (a trait
    // with only `def +(x: Int): Int`) — same failure, so it is a Hearth gap, not our packing. Re-enable once Hearth
    // overrides symbolic members.
    test("operator / symbolic method name".ignore) {
      // elided: `Mock.mock[MockSpec.Ops]` does not compile (missing override for `+`).
      ()
    }

    // deferred: methods carrying their OWN type parameters (`def describe[T](x: T): String`) cannot be mocked yet.
    // Hearth's AnonymousInstance does not type-apply such methods, so `OverrideContext.returnType` falls back to the
    // erased `Any` (via `knownReturning.getOrElse(Any)`) even when the declared return type is concrete (`String`).
    // The generated body is then typed `Any` and fails to satisfy the override's `String` return ("found Any, required
    // String"). Needs Hearth to surface the (re-bound) per-override type parameters and resolved return type to
    // OverrideContext. Non-generic methods of every other shape mock fine.
    test("polymorphic method (deferred — see comment)".ignore) {
      // Body intentionally elided: `Mock.mock[MockSpec.PolyConcrete]` does not even COMPILE (the macro expands at
      // compile time regardless of `.ignore`). Kept as a documented placeholder until Hearth resolves the gap above.
      ()
    }

    test("many parameters (>= 9)") {
      implicit val ctx: MockContext = new MockContext
      val m = Mock.mock[MockSpec.ManyParams]
      val _ = ctx.expecting("sum", 1, 2, 3, 4, 5, 6, 7, 8, 9).returning(45)
      m.sum(1, 2, 3, 4, 5, 6, 7, 8, 9) ==> 45
      ctx.verifyExpectations()
    }

    test("Function1 as a mocked trait") {
      implicit val ctx: MockContext = new MockContext
      val f = Mock.mock[Int => Boolean]
      val _ = ctx.expecting("apply", 1).returning(true)
      f(1) ==> true
      ctx.verifyExpectations()
    }

    test("concrete def and val are preserved (not intercepted)") {
      implicit val ctx: MockContext = new MockContext
      val m = Mock.mock[MockSpec.WithConcrete]
      m.concreteDef(3) ==> 9
      m.concreteVal ==> "concrete"
    }

    // deferred: AnonymousInstance overrides every member with a `def` (DefDef), which cannot override an abstract
    // `val abstractVal: String` ("stable, immutable value required to override"). Needs Hearth to emit a `val` override
    // for abstract vals. Probed in isolation — same failure.
    test("abstract val returns default".ignore) {
      // elided: `Mock.mock[MockSpec.WithAbstractVal]` does not compile (def cannot override abstract val).
      ()
    }
  }

  group("default-args and implicit/using params") {

    test("default argument parameter") {
      implicit val ctx: MockContext = new MockContext
      val m = Mock.mock[MockSpec.WithDefaults]
      val _ = ctx.expecting("greet", "world", "Hello").returning("Hello, world")
      m.greet("world") ==> "Hello, world"
      ctx.verifyExpectations()
    }

    // deferred: AnonymousInstance emits override parameter clauses as plain (explicit) params — the implicit clause of
    // `def run(cmd: String)(implicit cfg: Int)` is generated as a second EXPLICIT clause, so the abstract `run` is left
    // unimplemented ("Missing implementation for member ... def run(cmd)(implicit cfg)"). Probed in isolation — same
    // failure. Needs Hearth to preserve the `implicit`/`using` flag on override parameter clauses.
    test("implicit parameter is packed like a normal argument".ignore) {
      // elided: `Mock.mock[MockSpec.WithImplicit]` does not compile (implicit clause not preserved on the override).
      ()
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

  trait Logger {
    def log(msg: String): Unit
  }

  trait Defaults {
    def anInt(x: Int): Int
    def aString(x: Int): String
    def aBoolean(x: Int): Boolean
    def aDouble(x: Int): Double
    def aRef(x: Int): java.io.Serializable
  }

  trait Overloaded {
    def foo(x: Int): String
    def foo(x: Int, y: Int): String
    def bar(x: Int): String
    def bar(x: String): String
  }

  trait Shapes {
    def nullary: String
    def noParams(): String
    def curried(x: Int)(y: Double): String
    def repeatedParam(x: Int, ys: String*): String
    def byName(x: => Int): String
  }

  trait Ops {
    def +(x: Int): Int
  }

  trait PolyConcrete {
    def describe[T](x: T): String
  }

  trait ManyParams {
    def sum(a: Int, b: Int, c: Int, d: Int, e: Int, f: Int, g: Int, h: Int, i: Int): Int
  }

  trait WithConcrete {
    def concreteDef(x: Int): Int = x * x
    val concreteVal: String = "concrete"
    def needsImpl(x: Int): Int
  }

  trait WithAbstractVal {
    val abstractVal: String
  }

  trait WithDefaults {
    def greet(name: String, prefix: String = "Hello"): String
  }

  trait WithImplicit {
    def run(cmd: String)(implicit cfg: Int): String
  }

  /** Builds the nested-ordering tree from ScalaMock's NestedExpectationsTest. */
  def setupNested(ctx: MockContext): Unit = {
    val _ = ctx.expecting("log", "1").returning(())
    ctx.inSequence {
      val _ = ctx.expecting("log", "2.1").returning(())
      ctx.inAnyOrder {
        val _ = ctx.expecting("log", "2.2.1").returning(())
        ctx.inSequence {
          val _ = ctx.expecting("log", "2.2.2.1").returning(())
          val _ = ctx.expecting("log", "2.2.2.2").returning(())
        }
        val _ = ctx.expecting("log", "2.2.3").returning(()).anyNumberOfTimes()
      }
      val _ = ctx.expecting("log", "2.3").returning(())
    }
    val _ = ctx.expecting("log", "3").returning(())
  }
}
