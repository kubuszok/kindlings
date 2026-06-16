package hearth.kindlings.mock

import hearth.MacroSuite

/** Scala-3-only mock shapes: union / intersection parameter and return types, `this.type` returns, opaque parameter
  * types, and context-function returns. These use syntax that only exists on Scala 3, so they legitimately live under
  * `src/test/scala-3/` (not a way to hide cross-platform failures).
  */
final class MockScala3Spec extends MacroSuite {

  group("union & intersection types") {

    test("union parameter type") {
      given ctx: MockContext = new MockContext
      val m = Mock.mock[MockScala3Spec.UnionParam]
      val _ = ctx.expecting("handle", 1).returning("int")
      val _ = ctx.expecting("handle", "x").returning("string")
      m.handle(1) ==> "int"
      m.handle("x") ==> "string"
      ctx.verifyExpectations()
    }

    test("union return type") {
      given ctx: MockContext = new MockContext
      val m = Mock.mock[MockScala3Spec.UnionReturn]
      val _ = ctx.expecting("pick", true).returning(42)
      val _ = ctx.expecting("pick", false).returning("no")
      (m.pick(true): Int | String) ==> 42
      (m.pick(false): Int | String) ==> "no"
      ctx.verifyExpectations()
    }

    test("intersection parameter type") {
      given ctx: MockContext = new MockContext
      val m = Mock.mock[MockScala3Spec.IntersectionParam]
      val both = new MockScala3Spec.A with MockScala3Spec.B {}
      val _ = ctx.expecting("use", both).returning("ok")
      m.use(both) ==> "ok"
      ctx.verifyExpectations()
    }

    test("intersection return type") {
      given ctx: MockContext = new MockContext
      val m = Mock.mock[MockScala3Spec.IntersectionReturn]
      val both = new MockScala3Spec.A with MockScala3Spec.B {}
      val _ = ctx.expecting("make").returning(both)
      (m.make: MockScala3Spec.A & MockScala3Spec.B) ==> both
      ctx.verifyExpectations()
    }
  }

  // NOTE: the `this.type` return shape is now covered in the shared `MockSpec` — Hearth 0.3.1-48 fixed it on BOTH
  // platforms (it was broken on Scala 2 too, not only Scala 3), so it no longer belongs in a Scala-3-only spec.

  group("opaque parameter type") {

    test("an opaque-typed parameter is matched by its underlying value") {
      given ctx: MockContext = new MockContext
      val m = Mock.mock[MockScala3Spec.OpaqueParam]
      val id = MockScala3Spec.UserId(7)
      val _ = ctx.expecting("lookup", id).returning("user-7")
      m.lookup(id) ==> "user-7"
      ctx.verifyExpectations()
    }
  }

  group("context-function return type") {

    test("a method returning a context function") {
      // Resolved in Hearth 0.3.1-49: the context-function return (`Config ?=> String`) keeps its `ContextFunctionN`
      // shape on Scala 3.8.4 (previously `safeMemberType` desugared it into a trailing `using` clause → a String
      // reached a Function1 position). No mock-side change was needed.
      given ctx: MockContext = new MockContext
      val m = Mock.mock[MockScala3Spec.CtxReturn]
      // No `given Config` anywhere in this scope — `cfg` is a plain val applied EXPLICITLY at the call site. This keeps
      // the context-function value un-applied through `returning(...)` (Scala 3 only eta-applies a context function
      // when a matching `given` is in scope), so the mock genuinely round-trips a `Config ?=> String`.
      val cfg = new MockScala3Spec.Config {}
      val _ = ctx.expecting("build", "k").returning(MockScala3Spec.buildResult)
      m.build("k")(using cfg) ==> "built"
      ctx.verifyExpectations()
    }
  }
}

object MockScala3Spec {

  trait UnionParam {
    def handle(x: Int | String): String
  }

  trait UnionReturn {
    def pick(b: Boolean): Int | String
  }

  trait A
  trait B

  trait IntersectionParam {
    def use(x: A & B): String
  }

  trait IntersectionReturn {
    def make: A & B
  }

  opaque type UserId = Int
  object UserId {
    def apply(i: Int): UserId = i
  }

  trait OpaqueParam {
    def lookup(id: UserId): String
  }

  trait Config

  trait CtxReturn {
    def build(key: String): Config ?=> String
  }

  // A plain `Function1` used as the mocked context-function result. A `val: Config ?=> String` cannot be referenced
  // without a `given Config` in scope (every reference eta-applies it); a regular `Config => String` is stored as-is
  // and is the same `scala.Function1` at runtime, so the `build` override casts it to `Config ?=> String` cleanly.
  val buildResult: Config => String = (_: Config) => "built"
}
