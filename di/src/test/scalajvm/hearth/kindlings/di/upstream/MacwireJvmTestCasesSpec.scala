package hearth.kindlings.di.upstream

import hearth.kindlings.di.*

// Ported from macwire 2.6.7 (Apache License 2.0), tests/src/test/resources/test-cases/: every test case is a test
// body (macwire evaluates each file as a block), `com.softwaremill.macwire._` is replaced by `hearth.kindlings.di._`,
// and a `.failure` case checks the fragments macwire's CompileTests expects in the error.
@scala.annotation.nowarn
final class MacwireJvmTestCasesSpec extends hearth.MacroSuite {

  // macwire's cases declare their classes in a block, so its messages show simple names: drop the prefix of the
  // classes declared in this suite (and `java.lang.`) before looking for macwire's fragments
  private def simpleNames(errors: String): String =
    errors
      .replaceAll("\u001b\\[[0-9;]*m", "")
      .replaceAll("hearth\\.kindlings\\.di\\.upstream\\.[A-Za-z0-9_]+\\.", "")
      .replace("java.lang.", "")

  test("phantomConstructor (does not compile)") {
    val errors: String = compileErrors("""
      /**
        * In this case all constructors are private. However class has implicit member thus it will have 'phntom constructor'.
        * See ConstructorCrimper#isPhantomConstructor for more details.
        */

      trait A
      trait B
      trait C
      trait D

      class Target private (a: A) {

        implicit val d: D = new D {}

        private def this(b: B) = {
          this(new A{})
          throw new UnsupportedOperationException()
        }

        private def this(c: C) = this(new A{})

        @javax.inject.Inject
        private def this(d: D) = this(new A{})
      }

      lazy val a: A = throw new UnsupportedOperationException()
      lazy val b: B = throw new UnsupportedOperationException()
      lazy val c: C = throw new UnsupportedOperationException()
      lazy val d: D = throw new UnsupportedOperationException()

      wire[Target]
    """)
    assert(errors.nonEmpty, "expected a compilation error")
    assert(simpleNames(errors).contains("No public primary constructor found for [Target]"), errors)
    assert(simpleNames(errors).contains("has no apply methods constructing target type"), errors)
  }

  test("secondaryConstructorAnnotated") {
    val _ = MacwireJvmTestCasesSpec.case_secondaryConstructorAnnotated
  }

  test("secondaryConstructorNotAnnotated (does not compile)") {
    val errors: String = compileErrors("""
      class A()

      class B(val s: String, val a: A) {
        def this(a: A) = this("?", a)
      }

      object Test {
        val a: A = wire[A]
        val b: B = wire[B]
      }
    """)
    assert(errors.nonEmpty, "expected a compilation error")
    assert(simpleNames(errors).contains("Cannot find a value of type: [String]"), errors)
  }

  test("toManyInjectAnnotations (does not compile)") {
    val errors: String = compileErrors("""

      trait A
      trait B
      trait C
      trait D

      class Target(a: A) {

        def this(b: B) = {
          this(new A{})
          throw new UnsupportedOperationException()
        }

        @javax.inject.Inject
        def this(c: C) = this(new A{})

        @javax.inject.Inject
        def this(d: D) = this(new A{})
      }

      lazy val a: A = throw new UnsupportedOperationException()
      lazy val b: B = throw new UnsupportedOperationException()
      lazy val c: C = throw new UnsupportedOperationException()
      lazy val d: D = throw new UnsupportedOperationException()

      wire[Target]
    """)
    assert(errors.nonEmpty, "expected a compilation error")
    assert(
      simpleNames(errors).contains(
        "Ambiguous constructors annotated with @javax.inject.Inject or @jakarta.inject.Inject for type [Target]"
      ),
      errors
    )
  }

}

@scala.annotation.nowarn
object MacwireJvmTestCasesSpec {
  object case_secondaryConstructorAnnotated {
    class A()

    class B(val s: String, val a: A) {
      @javax.inject.Inject
      def this(a: A) = this("?", a)
    }

    object Test {
      val a: A = wire[A]
      val b: B = wire[B]
    }

    require(Test.b.s == "?")
    require(Test.b.a == Test.a)
  }

}
