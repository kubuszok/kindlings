package hearth.kindlings.di.upstream

import hearth.kindlings.di.*

// Ported from macwire 2.6.7 (Apache License 2.0), tests/src/test/resources/test-cases/autowire/: every test case is a test
// body (macwire evaluates each file as a block), `com.softwaremill.macwire._` is replaced by `hearth.kindlings.di._`,
// and a `.failure` case checks the fragments macwire's CompileTests expects in the error.
@scala.annotation.nowarn
final class MacwireAutowireTestCasesSpec extends hearth.MacroSuite {

  // macwire's cases declare their classes in a block, so its messages show simple names: drop the prefix of the
  // classes declared in this suite (and `java.lang.`) before looking for macwire's fragments
  private def simpleNames(errors: String): String =
    errors
      .replaceAll("\u001b\\[[0-9;]*m", "")
      .replaceAll("hearth\\.kindlings\\.di\\.upstream\\.[A-Za-z0-9_]+\\.", "")
      .replace("java.lang.", "")

  test("autowire/autowireMembersOf") {
    MacwireAutowireTestCasesSpec.case_autowire_autowireMembersOf()
  }

  test("autowire/autowireMembersOfDuplicate (does not compile)") {
    val errors: String = compileErrors("""
      class A
      class B
      class Deps(val a: A, b: B) {
        def bb: B = b
        def cc(z: Int): A = a
      }
      case class C(a: A, b: B)

      val d = new Deps(new A, new B)
      autowire[C](autowireMembersOf(d), new A)
    """)
    assert(errors.nonEmpty, "expected a compilation error")
    assert(simpleNames(errors).contains("duplicate type in dependencies list: A, for: new A"), errors)
  }

  test("autowire/classOf") {
    MacwireAutowireTestCasesSpec.case_autowire_classOf()
  }

  test("autowire/cyclicDependency (does not compile)") {
    val errors: String = compileErrors("""
      case class A(b: B)
      case class B(a: A)

      autowire[A]()
    """)
    assert(errors.nonEmpty, "expected a compilation error")
    assert(simpleNames(errors).contains("cyclic dependencies detected;"), errors)
    assert(simpleNames(errors).contains("wiring path: A -> B -> A"), errors)
  }

  test("autowire/duplicate (does not compile)") {
    val errors: String = compileErrors("""
      case class A()
      case class B(a: A)

      val a = A()
      autowire[B](a, a)
    """)
    assert(errors.nonEmpty, "expected a compilation error")
    assert(simpleNames(errors).contains("duplicate type in dependencies list: A, for: a"), errors)
  }

  test("autowire/function1fromMethod") {
    MacwireAutowireTestCasesSpec.case_autowire_function1fromMethod()
  }

  test("autowire/function2") {
    MacwireAutowireTestCasesSpec.case_autowire_function2()
  }

  test("autowire/function2constructorAsFunction") {
    MacwireAutowireTestCasesSpec.case_autowire_function2constructorAsFunction()
  }

  test("autowire/function2fromMethod") {
    MacwireAutowireTestCasesSpec.case_autowire_function2fromMethod()
  }

  test("autowire/plainTrait (does not compile)") {
    val errors: String = compileErrors("""
      trait A
      case class B(a: A)

      autowire[B]()
    """)
    assert(errors.nonEmpty, "expected a compilation error")
    assert(
      simpleNames(errors).contains(
        "cannot find a provided dependency, public constructor or public apply method for: A;"
      ),
      errors
    )
    assert(simpleNames(errors).contains("wiring path: B -> A"), errors)
  }

  test("autowire/primitives (does not compile)") {
    val errors: String = compileErrors("""
      class A(s: String)

      autowire[A]("xyz")
    """)
    assert(errors.nonEmpty, "expected a compilation error")
    assert(simpleNames(errors).contains("cannot use a primitive type or String in autowiring;"), errors)
    assert(simpleNames(errors).contains("wiring path: A -> String"), errors)
  }

  test("autowire/privateConstructor (does not compile)") {
    val errors: String = compileErrors("""
      case class A private ()

      autowire[A]()
    """)
    assert(errors.nonEmpty, "expected a compilation error")
    assert(
      simpleNames(errors).contains(
        "cannot find a provided dependency, public constructor or public apply method for: A;"
      ),
      errors
    )
    assert(simpleNames(errors).contains("wiring path: A"), errors)
  }

  test("autowire/simpleApply") {
    val _ = MacwireAutowireTestCasesSpec.case_autowire_simpleApply
  }

  test("autowire/simpleNoDependencies") {
    MacwireAutowireTestCasesSpec.case_autowire_simpleNoDependencies()
  }

  test("autowire/simpleOneDependency") {
    MacwireAutowireTestCasesSpec.case_autowire_simpleOneDependency()
  }

  test("autowire/simpleReuseDependency") {
    MacwireAutowireTestCasesSpec.case_autowire_simpleReuseDependency()
  }

  test("autowire/simpleReuseWired") {
    MacwireAutowireTestCasesSpec.case_autowire_simpleReuseWired()
  }

  test("autowire/simpleTwoDependencies") {
    MacwireAutowireTestCasesSpec.case_autowire_simpleTwoDependencies()
  }

  test("autowire/unusedDependency (does not compile)") {
    val errors: String = compileErrors("""
      case class A()
      case class B(a: A)
      case class C()

      autowire[B](C())
    """)
    assert(errors.nonEmpty, "expected a compilation error")
    assert(simpleNames(errors).contains("unused dependencies: C()"), errors)
  }

  test("autowire/useGivens") {
    MacwireAutowireTestCasesSpec.case_autowire_useGivens()
  }

}

@scala.annotation.nowarn
object MacwireAutowireTestCasesSpec {
  def case_autowire_autowireMembersOf(): Unit = {
    trait A
    trait B
    class Deps(val a: A, b: B) {
      def bb: B = b
      def cc(z: Int): A = a
    }
    case class C(a: A, b: B)

    val d = new Deps(new A {}, new B {})
    val c = autowire[C](autowireMembersOf(d))

    require(c.a eq d.a)
    require(c.b eq d.bb)
    ()
  }

  def case_autowire_classOf(): Unit = {
    trait A
    case class AImpl() extends A
    case class B(a: A)

    val b = autowire[B](classOf[AImpl])
    require(b.a.isInstanceOf[AImpl])
    ()
  }

  def case_autowire_function1fromMethod(): Unit = {
    case class C()
    trait A
    case class AImpl(c: C) extends A
    case class B(a: A)

    def createA(c: C): A = AImpl(c)

    val b = autowire[B](createA)
    require(b.a.isInstanceOf[AImpl])
    ()
  }

  def case_autowire_function2(): Unit = {
    case class C()
    case class D()
    trait A
    case class AImpl(c: C, d: D) extends A
    case class B(a: A)

    val createA = (c: C, d: D) => AImpl(c, d)

    val b = autowire[B](createA)
    require(b.a.isInstanceOf[AImpl])
    ()
  }

  def case_autowire_function2constructorAsFunction(): Unit = {
    case class C()
    case class D()
    trait A
    case class AImpl(c: C, d: D) extends A
    case class B(a: A)

    val b = autowire[B](AImpl(_, _))
    require(b.a.isInstanceOf[AImpl])
    ()
  }

  def case_autowire_function2fromMethod(): Unit = {
    case class C()
    case class D()
    trait A
    case class AImpl(c: C, d: D) extends A
    case class B(a: A)

    def createA(c: C, d: D): A = AImpl(c, d)

    val b = autowire[B](createA)
    require(b.a.isInstanceOf[AImpl])
    ()
  }

  object case_autowire_simpleApply {
    case class B()
    case class A private (b: B)
    object A {
      def apply(b: B): A = new A(b)

    }
    autowire[A]()
  }

  def case_autowire_simpleNoDependencies(): Unit = {
    case class A()
    case class B()
    case class C(a: A, b: B)
    case class D(c: C)

    val d = autowire[D]()

    require(d != null)
    require(d.c != null)
    require(d.c.a != null)
    require(d.c.b != null)
    ()
  }

  def case_autowire_simpleOneDependency(): Unit = {
    case class A()
    case class B()
    case class C(a: A, b: B)
    case class D(c: C)

    val a = A()
    val d = autowire[D](a)

    require(d.c.a eq a)
    ()
  }

  def case_autowire_simpleReuseDependency(): Unit = {
    case class A()
    case class B(a: A)
    case class C(a: A)
    case class D(b: B, c: C)

    val a = A()
    val d = autowire[D](a)

    require(d.b.a eq a)
    require(d.c.a eq a)
    ()
  }

  def case_autowire_simpleReuseWired(): Unit = {
    case class A()
    case class B(a: A)
    case class C(a: A)
    case class D(b: B, c: C)

    val d = autowire[D]()

    require(d.b.a eq d.c.a)
    ()
  }

  def case_autowire_simpleTwoDependencies(): Unit = {
    case class A()
    case class B()
    case class C(a: A, b: B)
    case class D(c: C)

    val a = A()
    val b = B()
    val d = autowire[D](a, b)

    require(d.c.a eq a)
    require(d.c.b eq b)
    ()
  }

  def case_autowire_useGivens(): Unit = {
    case class A1()
    case class A2()
    case class B1()
    case class B2()
    case class C(b1: B1, b2: B2)(using A1, A2) {
      def a1 = summon[A1]

    }
    {
      given A1 = A1()
      given A2 = A2()
      val c = autowire[C]()
      require(c.a1 eq summon[A1])
    }
    ()
  }

}
