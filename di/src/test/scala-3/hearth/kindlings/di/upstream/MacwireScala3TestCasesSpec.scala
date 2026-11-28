package hearth.kindlings.di.upstream

import hearth.kindlings.di.*

// Ported from macwire 2.6.7 (Apache License 2.0), tests/src/test/resources/test-cases/: every test case is a test
// body (macwire evaluates each file as a block), `com.softwaremill.macwire._` is replaced by `hearth.kindlings.di._`,
// and a `.failure` case checks the fragments macwire's CompileTests expects in the error.
@scala.annotation.nowarn
final class MacwireScala3TestCasesSpec extends hearth.MacroSuite {

  // macwire's cases declare their classes in a block, so its messages show simple names: drop the prefix of the
  // classes declared in this suite (and `java.lang.`) before looking for macwire's fragments
  private def simpleNames(errors: String): String =
    errors
      .replaceAll("\u001b\\[[0-9;]*m", "")
      .replaceAll("hearth\\.kindlings\\.di\\.upstream\\.[A-Za-z0-9_]+\\.", "")
      .replace("java.lang.", "")

  test("functionApplication (documented difference: parameters of enclosing lambdas are not candidates)") {
    val errors: String = compileErrors("""

      case class A()
      case class B(a: A)

      object Test {

        val someA = Some(A())

        def build(opt: Option[A]): Option[B] = {
          opt.map { a => wire[B] }
        }

        // equivalent with a for
        def forBuild(opt: Option[A]): Option[B] = {
          for (a <- opt) yield {
            wire[B]
          }
        }

        lazy val ab: Option[(A,B)] = Some(A(),B(A()))
      }

      require(Test.build(Test.someA).get.a eq Test.someA.get)
      require(Test.forBuild(Test.someA).get.a eq Test.someA.get)
    """)
    assert(errors.nonEmpty, "this macwire case now compiles: move it back to the supported cases")
  }

  test("implicitDepsWiredWithImplicitValsFromMethodScope") {
    MacwireScala3TestCasesSpec.case_implicitDepsWiredWithImplicitValsFromMethodScope()
  }

  test("methodWithSingleImplicitParamOk") {
    MacwireScala3TestCasesSpec.case_methodWithSingleImplicitParamOk()
  }

  test("t103 (documented difference: local vals are not candidates on Scala 3)") {
    val errors: String = compileErrors("""
      class Test1(val s: Set[String])

      val set1: Set[String] = Set()
      val t1: Test1 = wire[Test1]

      require(t1.s eq Set[String]())

      class Test2(val list: List[String])
      val list2 = List[String]()
      val t2: Test2 = wire[Test2]

      require(t2.list eq List[String]())
    """)
    assert(errors.nonEmpty, "this macwire case now compiles: move it back to the supported cases")
  }

  test("wireList (documented difference: imports)") {
    MacwireScala3TestCasesSpec.case_wireList__documented_difference__imports_()
  }

  test("wireWithImplicits (documented difference: local vals are not candidates on Scala 3)") {
    val errors: String = compileErrors("""
      case class A(n: Int, s: String)
      object A {
          def make(n: Int)(implicit s: String): A =
              A(n, s)
      }

      lazy val n = 0
      implicit lazy val s: String = "s"

      wireWith(A.make _)
    """)
    assert(errors.nonEmpty, "this macwire case now compiles: move it back to the supported cases")
  }

}

@scala.annotation.nowarn
object MacwireScala3TestCasesSpec {
  def case_implicitDepsWiredWithImplicitValsFromMethodScope(): Unit = {
    case class Dependency()
    case class Service()(implicit val dependency: Dependency)

    object MethodScope {
      def someMethod(dependency: Dependency)(implicit implicitDependency: Dependency): Unit = {
        val service: Service = wire[Service]
        require(service.dependency eq implicitDependency)
      }
    }

    implicit val someImplicit: Dependency = new Dependency

    MethodScope.someMethod(new Dependency)
    ()
  }

  def case_methodWithSingleImplicitParamOk(): Unit = {
    case class Dependency()
    case class Service()(implicit val dependency: Dependency)

    object Module {
      def someMethod(implicit implicitDependency: Dependency): Service = wire[Service]
    }
    implicit val dependency: Dependency = new Dependency()

    val service = Module.someMethod

    require(service.dependency eq dependency)
    ()
  }

  def case_wireList__documented_difference__imports_(): Unit = {
    trait A
    class A1 extends A
    class A2 extends A
    class A3 extends A
    class A4 extends A

    case class Group(as: List[A])

    class Plugin1 {
      lazy val a1: A = wire[A1]
    }

    class Plugin2 {
      lazy val a2: A = wire[A2]
    }

    trait Module3 {
      lazy val a3: A = wire[A3]
    }

    class App(plugin1: Plugin1, plugin2: Plugin2) extends Module3 {
      import plugin1.*
      import plugin2.*

      lazy val a4: A = wire[A4]

      // wireList looks at the same places as wireSet:
      // - enclosing members
      // - enclosing imports
      // - parents
      // but preserves the order of definition
      lazy val as: List[A] = wireList[A]

      // inject that List of A into Group
      lazy val group: Group = wire[Group]
    }

    val plugin1 = new Plugin1
    val plugin2 = new Plugin2
    val app = new App(plugin1, plugin2)

    // kindlings: members brought in by an import (`import plugin1._`) are not candidates, and the members declared
    // in the class come before the inherited ones (macwire: inherited first) (documented differences)
    require(app.group.as == List(app.a4, app.a3))
    ()
  }

}
