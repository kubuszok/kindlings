package hearth.kindlings.di.upstream

import hearth.kindlings.di.*

// Ported from macwire 2.6.7 (Apache License 2.0), tests/src/test/resources/test-cases/: every test case is a test
// body (macwire evaluates each file as a block), `com.softwaremill.macwire._` is replaced by `hearth.kindlings.di._`,
// and a `.failure` case checks the fragments macwire's CompileTests expects in the error.
@scala.annotation.nowarn
final class MacwireScala2TestCasesSpec extends hearth.MacroSuite {

  // macwire's cases declare their classes in a block, so its messages show simple names: drop the prefix of the
  // classes declared in this suite (and `java.lang.`) before looking for macwire's fragments
  private def simpleNames(errors: String): String =
    errors.replaceAll("\u001b\\[[0-9;]*m", "").replaceAll("hearth\\.kindlings\\.di\\.upstream\\.[A-Za-z0-9_]+\\.", "").replace("java.lang.", "")

  test("callByNameWireWith (documented difference: wireWith of a method with a by-name parameter, eta-expanded on Scala 2)") {
    val errors: String = compileErrors("""
      case class A()
      case class B()
      case class C(a: A, b: B)

      // not always used
      case class D(c: C)
      case class E(a: A, c: C)

      import scala.collection.mutable.ListBuffer

      object Test {
        val created = ListBuffer.empty[String]

        class Component private (val a: A)


        object Component {
          def create(dependency: => A) = { created.append("a"); new Component(dependency) }
        }

        val dependency: A = wire[A]
        val component: Component = wireWith(Component.create _)
      }

      require(Test.component.a != null)
      require(Test.created.size == 1)
      require(Test.created.head == "a")
    """)
    assert(errors.nonEmpty, "this macwire case now compiles: move it back to the supported cases")
  }

  test("forwardReferenceInBlock") {
    val _ = MacwireScala2TestCasesSpec.case_forwardReferenceInBlock
  }

  test("functionApplication (documented difference: parameters of enclosing lambdas are not candidates)") {
    val errors: String = compileErrors("""

      case class A()
      case class B(a: A)
      case class C(a: A, b: B)

      object Test {

        val someA = Some(A())

        def build(opt: Option[A]): Option[B] = {
          opt.map { a => wire[B] }
        }

        def buildSimpleCase(opt: Option[A]): Option[B] = {
          opt.map { case a => wire[B] }
        }

        // equivalent with a for
        def forBuild(opt: Option[A]): Option[B] = {
          for (a <- opt) yield {
            wire[B]
          }
        }

        def buildDeconstructingCase: Option[C] = {
          ab.map { case (a,b) => wire[C] }
        }

        // equivalent with a for
        def forBuildDeconstructingCase: Option[C] = {
          for ((a,b) <- ab) yield {
            wire[C]
          }
        }

        lazy val ab: Option[(A,B)] = Some(A(),B(A()))
      }

      require(Test.build(Test.someA).get.a eq Test.someA.get)
      require(Test.buildSimpleCase(Test.someA).get.a eq Test.someA.get)
      require(Test.buildDeconstructingCase.get.a eq Test.ab.get._1)
      require(Test.buildDeconstructingCase.get.b eq Test.ab.get._2)

      require(Test.forBuild(Test.someA).get.a eq Test.someA.get)
      require(Test.forBuildDeconstructingCase.get.a eq Test.ab.get._1)
      require(Test.forBuildDeconstructingCase.get.b eq Test.ab.get._2)
    """)
    assert(errors.nonEmpty, "this macwire case now compiles: move it back to the supported cases")
  }

  test("implicitDepsWiredWithImplicitValsFromMethodScope") {
    MacwireScala2TestCasesSpec.case_implicitDepsWiredWithImplicitValsFromMethodScope()
  }

  test("import (documented difference: members brought in by an import are not candidates)") {
    val errors: String = compileErrors("""
      case class A()
      case class B()
      case class C(a: A, b: B)

      // not always used
      case class D(c: C)
      case class E(a: A, c: C)

      class Base {
          lazy val theA: A = wire[A]
          lazy val theB: B = wire[B]
      }

      class Main(val base: Base) {
          import base.{theA}

          // not ambiguous as `base.theB` hasn't been imported
          lazy val myB: B = wire[B]

          lazy val theC: C = wire[C]
      }

      val main = new Main(new Base)

      require(main.theC.a eq main.base.theA)
      require(main.theC.b eq main.myB)
    """)
    assert(errors.nonEmpty, "this macwire case now compiles: move it back to the supported cases")
  }

  test("importAmbiguous (documented difference: members brought in by an import are not candidates) (does not compile)") {
    val errors: String = compileErrors("""
      case class A()
      case class B()
      case class C(a: A, b: B)

      // not always used
      case class D(c: C)
      case class E(a: A, c: C)

      class Base {
          lazy val theA: A = wire[A]
          lazy val theB: B = wire[B]
      }

      class Main(val base: Base) {
          import base._

          lazy val myA: A = wire[A]

          // error: both `myA` and `theA` are matching
          lazy val theC: C = wire[C]
      }

      val main = new Main(new Base)

      require(main.theC.a eq main.base.theA)
      require(main.theC.b eq main.base.theB)
    """)
    assert(errors.nonEmpty, "expected a compilation error")
  }

  test("importImplicit (documented difference: members brought in by an import are not candidates)") {
    val errors: String = compileErrors("""
      case class A()
      case class B(a: A)

      object Import {
          implicit val a: A = A()
      }

      object Test {
          import Import.a

          lazy val b: B = wire[B]
      }

      require(Test.b.a eq Import.a)
    """)
    assert(errors.nonEmpty, "this macwire case now compiles: move it back to the supported cases")
  }

  test("importPackage") {
    val _ = MacwireScala2TestCasesSpec.case_importPackage
  }

  test("importRename (documented difference: members brought in by an import are not candidates)") {
    val errors: String = compileErrors("""
      case class A()
      case class B()
      case class C(a: A, b: B)

      // not always used
      case class D(c: C)
      case class E(a: A, c: C)

      class Base {
          lazy val theA: A = wire[A]
          lazy val theB: B = wire[B]
      }

      class Main(val base: Base) {
          import base.{theA => blabla, theB => blibli}

          lazy val theC: C = wire[C]
      }

      val main = new Main(new Base)

      require(main.theC.a eq main.base.theA)
      require(main.theC.b eq main.base.theB)
    """)
    assert(errors.nonEmpty, "this macwire case now compiles: move it back to the supported cases")
  }

  test("importWildcard (documented difference: members brought in by an import are not candidates)") {
    val errors: String = compileErrors("""
      case class A()
      case class B()
      case class C(a: A, b: B)

      // not always used
      case class D(c: C)
      case class E(a: A, c: C)

      class Base {
          lazy val theA: A = wire[A]
          lazy val theB: B = wire[B]
      }

      class Main(val base: Base) {
          import base._

          lazy val theC: C = wire[C]
      }

      val main = new Main(new Base)

      require(main.theC.a eq main.base.theA)
      require(main.theC.b eq main.base.theB)
    """)
    assert(errors.nonEmpty, "this macwire case now compiles: move it back to the supported cases")
  }

  test("importWildcardDoNotImportObjectMethods (documented difference: members brought in by an import are not candidates)") {
    val errors: String = compileErrors("""
      case class A()
      case class B()
      case class C(a: A, b: B)

      // not always used
      case class D(c: C)
      case class E(a: A, c: C)

      class Base {
          lazy val theA: A = wire[A]
      }

      case class Stuff(a: A, i: Int, str: String)

      class Main(val base: Base) {
          // this test makes sure that we don't consider
          // object methods such as hashCode or toString
          import base._

          lazy val someInt = 42
          lazy val someString = "toto"

          lazy val stuff: Stuff = wire[Stuff]
      }

      val main = new Main(new Base)

      require(main.stuff.a eq main.base.theA)
      require(main.stuff.i == main.someInt)
      require(main.stuff.str == main.someString)
    """)
    assert(errors.nonEmpty, "this macwire case now compiles: move it back to the supported cases")
  }

  test("importWildcardVisibility (documented difference: members brought in by an import are not candidates)") {
    val errors: String = compileErrors("""
      case class A()
      case class B()
      case class C(a: A, b: B)

      // not always used
      case class D(c: C)
      case class E(a: A, c: C)

      class Base {
          private lazy val theA: A = wire[A]
          lazy val theB: B = wire[B]
      }

      class Main(val base: Base) {
          import base._

          lazy val myA: A = wire[A]

          // if visibility isn't taken into account we'll end up with an ambiguous error:
          // macwire we'll find both `base.theA` and `this.myA` to be eligible
          lazy val theC: C = wire[C]
      }

      val main = new Main(new Base)

      require(main.theC.a eq main.myA)
      require(main.theC.b eq main.base.theB)
    """)
    assert(errors.nonEmpty, "this macwire case now compiles: move it back to the supported cases")
  }

  test("inheritanceHKT") {
    val _ = MacwireScala2TestCasesSpec.case_inheritanceHKT
  }

  test("inheritanceWireWith") {
    val _ = MacwireScala2TestCasesSpec.case_inheritanceWireWith
  }

  test("methodContainingValDef (documented difference: local vals of a nested block are not candidates)") {
    val errors: String = compileErrors("""
      trait A
      case class A1() extends A
      case class A2() extends A
      case class A3() extends A
      case class B(a: A)

      object Test {
         def apply(i: Int) : B = {
            if( i > 1 ) {
               if( i > 2 ) {
                 val a = A3()
                 wire[B]
               } else {
                 val a = A2()
                 wire[B]
               }
            } else {
               val a = A1()
               wire[B]
            }
         }
      }

      require(Test(1).a == A1())
      require(Test(2).a == A2())
      require(Test(3).a == A3())
    """)
    assert(errors.nonEmpty, "this macwire case now compiles: move it back to the supported cases")
  }

  test("methodWithSingleImplicitParamOk") {
    MacwireScala2TestCasesSpec.case_methodWithSingleImplicitParamOk()
  }

  test("methodWithWiredWithinPatternMatch (documented difference: values bound by a pattern are not candidates)") {
    val errors: String = compileErrors("""
      case class A()
      case class B()
      case class C(a: A, b: B)

      // not always used
      case class D(c: C)
      case class E(a: A, c: C)

      object Module {
        // method's parameter 'a' will shadow def 'a'.
        def a(a: A): B => C = {
           3 match {
              case 3 => (b: B) => wire[C]
              case _ => (b: B) => null
           }
        }
      }

      val aDep = new A()
      val bDep = new B()
      val c = Module.a(aDep)(bDep)

      require(c.a eq aDep)
      require(c.b eq bDep)
    """)
    assert(errors.nonEmpty, "this macwire case now compiles: move it back to the supported cases")
  }

  test("nullaryMethodUsedAsCandidate (documented difference: members brought in by an import are not candidates: no ambiguity)") {
    val _ = MacwireScala2TestCasesSpec.case_nullaryMethodUsedAsCandidate__documented_difference__members_brought_in_by_an_import_are_not_candidates__no_ambiguity_
  }

  test("selfTypeHKT") {
    val _ = MacwireScala2TestCasesSpec.case_selfTypeHKT
  }

  test("t103") {
    MacwireScala2TestCasesSpec.case_t103()
  }

  test("wireLocalVal") {
    val _ = MacwireScala2TestCasesSpec.case_wireLocalVal
  }

  test("wireWithEmptyParams") {
    val _ = MacwireScala2TestCasesSpec.case_wireWithEmptyParams
  }

  test("wireWithImplicits") {
    val _ = MacwireScala2TestCasesSpec.case_wireWithImplicits
  }

}

@scala.annotation.nowarn
object MacwireScala2TestCasesSpec {
  object case_forwardReferenceInBlock {

    case class A()
    case class B(a: A)

    trait Parent {
        lazy val a: A = wire[A]
    }

    object Test extends Parent {

        def build: B = {
            val b: B = wire[B]

            // there used to be a bug where forwardA would be found eligible
            // and preferred over Parent.a
            val forwardA = A()
            b
        }

        def buildWithLazyDep: B = {
            val b: B = wire[B]

            // even though forwardA is lazy it is still not enough for it to
            // be eligible - if it was (eligible), that would fail too in a forward reference error
            lazy val forwardA = A()
            b
        }
    }

    require(Test.build.a eq Test.a)
    require(Test.buildWithLazyDep.a eq Test.a)
  }

  def case_implicitDepsWiredWithImplicitValsFromMethodScope(): Unit = {
    case class Dependency()
    case class Service()(implicit val dependency: Dependency)


    object MethodScope {
      def someMethod(dependency: Dependency)(implicit implicitDependency: Dependency): Unit = {
        val service: Service = wire[Service]
        require(service.dependency eq implicitDependency)
      }
    }

    implicit val someImplicit = new Dependency

    MethodScope.someMethod(new Dependency)
    ()
  }

  object case_importPackage {
    class A
    class B(val a: A)

    object Test {
       // import package or package object should be ignored


       import scala.collection.mutable._

       lazy val a: A = wire[A]
       lazy val b: B = wire[B]
    }

    require(Test.b.a eq Test.a)
  }

  object case_inheritanceHKT {
    import scala.language.higherKinds

    case class A[FA[_]]()
    case class B[FB[_]]( a:A[FB] )
    case class C[FC[_]]( a:A[FC], b:B[FC] )

    trait IO[T]
    trait OptionT[F[_], T]


    trait Base1[F[_]] {
      lazy val a: A[F] = wire[A[F]]
    }

    trait Child1[F[_]] extends Base1[F] {
      lazy val b: B[F] = wire[B[F]]
    }

    trait Child2[F[_]] extends Base1[F] with Child1[F] {
      lazy val c: C[F] = wire[C[F]]
    }

    object ChildIO extends Child2[IO]

    require(ChildIO.b.a == ChildIO.a)
    require(ChildIO.c.a == ChildIO.a)
    require(ChildIO.c.b == ChildIO.b)
  }

  object case_inheritanceWireWith {
    case class A()
    case class B()

    trait BProvider {
        lazy val b: B = wire[B]
    }

    object Test extends BProvider {

        case class C(a: A, b: B, s: String)

        object C {
            def factory(a: A, b: B): C = {
                val s = "hey!"
                wire[C]
            }
        }

        lazy val a: A = wire[A]
        lazy val c: C = wireWith(C.factory _)
    }

    require(Test.c.s == "hey!")
    require(Test.c.a eq Test.a)
    require(Test.c.b eq Test.b)
  }

  def case_methodWithSingleImplicitParamOk(): Unit = {
    case class Dependency()
    case class Service()(implicit val dependency: Dependency)


    object Module {
      def someMethod(implicit implicitDependency: Dependency): Service = wire[Service]
    }

    implicit val dependency = new Dependency()

    val service = Module.someMethod

    require(service.dependency eq dependency)
    ()
  }

  object case_nullaryMethodUsedAsCandidate__documented_difference__members_brought_in_by_an_import_are_not_candidates__no_ambiguity_ {
    class A()
    class B(val a: A)

    object Module {
      def foo(): A = new A()
    }

    object Test {
      import Module._

      val a: A = new A()
      val b: B = wire[B]
    }
  }

  object case_selfTypeHKT {
    import scala.language.higherKinds

    case class A[FA[_]]()
    case class B[FB[_]]( a:A[FB] )
    case class C[FC[_]]( a:A[FC], b:B[FC] )

    trait IO[T]
    trait OptionT[F[_], T]


    trait AProvider[APF[_]] {
        def a : A[APF]
    }

    trait BProvider[BPF[_]] {
        def b : B[BPF]
    }

    trait ModuleSimple[MSF[_]] { this : AProvider[MSF] =>
        lazy val b: B[MSF] = wire[B[MSF]]
        lazy val c: C[MSF] = wire[C[MSF]]
    }

    trait ModuleMultiple[MMF[_]] { this : AProvider[MMF] with BProvider[MMF] =>
        lazy val c: C[MMF] = wire[C[MMF]]
    }

    trait AProviderImpl[AIF[_]] extends AProvider[AIF] {
        lazy val a: A[AIF] = wire[A[AIF]]
    }

    trait ABProviderImpl[ABIF[_]] extends AProvider[ABIF] with BProvider[ABIF] {
        lazy val a: A[ABIF] = wire[A[ABIF]]
        lazy val b: B[ABIF] = wire[B[ABIF]]
    }

    object TestSimple extends ModuleSimple[IO] with AProviderImpl[IO]

    require(TestSimple.c.a == TestSimple.a)
    require(TestSimple.c.b == TestSimple.b)


    object TestMultiple extends ModuleMultiple[IO] with ABProviderImpl[IO]

    require(TestMultiple.c.a == TestMultiple.a)
    require(TestMultiple.c.b == TestMultiple.b)


    object ProviderAliasing {
      type AAndB[AABF[_]] = AProvider[AABF] with BProvider[AABF]
    }

    trait AliasedMultiModule[AMMF[_]] { this: ProviderAliasing.AAndB[AMMF] =>
      lazy val c: C[AMMF] = wire[C[AMMF]]
    }

    object TestAliasedMultiple extends AliasedMultiModule[IO] with ABProviderImpl[IO]
    require(TestAliasedMultiple.c.a == TestAliasedMultiple.a)
    require(TestAliasedMultiple.c.b == TestAliasedMultiple.b)
  }

  def case_t103(): Unit = {
    class Test1(val s: Set[String])

    val set1: Set[String] = Set()
    val t1: Test1 = wire[Test1]

    require(t1.s eq Set[String]())

    class Test2(val list: List[String])
    val list2 = List[String]()
    val t2: Test2 = wire[Test2]

    require(t2.list eq List[String]())
    ()
  }

  object case_wireLocalVal {
    case class A(s: String)

    object A {
      def create(): A = {
        val s = "foo"
        wire[A]
      }
    }
    require(A.create().s == "foo")
  }

  object case_wireWithEmptyParams {
    object Test {

        case class C(s: String)

        object C {
            def factory(): C = {
                val s = "hey!"
                wire[C]
            }
        }

        lazy val c: C = wireWith(C.factory _)
    }

    require(Test.c.s == "hey!")
  }

  object case_wireWithImplicits {
    case class A(n: Int, s: String)
    object A {
        def make(n: Int)(implicit s: String): A =
            A(n, s)
    }

    lazy val n = 0
    implicit lazy val s: String = "s"

    wireWith(A.make _)
  }

}
