package hearth.kindlings.di.upstream

import hearth.kindlings.di.*

// Ported from macwire 2.6.7 (Apache License 2.0), tests/src/test/resources/test-cases/: every test case is a test
// body (macwire evaluates each file as a block), `com.softwaremill.macwire._` is replaced by `hearth.kindlings.di._`,
// and a `.failure` case checks the fragments macwire's CompileTests expects in the error.
@scala.annotation.nowarn
final class MacwireTestCasesSpec extends hearth.MacroSuite {

  // macwire's cases declare their classes in a block, so its messages show simple names: drop the prefix of the
  // classes declared in this suite (and `java.lang.`) before looking for macwire's fragments
  private def simpleNames(errors: String): String =
    errors.replaceAll("\u001b\\[[0-9;]*m", "").replaceAll("hearth\\.kindlings\\.di\\.upstream\\.[A-Za-z0-9_]+\\.", "").replace("java.lang.", "")

  test("anonFuncAndMethodsArgsWiredOk (documented difference: parameters of enclosing lambdas are not candidates)") {
    val errors: String = compileErrors("""
      case class A()
      case class B()
      case class C(a: A, b: B)

      // not always used
      case class D(c: C)
      case class E(a: A, c: C)

      object Module {
        val a = (a: A) => {
          def b(b: B) = {
              wire[C]
          }
          b _
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

  test("anonFuncArgsWiredOk (documented difference: parameters of enclosing lambdas are not candidates)") {
    val errors: String = compileErrors("""
      case class A()
      case class B()
      case class C(a: A, b: B)

      // not always used
      case class D(c: C)
      case class E(a: A, c: C)

      object Module {
        val a = new A()
        val c1 = (b: B) => wire[C]
        val c2 = (b: B) => {
          val c: C = {
              System.getProperty("mary")
              wire[C]
          }
          // do sthg
          System.getProperty("john")
          c
        }
      }

      val b = new B()
      val c1 = Module.c1(b)
      val c2 = Module.c2(b)

      require(c1.a eq Module.a)
      require(c1.b eq b)

      require(c2.a eq Module.a)
      require(c2.b eq b)
    """)
    assert(errors.nonEmpty, "this macwire case now compiles: move it back to the supported cases")
  }

  test("callByNameCompanionObject") {
    val _ = MacwireTestCasesSpec.case_callByNameCompanionObject
  }

  test("callByNameConstructorParameter") {
    val _ = MacwireTestCasesSpec.case_callByNameConstructorParameter
  }

  test("classesWithTraitsLazyValsOkInTrait") {
    MacwireTestCasesSpec.case_classesWithTraitsLazyValsOkInTrait()
  }

  test("companionFactory") {
    val _ = MacwireTestCasesSpec.case_companionFactory
  }

  test("companionFactoryAmbiguous (does not compile)") {
    val errors: String = compileErrors("""
      case class A(i: Int)

      object Test {

          class PrivConstructor private(val a : A) {}
          object PrivConstructor {
            def apply(a: A) : PrivConstructor = new PrivConstructor(A(a.i + 1))
            def apply(a: A, i: Int) : PrivConstructor = new PrivConstructor(A(a.i + i))
          }

          val a = A(3)
          val pc: PrivConstructor = wire[PrivConstructor]
      }
    """)
    assert(errors.nonEmpty, "expected a compilation error")
    assert(simpleNames(errors).contains("No public primary constructor found for"), errors)
    assert(simpleNames(errors).contains("PrivConstructor"), errors)
    assert(simpleNames(errors).contains("and multiple matching apply methods in its companion object were found for"), errors)
  }

  test("companionObjectHasFakeApplyMethods (does not compile)") {
    val errors: String = compileErrors("""
      trait A

      object Test {

        class Target private(val a : A) {}
        object Target {
          def apply(a: A): String = "This is not the construction method"
        }

        val a: A = new A{}
        wire[Target]
      }

      Test
    """)
    assert(errors.nonEmpty, "expected a compilation error")
    assert(simpleNames(errors).contains("has no apply methods constructing target type"), errors)
    assert(simpleNames(errors).contains("Target]"), errors)
  }

  test("companionObjectHasNoMethods (does not compile)") {
    val errors: String = compileErrors("""
      trait A

      object Test {

        class Target private(val a : A) {}
        object Target

        val a: A = new A{}
        wire[Target]
      }

      Test
    """)
    assert(errors.nonEmpty, "expected a compilation error")
    assert(simpleNames(errors).contains("has no apply methods constructing target type"), errors)
    assert(simpleNames(errors).contains("Target]"), errors)
  }

  test("defWithParameterAreIneligible") {
    val _ = MacwireTestCasesSpec.case_defWithParameterAreIneligible
  }

  test("defWithoutParameter") {
    val _ = MacwireTestCasesSpec.case_defWithoutParameter
  }

  test("diamondInheritance") {
    val _ = MacwireTestCasesSpec.case_diamondInheritance
  }

  test("explicitDepsNotWiredWithImplicitVals (does not compile)") {
    val errors: String = compileErrors("""
      case class A()
      case class B()
      case class C(a: A, b: B)

      // not always used
      case class D(c: C)
      case class E(a: A, c: C)

      implicit val a: A = new A()
      implicit val b: B = new B()

      object Test  {
        val c: C = wire[C]
      }

      require(false) // oops! macro didn't fail as expected..
    """)
    assert(errors.nonEmpty, "expected a compilation error")
    assert(simpleNames(errors).contains("Cannot find a value of type: [A]"), errors)
  }

  test("explicitDepsWiredWithImplicitValsFromEnclosingModuleScope") {
    val _ = MacwireTestCasesSpec.case_explicitDepsWiredWithImplicitValsFromEnclosingModuleScope
  }

  test("explicitDepsWiredWithImplicitValsFromMethodScope (does not compile)") {
    val errors: String = compileErrors("""
      case class A()
      case class B()
      case class C(a: A, b: B)

      // not always used
      case class D(c: C)
      case class E(a: A, c: C)

      object MethodScope {
        def someMethod(dependency: A)(implicit implicitDependencyA: A, implicitDependencyB: B): Unit = {
          val c: C = wire[C]
          require(c.a eq implicitDependencyA)
          require(c.b eq implicitDependencyB)
        }
      }

      implicit val implicitA: A = new A()
      implicit val implicitB: B = new B()

      MethodScope.someMethod(new A())
    """)
    assert(errors.nonEmpty, "expected a compilation error")
    assert(simpleNames(errors).contains("Found multiple values of type [A]"), errors)
    assert(simpleNames(errors).contains("dependency"), errors)
    assert(simpleNames(errors).contains("implicitDependencyA"), errors)
  }

  test("explicitDepsWiredWithImplicitValsFromParentsScope") {
    val _ = MacwireTestCasesSpec.case_explicitDepsWiredWithImplicitValsFromParentsScope
  }

  test("forwardReferenceInClass") {
    val _ = MacwireTestCasesSpec.case_forwardReferenceInClass
  }

  test("implicitDepsWiredWithImplementedImplicitVals") {
    val _ = MacwireTestCasesSpec.case_implicitDepsWiredWithImplementedImplicitVals
  }

  test("implicitDepsWiredWithImplicitDefs") {
    val _ = MacwireTestCasesSpec.case_implicitDepsWiredWithImplicitDefs
  }

  test("implicitDepsWiredWithImplicitVals") {
    val _ = MacwireTestCasesSpec.case_implicitDepsWiredWithImplicitVals
  }

  test("implicitDepsWiredWithImplicitValsFromEnclosingModuleScope") {
    val _ = MacwireTestCasesSpec.case_implicitDepsWiredWithImplicitValsFromEnclosingModuleScope
  }

  test("implicitDepsWiredWithImplicitValsFromParentsScope") {
    val _ = MacwireTestCasesSpec.case_implicitDepsWiredWithImplicitValsFromParentsScope
  }

  test("inheritanceAbstractAndConcrete") {
    val _ = MacwireTestCasesSpec.case_inheritanceAbstractAndConcrete
  }

  test("inheritanceClassesWithTraitsLazyValsOkInTraits") {
    MacwireTestCasesSpec.case_inheritanceClassesWithTraitsLazyValsOkInTraits()
  }

  test("inheritanceDoubleSimpleLazyValsOkInTraits") {
    MacwireTestCasesSpec.case_inheritanceDoubleSimpleLazyValsOkInTraits()
  }

  test("inheritanceParametrized") {
    val _ = MacwireTestCasesSpec.case_inheritanceParametrized
  }

  test("inheritancePrivateMembersInParent") {
    val _ = MacwireTestCasesSpec.case_inheritancePrivateMembersInParent
  }

  test("inheritanceProtectedMembersInParent") {
    val _ = MacwireTestCasesSpec.case_inheritanceProtectedMembersInParent
  }

  test("inheritancePublicMembersInParent (documented difference: inherited members are in the same scope as declared ones (ambiguity))") {
    val errors: String = compileErrors("""
      case class A()
      case class B()
      case class C(a: A, b: B)

      // not always used
      case class D(c: C)
      case class E(a: A, c: C)

      trait Base {
        val aA: A = A()
      }

      object Module extends Base {
        val theA: A = A()
        lazy val b: B = wire[B]
        lazy val c: C = wire[C]
      }

      require(Module.c.a eq Module.theA)
    """)
    assert(errors.nonEmpty, "this macwire case now compiles: move it back to the supported cases")
  }

  test("inheritanceSimpleDefsOkInTraits") {
    MacwireTestCasesSpec.case_inheritanceSimpleDefsOkInTraits()
  }

  test("inheritanceSimpleLazyValsOkInTraits") {
    MacwireTestCasesSpec.case_inheritanceSimpleLazyValsOkInTraits()
  }

  test("inheritanceTwoLevelSimpleLazyValsOkInTraits") {
    MacwireTestCasesSpec.case_inheritanceTwoLevelSimpleLazyValsOkInTraits()
  }

  test("methodMixedOk") {
    MacwireTestCasesSpec.case_methodMixedOk()
  }

  test("methodParamsInApplyOk") {
    MacwireTestCasesSpec.case_methodParamsInApplyOk()
  }

  test("methodParamsOk") {
    MacwireTestCasesSpec.case_methodParamsOk()
  }

  test("methodSingleParamOk") {
    MacwireTestCasesSpec.case_methodSingleParamOk()
  }

  test("methodWithWiredWithinIfThenElse (documented difference: parameters of outer enclosing methods are not candidates)") {
    val errors: String = compileErrors("""
      case class A()
      case class B()
      case class C(a: A, b: B)

      // not always used
      case class D(c: C)
      case class E(a: A, c: C)

      object Module {
        // method's parameter 'a' will shadow def 'a'.
        def a(a: A) = {
          def b(b: B)(useWire: Boolean): C = {
            if (useWire) wire[C]
            else new C(new A, new B)
          }
          b _
        }
      }

      val aDep = new A()
      val bDep = new B()
      val c = Module.a(aDep)(bDep)(true)

      require(c.a eq aDep)
      require(c.b eq bDep)
    """)
    assert(errors.nonEmpty, "this macwire case now compiles: move it back to the supported cases")
  }

  test("multipleMethodParameters (does not compile)") {
    val errors: String = compileErrors("""
      case class A()
      case class B(a1: A, a2: A)

      class Test {
          def theB(a2: A, a1: A): B = wire[B]
      }

      val t = new Test
      val a1 = new A()
      val a2 = new A()

      t.theB(a2, a1)
    """)
    assert(errors.nonEmpty, "expected a compilation error")
    assert(simpleNames(errors).contains("Found multiple values of type [A]"), errors)
    assert(simpleNames(errors).contains("a1"), errors)
    assert(simpleNames(errors).contains("a2"), errors)
  }

  test("nestedAnonFuncsWired (documented difference: parameters of enclosing lambdas are not candidates)") {
    val errors: String = compileErrors("""
      case class A()
      case class B()
      case class C(a: A, b: B)

      // not always used
      case class D(c: C)
      case class E(a: A, c: C)

      object Module {
        // anonymous function's parameter 'a' will shadow val 'a'.
        val a = (a: A) =>
          (b: B) => wire[C]
      }

      val aDep = new A()
      val bDep = new B()
      val c = Module.a(aDep)(bDep)

      require(c.a eq aDep)
      require(c.b eq bDep)
    """)
    assert(errors.nonEmpty, "this macwire case now compiles: move it back to the supported cases")
  }

  test("nestedMethodsWired (documented difference: parameters of outer enclosing methods are not candidates: the inner parameter is used, no ambiguity)") {
    val _ = MacwireTestCasesSpec.case_nestedMethodsWired__documented_difference__parameters_of_outer_enclosing_methods_are_not_candidates__the_inner_parameter_is_used__no_ambiguity_
  }

  test("nestedMethodsWired (documented difference: parameters of outer enclosing methods are not candidates)") {
    val errors: String = compileErrors("""
      case class A()
      case class B()
      case class C(a: A, b: B)

      // not always used
      case class D(c: C)
      case class E(a: A, c: C)

      object Module {
        // method's parameter 'a' will shadow def 'a'.
        def a(a: A) = {
          def b(b: B): C = wire[C]
          b _
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

  test("nestedWithManyMatchingParamsWired (documented difference: parameters of outer enclosing methods are not candidates) (does not compile)") {
    val errors: String = compileErrors("""
      case class A()
      case class B()
      case class C(a: A, b: B)

      // not always used
      case class D(c: C)
      case class E(a: A, c: C)

      object Module {
        // anonymous function's parameter 'a' will shadow val 'a'.
        val a = (a1: A) =>
          (b: B) =>
            (a2: A) =>
              (a3: A) => wire[C]
      }

      val a1Dep = new A()
      val a2Dep = new A()
      val a3Dep = new A()
      val bDep = new B()

      // we're expecting compilation abort due to ambiguous dependencies' resolution.
      val c = Module.a(a1Dep)(bDep)(a2Dep)(a3Dep)
    """)
    assert(errors.nonEmpty, "expected a compilation error")
  }

  test("parametrizedMethodNotUsedAsCandidate") {
    val _ = MacwireTestCasesSpec.case_parametrizedMethodNotUsedAsCandidate
  }

  test("selfType") {
    val _ = MacwireTestCasesSpec.case_selfType
  }

  test("simpleDefsInTrait") {
    MacwireTestCasesSpec.case_simpleDefsInTrait()
  }

  test("simpleGenericClass") {
    val _ = MacwireTestCasesSpec.case_simpleGenericClass
  }

  test("simpleLazyValsInTrait") {
    MacwireTestCasesSpec.case_simpleLazyValsInTrait()
  }

  test("simpleLazyValsNotInOrder") {
    MacwireTestCasesSpec.case_simpleLazyValsNotInOrder()
  }

  test("simpleValsDuplicateValue (does not compile)") {
    val errors: String = compileErrors("""
      case class A()
      case class B()
      case class C(a: A, b: B)

      // not always used
      case class D(c: C)
      case class E(a: A, c: C)

      trait Test {
          val theA: A = wire[A]
          val theB1: B = wire[B]
          val theB2: B = wire[B]
          val theC: C = wire[C]
      }
    """)
    assert(errors.nonEmpty, "expected a compilation error")
    assert(simpleNames(errors).contains("Found multiple values of type [B]"), errors)
    assert(simpleNames(errors).contains("theB1"), errors)
    assert(simpleNames(errors).contains("theB2"), errors)
  }

  test("simpleValsImplicitParameterLists") {
    val _ = MacwireTestCasesSpec.case_simpleValsImplicitParameterLists
  }

  test("simpleValsInClass") {
    MacwireTestCasesSpec.case_simpleValsInClass()
  }

  test("simpleValsInClassParameters (documented difference: constructor parameters of the enclosing class that are not vals are not candidates)") {
    val errors: String = compileErrors("""
      case class A()
      case class B()
      case class C(a: A, b: B)

      // not always used
      case class D(c: C)
      case class E(a: A, c: C)

      class Test(theA: A,
                 val theB :B) {

          val theC: C = wire[C]
      }

      val a = A()
      val test = new Test(a, B())

      require(test.theC.a eq a)
      require(test.theC.b eq test.theB)
    """)
    assert(errors.nonEmpty, "this macwire case now compiles: move it back to the supported cases")
  }

  test("simpleValsInObject") {
    val _ = MacwireTestCasesSpec.case_simpleValsInObject
  }

  test("simpleValsInTrait") {
    MacwireTestCasesSpec.case_simpleValsInTrait()
  }

  test("simpleValsMissingValue (does not compile)") {
    val errors: String = compileErrors("""
      case class A()
      case class B()
      case class C(a: A, b: B)

      // not always used
      case class D(c: C)
      case class E(a: A, c: C)

      trait Test {
          val theA: A = wire[A]
          val theC: C = wire[C]
      }
    """)
    assert(errors.nonEmpty, "expected a compilation error")
    assert(simpleNames(errors).contains("Cannot find a value of type: [B]"), errors)
  }

  test("simpleValsMultipleParameterLists") {
    MacwireTestCasesSpec.case_simpleValsMultipleParameterLists()
  }

  test("simpleValsReferenceWithAscription") {
    MacwireTestCasesSpec.case_simpleValsReferenceWithAscription()
  }

  test("simpleWithAbstract") {
    MacwireTestCasesSpec.case_simpleWithAbstract()
  }

  test("valsFlaggedImplicit") {
    val _ = MacwireTestCasesSpec.case_valsFlaggedImplicit
  }

  test("wireRec") {
    val _ = MacwireTestCasesSpec.case_wireRec
  }

  test("wireRecEmptyString (does not compile)") {
    val errors: String = compileErrors("""
      case class E(s: String)

      object Test {
          lazy val d: E = wireRec[E]
      }
    """)
    assert(errors.nonEmpty, "expected a compilation error")
    assert(simpleNames(errors).contains("Cannot find a value of type: [String]"), errors)
  }

  test("wireRecMultiStage") {
    val _ = MacwireTestCasesSpec.case_wireRecMultiStage
  }

  test("wireSet (documented difference: imports)") {
    MacwireTestCasesSpec.case_wireSet__documented_difference__imports_()
  }

  test("wireWith") {
    val _ = MacwireTestCasesSpec.case_wireWith
  }

  test("wireWithTwoParamsLists (does not compile)") {
    val errors: String = compileErrors("""
      case class A()
      case class B()

      trait BProvider {
          lazy val b: B = wire[B]
      }

      object Test extends BProvider {

          case class C(a: A, b: B, s: String)

          object C {
              def factory(a: A)(b: B): C = {
                  val s = "hey!"
                  wire[C]
              }
          }

          lazy val a: A = wire[A]
          lazy val c: C = wireWith(C.factory _)
      }
    """)
    assert(errors.nonEmpty, "expected a compilation error")
  }

}

@scala.annotation.nowarn
object MacwireTestCasesSpec {
  object case_callByNameCompanionObject {
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
        def apply(dependency: => A) = { created.append("a"); new Component(dependency) }
      }

      val dependency =  new A()
      val component: Component = wire[Component]
    }

    require(Test.component.a != null)
    require(Test.created.size == 1)
    require(Test.created.head == "a")
  }

  object case_callByNameConstructorParameter {
    class A()

    class B(aProvider: => A) {
      def a = aProvider
    }

    class C[T]()

    class D(cProvider: => C[String]) {
      def c = cProvider
    }

    object Test {
      val a = new A()
      val b: B = wire[B]
      val cString = new C[String]()
      val cInt = new C[Int]()
      val d: D = wire[D]
    }

    require(Test.b.a eq Test.a)
    require(Test.d.c eq Test.cString)
  }

  def case_classesWithTraitsLazyValsOkInTrait(): Unit = {
    trait A
    class AImpl extends A

    trait B
    class BImpl extends B

    trait C
    class CImpl(val a: A, val b: B) extends C

    class DImpl(val c: C)


    trait Test {
        lazy val theA: AImpl = wire[AImpl]
        lazy val theB: BImpl = wire[BImpl]
        lazy val theC: CImpl = wire[CImpl]
        lazy val theD: DImpl = wire[DImpl]
    }

    val t = new Test {}

    require(t.theC.a != null)
    require(t.theC.b != null)

    require(t.theC.a eq t.theA)
    require(t.theC.b eq t.theB)

    require(t.theD.c != null)

    require(t.theD.c eq t.theC)
    ()
  }

  object case_companionFactory {
    case class A(i: Int)

    object Test {

        class PrivConstructor private(val a : A) {}
        object PrivConstructor {
          def apply(a: A) : PrivConstructor = new PrivConstructor(A(a.i + 1))
        }

        val a = A(3)
        val pc: PrivConstructor = wire[PrivConstructor]
    }

    require(Test.pc.a == A(4))
  }

  object case_defWithParameterAreIneligible {
    case class A()
    case class B(a: A)

    object Test {

        lazy val a: A = wire[A]

        def buildSomeA(i: Int): A = A()

        lazy val b: B = wire[B]
    }

    require(Test.b.a eq Test.a)
  }

  object case_defWithoutParameter {
    case class A()
    case class B(a: A)

    object Test {

        var countA: Int = 0

        def a: A = {
            countA += 1
            A()
        }

        lazy val b1: B = wire[B]
        lazy val b2: B = wire[B]
    }

    require(!(Test.b1.a eq Test.b2.a))
    require(Test.countA == 2)
  }

  object case_diamondInheritance {
    class Dependency
    class Service(val dependency: Dependency)
    class AnotherService(val dependency: Dependency)

    trait A {
      val dependency: Dependency = wire[Dependency]
    }

    trait B extends A {
      val service: Service = wire[Service]
    }

    object C extends A with B {
      val anotherService: AnotherService = wire[AnotherService]
    }

    require(C.dependency != null)
    require(C.service.dependency eq C.dependency)
    require(C.anotherService.dependency eq C.dependency)
  }

  object case_explicitDepsWiredWithImplicitValsFromEnclosingModuleScope {
    case class A()
    case class B()
    case class C(a: A, b: B)

    // not always used
    case class D(c: C)
    case class E(a: A, c: C)

    object ModuleScope {
      implicit val a: A = new A()
      implicit val b: B = new B()
      val c: C = wire[C]
    }
    require(ModuleScope.c.a eq ModuleScope.a)
    require(ModuleScope.c.b eq ModuleScope.b)
  }

  object case_explicitDepsWiredWithImplicitValsFromParentsScope {
    case class A()
    case class B()
    case class C(a: A, b: B)

    // not always used
    case class D(c: C)
    case class E(a: A, c: C)

    trait DependencyProvider {
      implicit val a: A = new A()
      implicit val b: B = new B()
    }

    object ParentsScope extends DependencyProvider {
      val c: C = wire[C]
    }

    require(ParentsScope.c.a eq ParentsScope.a)
    require(ParentsScope.c.b eq ParentsScope.b)
  }

  object case_forwardReferenceInClass {

    case class A()
    case class B(a: A)

    object Test {
        lazy val b: B = wire[B]
        lazy val a: A = wire[A]
    }

    require(Test.b.a eq Test.a)
  }

  object case_implicitDepsWiredWithImplementedImplicitVals {
    case class Dependency()
    case class Service()(implicit val dependency: Dependency)


    trait Abstract {
      implicit val dependency: Dependency
    }

    object Test extends Abstract {
      override implicit val dependency: Dependency = new Dependency
      val service: Service = wire[Service]
    }

    require(Test.service.dependency eq Test.dependency)
  }

  object case_implicitDepsWiredWithImplicitDefs {
    case class Dependency()
    case class Service()(implicit val dependency: Dependency)


    implicit def dependency: Dependency = new Dependency

    object Test  {
      val service: Service = wire[Service]
    }
  }

  object case_implicitDepsWiredWithImplicitVals {
    case class Dependency()
    case class Service()(implicit val dependency: Dependency)


    implicit val dependency: Dependency = new Dependency

    object Test  {
      val service: Service = wire[Service]
    }

    require(Test.service.dependency eq dependency)
  }

  object case_implicitDepsWiredWithImplicitValsFromEnclosingModuleScope {
    case class Dependency()
    case class Service()(implicit val dependency: Dependency)


    object ModuleScope {
      implicit val moduleImplicitDependency: Dependency = new Dependency()
      val service: Service = wire[Service]
    }
    require(ModuleScope.service.dependency eq ModuleScope.moduleImplicitDependency)
  }

  object case_implicitDepsWiredWithImplicitValsFromParentsScope {
    case class Dependency()
    case class Service()(implicit val dependency: Dependency)


    trait DependencyProvider {
      implicit val implicitDependency: Dependency = new Dependency
    }

    object ParentsScope extends DependencyProvider {
      val service: Service = wire[Service]
    }

    require(ParentsScope.service.dependency eq ParentsScope.implicitDependency)
  }

  object case_inheritanceAbstractAndConcrete {
    // See https://github.com/adamw/macwire/issues/82
    // *sometimes* (not on every compiler run) the trees for the identifiers X.a and Y.a were different hash codes, but
    // equal structurally. This only occurred when Y.a was also used in another wired class.

    class A()
    class B(val a: A)
    class C(val a: A)

    trait X {
      def a: A
    }

    trait Y {
      lazy val a: A = wire[A]
      lazy val c: C = wire[C]
    }

    object Z extends Y with X {
      val b: B = wire[B]
    }

    require(Z.b.a eq Z.a)
    require(Z.c.a eq Z.a)
  }

  def case_inheritanceClassesWithTraitsLazyValsOkInTraits(): Unit = {
    trait A
    class AImpl extends A

    trait B
    class BImpl extends B

    trait C
    class CImpl(val a: A, val b: B) extends C

    class DImpl(val c: C)


    trait Base {
        lazy val theA: AImpl = wire[AImpl]
        lazy val theB: BImpl = wire[BImpl]
    }

    trait Test extends Base {
        lazy val theC: CImpl = wire[CImpl]
        lazy val theD: DImpl = wire[DImpl]
    }

    val t = new Test {}

    require(t.theC.a != null)
    require(t.theC.b != null)

    require(t.theC.a eq t.theA)
    require(t.theC.b eq t.theB)

    require(t.theD.c != null)

    require(t.theD.c eq t.theC)
    ()
  }

  def case_inheritanceDoubleSimpleLazyValsOkInTraits(): Unit = {
    case class A()
    case class B()
    case class C(a: A, b: B)

    // not always used
    case class D(c: C)
    case class E(a: A, c: C)

    trait Base1 {
        lazy val theA: A = wire[A]
    }

    trait Base2 {
        lazy val theB: B = wire[B]
    }

    trait Test extends Base1 with Base2 {
        lazy val theC: C = wire[C]
    }

    val t = new Test {}

    require(t.theC.a != null)
    require(t.theC.b != null)

    require(t.theC.a eq t.theA)
    require(t.theC.b eq t.theB)
    ()
  }

  object case_inheritanceParametrized {
    case class A()
    case class B()
    case class C(a: A, b: B)

    // not always used
    case class D(c: C)
    case class E(a: A, c: C)

    class Base(name: String) {
        lazy val theA: A = wire[A]
        lazy val theB: B = wire[B]
    }

    object t extends Base("toto") {
        lazy val theC: C = wire[C]
    }

    require(t.theC.a eq t.theA)
    require(t.theC.b eq t.theB)
  }

  object case_inheritancePrivateMembersInParent {
    case class A()
    case class B()
    case class C(a: A, b: B)

    // not always used
    case class D(c: C)
    case class E(a: A, c: C)

    trait Base {
      private lazy val privateA: A = A()
      lazy val publicA: A = A()
    }

    object Module extends Base {
      lazy val b: B = wire[B]
      lazy val c: C = wire[C]
    }

    require(Module.c.a eq Module.publicA)
  }

  object case_inheritanceProtectedMembersInParent {
    case class A()
    case class B()
    case class C(a: A, b: B)

    // not always used
    case class D(c: C)
    case class E(a: A, c: C)

    trait Base {
      protected lazy val protectedA: A = A()
      lazy val publicA: A = A()
    }

    object Module extends Base {
      lazy val b: B = wire[B]
      lazy val c: C = wire[C]
    }

    require(Module.c.a eq Module.publicA)
  }

  def case_inheritanceSimpleDefsOkInTraits(): Unit = {
    case class A()
    case class B()
    case class C(a: A, b: B)

    // not always used
    case class D(c: C)
    case class E(a: A, c: C)

    trait Base {
        def theA: A = wire[A]
        def theB: B = wire[B]
    }

    trait Test extends Base {
        def theC: C = wire[C]
    }

    val t = new Test {}

    require(t.theC.a != null)
    require(t.theC.b != null)
    ()
  }

  def case_inheritanceSimpleLazyValsOkInTraits(): Unit = {
    case class A()
    case class B()
    case class C(a: A, b: B)

    // not always used
    case class D(c: C)
    case class E(a: A, c: C)

    trait Base {
        lazy val theA: A = wire[A]
        lazy val theB: B = wire[B]
    }

    trait Test extends Base {
        lazy val theC: C = wire[C]
    }

    val t = new Test {}

    require(t.theC.a != null)
    require(t.theC.b != null)

    require(t.theC.a eq t.theA)
    require(t.theC.b eq t.theB)
    ()
  }

  def case_inheritanceTwoLevelSimpleLazyValsOkInTraits(): Unit = {
    case class A()
    case class B()
    case class C(a: A, b: B)

    // not always used
    case class D(c: C)
    case class E(a: A, c: C)

    trait Base1 {
        lazy val theA: A = wire[A]
        lazy val theB: B = wire[B]
    }

    trait Base2 extends Base1 {
        lazy val theC: C = wire[C]
    }

    trait Test extends Base2 {
        lazy val theD: D = wire[D]
        lazy val theE: E = wire[E]
    }

    val t = new Test {}

    require(t.theC.a != null)
    require(t.theC.b != null)

    require(t.theC.a eq t.theA)
    require(t.theC.b eq t.theB)

    require(t.theD.c != null)
    require(t.theD.c eq t.theC)

    require(t.theE.a != null)
    require(t.theE.c != null)
    require(t.theE.a eq t.theA)
    require(t.theE.c eq t.theC)
    ()
  }

  def case_methodMixedOk(): Unit = {
    case class A()
    case class B()
    case class C(a: A, b: B)

    // not always used
    case class D(c: C)
    case class E(a: A, c: C)

    class Test {
        lazy val theA: A = wire[A]
        def theC(someB: B): C = wire[C]
    }

    val t = new Test
    val b = new B
    val c = t.theC(b)

    require(c != null)
    require(c.a != null)
    require(c.b != null)

    require(c.a eq t.theA)
    require(c.b eq b)
    ()
  }

  def case_methodParamsInApplyOk(): Unit = {
    case class A()
    case class B()
    case class C(a: A, b: B)

    // not always used
    case class D(c: C)
    case class E(a: A, c: C)

    class Test {
        object Props {
            def apply[T](creator: => T): T = creator
        }

        def theC(a: A, b: B): C = Props(wire[C])
    }

    val t = new Test
    val a = new A
    val b = new B
    val c = t.theC(a, b)

    require(c != null)
    require(c.a != null)
    require(c.b != null)

    require(c.a eq a)
    require(c.b eq b)
    ()
  }

  def case_methodParamsOk(): Unit = {
    case class A()
    case class B()
    case class C(a: A, b: B)

    // not always used
    case class D(c: C)
    case class E(a: A, c: C)

    class Test {
        def theC(someB: B, someA: A): C = wire[C]
    }

    val t = new Test
    val a = new A
    val b = new B
    val c = t.theC(b, a)

    require(c != null)
    require(c.a != null)
    require(c.b != null)

    require(c.a eq a)
    require(c.b eq b)
    ()
  }

  def case_methodSingleParamOk(): Unit = {
    case class A()
    case class B(a: A)

    class Test {
        def theB(someA: A): B = wire[B]
    }

    val t = new Test
    val a = new A
    val b = t.theB(a)

    require(b != null)
    require(b.a != null)

    require(b.a eq a)
    ()
  }

  object case_nestedMethodsWired__documented_difference__parameters_of_outer_enclosing_methods_are_not_candidates__the_inner_parameter_is_used__no_ambiguity_ {
    case class A()
    case class B()
    case class C(a: A, b: B)

    // not always used
    case class D(c: C)
    case class E(a: A, c: C)

    object Module {

      val b = new B()

      // method's parameter 'a' will shadow def 'a'.
      def a(outerA: A): A => B = {
        def innerA(innerA: A): C = wire[C]
        (aa: A) => b 
      }
    }

    val aDep = new A()
    Module.a(aDep)(aDep)
  }

  object case_parametrizedMethodNotUsedAsCandidate {
    class A()
    class B(val a: A)
    class C()

    object Module {
      def foo(c: C): A = new A()
    }

    object Test {
      import Module._

      val a: A = new A()
      val b: B = wire[B]
    }

    require(Test.b.a eq Test.a)
  }

  object case_selfType {
    case class A()
    case class B()
    case class C(a: A, b: B)

    // not always used
    case class D(c: C)
    case class E(a: A, c: C)

    trait AProvider {
        def a : A
    }

    trait BProvider {
        def b : B
    }

    trait ModuleSimple { this : AProvider =>
        lazy val b: B = wire[B]
        lazy val c: C = wire[C]
    }

    trait ModuleMultiple { this : AProvider with BProvider =>
        lazy val c: C = wire[C]
    }

    trait AProviderImpl extends AProvider {
        lazy val a: A = wire[A]
    }

    trait ABProviderImpl extends AProvider with BProvider {
        lazy val a: A = wire[A]
        lazy val b: B = wire[B]
    }

    object TestSimple extends ModuleSimple with AProviderImpl

    require(TestSimple.c.a == TestSimple.a)
    require(TestSimple.c.b == TestSimple.b)


    object TestMultiple extends ModuleMultiple with ABProviderImpl

    require(TestMultiple.c.a == TestMultiple.a)
    require(TestMultiple.c.b == TestMultiple.b)

    object ProviderAliasing {
      type AAndB = AProvider with BProvider
    }

    trait AliasedMultiModule { this: ProviderAliasing.AAndB =>
      lazy val c: C = wire[C]
    }

    object TestAliasedMultiple extends AliasedMultiModule with ABProviderImpl
    require(TestAliasedMultiple.c.a == TestAliasedMultiple.a)
    require(TestAliasedMultiple.c.b == TestAliasedMultiple.b)
  }

  def case_simpleDefsInTrait(): Unit = {
    case class A()
    case class B()
    case class C(a: A, b: B)

    // not always used
    case class D(c: C)
    case class E(a: A, c: C)

    trait Test {
        def theA: A = wire[A]
        def theB: B = wire[B]
        def theC: C = wire[C]
    }

    val t = new Test {}

    require(t.theC.a != null)
    require(t.theC.b != null)
    ()
  }

  object case_simpleGenericClass {
    import scala.language.higherKinds

    case class A[FA[_]]()
    case class B[FB[_]]( a:A[FB] )
    case class C[FC[_]]( a:A[FC], b:B[FC] )

    trait IO[T]
    trait OptionT[F[_], T]


    case class LocalA[X]()
    case class LocalB[X](la: LocalA[X])

    object Test {
      val la: LocalA[Int] = wire[LocalA[Int]]
      val lb: LocalB[Int] = wire[LocalB[Int]]
      val a: A[IO] = wire[A[IO]]
      val b: B[IO] = wire[B[IO]]
    }

    require(Test.lb.la eq Test.la)
    require(Test.b.a eq Test.a)
  }

  def case_simpleLazyValsInTrait(): Unit = {
    case class A()
    case class B()
    case class C(a: A, b: B)

    // not always used
    case class D(c: C)
    case class E(a: A, c: C)

    trait Test {
        lazy val theA: A = wire[A]
        lazy val theB: B = wire[B]
        lazy val theC: C = wire[C]
    }

    val t = new Test {}

    require(t.theC.a != null)
    require(t.theC.b != null)

    require(t.theC.a eq t.theA)
    require(t.theC.b eq t.theB)
    ()
  }

  def case_simpleLazyValsNotInOrder(): Unit = {
    case class A()
    case class B()
    case class C(a: A, b: B)

    // not always used
    case class D(c: C)
    case class E(a: A, c: C)

    trait Test {
        lazy val theD: D = wire[D]
        lazy val theC: C = wire[C]
        lazy val theB: B = wire[B]
        lazy val theA: A = wire[A]
    }

    val t = new Test {}

    require(t.theC.a != null)
    require(t.theC.b != null)

    require(t.theC.a eq t.theA)
    require(t.theC.b eq t.theB)

    require(t.theD.c != null)
    require(t.theD.c eq t.theC)
    ()
  }

  object case_simpleValsImplicitParameterLists {
    case class A()
    case class B()
    case class C(a: A, b: B)

    // not always used
    case class D(c: C)
    case class E(a: A, c: C)

    case class F(a: A)(implicit val b: B)

    object Implicits {
        implicit val theB: B = wire[B]
    }

    trait Test {
        import Implicits._

        val theA: A = wire[A]
        val theF: F = wire[F]
    }

    val t = new Test {}

    require(t.theF.a != null)
    require(t.theF.b != null)

    require(t.theF.a eq t.theA)
    require(t.theF.b eq Implicits.theB)
  }

  def case_simpleValsInClass(): Unit = {
    case class A()
    case class B()
    case class C(a: A, b: B)

    // not always used
    case class D(c: C)
    case class E(a: A, c: C)

    class Test {
        val theA: A = wire[A]
        val theB: B = wire[B]
        val theC: C = wire[C]
    }

    val t = new Test {}

    require(t.theC.a != null)
    require(t.theC.b != null)

    require(t.theC.a eq t.theA)
    require(t.theC.b eq t.theB)
    ()
  }

  object case_simpleValsInObject {
    case class A()
    case class B()
    case class C(a: A, b: B)

    // not always used
    case class D(c: C)
    case class E(a: A, c: C)

    object Test {
        val theA: A = wire[A]
        val theB: B = wire[B]
        val theC: C = wire[C]

        require(theC.a != null)
        require(theC.b != null)
    }

    require(Test.theC.a != null)
    require(Test.theC.b != null)

    require(Test.theC.a eq Test.theA)
    require(Test.theC.b eq Test.theB)
  }

  def case_simpleValsInTrait(): Unit = {
    case class A()
    case class B()
    case class C(a: A, b: B)

    // not always used
    case class D(c: C)
    case class E(a: A, c: C)

    trait Test {
        val theA: A = wire[A]
        val theB: B = wire[B]
        val theC: C = wire[C]
    }

    val t = new Test {}

    require(t.theC.a != null)
    require(t.theC.b != null)

    require(t.theC.a eq t.theA)
    require(t.theC.b eq t.theB)
    ()
  }

  def case_simpleValsMultipleParameterLists(): Unit = {
    case class A()
    case class B()
    case class C(a: A, b: B)

    // not always used
    case class D(c: C)
    case class E(a: A, c: C)

    case class F(a: A, b: B)(val c: C)(val d: D)

    trait Test {
        val theA: A = wire[A]
        val theB: B = wire[B]
        val theC: C = wire[C]
        val theD: D = wire[D]
        val theF: F = wire[F]
    }

    val t = new Test {}

    require(t.theC.a != null)
    require(t.theC.b != null)

    require(t.theC.a eq t.theA)
    require(t.theC.b eq t.theB)

    require(t.theF.a != null)
    require(t.theF.b != null)
    require(t.theF.c != null)
    require(t.theF.d != null)

    require(t.theF.a eq t.theA)
    require(t.theF.b eq t.theB)
    require(t.theF.c eq t.theC)
    require(t.theF.d eq t.theD)
    ()
  }

  def case_simpleValsReferenceWithAscription(): Unit = {
    case class A()
    case class B()
    case class C(a: A, b: B)

    // not always used
    case class D(c: C)
    case class E(a: A, c: C)

    trait Test {
        val theA: A = wire[A]
        val theB: B = wire[B]
        val theC: C = wire[C]
        val theD = new D(theC) // reference
    }

    val t = new Test {}

    require(t.theC.a != null)
    require(t.theC.b != null)

    require(t.theC.a eq t.theA)
    require(t.theC.b eq t.theB)
    ()
  }

  def case_simpleWithAbstract(): Unit = {
    case class A()
    case class B()
    case class C(a: A, b: B)

    // not always used
    case class D(c: C)
    case class E(a: A, c: C)

    trait Test {
        def theA: A
        def theB: B
        lazy val theC: C = wire[C]
    }


    val t = new Test {
        lazy val theA = new A
        lazy val theB = new B
    }

    require(t.theC.a != null)
    require(t.theC.b != null)
    ()
  }

  object case_valsFlaggedImplicit {
    case class A()
    case class B()
    case class C(a: A, b: B)

    // not always used
    case class D(c: C)
    case class E(a: A, c: C)

    // b isn't an implicit parameter it is an implicit member
    // this is (to me at least) a shadowy part of the specification
    class Service(val a: A, implicit val b: B)

    object Test {
        lazy val a: A = wire[A]
        lazy val b: B = wire[B]

        // if macwire let the compiler wire the dependencies it will fail
        // as `a` isn't implicit
        lazy val service: Service = wire[Service]
    }

    require(Test.service.a eq Test.a)
    require(Test.service.b eq Test.b)
  }

  object case_wireRec {
    case class A()
    case class B()
    case class C(a: A, b: B)
    case class D(a: A, c: C)

    object Test {
        lazy val b: B = wire[B]
        lazy val a: A = wire[A]

        lazy val d: D = wireRec[D]

        require(d.a == a)
        require(d.c.a == a)
        require(d.c.b == b)
    }
  }

  object case_wireRecMultiStage {
    case class A()
    case class B()
    case class C(a: A, b: B)
    case class D(a: A, c: C)
    case class E(c: C, d: D)

    object Test {
        lazy val e: E = wireRec[E]

        require(e.c.a == A())
        require(e.c == e.d.c)
    }
  }

  def case_wireSet__documented_difference__imports_(): Unit = {
    trait A
    class A1 extends A
    class A2 extends A
    class A3 extends A
    class A4 extends A

    case class Group(as: Set[A])

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
        import plugin1._
        import plugin2._

        lazy val a4: A = wire[A4]

        // wireSet look at the usual places:
        // - enclosing members
        // - enclosing imports
        // - parents
        lazy val as : Set[A] = wireSet[A]

        // inject that Set of A into Group
        lazy val group: Group = wire[Group]
    }

    val plugin1 = new Plugin1
    val plugin2 = new Plugin2
    val app = new App(plugin1, plugin2)

    // kindlings: members brought in by an import (`import plugin1._`) are not candidates (documented difference)
    require(app.group.as == Set(app.a3, app.a4))
    ()
  }

  object case_wireWith {
    case class A()
    case class B()

    object Test {
        case class C(a: A, b: B)

        object C {
            def factory(a: A, b: B): C = {
                wire[C]
            }
        }

        lazy val a: A = wire[A]
        lazy val b: B = wire[B]

        lazy val c: C = wireWith(C.factory _)
    }

    require(Test.c.a eq Test.a)
    require(Test.c.b eq Test.b)
  }

}
