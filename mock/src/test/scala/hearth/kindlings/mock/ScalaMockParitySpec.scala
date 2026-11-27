package hearth.kindlings.mock

import hearth.MacroSuite

import scala.reflect.ClassTag
import scala.util.{Failure, Try}

/** Cases of ScalaMock's own test suite (ScalaMock 7.6.3, Apache License 2.0) that kindlings-mock's `MockSpec` did not
  * cover yet: mostly the members of ScalaMock's `TestTrait` (core/shared/src/test/scala/com/paulbutcher/test/
  * TestTrait.scala) as exercised by `MockTest.scala`, `OrderingTest.scala` and `ReturnTest.scala` (scalatest/shared/
  * src/test/scala/com/paulbutcher/test/). Written with kindlings' name-keyed DSL; the expectations mirror upstream's.
  */
final class ScalaMockParitySpec extends MacroSuite {
  import ScalaMockParitySpec.*

  private def call(a: => Any): Unit = { val _ = a }

  group("ScalaMock MockTest: kinds of members") {

    test("cope with polymorphic overloaded methods (overloaded[T](x: T) next to monomorphic overloads)") {
      implicit val ctx: MockContext = new MockContext
      val m = Mock.mock[Overloads]
      val _ = ctx.expecting("overloaded", 1.23).returning("polymorphic")
      val _ = ctx.expecting("overloaded", 10).returning("int")
      val _ = ctx.expecting("overloaded", "foo").returning("string")
      m.overloaded(1.23) ==> "polymorphic"
      m.overloaded(10) ==> "int"
      m.overloaded("foo") ==> "string"
      ctx.verifyExpectations()
    }

    test("cope with curried polymorphic methods") {
      implicit val ctx: MockContext = new MockContext
      val m = Mock.mock[Members]
      val _ = ctx.expecting("polycurried", "a", 2).returning(("b", 3))
      m.polycurried("a")(2) ==> (("b", 3))
      ctx.verifyExpectations()
    }

    test("cope with parameters of polymorphic type (a tuple)") {
      implicit val ctx: MockContext = new MockContext
      val m = Mock.mock[Members]
      val _ = ctx.expecting("polymorphicParam", (42, 1.23)).returning("a return value")
      m.polymorphicParam((42, 1.23)) ==> "a return value"
      ctx.verifyExpectations()
    }

    test("cope with upper bounds, lower bounds and context bounds") {
      implicit val ctx: MockContext = new MockContext
      val m = Mock.mock[Members]
      val _ = ctx.expecting("upperBound", (42, 1.23)).returning(2)
      val _ = ctx.expecting("lowerBound", 1, List(2)).returning("lower")
      val _ = ctx.expecting("contextBound", 3, ArgMatcher.any).returning("context")
      m.upperBound((42, 1.23)) ==> 2
      m.lowerBound(1, List(2)) ==> "lower"
      m.contextBound(3) ==> "context"
      ctx.verifyExpectations()
    }

    test("cope with curried function returning methods") {
      implicit val ctx: MockContext = new MockContext
      val m = Mock.mock[Members]
      val f: Double => String = d => s"got $d"
      val _ = ctx.expecting("curriedFuncReturn", 10).returning(f)
      m.curriedFuncReturn(10)(1.5) ==> "got 1.5"
      ctx.verifyExpectations()
    }

    test("cope with methods where Seq[T] is the last parameter, and curried varargs") {
      implicit val ctx: MockContext = new MockContext
      val m = Mock.mock[Members]
      val _ = ctx.expecting("takesSeq", 1, Seq("a", "b")).returning("seq")
      val _ = ctx.expecting("curriedVarargs", 1, Seq("x", "y")).returning("varargs")
      m.takesSeq(1, Seq("a", "b")) ==> "seq"
      m.curriedVarargs(1)("x", "y") ==> "varargs"
      ctx.verifyExpectations()
    }

    test("cope with a function val") {
      implicit val ctx: MockContext = new MockContext
      val m = Mock.mock[Members]
      // an abstract val yields the default (null for a function): like ScalaMock, a val cannot be given expectations
      assert(m.fnVal == null)
      ctx.verifyExpectations()
    }

    test("mock methods that need a class tag") {
      implicit val ctx: MockContext = new MockContext
      val provider = Mock.mock[DataProviderComponent]
      val _ = ctx.expecting("find", 13, ArgMatcher.any).returning(Failure[User](new Exception()))
      assert(provider.find[User](13).isFailure)
      ctx.verifyExpectations()
    }

    test("mock a trait which has a final method / a protected method") {
      implicit val ctx: MockContext = new MockContext
      val f = Mock.mock[FinalMethodTrait]
      val p = Mock.mock[ProtectedMethodTrait]
      val _ = ctx.expecting("somePublicMethod", ArgMatcher.any).anyNumberOfTimes()
      f.somePublicMethod("x")
      p.somePublicMethod("y")
      f.someFinalMethod(1) ==> "final method"
      ctx.verifyExpectations()
    }

    test("be able to stub notify(Int)") {
      implicit val ctx: MockContext = new MockContext
      val s = Mock.stub[HasNotifyMethod]
      val _ = ctx.when("notify", 3).returning(4)
      s.notify(3) ==> 4
    }

    test("mock constructor arguments of type Option, Int and String") {
      implicit val ctx: MockContext = new MockContext
      call(Mock.mock[WithOption])
      call(Mock.mock[WithInt])
      call(Mock.mock[WithString])
      ctx.verifyExpectations()
    }

    test("mock a specialized trait (its concrete method keeps its implementation: documented difference)") {
      // ScalaMock mocks every method, also concrete ones (`(m.identity _).expects(42).returning(43)`); kindlings-mock
      // only mocks abstract members, by design (docs/user-guide/mock.md, "What gets mocked").
      implicit val ctx: MockContext = new MockContext
      val m = Mock.mock[SpecializedClass[Int]]
      m.identity(5) ==> 5
      val a = Mock.mock[AbstractSpecialized[Int]]
      val _ = ctx.expecting("identity", 42).returning(43)
      a.identity(42) ==> 43
      ctx.verifyExpectations()
    }
  }

  group("ScalaMock ReturnTest / OrderingTest") {

    test("match return value to provided arguments (different order)") {
      implicit val ctx: MockContext = new MockContext
      val m = Mock.mock[Members]
      val _ = ctx.expecting("oneParam", 1).returning("one")
      val _ = ctx.expecting("oneParam", 2).returning("two")
      m.oneParam(2) ==> "two"
      m.oneParam(1) ==> "one"
      ctx.verifyExpectations()
    }

    test("handle a sequence in which functions are called zero times") {
      implicit val ctx: MockContext = new MockContext
      val m = Mock.mock[Members]
      ctx.inSequence {
        val _ = ctx.expecting("oneParam", 1).once()
        val _ = ctx.expecting("oneParam", 2).never()
        val _ = ctx.expecting("oneParam", 3).repeat(0 to 1)
        val _ = ctx.expecting("oneParam", 4).once()
      }
      call(m.oneParam(1))
      call(m.oneParam(4))
      ctx.verifyExpectations()
    }

    test("not match a previous item in the sequence") {
      implicit val ctx: MockContext = new MockContext
      val m = Mock.mock[Members]
      ctx.inSequence {
        val _ = ctx.expecting("oneParam", 1).anyNumberOfTimes()
        val _ = ctx.expecting("oneParam", 2).once()
      }
      call(m.oneParam(1))
      call(m.oneParam(2))
      val _ = intercept[MockExpectationException](m.oneParam(1))
    }
  }
}

object ScalaMockParitySpec {

  trait Overloads {
    def overloaded(x: Int): String
    def overloaded(x: String): String
    def overloaded(x: Int, y: Double): String
    def overloaded[T](x: T): String
  }

  trait Members {
    def oneParam(x: Int): String
    def curriedFuncReturn(x: Int): Double => String
    def polycurried[T1, T2](x: T1)(y: T2): (T1, T2)
    def polymorphicParam(x: (Int, Double)): String
    def upperBound[T <: Product](x: T): Int
    def lowerBound[T >: U, U](x: T, y: List[U]): String
    def contextBound[T: Ordering](x: T): String
    def takesSeq(x: Int, ys: Seq[String]): String
    def curriedVarargs(x: Int)(ys: String*): String
    val fnVal: String => Int
  }

  trait WithEmbedded {
    trait Embedded {
      def m(x: Int, y: Double): String
    }
    def referencesEmbedded(): Embedded
  }

  final case class User(first: String, last: String, enabled: Boolean)
  trait DataProviderComponent {
    def find[T: ClassTag](id: Int): Try[T]
  }

  trait FinalMethodTrait {
    def somePublicMethod(param: String): Unit
    final def someFinalMethod(param: Int): String = "final method"
  }

  trait ProtectedMethodTrait {
    def somePublicMethod(param: String): Unit
    protected def protectedMethod(): Unit = ()
  }

  trait HasNotifyMethod {
    def notify(x: Int): Int
  }

  class WithOption(opt: Option[String]) { def get: Option[String] = opt }
  class WithInt(i: Int) { def get: Int = i }
  class WithString(s: String) { def get: String = s }

  // (one abstract member: a trait without any is not mockable on Scala 2, Hearth's `AnonymousInstance` emits
  // `new SpecializedClass[Int]()` without a body there)
  trait SpecializedClass[@specialized T] {
    def identity(x: T): T = x
    def other(): Int
  }
  trait AbstractSpecialized[@specialized T] {
    def identity(x: T): T
  }
}
