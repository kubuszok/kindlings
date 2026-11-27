package hearth.kindlings.catstaglessderivation

import cats.Show
import cats.data.{Cokleisli, EitherT, Kleisli, OptionT}

/** Algebras from cats-tagless' own test suites (`tests/src/test/scala-{2,3}/cats/tagless/tests`, v0.16.6), each shape
  * in its own algebra. Shared by the in-module parity specs.
  */
object UpstreamAlgebras {

  // autoFunctorKTests
  trait AlgWithNonEffectMethod[F[_]] {
    def a(i: Int): F[Int]
    def b(i: Int): Int
  }

  trait AlgWithDefaultImpl[F[_]] {
    def plusOne(i: Int): F[Int]
    def minusOne(i: Int): Int = i - 1
  }

  trait AlgWithTypeMember[F[_]] {
    type T
    def a(i: Int): F[T]
  }
  object AlgWithTypeMember {
    type Aux[F[_], T0] = AlgWithTypeMember[F] { type T = T0 }
  }
  type AlgWithTypeMemberString[F[_]] = AlgWithTypeMember.Aux[F, String]

  trait AlgWithTypeBound[F[_]] {
    type T <: AlgWithTypeBound.A
    def t: F[T]
  }
  object AlgWithTypeBound {
    sealed abstract class A
    case object B extends A
    case object C extends A
    type Aux[F[_], T0 <: A] = AlgWithTypeBound[F] { type T = T0 }
  }
  type AlgWithTypeBoundB[F[_]] = AlgWithTypeBound.Aux[F, AlgWithTypeBound.B.type]

  trait AlgWithExtraTP[F[_], T] {
    def a(i: Int): F[T]
  }
  type AlgWithExtraTPString[F[_]] = AlgWithExtraTP[F, String]

  trait AlgWithExtraTP2[T, F[_]] {
    def a(i: Int): F[T]
  }
  type AlgWithExtraTP2String[F[_]] = AlgWithExtraTP2[String, F]

  trait Increment[F[_]] {
    def plusOne(i: Int): F[Int]
  }

  trait AlgWithDef[F[_]] {
    def a: F[Int]
  }

  trait AlgWithTParamInMethod[F[_]] {
    def a[T](t: T): F[String]
  }

  trait AlgWithContextBounds[F[_]] {
    def a[T: Show](t: Int): F[String]
  }

  trait AlgWithAbstractTypeClass[F[_]] {
    type TC[T]
    def a[T: TC](t: T): F[String]
  }
  object AlgWithAbstractTypeClass {
    type Aux[F[_], TC0[_]] = AlgWithAbstractTypeClass[F] { type TC[T] = TC0[T] }
  }
  type AlgWithAbstractTypeClassShow[F[_]] = AlgWithAbstractTypeClass.Aux[F, Show]

  trait AlgWithCurryMethod[F[_]] {
    def a(t: Int = 42)(b: String): F[String]
  }

  trait AlgWithDefaultParameter[F[_]] {
    def greet(name: String = "World"): F[String]
  }

  trait AlgWithFinalMethod[F[_]] {
    def log(msg: String): F[String]
    final def info(msg: String): F[String] = log(s"[info] $msg")
    final def warn(msg: String): F[String] = log(s"[warn] $msg")
  }

  trait AlgWithByNameParameter[F[_]] {
    def log(msg: => String): F[String]
  }

  trait AlgWithVarArgsParameter[F[_]] {
    def sum(xs: Int*): Int
    def fSum(xs: Int*): F[Int]
  }

  trait AlgWithValues[F[_]] {
    val int: F[Int]
    lazy val str: F[String] = int.asInstanceOf[F[String]]
  }

  trait BuilderAlgebra[F[_]] {
    def unit: F[Unit]
    def withFoo(foo: String): BuilderAlgebra[F]
  }
  object BuilderAlgebra {
    final case class Named(name: String) extends BuilderAlgebra[List] {
      val unit: List[Unit] = List.fill(5)(())
      def withFoo(foo: String): BuilderAlgebra[List] = copy(name = foo)
    }
  }

  trait AlgWithContravariantK[F[_]] {
    def app(f: Cokleisli[F, String, Int])(x: String): F[Int]
    def unsafe(f: Cokleisli[F, Int, String])(x: Int): String
  }

  /** `F` nested in results through types that have a `FunctorK` (cats-tagless' `InvariantK` companion instances). */
  trait AlgWithTransformers[F[_]] {
    def either(i: Int): EitherT[F, String, Int]
    def kleisli(i: Int): Kleisli[F, String, Int]
    def option(i: Int): OptionT[F, Int]
  }

  /** `F` nested in a result through a type without a `FunctorK`: cats-tagless rejects it at compile time. */
  trait AlgWithListOfF[F[_]] {
    def all: List[F[Int]]
  }

  // TestAlgebras: SafeInvAlg (InvariantK, SemigroupalK) and CalculatorAlg (InvariantK, SemigroupalK)
  trait SafeInvAlg[F[_]] {
    def parseInt(fs: F[String]): F[Int]
    def doubleParser(precision: Int): Kleisli[F, String, Double]
    def parseIntOrError(fs: EitherT[F, String, String]): F[Int]
  }

  trait CalculatorAlg[F[_]] {
    def lit(i: Int): F[Int]
    def add(x: F[Int], y: F[Int]): F[Int]
    def show[A](expr: F[A]): String
  }

  // autoContravariantKTests
  trait ContraTestAlgebra[F[_]] {
    def sum(xs: F[Int]): Int
    def sumAll(xss: F[Int]*): Int
    def foldSpecialized(init: String)(f: (Int, String) => Int): Cokleisli[F, String, Int]
  }

  // autoInvariantKTests
  trait InvAlgWithVarArgsParameter[F[_]] {
    def sum(xs: Int*): Int
    def covariantSum(xs: Int*): F[Int]
    def contravariantSum(xs: F[Int]*): Int
    def invariantSum(xs: F[Int]*): F[Int]
  }

  trait InvAlgWithByNameParameter[F[_]] {
    def whenM(cond: F[Boolean])(action: => F[Unit]): F[Unit]
  }

  trait InvAlgWithDefaultImpl[F[_]] {
    def const(i: F[Int]): F[Int] = i
  }

  // autoSemigroupalKTests
  trait AlgWithConstantReturnTypes[F[_]] {
    def defer[A](x: => A): F[A]
    def unsafeRun[A](t: F[A]): A
    def unsafeRunAll[A](t: F[A]*): Seq[A]
    def toError(t: F[Int]): Exception
  }

  // autoApplyKTests
  trait AutoApplyKTestAlg[F[_]] {
    def parseInt(str: String): F[Int]
    def parseDouble(str: String): EitherT[F, String, Double]
    def divide(dividend: Float, divisor: Float): F[Float]
  }

  trait DepFn[A] {
    type Out
  }
  trait AlgWithDependentType[F[_]] {
    def dependent[A](f: DepFn[A])(out: f.Out): F[A => f.Out]
  }

  // autoInstrumentTests
  trait Lookup[F[_]] {
    def ->(id: String): F[Option[Long]]
  }
}
