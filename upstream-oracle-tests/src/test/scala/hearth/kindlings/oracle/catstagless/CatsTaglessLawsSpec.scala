package hearth.kindlings.oracle.catstagless

import cats.Eq
import cats.arrow.FunctionK
import cats.data.{Cokleisli, EitherT, Kleisli, Tuple2K}
import cats.laws.discipline.{ExhaustiveCheck, SerializableTests}
import cats.laws.discipline.arbitrary.*
import cats.laws.discipline.eq.*
import cats.tagless.laws.discipline.{
  ApplyKTests,
  ContravariantKTests,
  FunctorKTests,
  InstrumentTests,
  InvariantKTests,
  SemigroupalKTests
}
import org.scalacheck.{Arbitrary, Cogen, Gen}

import scala.util.Try

/** cats-tagless' discipline laws (`cats-tagless-laws`) for instances derived by cats-tagless-derivation, on the
  * algebras cats-tagless checks its own derivation with (`autoApplyKTests`, `autoInvariantKTests`,
  * `autoSemigroupalKTests`, `autoContravariantKTests`, `TestAlgebras`), including the `SerializableTests` it runs on
  * its instances.
  */
final class CatsTaglessLawsSpec extends munit.DisciplineSuite {
  import CatsTaglessLawsSpec.*

  private val k = TgShapesKindlingsSide
  // `SemigroupalKTests` / `ApplyKTests` build their isomorphisms from an implicit `InvariantK`.
  implicit private val applyKAlgInvariantK: cats.tagless.InvariantK[TgApplyKAlg] = k.applyKAlg
  implicit private val safeInvInvariantK: cats.tagless.InvariantK[TgSafeInv] = k.invariantKSafeInv
  implicit private val calculatorInvariantK0: cats.tagless.InvariantK[TgCalculator] = calculatorInvariantK

  // autoFunctorKTests / autoSemigroupalKTests / autoInstrumentTests on cats-tagless' SafeAlg
  implicit private val safeInvariantK: cats.tagless.InvariantK[TgSafe] = safeFunctorK
  checkAll("FunctorK[TgSafe]", FunctorKTests[TgSafe](safeFunctorK).functorK[Try, Option, List, Int])
  checkAll("FunctorK is Serializable", SerializableTests.serializable(safeFunctorK))
  checkAll("SemigroupalK[TgSafe]", SemigroupalKTests[TgSafe](safeSemigroupalK).semigroupalK[Try, Option, List])
  checkAll("Instrument[TgSafe]", InstrumentTests[TgSafe](safeInstrument).instrument[Try, Option, List, Int])
  checkAll("Instrument is Serializable", SerializableTests.serializable(safeInstrument))

  checkAll("ApplyK[TgApplyKAlg]", ApplyKTests[TgApplyKAlg](k.applyKAlg).applyK[Try, Option, List, Int])
  checkAll("ApplyK is Serializable", SerializableTests.serializable(k.applyKAlg))
  checkAll("InvariantK[TgSafeInv]", InvariantKTests[TgSafeInv](k.invariantKSafeInv).invariantK[Try, Option, List])
  checkAll(
    "InvariantK[TgCalculator]",
    InvariantKTests[TgCalculator](calculatorInvariantK).invariantK[Try, Option, List]
  )
  checkAll("InvariantK is Serializable", SerializableTests.serializable(k.invariantKSafeInv))
  checkAll(
    "SemigroupalK[TgSafeInv]",
    SemigroupalKTests[TgSafeInv](k.semigroupalKSafeInv).semigroupalK[Try, Option, List]
  )
  checkAll(
    "SemigroupalK[TgCalculator]",
    SemigroupalKTests[TgCalculator](k.semigroupalKCalculator).semigroupalK[Try, Option, List]
  )
  checkAll("SemigroupalK is Serializable", SerializableTests.serializable(k.semigroupalKCalculator))
  checkAll(
    "ContravariantK[TgContra]",
    ContravariantKTests[TgContra](k.contravariantKContra).contravariantK[Try, Option, List, Int]
  )
  checkAll("ContravariantK is Serializable", SerializableTests.serializable(k.contravariantKContra))
}
object CatsTaglessLawsSpec {
  import hearth.kindlings.catstaglessderivation.KindlingsInvariantK

  val calculatorInvariantK: cats.tagless.InvariantK[TgCalculator] = KindlingsInvariantK.derived[TgCalculator]
  val safeFunctorK: cats.tagless.FunctorK[TgSafe] =
    hearth.kindlings.catstaglessderivation.KindlingsFunctorK.derived[TgSafe]
  val safeSemigroupalK: cats.tagless.SemigroupalK[TgSafe] =
    hearth.kindlings.catstaglessderivation.KindlingsSemigroupalK.derived[TgSafe]
  val safeInstrument: cats.tagless.aop.Instrument[TgSafe] =
    hearth.kindlings.catstaglessderivation.KindlingsInstrument.derived[TgSafe]

  // --- TgSafe (cats-tagless' SafeAlg) ---

  implicit def eqSafe[F[_]](implicit a: Eq[F[Int]], b: Eq[F[Float]]): Eq[TgSafe[F]] =
    Eq.by(alg => (alg.parseInt _, (alg.divide _).tupled))

  implicit def arbitrarySafe[F[_]](implicit a: Arbitrary[F[Int]], b: Arbitrary[F[Float]]): Arbitrary[TgSafe[F]] =
    Arbitrary(for {
      parseIntF <- Arbitrary.arbitrary[String => F[Int]]
      divideF <- Arbitrary.arbitrary[((Float, Float)) => F[Float]]
    } yield new TgSafe[F] {
      def parseInt(str: String) = parseIntF(str)
      def divide(dividend: Float, divisor: Float) = divideF((dividend, divisor))
    })

  // --- cats-tagless' TestInstances ---

  implicit def arbitraryFunctionKId[F[_]]: Arbitrary[FunctionK[F, F]] = Arbitrary(Gen.const(FunctionK.id))
  implicit val arbitraryOptionList: Arbitrary[FunctionK[Option, List]] =
    Arbitrary(Gen.const(new FunctionK[Option, List] { def apply[A](fa: Option[A]): List[A] = fa.toList }))
  implicit val arbitraryListOption: Arbitrary[FunctionK[List, Option]] =
    Arbitrary(Gen.const(new FunctionK[List, Option] { def apply[A](fa: List[A]): Option[A] = fa.headOption }))
  implicit val arbitraryTryOption: Arbitrary[FunctionK[Try, Option]] =
    Arbitrary(Gen.const(new FunctionK[Try, Option] { def apply[A](fa: Try[A]): Option[A] = fa.toOption }))
  implicit val arbitraryOptionTry: Arbitrary[FunctionK[Option, Try]] =
    Arbitrary(Gen.const(new FunctionK[Option, Try] { def apply[A](fa: Option[A]): Try[A] = Try(fa.get) }))

  implicit val eqThrowable: Eq[Throwable] = Eq.allEqual

  implicit def exhaustiveCheckForArbitrary[A: Arbitrary]: ExhaustiveCheck[A] =
    ExhaustiveCheck.instance(Gen.resize(30, Arbitrary.arbitrary[List[A]]).sample.get)

  implicit def eqForKleisli[F[_], A, B](implicit ev: Eq[A => F[B]]): Eq[Kleisli[F, A, B]] = Eq.by(_.run)
  implicit def eqForCokleisli[F[_], A, B](implicit ev: Eq[F[A] => B]): Eq[Cokleisli[F, A, B]] = Eq.by(_.run)
  implicit def eqForTuple2K[F[_], G[_], A](implicit fa: Eq[F[A]], ga: Eq[G[A]]): Eq[Tuple2K[F, G, A]] =
    Eq.by(t2k => (t2k.first, t2k.second))

  // --- TgApplyKAlg (cats-tagless' AutoApplyKTestAlg) ---

  implicit def eqApplyKAlg[F[_]](implicit
      a: Eq[F[Int]],
      b: Eq[F[Float]],
      c: Eq[EitherT[F, String, Double]]
  ): Eq[TgApplyKAlg[F]] =
    Eq.by(alg => (alg.parseInt _, alg.parseDouble _, (alg.divide _).tupled))

  implicit def arbitraryApplyKAlg[F[_]](implicit
      a: Arbitrary[F[Int]],
      b: Arbitrary[F[Float]],
      c: Arbitrary[EitherT[F, String, Double]]
  ): Arbitrary[TgApplyKAlg[F]] = Arbitrary(for {
    pInt <- Arbitrary.arbitrary[String => F[Int]]
    pDouble <- Arbitrary.arbitrary[String => EitherT[F, String, Double]]
    div <- Arbitrary.arbitrary[((Float, Float)) => F[Float]]
  } yield new TgApplyKAlg[F] {
    def parseInt(str: String) = pInt(str)
    def parseDouble(str: String) = pDouble(str)
    def divide(dividend: Float, divisor: Float) = div((dividend, divisor))
  })

  // --- TgSafeInv (cats-tagless' SafeInvAlg) ---

  implicit def eqSafeInv[F[_]](implicit
      a: Eq[F[Int]],
      b: Eq[F[Double]],
      c: ExhaustiveCheck[F[String]],
      d: ExhaustiveCheck[EitherT[F, String, String]]
  ): Eq[TgSafeInv[F]] = Eq.by(alg => (alg.parseInt _, alg.doubleParser _, alg.parseIntOrError _))

  implicit def arbitrarySafeInv[F[_]](implicit
      a: Cogen[F[String]],
      b: Cogen[EitherT[F, String, String]],
      c: Arbitrary[F[Int]],
      d: Arbitrary[F[Double]]
  ): Arbitrary[TgSafeInv[F]] = Arbitrary(for {
    parseIntF <- Arbitrary.arbitrary[F[String] => F[Int]]
    doubleParserF <- Arbitrary.arbitrary[Int => Kleisli[F, String, Double]]
    parseIntOrErrorF <- Arbitrary.arbitrary[EitherT[F, String, String] => F[Int]]
  } yield new TgSafeInv[F] {
    def parseInt(fs: F[String]) = parseIntF(fs)
    def doubleParser(precision: Int) = doubleParserF(precision)
    def parseIntOrError(fs: EitherT[F, String, String]) = parseIntOrErrorF(fs)
  })

  // --- TgCalculator (cats-tagless' CalculatorAlg) ---

  implicit def eqCalculator[F[_]](implicit a: Eq[F[Int]], b: ExhaustiveCheck[F[Int]]): Eq[TgCalculator[F]] =
    Eq.by(alg => (alg.lit _, (alg.add _).tupled, alg.show[Int] _))

  implicit def arbitraryCalculator[F[_]](implicit a: Cogen[F[Int]], b: Arbitrary[F[Int]]): Arbitrary[TgCalculator[F]] =
    Arbitrary(for {
      litF <- Arbitrary.arbitrary[Int => F[Int]]
      addF <- Arbitrary.arbitrary[((F[Int], F[Int])) => F[Int]]
    } yield new TgCalculator[F] {
      def lit(i: Int) = litF(i)
      def add(x: F[Int], y: F[Int]) = addF((x, y))
      def show[A](expr: F[A]) = expr.toString
    })

  // --- TgContra (cats-tagless' autoContravariantKTests.TestAlgebra) ---

  implicit def eqContra[F[_]](implicit a: Arbitrary[F[Int]], b: Arbitrary[F[String]]): Eq[TgContra[F]] = {
    implicit val exInt: ExhaustiveCheck[F[Int]] = exhaustiveCheckForArbitrary[F[Int]]
    implicit val exSeq: ExhaustiveCheck[Seq[F[Int]]] =
      ExhaustiveCheck.instance(Gen.resize(5, Arbitrary.arbitrary[List[List[F[Int]]]]).sample.get.map(_.toSeq))
    Eq.by(alg =>
      (
        alg.sum _,
        (xs: Seq[F[Int]]) => alg.sumAll(xs*),
        (init: String) => alg.foldSpecialized(init)((i, s) => i + s.length)
      )
    )
  }

  implicit def arbitraryContra[F[_]](implicit
      a: Arbitrary[F[String]],
      b: Cogen[F[Int]],
      c: Cogen[F[String]]
  ): Arbitrary[TgContra[F]] = Arbitrary(for {
    s <- Arbitrary.arbitrary[F[Int] => Int]
    sa <- Arbitrary.arbitrary[Seq[F[Int]] => Int]
    fs <- Arbitrary.arbitrary[String => Cokleisli[F, String, Int]]
  } yield new TgContra[F] {
    def sum(xs: F[Int]) = s(xs)
    def sumAll(xss: F[Int]*) = sa(xss)
    def foldSpecialized(init: String)(f: (Int, String) => Int) = fs(init)
  })
}
