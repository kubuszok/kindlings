package hearth.kindlings.oracle.catstagless

import cats.data.{Cokleisli, EitherT, Kleisli, OptionT}

// Shapes from cats-tagless' own test suites (v0.16.6, `autoFunctorKTests`, `autoInvariantKTests`,
// `autoContravariantKTests`, `autoApplyKTests`, `TestAlgebras`): F nested in other types, concrete members, varargs.
// Shared by both sides of `CatsTaglessShapesOracleSpec`.

trait TgDefaultImpl[F[_]] {
  def plusOne(i: Int): F[Int]
  def minusOne(i: Int): Int = i - 1
}

trait TgTransformers[F[_]] {
  def either(i: Int): EitherT[F, String, Int]
  def kleisli(i: Int): Kleisli[F, String, Int]
  def option(i: Int): OptionT[F, Int]
}

trait TgCokleisliParam[F[_]] {
  def app(f: Cokleisli[F, String, Int])(x: String): F[Int]
  def unsafe(f: Cokleisli[F, Int, String])(x: Int): String
}

trait TgBuilder[F[_]] {
  def unit: F[Unit]
  def withFoo(foo: String): TgBuilder[F]
}

trait TgSafe[F[_]] {
  def parseInt(str: String): F[Int]
  def divide(dividend: Float, divisor: Float): F[Float]
}

trait TgSafeInv[F[_]] {
  def parseInt(fs: F[String]): F[Int]
  def doubleParser(precision: Int): Kleisli[F, String, Double]
  def parseIntOrError(fs: EitherT[F, String, String]): F[Int]
}

trait TgInvVarargs[F[_]] {
  def sum(xs: Int*): Int
  def covariantSum(xs: Int*): F[Int]
  def contravariantSum(xs: F[Int]*): Int
  def invariantSum(xs: F[Int]*): F[Int]
}

trait TgContra[F[_]] {
  def sum(xs: F[Int]): Int
  def sumAll(xss: F[Int]*): Int
  def foldSpecialized(init: String)(f: (Int, String) => Int): Cokleisli[F, String, Int]
}

trait TgCalculator[F[_]] {
  def lit(i: Int): F[Int]
  def add(x: F[Int], y: F[Int]): F[Int]
  def show[A](expr: F[A]): String
}

trait TgApplyKAlg[F[_]] {
  def parseInt(str: String): F[Int]
  def parseDouble(str: String): EitherT[F, String, Double]
  def divide(dividend: Float, divisor: Float): F[Float]
}

/** `F` nested in a result through a type without a `FunctorK`: neither side derives it. */
trait TgListOfF[F[_]] {
  def all: List[F[Int]]
}
