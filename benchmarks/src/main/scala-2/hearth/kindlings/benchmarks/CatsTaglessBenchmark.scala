package hearth.kindlings.benchmarks

import cats.arrow.FunctionK
import cats.data.{EitherT, Kleisli, Tuple2K}
import cats.tagless.{FunctorK, SemigroupalK}
import org.openjdk.jmh.annotations._

import java.util.concurrent.TimeUnit

/** A trait algebra with the member shapes cats-tagless' tests use: `F[A]`, `F` nested in `EitherT` / `Kleisli` (through
  * their `FunctorK` / `SemigroupalK`) and a member without `F`. Benchmarked on Scala 2.13, where cats-tagless'
  * `Derive.*` macros are not `@experimental`.
  */
trait TaglessBenchAlg[F[_]] {
  def get(i: Int): F[Int]
  def either(i: Int): EitherT[F, String, Int]
  def kleisli(i: Int): Kleisli[F, String, Int]
  def plain(i: Int): Int
}

object TaglessBenchInstances {
  val base: TaglessBenchAlg[Option] = new TaglessBenchAlg[Option] {
    def get(i: Int): Option[Int] = Some(i + 1)
    def either(i: Int): EitherT[Option, String, Int] = EitherT.rightT[Option, String](i)
    def kleisli(i: Int): Kleisli[Option, String, Int] = Kleisli((s: String) => Some(s.length + i))
    def plain(i: Int): Int = i * 2
  }
  val toList: FunctionK[Option, List] = new FunctionK[Option, List] {
    def apply[A](fa: Option[A]): List[A] = fa.toList
  }

  object kindlings {
    import hearth.kindlings.catstaglessderivation._
    val functorK: FunctorK[TaglessBenchAlg] = KindlingsFunctorK.derived[TaglessBenchAlg]
    val semigroupalK: SemigroupalK[TaglessBenchAlg] = KindlingsSemigroupalK.derived[TaglessBenchAlg]
  }
  object catsTagless {
    import cats.tagless.Derive
    val functorK: FunctorK[TaglessBenchAlg] = Derive.functorK[TaglessBenchAlg]
    val semigroupalK: SemigroupalK[TaglessBenchAlg] = Derive.semigroupalK[TaglessBenchAlg]
  }
}

@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
@Warmup(iterations = 5, time = 1)
@Measurement(iterations = 10, time = 1)
@Fork(2)
class CatsTaglessBenchmark {
  import TaglessBenchInstances._

  private val kindlingsMapped = kindlings.functorK.mapK(base)(toList)
  private val catsTaglessMapped = catsTagless.functorK.mapK(base)(toList)
  private val kindlingsProduct = kindlings.semigroupalK.productK(base, base)
  private val catsTaglessProduct = catsTagless.semigroupalK.productK(base, base)

  private def callAll(alg: TaglessBenchAlg[List]): Int =
    alg.get(1).head + alg.either(2).value.head.getOrElse(0) + alg.kleisli(3).run("ab").head + alg.plain(4)

  private def callAllProduct(alg: TaglessBenchAlg[Tuple2K[Option, Option, *]]): Int =
    alg.get(1).first.get + alg.either(2).value.second.get.getOrElse(0) + alg.kleisli(3).run("ab").first.get +
      alg.plain(4)

  /** Calls on an algebra transformed once (the usual way). */
  @Benchmark def kindlingsMapKCalls(): Int = callAll(kindlingsMapped)
  @Benchmark def catsTaglessMapKCalls(): Int = callAll(catsTaglessMapped)

  /** Transforming the algebra and calling it once. */
  @Benchmark def kindlingsMapKCreateAndCall(): Int = callAll(kindlings.functorK.mapK(base)(toList))
  @Benchmark def catsTaglessMapKCreateAndCall(): Int = callAll(catsTagless.functorK.mapK(base)(toList))

  @Benchmark def kindlingsGet(): Int = kindlingsMapped.get(1).head
  @Benchmark def catsTaglessGet(): Int = catsTaglessMapped.get(1).head
  @Benchmark def kindlingsEither(): Int = kindlingsMapped.either(2).value.head.getOrElse(0)
  @Benchmark def catsTaglessEither(): Int = catsTaglessMapped.either(2).value.head.getOrElse(0)
  @Benchmark def kindlingsKleisli(): Int = kindlingsMapped.kleisli(3).run("ab").head
  @Benchmark def catsTaglessKleisli(): Int = catsTaglessMapped.kleisli(3).run("ab").head
  @Benchmark def kindlingsPlain(): Int = kindlingsMapped.plain(4)
  @Benchmark def catsTaglessPlain(): Int = catsTaglessMapped.plain(4)

  @Benchmark def kindlingsProductKCalls(): Int = callAllProduct(kindlingsProduct)
  @Benchmark def catsTaglessProductKCalls(): Int = callAllProduct(catsTaglessProduct)
}
