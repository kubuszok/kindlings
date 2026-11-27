package hearth.kindlings.benchmarks

import cats.{Bifoldable, Bifunctor, Bitraverse, Foldable, Functor, Traverse}
import org.openjdk.jmh.annotations.*
import java.util.concurrent.TimeUnit

// kittens' `ADTs.Result` (fields nesting the type parameters) and `ADTs.AtLeastOne` (recursion through `Option`): the
// shapes cats-derivation derives with its general (erased) derivation. Bifunctor & co. are derived by kittens on
// Scala 3 only.
sealed trait BenchResult[+A, +E]
case object BenchDone extends BenchResult[Nothing, Nothing]
final case class BenchMaybe[+A, +E](either: Either[A, E]) extends BenchResult[A, E]
final case class BenchOk[+A](value: A, code: Int) extends BenchResult[A, Nothing]
final case class BenchErr[+E](err: E, errs: List[E]) extends BenchResult[Nothing, E]

sealed trait BenchAtLeastOne[+A]
final case class BenchSingle[+A](value: A) extends BenchAtLeastOne[A]
final case class BenchMore[+A](value: A, rest: Option[BenchAtLeastOne[A]]) extends BenchAtLeastOne[A]

object CatsNestedInstances {
  object kindlings {
    import hearth.kindlings.catsderivation.extensions.*
    val bifunctor: Bifunctor[BenchResult] = Bifunctor.derived[BenchResult]
    val bifoldable: Bifoldable[BenchResult] = Bifoldable.derived[BenchResult]
    val bitraverse: Bitraverse[BenchResult] = Bitraverse.derived[BenchResult]
    val functor: Functor[BenchAtLeastOne] = Functor.derived[BenchAtLeastOne]
    val foldable: Foldable[BenchAtLeastOne] = Foldable.derived[BenchAtLeastOne]
    val traverse: Traverse[BenchAtLeastOne] = Traverse.derived[BenchAtLeastOne]
  }
  object kittens {
    import cats.derived.semiauto
    val bifunctor: Bifunctor[BenchResult] = semiauto.bifunctor[BenchResult]
    val bifoldable: Bifoldable[BenchResult] = semiauto.bifoldable[BenchResult]
    val bitraverse: Bitraverse[BenchResult] = semiauto.bitraverse[BenchResult]
    val functor: Functor[BenchAtLeastOne] = semiauto.functor[BenchAtLeastOne]
    val foldable: Foldable[BenchAtLeastOne] = semiauto.foldable[BenchAtLeastOne]
    val traverse: Traverse[BenchAtLeastOne] = semiauto.traverse[BenchAtLeastOne]
  }

  val results: Array[BenchResult[Int, String]] = Array(
    BenchDone,
    BenchMaybe(Left(1)),
    BenchMaybe(Right("ab")),
    BenchOk(2, 7),
    BenchErr("x", List("yy", "zzz", "w"))
  )
  val atLeastOne: BenchAtLeastOne[Int] =
    (1 to 8).foldRight[BenchAtLeastOne[Int]](BenchSingle(0))((i, rest) => BenchMore(i, Some(rest)))
}

@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
@Warmup(iterations = 5, time = 1)
@Measurement(iterations = 10, time = 1)
@Fork(2)
class CatsNestedBenchmark {
  import CatsNestedInstances.*

  private def bimapAll(bf: Bifunctor[BenchResult]): Int = {
    var acc = 0
    var i = 0
    while (i < results.length) {
      acc += bf.bimap(results(i))(_ + 1, _.length).hashCode
      i += 1
    }
    acc
  }
  private def bifoldAll(bf: Bifoldable[BenchResult]): Int = {
    var acc = 0
    var i = 0
    while (i < results.length) {
      acc += bf.bifoldLeft(results(i), 0)(_ + _, _ + _.length)
      i += 1
    }
    acc
  }
  private def bitraverseAll(bt: Bitraverse[BenchResult]): Int = {
    var acc = 0
    var i = 0
    while (i < results.length) {
      acc += bt.bitraverse(results(i))(a => Option(a + 1), b => Option(b.length)).hashCode
      i += 1
    }
    acc
  }

  @Benchmark def kindlingsBimap(): Int = bimapAll(kindlings.bifunctor)
  @Benchmark def kittensBimap(): Int = bimapAll(kittens.bifunctor)
  @Benchmark def kindlingsBifoldLeft(): Int = bifoldAll(kindlings.bifoldable)
  @Benchmark def kittensBifoldLeft(): Int = bifoldAll(kittens.bifoldable)
  @Benchmark def kindlingsBitraverse(): Int = bitraverseAll(kindlings.bitraverse)
  @Benchmark def kittensBitraverse(): Int = bitraverseAll(kittens.bitraverse)

  @Benchmark def kindlingsRecursiveMap(): BenchAtLeastOne[Int] = kindlings.functor.map(atLeastOne)(_ + 1)
  @Benchmark def kittensRecursiveMap(): BenchAtLeastOne[Int] = kittens.functor.map(atLeastOne)(_ + 1)
  @Benchmark def kindlingsRecursiveFoldLeft(): Int = kindlings.foldable.foldLeft(atLeastOne, 0)(_ + _)
  @Benchmark def kittensRecursiveFoldLeft(): Int = kittens.foldable.foldLeft(atLeastOne, 0)(_ + _)
  @Benchmark def kindlingsRecursiveTraverse(): Option[BenchAtLeastOne[Int]] =
    kindlings.traverse.traverse(atLeastOne)(a => Option(a + 1))
  @Benchmark def kittensRecursiveTraverse(): Option[BenchAtLeastOne[Int]] =
    kittens.traverse.traverse(atLeastOne)(a => Option(a + 1))
}
