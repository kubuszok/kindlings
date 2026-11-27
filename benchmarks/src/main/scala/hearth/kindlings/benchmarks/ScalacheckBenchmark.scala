package hearth.kindlings.benchmarks

import org.openjdk.jmh.annotations.*
import java.util.concurrent.TimeUnit

import org.scalacheck.{Arbitrary, Gen}
import org.scalacheck.rng.Seed

/** What scalacheck-derived (and hand-written code) generates for `Person`: ScalaCheck's own instances for every field.
  * Kindlings generates exactly the same values for the same seed (checked by `ScalaCheckOracleSpec`).
  */
object ScalacheckHandWrittenInstances {
  import Arbitrary.arbitrary

  val addressGen: Gen[Address] = for {
    street <- arbitrary[String]
    city <- arbitrary[String]
    zip <- arbitrary[String]
    country <- arbitrary[String]
  } yield Address(street, city, zip, country)

  val personArbitrary: Arbitrary[Person] = Arbitrary(for {
    name <- arbitrary[String]
    age <- arbitrary[Int]
    email <- arbitrary[Option[String]]
    addresses <- Gen.listOf(addressGen)
    tags <- arbitrary[Map[String, String]]
    scores <- arbitrary[Vector[Double]]
  } yield Person(name, age, email, addresses, tags, scores))

  val params: Gen.Parameters = Gen.Parameters.default.withSize(10)
  val seed: Seed = Seed(42L)
}

@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
@Warmup(iterations = 5, time = 1)
@Measurement(iterations = 10, time = 1)
@Fork(2)
class ScalacheckArbitraryBenchmark {

  @Benchmark def kindlingsSimpleCC(): Option[SimpleCC] =
    KindlingsScalacheckInstances.simpleCCArbitrary.arbitrary.sample

  @Benchmark def kindlingsPerson(): Option[Person] =
    KindlingsScalacheckInstances.personArbitrary.arbitrary.sample

  @Benchmark def kindlingsSimpleADT(): Option[SimpleADT] =
    KindlingsScalacheckInstances.simpleADTArbitrary.arbitrary.sample

  // Same seed, same values: the cost of generating one `Person` (Option, List of case classes, Map, Vector fields).
  @Benchmark def kindlingsPersonFixedSeed(): Option[Person] =
    KindlingsScalacheckInstances.personArbitrary.arbitrary(
      ScalacheckHandWrittenInstances.params,
      ScalacheckHandWrittenInstances.seed
    )

  @Benchmark def scalacheckPersonFixedSeed(): Option[Person] =
    ScalacheckHandWrittenInstances.personArbitrary.arbitrary(
      ScalacheckHandWrittenInstances.params,
      ScalacheckHandWrittenInstances.seed
    )
}

@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
@Warmup(iterations = 5, time = 1)
@Measurement(iterations = 10, time = 1)
@Fork(2)
class ScalacheckShrinkBenchmark {

  @Benchmark def kindlingsSimpleCC(): LazyList[SimpleCC] =
    KindlingsScalacheckInstances.simpleCCShrink.shrink(BenchmarkData.simpleCC).to(LazyList).take(10)

  @Benchmark def kindlingsPerson(): LazyList[Person] =
    KindlingsScalacheckInstances.personShrink.shrink(BenchmarkData.person).to(LazyList).take(10)
}
