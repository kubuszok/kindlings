package hearth.kindlings.benchmarks

import org.openjdk.jmh.annotations.*
import java.util.concurrent.TimeUnit

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
