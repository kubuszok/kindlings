package hearth.kindlings.benchmarks

import org.openjdk.jmh.annotations.*
import java.util.concurrent.TimeUnit

@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
@Warmup(iterations = 5, time = 1)
@Measurement(iterations = 10, time = 1)
@Fork(2)
class CatsShowBenchmark {

  @Benchmark def kindlingsSimpleCC(): String =
    KindlingsCatsInstances.simpleCCShow.show(BenchmarkData.simpleCC)

  @Benchmark def originalSemiAutoSimpleCC(): String =
    OriginalCatsSemiAutoInstances.simpleCCShow.show(BenchmarkData.simpleCC)

  @Benchmark def originalAutoSimpleCC(): String =
    OriginalCatsAutoInstances.simpleCCShow.show(BenchmarkData.simpleCC)

  @Benchmark def kindlingsPerson(): String =
    KindlingsCatsInstances.personShow.show(BenchmarkData.person)

  @Benchmark def originalAutoPerson(): String =
    OriginalCatsAutoInstances.personShow.show(BenchmarkData.person)

  @Benchmark def kindlingsSimpleADT(): String =
    KindlingsCatsInstances.simpleADTShow.show(BenchmarkData.simpleADT)

  @Benchmark def originalSemiAutoSimpleADT(): String =
    OriginalCatsSemiAutoInstances.simpleADTShow.show(BenchmarkData.simpleADT)

  @Benchmark def originalAutoSimpleADT(): String =
    OriginalCatsAutoInstances.simpleADTShow.show(BenchmarkData.simpleADT)

  @Benchmark def kindlingsEvent(): String =
    KindlingsCatsInstances.eventShow.show(BenchmarkData.event)

  @Benchmark def originalSemiAutoEvent(): String =
    OriginalCatsSemiAutoInstances.eventShow.show(BenchmarkData.event)

  @Benchmark def originalAutoEvent(): String =
    OriginalCatsAutoInstances.eventShow.show(BenchmarkData.event)
}

@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
@Warmup(iterations = 5, time = 1)
@Measurement(iterations = 10, time = 1)
@Fork(2)
class CatsEqBenchmark {

  private val simpleCCPair = (BenchmarkData.simpleCC, BenchmarkData.simpleCC.copy(name = "Bob"))

  @Benchmark def kindlingsSimpleCCEqual(): Boolean =
    KindlingsCatsInstances.simpleCCEq.eqv(BenchmarkData.simpleCC, BenchmarkData.simpleCC)

  @Benchmark def kindlingsSimpleCCNotEqual(): Boolean =
    KindlingsCatsInstances.simpleCCEq.eqv(simpleCCPair._1, simpleCCPair._2)

  @Benchmark def originalSemiAutoSimpleCCEqual(): Boolean =
    OriginalCatsSemiAutoInstances.simpleCCEq.eqv(BenchmarkData.simpleCC, BenchmarkData.simpleCC)

  @Benchmark def originalAutoSimpleCCEqual(): Boolean =
    OriginalCatsAutoInstances.simpleCCEq.eqv(BenchmarkData.simpleCC, BenchmarkData.simpleCC)
}

@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
@Warmup(iterations = 5, time = 1)
@Measurement(iterations = 10, time = 1)
@Fork(2)
class CatsHashBenchmark {

  @Benchmark def kindlingsSimpleCC(): Int =
    KindlingsCatsInstances.simpleCCHash.hash(BenchmarkData.simpleCC)

  @Benchmark def originalSemiAutoSimpleCC(): Int =
    OriginalCatsSemiAutoInstances.simpleCCHash.hash(BenchmarkData.simpleCC)

  @Benchmark def originalAutoSimpleCC(): Int =
    OriginalCatsAutoInstances.simpleCCHash.hash(BenchmarkData.simpleCC)
}

@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
@Warmup(iterations = 5, time = 1)
@Measurement(iterations = 10, time = 1)
@Fork(2)
class CatsOrderBenchmark {

  private val simpleCCPair = (BenchmarkData.simpleCC, BenchmarkData.simpleCC.copy(name = "Bob"))

  @Benchmark def kindlingsSimpleCCCompare(): Int =
    KindlingsCatsInstances.simpleCCOrder.compare(simpleCCPair._1, simpleCCPair._2)

  @Benchmark def originalSemiAutoSimpleCCCompare(): Int =
    OriginalCatsSemiAutoInstances.simpleCCOrder.compare(simpleCCPair._1, simpleCCPair._2)

  @Benchmark def originalAutoSimpleCCCompare(): Int =
    OriginalCatsAutoInstances.simpleCCOrder.compare(simpleCCPair._1, simpleCCPair._2)
}
