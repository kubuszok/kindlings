package hearth.kindlings.benchmarks

import org.openjdk.jmh.annotations.*
import java.util.concurrent.TimeUnit

@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
@Warmup(iterations = 5, time = 1)
@Measurement(iterations = 10, time = 1)
@Fork(2)
class CatsShowPrettyKittensBenchmark {

  @Benchmark def kittensShowPrettySimpleCC(): String =
    OriginalCatsSemiAutoInstances.simpleCCShowPretty.show(BenchmarkData.simpleCC)

  @Benchmark def kittensShowPrettyPerson(): String =
    OriginalCatsSemiAutoInstances.personShowPretty.show(BenchmarkData.person)
}

@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
@Warmup(iterations = 5, time = 1)
@Measurement(iterations = 10, time = 1)
@Fork(2)
class CatsFoldableKittensBenchmark {

  private val box = BenchmarkData.simpleCCBox

  @Benchmark def kittensFoldLeft(): Int =
    OriginalCatsSemiAutoInstances.simpleCCBoxFoldable.foldLeft(box, 0)(_ + _)
}

@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
@Warmup(iterations = 5, time = 1)
@Measurement(iterations = 10, time = 1)
@Fork(2)
class CatsTraverseKittensBenchmark {

  private val box = BenchmarkData.simpleCCBox

  @Benchmark def kittensTraverse(): Option[SimpleCCBox[String]] =
    OriginalCatsSemiAutoInstances.simpleCCBoxTraverse.traverse(box)(v => Option(v.toString))
}
