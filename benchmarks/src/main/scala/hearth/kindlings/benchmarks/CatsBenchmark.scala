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

@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
@Warmup(iterations = 5, time = 1)
@Measurement(iterations = 10, time = 1)
@Fork(2)
class CatsSemigroupBenchmark {

  private val a = BenchmarkData.intPairA
  private val b = BenchmarkData.intPairB

  @Benchmark def kindlingsCombine(): IntPair =
    KindlingsCatsInstances.intPairSemigroup.combine(a, b)

  @Benchmark def originalSemiAutoCombine(): IntPair =
    OriginalCatsSemiAutoInstances.intPairSemigroup.combine(a, b)
}

@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
@Warmup(iterations = 5, time = 1)
@Measurement(iterations = 10, time = 1)
@Fork(2)
class CatsMonoidBenchmark {

  private val a = BenchmarkData.intPairA
  private val b = BenchmarkData.intPairB

  @Benchmark def kindlingsEmpty(): IntPair =
    KindlingsCatsInstances.intPairMonoid.empty

  @Benchmark def originalSemiAutoEmpty(): IntPair =
    OriginalCatsSemiAutoInstances.intPairMonoid.empty

  @Benchmark def kindlingsCombine(): IntPair =
    KindlingsCatsInstances.intPairMonoid.combine(a, b)

  @Benchmark def originalSemiAutoCombine(): IntPair =
    OriginalCatsSemiAutoInstances.intPairMonoid.combine(a, b)
}

@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
@Warmup(iterations = 5, time = 1)
@Measurement(iterations = 10, time = 1)
@Fork(2)
class CatsEmptyBenchmark {

  @Benchmark def kindlingsEmpty(): IntPair =
    KindlingsCatsInstances.intPairEmpty.empty

  @Benchmark def originalSemiAutoEmpty(): IntPair =
    OriginalCatsSemiAutoInstances.intPairEmpty.empty
}

@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
@Warmup(iterations = 5, time = 1)
@Measurement(iterations = 10, time = 1)
@Fork(2)
class CatsShowPrettyBenchmark {

  @Benchmark def kindlingsShowPrettySimpleCC(): String =
    KindlingsCatsInstances.simpleCCShowPretty.show(BenchmarkData.simpleCC)

  @Benchmark def kindlingsShowPrettyPerson(): String =
    KindlingsCatsInstances.personShowPretty.show(BenchmarkData.person)

  @Benchmark def kindlingsShowSimpleCC(): String =
    KindlingsCatsInstances.simpleCCShow.show(BenchmarkData.simpleCC)

  @Benchmark def kindlingsShowPerson(): String =
    KindlingsCatsInstances.personShow.show(BenchmarkData.person)

  @Benchmark def kindlingsFastShowPrettySimpleCC(): String =
    KindlingsFastShowPrettyInstances.simpleCCInstance
      .render(new StringBuilder(128), hearth.kindlings.fastshowpretty.RenderConfig.Default, 0)(BenchmarkData.simpleCC)
      .toString

  @Benchmark def kindlingsFastShowPrettyPerson(): String =
    KindlingsFastShowPrettyInstances.personInstance
      .render(new StringBuilder(1024), hearth.kindlings.fastshowpretty.RenderConfig.Default, 0)(BenchmarkData.person)
      .toString
}

@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
@Warmup(iterations = 5, time = 1)
@Measurement(iterations = 10, time = 1)
@Fork(2)
class CatsFunctorBenchmark {

  private val box = BenchmarkData.simpleCCBox

  @Benchmark def kindlingsSimpleCCBoxMap(): SimpleCCBox[String] =
    KindlingsCatsInstances.simpleCCBoxFunctor.map(box)(_.toString)

  @Benchmark def originalSemiAutoSimpleCCBoxMap(): SimpleCCBox[String] =
    OriginalCatsSemiAutoInstances.simpleCCBoxFunctor.map(box)(_.toString)
}

@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
@Warmup(iterations = 5, time = 1)
@Measurement(iterations = 10, time = 1)
@Fork(2)
class CatsFoldableBenchmark {

  private val box = BenchmarkData.simpleCCBox

  @Benchmark def kindlingsSimpleCCBoxFoldLeft(): Int =
    KindlingsCatsInstances.simpleCCBoxFoldable.foldLeft(box, 0)(_ + _)
}

@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
@Warmup(iterations = 5, time = 1)
@Measurement(iterations = 10, time = 1)
@Fork(2)
class CatsTraverseBenchmark {

  private val box = BenchmarkData.simpleCCBox

  @Benchmark def kindlingsSimpleCCBoxTraverse(): Option[SimpleCCBox[String]] =
    KindlingsCatsInstances.simpleCCBoxTraverse.traverse(box)(v => Option(v.toString))
}
