package hearth.kindlings.benchmarks

import hearth.kindlings.fastshowpretty.RenderConfig
import org.openjdk.jmh.annotations.*
import java.util.concurrent.TimeUnit

@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
@Warmup(iterations = 5, time = 1)
@Measurement(iterations = 10, time = 1)
@Fork(2)
class FastShowPrettyBenchmark {

  private val config = RenderConfig.Default

  @Benchmark def kindlingsSimpleCC(): String =
    KindlingsFastShowPrettyInstances.simpleCCInstance
      .render(new StringBuilder, config, 0)(BenchmarkData.simpleCC)
      .toString

  @Benchmark def kindlingsPerson(): String =
    KindlingsFastShowPrettyInstances.personInstance
      .render(new StringBuilder, config, 0)(BenchmarkData.person)
      .toString

  @Benchmark def kindlingsEvent(): String =
    KindlingsFastShowPrettyInstances.eventInstance
      .render(new StringBuilder, config, 0)(BenchmarkData.event)
      .toString

  @Benchmark def kindlingsSimpleADT(): String =
    KindlingsFastShowPrettyInstances.simpleADTInstance
      .render(new StringBuilder, config, 0)(BenchmarkData.simpleADT)
      .toString
}
