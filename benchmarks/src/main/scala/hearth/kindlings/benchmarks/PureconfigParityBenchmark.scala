package hearth.kindlings.benchmarks

import com.typesafe.config.ConfigValue
import hearth.kindlings.pureconfigderivation.{KindlingsConfigReader, KindlingsConfigWriter}
import pureconfig.{ConfigReader, ConfigWriter}
import org.openjdk.jmh.annotations.*
import java.util.concurrent.TimeUnit

/** Features matched with upstream pureconfig in the parity work: tuples read from / written as lists, `Option` fields
  * written through `WritesMissingKeys` and read through `ReadsMissingKeys`.
  */
final case class PureconfigTuples(endpoint: (String, Int), window: (Int, Long, Boolean), pairs: List[(String, Int)])

final case class PureconfigOptions(
    name: String,
    retries: Option[Int],
    comment: Option[String],
    timeout: Option[Long],
    missing: Option[String]
)

object PureconfigParityData {
  val tuples: PureconfigTuples =
    PureconfigTuples(("localhost", 8080), (10, 60000L, true), List("a" -> 1, "b" -> 2, "c" -> 3))
  val options: PureconfigOptions = PureconfigOptions("service", Some(3), Some("a comment"), None, None)
}

object KindlingsPureconfigParityInstances {
  val tuplesReader: ConfigReader[PureconfigTuples] = KindlingsConfigReader.derived[PureconfigTuples]
  val tuplesWriter: ConfigWriter[PureconfigTuples] = KindlingsConfigWriter.derived[PureconfigTuples]
  val optionsReader: ConfigReader[PureconfigOptions] = KindlingsConfigReader.derived[PureconfigOptions]
  val optionsWriter: ConfigWriter[PureconfigOptions] = KindlingsConfigWriter.derived[PureconfigOptions]
}

@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
@Warmup(iterations = 5, time = 1)
@Measurement(iterations = 10, time = 1)
@Fork(2)
class PureconfigParityWriteBenchmark {

  @Benchmark def kindlingsTuples(): ConfigValue =
    KindlingsPureconfigParityInstances.tuplesWriter.to(PureconfigParityData.tuples)

  @Benchmark def originalTuples(): ConfigValue =
    OriginalPureconfigParityInstances.tuplesWriter.to(PureconfigParityData.tuples)

  @Benchmark def kindlingsOptions(): ConfigValue =
    KindlingsPureconfigParityInstances.optionsWriter.to(PureconfigParityData.options)

  @Benchmark def originalOptions(): ConfigValue =
    OriginalPureconfigParityInstances.optionsWriter.to(PureconfigParityData.options)
}

@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
@Warmup(iterations = 5, time = 1)
@Measurement(iterations = 10, time = 1)
@Fork(2)
class PureconfigParityReadBenchmark {

  private var tuplesValue: ConfigValue = _
  private var optionsValue: ConfigValue = _

  @Setup(Level.Trial)
  def setup(): Unit = {
    tuplesValue = OriginalPureconfigParityInstances.tuplesWriter.to(PureconfigParityData.tuples)
    optionsValue = OriginalPureconfigParityInstances.optionsWriter.to(PureconfigParityData.options)
  }

  @Benchmark def kindlingsTuples(): ConfigReader.Result[PureconfigTuples] =
    KindlingsPureconfigParityInstances.tuplesReader.from(pureconfig.ConfigCursor(tuplesValue, Nil))

  @Benchmark def originalTuples(): ConfigReader.Result[PureconfigTuples] =
    OriginalPureconfigParityInstances.tuplesReader.from(pureconfig.ConfigCursor(tuplesValue, Nil))

  @Benchmark def kindlingsOptions(): ConfigReader.Result[PureconfigOptions] =
    KindlingsPureconfigParityInstances.optionsReader.from(pureconfig.ConfigCursor(optionsValue, Nil))

  @Benchmark def originalOptions(): ConfigReader.Result[PureconfigOptions] =
    OriginalPureconfigParityInstances.optionsReader.from(pureconfig.ConfigCursor(optionsValue, Nil))
}
