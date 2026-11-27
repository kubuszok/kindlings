package hearth.kindlings.benchmarks

import hearth.kindlings.sconfigderivation.{ConfigDecodingError, ConfigReader, ConfigWriter}
import org.ekrich.config.ConfigValue
import org.openjdk.jmh.annotations.*
import java.util.concurrent.TimeUnit

/** The sconfig counterparts of `PureconfigParityBenchmark` (same shapes and values): tuples read from / written as
  * lists, `Option` fields written through `WritesMissingKeys` and read through `ReadsMissingKeys`. sconfig has no
  * derivation of its own, so there is no upstream side; PureConfig's numbers for the same shapes are in
  * `PureconfigParityBenchmark`.
  */
final case class SconfigTuples(endpoint: (String, Int), window: (Int, Long, Boolean), pairs: List[(String, Int)])

final case class SconfigOptions(
    name: String,
    retries: Option[Int],
    comment: Option[String],
    timeout: Option[Long],
    missing: Option[String]
)

object KindlingsSconfigParityInstances {
  val tuples: SconfigTuples =
    SconfigTuples(("localhost", 8080), (10, 60000L, true), List("a" -> 1, "b" -> 2, "c" -> 3))
  val options: SconfigOptions = SconfigOptions("service", Some(3), Some("a comment"), None, None)

  val tuplesReader: ConfigReader[SconfigTuples] = ConfigReader.derived[SconfigTuples]
  val tuplesWriter: ConfigWriter[SconfigTuples] = ConfigWriter.derived[SconfigTuples]
  val optionsReader: ConfigReader[SconfigOptions] = ConfigReader.derived[SconfigOptions]
  val optionsWriter: ConfigWriter[SconfigOptions] = ConfigWriter.derived[SconfigOptions]
}

@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
@Warmup(iterations = 5, time = 1)
@Measurement(iterations = 10, time = 1)
@Fork(2)
class SconfigParityBenchmark {

  private var tuplesValue: ConfigValue = _
  private var optionsValue: ConfigValue = _

  @Setup(Level.Trial)
  def setup(): Unit = {
    tuplesValue = KindlingsSconfigParityInstances.tuplesWriter.to(KindlingsSconfigParityInstances.tuples)
    optionsValue = KindlingsSconfigParityInstances.optionsWriter.to(KindlingsSconfigParityInstances.options)
  }

  @Benchmark def writeTuples(): ConfigValue =
    KindlingsSconfigParityInstances.tuplesWriter.to(KindlingsSconfigParityInstances.tuples)

  @Benchmark def writeOptions(): ConfigValue =
    KindlingsSconfigParityInstances.optionsWriter.to(KindlingsSconfigParityInstances.options)

  @Benchmark def readTuples(): Either[ConfigDecodingError, SconfigTuples] =
    KindlingsSconfigParityInstances.tuplesReader.from(tuplesValue)

  @Benchmark def readOptions(): Either[ConfigDecodingError, SconfigOptions] =
    KindlingsSconfigParityInstances.optionsReader.from(optionsValue)
}
