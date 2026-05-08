package hearth.kindlings.benchmarks

import com.typesafe.config.ConfigValue
import hearth.kindlings.pureconfigderivation.{KindlingsConfigReader, KindlingsConfigWriter}
import pureconfig.{ConfigReader, ConfigWriter}
import org.openjdk.jmh.annotations._
import java.util.concurrent.TimeUnit

object KindlingsPureconfigInstances {
  val simpleCCReader: ConfigReader[SimpleCC] = KindlingsConfigReader.derive[SimpleCC]
  val simpleCCWriter: ConfigWriter[SimpleCC] = KindlingsConfigWriter.derive[SimpleCC]
  val personReader: ConfigReader[Person] = KindlingsConfigReader.derive[Person]
  val personWriter: ConfigWriter[Person] = KindlingsConfigWriter.derive[Person]
}

@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
@Warmup(iterations = 5, time = 1)
@Measurement(iterations = 10, time = 1)
@Fork(2)
class PureconfigWriteBenchmark {

  @Benchmark def kindlingsSimpleCC(): ConfigValue =
    KindlingsPureconfigInstances.simpleCCWriter.to(BenchmarkData.simpleCC)

  @Benchmark def originalSimpleCC(): ConfigValue =
    OriginalPureconfigInstances.simpleCCWriter.to(BenchmarkData.simpleCC)

  @Benchmark def kindlingsPerson(): ConfigValue =
    KindlingsPureconfigInstances.personWriter.to(BenchmarkData.person)

  @Benchmark def originalPerson(): ConfigValue =
    OriginalPureconfigInstances.personWriter.to(BenchmarkData.person)
}

@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
@Warmup(iterations = 5, time = 1)
@Measurement(iterations = 10, time = 1)
@Fork(2)
class PureconfigReadBenchmark {

  private var simpleCCValue: ConfigValue = _
  private var personValue: ConfigValue = _

  @Setup(Level.Trial)
  def setup(): Unit = {
    simpleCCValue = KindlingsPureconfigInstances.simpleCCWriter.to(BenchmarkData.simpleCC)
    personValue = KindlingsPureconfigInstances.personWriter.to(BenchmarkData.person)
  }

  @Benchmark def kindlingsSimpleCC(): ConfigReader.Result[SimpleCC] =
    KindlingsPureconfigInstances.simpleCCReader.from(pureconfig.ConfigCursor(simpleCCValue, Nil))

  @Benchmark def originalSimpleCC(): ConfigReader.Result[SimpleCC] =
    OriginalPureconfigInstances.simpleCCReader.from(pureconfig.ConfigCursor(simpleCCValue, Nil))

  @Benchmark def kindlingsPerson(): ConfigReader.Result[Person] =
    KindlingsPureconfigInstances.personReader.from(pureconfig.ConfigCursor(personValue, Nil))

  @Benchmark def originalPerson(): ConfigReader.Result[Person] =
    OriginalPureconfigInstances.personReader.from(pureconfig.ConfigCursor(personValue, Nil))
}
