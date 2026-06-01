package hearth.kindlings.benchmarks

import com.typesafe.config.ConfigValue
import hearth.kindlings.pureconfigderivation.{KindlingsConfigReader, KindlingsConfigWriter}
import pureconfig.{ConfigReader, ConfigWriter}
import org.openjdk.jmh.annotations.*
import java.util.concurrent.TimeUnit

object KindlingsPureconfigInstances {
  val simpleCCReader: ConfigReader[SimpleCC] = KindlingsConfigReader.derived[SimpleCC]
  val simpleCCWriter: ConfigWriter[SimpleCC] = KindlingsConfigWriter.derived[SimpleCC]
  val personReader: ConfigReader[Person] = KindlingsConfigReader.derived[Person]
  val personWriter: ConfigWriter[Person] = KindlingsConfigWriter.derived[Person]
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

  @Benchmark def originalSemiAutoSimpleCC(): ConfigValue =
    OriginalPureconfigSemiAutoInstances.simpleCCWriter.to(BenchmarkData.simpleCC)

  @Benchmark def kindlingsPerson(): ConfigValue =
    KindlingsPureconfigInstances.personWriter.to(BenchmarkData.person)

  @Benchmark def originalSemiAutoPerson(): ConfigValue =
    OriginalPureconfigSemiAutoInstances.personWriter.to(BenchmarkData.person)
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

  @Benchmark def originalSemiAutoSimpleCC(): ConfigReader.Result[SimpleCC] =
    OriginalPureconfigSemiAutoInstances.simpleCCReader.from(pureconfig.ConfigCursor(simpleCCValue, Nil))

  @Benchmark def kindlingsPerson(): ConfigReader.Result[Person] =
    KindlingsPureconfigInstances.personReader.from(pureconfig.ConfigCursor(personValue, Nil))

  @Benchmark def originalSemiAutoPerson(): ConfigReader.Result[Person] =
    OriginalPureconfigSemiAutoInstances.personReader.from(pureconfig.ConfigCursor(personValue, Nil))
}
