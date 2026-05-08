package hearth.kindlings.benchmarks

import hearth.kindlings.sconfigderivation.ConfigCodec
import org.ekrich.config.ConfigValue
import org.openjdk.jmh.annotations.*
import java.util.concurrent.TimeUnit

object KindlingsSconfigInstances {
  val simpleCCCodec: ConfigCodec[SimpleCC] = ConfigCodec.derive[SimpleCC]
  val personCodec: ConfigCodec[Person] = ConfigCodec.derive[Person]
  val eventCodec: ConfigCodec[Event] = ConfigCodec.derive[Event]
  val simpleADTCodec: ConfigCodec[SimpleADT] = ConfigCodec.derive[SimpleADT]
}

@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
@Warmup(iterations = 5, time = 1)
@Measurement(iterations = 10, time = 1)
@Fork(2)
class SconfigWriteBenchmark {

  @Benchmark def kindlingsSimpleCC(): ConfigValue =
    KindlingsSconfigInstances.simpleCCCodec.to(BenchmarkData.simpleCC)

  @Benchmark def kindlingsPerson(): ConfigValue =
    KindlingsSconfigInstances.personCodec.to(BenchmarkData.person)

  @Benchmark def kindlingsEvent(): ConfigValue =
    KindlingsSconfigInstances.eventCodec.to(BenchmarkData.event)

  @Benchmark def kindlingsSimpleADT(): ConfigValue =
    KindlingsSconfigInstances.simpleADTCodec.to(BenchmarkData.simpleADT)
}

@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
@Warmup(iterations = 5, time = 1)
@Measurement(iterations = 10, time = 1)
@Fork(2)
class SconfigReadBenchmark {

  private var simpleCCValue: ConfigValue = _
  private var personValue: ConfigValue = _
  private var eventValue: ConfigValue = _
  private var simpleADTValue: ConfigValue = _

  @Setup(Level.Trial)
  def setup(): Unit = {
    simpleCCValue = KindlingsSconfigInstances.simpleCCCodec.to(BenchmarkData.simpleCC)
    personValue = KindlingsSconfigInstances.personCodec.to(BenchmarkData.person)
    eventValue = KindlingsSconfigInstances.eventCodec.to(BenchmarkData.event)
    simpleADTValue = KindlingsSconfigInstances.simpleADTCodec.to(BenchmarkData.simpleADT)
  }

  @Benchmark def kindlingsSimpleCC(): Either[Any, SimpleCC] =
    KindlingsSconfigInstances.simpleCCCodec.from(simpleCCValue)

  @Benchmark def kindlingsPerson(): Either[Any, Person] =
    KindlingsSconfigInstances.personCodec.from(personValue)

  @Benchmark def kindlingsEvent(): Either[Any, Event] =
    KindlingsSconfigInstances.eventCodec.from(eventValue)

  @Benchmark def kindlingsSimpleADT(): Either[Any, SimpleADT] =
    KindlingsSconfigInstances.simpleADTCodec.from(simpleADTValue)
}
