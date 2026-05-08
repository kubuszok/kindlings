package hearth.kindlings.benchmarks

import com.github.plokhotnyuk.jsoniter_scala.core._
import org.openjdk.jmh.annotations._
import java.util.concurrent.TimeUnit

@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
@Warmup(iterations = 5, time = 1)
@Measurement(iterations = 10, time = 1)
@Fork(2)
class JsoniterWriteBenchmark {

  @Benchmark def kindlingsSemiAutoSimpleCC(): Array[Byte] =
    writeToArray(BenchmarkData.simpleCC)(KindlingsJsoniterInstances.simpleCCSemiAutoCodec)

  @Benchmark def kindlingsAutoSimpleCC(): Array[Byte] =
    writeToArray(BenchmarkData.simpleCC)(KindlingsJsoniterInstances.simpleCCAutoCodec)

  @Benchmark def originalSimpleCC(): Array[Byte] =
    writeToArray(BenchmarkData.simpleCC)(OriginalJsoniterInstances.simpleCCCodec)

  @Benchmark def kindlingsSemiAutoPerson(): Array[Byte] =
    writeToArray(BenchmarkData.person)(KindlingsJsoniterInstances.personSemiAutoCodec)

  @Benchmark def kindlingsAutoPerson(): Array[Byte] =
    writeToArray(BenchmarkData.person)(KindlingsJsoniterInstances.personAutoCodec)

  @Benchmark def originalPerson(): Array[Byte] =
    writeToArray(BenchmarkData.person)(OriginalJsoniterInstances.personCodec)

  @Benchmark def kindlingsSemiAutoSimpleADT(): Array[Byte] =
    writeToArray(BenchmarkData.simpleADT)(KindlingsJsoniterInstances.simpleADTSemiAutoCodec)

  @Benchmark def kindlingsAutoSimpleADT(): Array[Byte] =
    writeToArray(BenchmarkData.simpleADT)(KindlingsJsoniterInstances.simpleADTAutoCodec)

  @Benchmark def originalSimpleADT(): Array[Byte] =
    writeToArray(BenchmarkData.simpleADT)(OriginalJsoniterInstances.simpleADTCodec)

  @Benchmark def kindlingsSemiAutoEvent(): Array[Byte] =
    writeToArray(BenchmarkData.event)(KindlingsJsoniterInstances.eventSemiAutoCodec)

  @Benchmark def kindlingsAutoEvent(): Array[Byte] =
    writeToArray(BenchmarkData.event)(KindlingsJsoniterInstances.eventAutoCodec)

  @Benchmark def originalEvent(): Array[Byte] =
    writeToArray(BenchmarkData.event)(OriginalJsoniterInstances.eventCodec)
}

@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
@Warmup(iterations = 5, time = 1)
@Measurement(iterations = 10, time = 1)
@Fork(2)
class JsoniterReadBenchmark {

  private var simpleCCBytes: Array[Byte] = _
  private var personBytes: Array[Byte] = _
  private var simpleADTBytes: Array[Byte] = _
  private var eventBytes: Array[Byte] = _

  @Setup(Level.Trial)
  def setup(): Unit = {
    simpleCCBytes = writeToArray(BenchmarkData.simpleCC)(KindlingsJsoniterInstances.simpleCCSemiAutoCodec)
    personBytes = writeToArray(BenchmarkData.person)(KindlingsJsoniterInstances.personSemiAutoCodec)
    simpleADTBytes = writeToArray(BenchmarkData.simpleADT)(KindlingsJsoniterInstances.simpleADTSemiAutoCodec)
    eventBytes = writeToArray(BenchmarkData.event)(KindlingsJsoniterInstances.eventSemiAutoCodec)
  }

  @Benchmark def kindlingsSemiAutoSimpleCC(): SimpleCC =
    readFromArray(simpleCCBytes)(KindlingsJsoniterInstances.simpleCCSemiAutoCodec)

  @Benchmark def kindlingsAutoSimpleCC(): SimpleCC =
    readFromArray(simpleCCBytes)(KindlingsJsoniterInstances.simpleCCAutoCodec)

  @Benchmark def originalSimpleCC(): SimpleCC =
    readFromArray(simpleCCBytes)(OriginalJsoniterInstances.simpleCCCodec)

  @Benchmark def kindlingsSemiAutoPerson(): Person =
    readFromArray(personBytes)(KindlingsJsoniterInstances.personSemiAutoCodec)

  @Benchmark def kindlingsAutoPerson(): Person =
    readFromArray(personBytes)(KindlingsJsoniterInstances.personAutoCodec)

  @Benchmark def originalPerson(): Person =
    readFromArray(personBytes)(OriginalJsoniterInstances.personCodec)

  @Benchmark def kindlingsSemiAutoSimpleADT(): SimpleADT =
    readFromArray(simpleADTBytes)(KindlingsJsoniterInstances.simpleADTSemiAutoCodec)

  @Benchmark def kindlingsAutoSimpleADT(): SimpleADT =
    readFromArray(simpleADTBytes)(KindlingsJsoniterInstances.simpleADTAutoCodec)

  @Benchmark def originalSimpleADT(): SimpleADT =
    readFromArray(simpleADTBytes)(OriginalJsoniterInstances.simpleADTCodec)

  @Benchmark def kindlingsSemiAutoEvent(): Event =
    readFromArray(eventBytes)(KindlingsJsoniterInstances.eventSemiAutoCodec)

  @Benchmark def kindlingsAutoEvent(): Event =
    readFromArray(eventBytes)(KindlingsJsoniterInstances.eventAutoCodec)

  @Benchmark def originalEvent(): Event =
    readFromArray(eventBytes)(OriginalJsoniterInstances.eventCodec)
}
