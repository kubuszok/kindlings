package hearth.kindlings.benchmarks

import com.github.plokhotnyuk.jsoniter_scala.core.*
import org.openjdk.jmh.annotations.*
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

  @Benchmark def originalSemiAutoSimpleCC(): Array[Byte] =
    writeToArray(BenchmarkData.simpleCC)(OriginalJsoniterSemiAutoInstances.simpleCCCodec)

  @Benchmark def kindlingsSemiAutoPerson(): Array[Byte] =
    writeToArray(BenchmarkData.person)(KindlingsJsoniterInstances.personSemiAutoCodec)

  @Benchmark def kindlingsAutoPerson(): Array[Byte] =
    writeToArray(BenchmarkData.person)(KindlingsJsoniterInstances.personAutoCodec)

  @Benchmark def originalSemiAutoPerson(): Array[Byte] =
    writeToArray(BenchmarkData.person)(OriginalJsoniterSemiAutoInstances.personCodec)

  @Benchmark def kindlingsSemiAutoSimpleADT(): Array[Byte] =
    writeToArray(BenchmarkData.simpleADT)(KindlingsJsoniterInstances.simpleADTSemiAutoCodec)

  @Benchmark def kindlingsAutoSimpleADT(): Array[Byte] =
    writeToArray(BenchmarkData.simpleADT)(KindlingsJsoniterInstances.simpleADTAutoCodec)

  @Benchmark def originalSemiAutoSimpleADT(): Array[Byte] =
    writeToArray(BenchmarkData.simpleADT)(OriginalJsoniterSemiAutoInstances.simpleADTCodec)

  @Benchmark def kindlingsSemiAutoEvent(): Array[Byte] =
    writeToArray(BenchmarkData.event)(KindlingsJsoniterInstances.eventSemiAutoCodec)

  @Benchmark def kindlingsAutoEvent(): Array[Byte] =
    writeToArray(BenchmarkData.event)(KindlingsJsoniterInstances.eventAutoCodec)

  @Benchmark def originalSemiAutoEvent(): Array[Byte] =
    writeToArray(BenchmarkData.event)(OriginalJsoniterSemiAutoInstances.eventCodec)
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

  @Benchmark def originalSemiAutoSimpleCC(): SimpleCC =
    readFromArray(simpleCCBytes)(OriginalJsoniterSemiAutoInstances.simpleCCCodec)

  @Benchmark def kindlingsSemiAutoPerson(): Person =
    readFromArray(personBytes)(KindlingsJsoniterInstances.personSemiAutoCodec)

  @Benchmark def kindlingsAutoPerson(): Person =
    readFromArray(personBytes)(KindlingsJsoniterInstances.personAutoCodec)

  @Benchmark def originalSemiAutoPerson(): Person =
    readFromArray(personBytes)(OriginalJsoniterSemiAutoInstances.personCodec)

  @Benchmark def kindlingsSemiAutoSimpleADT(): SimpleADT =
    readFromArray(simpleADTBytes)(KindlingsJsoniterInstances.simpleADTSemiAutoCodec)

  @Benchmark def kindlingsAutoSimpleADT(): SimpleADT =
    readFromArray(simpleADTBytes)(KindlingsJsoniterInstances.simpleADTAutoCodec)

  @Benchmark def originalSemiAutoSimpleADT(): SimpleADT =
    readFromArray(simpleADTBytes)(OriginalJsoniterSemiAutoInstances.simpleADTCodec)

  @Benchmark def kindlingsSemiAutoEvent(): Event =
    readFromArray(eventBytes)(KindlingsJsoniterInstances.eventSemiAutoCodec)

  @Benchmark def kindlingsAutoEvent(): Event =
    readFromArray(eventBytes)(KindlingsJsoniterInstances.eventAutoCodec)

  @Benchmark def originalSemiAutoEvent(): Event =
    readFromArray(eventBytes)(OriginalJsoniterSemiAutoInstances.eventCodec)
}
