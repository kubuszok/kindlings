package hearth.kindlings.benchmarks

import io.circe.{Decoder, Json}
import org.openjdk.jmh.annotations.*
import java.util.concurrent.TimeUnit

@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
@Warmup(iterations = 5, time = 1)
@Measurement(iterations = 10, time = 1)
@Fork(2)
class CirceEncodeBenchmark {

  @Benchmark def kindlingsSemiAutoSimpleCC(): Json =
    KindlingsCirceInstances.simpleCCSemiAutoEncoder(BenchmarkData.simpleCC)

  @Benchmark def kindlingsAutoSimpleCC(): Json =
    KindlingsCirceInstances.simpleCCAutoEncoder(BenchmarkData.simpleCC)

  @Benchmark def originalSemiAutoSimpleCC(): Json =
    OriginalCirceSemiAutoInstances.simpleCCEncoder(BenchmarkData.simpleCC)

  @Benchmark def originalAutoSimpleCC(): Json =
    OriginalCirceAutoInstances.simpleCCEncoder(BenchmarkData.simpleCC)

  @Benchmark def kindlingsSemiAutoPerson(): Json =
    KindlingsCirceInstances.personSemiAutoEncoder(BenchmarkData.person)

  @Benchmark def kindlingsAutoPerson(): Json =
    KindlingsCirceInstances.personAutoEncoder(BenchmarkData.person)

  @Benchmark def originalSemiAutoPerson(): Json =
    OriginalCirceSemiAutoInstances.personEncoder(BenchmarkData.person)

  @Benchmark def originalAutoPerson(): Json =
    OriginalCirceAutoInstances.personEncoder(BenchmarkData.person)

  @Benchmark def kindlingsSemiAutoSimpleADT(): Json =
    KindlingsCirceInstances.simpleADTSemiAutoEncoder(BenchmarkData.simpleADT)

  @Benchmark def kindlingsAutoSimpleADT(): Json =
    KindlingsCirceInstances.simpleADTAutoEncoder(BenchmarkData.simpleADT)

  @Benchmark def originalSemiAutoSimpleADT(): Json =
    OriginalCirceSemiAutoInstances.simpleADTEncoder(BenchmarkData.simpleADT)

  @Benchmark def originalAutoSimpleADT(): Json =
    OriginalCirceAutoInstances.simpleADTEncoder(BenchmarkData.simpleADT)

  @Benchmark def kindlingsSemiAutoEvent(): Json =
    KindlingsCirceInstances.eventSemiAutoEncoder(BenchmarkData.event)

  @Benchmark def kindlingsAutoEvent(): Json =
    KindlingsCirceInstances.eventAutoEncoder(BenchmarkData.event)

  @Benchmark def originalSemiAutoEvent(): Json =
    OriginalCirceSemiAutoInstances.eventEncoder(BenchmarkData.event)

  @Benchmark def originalAutoEvent(): Json =
    OriginalCirceAutoInstances.eventEncoder(BenchmarkData.event)
}

@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
@Warmup(iterations = 5, time = 1)
@Measurement(iterations = 10, time = 1)
@Fork(2)
class CirceDecodeBenchmark {

  private var simpleCCJson: Json = _
  private var personJson: Json = _
  private var simpleADTJson: Json = _
  private var eventJson: Json = _

  @Setup(Level.Trial)
  def setup(): Unit = {
    simpleCCJson = KindlingsCirceInstances.simpleCCSemiAutoEncoder(BenchmarkData.simpleCC)
    personJson = KindlingsCirceInstances.personSemiAutoEncoder(BenchmarkData.person)
    simpleADTJson = KindlingsCirceInstances.simpleADTSemiAutoEncoder(BenchmarkData.simpleADT)
    eventJson = KindlingsCirceInstances.eventSemiAutoEncoder(BenchmarkData.event)
  }

  @Benchmark def kindlingsSemiAutoSimpleCC(): Decoder.Result[SimpleCC] =
    KindlingsCirceInstances.simpleCCSemiAutoDecoder.decodeJson(simpleCCJson)

  @Benchmark def kindlingsAutoSimpleCC(): Decoder.Result[SimpleCC] =
    KindlingsCirceInstances.simpleCCAutoDecoder.decodeJson(simpleCCJson)

  @Benchmark def originalSemiAutoSimpleCC(): Decoder.Result[SimpleCC] =
    OriginalCirceSemiAutoInstances.simpleCCDecoder.decodeJson(simpleCCJson)

  @Benchmark def originalAutoSimpleCC(): Decoder.Result[SimpleCC] =
    OriginalCirceAutoInstances.simpleCCDecoder.decodeJson(simpleCCJson)

  @Benchmark def kindlingsSemiAutoPerson(): Decoder.Result[Person] =
    KindlingsCirceInstances.personSemiAutoDecoder.decodeJson(personJson)

  @Benchmark def kindlingsAutoPerson(): Decoder.Result[Person] =
    KindlingsCirceInstances.personAutoDecoder.decodeJson(personJson)

  @Benchmark def originalSemiAutoPerson(): Decoder.Result[Person] =
    OriginalCirceSemiAutoInstances.personDecoder.decodeJson(personJson)

  @Benchmark def originalAutoPerson(): Decoder.Result[Person] =
    OriginalCirceAutoInstances.personDecoder.decodeJson(personJson)

  @Benchmark def kindlingsSemiAutoSimpleADT(): Decoder.Result[SimpleADT] =
    KindlingsCirceInstances.simpleADTSemiAutoDecoder.decodeJson(simpleADTJson)

  @Benchmark def kindlingsAutoSimpleADT(): Decoder.Result[SimpleADT] =
    KindlingsCirceInstances.simpleADTAutoDecoder.decodeJson(simpleADTJson)

  @Benchmark def originalSemiAutoSimpleADT(): Decoder.Result[SimpleADT] =
    OriginalCirceSemiAutoInstances.simpleADTDecoder.decodeJson(simpleADTJson)

  @Benchmark def originalAutoSimpleADT(): Decoder.Result[SimpleADT] =
    OriginalCirceAutoInstances.simpleADTDecoder.decodeJson(simpleADTJson)

  @Benchmark def kindlingsSemiAutoEvent(): Decoder.Result[Event] =
    KindlingsCirceInstances.eventSemiAutoDecoder.decodeJson(eventJson)

  @Benchmark def kindlingsAutoEvent(): Decoder.Result[Event] =
    KindlingsCirceInstances.eventAutoDecoder.decodeJson(eventJson)

  @Benchmark def originalSemiAutoEvent(): Decoder.Result[Event] =
    OriginalCirceSemiAutoInstances.eventDecoder.decodeJson(eventJson)

  @Benchmark def originalAutoEvent(): Decoder.Result[Event] =
    OriginalCirceAutoInstances.eventDecoder.decodeJson(eventJson)
}
