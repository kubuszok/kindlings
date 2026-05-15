package hearth.kindlings.benchmarks

import com.github.plokhotnyuk.jsoniter_scala.core.*
import io.circe.{Decoder, Json, JsoniterScalaCodec}
import org.openjdk.jmh.annotations.*
import java.util.concurrent.TimeUnit

@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
@Warmup(iterations = 5, time = 1)
@Measurement(iterations = 10, time = 1)
@Fork(2)
class CirceBoosterEncodeBenchmark {

  // Full pipeline: domain type → Encoder → Json → bytes/String
  // "Booster" = jsoniter-scala-circe writeToArray (Json → bytes)
  // "NoBooster" = circe .noSpaces (Json → String)

  private implicit val jsonCodec: JsonValueCodec[Json] =
    new JsoniterScalaCodec(64, 32, _ => true, JsoniterScalaCodec.defaultNumberParser)

  // --- SimpleCC ---

  @Benchmark def kindlingsBoosterSimpleCC(): Array[Byte] =
    writeToArray(KindlingsCirceInstances.simpleCCSemiAutoEncoder(BenchmarkData.simpleCC))

  @Benchmark def originalBoosterSimpleCC(): Array[Byte] =
    writeToArray(OriginalCirceSemiAutoInstances.simpleCCEncoder(BenchmarkData.simpleCC))

  @Benchmark def kindlingsNoBoosterSimpleCC(): String =
    KindlingsCirceInstances.simpleCCSemiAutoEncoder(BenchmarkData.simpleCC).noSpaces

  @Benchmark def originalNoBoosterSimpleCC(): String =
    OriginalCirceSemiAutoInstances.simpleCCEncoder(BenchmarkData.simpleCC).noSpaces

  // --- Person ---

  @Benchmark def kindlingsBoosterPerson(): Array[Byte] =
    writeToArray(KindlingsCirceInstances.personSemiAutoEncoder(BenchmarkData.person))

  @Benchmark def originalBoosterPerson(): Array[Byte] =
    writeToArray(OriginalCirceSemiAutoInstances.personEncoder(BenchmarkData.person))

  @Benchmark def kindlingsNoBoosterPerson(): String =
    KindlingsCirceInstances.personSemiAutoEncoder(BenchmarkData.person).noSpaces

  @Benchmark def originalNoBoosterPerson(): String =
    OriginalCirceSemiAutoInstances.personEncoder(BenchmarkData.person).noSpaces

  // --- SimpleADT ---

  @Benchmark def kindlingsBoosterSimpleADT(): Array[Byte] =
    writeToArray(KindlingsCirceInstances.simpleADTSemiAutoEncoder(BenchmarkData.simpleADT))

  @Benchmark def originalBoosterSimpleADT(): Array[Byte] =
    writeToArray(OriginalCirceSemiAutoInstances.simpleADTEncoder(BenchmarkData.simpleADT))

  @Benchmark def kindlingsNoBoosterSimpleADT(): String =
    KindlingsCirceInstances.simpleADTSemiAutoEncoder(BenchmarkData.simpleADT).noSpaces

  @Benchmark def originalNoBoosterSimpleADT(): String =
    OriginalCirceSemiAutoInstances.simpleADTEncoder(BenchmarkData.simpleADT).noSpaces

  // --- Event ---

  @Benchmark def kindlingsBoosterEvent(): Array[Byte] =
    writeToArray(KindlingsCirceInstances.eventSemiAutoEncoder(BenchmarkData.event))

  @Benchmark def originalBoosterEvent(): Array[Byte] =
    writeToArray(OriginalCirceSemiAutoInstances.eventEncoder(BenchmarkData.event))

  @Benchmark def kindlingsNoBoosterEvent(): String =
    KindlingsCirceInstances.eventSemiAutoEncoder(BenchmarkData.event).noSpaces

  @Benchmark def originalNoBoosterEvent(): String =
    OriginalCirceSemiAutoInstances.eventEncoder(BenchmarkData.event).noSpaces
}

@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
@Warmup(iterations = 5, time = 1)
@Measurement(iterations = 10, time = 1)
@Fork(2)
class CirceBoosterDecodeBenchmark {

  // Full pipeline: bytes/String → parse → Json → Decoder → domain type
  // "Booster" = jsoniter-scala-circe readFromArray (bytes → Json)
  // "NoBooster" = circe parser.parse (String → Json)

  private implicit val jsonCodec: JsonValueCodec[Json] =
    new JsoniterScalaCodec(64, 32, _ => true, JsoniterScalaCodec.defaultNumberParser)

  private var simpleCCBytes: Array[Byte] = _
  private var simpleCCString: String = _
  private var personBytes: Array[Byte] = _
  private var personString: String = _
  private var simpleADTBytes: Array[Byte] = _
  private var simpleADTString: String = _
  private var eventBytes: Array[Byte] = _
  private var eventString: String = _

  @Setup(Level.Trial)
  def setup(): Unit = {
    simpleCCBytes = writeToArray(KindlingsCirceInstances.simpleCCSemiAutoEncoder(BenchmarkData.simpleCC))
    simpleCCString = new String(simpleCCBytes)
    personBytes = writeToArray(KindlingsCirceInstances.personSemiAutoEncoder(BenchmarkData.person))
    personString = new String(personBytes)
    simpleADTBytes = writeToArray(KindlingsCirceInstances.simpleADTSemiAutoEncoder(BenchmarkData.simpleADT))
    simpleADTString = new String(simpleADTBytes)
    eventBytes = writeToArray(KindlingsCirceInstances.eventSemiAutoEncoder(BenchmarkData.event))
    eventString = new String(eventBytes)
  }

  // --- SimpleCC ---

  @Benchmark def kindlingsBoosterSimpleCC(): Decoder.Result[SimpleCC] = {
    val json = readFromArray[Json](simpleCCBytes)
    KindlingsCirceInstances.simpleCCSemiAutoDecoder.decodeJson(json)
  }

  @Benchmark def originalBoosterSimpleCC(): Decoder.Result[SimpleCC] = {
    val json = readFromArray[Json](simpleCCBytes)
    OriginalCirceSemiAutoInstances.simpleCCDecoder.decodeJson(json)
  }

  @Benchmark def kindlingsNoBoosterSimpleCC(): Decoder.Result[SimpleCC] = {
    val json = io.circe.parser.parse(simpleCCString).getOrElse(Json.Null)
    KindlingsCirceInstances.simpleCCSemiAutoDecoder.decodeJson(json)
  }

  @Benchmark def originalNoBoosterSimpleCC(): Decoder.Result[SimpleCC] = {
    val json = io.circe.parser.parse(simpleCCString).getOrElse(Json.Null)
    OriginalCirceSemiAutoInstances.simpleCCDecoder.decodeJson(json)
  }

  // --- Person ---

  @Benchmark def kindlingsBoosterPerson(): Decoder.Result[Person] = {
    val json = readFromArray[Json](personBytes)
    KindlingsCirceInstances.personSemiAutoDecoder.decodeJson(json)
  }

  @Benchmark def originalBoosterPerson(): Decoder.Result[Person] = {
    val json = readFromArray[Json](personBytes)
    OriginalCirceSemiAutoInstances.personDecoder.decodeJson(json)
  }

  @Benchmark def kindlingsNoBoosterPerson(): Decoder.Result[Person] = {
    val json = io.circe.parser.parse(personString).getOrElse(Json.Null)
    KindlingsCirceInstances.personSemiAutoDecoder.decodeJson(json)
  }

  @Benchmark def originalNoBoosterPerson(): Decoder.Result[Person] = {
    val json = io.circe.parser.parse(personString).getOrElse(Json.Null)
    OriginalCirceSemiAutoInstances.personDecoder.decodeJson(json)
  }

  // --- SimpleADT ---

  @Benchmark def kindlingsBoosterSimpleADT(): Decoder.Result[SimpleADT] = {
    val json = readFromArray[Json](simpleADTBytes)
    KindlingsCirceInstances.simpleADTSemiAutoDecoder.decodeJson(json)
  }

  @Benchmark def originalBoosterSimpleADT(): Decoder.Result[SimpleADT] = {
    val json = readFromArray[Json](simpleADTBytes)
    OriginalCirceSemiAutoInstances.simpleADTDecoder.decodeJson(json)
  }

  @Benchmark def kindlingsNoBoosterSimpleADT(): Decoder.Result[SimpleADT] = {
    val json = io.circe.parser.parse(simpleADTString).getOrElse(Json.Null)
    KindlingsCirceInstances.simpleADTSemiAutoDecoder.decodeJson(json)
  }

  @Benchmark def originalNoBoosterSimpleADT(): Decoder.Result[SimpleADT] = {
    val json = io.circe.parser.parse(simpleADTString).getOrElse(Json.Null)
    OriginalCirceSemiAutoInstances.simpleADTDecoder.decodeJson(json)
  }

  // --- Event ---

  @Benchmark def kindlingsBoosterEvent(): Decoder.Result[Event] = {
    val json = readFromArray[Json](eventBytes)
    KindlingsCirceInstances.eventSemiAutoDecoder.decodeJson(json)
  }

  @Benchmark def originalBoosterEvent(): Decoder.Result[Event] = {
    val json = readFromArray[Json](eventBytes)
    OriginalCirceSemiAutoInstances.eventDecoder.decodeJson(json)
  }

  @Benchmark def kindlingsNoBoosterEvent(): Decoder.Result[Event] = {
    val json = io.circe.parser.parse(eventString).getOrElse(Json.Null)
    KindlingsCirceInstances.eventSemiAutoDecoder.decodeJson(json)
  }

  @Benchmark def originalNoBoosterEvent(): Decoder.Result[Event] = {
    val json = io.circe.parser.parse(eventString).getOrElse(Json.Null)
    OriginalCirceSemiAutoInstances.eventDecoder.decodeJson(json)
  }
}
