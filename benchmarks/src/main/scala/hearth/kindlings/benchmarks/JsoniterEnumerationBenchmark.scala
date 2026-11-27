package hearth.kindlings.benchmarks

import com.github.plokhotnyuk.jsoniter_scala.core.*
import com.github.plokhotnyuk.jsoniter_scala.macros.{CodecMakerConfig, JsonCodecMaker}
import hearth.kindlings.jsoniterderivation.{JsoniterConfig, KindlingsJsonValueCodec}
import org.openjdk.jmh.annotations.*
import java.util.concurrent.TimeUnit

object BenchPlanet extends Enumeration {
  type Planet = Value
  val Mercury, Venus, Earth, Mars, Jupiter, Saturn, Uranus, Neptune: Planet = Value
}

/** `scala.Enumeration` values as fields, collection elements and map keys. */
final case class EnumerationData(
    home: BenchPlanet.Planet,
    target: BenchPlanet.Planet,
    route: List[BenchPlanet.Planet],
    distances: Map[BenchPlanet.Planet, Int]
)

/** `scala.Enumeration` values as fields only (no collection or map around them). */
final case class EnumerationFields(a: BenchPlanet.Planet, b: BenchPlanet.Planet, c: BenchPlanet.Planet, d: BenchPlanet.Planet)

object EnumerationBenchmarkData {
  val fields: EnumerationFields =
    EnumerationFields(BenchPlanet.Earth, BenchPlanet.Neptune, BenchPlanet.Mercury, BenchPlanet.Saturn)
  val value: EnumerationData = EnumerationData(
    BenchPlanet.Earth,
    BenchPlanet.Neptune,
    List(BenchPlanet.Earth, BenchPlanet.Mars, BenchPlanet.Jupiter, BenchPlanet.Saturn, BenchPlanet.Uranus),
    Map(BenchPlanet.Mars -> 78, BenchPlanet.Jupiter -> 628, BenchPlanet.Saturn -> 1275, BenchPlanet.Neptune -> 4351)
  )
}

object KindlingsEnumerationCodecs {
  val byName: JsonValueCodec[EnumerationData] = {
    implicit val config: JsoniterConfig = JsoniterConfig.default
    KindlingsJsonValueCodec.derived[EnumerationData]
  }
  val byId: JsonValueCodec[EnumerationData] = {
    implicit val config: JsoniterConfig = JsoniterConfig.default.withUseScalaEnumValueId
    KindlingsJsonValueCodec.derived[EnumerationData]
  }
  val fieldsByName: JsonValueCodec[EnumerationFields] = {
    implicit val config: JsoniterConfig = JsoniterConfig.default
    KindlingsJsonValueCodec.derived[EnumerationFields]
  }
  val fieldsById: JsonValueCodec[EnumerationFields] = {
    implicit val config: JsoniterConfig = JsoniterConfig.default.withUseScalaEnumValueId
    KindlingsJsonValueCodec.derived[EnumerationFields]
  }
}

object OriginalEnumerationCodecs {
  val byName: JsonValueCodec[EnumerationData] = JsonCodecMaker.make[EnumerationData]
  val byId: JsonValueCodec[EnumerationData] =
    JsonCodecMaker.make[EnumerationData](CodecMakerConfig.withUseScalaEnumValueId(true))
  val fieldsByName: JsonValueCodec[EnumerationFields] = JsonCodecMaker.make[EnumerationFields]
  val fieldsById: JsonValueCodec[EnumerationFields] =
    JsonCodecMaker.make[EnumerationFields](CodecMakerConfig.withUseScalaEnumValueId(true))
}

/** `scala.Enumeration` values (jsoniter-scala parity: looked up at runtime, by name or by id) against
  * `JsonCodecMaker.make`.
  */
@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
@Warmup(iterations = 5, time = 1)
@Measurement(iterations = 10, time = 1)
@Fork(2)
class JsoniterEnumerationBenchmark {

  private var byNameBytes: Array[Byte] = _
  private var byIdBytes: Array[Byte] = _

  @Setup(Level.Trial)
  def setup(): Unit = {
    byNameBytes = writeToArray(EnumerationBenchmarkData.value)(OriginalEnumerationCodecs.byName)
    byIdBytes = writeToArray(EnumerationBenchmarkData.value)(OriginalEnumerationCodecs.byId)
    require(java.util.Arrays.equals(byNameBytes, writeToArray(EnumerationBenchmarkData.value)(KindlingsEnumerationCodecs.byName)))
    require(java.util.Arrays.equals(byIdBytes, writeToArray(EnumerationBenchmarkData.value)(KindlingsEnumerationCodecs.byId)))
  }

  @Benchmark def kindlingsWriteByName(): Array[Byte] =
    writeToArray(EnumerationBenchmarkData.value)(KindlingsEnumerationCodecs.byName)

  @Benchmark def originalWriteByName(): Array[Byte] =
    writeToArray(EnumerationBenchmarkData.value)(OriginalEnumerationCodecs.byName)

  @Benchmark def kindlingsReadByName(): EnumerationData =
    readFromArray(byNameBytes)(KindlingsEnumerationCodecs.byName)

  @Benchmark def originalReadByName(): EnumerationData =
    readFromArray(byNameBytes)(OriginalEnumerationCodecs.byName)

  @Benchmark def kindlingsWriteById(): Array[Byte] =
    writeToArray(EnumerationBenchmarkData.value)(KindlingsEnumerationCodecs.byId)

  @Benchmark def originalWriteById(): Array[Byte] =
    writeToArray(EnumerationBenchmarkData.value)(OriginalEnumerationCodecs.byId)

  @Benchmark def kindlingsReadById(): EnumerationData =
    readFromArray(byIdBytes)(KindlingsEnumerationCodecs.byId)

  @Benchmark def originalReadById(): EnumerationData =
    readFromArray(byIdBytes)(OriginalEnumerationCodecs.byId)

  // fields only

  private val fieldsByNameBytes: Array[Byte] =
    writeToArray(EnumerationBenchmarkData.fields)(OriginalEnumerationCodecs.fieldsByName)
  private val fieldsByIdBytes: Array[Byte] =
    writeToArray(EnumerationBenchmarkData.fields)(OriginalEnumerationCodecs.fieldsById)

  @Benchmark def kindlingsWriteFieldsByName(): Array[Byte] =
    writeToArray(EnumerationBenchmarkData.fields)(KindlingsEnumerationCodecs.fieldsByName)

  @Benchmark def originalWriteFieldsByName(): Array[Byte] =
    writeToArray(EnumerationBenchmarkData.fields)(OriginalEnumerationCodecs.fieldsByName)

  @Benchmark def kindlingsReadFieldsByName(): EnumerationFields =
    readFromArray(fieldsByNameBytes)(KindlingsEnumerationCodecs.fieldsByName)

  @Benchmark def originalReadFieldsByName(): EnumerationFields =
    readFromArray(fieldsByNameBytes)(OriginalEnumerationCodecs.fieldsByName)

  @Benchmark def kindlingsWriteFieldsById(): Array[Byte] =
    writeToArray(EnumerationBenchmarkData.fields)(KindlingsEnumerationCodecs.fieldsById)

  @Benchmark def originalWriteFieldsById(): Array[Byte] =
    writeToArray(EnumerationBenchmarkData.fields)(OriginalEnumerationCodecs.fieldsById)

  @Benchmark def kindlingsReadFieldsById(): EnumerationFields =
    readFromArray(fieldsByIdBytes)(KindlingsEnumerationCodecs.fieldsById)

  @Benchmark def originalReadFieldsById(): EnumerationFields =
    readFromArray(fieldsByIdBytes)(OriginalEnumerationCodecs.fieldsById)
}
