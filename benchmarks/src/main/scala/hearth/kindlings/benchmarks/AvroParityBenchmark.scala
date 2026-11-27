package hearth.kindlings.benchmarks

import com.sksamuel.avro4s.{AvroFixed, AvroName, AvroNamespace}
import hearth.kindlings.avroderivation.{AvroConfig, AvroDecoder, AvroEncoder}
import hearth.kindlings.avroderivation.annotations.{avroFixed, avroName, avroNamespace}
import org.apache.avro.generic.GenericRecord
import org.openjdk.jmh.annotations.*
import java.util.concurrent.TimeUnit

// The avro4s features Kindlings gained with the parity work, on the paths that run per encoded/decoded value: ENUM
// symbols renamed with @avroName, a String as a FIXED value, and a field whose record lives in another namespace
// (annotated for both libraries: each reads only its own annotations).

sealed trait AvroOcean
@AvroName("atlan") @avroName("atlan") case object AvroAtlantic extends AvroOcean
@AvroName("pacif") @avroName("pacif") case object AvroPacific extends AvroOcean

case class AvroPort(name: String, berths: Int)

case class AvroVoyage(
    ocean: AvroOcean,
    crossed: List[AvroOcean],
    @AvroFixed(8) @avroFixed(8) code: String,
    @AvroNamespace("harbour") @avroNamespace("harbour") port: AvroPort
)

// Decimals rounded to the schema's scale (avro4s 4: an implicit RoundingMode; avro4s 5: always HALF_UP).
case class AvroPrice(gross: BigDecimal, net: BigDecimal)

object AvroParityData {
  val price: AvroPrice = AvroPrice(BigDecimal("1234.567"), BigDecimal("99.5"))
  val voyage: AvroVoyage =
    AvroVoyage(AvroPacific, List(AvroAtlantic, AvroPacific, AvroAtlantic), "VOY-0042", AvroPort("Gdansk", 12))
}

object KindlingsAvroParityInstances {
  val voyageEncoder: AvroEncoder[AvroVoyage] = AvroEncoder.derived[AvroVoyage]
  val voyageDecoder: AvroDecoder[AvroVoyage] = AvroDecoder.derived[AvroVoyage]
  val priceEncoder: AvroEncoder[AvroPrice] = {
    implicit val config: AvroConfig = AvroConfig.default.withDecimalConfig(8, 2, java.math.RoundingMode.HALF_UP)
    AvroEncoder.derived[AvroPrice]
  }
}

@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
@Warmup(iterations = 5, time = 1)
@Measurement(iterations = 10, time = 1)
@Fork(2)
class AvroParityEncodeBenchmark {

  @Benchmark def kindlingsVoyage(): Any =
    KindlingsAvroParityInstances.voyageEncoder.encode(AvroParityData.voyage)

  @Benchmark def originalVoyage(): GenericRecord =
    OriginalAvroParityInstances.voyageFormat.to(AvroParityData.voyage)

  @Benchmark def kindlingsRoundedDecimals(): Any =
    KindlingsAvroParityInstances.priceEncoder.encode(AvroParityData.price)

  @Benchmark def originalRoundedDecimals(): GenericRecord =
    OriginalAvroParityInstances.priceFormat.to(AvroParityData.price)
}

@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
@Warmup(iterations = 5, time = 1)
@Measurement(iterations = 10, time = 1)
@Fork(2)
class AvroParityDecodeBenchmark {

  private var kindlingsRecord: Any = _
  private var originalRecord: GenericRecord = _

  @Setup(Level.Trial)
  def setup(): Unit = {
    kindlingsRecord = KindlingsAvroParityInstances.voyageEncoder.encode(AvroParityData.voyage)
    originalRecord = OriginalAvroParityInstances.voyageFormat.to(AvroParityData.voyage)
  }

  @Benchmark def kindlingsVoyage(): AvroVoyage =
    KindlingsAvroParityInstances.voyageDecoder.decode(kindlingsRecord)

  @Benchmark def originalVoyage(): AvroVoyage =
    OriginalAvroParityInstances.voyageFormat.from(originalRecord)
}
