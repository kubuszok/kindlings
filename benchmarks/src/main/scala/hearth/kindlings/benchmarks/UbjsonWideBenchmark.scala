package hearth.kindlings.benchmarks

import hearth.kindlings.ubjsonderivation.{UBJsonConfig, UBJsonReader, UBJsonValueCodec, UBJsonWriter}
import org.openjdk.jmh.annotations.*
import java.util.concurrent.TimeUnit

/** Codecs whose config is only known at runtime (a parameter: the macro cannot evaluate it), the code path a config
  * defined next to the call site takes.
  */
object KindlingsUbjsonRuntimeConfigInstances {

  private def personCodec(config: UBJsonConfig): UBJsonValueCodec[Person] = {
    implicit val runtimeConfig: UBJsonConfig = config
    UBJsonValueCodec.derived[Person]
  }

  private def wide20Codec(config: UBJsonConfig): UBJsonValueCodec[Wide20] = {
    implicit val runtimeConfig: UBJsonConfig = config
    UBJsonValueCodec.derived[Wide20]
  }

  private def wide100Codec(config: UBJsonConfig): UBJsonValueCodec[Wide100] = {
    implicit val runtimeConfig: UBJsonConfig = config
    UBJsonValueCodec.derived[Wide100]
  }

  val person: UBJsonValueCodec[Person] = personCodec(UBJsonConfig())
  val wide20: UBJsonValueCodec[Wide20] = wide20Codec(UBJsonConfig())
  val wide100: UBJsonValueCodec[Wide100] = wide100Codec(UBJsonConfig())
}

object KindlingsUbjsonWideInstances {
  val wide20: UBJsonValueCodec[Wide20] = UBJsonValueCodec.derived[Wide20]
  val wide100: UBJsonValueCodec[Wide100] = UBJsonValueCodec.derived[Wide100]
}

/** Reading classes wider than [[UbjsonReadBenchmark]]'s, with the config known at compile time and only at runtime. */
@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
@Warmup(iterations = 5, time = 1)
@Measurement(iterations = 10, time = 1)
@Fork(2)
class UbjsonWideReadBenchmark {

  private var personBytes: Array[Byte] = _
  private var wide20Bytes: Array[Byte] = _
  private var wide100Bytes: Array[Byte] = _

  private def writeToArray[A](value: A, codec: UBJsonValueCodec[A]): Array[Byte] = {
    val writer = new UBJsonWriter()
    codec.encode(writer, value)
    writer.toByteArray
  }

  @Setup(Level.Trial)
  def setup(): Unit = {
    personBytes = writeToArray(BenchmarkData.person, KindlingsUbjsonInstances.personCodec)
    wide20Bytes = writeToArray(Wide20.value, KindlingsUbjsonWideInstances.wide20)
    wide100Bytes = writeToArray(Wide100(w0 = Some("x"), w99 = 5L), KindlingsUbjsonWideInstances.wide100)
  }

  @Benchmark def kindlingsWide20(): Wide20 =
    KindlingsUbjsonWideInstances.wide20.decode(new UBJsonReader(wide20Bytes))

  @Benchmark def kindlingsWide100(): Wide100 =
    KindlingsUbjsonWideInstances.wide100.decode(new UBJsonReader(wide100Bytes))

  @Benchmark def kindlingsRuntimeConfigPerson(): Person =
    KindlingsUbjsonRuntimeConfigInstances.person.decode(new UBJsonReader(personBytes))

  @Benchmark def kindlingsRuntimeConfigWide20(): Wide20 =
    KindlingsUbjsonRuntimeConfigInstances.wide20.decode(new UBJsonReader(wide20Bytes))

  @Benchmark def kindlingsRuntimeConfigWide100(): Wide100 =
    KindlingsUbjsonRuntimeConfigInstances.wide100.decode(new UBJsonReader(wide100Bytes))
}
