package hearth.kindlings.benchmarks

import hearth.kindlings.ubjsonderivation.{UBJsonReader, UBJsonValueCodec, UBJsonWriter}
import org.openjdk.jmh.annotations.*
import java.util.concurrent.TimeUnit

object KindlingsUbjsonInstances {
  val simpleCCCodec: UBJsonValueCodec[SimpleCC] = UBJsonValueCodec.derive[SimpleCC]
  val personCodec: UBJsonValueCodec[Person] = UBJsonValueCodec.derive[Person]
  val eventCodec: UBJsonValueCodec[Event] = UBJsonValueCodec.derive[Event]
  val simpleADTCodec: UBJsonValueCodec[SimpleADT] = UBJsonValueCodec.derive[SimpleADT]
}

@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
@Warmup(iterations = 5, time = 1)
@Measurement(iterations = 10, time = 1)
@Fork(2)
class UbjsonWriteBenchmark {

  private def writeToArray[A](value: A, codec: UBJsonValueCodec[A]): Array[Byte] = {
    val writer = new UBJsonWriter()
    codec.encode(writer, value)
    writer.toByteArray
  }

  @Benchmark def kindlingsSimpleCC(): Array[Byte] =
    writeToArray(BenchmarkData.simpleCC, KindlingsUbjsonInstances.simpleCCCodec)

  @Benchmark def kindlingsPerson(): Array[Byte] =
    writeToArray(BenchmarkData.person, KindlingsUbjsonInstances.personCodec)

  @Benchmark def kindlingsEvent(): Array[Byte] =
    writeToArray(BenchmarkData.event, KindlingsUbjsonInstances.eventCodec)

  @Benchmark def kindlingsSimpleADT(): Array[Byte] =
    writeToArray(BenchmarkData.simpleADT, KindlingsUbjsonInstances.simpleADTCodec)
}

@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
@Warmup(iterations = 5, time = 1)
@Measurement(iterations = 10, time = 1)
@Fork(2)
class UbjsonReadBenchmark {

  private var simpleCCBytes: Array[Byte] = _
  private var personBytes: Array[Byte] = _
  private var eventBytes: Array[Byte] = _
  private var simpleADTBytes: Array[Byte] = _

  private def writeToArray[A](value: A, codec: UBJsonValueCodec[A]): Array[Byte] = {
    val writer = new UBJsonWriter()
    codec.encode(writer, value)
    writer.toByteArray
  }

  @Setup(Level.Trial)
  def setup(): Unit = {
    simpleCCBytes = writeToArray(BenchmarkData.simpleCC, KindlingsUbjsonInstances.simpleCCCodec)
    personBytes = writeToArray(BenchmarkData.person, KindlingsUbjsonInstances.personCodec)
    eventBytes = writeToArray(BenchmarkData.event, KindlingsUbjsonInstances.eventCodec)
    simpleADTBytes = writeToArray(BenchmarkData.simpleADT, KindlingsUbjsonInstances.simpleADTCodec)
  }

  @Benchmark def kindlingsSimpleCC(): SimpleCC =
    KindlingsUbjsonInstances.simpleCCCodec.decode(new UBJsonReader(simpleCCBytes))

  @Benchmark def kindlingsPerson(): Person =
    KindlingsUbjsonInstances.personCodec.decode(new UBJsonReader(personBytes))

  @Benchmark def kindlingsEvent(): Event =
    KindlingsUbjsonInstances.eventCodec.decode(new UBJsonReader(eventBytes))

  @Benchmark def kindlingsSimpleADT(): SimpleADT =
    KindlingsUbjsonInstances.simpleADTCodec.decode(new UBJsonReader(simpleADTBytes))
}
