package hearth.kindlings.benchmarks

import hearth.kindlings.avroderivation.{AvroDecoder, AvroEncoder}
import org.apache.avro.generic.GenericRecord
import org.openjdk.jmh.annotations.*
import java.util.concurrent.TimeUnit

object KindlingsAvroInstances {
  val simpleCCEncoder: AvroEncoder[SimpleCC] = AvroEncoder.derive[SimpleCC]
  val simpleCCDecoder: AvroDecoder[SimpleCC] = AvroDecoder.derive[SimpleCC]
  val personEncoder: AvroEncoder[Person] = AvroEncoder.derive[Person]
  val personDecoder: AvroDecoder[Person] = AvroDecoder.derive[Person]
  val eventEncoder: AvroEncoder[Event] = AvroEncoder.derive[Event]
  val eventDecoder: AvroDecoder[Event] = AvroDecoder.derive[Event]
  val simpleADTEncoder: AvroEncoder[SimpleADT] = AvroEncoder.derive[SimpleADT]
  val simpleADTDecoder: AvroDecoder[SimpleADT] = AvroDecoder.derive[SimpleADT]
}

@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
@Warmup(iterations = 5, time = 1)
@Measurement(iterations = 10, time = 1)
@Fork(2)
class AvroEncodeBenchmark {

  @Benchmark def kindlingsSimpleCC(): Any =
    KindlingsAvroInstances.simpleCCEncoder.encode(BenchmarkData.simpleCC)

  @Benchmark def originalSimpleCC(): GenericRecord =
    OriginalAvroInstances.simpleCCFormat.to(BenchmarkData.simpleCC)

  @Benchmark def kindlingsPerson(): Any =
    KindlingsAvroInstances.personEncoder.encode(BenchmarkData.person)

  @Benchmark def originalPerson(): GenericRecord =
    OriginalAvroInstances.personFormat.to(BenchmarkData.person)

  @Benchmark def kindlingsEvent(): Any =
    KindlingsAvroInstances.eventEncoder.encode(BenchmarkData.event)

  @Benchmark def kindlingsSimpleADT(): Any =
    KindlingsAvroInstances.simpleADTEncoder.encode(BenchmarkData.simpleADT)
}

@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
@Warmup(iterations = 5, time = 1)
@Measurement(iterations = 10, time = 1)
@Fork(2)
class AvroDecodeBenchmark {

  private var simpleCCRecord: Any = _
  private var personRecord: Any = _
  private var eventRecord: Any = _
  private var simpleADTRecord: Any = _
  private var simpleCCGenericRecord: GenericRecord = _
  private var personGenericRecord: GenericRecord = _

  @Setup(Level.Trial)
  def setup(): Unit = {
    simpleCCRecord = KindlingsAvroInstances.simpleCCEncoder.encode(BenchmarkData.simpleCC)
    personRecord = KindlingsAvroInstances.personEncoder.encode(BenchmarkData.person)
    eventRecord = KindlingsAvroInstances.eventEncoder.encode(BenchmarkData.event)
    simpleADTRecord = KindlingsAvroInstances.simpleADTEncoder.encode(BenchmarkData.simpleADT)
    simpleCCGenericRecord = OriginalAvroInstances.simpleCCFormat.to(BenchmarkData.simpleCC)
    personGenericRecord = OriginalAvroInstances.personFormat.to(BenchmarkData.person)
  }

  @Benchmark def kindlingsSimpleCC(): SimpleCC =
    KindlingsAvroInstances.simpleCCDecoder.decode(simpleCCRecord)

  @Benchmark def originalSimpleCC(): SimpleCC =
    OriginalAvroInstances.simpleCCFormat.from(simpleCCGenericRecord)

  @Benchmark def kindlingsPerson(): Person =
    KindlingsAvroInstances.personDecoder.decode(personRecord)

  @Benchmark def originalPerson(): Person =
    OriginalAvroInstances.personFormat.from(personGenericRecord)

  @Benchmark def kindlingsEvent(): Event =
    KindlingsAvroInstances.eventDecoder.decode(eventRecord)

  @Benchmark def kindlingsSimpleADT(): SimpleADT =
    KindlingsAvroInstances.simpleADTDecoder.decode(simpleADTRecord)
}
