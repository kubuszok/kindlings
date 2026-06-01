package hearth.kindlings.benchmarks

import hearth.kindlings.avroderivation.{AvroDecoder, AvroEncoder}
import org.apache.avro.generic.GenericRecord
import org.openjdk.jmh.annotations.*
import java.util.concurrent.TimeUnit

object KindlingsAvroInstances {
  val simpleCCEncoder: AvroEncoder[SimpleCC] = AvroEncoder.derived[SimpleCC]
  val simpleCCDecoder: AvroDecoder[SimpleCC] = AvroDecoder.derived[SimpleCC]
  val personEncoder: AvroEncoder[Person] = AvroEncoder.derived[Person]
  val personDecoder: AvroDecoder[Person] = AvroDecoder.derived[Person]
  val eventEncoder: AvroEncoder[Event] = AvroEncoder.derived[Event]
  val eventDecoder: AvroDecoder[Event] = AvroDecoder.derived[Event]
  val simpleADTEncoder: AvroEncoder[SimpleADT] = AvroEncoder.derived[SimpleADT]
  val simpleADTDecoder: AvroDecoder[SimpleADT] = AvroDecoder.derived[SimpleADT]
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

  @Benchmark def originalSemiAutoSimpleCC(): GenericRecord =
    OriginalAvroSemiAutoInstances.simpleCCFormat.to(BenchmarkData.simpleCC)

  @Benchmark def originalAutoSimpleCC(): GenericRecord =
    OriginalAvroAutoInstances.simpleCCFormat.to(BenchmarkData.simpleCC)

  @Benchmark def kindlingsPerson(): Any =
    KindlingsAvroInstances.personEncoder.encode(BenchmarkData.person)

  @Benchmark def originalSemiAutoPerson(): GenericRecord =
    OriginalAvroSemiAutoInstances.personFormat.to(BenchmarkData.person)

  @Benchmark def originalAutoPerson(): GenericRecord =
    OriginalAvroAutoInstances.personFormat.to(BenchmarkData.person)

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
    simpleCCGenericRecord = OriginalAvroAutoInstances.simpleCCFormat.to(BenchmarkData.simpleCC)
    personGenericRecord = OriginalAvroAutoInstances.personFormat.to(BenchmarkData.person)
  }

  @Benchmark def kindlingsSimpleCC(): SimpleCC =
    KindlingsAvroInstances.simpleCCDecoder.decode(simpleCCRecord)

  @Benchmark def originalSemiAutoSimpleCC(): SimpleCC =
    OriginalAvroSemiAutoInstances.simpleCCFormat.from(simpleCCGenericRecord)

  @Benchmark def originalAutoSimpleCC(): SimpleCC =
    OriginalAvroAutoInstances.simpleCCFormat.from(simpleCCGenericRecord)

  @Benchmark def kindlingsPerson(): Person =
    KindlingsAvroInstances.personDecoder.decode(personRecord)

  @Benchmark def originalSemiAutoPerson(): Person =
    OriginalAvroSemiAutoInstances.personFormat.from(personGenericRecord)

  @Benchmark def originalAutoPerson(): Person =
    OriginalAvroAutoInstances.personFormat.from(personGenericRecord)

  @Benchmark def kindlingsEvent(): Event =
    KindlingsAvroInstances.eventDecoder.decode(eventRecord)

  @Benchmark def kindlingsSimpleADT(): SimpleADT =
    KindlingsAvroInstances.simpleADTDecoder.decode(simpleADTRecord)
}
