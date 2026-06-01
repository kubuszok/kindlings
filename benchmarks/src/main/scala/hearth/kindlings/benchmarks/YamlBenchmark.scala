package hearth.kindlings.benchmarks

import hearth.kindlings.yamlderivation.{KindlingsYamlDecoder, KindlingsYamlEncoder}
import org.virtuslab.yaml.{Node, YamlDecoder, YamlEncoder}
import org.openjdk.jmh.annotations.*
import java.util.concurrent.TimeUnit

object KindlingsYamlInstances {
  val simpleCCEncoder: YamlEncoder[SimpleCC] = KindlingsYamlEncoder.derived[SimpleCC]
  val simpleCCDecoder: YamlDecoder[SimpleCC] = KindlingsYamlDecoder.derived[SimpleCC]
  val personEncoder: YamlEncoder[Person] = KindlingsYamlEncoder.derived[Person]
  val personDecoder: YamlDecoder[Person] = KindlingsYamlDecoder.derived[Person]
  val eventEncoder: YamlEncoder[Event] = KindlingsYamlEncoder.derived[Event]
  val eventDecoder: YamlDecoder[Event] = KindlingsYamlDecoder.derived[Event]
  val simpleADTEncoder: YamlEncoder[SimpleADT] = KindlingsYamlEncoder.derived[SimpleADT]
  val simpleADTDecoder: YamlDecoder[SimpleADT] = KindlingsYamlDecoder.derived[SimpleADT]
}

@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
@Warmup(iterations = 5, time = 1)
@Measurement(iterations = 10, time = 1)
@Fork(2)
class YamlEncodeBenchmark {

  @Benchmark def kindlingsSimpleCC(): Node =
    KindlingsYamlInstances.simpleCCEncoder.asNode(BenchmarkData.simpleCC)

  @Benchmark def kindlingsPerson(): Node =
    KindlingsYamlInstances.personEncoder.asNode(BenchmarkData.person)

  @Benchmark def kindlingsEvent(): Node =
    KindlingsYamlInstances.eventEncoder.asNode(BenchmarkData.event)

  @Benchmark def kindlingsSimpleADT(): Node =
    KindlingsYamlInstances.simpleADTEncoder.asNode(BenchmarkData.simpleADT)
}

@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
@Warmup(iterations = 5, time = 1)
@Measurement(iterations = 10, time = 1)
@Fork(2)
class YamlDecodeBenchmark {

  private var simpleCCNode: Node = _
  private var personNode: Node = _
  private var eventNode: Node = _
  private var simpleADTNode: Node = _

  @Setup(Level.Trial)
  def setup(): Unit = {
    simpleCCNode = KindlingsYamlInstances.simpleCCEncoder.asNode(BenchmarkData.simpleCC)
    personNode = KindlingsYamlInstances.personEncoder.asNode(BenchmarkData.person)
    eventNode = KindlingsYamlInstances.eventEncoder.asNode(BenchmarkData.event)
    simpleADTNode = KindlingsYamlInstances.simpleADTEncoder.asNode(BenchmarkData.simpleADT)
  }

  @Benchmark def kindlingsSimpleCC(): Either[Any, SimpleCC] =
    KindlingsYamlInstances.simpleCCDecoder.construct(simpleCCNode)

  @Benchmark def kindlingsPerson(): Either[Any, Person] =
    KindlingsYamlInstances.personDecoder.construct(personNode)

  @Benchmark def kindlingsEvent(): Either[Any, Event] =
    KindlingsYamlInstances.eventDecoder.construct(eventNode)

  @Benchmark def kindlingsSimpleADT(): Either[Any, SimpleADT] =
    KindlingsYamlInstances.simpleADTDecoder.construct(simpleADTNode)
}
