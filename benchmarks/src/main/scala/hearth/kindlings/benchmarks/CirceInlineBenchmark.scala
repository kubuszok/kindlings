package hearth.kindlings.benchmarks

import hearth.kindlings.circederivation.KindlingsDecoder
import io.circe.{DecodingFailure, Json}
import org.openjdk.jmh.annotations.*
import java.util.concurrent.TimeUnit

/** The inline `KindlingsDecoder.decode[A](json)`: the decoder is derived at the call site and runs once per call. */
@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
@Warmup(iterations = 5, time = 1)
@Measurement(iterations = 10, time = 1)
@Fork(2)
class CirceInlineDecodeBenchmark {

  private var simpleCCJson: Json = _
  private var personJson: Json = _

  @Setup(Level.Trial)
  def setup(): Unit = {
    simpleCCJson = KindlingsCirceInstances.simpleCCSemiAutoEncoder(BenchmarkData.simpleCC)
    personJson = KindlingsCirceInstances.personSemiAutoEncoder(BenchmarkData.person)
  }

  @Benchmark def kindlingsInlineSimpleCC(): Either[DecodingFailure, SimpleCC] =
    KindlingsDecoder.decode[SimpleCC](simpleCCJson)

  @Benchmark def kindlingsInlinePerson(): Either[DecodingFailure, Person] =
    KindlingsDecoder.decode[Person](personJson)

  @Benchmark def kindlingsSemiAutoPerson(): Either[DecodingFailure, Person] =
    KindlingsCirceInstances.personSemiAutoDecoder.decodeJson(personJson)
}
