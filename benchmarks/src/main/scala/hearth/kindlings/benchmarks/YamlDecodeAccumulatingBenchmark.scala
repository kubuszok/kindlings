package hearth.kindlings.benchmarks

import hearth.kindlings.yamlderivation.{KindlingsYamlDecoder, KindlingsYamlEncoder, YamlConfig}
import org.virtuslab.yaml.{Node, YamlDecoder}
import org.openjdk.jmh.annotations.*
import java.util.concurrent.TimeUnit

// The same inputs as YamlDecodeFailureBenchmark, decoded with `YamlConfig.withAccumulatingErrors` (every failure
// reported with its path).

object KindlingsYamlAccumulatingInstances {
  implicit val config: YamlConfig = YamlConfig.default.withAccumulatingErrors
  val checked: YamlDecoder[YamlChecked] = KindlingsYamlDecoder.derived[YamlChecked]
  val person: YamlDecoder[Person] = KindlingsYamlDecoder.derived[Person]
}

@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
@Warmup(iterations = 5, time = 1)
@Measurement(iterations = 10, time = 1)
@Fork(2)
class YamlDecodeAccumulatingBenchmark {

  private val personNode: Node = KindlingsYamlEncoder.derived[Person].asNode(BenchmarkData.person)

  @Benchmark def accumulatingValid(): Either[Any, YamlChecked] =
    KindlingsYamlAccumulatingInstances.checked.construct(YamlFailureData.valid)

  @Benchmark def accumulatingFirstFieldFails(): Either[Any, YamlChecked] =
    KindlingsYamlAccumulatingInstances.checked.construct(YamlFailureData.firstFieldFails)

  @Benchmark def accumulatingManyFieldsFail(): Either[Any, YamlChecked] =
    KindlingsYamlAccumulatingInstances.checked.construct(YamlFailureData.manyFieldsFail)

  @Benchmark def accumulatingPersonValid(): Either[Any, Person] =
    KindlingsYamlAccumulatingInstances.person.construct(personNode)

  @Benchmark def accumulatingPersonFails(): Either[Any, Person] =
    KindlingsYamlAccumulatingInstances.person.construct(YamlFailureData.personFails)
}
