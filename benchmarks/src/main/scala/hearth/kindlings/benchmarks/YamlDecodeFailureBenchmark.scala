package hearth.kindlings.benchmarks

import hearth.kindlings.yamlderivation.{KindlingsYamlDecoder, KindlingsYamlEncoder}
import org.virtuslab.yaml.{Node, YamlDecoder}
import org.virtuslab.yaml.Node.{MappingNode, ScalarNode, SequenceNode}
import org.openjdk.jmh.annotations.*
import java.util.concurrent.TimeUnit

// Decoding with the default (fail-fast) configuration, on valid input and on input with failures: the first field
// failing, several fields failing (incl. collection items), a nested failure.

final case class YamlChecked(name: String, age: Int, active: Boolean, ids: List[Int], score: Double, label: String)

object YamlFailureData {
  private def scalar(value: String): Node = ScalarNode(value)
  private def mapping(entries: (String, Node)*): Node =
    MappingNode(entries.map { case (k, v) => (ScalarNode(k): Node) -> v }.toMap)
  private def ints(values: String*): Node = SequenceNode(values.map(scalar)*)

  val valid: Node = mapping(
    "name" -> scalar("Alice"),
    "age" -> scalar("30"),
    "active" -> scalar("true"),
    "ids" -> ints("1", "2", "3", "4", "5", "6", "7", "8"),
    "score" -> scalar("1.5"),
    "label" -> scalar("x")
  )
  val firstFieldFails: Node = mapping(
    "name" -> scalar("Alice"),
    "age" -> scalar("thirty"),
    "active" -> scalar("true"),
    "ids" -> ints("1", "2", "3", "4", "5", "6", "7", "8"),
    "score" -> scalar("1.5"),
    "label" -> scalar("x")
  )
  val manyFieldsFail: Node = mapping(
    "name" -> scalar("Alice"),
    "age" -> scalar("thirty"),
    "active" -> scalar("maybe"),
    "ids" -> ints("1", "x", "3", "y", "5", "6", "7", "z"),
    "score" -> scalar("high"),
    "label" -> scalar("x")
  )
  // the second address is not a mapping
  val personFails: Node = {
    val node = KindlingsYamlEncoder.derived[Person].asNode(BenchmarkData.person)
    node match {
      case MappingNode(entries, _) =>
        MappingNode(entries.map {
          case (key @ ScalarNode("addresses", _), SequenceNode(items, _)) =>
            key -> (SequenceNode((items.take(1) :+ scalar("nowhere"))*): Node)
          case other => other
        })
      case other => other
    }
  }
}

object KindlingsYamlFailFastInstances {
  val checked: YamlDecoder[YamlChecked] = KindlingsYamlDecoder.derived[YamlChecked]
  val person: YamlDecoder[Person] = KindlingsYamlDecoder.derived[Person]
}

@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
@Warmup(iterations = 5, time = 1)
@Measurement(iterations = 10, time = 1)
@Fork(2)
class YamlDecodeFailureBenchmark {

  private val personNode: Node = KindlingsYamlEncoder.derived[Person].asNode(BenchmarkData.person)

  @Benchmark def failFastValid(): Either[Any, YamlChecked] =
    KindlingsYamlFailFastInstances.checked.construct(YamlFailureData.valid)

  @Benchmark def failFastFirstFieldFails(): Either[Any, YamlChecked] =
    KindlingsYamlFailFastInstances.checked.construct(YamlFailureData.firstFieldFails)

  @Benchmark def failFastManyFieldsFail(): Either[Any, YamlChecked] =
    KindlingsYamlFailFastInstances.checked.construct(YamlFailureData.manyFieldsFail)

  @Benchmark def failFastPersonValid(): Either[Any, Person] =
    KindlingsYamlFailFastInstances.person.construct(personNode)

  @Benchmark def failFastPersonFails(): Either[Any, Person] =
    KindlingsYamlFailFastInstances.person.construct(YamlFailureData.personFails)
}
