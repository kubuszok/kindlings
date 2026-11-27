package hearth.kindlings.benchmarks

import hearth.kindlings.yamlderivation.{KindlingsYamlDecoder, KindlingsYamlEncoder}
import org.virtuslab.yaml.{Node, YamlCodec, YamlDecoder, YamlEncoder}
import org.openjdk.jmh.annotations.*
import java.util.concurrent.TimeUnit

// Kindlings vs scala-yaml's own derivation (Scala 3 only) on the paths where Kindlings follows scala-yaml's instances
// without delegating to them: collections and maps of derived types (String keys, keys of other types), constructor
// defaults of absent fields, private constructor fields.

final case class YamlEndpoint(host: String, port: Int)
final case class YamlRegistry(
    endpoints: List[YamlEndpoint],
    byName: Map[String, YamlEndpoint],
    byId: Map[Int, YamlEndpoint]
)
final case class YamlSettings(
    name: String,
    retries: Int = 3,
    timeout: Long = 1000L,
    label: String = "default",
    tags: List[String] = Nil,
    private val secret: String = "s"
)

object KindlingsYamlParityInstances {
  val registryEncoder: YamlEncoder[YamlRegistry] = KindlingsYamlEncoder.derived[YamlRegistry]
  val registryDecoder: YamlDecoder[YamlRegistry] = KindlingsYamlDecoder.derived[YamlRegistry]
  val settingsEncoder: YamlEncoder[YamlSettings] = KindlingsYamlEncoder.derived[YamlSettings]
  val settingsDecoder: YamlDecoder[YamlSettings] = KindlingsYamlDecoder.derived[YamlSettings]
}

object ScalaYamlParityInstances {
  given endpoint: YamlCodec[YamlEndpoint] = YamlCodec.derived
  val registry: YamlCodec[YamlRegistry] = YamlCodec.derived
  val settings: YamlCodec[YamlSettings] = YamlCodec.derived
}

object YamlParityData {
  private val endpoints = List.tabulate(20)(i => YamlEndpoint(s"host-$i", 8000 + i))
  val registry: YamlRegistry =
    YamlRegistry(endpoints, endpoints.map(e => e.host -> e).toMap, endpoints.map(e => e.port -> e).toMap)
  val settings: YamlSettings = YamlSettings("service")
}

@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
@Warmup(iterations = 5, time = 1)
@Measurement(iterations = 10, time = 1)
@Fork(2)
class YamlParityBenchmark {

  private var registryNode: Node = scala.compiletime.uninitialized
  private var settingsNode: Node = scala.compiletime.uninitialized
  private var settingsNameOnlyNode: Node = scala.compiletime.uninitialized

  @Setup(Level.Trial)
  def setup(): Unit = {
    registryNode = ScalaYamlParityInstances.registry.asNode(YamlParityData.registry)
    settingsNode = ScalaYamlParityInstances.settings.asNode(YamlParityData.settings)
    // every field with a default is absent
    settingsNameOnlyNode = Node.MappingNode(Node.ScalarNode("name") -> Node.ScalarNode("service"))
  }

  @Benchmark def kindlingsEncodeRegistry(): Node =
    KindlingsYamlParityInstances.registryEncoder.asNode(YamlParityData.registry)
  @Benchmark def scalaYamlEncodeRegistry(): Node =
    ScalaYamlParityInstances.registry.asNode(YamlParityData.registry)

  @Benchmark def kindlingsDecodeRegistry(): Either[Any, YamlRegistry] =
    KindlingsYamlParityInstances.registryDecoder.construct(registryNode)
  @Benchmark def scalaYamlDecodeRegistry(): Either[Any, YamlRegistry] =
    ScalaYamlParityInstances.registry.construct(registryNode)

  @Benchmark def kindlingsEncodeSettings(): Node =
    KindlingsYamlParityInstances.settingsEncoder.asNode(YamlParityData.settings)
  @Benchmark def scalaYamlEncodeSettings(): Node =
    ScalaYamlParityInstances.settings.asNode(YamlParityData.settings)

  @Benchmark def kindlingsDecodeSettings(): Either[Any, YamlSettings] =
    KindlingsYamlParityInstances.settingsDecoder.construct(settingsNode)
  @Benchmark def scalaYamlDecodeSettings(): Either[Any, YamlSettings] =
    ScalaYamlParityInstances.settings.construct(settingsNode)

  @Benchmark def kindlingsDecodeSettingsDefaults(): Either[Any, YamlSettings] =
    KindlingsYamlParityInstances.settingsDecoder.construct(settingsNameOnlyNode)
  @Benchmark def scalaYamlDecodeSettingsDefaults(): Either[Any, YamlSettings] =
    ScalaYamlParityInstances.settings.construct(settingsNameOnlyNode)
}
