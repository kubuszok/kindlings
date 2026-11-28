package hearth.kindlings.benchmarks

import hearth.kindlings.yamlderivation.{KindlingsYamlDecoder, KindlingsYamlEncoder}
import org.virtuslab.yaml.{Node, YamlCodec, YamlDecoder, YamlEncoder}
import org.openjdk.jmh.annotations.*
import java.util.concurrent.TimeUnit

// Map keys: Kindlings reads and writes them with key codecs (the exact `String` fast path, the built-in `Int` codec, enum
// names), scala-yaml with `YamlDecoder[K]` / `YamlEncoder[K]` through `forMap`.

final case class YamlStringIntMap(values: Map[String, Int])
final case class YamlStringEndpointMap(values: Map[String, YamlEndpoint])
final case class YamlIntEndpointMap(values: Map[Int, YamlEndpoint])
sealed trait YamlKeyColor
object YamlKeyColor {
  case object Red extends YamlKeyColor
  case object Orange extends YamlKeyColor
  case object Yellow extends YamlKeyColor
  case object Green extends YamlKeyColor
  case object Blue extends YamlKeyColor
  case object Indigo extends YamlKeyColor
  case object Violet extends YamlKeyColor
  case object Black extends YamlKeyColor
  val all: List[YamlKeyColor] = List(Red, Orange, Yellow, Green, Blue, Indigo, Violet, Black)
}
final case class YamlEnumIntMap(values: Map[YamlKeyColor, Int])

object KindlingsYamlKeyInstances {
  val stringIntEncoder: YamlEncoder[YamlStringIntMap] = KindlingsYamlEncoder.derived[YamlStringIntMap]
  val stringIntDecoder: YamlDecoder[YamlStringIntMap] = KindlingsYamlDecoder.derived[YamlStringIntMap]
  val stringEndpointEncoder: YamlEncoder[YamlStringEndpointMap] = KindlingsYamlEncoder.derived[YamlStringEndpointMap]
  val stringEndpointDecoder: YamlDecoder[YamlStringEndpointMap] = KindlingsYamlDecoder.derived[YamlStringEndpointMap]
  val intEndpointEncoder: YamlEncoder[YamlIntEndpointMap] = KindlingsYamlEncoder.derived[YamlIntEndpointMap]
  val intEndpointDecoder: YamlDecoder[YamlIntEndpointMap] = KindlingsYamlDecoder.derived[YamlIntEndpointMap]
  val enumIntEncoder: YamlEncoder[YamlEnumIntMap] = KindlingsYamlEncoder.derived[YamlEnumIntMap]
  val enumIntDecoder: YamlDecoder[YamlEnumIntMap] = KindlingsYamlDecoder.derived[YamlEnumIntMap]
}

object ScalaYamlKeyInstances {
  given endpoint: YamlCodec[YamlEndpoint] = YamlCodec.derived
  val stringInt: YamlCodec[YamlStringIntMap] = YamlCodec.derived
  val stringEndpoint: YamlCodec[YamlStringEndpointMap] = YamlCodec.derived
  val intEndpoint: YamlCodec[YamlIntEndpointMap] = YamlCodec.derived
}

object YamlKeyData {
  private val endpoints = List.tabulate(20)(i => YamlEndpoint(s"host-$i", 8000 + i))
  val stringInt: YamlStringIntMap = YamlStringIntMap(List.tabulate(20)(i => s"key-$i" -> i).toMap)
  val stringEndpoint: YamlStringEndpointMap = YamlStringEndpointMap(endpoints.map(e => e.host -> e).toMap)
  val intEndpoint: YamlIntEndpointMap = YamlIntEndpointMap(endpoints.map(e => e.port -> e).toMap)
  val enumInt: YamlEnumIntMap = YamlEnumIntMap(YamlKeyColor.all.zipWithIndex.toMap)
}

@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
@Warmup(iterations = 5, time = 1)
@Measurement(iterations = 5, time = 1)
@Fork(2)
class YamlKeyCodecBenchmark {

  private var stringIntNode: Node = scala.compiletime.uninitialized
  private var stringEndpointNode: Node = scala.compiletime.uninitialized
  private var intEndpointNode: Node = scala.compiletime.uninitialized
  private var enumIntNode: Node = scala.compiletime.uninitialized

  @Setup(Level.Trial)
  def setup(): Unit = {
    stringIntNode = ScalaYamlKeyInstances.stringInt.asNode(YamlKeyData.stringInt)
    stringEndpointNode = ScalaYamlKeyInstances.stringEndpoint.asNode(YamlKeyData.stringEndpoint)
    intEndpointNode = ScalaYamlKeyInstances.intEndpoint.asNode(YamlKeyData.intEndpoint)
    enumIntNode = KindlingsYamlKeyInstances.enumIntEncoder.asNode(YamlKeyData.enumInt)
  }

  @Benchmark def kindlingsEncodeStringInt(): Node =
    KindlingsYamlKeyInstances.stringIntEncoder.asNode(YamlKeyData.stringInt)
  @Benchmark def scalaYamlEncodeStringInt(): Node = ScalaYamlKeyInstances.stringInt.asNode(YamlKeyData.stringInt)
  @Benchmark def kindlingsDecodeStringInt(): Either[Any, YamlStringIntMap] =
    KindlingsYamlKeyInstances.stringIntDecoder.construct(stringIntNode)
  @Benchmark def scalaYamlDecodeStringInt(): Either[Any, YamlStringIntMap] =
    ScalaYamlKeyInstances.stringInt.construct(stringIntNode)

  @Benchmark def kindlingsEncodeStringEndpoint(): Node =
    KindlingsYamlKeyInstances.stringEndpointEncoder.asNode(YamlKeyData.stringEndpoint)
  @Benchmark def scalaYamlEncodeStringEndpoint(): Node =
    ScalaYamlKeyInstances.stringEndpoint.asNode(YamlKeyData.stringEndpoint)
  @Benchmark def kindlingsDecodeStringEndpoint(): Either[Any, YamlStringEndpointMap] =
    KindlingsYamlKeyInstances.stringEndpointDecoder.construct(stringEndpointNode)
  @Benchmark def scalaYamlDecodeStringEndpoint(): Either[Any, YamlStringEndpointMap] =
    ScalaYamlKeyInstances.stringEndpoint.construct(stringEndpointNode)

  @Benchmark def kindlingsEncodeIntEndpoint(): Node =
    KindlingsYamlKeyInstances.intEndpointEncoder.asNode(YamlKeyData.intEndpoint)
  @Benchmark def scalaYamlEncodeIntEndpoint(): Node =
    ScalaYamlKeyInstances.intEndpoint.asNode(YamlKeyData.intEndpoint)
  @Benchmark def kindlingsDecodeIntEndpoint(): Either[Any, YamlIntEndpointMap] =
    KindlingsYamlKeyInstances.intEndpointDecoder.construct(intEndpointNode)
  @Benchmark def scalaYamlDecodeIntEndpoint(): Either[Any, YamlIntEndpointMap] =
    ScalaYamlKeyInstances.intEndpoint.construct(intEndpointNode)

  // scala-yaml has no instances for enum keys to compare with (its derived sum encoder writes a case object as `{}`)
  @Benchmark def kindlingsEncodeEnumInt(): Node = KindlingsYamlKeyInstances.enumIntEncoder.asNode(YamlKeyData.enumInt)
  @Benchmark def kindlingsDecodeEnumInt(): Either[Any, YamlEnumIntMap] =
    KindlingsYamlKeyInstances.enumIntDecoder.construct(enumIntNode)
}
