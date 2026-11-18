package hearth.kindlings.benchmarks

import hearth.kindlings.yamlderivation.{KindlingsYamlDecoder, KindlingsYamlEncoder}
import org.virtuslab.yaml.{Node, YamlDecoder, YamlEncoder}
import org.openjdk.jmh.annotations.*
import java.util.concurrent.TimeUnit

/** 25 fields: the cost of looking up each field in the mapping grows with the number of fields. */
case class YamlWide25(
    y0: String,
    y1: Int,
    y2: Option[String],
    y3: Long,
    y4: Boolean,
    y5: String,
    y6: Int,
    y7: Option[String],
    y8: Long,
    y9: Boolean,
    y10: String,
    y11: Int,
    y12: Option[String],
    y13: Long,
    y14: Boolean,
    y15: String,
    y16: Int,
    y17: Option[String],
    y18: Long,
    y19: Boolean,
    y20: String,
    y21: Int,
    y22: Option[String],
    y23: Long,
    y24: Boolean
)

object YamlWide25 {
  val value: YamlWide25 = YamlWide25(
    y0 = "s",
    y1 = 1,
    y2 = Some("o"),
    y3 = 2L,
    y4 = true,
    y5 = "s",
    y6 = 1,
    y7 = Some("o"),
    y8 = 2L,
    y9 = true,
    y10 = "s",
    y11 = 1,
    y12 = Some("o"),
    y13 = 2L,
    y14 = true,
    y15 = "s",
    y16 = 1,
    y17 = Some("o"),
    y18 = 2L,
    y19 = true,
    y20 = "s",
    y21 = 1,
    y22 = Some("o"),
    y23 = 2L,
    y24 = true
  )
  val encoder: YamlEncoder[YamlWide25] = KindlingsYamlEncoder.derived[YamlWide25]
  val decoder: YamlDecoder[YamlWide25] = KindlingsYamlDecoder.derived[YamlWide25]
}

@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
@Warmup(iterations = 5, time = 1)
@Measurement(iterations = 10, time = 1)
@Fork(2)
class YamlWideDecodeBenchmark {

  private var wide25Node: Node = _
  private var wide25ParsedNode: Node = _

  @Setup(Level.Trial)
  def setup(): Unit = {
    import org.virtuslab.yaml.*
    wide25Node = YamlWide25.encoder.asNode(YamlWide25.value)
    wide25ParsedNode = wide25Node.asYaml.asNode.toOption.get
  }

  @Benchmark def kindlingsWide25(): Either[Any, YamlWide25] =
    YamlWide25.decoder.construct(wide25Node)

  @Benchmark def kindlingsWide25Parsed(): Either[Any, YamlWide25] =
    YamlWide25.decoder.construct(wide25ParsedNode)
}
