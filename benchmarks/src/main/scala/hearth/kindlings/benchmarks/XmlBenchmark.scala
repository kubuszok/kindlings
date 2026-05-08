package hearth.kindlings.benchmarks

import hearth.kindlings.xmlderivation.{KindlingsXmlDecoder, KindlingsXmlEncoder, XmlDecoder, XmlEncoder}
import org.openjdk.jmh.annotations._
import java.util.concurrent.TimeUnit

object KindlingsXmlInstances {
  val simpleCCEncoder: XmlEncoder[SimpleCC] = KindlingsXmlEncoder.derive[SimpleCC]
  val simpleCCDecoder: XmlDecoder[SimpleCC] = KindlingsXmlDecoder.derive[SimpleCC]
  val addressEncoder: XmlEncoder[Address] = KindlingsXmlEncoder.derive[Address]
  val addressDecoder: XmlDecoder[Address] = KindlingsXmlDecoder.derive[Address]
}

@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
@Warmup(iterations = 5, time = 1)
@Measurement(iterations = 10, time = 1)
@Fork(2)
class XmlEncodeBenchmark {

  @Benchmark def kindlingsSimpleCC(): scala.xml.Elem =
    KindlingsXmlInstances.simpleCCEncoder.encode(BenchmarkData.simpleCC, "SimpleCC")

  @Benchmark def kindlingsAddress(): scala.xml.Elem =
    KindlingsXmlInstances.addressEncoder.encode(BenchmarkData.address, "Address")
}

@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
@Warmup(iterations = 5, time = 1)
@Measurement(iterations = 10, time = 1)
@Fork(2)
class XmlDecodeBenchmark {

  private var simpleCCElem: scala.xml.Elem = _
  private var addressElem: scala.xml.Elem = _

  @Setup(Level.Trial)
  def setup(): Unit = {
    simpleCCElem = KindlingsXmlInstances.simpleCCEncoder.encode(BenchmarkData.simpleCC, "SimpleCC")
    addressElem = KindlingsXmlInstances.addressEncoder.encode(BenchmarkData.address, "Address")
  }

  @Benchmark def kindlingsSimpleCC(): Either[Any, SimpleCC] =
    KindlingsXmlInstances.simpleCCDecoder.decode(simpleCCElem)

  @Benchmark def kindlingsAddress(): Either[Any, Address] =
    KindlingsXmlInstances.addressDecoder.decode(addressElem)
}
