package hearth.kindlings.benchmarks

import com.github.plokhotnyuk.jsoniter_scala.core.*
import org.openjdk.jmh.annotations.*
import java.util.concurrent.TimeUnit

/** `SimpleCC` in a JVM which has used several codecs of the same library before, as an application does (the other
  * jsoniter benchmarks run one codec per JVM).
  *
  * All Kindlings codecs are instances of one class (`JsoniterDerivationFactories.lazyCodecInstance`) delegating to
  * lambdas, so the call sites of those lambdas are shared by every codec: once several codecs were used they are
  * megamorphic for the JIT. jsoniter-scala's `JsonCodecMaker.make` generates one class per codec.
  */
@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
@Warmup(iterations = 5, time = 1)
@Measurement(iterations = 10, time = 1)
@Fork(2)
class JsoniterManyCodecsBenchmark {

  private var simpleCCBytes: Array[Byte] = _

  @Setup(Level.Trial)
  def setup(): Unit = {
    simpleCCBytes = writeToArray(BenchmarkData.simpleCC)(OriginalJsoniterSemiAutoInstances.simpleCCCodec)
    val personBytes = writeToArray(BenchmarkData.person)(OriginalJsoniterSemiAutoInstances.personCodec)
    val eventBytes = writeToArray(BenchmarkData.event)(OriginalJsoniterSemiAutoInstances.eventCodec)
    val adtBytes = writeToArray(BenchmarkData.simpleADT)(OriginalJsoniterSemiAutoInstances.simpleADTCodec)
    // (Kindlings' default ADT encoding differs from jsoniter-scala's: each library reads its own output)
    val kPersonBytes = writeToArray(BenchmarkData.person)(KindlingsJsoniterInstances.personSemiAutoCodec)
    val kEventBytes = writeToArray(BenchmarkData.event)(KindlingsJsoniterInstances.eventSemiAutoCodec)
    val kAdtBytes = writeToArray(BenchmarkData.simpleADT)(KindlingsJsoniterInstances.simpleADTSemiAutoCodec)
    var i = 0
    var sink = 0
    while (i < 20000) {
      sink += writeToArray(BenchmarkData.person)(KindlingsJsoniterInstances.personSemiAutoCodec).length
      sink += writeToArray(BenchmarkData.event)(KindlingsJsoniterInstances.eventSemiAutoCodec).length
      sink += writeToArray(BenchmarkData.simpleADT)(KindlingsJsoniterInstances.simpleADTSemiAutoCodec).length
      sink += writeToArray(BenchmarkData.person)(OriginalJsoniterSemiAutoInstances.personCodec).length
      sink += writeToArray(BenchmarkData.event)(OriginalJsoniterSemiAutoInstances.eventCodec).length
      sink += writeToArray(BenchmarkData.simpleADT)(OriginalJsoniterSemiAutoInstances.simpleADTCodec).length
      sink += readFromArray(kPersonBytes)(KindlingsJsoniterInstances.personSemiAutoCodec).hashCode
      sink += readFromArray(kEventBytes)(KindlingsJsoniterInstances.eventSemiAutoCodec).hashCode
      sink += readFromArray(kAdtBytes)(KindlingsJsoniterInstances.simpleADTSemiAutoCodec).hashCode
      sink += readFromArray(personBytes)(OriginalJsoniterSemiAutoInstances.personCodec).hashCode
      sink += readFromArray(eventBytes)(OriginalJsoniterSemiAutoInstances.eventCodec).hashCode
      sink += readFromArray(adtBytes)(OriginalJsoniterSemiAutoInstances.simpleADTCodec).hashCode
      i += 1
    }
    if (sink == 42) println(sink)
  }

  @Benchmark def kindlingsWriteSimpleCC(): Array[Byte] =
    writeToArray(BenchmarkData.simpleCC)(KindlingsJsoniterInstances.simpleCCSemiAutoCodec)

  @Benchmark def originalWriteSimpleCC(): Array[Byte] =
    writeToArray(BenchmarkData.simpleCC)(OriginalJsoniterSemiAutoInstances.simpleCCCodec)

  @Benchmark def kindlingsReadSimpleCC(): SimpleCC =
    readFromArray(simpleCCBytes)(KindlingsJsoniterInstances.simpleCCSemiAutoCodec)

  @Benchmark def originalReadSimpleCC(): SimpleCC =
    readFromArray(simpleCCBytes)(OriginalJsoniterSemiAutoInstances.simpleCCCodec)
}
