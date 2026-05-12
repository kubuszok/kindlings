package hearth.kindlings.benchmarks

import com.github.plokhotnyuk.jsoniter_scala.core.*
import io.circe.Json
import org.openjdk.jmh.annotations.*
import java.util.concurrent.TimeUnit

// --- Show: 1-method type class ---

@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
@Warmup(iterations = 5, time = 1)
@Measurement(iterations = 10, time = 1)
@Fork(2)
class AnonVsLambdaShowBenchmark {

  @Benchmark def anonSimpleCC(): String =
    AnonClassInstances.showSimpleCC.show(BenchmarkData.simpleCC)

  @Benchmark def factorySimpleCC(): String =
    FactoryInstances.showSimpleCC.show(BenchmarkData.simpleCC)

  @Benchmark def anonPerson(): String =
    AnonClassInstances.showPerson.show(BenchmarkData.person)

  @Benchmark def factoryPerson(): String =
    FactoryInstances.showPerson.show(BenchmarkData.person)

  @Benchmark def anonEvent(): String =
    AnonClassInstances.showEvent.show(BenchmarkData.event)

  @Benchmark def factoryEvent(): String =
    FactoryInstances.showEvent.show(BenchmarkData.event)
}

// --- Hash: 2-method type class ---

@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
@Warmup(iterations = 5, time = 1)
@Measurement(iterations = 10, time = 1)
@Fork(2)
class AnonVsLambdaHashBenchmark {

  private val simpleCCOther = BenchmarkData.simpleCC.copy(name = "Bob")

  @Benchmark def anonHashSimpleCC(): Int =
    AnonClassInstances.hashSimpleCC.hash(BenchmarkData.simpleCC)

  @Benchmark def factoryHashSimpleCC(): Int =
    FactoryInstances.hashSimpleCC.hash(BenchmarkData.simpleCC)

  @Benchmark def anonEqvSimpleCC(): Boolean =
    AnonClassInstances.hashSimpleCC.eqv(BenchmarkData.simpleCC, simpleCCOther)

  @Benchmark def factoryEqvSimpleCC(): Boolean =
    FactoryInstances.hashSimpleCC.eqv(BenchmarkData.simpleCC, simpleCCOther)

  @Benchmark def anonHashPerson(): Int =
    AnonClassInstances.hashPerson.hash(BenchmarkData.person)

  @Benchmark def factoryHashPerson(): Int =
    FactoryInstances.hashPerson.hash(BenchmarkData.person)

  @Benchmark def anonEqvPerson(): Boolean =
    AnonClassInstances.hashPerson.eqv(BenchmarkData.person, BenchmarkData.person)

  @Benchmark def factoryEqvPerson(): Boolean =
    FactoryInstances.hashPerson.eqv(BenchmarkData.person, BenchmarkData.person)
}

// --- Encoder: 1-method type class with complex body ---

@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
@Warmup(iterations = 5, time = 1)
@Measurement(iterations = 10, time = 1)
@Fork(2)
class AnonVsLambdaEncoderBenchmark {

  @Benchmark def anonSimpleCC(): Json =
    AnonClassInstances.encoderSimpleCC(BenchmarkData.simpleCC)

  @Benchmark def factorySimpleCC(): Json =
    FactoryInstances.encoderSimpleCC(BenchmarkData.simpleCC)

  @Benchmark def anonPerson(): Json =
    AnonClassInstances.encoderPerson(BenchmarkData.person)

  @Benchmark def factoryPerson(): Json =
    FactoryInstances.encoderPerson(BenchmarkData.person)
}

// --- JsonValueCodec: 3-method type class ---

@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
@Warmup(iterations = 5, time = 1)
@Measurement(iterations = 10, time = 1)
@Fork(2)
class AnonVsLambdaCodecWriteBenchmark {

  @Benchmark def anonSimpleCC(): Array[Byte] =
    writeToArray(BenchmarkData.simpleCC)(AnonClassInstances.codecSimpleCC)

  @Benchmark def factorySimpleCC(): Array[Byte] =
    writeToArray(BenchmarkData.simpleCC)(FactoryInstances.codecSimpleCC)
}

@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
@Warmup(iterations = 5, time = 1)
@Measurement(iterations = 10, time = 1)
@Fork(2)
class AnonVsLambdaCodecReadBenchmark {

  private var simpleCCBytes: Array[Byte] = _

  @Setup(Level.Trial)
  def setup(): Unit =
    simpleCCBytes = writeToArray(BenchmarkData.simpleCC)(AnonClassInstances.codecSimpleCC)

  @Benchmark def anonSimpleCC(): SimpleCC =
    readFromArray(simpleCCBytes)(AnonClassInstances.codecSimpleCC)

  @Benchmark def factorySimpleCC(): SimpleCC =
    readFromArray(simpleCCBytes)(FactoryInstances.codecSimpleCC)
}

// --- Functor: polymorphic type class with erasure-based factory ---

@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
@Warmup(iterations = 5, time = 1)
@Measurement(iterations = 10, time = 1)
@Fork(2)
class AnonVsLambdaFunctorBenchmark {

  @Benchmark def anonMapSimpleCCBox(): SimpleCCBox[String] =
    AnonClassInstances.functorSimpleCCBox.map(BenchmarkData.simpleCCBox)(_.toString)

  @Benchmark def factoryMapSimpleCCBox(): SimpleCCBox[String] =
    FactoryInstances.functorSimpleCCBox.map(BenchmarkData.simpleCCBox)(_.toString)
}

// --- Instance creation overhead ---

@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
@Warmup(iterations = 5, time = 1)
@Measurement(iterations = 10, time = 1)
@Fork(2)
class AnonVsLambdaCreationBenchmark {

  @Benchmark def anonShowCreation(): cats.Show[SimpleCC] =
    new cats.Show[SimpleCC] {
      def show(a: SimpleCC): String = HandWrittenImpls.showSimpleCC(a)
    }

  @Benchmark def factoryShowCreation(): cats.Show[SimpleCC] =
    TypeClassFactories.showInstance(HandWrittenImpls.showSimpleCC)

  @Benchmark def anonHashCreation(): cats.kernel.Hash[SimpleCC] =
    new cats.kernel.Hash[SimpleCC] {
      def hash(x: SimpleCC): Int = HandWrittenImpls.hashSimpleCC(x)
      def eqv(x: SimpleCC, y: SimpleCC): Boolean = HandWrittenImpls.eqvSimpleCC(x, y)
    }

  @Benchmark def factoryHashCreation(): cats.kernel.Hash[SimpleCC] =
    TypeClassFactories.hashInstance(HandWrittenImpls.hashSimpleCC, HandWrittenImpls.eqvSimpleCC)

  @Benchmark def anonCodecCreation(): com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec[SimpleCC] =
    new com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec[SimpleCC] {
      def nullValue: SimpleCC = null
      def decodeValue(in: JsonReader, default: SimpleCC): SimpleCC =
        HandWrittenImpls.decodeSimpleCCJsoniter(in, default)
      def encodeValue(x: SimpleCC, out: JsonWriter): Unit =
        HandWrittenImpls.encodeSimpleCCJsoniter(x, out)
    }

  @Benchmark def factoryCodecCreation(): com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec[SimpleCC] =
    TypeClassFactories.codecInstance(
      null,
      HandWrittenImpls.decodeSimpleCCJsoniter,
      HandWrittenImpls.encodeSimpleCCJsoniter
    )
}
