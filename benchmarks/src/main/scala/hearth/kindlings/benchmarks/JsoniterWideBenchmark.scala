package hearth.kindlings.benchmarks

import com.github.plokhotnyuk.jsoniter_scala.core.*
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import hearth.kindlings.jsoniterderivation.{JsoniterConfig, KindlingsJsonValueCodec}
import org.openjdk.jmh.annotations.*
import java.util.concurrent.TimeUnit

/** A case class wider than the 8 keys under which decoders compare keys one by one, with defaults. */
case class Wide20(
    id: Long = 0L,
    name: String = "",
    email: Option[String] = None,
    age: Int = 0,
    active: Boolean = false,
    score: Double = 0.0,
    city: String = "",
    country: String = "",
    zip: String = "",
    street: String = "",
    phone: Option[String] = None,
    company: String = "",
    title: String = "",
    department: String = "",
    level: Int = 0,
    salary: Long = 0L,
    rating: Double = 0.0,
    verified: Boolean = false,
    notes: Option[String] = None,
    tags: List[String] = Nil
)

object Wide20 {
  val value: Wide20 = Wide20(
    id = 42L,
    name = "Alice",
    email = Some("alice@example.com"),
    age = 30,
    active = true,
    score = 9.5,
    city = "Springfield",
    country = "US",
    zip = "62704",
    street = "123 Main St",
    phone = Some("555-0100"),
    company = "Acme",
    title = "Engineer",
    department = "R&D",
    level = 3,
    salary = 100000L,
    rating = 4.5,
    verified = true,
    notes = None,
    tags = List("a", "b")
  )
}

/** 100 fields: wider than the keys a decoder searches by hash itself. */
case class Wide100(
    w0: Option[String] = None,
    w1: Int = 7,
    w2: String = "d",
    w3: Long = 9L,
    w4: Boolean = true,
    w5: Option[Int] = Some(1),
    w6: Option[String] = None,
    w7: Int = 7,
    w8: String = "d",
    w9: Long = 9L,
    w10: Boolean = true,
    w11: Option[Int] = Some(1),
    w12: Option[String] = None,
    w13: Int = 7,
    w14: String = "d",
    w15: Long = 9L,
    w16: Boolean = true,
    w17: Option[Int] = Some(1),
    w18: Option[String] = None,
    w19: Int = 7,
    w20: String = "d",
    w21: Long = 9L,
    w22: Boolean = true,
    w23: Option[Int] = Some(1),
    w24: Option[String] = None,
    w25: Int = 7,
    w26: String = "d",
    w27: Long = 9L,
    w28: Boolean = true,
    w29: Option[Int] = Some(1),
    w30: Option[String] = None,
    w31: Int = 7,
    w32: String = "d",
    w33: Long = 9L,
    w34: Boolean = true,
    w35: Option[Int] = Some(1),
    w36: Option[String] = None,
    w37: Int = 7,
    w38: String = "d",
    w39: Long = 9L,
    w40: Boolean = true,
    w41: Option[Int] = Some(1),
    w42: Option[String] = None,
    w43: Int = 7,
    w44: String = "d",
    w45: Long = 9L,
    w46: Boolean = true,
    w47: Option[Int] = Some(1),
    w48: Option[String] = None,
    w49: Int = 7,
    w50: String = "d",
    w51: Long = 9L,
    w52: Boolean = true,
    w53: Option[Int] = Some(1),
    w54: Option[String] = None,
    w55: Int = 7,
    w56: String = "d",
    w57: Long = 9L,
    w58: Boolean = true,
    w59: Option[Int] = Some(1),
    w60: Option[String] = None,
    w61: Int = 7,
    w62: String = "d",
    w63: Long = 9L,
    w64: Boolean = true,
    w65: Option[Int] = Some(1),
    w66: Option[String] = None,
    w67: Int = 7,
    w68: String = "d",
    w69: Long = 9L,
    w70: Boolean = true,
    w71: Option[Int] = Some(1),
    w72: Option[String] = None,
    w73: Int = 7,
    w74: String = "d",
    w75: Long = 9L,
    w76: Boolean = true,
    w77: Option[Int] = Some(1),
    w78: Option[String] = None,
    w79: Int = 7,
    w80: String = "d",
    w81: Long = 9L,
    w82: Boolean = true,
    w83: Option[Int] = Some(1),
    w84: Option[String] = None,
    w85: Int = 7,
    w86: String = "d",
    w87: Long = 9L,
    w88: Boolean = true,
    w89: Option[Int] = Some(1),
    w90: Option[String] = None,
    w91: Int = 7,
    w92: String = "d",
    w93: Long = 9L,
    w94: Boolean = true,
    w95: Option[Int] = Some(1),
    w96: Option[String] = None,
    w97: Int = 7,
    w98: String = "d",
    w99: Long = 9L
)

/** Codecs whose config is only known at runtime (a parameter: the macro cannot evaluate it), the code path a config
  * defined next to the call site takes.
  */
object KindlingsJsoniterRuntimeConfigInstances {

  private def personCodec(config: JsoniterConfig): JsonValueCodec[Person] = {
    implicit val runtimeConfig: JsoniterConfig = config
    KindlingsJsonValueCodec.derived[Person]
  }

  private def wide20Codec(config: JsoniterConfig): JsonValueCodec[Wide20] = {
    implicit val runtimeConfig: JsoniterConfig = config
    KindlingsJsonValueCodec.derived[Wide20]
  }

  val person: JsonValueCodec[Person] = personCodec(JsoniterConfig.default)
  val wide20: JsonValueCodec[Wide20] = wide20Codec(JsoniterConfig.default)
}

object KindlingsJsoniterWideInstances {
  val wide20: JsonValueCodec[Wide20] = KindlingsJsonValueCodec.derived[Wide20]
  val wide100: JsonValueCodec[Wide100] = KindlingsJsonValueCodec.derived[Wide100]
}

object OriginalJsoniterWideInstances {
  val wide20: JsonValueCodec[Wide20] = JsonCodecMaker.make[Wide20]
  val wide100: JsonValueCodec[Wide100] = JsonCodecMaker.make[Wide100]
}

@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
@Warmup(iterations = 5, time = 1)
@Measurement(iterations = 10, time = 1)
@Fork(2)
class JsoniterWideReadBenchmark {

  private var personBytes: Array[Byte] = _
  private var wide20Bytes: Array[Byte] = _
  private var wide100Bytes: Array[Byte] = _

  @Setup(Level.Trial)
  def setup(): Unit = {
    personBytes = writeToArray(BenchmarkData.person)(KindlingsJsoniterInstances.personSemiAutoCodec)
    wide20Bytes = writeToArray(Wide20.value)(KindlingsJsoniterWideInstances.wide20)
    wide100Bytes = writeToArray(Wide100(w0 = Some("x"), w99 = 5L))(KindlingsJsoniterWideInstances.wide100)
  }

  @Benchmark def kindlingsWide20(): Wide20 =
    readFromArray(wide20Bytes)(KindlingsJsoniterWideInstances.wide20)

  @Benchmark def originalWide20(): Wide20 =
    readFromArray(wide20Bytes)(OriginalJsoniterWideInstances.wide20)

  @Benchmark def kindlingsWide100(): Wide100 =
    readFromArray(wide100Bytes)(KindlingsJsoniterWideInstances.wide100)

  @Benchmark def originalWide100(): Wide100 =
    readFromArray(wide100Bytes)(OriginalJsoniterWideInstances.wide100)

  @Benchmark def kindlingsRuntimeConfigWide20(): Wide20 =
    readFromArray(wide20Bytes)(KindlingsJsoniterRuntimeConfigInstances.wide20)

  @Benchmark def kindlingsRuntimeConfigPerson(): Person =
    readFromArray(personBytes)(KindlingsJsoniterRuntimeConfigInstances.person)
}
