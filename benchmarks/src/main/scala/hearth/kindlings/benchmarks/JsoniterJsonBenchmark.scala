package hearth.kindlings.benchmarks

import com.github.plokhotnyuk.jsoniter_scala.core.{readFromArray, writeToArray, JsonReader, JsonValueCodec}
import org.openjdk.jmh.annotations.*

import java.nio.charset.StandardCharsets
import java.util.concurrent.TimeUnit

/** A JSON document with every kind of value: strings, integers, decimals, exponents, big numbers, `null`s, nesting. */
object JsoniterJsonBenchData {

  val json: String = (1 to 50)
    .map { i =>
      s"""{"id":$i,"name":"user-$i","email":"user$i@example.com","score":${i * 1.25},"ratio":${i}e-3,""" +
        s""""balance":${4503599627370000L + i * 1000L},"nickname":null,"active":${i % 2 == 0},""" +
        s""""tags":["a$i","b$i","c$i"],"address":{"street":"$i Main St","zip":"${10000 + i}","geo":[${i * 0.5},-${i * 0.25}],"unit":null}}"""
    }
    .mkString("[", ",", "]")

  val bytes: Array[Byte] = json.getBytes(StandardCharsets.UTF_8)
}

/** `kindlings-jsoniter-json`'s `JsonCodec`. */
object KindlingsJsoniterJsonInstances {
  import hearth.kindlings.jsoniterjson.Json
  import hearth.kindlings.jsoniterjson.codec.JsonCodec

  val default: JsonValueCodec[Json] = JsonCodec.jsonValueCodec
  val sortKeys: JsonValueCodec[Json] = JsonCodec.jsonCodec(sortKeys = true)
  val dropNulls: JsonValueCodec[Json] = JsonCodec.jsonCodec(doSerialize = json => !json.isNull)
  val jsCompatible: JsonValueCodec[Json] = JsonCodec.jsonCodec(numberSerializer = JsonCodec.jsCompatibleNumberSerializer)
  val rounding: JsonValueCodec[Json] =
    JsonCodec.jsonCodec(numberParser = JsonCodec.numberParser(mc = JsonReader.bigDecimalMathContext))

  val value: Json = readFromArray(JsoniterJsonBenchData.bytes)(default)
}

/** jsoniter-scala-circe's `JsoniterScalaCodec` for circe's `Json`: the upstream baseline. */
object OriginalJsoniterCirceInstances {
  import com.github.plokhotnyuk.jsoniter_scala.circe.JsoniterScalaCodec
  import io.circe.Json

  val default: JsonValueCodec[Json] = JsoniterScalaCodec.jsonC3c
  val sortKeys: JsonValueCodec[Json] = JsoniterScalaCodec.jsonCodec(sortKeys = true)
  val dropNulls: JsonValueCodec[Json] = JsoniterScalaCodec.jsonCodec(doSerialize = _ ne Json.Null)
  val jsCompatible: JsonValueCodec[Json] =
    JsoniterScalaCodec.jsonCodec(numberSerializer = io.circe.JsoniterScalaCodec.jsCompatibleNumberSerializer)
  val rounding: JsonValueCodec[Json] =
    JsoniterScalaCodec.jsonCodec(numberParser =
      io.circe.JsoniterScalaCodec.numberParser(mc = JsonReader.bigDecimalMathContext)
    )

  val value: Json = readFromArray(JsoniterJsonBenchData.bytes)(default)
}

/** Reading and writing a JSON AST: `kindlings-jsoniter-json` vs circe's `Json` with jsoniter-scala-circe, with the
  * default codec and with each option of `jsonCodec`.
  */
@State(Scope.Thread)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
@Warmup(iterations = 5, time = 1)
@Measurement(iterations = 5, time = 1)
@Fork(2)
class JsoniterJsonBenchmark {

  private val bytes = JsoniterJsonBenchData.bytes
  private val k = KindlingsJsoniterJsonInstances
  private val u = OriginalJsoniterCirceInstances

  @Benchmark def readDefaultKindlings: Any = readFromArray(bytes)(k.default)
  @Benchmark def readDefaultUpstream: Any = readFromArray(bytes)(u.default)

  @Benchmark def readRoundingParserKindlings: Any = readFromArray(bytes)(k.rounding)
  @Benchmark def readRoundingParserUpstream: Any = readFromArray(bytes)(u.rounding)

  @Benchmark def writeDefaultKindlings: Array[Byte] = writeToArray(k.value)(k.default)
  @Benchmark def writeDefaultUpstream: Array[Byte] = writeToArray(u.value)(u.default)

  @Benchmark def writeSortKeysKindlings: Array[Byte] = writeToArray(k.value)(k.sortKeys)
  @Benchmark def writeSortKeysUpstream: Array[Byte] = writeToArray(u.value)(u.sortKeys)

  @Benchmark def writeDropNullsKindlings: Array[Byte] = writeToArray(k.value)(k.dropNulls)
  @Benchmark def writeDropNullsUpstream: Array[Byte] = writeToArray(u.value)(u.dropNulls)

  @Benchmark def writeJsCompatibleKindlings: Array[Byte] = writeToArray(k.value)(k.jsCompatible)
  @Benchmark def writeJsCompatibleUpstream: Array[Byte] = writeToArray(u.value)(u.jsCompatible)
}
