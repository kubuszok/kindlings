package hearth.kindlings.jsoniterjson.codec

import com.github.plokhotnyuk.jsoniter_scala.core.{JsonReader, JsonValueCodec, JsonWriter}
import hearth.kindlings.jsoniterjson.{Json, JsonNumber, JsonObject}

import java.nio.charset.StandardCharsets

/** Hand-written `JsonValueCodec[Json]` that reads/writes JSON tokens from/to jsoniter-scala's streaming API, like
  * jsoniter-scala-circe's `JsoniterScalaCodec.jsonC3c` for circe's `Json`:
  *
  *   - numbers are read as `BigDecimal`s to preserve precision (a negative zero keeps its sign: `-0`, `-0.0`) and
  *     written as their text (`JsonNumber.toString`);
  *   - nested arrays and objects deeper than [[DefaultMaxDepth]] are rejected when reading and writing (use
  *     [[withMaxDepth]] for another limit).
  */
object JsonCodec {

  /** The maximum depth of nested arrays and objects of [[jsonValueCodec]] (128, as jsoniter-scala-circe's). */
  final val DefaultMaxDepth = 128

  implicit val jsonValueCodec: JsonValueCodec[Json] = withMaxDepth(DefaultMaxDepth)

  /** A codec rejecting arrays and objects nested deeper than `maxDepth` (when reading and when writing). */
  def withMaxDepth(maxDepth: Int): JsonValueCodec[Json] = new JsonValueCodec[Json] {

    val nullValue: Json = Json.Null

    def decodeValue(in: JsonReader, default: Json): Json = decode(in, maxDepth)

    def encodeValue(x: Json, out: JsonWriter): Unit = encode(x, out, maxDepth)

    private def decode(in: JsonReader, depth: Int): Json = {
      val b = in.nextToken()
      if (b == 'n'.toByte) {
        in.readNullOrError(Json.Null, "expected null")
      } else if (b == 't'.toByte || b == 'f'.toByte) {
        in.rollbackToken()
        Json.Bool(in.readBoolean())
      } else if (b == '"'.toByte) {
        in.rollbackToken()
        Json.Str(in.readString(null))
      } else if (b == '['.toByte) {
        val depthM1 = depth - 1
        if (depthM1 < 0) in.decodeError("depth limit exceeded")
        if (in.isNextToken(']'.toByte)) Json.Arr(Vector.empty)
        else {
          in.rollbackToken()
          val builder = Vector.newBuilder[Json]
          builder += decode(in, depthM1)
          while (in.isNextToken(','.toByte)) builder += decode(in, depthM1)
          if (!in.isCurrentToken(']'.toByte)) in.decodeError("expected ']' or ','")
          Json.Arr(builder.result())
        }
      } else if (b == '{'.toByte) {
        val depthM1 = depth - 1
        if (depthM1 < 0) in.decodeError("depth limit exceeded")
        if (in.isNextToken('}'.toByte)) Json.Obj(JsonObject.empty)
        else {
          in.rollbackToken()
          val builder = Vector.newBuilder[(String, Json)]
          val key = in.readKeyAsString()
          builder += ((key, decode(in, depthM1)))
          while (in.isNextToken(','.toByte)) {
            val k = in.readKeyAsString()
            builder += ((k, decode(in, depthM1)))
          }
          if (!in.isCurrentToken('}'.toByte)) in.decodeError("expected '}' or ','")
          Json.Obj(JsonObject(builder.result()))
        }
      } else {
        val isNegative = b == '-'.toByte
        in.rollbackToken()
        val bd = in.readBigDecimal(null)
        // `BigDecimal` has no negative zero: keep the sign in the text, as circe does
        if (isNegative && bd.signum == 0) Json.Num(JsonNumber.fromString("-" + bd.toString).get)
        else Json.Num(JsonNumber.fromBigDecimal(bd))
      }
    }

    private def encode(json: Json, out: JsonWriter, depth: Int): Unit = json match {
      case Json.Null    => out.writeNull()
      case Json.Bool(b) => out.writeVal(b)
      case Json.Str(s)  => out.writeVal(s)
      // the text of a `JsonNumber` is always a valid JSON number (see `JsonNumber.fromString`)
      case Json.Num(n)  => out.writeRawVal(n.toString.getBytes(StandardCharsets.UTF_8))
      case Json.Arr(vs) =>
        val depthM1 = depth - 1
        if (depthM1 < 0) out.encodeError("depth limit exceeded")
        out.writeArrayStart()
        val iter = vs.iterator
        while (iter.hasNext) encode(iter.next(), out, depthM1)
        out.writeArrayEnd()
      case Json.Obj(obj) =>
        val depthM1 = depth - 1
        if (depthM1 < 0) out.encodeError("depth limit exceeded")
        out.writeObjectStart()
        val iter = obj.fields.iterator
        while (iter.hasNext) {
          val (key, value) = iter.next()
          out.writeKey(key)
          encode(value, out, depthM1)
        }
        out.writeObjectEnd()
    }
  }
}
