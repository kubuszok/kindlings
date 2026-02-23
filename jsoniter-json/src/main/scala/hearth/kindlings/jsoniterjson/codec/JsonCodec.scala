package hearth.kindlings.jsoniterjson.codec

import com.github.plokhotnyuk.jsoniter_scala.core.{JsonReader, JsonValueCodec, JsonWriter}
import hearth.kindlings.jsoniterjson.{Json, JsonNumber, JsonObject}

/** Hand-written `JsonValueCodec[Json]` that reads/writes JSON tokens from/to jsoniter-scala's streaming API.
  *
  * Numbers are read as `BigDecimal` to preserve precision.
  */
object JsonCodec {

  implicit val jsonValueCodec: JsonValueCodec[Json] = new JsonValueCodec[Json] {

    val nullValue: Json = Json.Null

    def decodeValue(in: JsonReader, default: Json): Json = decode(in)

    def encodeValue(x: Json, out: JsonWriter): Unit = encode(x, out)

    private def decode(in: JsonReader): Json = {
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
        if (in.isNextToken(']'.toByte)) Json.Arr(Vector.empty)
        else {
          in.rollbackToken()
          val builder = Vector.newBuilder[Json]
          builder += decode(in)
          while (in.isNextToken(','.toByte)) builder += decode(in)
          if (!in.isCurrentToken(']'.toByte)) in.decodeError("expected ']' or ','")
          Json.Arr(builder.result())
        }
      } else if (b == '{'.toByte) {
        if (in.isNextToken('}'.toByte)) Json.Obj(JsonObject.empty)
        else {
          in.rollbackToken()
          val builder = Vector.newBuilder[(String, Json)]
          val key = in.readKeyAsString()
          builder += ((key, decode(in)))
          while (in.isNextToken(','.toByte)) {
            val k = in.readKeyAsString()
            builder += ((k, decode(in)))
          }
          if (!in.isCurrentToken('}'.toByte)) in.decodeError("expected '}' or ','")
          Json.Obj(JsonObject(builder.result()))
        }
      } else {
        in.rollbackToken()
        val bd = in.readBigDecimal(null)
        Json.Num(JsonNumber.fromBigDecimal(bd))
      }
    }

    private def encode(json: Json, out: JsonWriter): Unit = json match {
      case Json.Null    => out.writeNull()
      case Json.Bool(b) => out.writeVal(b)
      case Json.Str(s)  => out.writeVal(s)
      case Json.Num(n)  =>
        n.toBigDecimal match {
          case Some(bd) => out.writeVal(bd)
          case None     => out.writeVal(n.value) // fallback to string representation
        }
      case Json.Arr(vs) =>
        out.writeArrayStart()
        val iter = vs.iterator
        while (iter.hasNext) encode(iter.next(), out)
        out.writeArrayEnd()
      case Json.Obj(obj) =>
        out.writeObjectStart()
        val iter = obj.fields.iterator
        while (iter.hasNext) {
          val (key, value) = iter.next()
          out.writeKey(key)
          encode(value, out)
        }
        out.writeObjectEnd()
    }
  }
}
