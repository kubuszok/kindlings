package hearth.kindlings.jsoniterjson.codec

import com.github.plokhotnyuk.jsoniter_scala.core.{JsonReader, JsonValueCodec, JsonWriter}
import hearth.kindlings.jsoniterjson.{Json, JsonNumber, JsonObject}

import java.nio.charset.StandardCharsets

/** Hand-written `JsonValueCodec[Json]` that reads/writes JSON tokens from/to jsoniter-scala's streaming API, like
  * jsoniter-scala-circe's `JsoniterScalaCodec.jsonC3c` for circe's `Json`:
  *
  *   - numbers are read exactly, to preserve precision (a negative zero keeps its sign: `-0`, `-0.0`), and written as
  *     the value they were made from (their text, `JsonNumber.toString`);
  *   - nested arrays and objects deeper than [[DefaultMaxDepth]] are rejected when reading and writing (use
  *     [[withMaxDepth]] for another limit).
  *
  * [[jsonCodec]] has the options of jsoniter-scala-circe's `JsoniterScalaCodec.jsonCodec`: the depth limit, a filter of
  * the object fields to write, the number parser and serializer, and sorting of the keys.
  */
object JsonCodec {

  /** The maximum depth of nested arrays and objects of [[jsonValueCodec]] (128, as jsoniter-scala-circe's). */
  final val DefaultMaxDepth = 128

  /** The default of [[jsonCodec]]'s `doSerialize`: every field is written. */
  val serializeAll: Json => Boolean = _ => true

  /** The default number parser: numbers are read exactly, within `JsonReader`'s default limits of digits (308) and
    * scale (6178).
    */
  val defaultNumberParser: JsonReader => Json = in =>
    readNumber(in, java.math.MathContext.UNLIMITED, JsonReader.bigDecimalScaleLimit, JsonReader.bigDecimalDigitsLimit)

  /** The default number serializer, like jsoniter-scala-circe's: a number made from a `Long`, a `Float`, a `Double` or
    * a `BigDecimal` is written by the writer as that value, another one as its text.
    */
  val defaultNumberSerializer: (JsonWriter, JsonNumber) => Unit = (out, n) => writeNumber(out, n)

  /** A number serializer for JavaScript clients, like jsoniter-scala-circe's `jsCompatibleNumberSerializer`: numbers
    * that lose precision when JavaScript parses them to `Number` are written as JSON strings - integers outside
    * `[-2^52, 2^52)`, decimals with more than 52 bits in the unscaled value or a scale outside `[-256, 256]`. Numbers
    * made from a `Double` or a `Float` are always written as JSON numbers.
    */
  val jsCompatibleNumberSerializer: (JsonWriter, JsonNumber) => Unit = (out, n) =>
    (n.kind: @scala.annotation.switch) match {
      case JsonNumber.LongKind =>
        val v = n.longValue
        if (v >= -4503599627370496L && v < 4503599627370496L) out.writeVal(v)
        else out.writeValAsString(v)
      case JsonNumber.FloatKind      => out.writeVal(n.floatValue)
      case JsonNumber.DoubleKind     => out.writeVal(n.doubleValue)
      case JsonNumber.BigDecimalKind => writeJsCompatible(out, n.bigDecimalValue)
      case _                         =>
        n.toBigDecimal match {
          // (as circe's `toBigDecimal`: a zero of any sign and scale is `0`)
          case Some(bd) =>
            val v = bd.bigDecimal
            writeJsCompatible(out, if (v.signum == 0) java.math.BigDecimal.ZERO else v)
          case None => out.writeNonEscapedAsciiVal(n.toString)
        }
    }

  private def writeJsCompatible(out: JsonWriter, v: java.math.BigDecimal): Unit = {
    val scale = v.scale
    if (v.unscaledValue.bitLength <= 52 && scale >= -256 && scale <= 256) out.writeVal(v)
    else out.writeValAsString(v)
  }

  implicit val jsonValueCodec: JsonValueCodec[Json] = withMaxDepth(DefaultMaxDepth)

  /** A codec rejecting arrays and objects nested deeper than `maxDepth` (when reading and when writing). */
  def withMaxDepth(maxDepth: Int): JsonValueCodec[Json] = new ConfiguredCodec(maxDepth, null, null, null, false)

  /** A codec with the options of jsoniter-scala-circe's `JsoniterScalaCodec.jsonCodec` (its `initialSize`, a size hint
    * for circe's map of fields, has no counterpart: a [[JsonObject]] is a vector of fields).
    *
    * @param maxDepth
    *   the maximum depth of nested arrays and objects, when reading and when writing
    * @param doSerialize
    *   object fields whose value it rejects are not written (e.g. `_ != Json.Null` to drop `null` fields)
    * @param numberParser
    *   reads a JSON number (the reader is positioned at its first character); see [[numberParser]]
    * @param numberSerializer
    *   writes a JSON number, e.g. [[jsCompatibleNumberSerializer]]
    * @param sortKeys
    *   write the fields of every object sorted by key (`String.compareTo`; fields with the same key keep their order)
    */
  def jsonCodec(
      maxDepth: Int = DefaultMaxDepth,
      doSerialize: Json => Boolean = serializeAll,
      numberParser: JsonReader => Json = defaultNumberParser,
      numberSerializer: (JsonWriter, JsonNumber) => Unit = defaultNumberSerializer,
      sortKeys: Boolean = false
  ): JsonValueCodec[Json] = new ConfiguredCodec(
    maxDepth,
    // the defaults are inlined by the codec (no call per field / per number)
    if (doSerialize eq serializeAll) null else doSerialize,
    if (numberParser eq defaultNumberParser) null else numberParser,
    if (numberSerializer eq defaultNumberSerializer) null else numberSerializer,
    sortKeys
  )

  /** A number parser like jsoniter-scala-circe's `JsoniterScalaCodec.numberParser`: the number keeps its value (and a
    * negative zero its sign), rounded with `mc`; numbers with `digitsLimit` or more digits, or a scale of `scaleLimit`
    * or more, are rejected.
    *
    * @param mc
    *   the math context for rounding of decimal values (`MathContext.UNLIMITED` reads them exactly)
    * @param scaleLimit
    *   the exclusive limit of the absolute scale
    * @param digitsLimit
    *   the exclusive limit of decimal digits in the mantissa
    */
  def numberParser(
      mc: java.math.MathContext = java.math.MathContext.UNLIMITED,
      scaleLimit: Int = JsonReader.bigDecimalScaleLimit,
      digitsLimit: Int = JsonReader.bigDecimalDigitsLimit
  ): JsonReader => Json = in => readNumber(in, mc, scaleLimit, digitsLimit)

  private def writeNumber(out: JsonWriter, n: JsonNumber): Unit =
    (n.kind: @scala.annotation.switch) match {
      case JsonNumber.LongKind       => out.writeVal(n.longValue)
      case JsonNumber.FloatKind      => out.writeVal(n.floatValue)
      case JsonNumber.DoubleKind     => out.writeVal(n.doubleValue)
      case JsonNumber.BigDecimalKind => out.writeVal(n.bigDecimalValue)
      // the text of a `JsonNumber` is always a valid JSON number (see `JsonNumber.fromString`)
      case _ => out.writeRawVal(n.toString.getBytes(StandardCharsets.ISO_8859_1))
    }

  /** The kinds of numbers jsoniter-scala-circe's parser makes: a `Long` when the number is an integer that fits one,
    * else an exact `BigDecimal`; neither has a negative zero, which is kept as its text (as circe does).
    */
  private def readNumber(in: JsonReader, mc: java.math.MathContext, scaleLimit: Int, digitsLimit: Int): Json = {
    val isNegative = in.isNextToken('-'.toByte)
    in.rollbackToken()
    Json.Num(in.readNumber(null, mc, scaleLimit, digitsLimit) match {
      case l: java.lang.Long =>
        val v = l.longValue
        if (v != 0L || !isNegative) JsonNumber.fromLong(v) else JsonNumber.unsafeFromJsonText("-0")
      case bd: java.math.BigDecimal =>
        if (bd.signum != 0 || !isNegative) JsonNumber.fromJavaBigDecimal(bd)
        else JsonNumber.unsafeFromJsonText("-" + bd.toString)
      case bi: java.math.BigInteger => JsonNumber.fromJavaBigDecimal(new java.math.BigDecimal(bi))
      case other                    => JsonNumber.unsafeFromJsonText(other.toString)
    })
  }

  private val EmptyArray: Json = Json.Arr(Vector.empty)
  private val EmptyObject: Json = Json.Obj(JsonObject.empty)
  private val KeyComparator: java.util.Comparator[(String, Json)] = new java.util.Comparator[(String, Json)] {
    def compare(a: (String, Json), b: (String, Json)): Int = a._1.compareTo(b._1)
  }

  final private class ConfiguredCodec(
      maxDepth: Int,
      doSerialize: Json => Boolean, // null: every field
      numberParser: JsonReader => Json, // null: `defaultNumberParser`
      numberSerializer: (JsonWriter, JsonNumber) => Unit, // null: `defaultNumberSerializer`
      sortKeys: Boolean
  ) extends JsonValueCodec[Json] {

    val nullValue: Json = Json.Null

    def decodeValue(in: JsonReader, default: Json): Json = decode(in, maxDepth)

    def encodeValue(x: Json, out: JsonWriter): Unit = encode(x, out, maxDepth)

    private def decode(in: JsonReader, depth: Int): Json = {
      val b = in.nextToken()
      if (b == '"'.toByte) {
        in.rollbackToken()
        Json.Str(in.readString(null))
      } else if (b == 't'.toByte || b == 'f'.toByte) {
        in.rollbackToken()
        if (in.readBoolean()) Json.True else Json.False
      } else if ((b >= '0'.toByte && b <= '9'.toByte) || b == '-'.toByte) {
        in.rollbackToken()
        if (numberParser eq null)
          readNumber(
            in,
            java.math.MathContext.UNLIMITED,
            JsonReader.bigDecimalScaleLimit,
            JsonReader.bigDecimalDigitsLimit
          )
        else numberParser(in)
      } else if (b == '['.toByte) {
        val depthM1 = depth - 1
        if (depthM1 < 0) in.decodeError("depth limit exceeded")
        if (in.isNextToken(']'.toByte)) EmptyArray
        else {
          in.rollbackToken()
          val builder = Vector.newBuilder[Json]
          builder += decode(in, depthM1)
          while (in.isNextToken(','.toByte)) builder += decode(in, depthM1)
          if (!in.isCurrentToken(']'.toByte)) in.arrayEndOrCommaError()
          Json.Arr(builder.result())
        }
      } else if (b == '{'.toByte) {
        val depthM1 = depth - 1
        if (depthM1 < 0) in.decodeError("depth limit exceeded")
        if (in.isNextToken('}'.toByte)) EmptyObject
        else {
          in.rollbackToken()
          val builder = Vector.newBuilder[(String, Json)]
          val key = in.readKeyAsString()
          builder += ((key, decode(in, depthM1)))
          while (in.isNextToken(','.toByte)) {
            val k = in.readKeyAsString()
            builder += ((k, decode(in, depthM1)))
          }
          if (!in.isCurrentToken('}'.toByte)) in.objectEndOrCommaError()
          Json.Obj(JsonObject(builder.result()))
        }
      } else in.readNullOrError(Json.Null, "expected JSON value")
    }

    private def encode(json: Json, out: JsonWriter, depth: Int): Unit = json match {
      case Json.Str(s)  => out.writeVal(s)
      case Json.Bool(b) => out.writeVal(b)
      case Json.Num(n)  =>
        if (numberSerializer eq null) writeNumber(out, n) else numberSerializer(out, n)
      case Json.Arr(vs) =>
        val depthM1 = depth - 1
        if (depthM1 < 0) out.encodeError("depth limit exceeded")
        out.writeArrayStart()
        // (by index: a `Vector`'s iterator is a large object, allocated once per array / object otherwise)
        val size = vs.length
        var i = 0
        while (i < size) {
          encode(vs(i), out, depthM1)
          i += 1
        }
        out.writeArrayEnd()
      case Json.Obj(obj) =>
        val depthM1 = depth - 1
        if (depthM1 < 0) out.encodeError("depth limit exceeded")
        out.writeObjectStart()
        if (sortKeys) encodeSortedFields(obj, out, depthM1)
        else {
          val fields = obj.fields
          val size = fields.length
          var i = 0
          if (doSerialize eq null)
            while (i < size) {
              val kv = fields(i)
              out.writeKey(kv._1)
              encode(kv._2, out, depthM1)
              i += 1
            }
          else
            while (i < size) {
              val kv = fields(i)
              if (doSerialize(kv._2)) {
                out.writeKey(kv._1)
                encode(kv._2, out, depthM1)
              }
              i += 1
            }
        }
        out.writeObjectEnd()
      case Json.Null => out.writeNull()
    }

    private def encodeSortedFields(obj: JsonObject, out: JsonWriter, depthM1: Int): Unit = {
      val source = obj.fields
      val fields = new Array[(String, Json)](source.length)
      var j = 0
      while (j < fields.length) {
        fields(j) = source(j)
        j += 1
      }
      java.util.Arrays.sort(fields, KeyComparator) // stable: fields with the same key keep their order
      var i = 0
      while (i < fields.length) {
        val kv = fields(i)
        i += 1
        if ((doSerialize eq null) || doSerialize(kv._2)) {
          out.writeKey(kv._1)
          encode(kv._2, out, depthM1)
        }
      }
    }
  }
}
