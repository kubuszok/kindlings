package hearth.kindlings.jsoniterjson.codec

import com.github.plokhotnyuk.jsoniter_scala.core.{JsonReader, JsonValueCodec, JsonWriter}
import hearth.kindlings.jsoniterjson.{Json, JsonNumber, JsonObject}

import java.nio.charset.StandardCharsets

/** Hand-written `JsonValueCodec[Json]` that reads/writes JSON tokens from/to jsoniter-scala's streaming API, like
  * jsoniter-scala-circe's `JsoniterScalaCodec.jsonC3c` for circe's `Json`:
  *
  *   - numbers are read exactly, to preserve precision (a negative zero keeps its sign: `-0`, `-0.0`), and written as
  *     their text (`JsonNumber.toString`);
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

  /** The default number serializer: the text of the number as it is. */
  val defaultNumberSerializer: (JsonWriter, JsonNumber) => Unit = (out, n) => writeNumberText(out, n)

  /** A number serializer for JavaScript clients, like jsoniter-scala-circe's `jsCompatibleNumberSerializer`: numbers
    * that lose precision when JavaScript parses them to `Number` are written as JSON strings - integers outside
    * `[-2^52, 2^52)`, decimals with more than 52 bits in the unscaled value or a scale outside `[-256, 256]`. A number
    * whose text is the text of a `Double` (as [[JsonNumber.fromDouble]] and [[JsonNumber.fromFloat]] write it) is
    * always written as a JSON number.
    */
  val jsCompatibleNumberSerializer: (JsonWriter, JsonNumber) => Unit = (out, n) => {
    val text = n.toString
    val long = plainLong(text)
    if (long != NotALong) {
      if (long >= -4503599627370496L && long < 4503599627370496L) out.writeVal(long)
      else out.writeValAsString(long)
    } else if (isShortPlainDecimal(text)) writeRawText(out, text) // (what upstream writes for its `BigDecimal`)
    else
      n.toBigDecimal match {
        case Some(bd) =>
          val u = bd.bigDecimal
          val scale = u.scale
          val fits = u.unscaledValue.bitLength <= 52 && scale >= -256 && scale <= 256
          // Only texts a `Double` could have (an exponent, a negative zero, more than 52 bits) are compared with the
          // `Double`'s text; the others are written as upstream writes the `BigDecimal` (also when `fromString` got
          // another spelling of it).
          val maybeDoubleText = !fits || u.signum == 0 || text.indexOf('E') >= 0
          if (maybeDoubleText && isDoubleText(n, text)) writeRawText(out, text)
          else if (u.signum == 0 && text.charAt(0) == '-') out.writeVal(0) // upstream: a negative zero is written as 0
          else if (fits) out.writeVal(u)
          else out.writeValAsString(u)
        case None => out.writeNonEscapedAsciiVal(text)
      }
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

  /** The text of the number as it is (always a valid JSON number, see `JsonNumber.fromString`). */
  private def writeNumberText(out: JsonWriter, n: JsonNumber): Unit = writeRawText(out, n.toString)

  private def writeRawText(out: JsonWriter, text: String): Unit =
    out.writeRawVal(text.getBytes(StandardCharsets.ISO_8859_1)) // (ASCII)

  /** A decimal written without an exponent, with at most 15 digits (so less than 2^52 unscaled, and a scale within
    * `[0, 256]`) and a non-zero digit in its integer part or its first 6 fraction digits: `java.math.BigDecimal` (and
    * so upstream's serializer) writes such a number exactly as this text, as a JSON number.
    */
  private def isShortPlainDecimal(text: String): Boolean = {
    val len = text.length
    if (len > 17) false // sign + 15 digits + point
    else {
      var i = if (text.charAt(0) == '-') 1 else 0
      var digits = 0
      var point = -1
      var significantAt = -1 // the first non-zero digit
      var plain = true
      while (plain && i < len) {
        val c = text.charAt(i)
        if (c >= '0' && c <= '9') {
          digits += 1
          if (significantAt < 0 && c != '0') significantAt = i
        } else if (c == '.') point = i
        else plain = false
        i += 1
      }
      plain && point >= 0 && digits <= 15 && significantAt >= 0 && significantAt <= point + 6
    }
  }

  // (`Long.MinValue` itself is then handled as a decimal: it is outside of the JS-safe range either way)
  final private val NotALong = Long.MinValue

  /** The value of a number written as plain digits (`-?[0-9]+`) that fits a `Long`, or [[NotALong]]. */
  private def plainLong(text: String): Long = {
    val len = text.length
    if (len > 20) NotALong
    else {
      var i = if (text.charAt(0) == '-') 1 else 0
      while (i < len && text.charAt(i) >= '0' && text.charAt(i) <= '9') i += 1
      if (i != len) NotALong
      else
        try text.toLong
        catch { case _: NumberFormatException => NotALong }
    }
  }

  /** Whether `text` is how a `Double` is written (then JavaScript reads the very same `Double` back). */
  private def isDoubleText(n: JsonNumber, text: String): Boolean = n.toDouble match {
    case Some(d) => JsonNumber.fromDouble(d).exists(_.toString == text)
    case None    => false
  }

  private def readNumber(in: JsonReader, mc: java.math.MathContext, scaleLimit: Int, digitsLimit: Int): Json = {
    val isNegative = in.isNextToken('-'.toByte)
    in.rollbackToken()
    // a `Long`, a `java.math.BigDecimal` or a `java.math.BigInteger`; none has a negative zero: its sign is kept in
    // the text, as circe does
    val number = in.readNumber(null, mc, scaleLimit, digitsLimit)
    val text = number.toString
    val isZero = number match {
      case l: java.lang.Long        => l.longValue == 0L
      case bd: java.math.BigDecimal => bd.signum == 0
      case _                        => false
    }
    Json.Num(JsonNumber.unsafeFromJsonText(if (isNegative && isZero) "-" + text else text))
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
        if (numberSerializer eq null) writeNumberText(out, n) else numberSerializer(out, n)
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
        if (sortKeys) encodeSortedFields(obj, out, depthM1)
        else if (doSerialize eq null) {
          val iter = obj.fields.iterator
          while (iter.hasNext) {
            val kv = iter.next()
            out.writeKey(kv._1)
            encode(kv._2, out, depthM1)
          }
        } else {
          val iter = obj.fields.iterator
          while (iter.hasNext) {
            val kv = iter.next()
            if (doSerialize(kv._2)) {
              out.writeKey(kv._1)
              encode(kv._2, out, depthM1)
            }
          }
        }
        out.writeObjectEnd()
      case Json.Null => out.writeNull()
    }

    private def encodeSortedFields(obj: JsonObject, out: JsonWriter, depthM1: Int): Unit = {
      val fields = new Array[(String, Json)](obj.fields.size)
      val _ = obj.fields.copyToArray(fields, 0)
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
