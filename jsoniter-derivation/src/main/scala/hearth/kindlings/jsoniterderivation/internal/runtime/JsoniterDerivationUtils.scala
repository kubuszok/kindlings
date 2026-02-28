package hearth.kindlings.jsoniterderivation.internal.runtime

import com.github.plokhotnyuk.jsoniter_scala.core.{JsonKeyCodec, JsonReader, JsonValueCodec, JsonWriter}
import hearth.kindlings.jsoniterderivation.KindlingsJsonCodec

object JsoniterDerivationUtils {

  def jsonCodec[A](
      valueCodec: JsonValueCodec[A],
      keyCodec: JsonKeyCodec[A]
  ): KindlingsJsonCodec[A] = new KindlingsJsonCodec[A] {
    def nullValue: A = valueCodec.nullValue
    def decodeValue(in: JsonReader, default: A): A = valueCodec.decodeValue(in, default)
    def encodeValue(x: A, out: JsonWriter): Unit = valueCodec.encodeValue(x, out)
    def decodeKey(in: JsonReader): A = keyCodec.decodeKey(in)
    def encodeKey(x: A, out: JsonWriter): Unit = keyCodec.encodeKey(x, out)
  }

  // --- Encoder helpers ---

  def writeArray[A](out: JsonWriter, items: Iterable[A], encodeItem: A => Unit): Unit = {
    out.writeArrayStart()
    val iter = items.iterator
    while (iter.hasNext) encodeItem(iter.next())
    out.writeArrayEnd()
  }

  def writeMap[P, V](
      out: JsonWriter,
      pairs: Iterable[P],
      extractKey: P => String,
      encodeValue: V => Unit
  ): Unit = {
    out.writeObjectStart()
    val iter = pairs.iterator
    while (iter.hasNext) {
      val p = iter.next()
      out.writeKey(extractKey(p))
      encodeValue(p.asInstanceOf[V]) // The caller should handle the value extraction
    }
    out.writeObjectEnd()
  }

  def writeMapStringKeyed[V](
      out: JsonWriter,
      entries: Iterable[(String, V)],
      encodeValue: V => Unit
  ): Unit = {
    out.writeObjectStart()
    val iter = entries.iterator
    while (iter.hasNext) {
      val (key, value) = iter.next()
      out.writeKey(key)
      encodeValue(value)
    }
    out.writeObjectEnd()
  }

  def writeEnumAsString(out: JsonWriter, typeName: String): Unit =
    out.writeVal(typeName)

  def writeScalaEnumValueId(out: JsonWriter, value: Any): Unit =
    out.writeVal(value.asInstanceOf[scala.Enumeration#Value].id)

  def readEnumAsString[A](in: JsonReader)(dispatch: String => A): A = {
    val typeName = in.readString(null)
    dispatch(typeName)
  }

  def readScalaEnumValueId[A](in: JsonReader)(dispatch: Int => A): A = {
    val id = in.readInt()
    dispatch(id)
  }

  def scalaEnumValueId(value: Any): Int =
    value.asInstanceOf[scala.Enumeration#Value].id

  def readCirceLikeObject[A](in: JsonReader)(
      stringDispatch: String => A,
      wrapperDispatch: String => A
  ): A =
    if (in.isNextToken('"')) {
      in.rollbackToken()
      readEnumAsString[A](in)(stringDispatch)
    } else {
      in.rollbackToken()
      readWrapped[A](in)(wrapperDispatch)
    }

  def writeWrapped(out: JsonWriter, typeName: String)(encodeInner: => Unit): Unit = {
    out.writeObjectStart()
    out.writeKey(typeName)
    encodeInner
    out.writeObjectEnd()
  }

  def writeWithDiscriminator(out: JsonWriter, discriminatorField: String, typeName: String)(
      encodeFields: => Unit
  ): Unit = {
    // We need to inject the discriminator as the first field.
    // The encodeFields writes: writeObjectStart(), writeKey(f1), ..., writeObjectEnd()
    // But we need to intercept to add the discriminator before the other fields.
    // Since we can't intercept, we'll write the discriminator first then relay the other fields.
    out.writeObjectStart()
    out.writeKey(discriminatorField)
    out.writeVal(typeName)
    // Now the encodeFields would normally write start/fields/end for a case class.
    // We need encodeFields to write just the fields without start/end.
    // This requires a different approach - encodeFields should just be the field writes.
    // For now, let's handle this by not calling encodeFields directly.
    // Instead, encodeFields is the full object encoding. We'll need to restructure.
    // Actually, the macro generates writeObjectStart + fields + writeObjectEnd for case classes.
    // For discriminator mode, we need to replace that with just the fields.
    // Let's handle this at the macro level instead.
    encodeFields // This includes writeObjectStart/writeObjectEnd which is wrong for discriminator mode
    out.writeObjectEnd()
  }

  // --- Decoder helpers ---

  def readOption[A](in: JsonReader)(decodeInner: JsonReader => A): Option[A] =
    if (in.isNextToken('n'.toByte)) {
      in.readNullOrError[Option[A]](None, "expected null")
    } else {
      in.rollbackToken()
      Some(decodeInner(in))
    }

  def readCollection[Item, Coll](
      in: JsonReader,
      decodeItem: JsonReader => Item,
      factory: scala.collection.Factory[Item, Coll],
      maxInserts: Int
  ): Coll = {
    val builder = factory.newBuilder
    if (!in.isNextToken('['.toByte)) in.decodeError("expected '[' or null")
    if (!in.isNextToken(']'.toByte)) {
      in.rollbackToken()
      var count = 1
      builder += decodeItem(in)
      while (in.isNextToken(','.toByte)) {
        count += 1
        if (count > maxInserts) in.decodeError(s"too many collection items (max: $maxInserts)")
        builder += decodeItem(in)
      }
      if (!in.isCurrentToken(']'.toByte)) in.decodeError("expected ']' or ','")
    }
    builder.result()
  }

  def readMap[V, M](
      in: JsonReader,
      decodeValue: JsonReader => V,
      factory: scala.collection.Factory[(String, V), M],
      maxInserts: Int
  ): M = {
    val builder = factory.newBuilder
    if (!in.isNextToken('{'.toByte)) in.decodeError("expected '{' or null")
    if (!in.isNextToken('}'.toByte)) {
      in.rollbackToken()
      var count = 1
      val key = in.readKeyAsString()
      builder += ((key, decodeValue(in)))
      while (in.isNextToken(','.toByte)) {
        count += 1
        if (count > maxInserts) in.decodeError(s"too many map entries (max: $maxInserts)")
        val k = in.readKeyAsString()
        builder += ((k, decodeValue(in)))
      }
      if (!in.isCurrentToken('}'.toByte)) in.decodeError("expected '}' or ','")
    }
    builder.result()
  }

  def readEmptyObject(in: JsonReader): Unit = {
    if (!in.isNextToken('{'.toByte)) in.decodeError("expected '{'")
    if (!in.isNextToken('}'.toByte)) in.decodeError("expected '}'")
  }

  def readObject[A](
      in: JsonReader,
      fieldCount: Int,
      construct: Array[Any] => A
  )(dispatch: (String, Array[Any], JsonReader) => Unit): A = {
    val arr = new Array[Any](fieldCount)
    if (!in.isNextToken('{'.toByte)) in.decodeError("expected '{'")
    if (!in.isNextToken('}'.toByte)) {
      in.rollbackToken()
      val key = in.readKeyAsString()
      dispatch(key, arr, in)
      while (in.isNextToken(','.toByte)) {
        val k = in.readKeyAsString()
        dispatch(k, arr, in)
      }
      if (!in.isCurrentToken('}'.toByte)) in.decodeError("expected '}' or ','")
    }
    construct(arr)
  }

  /** Read remaining fields of an already-opened object (e.g., after discriminator field was consumed). */
  def readObjectInline[A](
      in: JsonReader,
      fieldCount: Int,
      construct: Array[Any] => A
  )(dispatch: (String, Array[Any], JsonReader) => Unit): A = {
    val arr = new Array[Any](fieldCount)
    // Object is already open and discriminator key-value has been read.
    // Check for more fields (comma after discriminator value).
    if (in.isNextToken(','.toByte)) {
      val key = in.readKeyAsString()
      dispatch(key, arr, in)
      while (in.isNextToken(','.toByte)) {
        val k = in.readKeyAsString()
        dispatch(k, arr, in)
      }
      if (!in.isCurrentToken('}'.toByte)) in.decodeError("expected '}' or ','")
    } else {
      if (!in.isCurrentToken('}'.toByte)) in.decodeError("expected '}'")
    }
    construct(arr)
  }

  def readWrapped[A](in: JsonReader)(dispatch: String => A): A = {
    if (!in.isNextToken('{'.toByte)) in.decodeError("expected '{' for wrapped enum")
    val typeName = in.readKeyAsString()
    val result = dispatch(typeName)
    if (!in.isNextToken('}'.toByte)) in.decodeError("expected '}' after wrapped enum value")
    result
  }

  def readWithDiscriminator[A](in: JsonReader, discriminatorField: String)(dispatch: String => A): A = {
    // Read the object, find the discriminator field, then dispatch
    // For simplicity, we require the discriminator to appear first
    if (!in.isNextToken('{'.toByte)) in.decodeError("expected '{' for discriminated enum")
    val firstKey = in.readKeyAsString()
    if (firstKey != discriminatorField)
      in.decodeError("expected discriminator field '" + discriminatorField + "' as first field, got '" + firstKey + "'")
    val typeName = in.readString(null)
    // Now we need to read the rest of the object as the body
    // The dispatch function will continue reading from the same reader
    dispatch(typeName)
  }

  /** Write a map with custom key encoding. */
  def writeMapWithKeyEncoder[K, V](
      out: JsonWriter,
      entries: Iterable[(K, V)],
      encodeKey: (K, JsonWriter) => Unit,
      encodeValue: V => Unit
  ): Unit = {
    out.writeObjectStart()
    val iter = entries.iterator
    while (iter.hasNext) {
      val (key, value) = iter.next()
      encodeKey(key, out)
      encodeValue(value)
    }
    out.writeObjectEnd()
  }

  /** Read a map with custom key decoding. */
  def readMapWithKeyDecoder[K, V, M](
      in: JsonReader,
      decodeKey: JsonReader => K,
      decodeValue: JsonReader => V,
      factory: scala.collection.Factory[(K, V), M],
      maxInserts: Int
  ): M = {
    val builder = factory.newBuilder
    if (!in.isNextToken('{'.toByte)) in.decodeError("expected '{' or null")
    if (!in.isNextToken('}'.toByte)) {
      in.rollbackToken()
      var count = 1
      val key = decodeKey(in)
      builder += ((key, decodeValue(in)))
      while (in.isNextToken(','.toByte)) {
        count += 1
        if (count > maxInserts) in.decodeError(s"too many map entries (max: $maxInserts)")
        val k = decodeKey(in)
        builder += ((k, decodeValue(in)))
      }
      if (!in.isCurrentToken('}'.toByte)) in.decodeError("expected '}' or ','")
    }
    builder.result()
  }

  // --- @stringified helpers ---

  def writeStringifiedInt(out: JsonWriter, value: Int): Unit = out.writeVal(value.toString)
  def writeStringifiedLong(out: JsonWriter, value: Long): Unit = out.writeVal(value.toString)
  def writeStringifiedDouble(out: JsonWriter, value: Double): Unit = out.writeVal(value.toString)
  def writeStringifiedFloat(out: JsonWriter, value: Float): Unit = out.writeVal(value.toString)
  def writeStringifiedShort(out: JsonWriter, value: Short): Unit = out.writeVal(value.toString)
  def writeStringifiedByte(out: JsonWriter, value: Byte): Unit = out.writeVal(value.toString)
  def writeStringifiedBigDecimal(out: JsonWriter, value: BigDecimal): Unit = out.writeVal(value.toString)
  def writeStringifiedBigInt(out: JsonWriter, value: BigInt): Unit = out.writeVal(value.toString)

  def readStringifiedInt(in: JsonReader): Int = in.readString(null).toInt
  def readStringifiedLong(in: JsonReader): Long = in.readString(null).toLong
  def readStringifiedDouble(in: JsonReader): Double = in.readString(null).toDouble
  def readStringifiedFloat(in: JsonReader): Float = in.readString(null).toFloat
  def readStringifiedShort(in: JsonReader): Short = in.readString(null).toShort
  def readStringifiedByte(in: JsonReader): Byte = in.readString(null).toByte
  def readStringifiedBigDecimal(in: JsonReader): BigDecimal = BigDecimal(in.readString(null))
  def readStringifiedBigInt(in: JsonReader): BigInt = BigInt(in.readString(null))

  // --- mapAsArray helpers ---

  def writeMapAsArray[K, V](
      out: JsonWriter,
      entries: Iterable[(K, V)],
      encodeKey: K => Unit,
      encodeValue: V => Unit
  ): Unit = {
    out.writeArrayStart()
    val iter = entries.iterator
    while (iter.hasNext) {
      val (key, value) = iter.next()
      out.writeArrayStart()
      encodeKey(key)
      encodeValue(value)
      out.writeArrayEnd()
    }
    out.writeArrayEnd()
  }

  def readMapAsArray[K, V, M](
      in: JsonReader,
      decodeKey: JsonReader => K,
      decodeValue: JsonReader => V,
      factory: scala.collection.Factory[(K, V), M],
      maxInserts: Int
  ): M = {
    val builder = factory.newBuilder
    if (!in.isNextToken('['.toByte)) in.decodeError("expected '[' for map-as-array")
    if (!in.isNextToken(']'.toByte)) {
      in.rollbackToken()
      var count = 1
      if (!in.isNextToken('['.toByte)) in.decodeError("expected '[' for pair")
      val k = decodeKey(in)
      if (!in.isNextToken(','.toByte)) in.decodeError("expected ',' between key and value in pair")
      val v = decodeValue(in)
      if (!in.isNextToken(']'.toByte)) in.decodeError("expected ']' after pair")
      builder += ((k, v))
      while (in.isNextToken(','.toByte)) {
        count += 1
        if (count > maxInserts) in.decodeError(s"too many map entries (max: $maxInserts)")
        if (!in.isNextToken('['.toByte)) in.decodeError("expected '[' for pair")
        val k = decodeKey(in)
        if (!in.isNextToken(','.toByte)) in.decodeError("expected ',' between key and value in pair")
        val v = decodeValue(in)
        if (!in.isNextToken(']'.toByte)) in.decodeError("expected ']' after pair")
        builder += ((k, v))
      }
      if (!in.isCurrentToken(']'.toByte)) in.decodeError("expected ']' or ','")
    }
    builder.result()
  }

  /** Cast an `Any` value to `A`, using a decode function purely for type inference. */
  @scala.annotation.nowarn("msg=unused explicit parameter")
  def unsafeCast[A](value: Any, decodeFn: JsonReader => A): A = value.asInstanceOf[A]

  // --- Validation helpers ---

  def throwMissingField(fieldName: String): Nothing =
    throw new IllegalArgumentException(s"missing required field: $fieldName")

  def throwDuplicateField(in: JsonReader, fieldName: String): Nothing =
    in.decodeError(s"duplicate field: $fieldName")

  // --- BigDecimal / BigInt validation ---

  // --- java.time helpers ---

  def readInstant(in: JsonReader): java.time.Instant =
    try java.time.Instant.parse(in.readString(null))
    catch { case e: java.time.format.DateTimeParseException => in.decodeError(e.getMessage) }

  def readLocalDate(in: JsonReader): java.time.LocalDate =
    try java.time.LocalDate.parse(in.readString(null))
    catch { case e: java.time.format.DateTimeParseException => in.decodeError(e.getMessage) }

  def readLocalTime(in: JsonReader): java.time.LocalTime =
    try java.time.LocalTime.parse(in.readString(null))
    catch { case e: java.time.format.DateTimeParseException => in.decodeError(e.getMessage) }

  def readLocalDateTime(in: JsonReader): java.time.LocalDateTime =
    try java.time.LocalDateTime.parse(in.readString(null))
    catch { case e: java.time.format.DateTimeParseException => in.decodeError(e.getMessage) }

  def readOffsetDateTime(in: JsonReader): java.time.OffsetDateTime =
    try java.time.OffsetDateTime.parse(in.readString(null))
    catch { case e: java.time.format.DateTimeParseException => in.decodeError(e.getMessage) }

  def readZonedDateTime(in: JsonReader): java.time.ZonedDateTime =
    try java.time.ZonedDateTime.parse(in.readString(null))
    catch { case e: java.time.format.DateTimeParseException => in.decodeError(e.getMessage) }

  def readDuration(in: JsonReader): java.time.Duration =
    try java.time.Duration.parse(in.readString(null))
    catch { case e: java.time.format.DateTimeParseException => in.decodeError(e.getMessage) }

  def readPeriod(in: JsonReader): java.time.Period =
    try java.time.Period.parse(in.readString(null))
    catch { case e: java.time.format.DateTimeParseException => in.decodeError(e.getMessage) }

  // --- BigDecimal / BigInt validation ---

  def validateBigDecimal(
      in: JsonReader,
      value: BigDecimal,
      precision: Int,
      scaleLimit: Int,
      digitsLimit: Int
  ): BigDecimal = {
    if (value ne null) {
      val u = value.underlying()
      if (u.precision > precision)
        in.decodeError(s"BigDecimal precision ${u.precision} exceeds limit $precision")
      if (u.scale > scaleLimit || u.scale < -scaleLimit)
        in.decodeError(s"BigDecimal scale ${u.scale} exceeds limit $scaleLimit")
      if (u.unscaledValue.bitLength * 30103 / 100000 + 1 > digitsLimit) // log10(2) ≈ 0.30103
        in.decodeError(s"BigDecimal digits exceed limit $digitsLimit")
    }
    value
  }

  def validateBigInt(in: JsonReader, value: BigInt, digitsLimit: Int): BigInt = {
    if (value ne null) {
      if (value.underlying.bitLength * 30103 / 100000 + 1 > digitsLimit) // log10(2) ≈ 0.30103
        in.decodeError(s"BigInt digits exceed limit $digitsLimit")
    }
    value
  }
}
