package hearth.kindlings.ubjsonderivation.internal.runtime

import hearth.kindlings.ubjsonderivation.{UBJsonReader, UBJsonValueCodec, UBJsonWriter}

object UBJsonDerivationUtils {

  // --- Encoder helpers ---

  def writeArray[A](out: UBJsonWriter, items: Iterable[A], encodeItem: A => Unit): Unit = {
    out.writeArrayStart()
    val iter = items.iterator
    while (iter.hasNext) encodeItem(iter.next())
    out.writeArrayEnd()
  }

  def writeMapStringKeyed[V](
      out: UBJsonWriter,
      entries: Iterable[(String, V)],
      encodeValue: V => Unit
  ): Unit = {
    out.writeObjectStart()
    val iter = entries.iterator
    while (iter.hasNext) {
      val (key, value) = iter.next()
      out.writeFieldName(key)
      encodeValue(value)
    }
    out.writeObjectEnd()
  }

  def writeMapWithKeyEncoder[K, V](
      out: UBJsonWriter,
      entries: Iterable[(K, V)],
      encodeKey: K => String,
      encodeValue: V => Unit
  ): Unit = {
    out.writeObjectStart()
    val iter = entries.iterator
    while (iter.hasNext) {
      val (key, value) = iter.next()
      out.writeFieldName(encodeKey(key))
      encodeValue(value)
    }
    out.writeObjectEnd()
  }

  def writeEnumAsString(out: UBJsonWriter, typeName: String): Unit =
    out.writeString(typeName)

  def writeWrapped(out: UBJsonWriter, typeName: String)(encodeInner: => Unit): Unit = {
    out.writeObjectStart()
    out.writeFieldName(typeName)
    encodeInner
    out.writeObjectEnd()
  }

  // --- Decoder helpers ---

  def readOption[A](in: UBJsonReader)(decodeInner: UBJsonReader => A): Option[A] =
    if (in.isNextNull()) None
    else Some(decodeInner(in))

  def readCollection[Item, Coll](
      in: UBJsonReader,
      decodeItem: UBJsonReader => Item,
      factory: scala.collection.Factory[Item, Coll],
      maxInserts: Int
  ): Coll = {
    val builder = factory.newBuilder
    in.readArrayStart()
    var count = 0
    while (!in.isArrayEnd()) {
      count += 1
      if (count > maxInserts) in.decodeError(s"too many collection items (max: $maxInserts)")
      builder += decodeItem(in)
    }
    builder.result()
  }

  def readMap[V, M](
      in: UBJsonReader,
      decodeValue: UBJsonReader => V,
      factory: scala.collection.Factory[(String, V), M],
      maxInserts: Int
  ): M = {
    val builder = factory.newBuilder
    in.readObjectStart()
    var count = 0
    while (!in.isObjectEnd()) {
      count += 1
      if (count > maxInserts) in.decodeError(s"too many map entries (max: $maxInserts)")
      val key = in.readFieldName()
      builder += ((key, decodeValue(in)))
    }
    builder.result()
  }

  def readMapWithKeyDecoder[K, V, M](
      in: UBJsonReader,
      decodeKey: String => K,
      decodeValue: UBJsonReader => V,
      factory: scala.collection.Factory[(K, V), M],
      maxInserts: Int
  ): M = {
    val builder = factory.newBuilder
    in.readObjectStart()
    var count = 0
    while (!in.isObjectEnd()) {
      count += 1
      if (count > maxInserts) in.decodeError(s"too many map entries (max: $maxInserts)")
      val keyStr = in.readFieldName()
      val key = decodeKey(keyStr)
      builder += ((key, decodeValue(in)))
    }
    builder.result()
  }

  def readEmptyObject(in: UBJsonReader): Unit = {
    in.readObjectStart()
    if (!in.isObjectEnd()) in.decodeError("expected empty object '}'")
  }

  def readObject[A](
      in: UBJsonReader,
      fieldCount: Int,
      construct: Array[Any] => A
  )(dispatch: (String, Array[Any], UBJsonReader) => Unit): A = {
    val arr = new Array[Any](fieldCount)
    in.readObjectStart()
    while (!in.isObjectEnd()) {
      val key = in.readFieldName()
      dispatch(key, arr, in)
    }
    construct(arr)
  }

  /** Read remaining fields of an already-opened object (e.g., after discriminator field was consumed). */
  def readObjectInline[A](
      in: UBJsonReader,
      fieldCount: Int,
      construct: Array[Any] => A
  )(dispatch: (String, Array[Any], UBJsonReader) => Unit): A = {
    val arr = new Array[Any](fieldCount)
    while (!in.isObjectEnd()) {
      val key = in.readFieldName()
      dispatch(key, arr, in)
    }
    construct(arr)
  }

  def readWrapped[A](in: UBJsonReader)(dispatch: String => A): A = {
    in.readObjectStart()
    val typeName = in.readFieldName()
    val result = dispatch(typeName)
    if (!in.isObjectEnd()) in.decodeError("expected '}' after wrapped enum value")
    result
  }

  def readWithDiscriminator[A](in: UBJsonReader, discriminatorField: String)(dispatch: String => A): A = {
    in.readObjectStart()
    val firstKey = in.readFieldName()
    if (firstKey != discriminatorField)
      in.decodeError("expected discriminator field '" + discriminatorField + "' as first field, got '" + firstKey + "'")
    val typeName = in.readString()
    dispatch(typeName)
  }

  def readEnumAsString[A](in: UBJsonReader)(dispatch: String => A): A = {
    val typeName = in.readString()
    dispatch(typeName)
  }

  /** Cast an `Any` value to `A`, using a decode function purely for type inference. */
  @scala.annotation.nowarn("msg=unused explicit parameter")
  def unsafeCast[A](value: Any, decodeFn: UBJsonReader => A): A = value.asInstanceOf[A]

  // --- Validation helpers ---

  def throwMissingField(fieldName: String): Nothing =
    throw new IllegalArgumentException(s"missing required field: $fieldName")

  def throwDuplicateField(in: UBJsonReader, fieldName: String): Nothing =
    in.decodeError(s"duplicate field: $fieldName")

  // --- BigDecimal / BigInt validation ---

  def validateBigDecimal(
      in: UBJsonReader,
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
      if (u.unscaledValue.bitLength * 30103 / 100000 + 1 > digitsLimit) // log10(2) ~ 0.30103
        in.decodeError(s"BigDecimal digits exceed limit $digitsLimit")
    }
    value
  }

  def validateBigInt(in: UBJsonReader, value: BigInt, digitsLimit: Int): BigInt = {
    if (value ne null) {
      if (value.underlying.bitLength * 30103 / 100000 + 1 > digitsLimit)
        in.decodeError(s"BigInt digits exceed limit $digitsLimit")
    }
    value
  }

  // --- Convenience methods for encoding/decoding ---

  def writeToBytes[A](value: A)(codec: UBJsonValueCodec[A]): Array[Byte] = {
    val writer = new UBJsonWriter()
    codec.encode(writer, value)
    writer.toByteArray
  }

  def readFromBytes[A](bytes: Array[Byte])(codec: UBJsonValueCodec[A]): A =
    codec.decode(new UBJsonReader(bytes))
}
