package hearth.kindlings.jsoniterderivation.internal.runtime

import com.github.plokhotnyuk.jsoniter_scala.core.{JsonReader, JsonWriter}

object JsoniterDerivationUtils {

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
      factory: scala.collection.Factory[Item, Coll]
  ): Coll = {
    val builder = factory.newBuilder
    if (!in.isNextToken('['.toByte)) in.decodeError("expected '[' or null")
    if (!in.isNextToken(']'.toByte)) {
      in.rollbackToken()
      builder += decodeItem(in)
      while (in.isNextToken(','.toByte)) builder += decodeItem(in)
      if (!in.isCurrentToken(']'.toByte)) in.decodeError("expected ']' or ','")
    }
    builder.result()
  }

  def readMap[V, M](
      in: JsonReader,
      decodeValue: JsonReader => V,
      factory: scala.collection.Factory[(String, V), M]
  ): M = {
    val builder = factory.newBuilder
    if (!in.isNextToken('{'.toByte)) in.decodeError("expected '{' or null")
    if (!in.isNextToken('}'.toByte)) {
      in.rollbackToken()
      val key = in.readKeyAsString()
      builder += ((key, decodeValue(in)))
      while (in.isNextToken(','.toByte)) {
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

  /** Cast an `Any` value to `A`, using a decode function purely for type inference. */
  @scala.annotation.nowarn("msg=unused explicit parameter")
  def unsafeCast[A](value: Any, decodeFn: JsonReader => A): A = value.asInstanceOf[A]
}
