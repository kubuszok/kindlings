package hearth.kindlings.ubjsonderivation.internal.runtime

import hearth.kindlings.ubjsonderivation.{UBJsonConfig, UBJsonReader, UBJsonValueCodec, UBJsonWriter}

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

  /** `readOption` for `Option[Option[_]]` with `skipNestedOptionValues`: `null` decodes as `Some(None)` (an absent
    * field still decodes as `None`), as in jsoniter-scala.
    */
  def readNestedOption[A](in: UBJsonReader, skipNestedOptionValues: Boolean)(
      decodeInner: UBJsonReader => A
  ): Option[A] =
    if (in.isNextNull()) { if (skipNestedOptionValues) Some(None.asInstanceOf[A]) else None }
    else Some(decodeInner(in))

  /** An `Option` nested directly in another `Option` (without `skipNestedOptionValues`) is encoded as jsoniter-scala
    * encodes it: as the ADT `Option` (leaves `scala.Some` with a `value` field and `scala.None`), using the config's
    * ADT encoding: `{"type":"Some","value":1}` / `{"type":"None"}` with a discriminator, `{"Some":{"value":1}}` /
    * `"None"` without (`{"None":{}}` with `circeLikeObjectEncoding`). So `Some(None)` survives a round-trip.
    */
  def writeNestedOptionAsAdt[A](out: UBJsonWriter, x: Option[A], config: UBJsonConfig)(encodeValue: A => Unit): Unit = {
    val valueKey = config.fieldNameMapper("value")
    if ((x ne null) && x.isDefined) {
      val someName = config.adtLeafClassNameMapper("scala.Some")
      config.discriminatorFieldName match {
        case Some(discriminator) =>
          out.writeObjectStart()
          out.writeFieldName(discriminator)
          out.writeString(someName)
          out.writeFieldName(valueKey)
          encodeValue(x.get)
          out.writeObjectEnd()
        case None =>
          out.writeObjectStart()
          out.writeFieldName(someName)
          out.writeObjectStart()
          out.writeFieldName(valueKey)
          encodeValue(x.get)
          out.writeObjectEnd()
          out.writeObjectEnd()
      }
    } else {
      val noneName = config.adtLeafClassNameMapper("scala.None")
      config.discriminatorFieldName match {
        case Some(discriminator) =>
          out.writeObjectStart()
          out.writeFieldName(discriminator)
          out.writeString(noneName)
          out.writeObjectEnd()
        case None =>
          if (config.circeLikeObjectEncoding) {
            out.writeObjectStart()
            out.writeFieldName(noneName)
            out.writeObjectStart()
            out.writeObjectEnd()
            out.writeObjectEnd()
          } else out.writeString(noneName)
      }
    }
  }

  /** Reads what [[writeNestedOptionAsAdt]] writes (with a discriminator, it may be anywhere in the object). */
  def readNestedOptionAsAdt[A](in: UBJsonReader, config: UBJsonConfig)(decodeValue: UBJsonReader => A): Option[A] = {
    val someName = config.adtLeafClassNameMapper("scala.Some")
    val noneName = config.adtLeafClassNameMapper("scala.None")
    val valueKey = config.fieldNameMapper("value")
    // reads the rest of an opened object: the `value` field and, with a discriminator, the discriminator
    def readFields(discriminator: String): (String, Option[A]) = {
      var typeName: String = null
      var value: Option[A] = None
      while (!in.isObjectEnd()) {
        val key = in.readFieldName()
        if ((discriminator ne null) && key == discriminator) {
          if ((typeName ne null) && config.checkFieldDuplication) throwDuplicateField(in, key)
          typeName = in.readString()
        } else if (key == valueKey) {
          if (value.isDefined && config.checkFieldDuplication) throwDuplicateField(in, key)
          value = Some(decodeValue(in))
        } else if (config.skipUnexpectedFields) in.skip()
        else in.decodeError("unexpected field \"" + key + "\"")
      }
      (typeName, value)
    }
    def some(value: Option[A]): Option[A] =
      if (value.isEmpty) in.decodeError("missing required field \"" + valueKey + "\"") else value
    config.discriminatorFieldName match {
      case Some(discriminator) =>
        in.readObjectStart()
        val (typeName, value) = readFields(discriminator)
        if (typeName == someName) some(value)
        else if (typeName == noneName) None
        else if (typeName eq null) in.decodeError("missing discriminator field '" + discriminator + "'")
        else in.decodeError("illegal value of discriminator field \"" + discriminator + "\"")
      case None =>
        if (in.peekToken() == 'S'.toByte) {
          val name = in.readString()
          if (name == noneName) None else in.decodeError("illegal discriminator")
        } else {
          in.readObjectStart()
          val name = in.readFieldName()
          val result =
            if (name == someName) {
              in.readObjectStart()
              some(readFields(null)._2)
            } else if (name == noneName) {
              readObjectWithoutFields(in, config.skipUnexpectedFields, alreadyOpened = false)
              None
            } else in.decodeError("illegal discriminator")
          if (!in.isObjectEnd()) in.decodeError("expected '}' after wrapped enum value")
          result
        }
    }
  }

  def writeScalaEnumValueId(out: UBJsonWriter, value: Any): Unit =
    out.writeInt(value.asInstanceOf[scala.Enumeration#Value].id)

  def readScalaEnumValueId[A](in: UBJsonReader)(dispatch: Int => A): A = dispatch(in.readInt())

  def scalaEnumValueId(value: Any): Int = value.asInstanceOf[scala.Enumeration#Value].id

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

  /** Reads an object of a type without (non-transient) fields: unknown keys are skipped when `skipUnexpected`, rejected
    * otherwise. When `alreadyOpened`, the object start (and a discriminator) was consumed already.
    */
  def readObjectWithoutFields(in: UBJsonReader, skipUnexpected: Boolean, alreadyOpened: Boolean): Unit = {
    val skipKey = if (alreadyOpened) in.takePendingSkipKey() else null
    if (!alreadyOpened) in.readObjectStart()
    while (!in.isObjectEnd()) {
      val key = in.readFieldName()
      if (skipUnexpected || key == skipKey) in.skip()
      else in.decodeError("unexpected field \"" + key + "\"")
    }
  }

  /** `==` for values, contents comparison for arrays (`transientDefault` of array-typed fields). */
  def valueEquals(a: Any, b: Any): Boolean = (a, b) match {
    case (x: Array[?], y: Array[?]) => java.util.Objects.deepEquals(x, y)
    case _                          => a == b
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
    val arr = newFieldArray(fieldCount)
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
    val skipKey = in.takePendingSkipKey()
    val arr = newFieldArray(fieldCount)
    while (!in.isObjectEnd()) {
      val key = in.readFieldName()
      if (key == skipKey) in.skip() else dispatch(key, arr, in)
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

  /** As jsoniter-scala with `requireDiscriminatorFirst = false`: finds the discriminator anywhere in the object, then
    * returns to the object start, so that the leaf decoder reads all its fields (skipping the discriminator).
    */
  def readWithDiscriminatorAnywhere[A](in: UBJsonReader, discriminatorField: String)(dispatch: String => A): A = {
    in.readObjectStart()
    in.setMark()
    var typeName: String = null
    while (typeName == null && !in.isObjectEnd()) {
      val key = in.readFieldName()
      if (key == discriminatorField) typeName = in.readString()
      else in.skip()
    }
    if (typeName == null) in.decodeError("missing discriminator field '" + discriminatorField + "'")
    in.rollbackToMark()
    in.pendingSkipKey = discriminatorField
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

  /** Marks slots of [[readObject]]/[[readObjectInline]] arrays whose fields were absent from the object (`null` cannot
    * be used, because it is a legal decoded value).
    */
  private object AbsentField

  @inline def isAbsentField(value: Any): Boolean = value.asInstanceOf[AnyRef] eq AbsentField

  /** A `null` for a reference-typed field with a default value decodes as the default (as in jsoniter-scala). */
  @inline def readNullAsDefault[A](in: UBJsonReader, default: => A)(decode: UBJsonReader => A): A =
    if (in.isNextNull()) default else decode(in)

  /** A decoded collection/map field that came out empty is the field's default, as in jsoniter-scala (whose collection
    * decoders return their `default` argument for an empty array/object).
    */
  def emptyAsDefault[A](value: A, default: => A): A = (value: Any) match {
    case it: Iterable[?]             => if (it.isEmpty) default else value
    case arr: Array[?]               => if (arr.length == 0) default else value
    case it: java.util.Collection[?] => if (it.isEmpty) default else value
    case m: java.util.Map[?, ?]      => if (m.isEmpty) default else value
    case _                           => value
  }

  /** For a `scala.transient` field with a runtime config: skips the value (keeping the slot absent, so it gets the
    * default) when `scalaTransientSupport` is on.
    */
  def readUnlessTransient(in: UBJsonReader, transient: Boolean, decode: UBJsonReader => Any): Any =
    if (transient) {
      in.skip()
      AbsentField
    } else decode(in)

  /** The keys of a case class decoder whose field names are only known at runtime (a config the macro could not
    * evaluate), built once per codec instance: a key is looked up by hash instead of being compared with every field's
    * name (each computed with `fieldNameMapper`) on every key of every decode.
    *
    * @param names
    *   the (mapped) key of each field, in the order of the decoder's field array
    */
  final class FieldIndex(names: Array[String]) {
    // a few names are compared one by one (cheaper than hashing the key), more are looked up by hash
    private val indices: java.util.HashMap[String, Integer] =
      if (names.length <= FieldIndex.MaxNamesCompared) null
      else {
        val map = new java.util.HashMap[String, Integer](names.length * 2)
        var i = names.length - 1
        while (i >= 0) { // the first field wins when names collide, as with a chain of comparisons
          map.put(names(i), Integer.valueOf(i))
          i -= 1
        }
        map
      }

    /** The index of the field with this key, `-1` for an unknown key. */
    def indexOf(key: String): Int =
      if (indices eq null) {
        var i = 0
        while (i < names.length && names(i) != key) i += 1
        if (i < names.length) i else -1
      } else {
        val i = indices.get(key)
        if (i eq null) -1 else i.intValue
      }

    /** The key of the field at index `i`. */
    def name(i: Int): String = names(i)
  }

  object FieldIndex {
    private val MaxNamesCompared = 8
  }

  def fieldIndex(names: Array[String]): FieldIndex = new FieldIndex(names)

  /** A field array of [[readObject]]'s shape (every slot absent), for decoders reading the object themselves. */
  def absentFields(fieldCount: Int): Array[Any] = newFieldArray(fieldCount)

  /** Fails with `unexpected field: <name>` (a key no field has, with `skipUnexpectedFields = false`). */
  def unexpectedField(in: UBJsonReader, fieldName: String): Nothing =
    in.decodeError("unexpected field: " + fieldName)

  private def newFieldArray(fieldCount: Int): Array[Any] = {
    val arr = new Array[Any](fieldCount)
    java.util.Arrays.fill(arr.asInstanceOf[Array[AnyRef]], AbsentField)
    arr
  }

  /** Fails with `missing required field "<name>"`. */
  def missingRequiredField[A](in: UBJsonReader, fieldName: String): A =
    in.decodeError("missing required field \"" + fieldName + "\"")

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
