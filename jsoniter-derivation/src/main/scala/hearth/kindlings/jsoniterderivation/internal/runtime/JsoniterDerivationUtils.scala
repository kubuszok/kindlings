package hearth.kindlings.jsoniterderivation.internal.runtime

import com.github.plokhotnyuk.jsoniter_scala.core.{JsonKeyCodec, JsonReader, JsonValueCodec, JsonWriter}
import hearth.kindlings.jsoniterderivation.{JsoniterConfig, KindlingsJsonCodec}

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

  def writeScalaEnumValueId(out: JsonWriter, value: Any, asString: Boolean): Unit =
    if (asString) out.writeValAsString(value.asInstanceOf[scala.Enumeration#Value].id)
    else out.writeVal(value.asInstanceOf[scala.Enumeration#Value].id)

  def readEnumAsString[A](in: JsonReader)(dispatch: String => A): A = {
    val typeName = in.readString(null)
    dispatch(typeName)
  }

  def readScalaEnumValueId[A](in: JsonReader, asString: Boolean)(dispatch: Int => A): A =
    dispatch(if (asString) in.readStringAsInt() else in.readInt())

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

  /** Top-level decoding: as jsoniter-scala, a JSON `null` decodes as the `default` argument of `decodeValue` when it is
    * not `null` (codecs' `nullValue`s are `None` / empty collections / zeros); otherwise the decoder handles `null`.
    */
  def readNullAsDefaultArgument[A](in: JsonReader, default: A, applies: Boolean)(decode: JsonReader => A): A =
    if (!applies || (default.asInstanceOf[AnyRef] eq null)) decode(in)
    else if (in.isNextToken('n'.toByte)) in.readNullOrError[A](default, "expected null")
    else {
      in.rollbackToken()
      decode(in)
    }

  def readOption[A](in: JsonReader)(decodeInner: JsonReader => A): Option[A] =
    if (in.isNextToken('n'.toByte)) {
      in.readNullOrError[Option[A]](None, "expected null")
    } else {
      in.rollbackToken()
      Some(decodeInner(in))
    }

  /** `readOption` for `Option[Option[_]]` with `skipNestedOptionValues`: a JSON `null` decodes as `Some(None)` (an
    * absent field still decodes as `None`), as in jsoniter-scala.
    */
  def readNestedOption[A](in: JsonReader, skipNestedOptionValues: Boolean)(decodeInner: JsonReader => A): Option[A] =
    if (in.isNextToken('n'.toByte)) {
      in.readNullOrError[Option[A]](if (skipNestedOptionValues) Some(None.asInstanceOf[A]) else None, "expected null")
    } else {
      in.rollbackToken()
      Some(decodeInner(in))
    }

  /** An `Option` nested directly in another `Option` (without `skipNestedOptionValues`) is encoded as jsoniter-scala
    * encodes it: as the ADT `Option` (leaves `scala.Some` with a `value` field and `scala.None`), using the config's
    * ADT encoding: `{"type":"Some","value":1}` / `{"type":"None"}` with a discriminator, `{"Some":{"value":1}}` /
    * `"None"` without (`{"None":{}}` with `circeLikeObjectEncoding`). So `Some(None)` survives a round-trip.
    */
  def writeNestedOptionAsAdt[A](out: JsonWriter, x: Option[A], config: JsoniterConfig)(encodeValue: A => Unit): Unit = {
    val valueKey = config.fieldNameMapper("value")
    if ((x ne null) && x.isDefined) {
      val someName = config.adtLeafClassNameMapper("scala.Some")
      config.discriminatorFieldName match {
        case Some(discriminator) =>
          out.writeObjectStart()
          out.writeKey(discriminator)
          out.writeVal(someName)
          out.writeKey(valueKey)
          encodeValue(x.get)
          out.writeObjectEnd()
        case None =>
          out.writeObjectStart()
          out.writeKey(someName)
          out.writeObjectStart()
          out.writeKey(valueKey)
          encodeValue(x.get)
          out.writeObjectEnd()
          out.writeObjectEnd()
      }
    } else {
      val noneName = config.adtLeafClassNameMapper("scala.None")
      config.discriminatorFieldName match {
        case Some(discriminator) =>
          out.writeObjectStart()
          out.writeKey(discriminator)
          out.writeVal(noneName)
          out.writeObjectEnd()
        case None =>
          if (config.circeLikeObjectEncoding) {
            out.writeObjectStart()
            out.writeKey(noneName)
            out.writeObjectStart()
            out.writeObjectEnd()
            out.writeObjectEnd()
          } else out.writeVal(noneName)
      }
    }
  }

  /** Reads what [[writeNestedOptionAsAdt]] writes. */
  def readNestedOptionAsAdt[A](in: JsonReader, config: JsoniterConfig)(decodeValue: JsonReader => A): Option[A] = {
    val someName = config.adtLeafClassNameMapper("scala.Some")
    val noneName = config.adtLeafClassNameMapper("scala.None")
    val valueKey = config.fieldNameMapper("value")
    config.discriminatorFieldName match {
      case Some(discriminator) =>
        readWithDiscriminator[Option[A]](in, discriminator, config.requireDiscriminatorFirst) { (typeName, afterOpen) =>
          if (typeName == someName)
            Some(readValueField(in, valueKey, discriminator, afterOpen, config)(decodeValue))
          else if (typeName == noneName) {
            readInlineObjectWithoutFields(in, true, null, false, afterOpen)
            None
          } else unknownDiscriminator[Option[A]](in, discriminator)
        }
      case None =>
        if (in.isNextToken('"'.toByte)) {
          in.rollbackToken()
          val name = in.readString(null)
          if (name == noneName) None else in.discriminatorError()
        } else {
          in.rollbackToken()
          readWrapped[Option[A]](in) { name =>
            if (name == someName) {
              if (!in.isNextToken('{'.toByte)) in.decodeError("expected '{'")
              Some(readValueField(in, valueKey, null, true, config)(decodeValue))
            } else if (name == noneName) {
              readEmptyObject(in)
              None
            } else in.discriminatorError()
          }
        }
    }
  }

  /** Reads the rest of an object with the single `valueKey` field (see `readObjectInline` for `afterOpen`). */
  private def readValueField[A](
      in: JsonReader,
      valueKey: String,
      discriminator: String,
      afterOpen: Boolean,
      config: JsoniterConfig
  )(decodeValue: JsonReader => A): A = {
    var value: A = null.asInstanceOf[A]
    var present = false
    val hasKeys =
      if (afterOpen) {
        if (in.isNextToken('}'.toByte)) false
        else {
          in.rollbackToken()
          true
        }
      } else if (in.isNextToken(','.toByte)) true
      else {
        if (!in.isCurrentToken('}'.toByte)) in.objectEndOrCommaError()
        false
      }
    if (hasKeys) {
      var discriminatorSeen = !afterOpen || (discriminator eq null)
      var l = -1
      while (l < 0 || in.isNextToken(','.toByte)) {
        l = in.readKeyAsCharBuf()
        if (in.isCharBufEqualsTo(l, valueKey)) {
          if (present && config.checkFieldDuplication) in.duplicatedKeyError(l)
          present = true
          value = decodeValue(in)
        } else if ((discriminator ne null) && in.isCharBufEqualsTo(l, discriminator)) {
          if (discriminatorSeen && config.checkFieldDuplication) in.duplicatedKeyError(l)
          discriminatorSeen = true
          in.skip()
        } else if (config.skipUnexpectedFields) in.skip()
        else in.unexpectedKeyError(l)
      }
      if (!in.isCurrentToken('}'.toByte)) in.objectEndOrCommaError()
    }
    if (!present) in.requiredFieldError(valueKey)
    value
  }

  def readCollection[Item, Coll](
      in: JsonReader,
      decodeItem: JsonReader => Item,
      factory: scala.collection.Factory[Item, Coll],
      maxInserts: Int
  ): Coll = {
    val builder = factory.newBuilder
    if (openArray(in, nullAsEmpty = true) && !in.isNextToken(']'.toByte)) {
      in.rollbackToken()
      var count = 1
      builder += decodeItem(in)
      while (in.isNextToken(','.toByte)) {
        count += 1
        if (count > maxInserts) in.decodeError("too many set inserts")
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
    if (openObject(in, nullAsEmpty = true) && !in.isNextToken('}'.toByte)) {
      in.rollbackToken()
      var count = 1
      val key = in.readKeyAsString()
      builder += ((key, decodeValue(in)))
      while (in.isNextToken(','.toByte)) {
        count += 1
        if (count > maxInserts) in.decodeError("too many map inserts")
        val k = in.readKeyAsString()
        builder += ((k, decodeValue(in)))
      }
      if (!in.isCurrentToken('}'.toByte)) in.decodeError("expected '}' or ','")
    }
    builder.result()
  }

  /** Reads a JSON object for a type without (non-transient) fields. Every key is skipped when `skipUnexpected`,
    * otherwise only `allowedKey` (a discriminator emitted by `alwaysEmitDiscriminator`, or `null`) is, and other keys
    * fail with jsoniter-scala's `unexpected field` error. When `alreadyOpened`, the `{` and the discriminator of a
    * sealed trait were consumed already, and only the rest of the object is read.
    */
  def readObjectWithoutFields(
      in: JsonReader,
      skipUnexpected: Boolean,
      allowedKey: String,
      alreadyOpened: Boolean
  ): Unit = {
    val hasKeys =
      if (alreadyOpened) {
        if (in.isNextToken(','.toByte)) true
        else {
          if (!in.isCurrentToken('}'.toByte)) in.objectEndOrCommaError()
          false
        }
      } else {
        if (!in.isNextToken('{'.toByte)) in.decodeError("expected '{'")
        if (in.isNextToken('}'.toByte)) false
        else {
          in.rollbackToken()
          true
        }
      }
    if (hasKeys) {
      var l = -1
      while (l < 0 || in.isNextToken(','.toByte)) {
        l = in.readKeyAsCharBuf()
        if (skipUnexpected || ((allowedKey ne null) && in.isCharBufEqualsTo(l, allowedKey))) in.skip()
        else in.unexpectedKeyError(l)
      }
      if (!in.isCurrentToken('}'.toByte)) in.objectEndOrCommaError()
    }
  }

  /** Reads an object of a type without fields, skipping any keys (jsoniter-scala skips the content of objects decoded
    * as case objects).
    */
  def readEmptyObject(in: JsonReader): Unit =
    readObjectWithoutFields(in, skipUnexpected = true, null, alreadyOpened = false)

  /** `[key, value]` pairs of `mapAsArray`: `[` before the key, `,` between, `]` after the value. */
  def openPair(in: JsonReader): Unit = if (!in.isNextToken('['.toByte)) in.decodeError("expected '['")
  def pairSeparator(in: JsonReader): Unit =
    if (!in.isNextToken(','.toByte)) in.commaError()
  def closePair(in: JsonReader): Unit = if (!in.isNextToken(']'.toByte)) in.arrayEndError()

  /** Reads `{}` or `null` as `Unit` (upstream jsoniter-scala has no `Unit` codec; this one is balanced). */
  def readUnit(in: JsonReader): Unit =
    if (in.isNextToken('n'.toByte)) in.readNullOrError((), "expected '{' or null")
    else {
      in.rollbackToken()
      if (!in.isNextToken('{'.toByte) || !in.isNextToken('}'.toByte)) in.decodeError("expected '{}' or null")
    }

  /** Opens a JSON array; when `nullAsEmpty`, a JSON `null` is consumed instead and `false` is returned (the collection
    * is empty), as jsoniter-scala does for collections (`readNullOrTokenError(default, '[')`).
    */
  def openArray(in: JsonReader, nullAsEmpty: Boolean): Boolean =
    if (in.isNextToken('['.toByte)) true
    else if (nullAsEmpty) in.readNullOrTokenError(false, '['.toByte)
    else in.decodeError("expected '['")

  /** Like [[openArray]], for JSON objects. */
  def openObject(in: JsonReader, nullAsEmpty: Boolean): Boolean =
    if (in.isNextToken('{'.toByte)) true
    else if (nullAsEmpty) in.readNullOrTokenError(false, '{'.toByte)
    else in.decodeError("expected '{'")

  /** Decodes a field that has a default value: a JSON `null` decodes as the default (as in jsoniter-scala, which passes
    * the field's current value as `default` to the field's decoder).
    */
  @inline def readNullAsDefault[A](in: JsonReader, default: => A)(decode: JsonReader => A): A =
    if (in.isNextToken('n'.toByte)) in.readNullOrError(default, "expected value or null")
    else {
      in.rollbackToken()
      decode(in)
    }

  /** A decoded collection/map field that came out empty (from `[]`/`{}`) is the field's default, as in jsoniter-scala
    * (whose collection decoders return their `default` argument for an empty JSON array/object).
    */
  def emptyAsDefault[A](value: A, default: => A): A = (value: Any) match {
    case it: Iterable[?]             => if (it.isEmpty) default else value
    case arr: Array[?]               => if (arr.length == 0) default else value
    case it: java.util.Collection[?] => if (it.isEmpty) default else value
    case m: java.util.Map[?, ?]      => if (m.isEmpty) default else value
    case _                           => value
  }

  /** MathContext used to round decoded `BigDecimal`s to `bigDecimalPrecision` (same as jsoniter-scala). */
  def mathContext(precision: Int): java.math.MathContext =
    if (precision == java.math.MathContext.DECIMAL128.getPrecision) java.math.MathContext.DECIMAL128
    else new java.math.MathContext(precision, java.math.RoundingMode.HALF_EVEN)

  /** `==` for values, contents comparison for arrays (`transientDefault` of array-typed fields). */
  def valueEquals(a: Any, b: Any): Boolean = (a, b) match {
    case (x: Array[?], y: Array[?]) => java.util.Objects.deepEquals(x, y)
    case _                          => a == b
  }

  /** Reads the rest of an ADT leaf object without (non-transient) fields in discriminator mode. When `afterOpen`, the
    * reader is right after the `{` (the object was re-read from a mark, `requireDiscriminatorFirst = false`) and the
    * discriminator is still in the object; otherwise the discriminator was consumed already. The discriminator is
    * skipped (a second one is a duplicate when `checkDuplication`), other keys are skipped when `skipUnexpected` or
    * fail with jsoniter-scala's `unexpected field` error.
    */
  def readInlineObjectWithoutFields(
      in: JsonReader,
      skipUnexpected: Boolean,
      discriminator: String,
      checkDuplication: Boolean,
      afterOpen: Boolean
  ): Unit = {
    val hasKeys =
      if (afterOpen) {
        if (in.isNextToken('}'.toByte)) false
        else {
          in.rollbackToken()
          true
        }
      } else if (in.isNextToken(','.toByte)) true
      else {
        if (!in.isCurrentToken('}'.toByte)) in.objectEndOrCommaError()
        false
      }
    if (hasKeys) {
      var discriminatorSeen = !afterOpen
      var l = -1
      while (l < 0 || in.isNextToken(','.toByte)) {
        l = in.readKeyAsCharBuf()
        if ((discriminator ne null) && in.isCharBufEqualsTo(l, discriminator)) {
          if (discriminatorSeen && checkDuplication) in.duplicatedKeyError(l)
          discriminatorSeen = true
          in.skip()
        } else if (skipUnexpected) in.skip()
        else in.unexpectedKeyError(l)
      }
      if (!in.isCurrentToken('}'.toByte)) in.objectEndOrCommaError()
    }
  }

  def readObject[A](
      in: JsonReader,
      fieldCount: Int,
      construct: Array[Any] => A
  )(dispatch: (String, Array[Any], JsonReader) => Unit): A = {
    val arr = newFieldArray(fieldCount)
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

  def readObjectOptimized[A](
      in: JsonReader,
      fieldCount: Int,
      construct: Array[Any] => A
  )(dispatch: (String, Array[Any], JsonReader) => Unit): A = {
    val arr = newFieldArray(fieldCount)
    if (!in.isNextToken('{'.toByte)) in.decodeError("expected '{'")
    if (!in.isNextToken('}'.toByte)) {
      in.rollbackToken()
      dispatch(in.readKeyAsString(), arr, in)
      while (in.isNextToken(','.toByte))
        dispatch(in.readKeyAsString(), arr, in)
      if (!in.isCurrentToken('}'.toByte)) in.objectEndOrCommaError()
    }
    construct(arr)
  }

  def readObjectDirect[A](in: JsonReader, construct: => A)(dispatch: (String, JsonReader) => Unit): A = {
    if (!in.isNextToken('{'.toByte)) in.decodeError("expected '{'")
    if (!in.isNextToken('}'.toByte)) {
      in.rollbackToken()
      dispatch(in.readKeyAsString(), in)
      while (in.isNextToken(','.toByte))
        dispatch(in.readKeyAsString(), in)
      if (!in.isCurrentToken('}'.toByte)) in.objectEndOrCommaError()
    }
    construct
  }

  /** Reads the remaining fields of an ADT leaf object in discriminator mode. When `afterOpen`, the reader is right
    * after the `{` (the object was re-read from a mark, `requireDiscriminatorFirst = false`): the first occurrence of
    * the `discriminator` key is skipped here, later ones reach `dispatch` (which treats them as duplicates). Otherwise
    * the discriminator was consumed already and the reader is right after its value.
    */
  def readObjectInline[A](
      in: JsonReader,
      fieldCount: Int,
      construct: Array[Any] => A,
      afterOpen: Boolean,
      discriminator: String
  )(dispatch: (String, Array[Any], JsonReader) => Unit): A = {
    val arr = newFieldArray(fieldCount)
    val hasKeys =
      if (afterOpen) {
        if (in.isNextToken('}'.toByte)) false
        else {
          in.rollbackToken()
          true
        }
      } else if (in.isNextToken(','.toByte)) true
      else {
        if (!in.isCurrentToken('}'.toByte)) in.objectEndOrCommaError()
        false
      }
    if (hasKeys) {
      var discriminatorSeen = !afterOpen
      var first = true
      while (first || in.isNextToken(','.toByte)) {
        first = false
        val key = in.readKeyAsString()
        if (!discriminatorSeen && key == discriminator) {
          discriminatorSeen = true
          in.skip()
        } else dispatch(key, arr, in)
      }
      if (!in.isCurrentToken('}'.toByte)) in.objectEndOrCommaError()
    }
    construct(arr)
  }

  def readWrapped[A](in: JsonReader)(dispatch: String => A): A = {
    if (!in.isNextToken('{'.toByte)) in.decodeError("expected '{'")
    val typeName = in.readKeyAsString()
    val result = dispatch(typeName)
    if (!in.isNextToken('}'.toByte)) in.objectEndOrCommaError()
    result
  }

  /** Reads an ADT object in discriminator mode and dispatches on the discriminator value; `dispatch` receives the value
    * and whether the reader is right after the object's `{` (`afterOpen`, see `readObjectInline`) or right after the
    * discriminator value. As in jsoniter-scala: with `requireFirst` the discriminator must be the first key (`expected
    * key: "type"` otherwise); without it, the decoder marks the object start, skips to the discriminator (`missing
    * required field "type"` when absent) and re-reads the object from the mark.
    */
  def readWithDiscriminator[A](in: JsonReader, discriminatorField: String, requireFirst: Boolean)(
      dispatch: (String, Boolean) => A
  ): A = readWithDiscriminator[A](in, discriminatorField, requireFirst, false)(dispatch)

  /** With `markObjectStart`, a mark is set right before the object's `{` while `dispatch` runs, so that a leaf decoded
    * by a user-provided codec can `rollbackToMark()` and read the whole object (as jsoniter-scala does).
    */
  def readWithDiscriminator[A](
      in: JsonReader,
      discriminatorField: String,
      requireFirst: Boolean,
      markObjectStart: Boolean
  )(
      dispatch: (String, Boolean) => A
  ): A =
    if (requireFirst) {
      if (markObjectStart) in.setMark()
      if (!in.isNextToken('{'.toByte)) in.decodeError("expected '{'")
      if (!in.isCharBufEqualsTo(in.readKeyAsCharBuf(), discriminatorField))
        in.decodeError("expected key: \"" + discriminatorField + "\"")
      val result = dispatch(in.readString(null), false)
      if (markObjectStart) releaseMark(in)
      result
    } else {
      in.setMark()
      if (in.isNextToken('{'.toByte)) {
        if (in.skipToKey(discriminatorField)) {
          val typeName = in.readString(null)
          in.rollbackToMark()
          if (markObjectStart) in.setMark()
          val _ = in.isNextToken('{'.toByte)
          val result = dispatch(typeName, true)
          if (markObjectStart) releaseMark(in)
          result
        } else in.requiredFieldError(discriminatorField)
      } else {
        in.resetMark()
        in.decodeError("expected '{'")
      }
    }

  // Nested decoders may have used (and released) the reader's single mark meanwhile.
  private def releaseMark(in: JsonReader): Unit =
    try in.resetMark()
    catch { case _: IllegalStateException => () }

  /** A leaf with a user-provided codec in discriminator mode: re-reads the whole object with it. */
  def readWholeObjectWith[A](in: JsonReader, codec: JsonValueCodec[A]): A = {
    in.rollbackToMark()
    in.setMark() // released by readWithDiscriminator
    codec.decodeValue(in, codec.nullValue)
  }

  /** jsoniter-scala's error for an unknown discriminator value (`illegal value of discriminator field "type"`), or for
    * an unknown leaf name without a discriminator field (`illegal discriminator`).
    */
  def unknownDiscriminator[A](in: JsonReader, discriminatorField: String): A =
    if (discriminatorField eq null) in.discriminatorError()
    else in.discriminatorValueError(discriminatorField)

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
    if (openObject(in, nullAsEmpty = true) && !in.isNextToken('}'.toByte)) {
      in.rollbackToken()
      var count = 1
      val key = decodeKey(in)
      builder += ((key, decodeValue(in)))
      while (in.isNextToken(','.toByte)) {
        count += 1
        if (count > maxInserts) in.decodeError("too many map inserts")
        val k = decodeKey(in)
        builder += ((k, decodeValue(in)))
      }
      if (!in.isCurrentToken('}'.toByte)) in.decodeError("expected '}' or ','")
    }
    builder.result()
  }

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
    if (openArray(in, nullAsEmpty = true) && !in.isNextToken(']'.toByte)) {
      in.rollbackToken()
      var count = 1
      if (!in.isNextToken('['.toByte)) in.decodeError("expected '['")
      val k = decodeKey(in)
      if (!in.isNextToken(','.toByte)) in.commaError()
      val v = decodeValue(in)
      if (!in.isNextToken(']'.toByte)) in.arrayEndError()
      builder += ((k, v))
      while (in.isNextToken(','.toByte)) {
        count += 1
        if (count > maxInserts) in.decodeError("too many map inserts")
        if (!in.isNextToken('['.toByte)) in.decodeError("expected '['")
        val k = decodeKey(in)
        if (!in.isNextToken(','.toByte)) in.commaError()
        val v = decodeValue(in)
        if (!in.isNextToken(']'.toByte)) in.arrayEndError()
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

  /** Marks slots of [[readObject]]/`readObjectInline` arrays whose fields were absent from the JSON object (`null`
    * cannot be used, because it is a legal decoded value).
    */
  private object AbsentField

  @inline def isAbsentField(value: Any): Boolean = value.asInstanceOf[AnyRef] eq AbsentField

  /** For a `scala.transient` field with a runtime config: skips the value (keeping the slot absent, so it gets the
    * default) when `scalaTransientSupport` is on.
    */
  def readUnlessTransient(in: JsonReader, transient: Boolean, decode: JsonReader => Any): Any =
    if (transient) {
      in.skip()
      AbsentField
    } else decode(in)

  private def newFieldArray(fieldCount: Int): Array[Any] = {
    val arr = new Array[Any](fieldCount)
    java.util.Arrays.fill(arr.asInstanceOf[Array[AnyRef]], AbsentField)
    arr
  }

  /** Fails with jsoniter-scala's `missing required field "<name>"` error. */
  def missingRequiredField[A](in: JsonReader, fieldName: String): A = in.requiredFieldError(fieldName)

  /** jsoniter-scala's `unexpected field "<name>"` error, for decoders that read keys as `String`s. */
  def unexpectedField(in: JsonReader, fieldName: String): Nothing =
    in.decodeError("unexpected field \"" + fieldName + "\"")

  def throwDuplicateField(in: JsonReader, fieldName: String): Nothing =
    in.decodeError("duplicated field \"" + fieldName + "\"")

  // --- BitSet helpers ---

  def readImmutableBitSet(in: JsonReader, limit: Int): scala.collection.immutable.BitSet =
    readImmutableBitSet(in, limit, false)

  /** As jsoniter-scala: values are JSON numbers (strings with `isStringified`/`@stringified`), `illegal value for bit
    * set` outside of `[0, bitSetValueLimit)`.
    */
  def readImmutableBitSet(in: JsonReader, limit: Int, stringified: Boolean): scala.collection.immutable.BitSet =
    scala.collection.immutable.BitSet.fromBitMaskNoCopy(readBitSetMask(in, limit, stringified))

  def readMutableBitSet(in: JsonReader, limit: Int): scala.collection.mutable.BitSet =
    readMutableBitSet(in, limit, false)

  def readMutableBitSet(in: JsonReader, limit: Int, stringified: Boolean): scala.collection.mutable.BitSet =
    scala.collection.mutable.BitSet.fromBitMaskNoCopy(readBitSetMask(in, limit, stringified))

  private def readBitSetMask(in: JsonReader, limit: Int, stringified: Boolean): Array[Long] = {
    var mask = new Array[Long](2)
    if (!in.isNextToken('['.toByte)) in.decodeError("expected '[' or null")
    if (!in.isNextToken(']'.toByte)) {
      in.rollbackToken()
      var first = true
      while (first || in.isNextToken(','.toByte)) {
        first = false
        val v = if (stringified) in.readStringAsInt() else in.readInt()
        if (v < 0 || v >= limit) in.decodeError("illegal value for bit set")
        val i = v >>> 6
        if (i >= mask.length) mask = java.util.Arrays.copyOf(mask, Math.max(mask.length << 1, i + 1))
        mask(i) |= 1L << v
      }
      if (!in.isCurrentToken(']'.toByte)) in.arrayEndOrCommaError()
    }
    mask
  }

  def writeBitSet(out: JsonWriter, value: scala.collection.BitSet): Unit = writeBitSet(out, value, false)

  def writeBitSet(out: JsonWriter, value: scala.collection.BitSet, stringified: Boolean): Unit = {
    out.writeArrayStart()
    val iter = value.iterator
    if (stringified) while (iter.hasNext) out.writeValAsString(iter.next())
    else while (iter.hasNext) out.writeVal(iter.next())
    out.writeArrayEnd()
  }

  def mapEnumName(
      config: hearth.kindlings.jsoniterderivation.JsoniterConfig,
      caseName: String,
      isJavaEnum: Boolean
  ): String = {
    val base = config.adtLeafClassNameMapper(caseName)
    if (isJavaEnum) config.javaEnumValueNameMapper(base) else base
  }

  // --- Helpers of Kindlings 0.3.x, kept for binary compatibility of codecs compiled against it (with their 0.3.2
  // behaviour); the macro no longer emits them ---

  final private val Compat03 =
    "kept for binary compatibility of code compiled against Kindlings 0.3.x; the macro no longer emits it"

  @deprecated(Compat03, "0.3.4")
  def writeScalaEnumValueId(out: JsonWriter, value: Any): Unit =
    out.writeVal(value.asInstanceOf[scala.Enumeration#Value].id)

  @deprecated(Compat03, "0.3.4")
  def readScalaEnumValueId[A](in: JsonReader)(dispatch: Int => A): A = {
    val id = in.readInt()
    dispatch(id)
  }

  /** Skip remaining fields and consume closing } of an already-opened inline object (e.g., after the discriminator
    * field was consumed).
    */
  @deprecated(Compat03, "0.3.4")
  @scala.annotation.nowarn("msg=unused value|discarded non-Unit")
  def skipInlineObjectRemainder(in: JsonReader): Unit =
    if (in.isNextToken(','.toByte)) {
      in.readKeyAsString()
      in.skip()
      while (in.isNextToken(','.toByte)) {
        in.readKeyAsString()
        in.skip()
      }
      if (!in.isCurrentToken('}'.toByte)) in.decodeError("expected '}' or ','")
    } else {
      if (!in.isCurrentToken('}'.toByte)) in.decodeError("expected '}'")
    }

  @deprecated(Compat03, "0.3.4")
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

  @deprecated(Compat03, "0.3.4")
  def readWithDiscriminator[A](in: JsonReader, discriminatorField: String)(dispatch: String => A): A = {
    // The discriminator has to be the first field.
    if (!in.isNextToken('{'.toByte)) in.decodeError("expected '{' for discriminated enum")
    val firstKey = in.readKeyAsString()
    if (firstKey != discriminatorField)
      in.decodeError("expected discriminator field '" + discriminatorField + "' as first field, got '" + firstKey + "'")
    val typeName = in.readString(null)
    // The dispatch function continues reading the rest of the object from the same reader.
    dispatch(typeName)
  }

  @deprecated(Compat03, "0.3.4")
  def throwMissingField(fieldName: String): Nothing =
    throw new IllegalArgumentException(s"missing required field: $fieldName")

  @deprecated(Compat03, "0.3.4")
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

  @deprecated(Compat03, "0.3.4")
  def validateBigInt(in: JsonReader, value: BigInt, digitsLimit: Int): BigInt = {
    if (value ne null) {
      if (value.underlying.bitLength * 30103 / 100000 + 1 > digitsLimit) // log10(2) ≈ 0.30103
        in.decodeError(s"BigInt digits exceed limit $digitsLimit")
    }
    value
  }
}
