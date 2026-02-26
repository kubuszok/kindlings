package hearth.kindlings.circederivation.internal.runtime

import cats.data.{NonEmptyList, Validated, ValidatedNel}
import hearth.kindlings.circederivation.KindlingsCodecAsObject
import io.circe.{Decoder, DecodingFailure, Encoder, HCursor, Json, JsonObject}

object CirceDerivationUtils {

  // --- Encoder helpers ---

  def jsonFromFields(fields: List[(String, Json)]): Json =
    Json.fromJsonObject(JsonObject.fromIterable(fields))

  def wrapWithTypeName(typeName: String, inner: Json): Json =
    Json.obj(typeName -> inner)

  def addDiscriminator(discriminatorField: String, typeName: String, inner: Json): Json =
    inner.asObject match {
      case Some(obj) => Json.fromJsonObject((discriminatorField, Json.fromString(typeName)) +: obj)
      case None      => Json.obj(discriminatorField -> Json.fromString(typeName), "value" -> inner)
    }

  def encodeEnumAsString(typeName: String): Json =
    Json.fromString(typeName)

  def decodeEnumFromString[A](cursor: HCursor, knownSubtypes: List[String])(
      dispatch: String => Either[DecodingFailure, A]
  ): Either[DecodingFailure, A] =
    cursor.as[String] match {
      case Right(typeName) => dispatch(typeName)
      case Left(_)         =>
        Left(
          DecodingFailure(
            s"Expected a JSON string for enum value. Known values: ${knownSubtypes.mkString(", ")}",
            cursor.history
          )
        )
    }

  def encodeOption[A](value: Option[A], encoder: A => Json): Json = value match {
    case Some(a) => encoder(a)
    case None    => Json.Null
  }

  def encodeIterable[A](items: Iterable[A], encoder: A => Json): Json =
    Json.fromValues(items.map(encoder))

  def encodeMapEntries[K, V](entries: Iterable[(K, V)], encodeKey: K => String, encodeValue: V => Json): Json =
    Json.fromJsonObject(JsonObject.fromIterable(entries.map { case (k, v) => (encodeKey(k), encodeValue(v)) }))

  def jsonFromMappedPairs[P](pairs: Iterable[P], toPair: P => (String, Json)): Json =
    Json.fromJsonObject(JsonObject.fromIterable(pairs.map(toPair)))

  // --- Decoder helpers ---

  def decodeField[A](cursor: HCursor, fieldName: String, decoder: Decoder[A]): Decoder.Result[A] =
    cursor.downField(fieldName).as[A](decoder)

  def decodeFieldWithDefault[A](
      cursor: HCursor,
      fieldName: String,
      decoder: Decoder[A],
      default: A
  ): Decoder.Result[A] = {
    val field = cursor.downField(fieldName)
    if (field.failed) Right(default)
    else field.as[A](decoder)
  }

  def decodeOptionField[A](cursor: HCursor, fieldName: String, decoder: Decoder[A]): Decoder.Result[Option[A]] = {
    val field = cursor.downField(fieldName)
    if (field.failed) Right(None)
    else
      field.focus match {
        case Some(json) if json.isNull => Right(None)
        case _                         => field.as[A](decoder).map(Some(_))
      }
  }

  def checkStrictDecoding(cursor: HCursor, expectedFields: Set[String]): Decoder.Result[Unit] = {
    val actualKeys = cursor.keys.map(_.toSet).getOrElse(Set.empty)
    val unexpected = actualKeys -- expectedFields
    if (unexpected.isEmpty) Right(())
    else
      Left(
        DecodingFailure(
          s"Unexpected field(s): ${unexpected.toList.sorted.mkString(", ")}",
          cursor.history
        )
      )
  }

  def checkStrictDecodingWithDiscriminator(
      cursor: HCursor,
      expectedFields: Set[String],
      discriminatorField: String
  ): Decoder.Result[Unit] =
    checkStrictDecoding(cursor, expectedFields + discriminatorField)

  def decodeWrapped(cursor: HCursor): Decoder.Result[(String, HCursor)] =
    cursor.keys.flatMap(_.headOption) match {
      case Some(typeName) =>
        Right((typeName, cursor.downField(typeName).success.getOrElse(cursor)))
      case None =>
        Left(
          DecodingFailure(
            "Expected a JSON object with a single key as type discriminator",
            cursor.history
          )
        )
    }

  def decodeDiscriminator(cursor: HCursor, field: String): Decoder.Result[(String, HCursor)] =
    cursor.downField(field).as[String] match {
      case Right(typeName) => Right((typeName, cursor))
      case Left(failure)   => Left(failure)
    }

  def failedToMatchSubtype(typeName: String, cursor: HCursor, knownSubtypes: List[String]): DecodingFailure =
    DecodingFailure(
      s"Unknown type discriminator: $typeName. Expected one of: ${knownSubtypes.mkString(", ")}",
      cursor.history
    )

  def decodeOption[A](cursor: HCursor, decoder: Decoder[A]): Decoder.Result[Option[A]] =
    cursor.focus match {
      case Some(json) if json.isNull => Right(None)
      case _                         => decoder(cursor).map(Some(_))
    }

  /** Like `decodeFieldWithDefault` but uses `Any` for the default value to avoid path-dependent type issues in macros.
    * The decoder's type parameter `A` serves as the type witness. Returns `Decoder.Result[Any]`.
    */
  def decodeFieldWithUnsafeDefault[A](
      cursor: HCursor,
      fieldName: String,
      decoder: Decoder[A],
      default: Any
  ): Decoder.Result[Any] = {
    val field = cursor.downField(fieldName)
    if (field.failed) Right(default)
    else field.as[A](decoder).asInstanceOf[Decoder.Result[Any]]
  }

  /** Cast an `Any` value to `A`, using a `Decoder[A]` purely for type inference. The decoder is not called - this is a
    * compile-time trick to avoid path-dependent type aliases in Scala 2 macro-generated code.
    */
  @scala.annotation.nowarn("msg=unused explicit parameter")
  def unsafeCast[A](value: Any, decoder: Decoder[A]): A = value.asInstanceOf[A]

  def checkIsObject(cursor: HCursor): Either[DecodingFailure, Unit] =
    if (cursor.value.isObject) Right(())
    else Left(DecodingFailure("Expected JSON object", cursor.history))

  def sequenceDecodeResults(results: List[Either[DecodingFailure, Any]]): Either[DecodingFailure, Array[Any]] = {
    val arr = new Array[Any](results.size)
    var i = 0
    val iter = results.iterator
    while (iter.hasNext)
      iter.next() match {
        case Right(v) => arr(i) = v; i += 1
        case Left(e)  => return Left(e)
      }
    Right(arr)
  }

  /** Creates a Decoder[A] from a decode function. Type A inferred from the function. */
  def decoderFromFn[A](decode: HCursor => Either[DecodingFailure, A]): Decoder[A] =
    new Decoder[A] { def apply(c: HCursor): Decoder.Result[A] = decode(c) }

  def decoderFromFnWithAcc[A](
      decode: HCursor => Either[DecodingFailure, A],
      decodeAcc: HCursor => ValidatedNel[DecodingFailure, A]
  ): Decoder[A] =
    new Decoder[A] {
      def apply(c: HCursor): Decoder.Result[A] = decode(c)
      override def decodeAccumulating(c: HCursor): Decoder.AccumulatingResult[A] = decodeAcc(c)
    }

  /** Decodes Option[A] from cursor using a decode function. Returns Right(None) for null. */
  def decodeOptionFromFn[A](
      cursor: HCursor,
      decode: HCursor => Either[DecodingFailure, A]
  ): Either[DecodingFailure, Option[A]] =
    cursor.focus match {
      case Some(json) if json.isNull => Right(None)
      case _                         => decode(cursor).map(Some(_))
    }

  /** Decodes a collection using an item decoder and factory. */
  def decodeCollectionWith[Item, Coll](
      cursor: HCursor,
      itemDecoder: Decoder[Item],
      factory: scala.collection.Factory[Item, Coll]
  ): Either[DecodingFailure, Coll] =
    cursor.values match {
      case None             => Left(DecodingFailure("Expected JSON array", cursor.history))
      case Some(jsonValues) =>
        val builder = factory.newBuilder
        val iter = jsonValues.iterator
        while (iter.hasNext)
          itemDecoder.decodeJson(iter.next()) match {
            case Right(item) => builder += item
            case Left(err)   => return Left(err)
          }
        Right(builder.result())
    }

  /** Decodes a Map[String, V] using a value decoder and factory. */
  def decodeMapWith[V, M](
      cursor: HCursor,
      valueDecoder: Decoder[V],
      factory: scala.collection.Factory[(String, V), M]
  ): Either[DecodingFailure, M] =
    cursor.keys match {
      case None       => Left(DecodingFailure("Expected JSON object", cursor.history))
      case Some(keys) =>
        val builder = factory.newBuilder
        val iter = keys.iterator
        while (iter.hasNext) {
          val key = iter.next()
          cursor.downField(key).as(valueDecoder) match {
            case Right(value) => builder += ((key, value))
            case Left(err)    => return Left(err)
          }
        }
        Right(builder.result())
    }

  /** Decodes a Map[K, V] using a key decoder function, value decoder, and factory. */
  def decodeMapWithKeyDecoder[K, V, M](
      cursor: HCursor,
      decodeKey: String => Either[DecodingFailure, K],
      valueDecoder: Decoder[V],
      factory: scala.collection.Factory[(K, V), M]
  ): Either[DecodingFailure, M] =
    cursor.keys match {
      case None       => Left(DecodingFailure("Expected JSON object", cursor.history))
      case Some(keys) =>
        val builder = factory.newBuilder
        val iter = keys.iterator
        while (iter.hasNext) {
          val keyStr = iter.next()
          decodeKey(keyStr) match {
            case Left(err) => return Left(err)
            case Right(k)  =>
              cursor.downField(keyStr).as(valueDecoder) match {
                case Right(value) => builder += ((k, value))
                case Left(err)    => return Left(err)
              }
          }
        }
        Right(builder.result())
    }

  /** Decodes an enum key from a string using a lookup map built at macro time. */
  def decodeEnumKey[K](
      keyStr: String,
      lookup: Map[String, K]
  ): Either[DecodingFailure, K] =
    lookup.get(keyStr) match {
      case Some(k) => Right(k)
      case None    =>
        Left(
          DecodingFailure(
            s"Unknown enum key: $keyStr. Expected one of: ${lookup.keys.mkString(", ")}",
            Nil
          )
        )
    }

  // --- Built-in key decoder helpers ---

  def decodeKeyInt(keyStr: String): Either[DecodingFailure, Int] =
    try Right(java.lang.Integer.parseInt(keyStr))
    catch { case _: NumberFormatException => Left(DecodingFailure(s"Couldn't decode $keyStr as Int key", Nil)) }

  def decodeKeyLong(keyStr: String): Either[DecodingFailure, Long] =
    try Right(java.lang.Long.parseLong(keyStr))
    catch { case _: NumberFormatException => Left(DecodingFailure(s"Couldn't decode $keyStr as Long key", Nil)) }

  def decodeKeyDouble(keyStr: String): Either[DecodingFailure, Double] =
    try Right(java.lang.Double.parseDouble(keyStr))
    catch { case _: NumberFormatException => Left(DecodingFailure(s"Couldn't decode $keyStr as Double key", Nil)) }

  def decodeKeyShort(keyStr: String): Either[DecodingFailure, Short] =
    try Right(java.lang.Short.parseShort(keyStr))
    catch { case _: NumberFormatException => Left(DecodingFailure(s"Couldn't decode $keyStr as Short key", Nil)) }

  def decodeKeyByte(keyStr: String): Either[DecodingFailure, Byte] =
    try Right(java.lang.Byte.parseByte(keyStr))
    catch { case _: NumberFormatException => Left(DecodingFailure(s"Couldn't decode $keyStr as Byte key", Nil)) }

  // --- Accumulating decoder helpers ---

  def sequenceDecodeResultsAccumulating(
      results: List[ValidatedNel[DecodingFailure, Any]]
  ): ValidatedNel[DecodingFailure, Array[Any]] = {
    val arr = new Array[Any](results.size)
    var errors: List[DecodingFailure] = Nil
    var i = 0
    val iter = results.iterator
    while (iter.hasNext) {
      iter.next() match {
        case Validated.Valid(v)   => arr(i) = v
        case Validated.Invalid(e) => errors = e.toList reverse_::: errors
      }
      i += 1
    }
    NonEmptyList.fromList(errors.reverse) match {
      case Some(nel) => Validated.Invalid(nel)
      case None      => Validated.Valid(arr)
    }
  }

  def checkStrictDecodingAccumulating(
      cursor: HCursor,
      expectedFields: Set[String]
  ): ValidatedNel[DecodingFailure, Unit] =
    Validated.fromEither(checkStrictDecoding(cursor, expectedFields)).leftMap(NonEmptyList.one)

  def checkIsObjectAccumulating(cursor: HCursor): ValidatedNel[DecodingFailure, Unit] =
    Validated.fromEither(checkIsObject(cursor)).leftMap(NonEmptyList.one)

  def decodeFieldAccumulating[A](
      cursor: HCursor,
      fieldName: String,
      decoder: Decoder[A]
  ): ValidatedNel[DecodingFailure, A] =
    decoder.tryDecodeAccumulating(cursor.downField(fieldName))

  def decodeFieldWithDefaultAccumulating[A](
      cursor: HCursor,
      fieldName: String,
      decoder: Decoder[A],
      default: Any
  ): ValidatedNel[DecodingFailure, Any] = {
    val field = cursor.downField(fieldName)
    if (field.failed) Validated.Valid(default)
    else decoder.tryDecodeAccumulating(field).map(x => x: Any)
  }

  // --- Codec.AsObject combiner ---

  def codecAsObject[A](
      enc: Encoder.AsObject[A],
      dec: Decoder[A]
  ): KindlingsCodecAsObject[A] = new KindlingsCodecAsObject[A] {
    override def encodeObject(a: A): JsonObject = enc.encodeObject(a)
    override def apply(c: HCursor): Decoder.Result[A] = dec(c)
  }
}
