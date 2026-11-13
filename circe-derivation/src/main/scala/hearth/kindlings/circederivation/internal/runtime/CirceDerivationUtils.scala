package hearth.kindlings.circederivation.internal.runtime

import cats.data.{NonEmptyList, Validated, ValidatedNel}
import hearth.kindlings.circederivation.{ExtrasDecoder, KindlingsCodecAsObject}
import io.circe.{Decoder, DecodingFailure, Encoder, HCursor, Json, JsonObject}

object CirceDerivationUtils {

  /** Thrown from inside a collection/map builder loop: carries the first failure (fail-fast) or all of them
    * (accumulating).
    */
  class CollectionBuildException(val failures: NonEmptyList[DecodingFailure])
      extends RuntimeException("circe collection decoding error", null, false, false) {
    def this(failure: DecodingFailure) = this(NonEmptyList.one(failure))
    def failure: DecodingFailure = failures.head
  }

  // --- Results selected by `failFast` ---
  //
  // Derived decode code returns `Any`: an `Either[DecodingFailure, A]` when `failFast` is true and a
  // `ValidatedNel[DecodingFailure, A]` when it is false (see `DecoderMacrosImpl.DecoderCtx`).

  /** Reinterprets a decode result (an `Either` or a `ValidatedNel`, see above) without a runtime check: the type
    * parameter erases the cast, so a `ValidatedNel` passing through an `Either`-typed expression is not checked.
    */
  def unsafeResult[R](result: Any): R = result.asInstanceOf[R]

  def okResult(failFast: Boolean, value: Any): Any =
    if (failFast) Right(value) else Validated.Valid(value)

  def failResult(failFast: Boolean, failure: DecodingFailure): Any =
    if (failFast) Left(failure) else Validated.invalidNel(failure)

  def failuresResult(failFast: Boolean, failures: NonEmptyList[DecodingFailure]): Any =
    if (failFast) Left(failures.head) else Validated.Invalid(failures)

  def collectionFailure(failFast: Boolean, e: CollectionBuildException): Any =
    failuresResult(failFast, e.failures)

  def fromEitherResult(failFast: Boolean, either: Either[DecodingFailure, Any]): Any =
    if (failFast) either
    else
      either match {
        case Right(v) => Validated.Valid(v)
        case Left(e)  => Validated.invalidNel(e)
      }

  /** Runs a decoder in the mode selected by `failFast` (`decodeAccumulating` when accumulating). */
  def runDecoder[A](decoder: Decoder[A], cursor: HCursor, failFast: Boolean): Any =
    if (failFast) decoder(cursor) else decoder.decodeAccumulating(cursor)

  /** Continues a result with a fail-fast step (e.g. a value type's smart constructor). */
  def flatMapResult[A, B](failFast: Boolean, result: Any, f: A => Either[DecodingFailure, B]): Any =
    if (failFast) result.asInstanceOf[Either[DecodingFailure, A]].flatMap(f)
    else
      result.asInstanceOf[ValidatedNel[DecodingFailure, A]] match {
        case Validated.Valid(a) =>
          f(a) match {
            case Right(b) => Validated.Valid(b)
            case Left(e)  => Validated.invalidNel(e)
          }
        case invalid => invalid
      }

  def mapResult[A, B](failFast: Boolean, result: Any, f: A => B): Any =
    if (failFast) result.asInstanceOf[Either[DecodingFailure, A]].map(f)
    else result.asInstanceOf[ValidatedNel[DecodingFailure, A]].map(f)

  /** Decodes the elements of a tuple from a JSON array of exactly `decoders.size` elements (circe's `decodeTupleN`). */
  def decodeTupleElements(cursor: HCursor, failFast: Boolean, decoders: List[Decoder[Any]]): Any = {
    val n = decoders.size
    cursor.value.asArray match {
      case Some(values) if values.lengthCompare(n) == 0 =>
        val arr = new Array[Any](n)
        val errors = List.newBuilder[DecodingFailure]
        var failed = false
        var i = 0
        val it = decoders.iterator
        while (it.hasNext) {
          val decoder = it.next()
          val elem = cursor.downN(i)
          if (failFast)
            decoder.tryDecode(elem) match {
              case Right(v) => arr(i) = v
              case Left(e)  => return Left(e)
            }
          else
            decoder.tryDecodeAccumulating(elem) match {
              case Validated.Valid(v)    => arr(i) = v
              case Validated.Invalid(es) => failed = true; errors ++= es.toList
            }
          i += 1
        }
        if (failFast) Right(arr)
        else if (failed) Validated.Invalid(NonEmptyList.fromListUnsafe(errors.result()))
        else Validated.Valid(arr)
      case _ => failResult(failFast, wrongTypeFailure("array", cursor))
    }
  }

  /** Decodes an `Option[A]` (`null` is `None`) with a decode function honouring `failFast`. */
  def decodeOptionResult(cursor: HCursor, failFast: Boolean, decode: HCursor => Any): Any =
    if (cursor.value.isNull) okResult(failFast, None)
    else if (failFast) decode(cursor).asInstanceOf[Either[DecodingFailure, Any]].map(Some(_))
    else decode(cursor).asInstanceOf[ValidatedNel[DecodingFailure, Any]].map(Some(_))

  /** `decoder.map(f)` that also keeps `decoder`'s `tryDecode`/`tryDecodeAccumulating` (e.g. an `Option` decoder's
    * absent-key handling), like generic-extras' unwrapped decoders.
    */
  def mapDecoder[A, B](decoder: Decoder[A], f: A => B): Decoder[B] =
    new Decoder[B] {
      def apply(c: HCursor): Decoder.Result[B] = decoder(c).map(f)
      override def decodeAccumulating(c: HCursor): Decoder.AccumulatingResult[B] = decoder.decodeAccumulating(c).map(f)
      override def tryDecode(c: io.circe.ACursor): Decoder.Result[B] = decoder.tryDecode(c).map(f)
      override def tryDecodeAccumulating(c: io.circe.ACursor): Decoder.AccumulatingResult[B] =
        decoder.tryDecodeAccumulating(c).map(f)
    }

  /** A `Decoder` from a decode function honouring `failFast`: `apply` and `decodeAccumulating` both delegate to it. */
  def decoderFromFF[A](decode: (HCursor, Boolean) => Any): Decoder[A] =
    new Decoder[A] {
      def apply(c: HCursor): Decoder.Result[A] = decode(c, true).asInstanceOf[Decoder.Result[A]]
      override def decodeAccumulating(c: HCursor): Decoder.AccumulatingResult[A] =
        decode(c, false).asInstanceOf[Decoder.AccumulatingResult[A]]
    }

  /** Like [[decoderFromFF]] for a type whose decoder accepts `null` (an `Option`, or a value type wrapping one): like
    * circe's `Decoder[Option[A]]`, an absent key (a failed cursor whose focus was an object) is decoded as `null`, and
    * a failed cursor with an incorrect focus fails with a missing-field error.
    */
  def nullableDecoderFromFF[A](decode: (HCursor, Boolean) => Any): Decoder[A] =
    new Decoder[A] with DerivedNullableDecoder {
      def apply(c: HCursor): Decoder.Result[A] = decode(c, true).asInstanceOf[Decoder.Result[A]]
      override def decodeAccumulating(c: HCursor): Decoder.AccumulatingResult[A] =
        decode(c, false).asInstanceOf[Decoder.AccumulatingResult[A]]
      override def tryDecode(c: io.circe.ACursor): Decoder.Result[A] = c match {
        case h: HCursor => apply(h)
        case f          =>
          if (missingKey(f))
            decode(Json.Null.hcursor, true) match {
              // the same instance as circe's `Decoder[Option[A]]`, so `useDefaults` can tell an absent key apart
              case Right(None) => keyMissingNone.asInstanceOf[Decoder.Result[A]]
              case Right(a)    => Right(a.asInstanceOf[A])
              // `null` is not accepted (e.g. a value class encoded as a product): report the field as missing
              case _ => Left(DecodingFailure(DecodingFailure.Reason.MissingField, f.history))
            }
          else Left(DecodingFailure(DecodingFailure.Reason.MissingField, f.history))
      }
      override def tryDecodeAccumulating(c: io.circe.ACursor): Decoder.AccumulatingResult[A] = c match {
        case h: HCursor => decodeAccumulating(h)
        case f          =>
          if (missingKey(f))
            decode(Json.Null.hcursor, false) match {
              case Validated.Valid(None) => keyMissingNoneAccumulating.asInstanceOf[Decoder.AccumulatingResult[A]]
              case Validated.Valid(a)    => Validated.Valid(a.asInstanceOf[A])
              case _ => Validated.invalidNel(DecodingFailure(DecodingFailure.Reason.MissingField, f.history))
            }
          else Validated.invalidNel(DecodingFailure(DecodingFailure.Reason.MissingField, f.history))
      }
    }

  /** Marks the decoders built by [[nullableDecoderFromFF]]: for them an absent key is only decoded as `null` to fill a
    * field without a usable default, so with `useDefaults` an absent key takes the field's default (also for a value
    * type wrapping an `Option`, whose decoder cannot return circe's `keyMissingNone`), like a plain `Option` field.
    */
  private trait DerivedNullableDecoder

  /** An absent key decoded by a derived nullable decoder: the default is used for it (see [[DerivedNullableDecoder]]).
    */
  private def absentKeyOfDerivedNullable(decoder: Decoder[?], c: io.circe.ACursor): Boolean =
    decoder.isInstanceOf[DerivedNullableDecoder] && missingKey(c)

  private def missingKey(c: io.circe.ACursor): Boolean = c match {
    case f: io.circe.FailedCursor => !f.incorrectFocus
    case _                        => false
  }

  // --- Encoder helpers ---

  def jsonFromFields(fields: List[(String, Json)]): Json =
    Json.fromJsonObject(JsonObject.fromIterable(fields))

  def wrapWithTypeName(typeName: String, inner: Json): Json =
    Json.obj(typeName -> inner)

  /** The JSON object an `Encoder.AsObject` returns (see `KindlingsEncoder.deriveAsObject`). */
  def asJsonObjectOrThrow(json: Json): io.circe.JsonObject = json.asObject match {
    case Some(obj) => obj
    case None      =>
      throw new IllegalStateException(
        "Encoder.AsObject: produced non-object JSON. This can happen when using enumAsStrings=true " +
          "with a sealed trait of case objects. Use KindlingsEncoder.derived instead."
      )
  }

  def addDiscriminator(discriminatorField: String, typeName: String, inner: Json): Json =
    inner.asObject match {
      // Appended after the fields, like circe's `ConfiguredEncoder.encodeSum` and generic-extras (`JsonObject.add`).
      case Some(obj) => Json.fromJsonObject(obj.add(discriminatorField, Json.fromString(typeName)))
      // A child that does not encode to a JSON object cannot carry the discriminator: like circe-generic-extras, fall
      // back to the wrapped form (`{"Name": inner}`); `decodeDiscriminatorOrWrapped` reads it back.
      case None => Json.obj(typeName -> inner)
    }

  /** For a child with a user-provided encoder (circe's `ConfiguredEncoder.encodeSum`): when its JSON object already
    * contains the discriminator it is left as is, otherwise the discriminator is added.
    */
  def addDiscriminatorIfAbsent(discriminatorField: String, typeName: String, inner: Json): Json =
    inner.asObject match {
      case Some(obj) if obj.contains(discriminatorField) => inner
      case _                                             => addDiscriminator(discriminatorField, typeName, inner)
    }

  def encodeEnumAsString(typeName: String): Json =
    Json.fromString(typeName)

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

  /** Like [[jsonFromMappedPairs]] for non-`String` keys: `toPair` returns the key, which `encodeKey` turns into a
    * `String`.
    */
  def jsonFromKeyedPairs[P, K](pairs: Iterable[P], toPair: P => (K, Json), encodeKey: K => String): Json =
    Json.fromJsonObject(JsonObject.fromIterable(pairs.map { p =>
      val (k, json) = toPair(p)
      (encodeKey(k), json)
    }))

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

  /** Strict decoding of a product, like circe's `ConfiguredDecoder`: unexpected keys are listed in JSON order and the
    * valid ones in declaration order (followed by the discriminator).
    */
  def checkStrictDecoding(cursor: HCursor, typeName: String, expectedFields: List[String]): Decoder.Result[Unit] = {
    val keys = cursor.keys.fold(List.empty[String])(_.toList)
    val unexpected = keys.filterNot(expectedFields.contains)
    if (unexpected.isEmpty) Right(())
    else
      Left(
        DecodingFailure(
          s"Strict decoding $typeName - unexpected fields: ${unexpected.mkString(", ")}; valid fields: ${expectedFields
              .mkString(", ")}.",
          cursor.history
        )
      )
  }

  /** Reads a wrapper-style (`{"Constructor": {...}}`) sum like circe's `ConfiguredDecoder.decodeSum`: a non-object or
    * an empty object fails, with `strictDecoding` a multi-key object is rejected (listing `constructorNames`, the
    * direct constructors like circe); otherwise the first key naming a known constructor wins and superfluous keys are
    * ignored (when no key is known, the last one is reported as unknown). A name is known when it is in `knownNames` or
    * handled by one of the Kindlings-derived `intermediates`.
    */
  def decodeWrapped(
      cursor: HCursor,
      sumName: String,
      strictDecoding: Boolean,
      constructorNames: => List[String],
      knownNames: => List[String],
      intermediates: => List[Decoder[?]]
  ): Decoder.Result[(String, HCursor)] =
    cursor.keys match {
      case None       => Left(wrongTypeFailure("object", cursor))
      case Some(keys) =>
        val iter = keys.iterator
        if (!iter.hasNext) Left(wrongTypeFailure("non-empty json object", cursor))
        else {
          val first = iter.next()
          def at(name: String): Decoder.Result[(String, HCursor)] =
            Right((name, cursor.downField(name).asInstanceOf[HCursor]))
          if (!iter.hasNext) at(first)
          else if (strictDecoding)
            Left(
              DecodingFailure(
                s"Strict decoding $sumName - expected a single key json object with one of: ${constructorNames
                    .mkString(", ")}.",
                cursor.history
              )
            )
          else {
            val knownSet = knownNames.toSet
            val known: String => Boolean = n => knownSet(n) || (intermediateFor(n, intermediates) ne null)
            if (known(first)) at(first)
            else keys.find(known).fold(at(keys.last))(at)
          }
        }
    }

  /** Reads the discriminator; when it is absent and the object is a single-key wrapper naming a known constructor (the
    * form [[addDiscriminator]] writes for children that do not encode to an object), reads it as a wrapper instead.
    */
  def decodeDiscriminatorOrWrapped(
      cursor: HCursor,
      sumName: String,
      field: String,
      knownNames: => Set[String],
      intermediates: => List[Decoder[?]]
  ): Decoder.Result[(String, HCursor)] = {
    val disc = cursor.downField(field)
    if (disc.failed)
      cursor.keys match {
        case Some(keys)
            if keys.size == 1 && (knownNames(keys.head) || (intermediateFor(keys.head, intermediates) ne null)) =>
          Right((keys.head, cursor.downField(keys.head).asInstanceOf[HCursor]))
        case _ => decodeDiscriminator(cursor, sumName, field)
      }
    else decodeDiscriminator(cursor, sumName, field)
  }

  /** Like circe's `ConfiguredDecoder`: the discriminator is read as `Option[String]`; a missing or `null` one fails. */
  def decodeDiscriminator(cursor: HCursor, sumName: String, field: String): Decoder.Result[(String, HCursor)] = {
    val disc = cursor.downField(field)
    Decoder.decodeOption(Decoder.decodeString).tryDecode(disc) match {
      case Right(Some(typeName)) => Right((typeName, cursor))
      case Right(None)           =>
        Left(DecodingFailure(s"$sumName: could not find discriminator field '$field' or its null.", disc.history))
      case Left(failure) => Left(failure)
    }
  }

  def failedToMatchSubtype(sumName: String, typeName: String, cursor: HCursor): DecodingFailure =
    DecodingFailure(s"type $sumName has no class/object/case named '$typeName'.", cursor.history)

  /** `enumAsStrings` enabled (only at runtime) for a sum with non-singleton cases. */
  def enumAsStringsNotSupportedMessage(sumName: String): String =
    s"enumAsStrings is enabled but $sumName has cases that are not singletons; only sums of case objects/parameterless " +
      "enum cases can be encoded as strings (like circe's ConfiguredEnumCodec)"

  def enumAsStringsNotSupported(sumName: String, cursor: HCursor): DecodingFailure =
    DecodingFailure(enumAsStringsNotSupportedMessage(sumName), cursor.history)

  /** circe's `ConfiguredEnumDecoder` failure for an unknown case name. */
  def unknownEnumCase(enumName: String, caseName: String, cursor: HCursor): DecodingFailure =
    DecodingFailure(s"enum $enumName does not contain case: $caseName", cursor.history)

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

  /** circe's `Decoder.keyMissingNone` / `keyMissingNoneAccumulating` (`private[circe]`): the exact instances its
    * `Decoder[Option[A]]` returns for an absent key, which `ConfiguredDecoder` compares by reference to tell an absent
    * key (the default is used) from an explicit `null` (`None` is kept).
    */
  private val absentKey: io.circe.ACursor = Json.obj().hcursor.downField("absent")
  val keyMissingNone: Decoder.Result[Option[Nothing]] =
    Decoder.decodeOption(Decoder.decodeInt).tryDecode(absentKey).asInstanceOf[Decoder.Result[Option[Nothing]]]
  val keyMissingNoneAccumulating: Decoder.AccumulatingResult[Option[Nothing]] =
    Decoder
      .decodeOption(Decoder.decodeInt)
      .tryDecodeAccumulating(absentKey)
      .asInstanceOf[Decoder.AccumulatingResult[Option[Nothing]]]

  /** Decodes a field that has a default value, when `useDefaults` is enabled, exactly like circe's
    * `ConfiguredDecoder.decodeProduct`: the field decoder's `tryDecode` result is kept, unless it is circe's
    * `keyMissingNone` (an absent key decoded by an `Option` decoder) or it is a failure on an absent key or a `null`
    * value; then the default is used. So a user-provided `Decoder[Option[X]]` that decodes an absent key to a value is
    * still consulted. Returns `Decoder.Result[Any]` (see `decodeFieldWithUnsafeDefault`).
    */
  def decodeFieldUsingDefault[A](
      cursor: HCursor,
      fieldName: String,
      decoder: Decoder[A],
      default: Any
  ): Decoder.Result[Any] = {
    val field = cursor.downField(fieldName)
    if (absentKeyOfDerivedNullable(decoder, field)) Right(default)
    else
      decoder.tryDecode(field) match {
        case r @ Right(_) if r.ne(keyMissingNone)                            => r
        case l @ Left(_) if field.succeeded && !field.focus.exists(_.isNull) => l
        case _                                                               => Right(default)
      }
  }

  /** Accumulating variant of `decodeFieldUsingDefault`. */
  def decodeFieldUsingDefaultAccumulating[A](
      cursor: HCursor,
      fieldName: String,
      decoder: Decoder[A],
      default: Any
  ): ValidatedNel[DecodingFailure, Any] = {
    val field = cursor.downField(fieldName)
    if (absentKeyOfDerivedNullable(decoder, field)) Validated.Valid(default)
    else
      decoder.tryDecodeAccumulating(field) match {
        case v @ Validated.Valid(_) if v.ne(keyMissingNoneAccumulating)                   => v
        case i @ Validated.Invalid(_) if field.succeeded && !field.focus.exists(_.isNull) => i
        case _                                                                            => Validated.Valid(default)
      }
  }

  /** Combines a strict-decoding check with the field results in accumulating mode: like circe, the strict failure comes
    * first and the field errors are still reported.
    */
  def withStrictFailureAccumulating[A](
      strict: ValidatedNel[DecodingFailure, Unit],
      decoded: ValidatedNel[DecodingFailure, A]
  ): ValidatedNel[DecodingFailure, A] =
    (strict, decoded) match {
      case (Validated.Invalid(s), Validated.Invalid(d)) => Validated.Invalid(s.concatNel(d))
      case (Validated.Invalid(s), _)                    => Validated.Invalid(s)
      case (_, d)                                       => d
    }

  /** Decodes an Option field that has no default value. When the field is absent from the JSON, returns Right(None)
    * instead of failing. When the field is present but null, also returns Right(None). Otherwise decodes normally. Uses
    * `Any` return type to avoid path-dependent type issues in macros (same pattern as `decodeFieldWithUnsafeDefault`).
    */
  def decodeOptionFieldAsAny[A](
      cursor: HCursor,
      fieldName: String,
      decoder: Decoder[A]
  ): Decoder.Result[Any] =
    // `tryDecode` on the (possibly failed) field cursor: circe's and Kindlings' Option decoders turn an absent key into
    // `None`, and a user-provided `Decoder[Option[X]]` is consulted too (like circe's derived decoders).
    decoder.tryDecode(cursor.downField(fieldName)).asInstanceOf[Decoder.Result[Any]]

  /** Accumulating variant of `decodeOptionFieldAsAny`. */
  def decodeOptionFieldAccumulatingAsAny[A](
      cursor: HCursor,
      fieldName: String,
      decoder: Decoder[A]
  ): ValidatedNel[DecodingFailure, Any] =
    decoder.tryDecodeAccumulating(cursor.downField(fieldName)).map(x => x: Any)

  /** Cast an `Any` value to `A`, using a `Decoder[A]` purely for type inference. The decoder is not called - this is a
    * compile-time trick to avoid path-dependent type aliases in Scala 2 macro-generated code.
    */
  @scala.annotation.nowarn("msg=unused explicit parameter")
  def unsafeCast[A](value: Any, decoder: Decoder[A]): A = value.asInstanceOf[A]

  /** Same failure circe's derived product decoders report for a non-object input (bug #235). */
  def notAnObjectFailure(cursor: HCursor): DecodingFailure =
    DecodingFailure(DecodingFailure.Reason.WrongTypeExpectation("object", cursor.value), cursor.history)

  /** Fails with [[notAnObjectFailure]] as `Either` (failFast) or `ValidatedNel` (accumulating). */
  def notAnObject(cursor: HCursor, failFast: Boolean): Any =
    if (failFast) Left(notAnObjectFailure(cursor)) else Validated.invalidNel(notAnObjectFailure(cursor))

  /** Like circe's `Decoder[Unit]`: accepts only `null`, `{}` and `[]`. */
  def decodeUnit(cursor: HCursor): Either[DecodingFailure, Unit] = {
    val v = cursor.value
    if (v.isNull || v.asObject.exists(_.isEmpty) || v.asArray.exists(_.isEmpty)) Right(())
    else Left(DecodingFailure(DecodingFailure.Reason.WrongTypeExpectation("'null' or '[]' or '{}'", v), cursor.history))
  }

  /** circe's `WrongTypeExpectation` failure (e.g. `"array"`, `"object"`). */
  def wrongTypeFailure(expected: String, cursor: HCursor): DecodingFailure =
    DecodingFailure(DecodingFailure.Reason.WrongTypeExpectation(expected, cursor.value), cursor.history)

  /** Collects element failures inside collection/map decoding loops. */
  final class FailureCollector {
    private[this] var errors: List[DecodingFailure] = Nil

    /** Records the failures of `result` (an `Either` when `failFast`, else a `ValidatedNel`); returns whether it
      * succeeded. Fail-fast throws on the first failure.
      */
    def add(failFast: Boolean, result: Any): Boolean =
      if (failFast) result match {
        case _: Right[?, ?]           => true
        case Left(e: DecodingFailure) => throw new CollectionBuildException(e)
        case _                        => true
      }
      else
        result match {
          case _: Validated.Valid[?]         => errors.isEmpty
          case invalid: Validated.Invalid[?] =>
            errors = invalid.e.asInstanceOf[NonEmptyList[DecodingFailure]].toList reverse_::: errors
            false
          case _ => errors.isEmpty
        }

    def addFailure(failFast: Boolean, failure: DecodingFailure): Unit =
      if (failFast) throw new CollectionBuildException(failure) else errors = failure :: errors

    def throwIfFailed(): Unit =
      if (errors.nonEmpty) throw new CollectionBuildException(NonEmptyList.fromListUnsafe(errors.reverse))
  }

  /** The value of a successful result (`Right` or `Valid`). */
  def resultValue[A](result: Any): A = result match {
    case Right(v)           => v.asInstanceOf[A]
    case Validated.Valid(v) => v.asInstanceOf[A]
    case other              => throw new IllegalStateException(s"Not a successful decoding result: $other")
  }

  /** A case object decoded on its own (outside a sum): an object is required and, with `strictDecoding`, only the
    * discriminator key is accepted (circe's `ConfiguredDecoder.decodeProductBase` for a product without fields).
    */
  def checkSingletonObject(
      cursor: HCursor,
      typeName: String,
      config: hearth.kindlings.circederivation.Configuration
  ): Either[DecodingFailure, Unit] =
    if (!cursor.value.isObject) Left(notAnObjectFailure(cursor))
    else if (config.strictDecoding) checkStrictDecoding(cursor, typeName, config.discriminator.toList)
    else Right(())

  def checkIsObject(cursor: HCursor): Either[DecodingFailure, Unit] =
    if (cursor.value.isObject) Right(())
    else Left(notAnObjectFailure(cursor))

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
        // Decode through the array cursor (like circe's SeqDecoder) so element errors keep their history.
        var c: io.circe.ACursor = if (jsonValues.isEmpty) null else cursor.downArray
        while (c ne null)
          c.as[Item](itemDecoder) match {
            case Right(item) =>
              builder += item
              val next = c.right
              c = if (next.succeeded) next else null
            case Left(err) => return Left(err)
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

  /** circe's `MapDecoder` failure for a key its `KeyDecoder` rejects (the history is added by [[keyFailure]]). */
  val invalidKeyFailure: DecodingFailure = DecodingFailure("Couldn't decode key.", Nil)

  /** A key failure reported at the key's cursor, like circe's `MapDecoder`. */
  def keyFailure(failure: DecodingFailure, keyCursor: HCursor): DecodingFailure =
    DecodingFailure(failure.reason, keyCursor.history)

  def decodeKeyInt(keyStr: String): Either[DecodingFailure, Int] =
    try Right(java.lang.Integer.parseInt(keyStr))
    catch { case _: NumberFormatException => Left(invalidKeyFailure) }

  def decodeKeyLong(keyStr: String): Either[DecodingFailure, Long] =
    try Right(java.lang.Long.parseLong(keyStr))
    catch { case _: NumberFormatException => Left(invalidKeyFailure) }

  def decodeKeyDouble(keyStr: String): Either[DecodingFailure, Double] =
    try Right(java.lang.Double.parseDouble(keyStr))
    catch { case _: NumberFormatException => Left(invalidKeyFailure) }

  def decodeKeyShort(keyStr: String): Either[DecodingFailure, Short] =
    try Right(java.lang.Short.parseShort(keyStr))
    catch { case _: NumberFormatException => Left(invalidKeyFailure) }

  def decodeKeyByte(keyStr: String): Either[DecodingFailure, Byte] =
    try Right(java.lang.Byte.parseByte(keyStr))
    catch { case _: NumberFormatException => Left(invalidKeyFailure) }

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
      typeName: String,
      expectedFields: List[String]
  ): ValidatedNel[DecodingFailure, Unit] =
    Validated.fromEither(checkStrictDecoding(cursor, typeName, expectedFields)).leftMap(NonEmptyList.one)

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

  /** `KindlingsDecoder.patch`: every field is decoded as `Option[V]` (circe's `Decoder.decodeOption`, so an absent or
    * `null` key yields `None` and keeps the original value, and a non-object patch fails with circe's "missing field"),
    * then `patch` rebuilds the value from the decoded options and the original.
    */
  def patchDecoder[A](
      names: List[String],
      decoders: List[Decoder[Any]],
      patch: (Array[Any], A) => A
  ): Decoder[A => A] = new Decoder[A => A] {
    private[this] val fieldNames: Array[String] = names.toArray
    private[this] val optionDecoders: Array[Decoder[Option[Any]]] =
      decoders.map(d => Decoder.decodeOption[Any](d)).toArray

    def apply(c: HCursor): Decoder.Result[A => A] = {
      val values = new Array[Any](fieldNames.length)
      var i = 0
      while (i < fieldNames.length) {
        optionDecoders(i).tryDecode(c.downField(fieldNames(i))) match {
          case Right(v)  => values(i) = v
          case Left(err) => return Left(err)
        }
        i += 1
      }
      Right((original: A) => patch(values, original))
    }

    override def decodeAccumulating(c: HCursor): Decoder.AccumulatingResult[A => A] = {
      val values = new Array[Any](fieldNames.length)
      val errors = List.newBuilder[DecodingFailure]
      var failed = false
      var i = 0
      while (i < fieldNames.length) {
        optionDecoders(i).tryDecodeAccumulating(c.downField(fieldNames(i))) match {
          case Validated.Valid(v)    => values(i) = v
          case Validated.Invalid(es) => failed = true; errors ++= es.toList
        }
        i += 1
      }
      if (failed) Validated.Invalid(NonEmptyList.fromListUnsafe(errors.result()))
      else Validated.Valid((original: A) => patch(values, original))
    }
  }

  /** `KindlingsDecoder.incomplete`: decodes the fields not supplied by the function's arguments (each with its own
    * decoder's `tryDecode`, so like circe an absent `Option` field is `None`), then returns a function of `arity`
    * arguments building the value from the decoded fields and the arguments.
    */
  /** The value of a `Labelled` argument of an incomplete decoder's function. */
  def labelledValue(labelled: Any): Any = labelled.asInstanceOf[hearth.kindlings.circederivation.Labelled[?, ?]].value

  def incompleteDecoder[F](
      names: List[String],
      decoders: List[Decoder[Any]],
      arity: Int,
      build: (Array[Any], Array[Any]) => Any
  ): Decoder[F] = new Decoder[F] {
    private[this] val fieldNames: Array[String] = names.toArray
    private[this] val fieldDecoders: Array[Decoder[Any]] = decoders.toArray

    private def function(values: Array[Any]): F =
      IncompleteFunctions.fromArray(arity, args => build(values, args)).asInstanceOf[F]

    def apply(c: HCursor): Decoder.Result[F] = {
      val values = new Array[Any](fieldNames.length)
      var i = 0
      while (i < fieldNames.length) {
        fieldDecoders(i).tryDecode(c.downField(fieldNames(i))) match {
          case Right(v)  => values(i) = v
          case Left(err) => return Left(err)
        }
        i += 1
      }
      Right(function(values))
    }

    override def decodeAccumulating(c: HCursor): Decoder.AccumulatingResult[F] = {
      val values = new Array[Any](fieldNames.length)
      val errors = List.newBuilder[DecodingFailure]
      var failed = false
      var i = 0
      while (i < fieldNames.length) {
        fieldDecoders(i).tryDecodeAccumulating(c.downField(fieldNames(i))) match {
          case Validated.Valid(v)    => values(i) = v
          case Validated.Invalid(es) => failed = true; errors ++= es.toList
        }
        i += 1
      }
      if (failed) Validated.Invalid(NonEmptyList.fromListUnsafe(errors.result()))
      else Validated.Valid(function(values))
    }
  }

  /** A field decoder that, when `enabled` (`useDefaults`), falls back to `default` like `decodeFieldUsingDefault`. */
  def defaultingDecoder[A](decoder: Decoder[A], default: Any, enabled: Boolean): Decoder[Any] =
    if (!enabled) decoder.asInstanceOf[Decoder[Any]]
    else
      new Decoder[Any] {
        def apply(c: HCursor): Decoder.Result[Any] = tryDecode(c)
        override def decodeAccumulating(c: HCursor): Decoder.AccumulatingResult[Any] = tryDecodeAccumulating(c)
        override def tryDecode(c: io.circe.ACursor): Decoder.Result[Any] =
          if (absentKeyOfDerivedNullable(decoder, c)) Right(default)
          else
            decoder.tryDecode(c) match {
              case r @ Right(_) if r.ne(keyMissingNone)                    => r
              case l @ Left(_) if c.succeeded && !c.focus.exists(_.isNull) => l
              case _                                                       => Right(default)
            }
        override def tryDecodeAccumulating(c: io.circe.ACursor): Decoder.AccumulatingResult[Any] =
          if (absentKeyOfDerivedNullable(decoder, c)) Validated.Valid(default)
          else
            decoder.tryDecodeAccumulating(c) match {
              case v @ Validated.Valid(_) if v.ne(keyMissingNoneAccumulating)           => v
              case i @ Validated.Invalid(_) if c.succeeded && !c.focus.exists(_.isNull) => i
              case _                                                                    => Validated.Valid(default)
            }
      }

  /** An unchecked cast with an explicit type argument (avoids path-dependent type aliases in Scala 2 macro code). */
  def castTo[A](value: Any): A = value.asInstanceOf[A]

  /** The patched value of a field: the decoded `Some(v)`, or the original value for `None`. */
  def patchValue[V](patched: Any, original: V): V = patched match {
    case Some(v) => v.asInstanceOf[V]
    case _       => original
  }

  // --- Codec.AsObject combiner ---

  def codecAsObject[A](
      enc: Encoder.AsObject[A],
      dec: Decoder[A]
  ): KindlingsCodecAsObject[A] = dec match {
    // Keep the sum markers (both instances were derived for the same type, so both are sums or neither is).
    case sumDec: KindlingsSumDecoder if enc.isInstanceOf[KindlingsSumEncoder] =>
      new KindlingsCodecAsObject[A] with KindlingsSumEncoder with KindlingsSumDecoder {
        override def encodeObject(a: A): JsonObject = enc.encodeObject(a)
        override def apply(c: HCursor): Decoder.Result[A] = dec(c)
        override def decodeAccumulating(c: HCursor): Decoder.AccumulatingResult[A] = dec.decodeAccumulating(c)
        def kindlingsHandlesConstructor(constructorName: String): Boolean =
          sumDec.kindlingsHandlesConstructor(constructorName)
      }
    case extras: ExtrasDecoder[A @unchecked] =>
      // generic-extras' ConfiguredAsObjectCodec delegates isStrict/decodeStrict to its decoder
      new KindlingsCodecAsObject[A] {
        override def encodeObject(a: A): JsonObject = enc.encodeObject(a)
        override def apply(c: HCursor): Decoder.Result[A] = dec(c)
        override def decodeAccumulating(c: HCursor): Decoder.AccumulatingResult[A] = dec.decodeAccumulating(c)
        override def isStrict: Boolean = extras.isStrict
        override def decodeStrict(c: HCursor): ExtrasDecoder.StrictResult[A] = extras.decodeStrict(c)
      }
    case _ =>
      new KindlingsCodecAsObject[A] {
        override def encodeObject(a: A): JsonObject = enc.encodeObject(a)
        override def apply(c: HCursor): Decoder.Result[A] = dec(c)
        override def decodeAccumulating(c: HCursor): Decoder.AccumulatingResult[A] = dec.decodeAccumulating(c)
      }
  }

  // --- Kindlings-derived intermediate sums (see KindlingsSumEncoder / KindlingsSumDecoder) ---

  /** Whether an intermediate's encoder was derived by Kindlings for a sum: its output is used as it is. */
  def isKindlingsSumEncoder(encoder: Encoder[?]): Boolean = encoder.isInstanceOf[KindlingsSumEncoder]

  /** The first of `intermediates` that is a Kindlings-derived sum decoder handling `constructorName`, or `null`. */
  def intermediateFor(constructorName: String, intermediates: List[Decoder[?]]): Decoder[Any] = {
    var rest = intermediates
    while (rest.nonEmpty) {
      rest.head match {
        case d: KindlingsSumDecoder if d.kindlingsHandlesConstructor(constructorName) =>
          return d.asInstanceOf[Decoder[Any]]
        case _ =>
      }
      rest = rest.tail
    }
    null
  }

  /** [[KindlingsSumDecoder.kindlingsHandlesConstructor]] of a derived sum: one of its own (flattened) constructor
    * names, or a name handled by one of its Kindlings-derived intermediates.
    */
  def sumHandles(
      constructorName: String,
      transformConstructorNames: String => String,
      names: List[String],
      intermediates: => List[Decoder[?]]
  ): Boolean =
    names.exists(n => transformConstructorNames(n) == constructorName) ||
      (intermediateFor(constructorName, intermediates) ne null)

  // --- Helpers of Kindlings 0.3.x, kept for binary compatibility of code compiled against it (with their 0.3.2
  // behaviour); the macros no longer emit them ---

  final private val Compat03 =
    "kept for binary compatibility of code compiled against Kindlings 0.3.x; the macro no longer emits it"

  @deprecated(Compat03, "0.3.4")
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

  @deprecated(Compat03, "0.3.4")
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

  @deprecated(Compat03, "0.3.4")
  def checkStrictDecodingWithDiscriminator(
      cursor: HCursor,
      expectedFields: Set[String],
      discriminatorField: String
  ): Decoder.Result[Unit] =
    checkStrictDecoding(cursor, expectedFields + discriminatorField)

  @deprecated(Compat03, "0.3.4")
  def checkStrictDecodingAccumulating(
      cursor: HCursor,
      expectedFields: Set[String]
  ): ValidatedNel[DecodingFailure, Unit] =
    Validated.fromEither(checkStrictDecoding(cursor, expectedFields)).leftMap(NonEmptyList.one)

  @deprecated(Compat03, "0.3.4")
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

  @deprecated(Compat03, "0.3.4")
  def decodeDiscriminator(cursor: HCursor, field: String): Decoder.Result[(String, HCursor)] =
    cursor.downField(field).as[String] match {
      case Right(typeName) => Right((typeName, cursor))
      case Left(failure)   => Left(failure)
    }

  @deprecated(Compat03, "0.3.4")
  def failedToMatchSubtype(typeName: String, cursor: HCursor, knownSubtypes: List[String]): DecodingFailure =
    DecodingFailure(
      s"Unknown type discriminator: $typeName. Expected one of: ${knownSubtypes.mkString(", ")}",
      cursor.history
    )

  /** The 0.3.x `KindlingsDecoder.patch`: the patch JSON is deep-merged into the encoded original, which is decoded back
    * (keeping the original when that fails).
    */
  @deprecated(Compat03, "0.3.4")
  def patchDecoder[A](decoder: Decoder[A], encoder: Encoder[A]): Decoder[A => A] =
    new Decoder[A => A] {
      def apply(c: HCursor): Decoder.Result[A => A] = {
        val patchJson = c.value
        if (patchJson.isNull || patchJson.asObject.exists(_.isEmpty)) Right(identity)
        else
          Right { (original: A) =>
            val originalJson = encoder(original)
            val merged = deepMergeCompat03(originalJson, patchJson)
            decoder.decodeJson(merged).getOrElse(original)
          }
      }
    }

  private def deepMergeCompat03(base: Json, patch: Json): Json =
    (base.asObject, patch.asObject) match {
      case (Some(baseObj), Some(patchObj)) =>
        val merged = baseObj.toMap ++ patchObj.toMap.map { case (k, v) =>
          k -> baseObj(k).map(deepMergeCompat03(_, v)).getOrElse(v)
        }
        Json.fromJsonObject(JsonObject.fromMap(merged))
      case _ => patch
    }
}
