package hearth.kindlings.jsoniterderivation.internal.compiletime

import hearth.MacroCommons
import hearth.fp.data.NonEmptyVector
import hearth.fp.effect.*
import hearth.std.*

import hearth.kindlings.jsoniterderivation.{annotations, JsoniterConfig, KindlingsJsonCodec, KindlingsJsonValueCodec}
import com.github.plokhotnyuk.jsoniter_scala.core.{
  readFromString,
  writeToString,
  JsonCodec,
  JsonKeyCodec,
  JsonReader,
  JsonReaderException,
  JsonValueCodec,
  JsonWriter
}

trait CodecMacrosImpl
    extends hearth.kindlings.derivation.compiletime.DerivationTimeout
    with hearth.kindlings.derivation.compiletime.MethodFolds
    with rules.EncoderUseCachedDefWhenAvailableRuleImpl
    with rules.EncoderUseImplicitWhenAvailableRuleImpl
    with rules.EncoderHandleAsLiteralTypeRuleImpl
    with rules.EncoderHandleAsBuiltInRuleImpl
    with rules.EncoderHandleAsValueTypeRuleImpl
    with rules.EncoderHandleAsOptionRuleImpl
    with rules.EncoderHandleAsMapRuleImpl
    with rules.EncoderHandleAsCollectionRuleImpl
    with rules.EncoderHandleAsNamedTupleRuleImpl
    with rules.EncoderHandleAsOneValueClassRuleImpl
    with rules.EncoderHandleAsSingletonRuleImpl
    with rules.EncoderHandleAsCaseClassRuleImpl
    with rules.EncoderHandleAsEnumRuleImpl
    with rules.DecoderUseCachedDefWhenAvailableRuleImpl
    with rules.DecoderUseImplicitWhenAvailableRuleImpl
    with rules.DecoderHandleAsLiteralTypeRuleImpl
    with rules.DecoderHandleAsBuiltInRuleImpl
    with rules.DecoderHandleAsValueTypeRuleImpl
    with rules.DecoderHandleAsOptionRuleImpl
    with rules.DecoderHandleAsMapRuleImpl
    with rules.DecoderHandleAsCollectionRuleImpl
    with rules.DecoderHandleAsNamedTupleRuleImpl
    with rules.DecoderHandleAsOneValueClassRuleImpl
    with rules.DecoderHandleAsSingletonRuleImpl
    with rules.DecoderHandleAsCaseClassRuleImpl
    with rules.DecoderHandleAsEnumRuleImpl { this: MacroCommons & StdExtensions & AnnotationSupport =>

  override protected def derivationSettingsNamespace: String = "jsoniterDerivation"

  // Shared type representations — centralized here so all rules access them consistently.
  private[compiletime] object CTypes {

    def JsonCodec: Type.Ctor1[JsonCodec] = Type.Ctor1.of[JsonCodec]
    def JsonKeyCodec: Type.Ctor1[JsonKeyCodec] = Type.Ctor1.of[JsonKeyCodec]
    def JsonValueCodec: Type.Ctor1[JsonValueCodec] = Type.Ctor1.of[JsonValueCodec]
    def KindlingsJsonCodec: Type.Ctor1[KindlingsJsonCodec] = Type.Ctor1.of[KindlingsJsonCodec]
    def KindlingsJsonValueCodec: Type.Ctor1[KindlingsJsonValueCodec] =
      Type.Ctor1.of[KindlingsJsonValueCodec]
    val CodecLogDerivation: Type[hearth.kindlings.jsoniterderivation.KindlingsJsonValueCodec.LogDerivation] =
      Type.of[hearth.kindlings.jsoniterderivation.KindlingsJsonValueCodec.LogDerivation]
    val JsoniterConfig: Type[JsoniterConfig] = Type.of[JsoniterConfig]
    val JsonReader: Type[JsonReader] = Type.of[JsonReader]
    val JsonWriter: Type[JsonWriter] = Type.of[JsonWriter]
    val String: Type[String] = Type.of[String]
    val Unit: Type[Unit] = Type.of[Unit]
    val Any: Type[Any] = Type.of[Any]
    val ArrayAny: Type[Array[Any]] = Type.of[Array[Any]]
    val ListString: Type[List[String]] = Type.of[List[String]]
    val JsonReaderException: Type[JsonReaderException] = Type.of[JsonReaderException]
    def EitherJsonReaderException[A: Type]: Type[Either[JsonReaderException, A]] =
      Type.of[Either[JsonReaderException, A]]
    val FieldName: Type[annotations.fieldName] = Type.of[annotations.fieldName]
    val TransientField: Type[annotations.transientField] = Type.of[annotations.transientField]
    val Stringified: Type[annotations.stringified] = Type.of[annotations.stringified]
    val Int: Type[Int] = Type.of[Int]
    val Long: Type[Long] = Type.of[Long]
    val Double: Type[Double] = Type.of[Double]
    val Float: Type[Float] = Type.of[Float]
    val Short: Type[Short] = Type.of[Short]
    val Byte: Type[Byte] = Type.of[Byte]
    val Boolean: Type[Boolean] = Type.of[Boolean]
    val BigDecimal: Type[BigDecimal] = Type.of[BigDecimal]
    val BigInt: Type[BigInt] = Type.of[BigInt]
    val Product: Type[Product] = Type.of[Product]
    val Instant: Type[java.time.Instant] = Type.of[java.time.Instant]
    val LocalDate: Type[java.time.LocalDate] = Type.of[java.time.LocalDate]
    val LocalTime: Type[java.time.LocalTime] = Type.of[java.time.LocalTime]
    val LocalDateTime: Type[java.time.LocalDateTime] = Type.of[java.time.LocalDateTime]
    val OffsetDateTime: Type[java.time.OffsetDateTime] = Type.of[java.time.OffsetDateTime]
    val ZonedDateTime: Type[java.time.ZonedDateTime] = Type.of[java.time.ZonedDateTime]
    val Duration: Type[java.time.Duration] = Type.of[java.time.Duration]
    val MonthDay: Type[java.time.MonthDay] = Type.of[java.time.MonthDay]
    val OffsetTime: Type[java.time.OffsetTime] = Type.of[java.time.OffsetTime]
    val Period: Type[java.time.Period] = Type.of[java.time.Period]
    val Year: Type[java.time.Year] = Type.of[java.time.Year]
    val YearMonth: Type[java.time.YearMonth] = Type.of[java.time.YearMonth]
    val ZoneId: Type[java.time.ZoneId] = Type.of[java.time.ZoneId]
    val ZoneOffset: Type[java.time.ZoneOffset] = Type.of[java.time.ZoneOffset]
    val UUID: Type[java.util.UUID] = Type.of[java.util.UUID]
    // Java boxed primitives
    val JavaByte: Type[java.lang.Byte] = Type.of[java.lang.Byte]
    val JavaShort: Type[java.lang.Short] = Type.of[java.lang.Short]
    val JavaInteger: Type[java.lang.Integer] = Type.of[java.lang.Integer]
    val JavaLong: Type[java.lang.Long] = Type.of[java.lang.Long]
    val JavaFloat: Type[java.lang.Float] = Type.of[java.lang.Float]
    val JavaDouble: Type[java.lang.Double] = Type.of[java.lang.Double]
    val JavaBoolean: Type[java.lang.Boolean] = Type.of[java.lang.Boolean]
    val JavaCharacter: Type[java.lang.Character] = Type.of[java.lang.Character]
    // BitSet types
    val ImmutableBitSet: Type[scala.collection.immutable.BitSet] = Type.of[scala.collection.immutable.BitSet]
    val MutableBitSet: Type[scala.collection.mutable.BitSet] = Type.of[scala.collection.mutable.BitSet]
    val CollectionBitSet: Type[scala.collection.BitSet] = Type.of[scala.collection.BitSet]
  }

  // Entrypoints

  /** Derive a combined JsonValueCodec for type A.
    *
    * To avoid Scala 3 cross-splice staging issues, all derivation (encode, decode, nullValue) is performed in a single
    * MIO.scoped/runSafe call using LambdaBuilder. The resulting function expressions are then spliced into the final
    * Expr.quote without creating new expressions inside sibling splices.
    */
  @scala.annotation.nowarn("msg=is never used")
  def deriveCodecTypeClass[A: Type](configExpr: Expr[JsoniterConfig]): Expr[KindlingsJsonValueCodec[A]] = {
    implicit val CodecA: Type[JsonValueCodec[A]] = CTypes.JsonValueCodec[A]
    implicit val KindlingsCodecA: Type[KindlingsJsonValueCodec[A]] = CTypes.KindlingsJsonValueCodec[A]
    implicit val ConfigT: Type[JsoniterConfig] = CTypes.JsoniterConfig
    implicit val JsonReaderT: Type[JsonReader] = CTypes.JsonReader
    implicit val JsonWriterT: Type[JsonWriter] = CTypes.JsonWriter
    implicit val UnitT: Type[Unit] = CTypes.Unit

    deriveCodecFromCtxAndAdaptForEntrypoint[A, KindlingsJsonValueCodec[A]](
      "KindlingsJsonValueCodec.derived",
      configExpr
    ) { case (encodeFn, decodeFn, nullValueExpr) =>
      Expr.quote {
        hearth.kindlings.jsoniterderivation.internal.runtime.JsoniterDerivationFactories.codecInstance[A](
          Expr.splice(nullValueExpr),
          (in: JsonReader, default: A) => {
            val _ = default
            if (Expr.splice(configExpr).encodingOnly)
              throw new UnsupportedOperationException("encoding-only codec cannot decode")
            Expr.splice(decodeFn(Expr.quote(in), configExpr))
          },
          (x: A, out: JsonWriter) =>
            if (Expr.splice(configExpr).decodingOnly)
              throw new UnsupportedOperationException("decoding-only codec cannot encode")
            else
              Expr.splice(encodeFn(Expr.quote(x), Expr.quote(out), configExpr))
        )
      }
    }
  }

  // Inline encode/decode entrypoints

  /** Derive an inline writeToString expression for type A.
    *
    * Checks for a user-provided implicit JsonValueCodec[A] first. If found, uses it directly. Otherwise derives only
    * the encoder and creates a stub codec.
    */
  @scala.annotation.nowarn("msg=is never used")
  def deriveInlineWriteToString[A: Type](valueExpr: Expr[A], configExpr: Expr[JsoniterConfig]): Expr[String] = {
    implicit val StringT: Type[String] = CTypes.String
    implicit val CodecA: Type[JsonValueCodec[A]] = CTypes.JsonValueCodec[A]
    implicit val ConfigT: Type[JsoniterConfig] = CTypes.JsoniterConfig
    implicit val JsonReaderT: Type[JsonReader] = CTypes.JsonReader
    implicit val JsonWriterT: Type[JsonWriter] = CTypes.JsonWriter
    implicit val UnitT: Type[Unit] = CTypes.Unit

    if (Type[A] =:= Type.of[Nothing].asInstanceOf[Type[A]] || Type[A] =:= Type.of[Any].asInstanceOf[Type[A]])
      Environment.reportErrorAndAbort(
        s"KindlingsJsonValueCodec.writeToString: type parameter was inferred as ${Type[A].prettyPrint}, which is likely unintended.\n" +
          "Provide an explicit type parameter, e.g.: KindlingsJsonValueCodec.writeToString[MyType](...)\n" +
          "or add a type ascription to the result variable."
      )

    summonJsonValueCodecCached[A] match {
      case Right(codecExpr) =>
        Expr.quote {
          writeToString[A](Expr.splice(valueExpr))(Expr.splice(codecExpr))
        }
      case Left(_) =>
        deriveEncoderOnlyFromCtxAndAdaptForEntrypoint[A, String]("KindlingsJsonValueCodec.writeToString") { encodeFn =>
          Expr.quote {
            writeToString[A](Expr.splice(valueExpr))(new JsonValueCodec[A] {
              def nullValue: A = null.asInstanceOf[A]
              def decodeValue(in: JsonReader, default: A): A =
                throw new UnsupportedOperationException("encode-only codec")
              def encodeValue(x: A, out: JsonWriter): Unit =
                Expr.splice(encodeFn(Expr.quote(x), Expr.quote(out), configExpr))
            })
          }
        }
    }
  }

  /** Derive an inline readFromString expression for type A.
    *
    * Checks for a user-provided implicit JsonValueCodec[A] first. If found, uses it directly. Otherwise derives only
    * the decoder and nullValue, creating a stub codec.
    */
  @scala.annotation.nowarn("msg=is never used")
  def deriveInlineReadFromString[A: Type](
      jsonExpr: Expr[String],
      configExpr: Expr[JsoniterConfig]
  ): Expr[Either[JsonReaderException, A]] = {
    implicit val StringT: Type[String] = CTypes.String
    implicit val CodecA: Type[JsonValueCodec[A]] = CTypes.JsonValueCodec[A]
    implicit val ConfigT: Type[JsoniterConfig] = CTypes.JsoniterConfig
    implicit val JsonReaderT: Type[JsonReader] = CTypes.JsonReader
    implicit val JsonWriterT: Type[JsonWriter] = CTypes.JsonWriter
    implicit val UnitT: Type[Unit] = CTypes.Unit
    implicit val EitherT: Type[Either[JsonReaderException, A]] = CTypes.EitherJsonReaderException[A]
    implicit val JsonReaderExceptionT: Type[JsonReaderException] = CTypes.JsonReaderException

    if (Type[A] =:= Type.of[Nothing].asInstanceOf[Type[A]] || Type[A] =:= Type.of[Any].asInstanceOf[Type[A]])
      Environment.reportErrorAndAbort(
        s"KindlingsJsonValueCodec.readFromString: type parameter was inferred as ${Type[A].prettyPrint}, which is likely unintended.\n" +
          "Provide an explicit type parameter, e.g.: KindlingsJsonValueCodec.readFromString[MyType](...)\n" +
          "or add a type ascription to the result variable."
      )

    summonJsonValueCodecCached[A] match {
      case Right(codecExpr) =>
        Expr.quote {
          try Right(readFromString[A](Expr.splice(jsonExpr))(Expr.splice(codecExpr)))
          catch { case e: JsonReaderException => Left(e) }
        }
      case Left(_) =>
        deriveDecoderOnlyFromCtxAndAdaptForEntrypoint[A, Either[JsonReaderException, A]](
          "KindlingsJsonValueCodec.readFromString"
        ) { case (decodeFn, nullValueExpr) =>
          Expr.quote {
            try
              Right(readFromString[A](Expr.splice(jsonExpr))(new JsonValueCodec[A] {
                def nullValue: A = Expr.splice(nullValueExpr)
                def decodeValue(in: JsonReader, default: A): A = {
                  val _ = default
                  Expr.splice(decodeFn(Expr.quote(in), configExpr))
                }
                def encodeValue(x: A, out: JsonWriter): Unit =
                  throw new UnsupportedOperationException("decode-only codec")
              }))
            catch { case e: JsonReaderException => Left(e) }
          }
        }
    }
  }

  /** Derive a standalone JsonKeyCodec for type A.
    *
    * Uses the existing deriveKeyEncoding/deriveKeyDecoding methods with minimal stub contexts. Only succeeds for types
    * with a natural key representation: built-in primitives, value types wrapping those, and sealed traits/enums of
    * case objects.
    */
  @scala.annotation.nowarn("msg=is never used")
  def deriveKeyCodecTypeClass[A: Type](configExpr: Expr[JsoniterConfig]): Expr[JsonKeyCodec[A]] = {
    implicit val JsonKeyCodecA: Type[JsonKeyCodec[A]] = CTypes.JsonKeyCodec[A]
    implicit val ConfigT: Type[JsoniterConfig] = CTypes.JsoniterConfig
    implicit val JsonReaderT: Type[JsonReader] = CTypes.JsonReader
    implicit val JsonWriterT: Type[JsonWriter] = CTypes.JsonWriter
    implicit val UnitT: Type[Unit] = CTypes.Unit

    if (Type[A] =:= Type.of[Nothing].asInstanceOf[Type[A]] || Type[A] =:= Type.of[Any].asInstanceOf[Type[A]])
      Environment.reportErrorAndAbort(
        s"KindlingsJsonCodec.deriveKeyCodec: type parameter was inferred as ${Type[A].prettyPrint}, which is likely unintended.\n" +
          "Provide an explicit type parameter, e.g.: KindlingsJsonCodec.deriveKeyCodec[MyType](...)\n" +
          "or add a type ascription to the result variable."
      )

    Log
      .namedScope(
        s"Deriving key codec for ${Type[A].prettyPrint} at: ${Environment.currentPosition.prettyPrint}"
      ) {
        MIO.scoped { runSafe =>
          val cache = ValDefsCache.mlocal
          val selfType: Option[??] = None

          // Derive key encoding using a stub EncoderCtx
          val keyEncMIO: MIO[Option[Expr[(A, JsonWriter) => Unit]]] = {
            val stubValue = Expr.quote(null.asInstanceOf[A])
            val stubWriter = Expr.quote(null.asInstanceOf[JsonWriter])
            implicit val ctx: EncoderCtx[A] = EncoderCtx.from(stubValue, stubWriter, configExpr, cache, selfType)
            EncoderHandleAsMapRule.deriveKeyEncoding[A]
          }

          // Derive key decoding using a stub DecoderCtx
          val keyDecMIO: MIO[Option[Expr[JsonReader => A]]] = {
            implicit val StringT: Type[String] = CTypes.String
            val stubReader = Expr.quote(null.asInstanceOf[JsonReader])
            implicit val ctx: DecoderCtx[A] = DecoderCtx.from(stubReader, configExpr, cache, selfType)
            DecoderHandleAsMapRule.deriveKeyDecoding[A]
          }

          val (keyEncOpt, keyDecOpt) = runSafe {
            for {
              _ <- ensureStandardExtensionsLoaded()
              result <- keyEncMIO.parTuple(keyDecMIO)
            } yield result
          }

          val (encFn, decFn) = (keyEncOpt, keyDecOpt) match {
            case (Some(enc), Some(dec)) => (enc, dec)
            case _                      =>
              Environment.reportErrorAndAbort(
                s"KindlingsJsonCodec.deriveKeyCodec: Cannot derive JsonKeyCodec for ${Type[A].prettyPrint}.\n" +
                  "Key codecs can only be derived for: primitive types (Int, Long, Double, Float, Short, Boolean, BigDecimal, BigInt),\n" +
                  "value types wrapping those primitives, and sealed traits/enums of case objects."
              )
          }

          val vals = runSafe(cache.get)
          val resultExpr = Expr.quote {
            new JsonKeyCodec[A] {
              def decodeKey(in: JsonReader): A = Expr.splice(decFn).apply(Expr.splice(Expr.quote(in)))
              def encodeKey(x: A, out: JsonWriter): Unit =
                Expr.splice(encFn).apply(Expr.splice(Expr.quote(x)), Expr.splice(Expr.quote(out)))
            }
          }
          vals.toValDefs.use(_ => resultExpr)
        }
      }
      .flatTap { result =>
        Log.info(s"Derived final key codec result: ${result.prettyPrint}")
      }
      .runToExprOrFail(
        "KindlingsJsonCodec.deriveKeyCodec",
        infoRendering = if (shouldWeLogCodecDerivation) RenderFrom(Log.Level.Info) else DontRender,
        errorRendering = if (shouldWeLogCodecDerivation) RenderFrom(Log.Level.Info) else DontRender,
        timeout = derivationTimeout
      )(renderDerivationErrorMessage)
  }

  /** Derive a combined JsonCodec (value + key) for type A. Used only by Scala 2 macro bridge. Scala 3 avoids splice
    * isolation by calling separate inline methods.
    */
  @scala.annotation.nowarn("msg=is never used")
  def deriveJsonCodecTypeClass[A: Type](configExpr: Expr[JsoniterConfig]): Expr[KindlingsJsonCodec[A]] = {
    implicit val ConfigT: Type[JsoniterConfig] = CTypes.JsoniterConfig
    implicit val JsonReaderT: Type[JsonReader] = CTypes.JsonReader
    implicit val JsonWriterT: Type[JsonWriter] = CTypes.JsonWriter
    implicit val UnitT: Type[Unit] = CTypes.Unit
    implicit val JsonValueCodecA: Type[JsonValueCodec[A]] = CTypes.JsonValueCodec[A]
    implicit val JsonKeyCodecA: Type[JsonKeyCodec[A]] = CTypes.JsonKeyCodec[A]
    implicit val KindlingsJsonCodecA: Type[KindlingsJsonCodec[A]] = CTypes.KindlingsJsonCodec[A]

    if (Type[A] =:= Type.of[Nothing].asInstanceOf[Type[A]] || Type[A] =:= Type.of[Any].asInstanceOf[Type[A]])
      Environment.reportErrorAndAbort(
        s"KindlingsJsonCodec.derived: type parameter was inferred as ${Type[A].prettyPrint}, which is likely unintended.\n" +
          "Provide an explicit type parameter, e.g.: KindlingsJsonCodec.derived[MyType](...)\n" +
          "or add a type ascription to the result variable."
      )

    Log
      .namedScope(
        s"Deriving combined json codec for ${Type[A].prettyPrint} at: ${Environment.currentPosition.prettyPrint}"
      ) {
        MIO.scoped { runSafe =>
          val cache = ValDefsCache.mlocal
          val selfType: Option[??] = Some(Type[A].as_??)
          val evConfig: Option[JsoniterConfig] = configExpr.semiEval.toOption

          // Validate config for contradictory flag combinations at compile time
          validateConfigOrAbort(evConfig, "KindlingsJsonCodec.derived")

          // Value codec parts — same as deriveCodecTypeClass
          val encMIO: MIO[(Expr[A], Expr[JsonWriter], Expr[JsoniterConfig]) => Expr[Unit]] = {
            val defBuilder =
              ValDefBuilder.ofDef3[A, JsonWriter, JsoniterConfig, Unit](s"codec_encode_${Type[A].shortName}")
            for {
              _ <- Log.info(s"Forward-declaring json codec encode body for ${Type[A].prettyPrint}")
              _ <- cache.forwardDeclare("codec-encode-body", defBuilder)
              _ <- MIO.scoped { rs =>
                rs(cache.buildCachedWith("codec-encode-body", defBuilder) { case (_, (v, w, c)) =>
                  rs(deriveEncoderRecursively[A](using EncoderCtx.from(v, w, c, cache, selfType, evConfig)))
                })
              }
              _ <- Log.info(s"Defined json codec encode body for ${Type[A].prettyPrint}")
              fn <- cache.get3Ary[A, JsonWriter, JsoniterConfig, Unit]("codec-encode-body")
            } yield fn.get
          }

          val decMIO: MIO[(Expr[JsonReader], Expr[JsoniterConfig]) => Expr[A]] = {
            val defBuilder =
              ValDefBuilder.ofDef2[JsonReader, JsoniterConfig, A](s"codec_decode_${Type[A].shortName}")
            for {
              _ <- Log.info(s"Forward-declaring json codec decode body for ${Type[A].prettyPrint}")
              _ <- cache.forwardDeclare("codec-decode-body", defBuilder)
              _ <- MIO.scoped { rs =>
                rs(cache.buildCachedWith("codec-decode-body", defBuilder) { case (_, (r, c)) =>
                  rs(deriveDecoderRecursively[A](using DecoderCtx.from(r, c, cache, selfType, evConfig)))
                })
              }
              _ <- Log.info(s"Defined json codec decode body for ${Type[A].prettyPrint}")
              fn <- cache.get2Ary[JsonReader, JsoniterConfig, A]("codec-decode-body")
            } yield fn.get
          }

          val nullMIO: MIO[Expr[A]] = deriveNullValue[A]

          // Key codec parts
          val keyEncMIO: MIO[Option[Expr[(A, JsonWriter) => Unit]]] = {
            val stubValue = Expr.quote(null.asInstanceOf[A])
            val stubWriter = Expr.quote(null.asInstanceOf[JsonWriter])
            implicit val ctx: EncoderCtx[A] =
              EncoderCtx.from(stubValue, stubWriter, configExpr, cache, selfType, evConfig)
            EncoderHandleAsMapRule.deriveKeyEncoding[A]
          }

          val keyDecMIO: MIO[Option[Expr[JsonReader => A]]] = {
            implicit val StringT: Type[String] = CTypes.String
            val stubReader = Expr.quote(null.asInstanceOf[JsonReader])
            implicit val ctx: DecoderCtx[A] = DecoderCtx.from(stubReader, configExpr, cache, selfType, evConfig)
            DecoderHandleAsMapRule.deriveKeyDecoding[A]
          }

          val (((encFn, decFn), nullVal), (keyEncOpt, keyDecOpt)) = runSafe {
            for {
              _ <- ensureStandardExtensionsLoaded()
              result <- encMIO.parTuple(decMIO).parTuple(nullMIO).parTuple(keyEncMIO.parTuple(keyDecMIO))
            } yield result
          }

          val (keyEncFn, keyDecFn) = (keyEncOpt, keyDecOpt) match {
            case (Some(enc), Some(dec)) => (enc, dec)
            case _                      =>
              Environment.reportErrorAndAbort(
                s"KindlingsJsonCodec.derived: Cannot derive JsonKeyCodec for ${Type[A].prettyPrint}.\n" +
                  "Key codecs can only be derived for: primitive types (Int, Long, Double, Float, Short, Boolean, BigDecimal, BigInt),\n" +
                  "value types wrapping those primitives, and sealed traits/enums of case objects.\n" +
                  "If you only need value encoding/decoding, use KindlingsJsonValueCodec.derived instead."
              )
          }

          val vals = runSafe(cache.get)
          val resultExpr = Expr.quote {
            hearth.kindlings.jsoniterderivation.internal.runtime.JsoniterDerivationFactories.jsonCodecInstance[A](
              Expr.splice(nullVal),
              (in: JsonReader, default: A) => {
                val _ = default
                Expr.splice(decFn(Expr.quote(in), configExpr))
              },
              (x: A, out: JsonWriter) => Expr.splice(encFn(Expr.quote(x), Expr.quote(out), configExpr)),
              (in: JsonReader) => Expr.splice(keyDecFn).apply(Expr.splice(Expr.quote(in))),
              (x: A, out: JsonWriter) =>
                Expr.splice(keyEncFn).apply(Expr.splice(Expr.quote(x)), Expr.splice(Expr.quote(out)))
            )
          }
          vals.toValDefs.use(_ => resultExpr)
        }
      }
      .flatTap { result =>
        Log.info(s"Derived final combined json codec result: ${result.prettyPrint}")
      }
      .runToExprOrFail(
        "KindlingsJsonCodec.derived",
        infoRendering = if (shouldWeLogCodecDerivation) RenderFrom(Log.Level.Info) else DontRender,
        errorRendering = if (shouldWeLogCodecDerivation) RenderFrom(Log.Level.Info) else DontRender,
        timeout = derivationTimeout
      )(renderDerivationErrorMessage)
  }

  // Handles logging, error reporting and prepending "cached" defs and vals to the result.

  def deriveEncoderOnlyFromCtxAndAdaptForEntrypoint[A: Type, Out: Type](macroName: String)(
      provideCtxAndAdapt: (
          (Expr[A], Expr[JsonWriter], Expr[JsoniterConfig]) => Expr[Unit]
      ) => Expr[Out]
  ): Expr[Out] = {
    if (Type[A] =:= Type.of[Nothing].asInstanceOf[Type[A]] || Type[A] =:= Type.of[Any].asInstanceOf[Type[A]])
      Environment.reportErrorAndAbort(
        s"$macroName: type parameter was inferred as ${Type[A].prettyPrint}, which is likely unintended.\n" +
          s"Provide an explicit type parameter, e.g.: $macroName[MyType](...)\n" +
          "or add a type ascription to the result variable."
      )
    Log
      .namedScope(
        s"Deriving encoder-only for ${Type[A].prettyPrint} at: ${Environment.currentPosition.prettyPrint}"
      ) {
        implicit val ConfigT: Type[JsoniterConfig] = CTypes.JsoniterConfig
        implicit val JsonWriterT: Type[JsonWriter] = CTypes.JsonWriter
        implicit val UnitT: Type[Unit] = CTypes.Unit

        MIO.scoped { runSafe =>
          val cache = ValDefsCache.mlocal
          val selfType: Option[??] = None

          val encMIO: MIO[(Expr[A], Expr[JsonWriter], Expr[JsoniterConfig]) => Expr[Unit]] = {
            val defBuilder =
              ValDefBuilder.ofDef3[A, JsonWriter, JsoniterConfig, Unit](s"codec_encode_${Type[A].shortName}")
            for {
              _ <- Log.info(s"Forward-declaring encoder-only encode body for ${Type[A].prettyPrint}")
              _ <- cache.forwardDeclare("codec-encode-body", defBuilder)
              _ <- MIO.scoped { rs =>
                rs(cache.buildCachedWith("codec-encode-body", defBuilder) { case (_, (v, w, c)) =>
                  rs(deriveEncoderRecursively[A](using EncoderCtx.from(v, w, c, cache, selfType)))
                })
              }
              _ <- Log.info(s"Defined encoder-only encode body for ${Type[A].prettyPrint}")
              fn <- cache.get3Ary[A, JsonWriter, JsoniterConfig, Unit]("codec-encode-body")
            } yield fn.get
          }

          val encFn = runSafe {
            for {
              _ <- ensureStandardExtensionsLoaded()
              result <- encMIO
            } yield result
          }

          val vals = runSafe(cache.get)
          val resultExpr = provideCtxAndAdapt(encFn)
          vals.toValDefs.use(_ => resultExpr)
        }
      }
      .flatTap { result =>
        Log.info(s"Derived final encoder-only result: ${result.prettyPrint}")
      }
      .runToExprOrFail(
        macroName,
        infoRendering = if (shouldWeLogCodecDerivation) RenderFrom(Log.Level.Info) else DontRender,
        errorRendering = if (shouldWeLogCodecDerivation) RenderFrom(Log.Level.Info) else DontRender,
        timeout = derivationTimeout
      )(renderDerivationErrorMessage)
  }

  def deriveDecoderOnlyFromCtxAndAdaptForEntrypoint[A: Type, Out: Type](macroName: String)(
      provideCtxAndAdapt: (
          (Expr[JsonReader], Expr[JsoniterConfig]) => Expr[A],
          Expr[A]
      ) => Expr[Out]
  ): Expr[Out] = {
    if (Type[A] =:= Type.of[Nothing].asInstanceOf[Type[A]] || Type[A] =:= Type.of[Any].asInstanceOf[Type[A]])
      Environment.reportErrorAndAbort(
        s"$macroName: type parameter was inferred as ${Type[A].prettyPrint}, which is likely unintended.\n" +
          s"Provide an explicit type parameter, e.g.: $macroName[MyType](...)\n" +
          "or add a type ascription to the result variable."
      )
    Log
      .namedScope(
        s"Deriving decoder-only for ${Type[A].prettyPrint} at: ${Environment.currentPosition.prettyPrint}"
      ) {
        implicit val ConfigT: Type[JsoniterConfig] = CTypes.JsoniterConfig
        implicit val JsonReaderT: Type[JsonReader] = CTypes.JsonReader

        MIO.scoped { runSafe =>
          val cache = ValDefsCache.mlocal
          val selfType: Option[??] = None

          val decMIO: MIO[(Expr[JsonReader], Expr[JsoniterConfig]) => Expr[A]] = {
            val defBuilder =
              ValDefBuilder.ofDef2[JsonReader, JsoniterConfig, A](s"codec_decode_${Type[A].shortName}")
            for {
              _ <- Log.info(s"Forward-declaring decoder-only decode body for ${Type[A].prettyPrint}")
              _ <- cache.forwardDeclare("codec-decode-body", defBuilder)
              _ <- MIO.scoped { rs =>
                rs(cache.buildCachedWith("codec-decode-body", defBuilder) { case (_, (r, c)) =>
                  rs(deriveDecoderRecursively[A](using DecoderCtx.from(r, c, cache, selfType)))
                })
              }
              _ <- Log.info(s"Defined decoder-only decode body for ${Type[A].prettyPrint}")
              fn <- cache.get2Ary[JsonReader, JsoniterConfig, A]("codec-decode-body")
            } yield fn.get
          }

          val nullMIO: MIO[Expr[A]] = deriveNullValue[A]

          val (decFn, nullVal) = runSafe {
            for {
              _ <- ensureStandardExtensionsLoaded()
              result <- decMIO.parTuple(nullMIO)
            } yield result
          }

          val vals = runSafe(cache.get)
          val resultExpr = provideCtxAndAdapt(decFn, nullVal)
          vals.toValDefs.use(_ => resultExpr)
        }
      }
      .flatTap { result =>
        Log.info(s"Derived final decoder-only result: ${result.prettyPrint}")
      }
      .runToExprOrFail(
        macroName,
        infoRendering = if (shouldWeLogCodecDerivation) RenderFrom(Log.Level.Info) else DontRender,
        errorRendering = if (shouldWeLogCodecDerivation) RenderFrom(Log.Level.Info) else DontRender,
        timeout = derivationTimeout
      )(renderDerivationErrorMessage)
  }

  def deriveCodecFromCtxAndAdaptForEntrypoint[A: Type, Out: Type](
      macroName: String,
      configExpr: Expr[JsoniterConfig]
  )(
      provideCtxAndAdapt: (
          (Expr[A], Expr[JsonWriter], Expr[JsoniterConfig]) => Expr[Unit],
          (Expr[JsonReader], Expr[JsoniterConfig]) => Expr[A],
          Expr[A]
      ) => Expr[Out]
  ): Expr[Out] = {
    if (Type[A] =:= Type.of[Nothing].asInstanceOf[Type[A]] || Type[A] =:= Type.of[Any].asInstanceOf[Type[A]])
      Environment.reportErrorAndAbort(
        s"$macroName: type parameter was inferred as ${Type[A].prettyPrint}, which is likely unintended.\n" +
          s"Provide an explicit type parameter, e.g.: $macroName[MyType](...)\n" +
          "or add a type ascription to the result variable."
      )
    Log
      .namedScope(
        s"Deriving codec for ${Type[A].prettyPrint} at: ${Environment.currentPosition.prettyPrint}"
      ) {
        implicit val ConfigT: Type[JsoniterConfig] = CTypes.JsoniterConfig
        implicit val JsonReaderT: Type[JsonReader] = CTypes.JsonReader
        implicit val JsonWriterT: Type[JsonWriter] = CTypes.JsonWriter
        implicit val UnitT: Type[Unit] = CTypes.Unit

        // Three separate MIO values for encoder, decoder and null value, combined with parTuple
        // for parallel error aggregation. Each derivation uses ValDefBuilder to cache its body
        // as a def in the shared ValDefsCache. The returned Scala-level functions generate
        // method-call expressions that are safe to use in any Expr.splice context (no cross-splice
        // staging issues on Scala 3).
        MIO.scoped { runSafe =>
          val cache = ValDefsCache.mlocal
          val selfType: Option[??] = Some(Type[A].as_??)

          // semiEval: evaluate config at compile time for optimized codegen.
          // KNOWN ISSUE (hearth): semiEval fails on Inlined(...) wrapping module field access
          // like JsoniterConfig.default. Pending fix in hearth 0.3.1.
          val evConfig: Option[JsoniterConfig] = configExpr.semiEval.toOption

          // Validate config for contradictory flag combinations at compile time
          validateConfigOrAbort(evConfig, macroName)

          // Encoder: cache as def, derive body inside, extract function from cache
          val encMIO: MIO[(Expr[A], Expr[JsonWriter], Expr[JsoniterConfig]) => Expr[Unit]] = {
            val defBuilder =
              ValDefBuilder.ofDef3[A, JsonWriter, JsoniterConfig, Unit](s"codec_encode_${Type[A].shortName}")
            for {
              _ <- Log.info(s"Forward-declaring codec encode body for ${Type[A].prettyPrint}")
              _ <- cache.forwardDeclare("codec-encode-body", defBuilder)
              _ <- MIO.scoped { rs =>
                rs(cache.buildCachedWith("codec-encode-body", defBuilder) { case (_, (v, w, c)) =>
                  rs(deriveEncoderRecursively[A](using EncoderCtx.from(v, w, c, cache, selfType, evConfig)))
                })
              }
              _ <- Log.info(s"Defined codec encode body for ${Type[A].prettyPrint}")
              fn <- cache.get3Ary[A, JsonWriter, JsoniterConfig, Unit]("codec-encode-body")
            } yield fn.get
          }

          // Decoder: same pattern with ofDef2
          val decMIO: MIO[(Expr[JsonReader], Expr[JsoniterConfig]) => Expr[A]] = {
            val defBuilder =
              ValDefBuilder.ofDef2[JsonReader, JsoniterConfig, A](s"codec_decode_${Type[A].shortName}")
            for {
              _ <- Log.info(s"Forward-declaring codec decode body for ${Type[A].prettyPrint}")
              _ <- cache.forwardDeclare("codec-decode-body", defBuilder)
              _ <- MIO.scoped { rs =>
                rs(cache.buildCachedWith("codec-decode-body", defBuilder) { case (_, (r, c)) =>
                  rs(deriveDecoderRecursively[A](using DecoderCtx.from(r, c, cache, selfType, evConfig)))
                })
              }
              _ <- Log.info(s"Defined codec decode body for ${Type[A].prettyPrint}")
              fn <- cache.get2Ary[JsonReader, JsoniterConfig, A]("codec-decode-body")
            } yield fn.get
          }

          // Null value
          val nullMIO: MIO[Expr[A]] = deriveNullValue[A]

          // Combine with parTuple (parallel error aggregation)
          val ((encFn, decFn), nullVal) = runSafe {
            for {
              _ <- ensureStandardExtensionsLoaded()
              result <- encMIO.parTuple(decMIO).parTuple(nullMIO)
            } yield result
          }

          val vals = runSafe(cache.get)
          val resultExpr = provideCtxAndAdapt(encFn, decFn, nullVal)
          vals.toValDefs.use(_ => resultExpr)
        }
      }
      .flatTap { result =>
        Log.info(s"Derived final codec result: ${result.prettyPrint}")
      }
      .runToExprOrFail(
        macroName,
        infoRendering = if (shouldWeLogCodecDerivation) RenderFrom(Log.Level.Info) else DontRender,
        errorRendering = if (shouldWeLogCodecDerivation) RenderFrom(Log.Level.Info) else DontRender,
        timeout = derivationTimeout
      )(renderDerivationErrorMessage)
  }

  def shouldWeLogCodecDerivation: Boolean = {
    implicit val LogDerivation: Type[KindlingsJsonValueCodec.LogDerivation] = CTypes.CodecLogDerivation
    def logDerivationImported = Expr.summonImplicit[KindlingsJsonValueCodec.LogDerivation].isDefined

    def logDerivationSetGlobally = (for {
      data <- Environment.typedSettings.toOption
      jsoniterDerivation <- data.get("jsoniterDerivation")
      shouldLog <- jsoniterDerivation.get("logDerivation").flatMap(_.asBoolean)
    } yield shouldLog).getOrElse(false)

    logDerivationImported || logDerivationSetGlobally
  }

  private def renderDerivationErrorMessage(errorLogs: String, errors: NonEmptyVector[Throwable]): String = {
    val errorsRendered = errors
      .map { e =>
        e.getMessage.split("\n").toList match {
          case head :: tail => (("  - " + head) :: tail.map("    " + _)).mkString("\n")
          case _            => "  - " + e.getMessage
        }
      }
      .mkString("\n")
    val hint =
      "Enable debug logging with: import hearth.kindlings.jsoniterderivation.debug.logDerivationForKindlingsJsonValueCodec or scalac option -Xmacro-settings:jsoniterDerivation.logDerivation=true"
    if (errorLogs.nonEmpty)
      s"""Macro derivation failed with the following errors:
         |$errorsRendered
         |and the following logs:
         |$errorLogs
         |$hint""".stripMargin
    else
      s"""Macro derivation failed with the following errors:
         |$errorsRendered
         |$hint""".stripMargin
  }

  // Config conflict validation — reports compile-time errors or warnings when the config is
  // statically known (via semiEval) and contains contradictory flag combinations.
  //
  // - decodingOnly + encodingOnly: hard error (codec is completely unusable)
  // - requireCollectionFields + transientEmpty: warning (well-defined but likely unintended)
  // - requireDefaultFields + transientDefault: warning (well-defined but likely unintended)

  private def validateConfigOrAbort(evConfig: Option[JsoniterConfig], macroName: String): Unit =
    evConfig.foreach { config =>
      // Hard error: a codec that can neither encode nor decode is useless
      if (config.decodingOnly && config.encodingOnly)
        Environment.reportErrorAndAbort(
          s"$macroName: JsoniterConfig has both decodingOnly = true and encodingOnly = true. " +
            "A codec cannot both reject encoding and reject decoding."
        )
      // Warnings for confusing but technically valid combinations
      if (config.requireCollectionFields && config.transientEmpty)
        Environment.reportWarn(
          s"$macroName: JsoniterConfig has both requireCollectionFields = true and transientEmpty = true. " +
            "The encoder will omit empty collections but the decoder will reject their absence."
        )
      if (config.requireDefaultFields && config.transientDefault)
        Environment.reportWarn(
          s"$macroName: JsoniterConfig has both requireDefaultFields = true and transientDefault = true. " +
            "The encoder will omit default-valued fields but the decoder will reject their absence."
        )
    }

  // Null value derivation

  @scala.annotation.nowarn("msg=is never used")
  def deriveNullValue[A: Type]: MIO[Expr[A]] = MIO.pure {
    if (Type[A] <:< Type.of[AnyRef]) Expr.quote(null.asInstanceOf[A])
    else if (Type[A] =:= Type.of[Boolean]) Expr.quote(false.asInstanceOf[A])
    else if (Type[A] =:= Type.of[Byte]) Expr.quote(0.toByte.asInstanceOf[A])
    else if (Type[A] =:= Type.of[Short]) Expr.quote(0.toShort.asInstanceOf[A])
    else if (Type[A] =:= Type.of[Int]) Expr.quote(0.asInstanceOf[A])
    else if (Type[A] =:= Type.of[Long]) Expr.quote(0L.asInstanceOf[A])
    else if (Type[A] =:= Type.of[Float]) Expr.quote(0.0f.asInstanceOf[A])
    else if (Type[A] =:= Type.of[Double]) Expr.quote(0.0.asInstanceOf[A])
    else if (Type[A] =:= Type.of[Char]) Expr.quote('\u0000'.asInstanceOf[A])
    else Expr.quote(null.asInstanceOf[A])
  }

  // Encoder Context

  final case class EncoderCtx[A](
      tpe: Type[A],
      value: Expr[A],
      writer: Expr[JsonWriter],
      config: Expr[JsoniterConfig],
      cache: MLocal[ValDefsCache],
      derivedType: Option[??],
      evaluatedConfig: Option[JsoniterConfig] = None
  ) {

    def nest[B: Type](newValue: Expr[B]): EncoderCtx[B] = copy[B](
      tpe = Type[B],
      value = newValue
    )

    def nestInCache(
        newValue: Expr[A],
        newWriter: Expr[JsonWriter],
        newConfig: Expr[JsoniterConfig]
    ): EncoderCtx[A] = copy(
      value = newValue,
      writer = newWriter,
      config = newConfig
    )

    def getInstance[B: Type]: MIO[Option[Expr[JsonValueCodec[B]]]] = {
      implicit val CodecB: Type[JsonValueCodec[B]] = CTypes.JsonValueCodec[B]
      cache.get0Ary[JsonValueCodec[B]]("cached-codec-instance")
    }
    def setInstance[B: Type](instance: Expr[JsonValueCodec[B]]): MIO[Unit] = {
      implicit val CodecB: Type[JsonValueCodec[B]] = CTypes.JsonValueCodec[B]
      Log.info(s"Caching JsonValueCodec instance for ${Type[B].prettyPrint}") >>
        cache.buildCachedWith(
          "cached-codec-instance",
          ValDefBuilder.ofLazy[JsonValueCodec[B]](s"codec_${Type[B].shortName}")
        )(_ => instance)
    }

    def getHelper[B: Type]: MIO[Option[(Expr[B], Expr[JsonWriter], Expr[JsoniterConfig]) => Expr[Unit]]] = {
      implicit val UnitT: Type[Unit] = CTypes.Unit
      implicit val JsonWriterT: Type[JsonWriter] = CTypes.JsonWriter
      implicit val ConfigT: Type[JsoniterConfig] = CTypes.JsoniterConfig
      cache.get3Ary[B, JsonWriter, JsoniterConfig, Unit]("cached-encode-method")
    }
    def setHelper[B: Type](
        helper: (Expr[B], Expr[JsonWriter], Expr[JsoniterConfig]) => MIO[Expr[Unit]]
    ): MIO[Unit] = {
      implicit val UnitT: Type[Unit] = CTypes.Unit
      implicit val JsonWriterT: Type[JsonWriter] = CTypes.JsonWriter
      implicit val ConfigT: Type[JsoniterConfig] = CTypes.JsoniterConfig
      val defBuilder =
        ValDefBuilder.ofDef3[B, JsonWriter, JsoniterConfig, Unit](s"encode_${Type[B].shortName}")
      for {
        _ <- Log.info(s"Forward-declaring encode helper for ${Type[B].prettyPrint}")
        _ <- cache.forwardDeclare("cached-encode-method", defBuilder)
        _ <- MIO.scoped { runSafe =>
          runSafe(cache.buildCachedWith("cached-encode-method", defBuilder) { case (_, (value, writer, config)) =>
            runSafe(helper(value, writer, config))
          })
        }
        _ <- Log.info(s"Defined encode helper for ${Type[B].prettyPrint}")
      } yield ()
    }

    override def toString: String =
      s"encode[${tpe.prettyPrint}](value = ${value.prettyPrint}, writer = ${writer.prettyPrint}, config = ${config.prettyPrint})"
  }
  object EncoderCtx {

    def from[A: Type](
        value: Expr[A],
        writer: Expr[JsonWriter],
        config: Expr[JsoniterConfig],
        cache: MLocal[ValDefsCache],
        derivedType: Option[??],
        evaluatedConfig: Option[JsoniterConfig] = None
    ): EncoderCtx[A] = EncoderCtx(
      tpe = Type[A],
      value = value,
      writer = writer,
      config = config,
      cache = cache,
      derivedType = derivedType,
      evaluatedConfig = evaluatedConfig
    )
  }

  def ectx[A](implicit A: EncoderCtx[A]): EncoderCtx[A] = A

  implicit def currentEncoderValueType[A: EncoderCtx]: Type[A] = ectx.tpe

  abstract class EncoderDerivationRule(val name: String) extends Rule {
    def apply[A: EncoderCtx]: MIO[Rule.Applicability[Expr[Unit]]]
  }

  // Encoder derivation

  def deriveEncoderRecursively[A: EncoderCtx]: MIO[Expr[Unit]] =
    // Cache ALL types (including built-ins, options, collections) as helper defs.
    // Without this, built-in types are re-derived for every field reference, causing
    // O(n*m) compilation time for large type graphs (n types × m fields).
    // The forward-declaration also breaks recursive self-references (e.g., Node → List[Node]).
    ectx.getHelper[A].flatMap {
      case Some(helperCall) =>
        Log.info(s"Using cached encoder for ${Type[A].prettyPrint}") >>
          MIO.pure(helperCall(ectx.value, ectx.writer, ectx.config))
      case None =>
        for {
          _ <- ectx.setHelper[A] { (value, writer, config) =>
            deriveEncoderViaRules[A](using ectx.nestInCache(value, writer, config))
          }
          helper <- ectx.getHelper[A]
        } yield helper.get(ectx.value, ectx.writer, ectx.config)
    }

  private def deriveEncoderViaRules[A: EncoderCtx]: MIO[Expr[Unit]] =
    Log
      .namedScope(s"Deriving encoder for type ${Type[A].prettyPrint}") {
        Rules(
          // Note: EncoderUseCachedDefWhenAvailableRule / DecoderUseCachedDefWhenAvailableRule are NOT
          // included here because caching is handled by derive*Recursively (which wraps in setHelper).
          EncoderHandleAsLiteralTypeRule,
          EncoderUseImplicitWhenAvailableRule,
          EncoderHandleAsBuiltInRule,
          EncoderHandleAsValueTypeRule,
          EncoderHandleAsOptionRule,
          EncoderHandleAsMapRule,
          EncoderHandleAsCollectionRule,
          EncoderHandleAsNamedTupleRule,
          EncoderHandleAsOneValueClassRule,
          EncoderHandleAsSingletonRule,
          EncoderHandleAsCaseClassRule,
          EncoderHandleAsEnumRule
        )(rule => Log.namedScope(s"encoder rule:${rule.name}")(rule[A])).flatMap {
          case Right(result) =>
            Log.info(s"Derived encoder for ${Type[A].prettyPrint}: ${result.prettyPrint}") >>
              MIO.pure(result)
          case Left(reasons) =>
            val reasonsStrings = reasons.toListMap.view.map { case (rule, reasons) =>
              if (reasons.isEmpty) s"The rule ${rule.name} was not applicable"
              else
                s" - The rule ${rule.name} was not applicable, for the following reasons: ${reasons.mkString(", ")}"
            }.toList
            val err = CodecDerivationError.UnsupportedType(Type[A].prettyPrint, reasonsStrings)
            Log.error(err.message) >> MIO.fail(err)
        }
      }

  // Decoder Context

  final case class DecoderCtx[A](
      tpe: Type[A],
      reader: Expr[JsonReader],
      config: Expr[JsoniterConfig],
      cache: MLocal[ValDefsCache],
      derivedType: Option[??],
      evaluatedConfig: Option[JsoniterConfig] = None
  ) {

    def nest[B: Type](newReader: Expr[JsonReader]): DecoderCtx[B] = copy[B](
      tpe = Type[B],
      reader = newReader
    )

    def nestInCache(
        newReader: Expr[JsonReader],
        newConfig: Expr[JsoniterConfig]
    ): DecoderCtx[A] = copy(
      reader = newReader,
      config = newConfig
    )

    def getInstance[B: Type]: MIO[Option[Expr[JsonValueCodec[B]]]] = {
      implicit val CodecB: Type[JsonValueCodec[B]] = CTypes.JsonValueCodec[B]
      cache.get0Ary[JsonValueCodec[B]]("cached-codec-instance")
    }
    def setInstance[B: Type](instance: Expr[JsonValueCodec[B]]): MIO[Unit] = {
      implicit val CodecB: Type[JsonValueCodec[B]] = CTypes.JsonValueCodec[B]
      Log.info(s"Caching JsonValueCodec instance for ${Type[B].prettyPrint}") >>
        cache.buildCachedWith(
          "cached-codec-instance",
          ValDefBuilder.ofLazy[JsonValueCodec[B]](s"codec_${Type[B].shortName}")
        )(_ => instance)
    }

    def getHelper[B: Type]: MIO[Option[(Expr[JsonReader], Expr[JsoniterConfig]) => Expr[B]]] = {
      implicit val JsonReaderT: Type[JsonReader] = CTypes.JsonReader
      implicit val ConfigT: Type[JsoniterConfig] = CTypes.JsoniterConfig
      cache.get2Ary[JsonReader, JsoniterConfig, B]("cached-decode-method")
    }
    def setHelper[B: Type](
        helper: (Expr[JsonReader], Expr[JsoniterConfig]) => MIO[Expr[B]]
    ): MIO[Unit] = {
      implicit val JsonReaderT: Type[JsonReader] = CTypes.JsonReader
      implicit val ConfigT: Type[JsoniterConfig] = CTypes.JsoniterConfig
      val defBuilder =
        ValDefBuilder.ofDef2[JsonReader, JsoniterConfig, B](s"decode_${Type[B].shortName}")
      for {
        _ <- Log.info(s"Forward-declaring decode helper for ${Type[B].prettyPrint}")
        _ <- cache.forwardDeclare("cached-decode-method", defBuilder)
        _ <- MIO.scoped { runSafe =>
          runSafe(cache.buildCachedWith("cached-decode-method", defBuilder) { case (_, (reader, config)) =>
            runSafe(helper(reader, config))
          })
        }
        _ <- Log.info(s"Defined decode helper for ${Type[B].prettyPrint}")
      } yield ()
    }

    override def toString: String =
      s"decode[${tpe.prettyPrint}](reader = ${reader.prettyPrint}, config = ${config.prettyPrint})"
  }
  object DecoderCtx {

    def from[A: Type](
        reader: Expr[JsonReader],
        config: Expr[JsoniterConfig],
        cache: MLocal[ValDefsCache],
        derivedType: Option[??],
        evaluatedConfig: Option[JsoniterConfig] = None
    ): DecoderCtx[A] = DecoderCtx(
      tpe = Type[A],
      reader = reader,
      config = config,
      cache = cache,
      derivedType = derivedType,
      evaluatedConfig = evaluatedConfig
    )
  }

  def dctx[A](implicit A: DecoderCtx[A]): DecoderCtx[A] = A

  implicit def currentDecoderValueType[A: DecoderCtx]: Type[A] = dctx.tpe

  abstract class DecoderDerivationRule(val name: String) extends Rule {
    def apply[A: DecoderCtx]: MIO[Rule.Applicability[Expr[A]]]
  }

  // Decoder derivation

  // Guard to ensure loadStandardExtensions() is called at most once per expansion,
  // since it is not idempotent (each call re-registers all providers).
  private var standardExtensionsLoaded: Boolean = false
  private def ensureStandardExtensionsLoaded(): MIO[Unit] =
    if (standardExtensionsLoaded) MIO.pure(())
    else
      Environment.loadStandardExtensions().toMIO(allowFailures = false).map { _ =>
        standardExtensionsLoaded = true
        ()
      }

  // Shared ignored implicits for summonExprIgnoring — prevents infinite macro expansion
  // when summoning finds library auto-derivation methods (e.g., KindlingsJsonCodec.derived).
  private[compiletime] lazy val codecIgnoredImplicits: Seq[UntypedMethod] =
    Type.of[KindlingsJsonValueCodec.type].methods.collect {
      case method if method.isImplicit => method.asUntyped
    } ++ Type.of[KindlingsJsonCodec.type].methods.collect {
      case method if method.isImplicit => method.asUntyped
    }

  // Cache for summonExprIgnoring results — avoids re-summoning the same type.
  // Key: UntypedType, Value: Right(expr) on success, Left(reason) on failure.
  private val summonCodecCache: scala.collection.mutable.Map[UntypedType, Either[String, Any]] =
    scala.collection.mutable.Map.empty

  /** Summon JsonValueCodec[A] with caching. Returns cached result on repeated calls for the same type. */
  private[compiletime] def summonJsonValueCodecCached[A: Type]: Either[String, Expr[JsonValueCodec[A]]] =
    summonCodecCache
      .getOrElseUpdate(
        Type[A].asUntyped,
        CTypes.JsonValueCodec[A].summonExprIgnoring(codecIgnoredImplicits*).toEither
      )
      .asInstanceOf[Either[String, Expr[JsonValueCodec[A]]]]

  // Debug counter to detect runaway decoder derivation — fail gracefully instead of OOM
  private var decoderDerivationCounter: Int = 0
  private val decoderDerivationLimit: Int = 500

  def deriveDecoderRecursively[A: DecoderCtx]: MIO[Expr[A]] = {
    decoderDerivationCounter += 1
    if (decoderDerivationCounter > decoderDerivationLimit)
      MIO.fail(
        new RuntimeException(
          s"Derivation limit ($decoderDerivationLimit) exceeded at decoder for ${Type[A].prettyPrint}. " +
            "This likely indicates an infinite derivation loop."
        )
      )
    else
      dctx.getHelper[A].flatMap {
        case Some(helperCall) =>
          Log.info(s"Using cached decoder for ${Type[A].prettyPrint}") >>
            MIO.pure(helperCall(dctx.reader, dctx.config))
        case None =>
          for {
            _ <- dctx.setHelper[A] { (reader, config) =>
              deriveDecoderViaRules[A](using dctx.nestInCache(reader, config))
            }
            helper <- dctx.getHelper[A]
          } yield helper.get(dctx.reader, dctx.config)
      }
  }

  private def deriveDecoderViaRules[A: DecoderCtx]: MIO[Expr[A]] =
    Log
      .namedScope(s"Deriving decoder for type ${Type[A].prettyPrint}") {
        Rules(
          // Note: EncoderUseCachedDefWhenAvailableRule / DecoderUseCachedDefWhenAvailableRule are NOT
          // included here because caching is handled by derive*Recursively (which wraps in setHelper).
          DecoderHandleAsLiteralTypeRule,
          DecoderUseImplicitWhenAvailableRule,
          DecoderHandleAsBuiltInRule,
          DecoderHandleAsValueTypeRule,
          DecoderHandleAsOptionRule,
          DecoderHandleAsMapRule,
          DecoderHandleAsCollectionRule,
          DecoderHandleAsNamedTupleRule,
          DecoderHandleAsOneValueClassRule,
          DecoderHandleAsSingletonRule,
          DecoderHandleAsCaseClassRule,
          DecoderHandleAsEnumRule
        )(rule => Log.namedScope(s"decoder rule:${rule.name}")(rule[A])).flatMap {
          case Right(result) =>
            Log.info(s"Derived decoder for ${Type[A].prettyPrint}: ${result.prettyPrint}") >>
              MIO.pure(result)
          case Left(reasons) =>
            val reasonsStrings = reasons.toListMap.view.map { case (rule, reasons) =>
              if (reasons.isEmpty) s"The rule ${rule.name} was not applicable"
              else
                s" - The rule ${rule.name} was not applicable, for the following reasons: ${reasons.mkString(", ")}"
            }.toList
            val err = CodecDerivationError.UnsupportedType(Type[A].prettyPrint, reasonsStrings)
            Log.error(err.message) >> MIO.fail(err)
        }
      }
}
