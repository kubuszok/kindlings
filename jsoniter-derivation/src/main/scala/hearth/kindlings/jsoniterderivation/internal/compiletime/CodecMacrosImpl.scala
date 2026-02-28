package hearth.kindlings.jsoniterderivation.internal.compiletime

import hearth.MacroCommons
import hearth.fp.data.NonEmptyList
import hearth.fp.effect.*
import hearth.fp.syntax.*
import hearth.std.*

import hearth.kindlings.jsoniterderivation.{JsoniterConfig, KindlingsJsonCodec, KindlingsJsonValueCodec}
import hearth.kindlings.jsoniterderivation.annotations.{fieldName as fieldNameAnn, stringified, transientField}
import hearth.kindlings.jsoniterderivation.internal.runtime.JsoniterDerivationUtils
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

trait CodecMacrosImpl { this: MacroCommons & StdExtensions & AnnotationSupport =>

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

    deriveCodecFromCtxAndAdaptForEntrypoint[A, KindlingsJsonValueCodec[A]]("KindlingsJsonValueCodec.derived") {
      case (encodeFn, decodeFn, nullValueExpr) =>
        Expr.quote {
          new KindlingsJsonValueCodec[A] {
            def nullValue: A = Expr.splice(nullValueExpr)
            def decodeValue(in: JsonReader, default: A): A = {
              val _ = default
              if (Expr.splice(configExpr).encodingOnly)
                throw new UnsupportedOperationException("encoding-only codec cannot decode")
              Expr.splice(decodeFn(Expr.quote(in), configExpr))
            }
            def encodeValue(x: A, out: JsonWriter): Unit =
              if (Expr.splice(configExpr).decodingOnly)
                throw new UnsupportedOperationException("decoding-only codec cannot encode")
              else
                Expr.splice(encodeFn(Expr.quote(x), Expr.quote(out), configExpr))
          }
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

    CTypes.JsonValueCodec[A].summonExprIgnoring(EncUseImplicitWhenAvailableRule.ignoredImplicits*).toEither match {
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

    CTypes.JsonValueCodec[A].summonExprIgnoring(DecUseImplicitWhenAvailableRule.ignoredImplicits*).toEither match {
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
            EncHandleAsMapRule.deriveKeyEncoding[A]
          }

          // Derive key decoding using a stub DecoderCtx
          val keyDecMIO: MIO[Option[Expr[JsonReader => A]]] = {
            implicit val StringT: Type[String] = CTypes.String
            val stubReader = Expr.quote(null.asInstanceOf[JsonReader])
            implicit val ctx: DecoderCtx[A] = DecoderCtx.from(stubReader, configExpr, cache, selfType)
            DecHandleAsMapRule.deriveKeyDecoding[A]
          }

          val (keyEncOpt, keyDecOpt) = runSafe {
            for {
              _ <- Environment.loadStandardExtensions().toMIO(allowFailures = false)
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
        errorRendering = if (shouldWeLogCodecDerivation) RenderFrom(Log.Level.Info) else DontRender
      ) { (errorLogs, errors) =>
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
        s"KindlingsJsonCodec.derive: type parameter was inferred as ${Type[A].prettyPrint}, which is likely unintended.\n" +
          "Provide an explicit type parameter, e.g.: KindlingsJsonCodec.derive[MyType](...)\n" +
          "or add a type ascription to the result variable."
      )

    Log
      .namedScope(
        s"Deriving combined json codec for ${Type[A].prettyPrint} at: ${Environment.currentPosition.prettyPrint}"
      ) {
        MIO.scoped { runSafe =>
          val cache = ValDefsCache.mlocal
          val selfType: Option[??] = Some(Type[A].as_??)

          // Value codec parts — same as deriveCodecTypeClass
          val encMIO: MIO[(Expr[A], Expr[JsonWriter], Expr[JsoniterConfig]) => Expr[Unit]] = {
            val defBuilder =
              ValDefBuilder.ofDef3[A, JsonWriter, JsoniterConfig, Unit](s"codec_encode_${Type[A].shortName}")
            for {
              _ <- Log.info(s"Forward-declaring json codec encode body for ${Type[A].prettyPrint}")
              _ <- cache.forwardDeclare("codec-encode-body", defBuilder)
              _ <- MIO.scoped { rs =>
                rs(cache.buildCachedWith("codec-encode-body", defBuilder) { case (_, (v, w, c)) =>
                  rs(deriveEncoderRecursively[A](using EncoderCtx.from(v, w, c, cache, selfType)))
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
                  rs(deriveDecoderRecursively[A](using DecoderCtx.from(r, c, cache, selfType)))
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
            implicit val ctx: EncoderCtx[A] = EncoderCtx.from(stubValue, stubWriter, configExpr, cache, selfType)
            EncHandleAsMapRule.deriveKeyEncoding[A]
          }

          val keyDecMIO: MIO[Option[Expr[JsonReader => A]]] = {
            implicit val StringT: Type[String] = CTypes.String
            val stubReader = Expr.quote(null.asInstanceOf[JsonReader])
            implicit val ctx: DecoderCtx[A] = DecoderCtx.from(stubReader, configExpr, cache, selfType)
            DecHandleAsMapRule.deriveKeyDecoding[A]
          }

          val (((encFn, decFn), nullVal), (keyEncOpt, keyDecOpt)) = runSafe {
            for {
              _ <- Environment.loadStandardExtensions().toMIO(allowFailures = false)
              result <- encMIO.parTuple(decMIO).parTuple(nullMIO).parTuple(keyEncMIO.parTuple(keyDecMIO))
            } yield result
          }

          val (keyEncFn, keyDecFn) = (keyEncOpt, keyDecOpt) match {
            case (Some(enc), Some(dec)) => (enc, dec)
            case _                      =>
              Environment.reportErrorAndAbort(
                s"KindlingsJsonCodec.derive: Cannot derive JsonKeyCodec for ${Type[A].prettyPrint}.\n" +
                  "Key codecs can only be derived for: primitive types (Int, Long, Double, Float, Short, Boolean, BigDecimal, BigInt),\n" +
                  "value types wrapping those primitives, and sealed traits/enums of case objects.\n" +
                  "If you only need value encoding/decoding, use KindlingsJsonValueCodec.derive instead."
              )
          }

          val vals = runSafe(cache.get)
          val resultExpr = Expr.quote {
            new KindlingsJsonCodec[A] {
              def nullValue: A = Expr.splice(nullVal)
              def decodeValue(in: JsonReader, default: A): A = {
                val _ = default
                Expr.splice(decFn(Expr.quote(in), configExpr))
              }
              def encodeValue(x: A, out: JsonWriter): Unit =
                Expr.splice(encFn(Expr.quote(x), Expr.quote(out), configExpr))
              def decodeKey(in: JsonReader): A = Expr.splice(keyDecFn).apply(Expr.splice(Expr.quote(in)))
              def encodeKey(x: A, out: JsonWriter): Unit =
                Expr.splice(keyEncFn).apply(Expr.splice(Expr.quote(x)), Expr.splice(Expr.quote(out)))
            }
          }
          vals.toValDefs.use(_ => resultExpr)
        }
      }
      .flatTap { result =>
        Log.info(s"Derived final combined json codec result: ${result.prettyPrint}")
      }
      .runToExprOrFail(
        "KindlingsJsonCodec.derive",
        infoRendering = if (shouldWeLogCodecDerivation) RenderFrom(Log.Level.Info) else DontRender,
        errorRendering = if (shouldWeLogCodecDerivation) RenderFrom(Log.Level.Info) else DontRender
      ) { (errorLogs, errors) =>
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
              _ <- Environment.loadStandardExtensions().toMIO(allowFailures = false)
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
        errorRendering = if (shouldWeLogCodecDerivation) RenderFrom(Log.Level.Info) else DontRender
      ) { (errorLogs, errors) =>
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
              _ <- Environment.loadStandardExtensions().toMIO(allowFailures = false)
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
        errorRendering = if (shouldWeLogCodecDerivation) RenderFrom(Log.Level.Info) else DontRender
      ) { (errorLogs, errors) =>
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
  }

  def deriveCodecFromCtxAndAdaptForEntrypoint[A: Type, Out: Type](macroName: String)(
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

          // Encoder: cache as def, derive body inside, extract function from cache
          val encMIO: MIO[(Expr[A], Expr[JsonWriter], Expr[JsoniterConfig]) => Expr[Unit]] = {
            val defBuilder =
              ValDefBuilder.ofDef3[A, JsonWriter, JsoniterConfig, Unit](s"codec_encode_${Type[A].shortName}")
            for {
              _ <- Log.info(s"Forward-declaring codec encode body for ${Type[A].prettyPrint}")
              _ <- cache.forwardDeclare("codec-encode-body", defBuilder)
              _ <- MIO.scoped { rs =>
                rs(cache.buildCachedWith("codec-encode-body", defBuilder) { case (_, (v, w, c)) =>
                  rs(deriveEncoderRecursively[A](using EncoderCtx.from(v, w, c, cache, selfType)))
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
                  rs(deriveDecoderRecursively[A](using DecoderCtx.from(r, c, cache, selfType)))
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
              _ <- Environment.loadStandardExtensions().toMIO(allowFailures = false)
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
        errorRendering = if (shouldWeLogCodecDerivation) RenderFrom(Log.Level.Info) else DontRender
      ) { (errorLogs, errors) =>
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
      derivedType: Option[??]
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
        derivedType: Option[??]
    ): EncoderCtx[A] = EncoderCtx(
      tpe = Type[A],
      value = value,
      writer = writer,
      config = config,
      cache = cache,
      derivedType = derivedType
    )
  }

  def ectx[A](implicit A: EncoderCtx[A]): EncoderCtx[A] = A

  implicit def currentEncoderValueType[A: EncoderCtx]: Type[A] = ectx.tpe

  abstract class EncoderDerivationRule(val name: String) extends Rule {
    def apply[A: EncoderCtx]: MIO[Rule.Applicability[Expr[Unit]]]
  }

  // Encoder derivation

  def deriveEncoderRecursively[A: EncoderCtx]: MIO[Expr[Unit]] =
    Log
      .namedScope(s"Deriving encoder for type ${Type[A].prettyPrint}") {
        Rules(
          EncUseCachedDefWhenAvailableRule,
          EncHandleAsLiteralTypeRule,
          EncUseImplicitWhenAvailableRule,
          EncHandleAsBuiltInRule,
          EncHandleAsValueTypeRule,
          EncHandleAsOptionRule,
          EncHandleAsMapRule,
          EncHandleAsCollectionRule,
          EncHandleAsNamedTupleRule,
          EncHandleAsSingletonRule,
          EncHandleAsCaseClassRule,
          EncHandleAsEnumRule
        )(_[A]).flatMap {
          case Right(result) =>
            Log.info(s"Derived encoder for ${Type[A].prettyPrint}: ${result.prettyPrint}") >>
              MIO.pure(result)
          case Left(reasons) =>
            val reasonsStrings = reasons.toListMap
              .removed(EncUseCachedDefWhenAvailableRule)
              .view
              .map { case (rule, reasons) =>
                if (reasons.isEmpty) s"The rule ${rule.name} was not applicable"
                else
                  s" - The rule ${rule.name} was not applicable, for the following reasons: ${reasons.mkString(", ")}"
              }
              .toList
            val err = CodecDerivationError.UnsupportedType(Type[A].prettyPrint, reasonsStrings)
            Log.error(err.message) >> MIO.fail(err)
        }
      }

  // Encoder Rules

  object EncUseCachedDefWhenAvailableRule extends EncoderDerivationRule("use cached def when available") {

    def apply[A: EncoderCtx]: MIO[Rule.Applicability[Expr[Unit]]] =
      Log.info(s"Attempting to use cached encoder for ${Type[A].prettyPrint}") >>
        ectx.getInstance[A].flatMap {
          case Some(instance) => callCachedInstance[A](instance)
          case None           =>
            ectx.getHelper[A].flatMap {
              case Some(helperCall) => callCachedHelper[A](helperCall)
              case None             => yieldUnsupported[A]
            }
        }

    private def callCachedInstance[A: EncoderCtx](
        instance: Expr[JsonValueCodec[A]]
    ): MIO[Rule.Applicability[Expr[Unit]]] =
      Log.info(s"Found cached codec instance for ${Type[A].prettyPrint}") >> MIO.pure(Rule.matched(Expr.quote {
        Expr.splice(instance).encodeValue(Expr.splice(ectx.value), Expr.splice(ectx.writer))
      }))

    private def callCachedHelper[A: EncoderCtx](
        helperCall: (Expr[A], Expr[JsonWriter], Expr[JsoniterConfig]) => Expr[Unit]
    ): MIO[Rule.Applicability[Expr[Unit]]] =
      Log.info(s"Found cached encoder helper for ${Type[A].prettyPrint}") >> MIO.pure(
        Rule.matched(helperCall(ectx.value, ectx.writer, ectx.config))
      )

    private def yieldUnsupported[A: EncoderCtx]: MIO[Rule.Applicability[Expr[Unit]]] =
      MIO.pure(Rule.yielded(s"The type ${Type[A].prettyPrint} does not have a cached encoder"))
  }

  object EncUseImplicitWhenAvailableRule extends EncoderDerivationRule("use implicit when available") {

    lazy val ignoredImplicits: Seq[UntypedMethod] = {
      val ours = Type.of[KindlingsJsonValueCodec.type].methods.collect {
        case method if method.value.name == "derived" => method.value.asUntyped
      }
      ours
    }

    def apply[A: EncoderCtx]: MIO[Rule.Applicability[Expr[Unit]]] =
      Log.info(s"Attempting to use implicit JsonValueCodec for ${Type[A].prettyPrint}") >> {
        if (ectx.derivedType.exists(_.Underlying =:= Type[A]))
          MIO.pure(
            Rule.yielded(s"The type ${Type[A].prettyPrint} is the type being derived, skipping implicit search")
          )
        else
          CTypes.JsonValueCodec[A].summonExprIgnoring(ignoredImplicits*).toEither match {
            case Right(instanceExpr) => cacheAndUse[A](instanceExpr)
            case Left(reason)        => yieldUnsupported[A](reason)
          }
      }

    private def cacheAndUse[A: EncoderCtx](
        instanceExpr: Expr[JsonValueCodec[A]]
    ): MIO[Rule.Applicability[Expr[Unit]]] =
      Log.info(s"Found implicit codec ${instanceExpr.prettyPrint}, using directly") >>
        MIO.pure(Rule.matched(Expr.quote {
          Expr.splice(instanceExpr).encodeValue(Expr.splice(ectx.value), Expr.splice(ectx.writer))
        }))

    private def yieldUnsupported[A: EncoderCtx](reason: String): MIO[Rule.Applicability[Expr[Unit]]] =
      MIO.pure(
        Rule.yielded(
          s"The type ${Type[A].prettyPrint} does not have an implicit JsonValueCodec instance: $reason"
        )
      )
  }

  object EncHandleAsLiteralTypeRule extends EncoderDerivationRule("handle as literal type when possible") {

    def apply[A: EncoderCtx]: MIO[Rule.Applicability[Expr[Unit]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as a literal type") >> {
        extractLiteralEncoder[A] match {
          case Some(expr) => MIO.pure(Rule.matched(expr))
          case None       => MIO.pure(Rule.yielded(s"The type ${Type[A].prettyPrint} is not a literal type"))
        }
      }

    private def extractLiteralEncoder[A: EncoderCtx]: Option[Expr[Unit]] = {
      val writer = ectx.writer
      Type.StringCodec.fromType(Type[A]).map { e =>
        val v: String = e.value
        Expr.quote(Expr.splice(writer).writeVal(Expr.splice(Expr(v))))
      } orElse Type.IntCodec.fromType(Type[A]).map { e =>
        val v: Int = e.value
        Expr.quote(Expr.splice(writer).writeVal(Expr.splice(Expr(v))))
      } orElse Type.LongCodec.fromType(Type[A]).map { e =>
        val v: Long = e.value
        Expr.quote(Expr.splice(writer).writeVal(Expr.splice(Expr(v))))
      } orElse Type.DoubleCodec.fromType(Type[A]).map { e =>
        val v: Double = e.value
        Expr.quote(Expr.splice(writer).writeVal(Expr.splice(Expr(v))))
      } orElse Type.FloatCodec.fromType(Type[A]).map { e =>
        val v: Float = e.value
        Expr.quote(Expr.splice(writer).writeVal(Expr.splice(Expr(v))))
      } orElse Type.BooleanCodec.fromType(Type[A]).map { e =>
        val v: Boolean = e.value
        Expr.quote(Expr.splice(writer).writeVal(Expr.splice(Expr(v))))
      } orElse Type.ShortCodec.fromType(Type[A]).map { e =>
        val v: Short = e.value
        Expr.quote(Expr.splice(writer).writeVal(Expr.splice(Expr(v))))
      } orElse Type.ByteCodec.fromType(Type[A]).map { e =>
        val v: Byte = e.value
        Expr.quote(Expr.splice(writer).writeVal(Expr.splice(Expr(v))))
      } orElse Type.CharCodec.fromType(Type[A]).map { e =>
        val v: Char = e.value
        Expr.quote(Expr.splice(writer).writeVal(Expr.splice(Expr(v)).toString))
      }
    }
  }

  @scala.annotation.nowarn("msg=is never used")
  object EncHandleAsBuiltInRule extends EncoderDerivationRule("handle as built-in type") {

    def apply[A: EncoderCtx]: MIO[Rule.Applicability[Expr[Unit]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as a built-in type") >> {
        val writer = ectx.writer
        val value = ectx.value

        val result: Option[Expr[Unit]] =
          if (Type[A] =:= Type.of[Int])
            Some(Expr.quote(Expr.splice(writer).writeVal(Expr.splice(value).asInstanceOf[Int])))
          else if (Type[A] =:= Type.of[Long])
            Some(Expr.quote(Expr.splice(writer).writeVal(Expr.splice(value).asInstanceOf[Long])))
          else if (Type[A] =:= Type.of[Double])
            Some(Expr.quote(Expr.splice(writer).writeVal(Expr.splice(value).asInstanceOf[Double])))
          else if (Type[A] =:= Type.of[Float])
            Some(Expr.quote(Expr.splice(writer).writeVal(Expr.splice(value).asInstanceOf[Float])))
          else if (Type[A] =:= Type.of[Boolean])
            Some(Expr.quote(Expr.splice(writer).writeVal(Expr.splice(value).asInstanceOf[Boolean])))
          else if (Type[A] =:= Type.of[String])
            Some(Expr.quote(Expr.splice(writer).writeVal(Expr.splice(value).asInstanceOf[String])))
          else if (Type[A] =:= Type.of[Byte])
            Some(Expr.quote(Expr.splice(writer).writeVal(Expr.splice(value).asInstanceOf[Byte])))
          else if (Type[A] =:= Type.of[Short])
            Some(Expr.quote(Expr.splice(writer).writeVal(Expr.splice(value).asInstanceOf[Short])))
          else if (Type[A] =:= Type.of[Char])
            Some(Expr.quote(Expr.splice(writer).writeVal(Expr.splice(value).asInstanceOf[Char].toString)))
          else if (Type[A] =:= Type.of[BigDecimal])
            Some(Expr.quote(Expr.splice(writer).writeVal(Expr.splice(value).asInstanceOf[BigDecimal])))
          else if (Type[A] =:= Type.of[BigInt])
            Some(Expr.quote(Expr.splice(writer).writeVal(Expr.splice(value).asInstanceOf[BigInt])))
          else if (Type[A] =:= CTypes.Instant)
            Some(Expr.quote(Expr.splice(writer).writeVal(Expr.splice(value).asInstanceOf[java.time.Instant].toString)))
          else if (Type[A] =:= CTypes.LocalDate)
            Some(
              Expr.quote(Expr.splice(writer).writeVal(Expr.splice(value).asInstanceOf[java.time.LocalDate].toString))
            )
          else if (Type[A] =:= CTypes.LocalTime)
            Some(
              Expr.quote(Expr.splice(writer).writeVal(Expr.splice(value).asInstanceOf[java.time.LocalTime].toString))
            )
          else if (Type[A] =:= CTypes.LocalDateTime)
            Some(
              Expr.quote(
                Expr.splice(writer).writeVal(Expr.splice(value).asInstanceOf[java.time.LocalDateTime].toString)
              )
            )
          else if (Type[A] =:= CTypes.OffsetDateTime)
            Some(
              Expr.quote(
                Expr.splice(writer).writeVal(Expr.splice(value).asInstanceOf[java.time.OffsetDateTime].toString)
              )
            )
          else if (Type[A] =:= CTypes.ZonedDateTime)
            Some(
              Expr.quote(
                Expr.splice(writer).writeVal(Expr.splice(value).asInstanceOf[java.time.ZonedDateTime].toString)
              )
            )
          else if (Type[A] =:= CTypes.Duration)
            Some(
              Expr.quote(Expr.splice(writer).writeVal(Expr.splice(value).asInstanceOf[java.time.Duration].toString))
            )
          else if (Type[A] =:= CTypes.Period)
            Some(
              Expr.quote(Expr.splice(writer).writeVal(Expr.splice(value).asInstanceOf[java.time.Period].toString))
            )
          else
            None

        MIO.pure(result match {
          case Some(expr) => Rule.matched(expr)
          case None       => Rule.yielded(s"The type ${Type[A].prettyPrint} is not a built-in type")
        })
      }
  }

  object EncHandleAsValueTypeRule extends EncoderDerivationRule("handle as value type when possible") {

    def apply[A: EncoderCtx]: MIO[Rule.Applicability[Expr[Unit]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as a value type") >> {
        Type[A] match {
          case IsValueType(isValueType) =>
            import isValueType.Underlying as Inner
            val unwrappedExpr = isValueType.value.unwrap(ectx.value)
            for {
              innerResult <- deriveEncoderRecursively[Inner](using ectx.nest(unwrappedExpr))
            } yield Rule.matched(innerResult)

          case _ =>
            MIO.pure(Rule.yielded(s"The type ${Type[A].prettyPrint} is not a value type"))
        }
      }
  }

  object EncHandleAsOptionRule extends EncoderDerivationRule("handle as Option when possible") {
    implicit val UnitT: Type[Unit] = CTypes.Unit

    def apply[A: EncoderCtx]: MIO[Rule.Applicability[Expr[Unit]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as Option") >> {
        Type[A] match {
          case IsOption(isOption) =>
            import isOption.Underlying as Inner
            LambdaBuilder
              .of1[Inner]("inner")
              .traverse { innerExpr =>
                deriveEncoderRecursively[Inner](using ectx.nest(innerExpr))
              }
              .map { builder =>
                val lambda = builder.build[Unit]
                Rule.matched(
                  isOption.value.fold[Unit](ectx.value)(
                    onEmpty = Expr.quote(Expr.splice(ectx.writer).writeNull()),
                    onSome = innerExpr =>
                      Expr.quote {
                        Expr.splice(lambda).apply(Expr.splice(innerExpr))
                      }
                  )
                )
              }

          case _ =>
            MIO.pure(Rule.yielded(s"The type ${Type[A].prettyPrint} is not an Option"))
        }
      }
  }

  @scala.annotation.nowarn("msg=Infinite loop")
  object EncHandleAsMapRule extends EncoderDerivationRule("handle as map when possible") {
    implicit val UnitT: Type[Unit] = CTypes.Unit
    implicit val StringT: Type[String] = CTypes.String
    implicit val JsonWriterT: Type[JsonWriter] = CTypes.JsonWriter

    def apply[A: EncoderCtx]: MIO[Rule.Applicability[Expr[Unit]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as a map") >> {
        Type[A] match {
          case IsMap(isMap) =>
            import isMap.Underlying as Pair
            deriveMapEntries[A, Pair](isMap.value)

          case _ =>
            MIO.pure(Rule.yielded(s"The type ${Type[A].prettyPrint} is not a map"))
        }
      }

    private def deriveMapEntries[A: EncoderCtx, Pair: Type](
        isMap: IsMapOf[A, Pair]
    ): MIO[Rule.Applicability[Expr[Unit]]] = {
      import isMap.{Key, Value}
      @scala.annotation.nowarn("msg=is never used")
      implicit val JsoniterConfigT: Type[JsoniterConfig] = CTypes.JsoniterConfig
      if (Key <:< Type[String]) {
        // String keys — derive value encoder, plus key-as-value encoder for mapAsArray
        LambdaBuilder
          .of1[Value]("mapValue")
          .traverse { valueExpr =>
            deriveEncoderRecursively[Value](using ectx.nest(valueExpr))
          }
          .flatMap { valueBuilder =>
            val valueLambda = valueBuilder.build[Unit]
            // Derive a key-as-value encoder for String (for mapAsArray mode)
            LambdaBuilder
              .of1[Key]("keyAsValue")
              .traverse { keyExpr =>
                // String keys: write as string value
                MIO.pure(Expr.quote {
                  Expr.splice(ectx.writer).writeVal(Expr.splice(keyExpr).asInstanceOf[String])
                })
              }
              .map { keyValueBuilder =>
                val keyValueLambda = keyValueBuilder.build[Unit]
                val iterableExpr = isMap.asIterable(ectx.value)
                Rule.matched(Expr.quote {
                  if (Expr.splice(ectx.config).mapAsArray) {
                    JsoniterDerivationUtils.writeMapAsArray[Key, Value](
                      Expr.splice(ectx.writer),
                      Expr.splice(iterableExpr).asInstanceOf[Iterable[(Key, Value)]],
                      Expr.splice(keyValueLambda),
                      Expr.splice(valueLambda)
                    )
                  } else {
                    JsoniterDerivationUtils.writeMapStringKeyed[Value](
                      Expr.splice(ectx.writer),
                      Expr.splice(iterableExpr).asInstanceOf[Iterable[(String, Value)]],
                      Expr.splice(valueLambda)
                    )
                  }
                })
              }
          }
      } else {
        // Non-String keys — try to derive key encoding for object mode
        deriveKeyEncoding[Key].flatMap {
          case Some(keyEncoderLambda) =>
            // Derive value encoder for V
            LambdaBuilder
              .of1[Value]("mapValue")
              .traverse { valueExpr =>
                deriveEncoderRecursively[Value](using ectx.nest(valueExpr))
              }
              .flatMap { valueBuilder =>
                val valueLambda = valueBuilder.build[Unit]
                // Derive key-as-value encoder for mapAsArray mode
                LambdaBuilder
                  .of1[Key]("keyAsValue")
                  .traverse { keyExpr =>
                    deriveEncoderRecursively[Key](using ectx.nest(keyExpr))
                  }
                  .map { keyValueBuilder =>
                    val keyValueLambda = keyValueBuilder.build[Unit]
                    val iterableExpr = isMap.asIterable(ectx.value)
                    Rule.matched(Expr.quote {
                      if (Expr.splice(ectx.config).mapAsArray) {
                        JsoniterDerivationUtils.writeMapAsArray[Key, Value](
                          Expr.splice(ectx.writer),
                          Expr.splice(iterableExpr).asInstanceOf[Iterable[(Key, Value)]],
                          Expr.splice(keyValueLambda),
                          Expr.splice(valueLambda)
                        )
                      } else {
                        JsoniterDerivationUtils.writeMapWithKeyEncoder[Key, Value](
                          Expr.splice(ectx.writer),
                          Expr.splice(iterableExpr).asInstanceOf[Iterable[(Key, Value)]],
                          Expr.splice(keyEncoderLambda),
                          Expr.splice(valueLambda)
                        )
                      }
                    })
                  }
              }
          case None =>
            // No key encoder — try value-level encoding for mapAsArray-only support
            LambdaBuilder
              .of1[Value]("mapValue")
              .traverse { valueExpr =>
                deriveEncoderRecursively[Value](using ectx.nest(valueExpr))
              }
              .flatMap { valueBuilder =>
                val valueLambda = valueBuilder.build[Unit]
                LambdaBuilder
                  .of1[Key]("keyAsValue")
                  .traverse { keyExpr =>
                    deriveEncoderRecursively[Key](using ectx.nest(keyExpr))
                  }
                  .map { keyValueBuilder =>
                    val keyValueLambda = keyValueBuilder.build[Unit]
                    val iterableExpr = isMap.asIterable(ectx.value)
                    Rule.matched(Expr.quote {
                      if (Expr.splice(ectx.config).mapAsArray) {
                        JsoniterDerivationUtils.writeMapAsArray[Key, Value](
                          Expr.splice(ectx.writer),
                          Expr.splice(iterableExpr).asInstanceOf[Iterable[(Key, Value)]],
                          Expr.splice(keyValueLambda),
                          Expr.splice(valueLambda)
                        )
                      } else {
                        throw new IllegalArgumentException(
                          "Map key type " + Expr.splice(Expr(Key.prettyPrint)) +
                            " cannot be used as JSON object key. Use mapAsArray config option."
                        )
                      }
                    })
                  }
              }
              .recoverWith { _ =>
                MIO.pure(
                  Rule.yielded(
                    s"Map key type ${Key.prettyPrint} is not String and no key encoder could be derived"
                  )
                )
              }
        }
      }
    }

    /** Try to derive a (K, JsonWriter) => Unit function for map key encoding. Returns None if derivation fails. */
    @scala.annotation.nowarn("msg=is never used")
    private[compiletime] def deriveKeyEncoding[K: Type](implicit
        ctx: EncoderCtx[?]
    ): MIO[Option[Expr[(K, JsonWriter) => Unit]]] =

      Log.info(s"Attempting to derive key encoder for ${Type[K].prettyPrint}") >> {
        // 1. Built-in types with native writeKey overloads
        val builtIn: Option[MIO[Option[Expr[(K, JsonWriter) => Unit]]]] = {
          def makeKeyEncoder(
              body: (Expr[K], Expr[JsonWriter]) => Expr[Unit]
          ): MIO[Option[Expr[(K, JsonWriter) => Unit]]] =
            LambdaBuilder
              .of2[K, JsonWriter]("key", "writer")
              .traverse { case (keyExpr, writerExpr) =>
                MIO.pure(body(keyExpr, writerExpr))
              }
              .map(builder => Some(builder.build[Unit]): Option[Expr[(K, JsonWriter) => Unit]])

          if (Type[K] =:= Type.of[Int])
            Some(makeKeyEncoder((k, w) => Expr.quote(Expr.splice(w).writeKey(Expr.splice(k).asInstanceOf[Int]))))
          else if (Type[K] =:= Type.of[Long])
            Some(makeKeyEncoder((k, w) => Expr.quote(Expr.splice(w).writeKey(Expr.splice(k).asInstanceOf[Long]))))
          else if (Type[K] =:= Type.of[Double])
            Some(makeKeyEncoder((k, w) => Expr.quote(Expr.splice(w).writeKey(Expr.splice(k).asInstanceOf[Double]))))
          else if (Type[K] =:= Type.of[Float])
            Some(makeKeyEncoder((k, w) => Expr.quote(Expr.splice(w).writeKey(Expr.splice(k).asInstanceOf[Float]))))
          else if (Type[K] =:= Type.of[Short])
            Some(makeKeyEncoder((k, w) => Expr.quote(Expr.splice(w).writeKey(Expr.splice(k).asInstanceOf[Short]))))
          else if (Type[K] =:= Type.of[Boolean])
            Some(makeKeyEncoder((k, w) => Expr.quote(Expr.splice(w).writeKey(Expr.splice(k).asInstanceOf[Boolean]))))
          else if (Type[K] =:= Type.of[BigDecimal])
            Some(makeKeyEncoder((k, w) => Expr.quote(Expr.splice(w).writeKey(Expr.splice(k).asInstanceOf[BigDecimal]))))
          else if (Type[K] =:= Type.of[BigInt])
            Some(makeKeyEncoder((k, w) => Expr.quote(Expr.splice(w).writeKey(Expr.splice(k).asInstanceOf[BigInt]))))
          else
            None
        }

        builtIn.getOrElse {
          // 2. Try summoning user-provided JsonKeyCodec[K]
          CTypes.JsonKeyCodec[K].summonExprIgnoring().toEither match {
            case Right(keyCodecExpr) =>
              Log.info(s"Found implicit JsonKeyCodec[${Type[K].prettyPrint}]") >>
                LambdaBuilder
                  .of2[K, JsonWriter]("key", "writer")
                  .traverse { case (keyExpr, writerExpr) =>
                    MIO.pure(Expr.quote {
                      Expr.splice(keyCodecExpr).encodeKey(Expr.splice(keyExpr), Expr.splice(writerExpr))
                    })
                  }
                  .map(builder => Some(builder.build[Unit]): Option[Expr[(K, JsonWriter) => Unit]])
            case Left(_) =>
              // 3. Value type — unwrap to inner, recurse
              Type[K] match {
                case IsValueType(isValueType) =>
                  import isValueType.Underlying as Inner
                  deriveKeyEncoding[Inner].flatMap {
                    case Some(innerKeyEncoder) =>
                      LambdaBuilder
                        .of2[K, JsonWriter]("key", "writer")
                        .traverse { case (keyExpr, writerExpr) =>
                          val unwrapped = isValueType.value.unwrap(keyExpr)
                          MIO.pure(Expr.quote {
                            Expr.splice(innerKeyEncoder).apply(Expr.splice(unwrapped), Expr.splice(writerExpr))
                          })
                        }
                        .map(builder => Some(builder.build[Unit]): Option[Expr[(K, JsonWriter) => Unit]])
                    case None => MIO.pure(None)
                  }
                case _ =>
                  // 4. Enum (all case objects) — write case name as key
                  Enum.parse[K].toOption match {
                    case Some(enumm) =>
                      val childrenList = enumm.directChildren.toList
                      val allCaseObjects = Type[K].isEnumeration || Type[K].isJavaEnum || childrenList.forall {
                        case (_, child) =>
                          SingletonValue.unapply(child.Underlying).isDefined
                      }
                      if (allCaseObjects) {
                        // Use .toString on case objects — for case objects, .toString returns the simple name.
                        // Use adtLeafClassNameMapper for custom name mapping.
                        LambdaBuilder
                          .of2[K, JsonWriter]("key", "writer")
                          .traverse { case (keyExpr, writerExpr) =>
                            MIO.pure(Expr.quote {
                              Expr
                                .splice(writerExpr)
                                .writeKey(
                                  Expr.splice(ctx.config).adtLeafClassNameMapper(Expr.splice(keyExpr).toString)
                                )
                            })
                          }
                          .map(builder => Some(builder.build[Unit]): Option[Expr[(K, JsonWriter) => Unit]])
                      } else MIO.pure(None)
                    case None => MIO.pure(None)
                  }
              }
          }
        }
      }
  }

  object EncHandleAsCollectionRule extends EncoderDerivationRule("handle as collection when possible") {
    implicit val UnitT: Type[Unit] = CTypes.Unit

    def apply[A: EncoderCtx]: MIO[Rule.Applicability[Expr[Unit]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as a collection") >> {
        Type[A] match {
          case IsCollection(isCollection) =>
            import isCollection.Underlying as Item
            LambdaBuilder
              .of1[Item]("item")
              .traverse { itemExpr =>
                deriveEncoderRecursively[Item](using ectx.nest(itemExpr))
              }
              .map { builder =>
                val lambda = builder.build[Unit]
                val iterableExpr = isCollection.value.asIterable(ectx.value)
                Rule.matched(Expr.quote {
                  JsoniterDerivationUtils.writeArray[Item](
                    Expr.splice(ectx.writer),
                    Expr.splice(iterableExpr),
                    Expr.splice(lambda)
                  )
                })
              }

          case _ =>
            MIO.pure(Rule.yielded(s"The type ${Type[A].prettyPrint} is not a collection"))
        }
      }
  }

  object EncHandleAsNamedTupleRule extends EncoderDerivationRule("handle as named tuple when possible") {

    def apply[A: EncoderCtx]: MIO[Rule.Applicability[Expr[Unit]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as a named tuple") >> {
        NamedTuple.parse[A].toEither match {
          case Right(namedTuple) =>
            for {
              _ <- ectx.setHelper[A] { (value, writer, config) =>
                encodeNamedTupleFields[A](namedTuple.primaryConstructor)(using ectx.nestInCache(value, writer, config))
              }
              result <- ectx.getHelper[A].flatMap {
                case Some(helperCall) => MIO.pure(Rule.matched(helperCall(ectx.value, ectx.writer, ectx.config)))
                case None             => MIO.pure(Rule.yielded(s"Failed to build helper for ${Type[A].prettyPrint}"))
              }
            } yield result
          case Left(reason) =>
            MIO.pure(Rule.yielded(reason))
        }
      }

    @scala.annotation.nowarn("msg=is never used")
    private def encodeNamedTupleFields[A: EncoderCtx](
        constructor: Method.NoInstance[A]
    ): MIO[Expr[Unit]] = {
      implicit val StringT: Type[String] = CTypes.String
      implicit val JsonWriterT: Type[JsonWriter] = CTypes.JsonWriter
      implicit val UnitT: Type[Unit] = CTypes.Unit
      implicit val ProductType: Type[Product] = CTypes.Product
      implicit val IntType: Type[Int] = CTypes.Int

      val fields = constructor.parameters.flatten.toList

      val fieldsEnc = NonEmptyList.fromList(fields) match {
        case Some(fieldValues) =>
          fieldValues
            .parTraverse { case (fName, param) =>
              import param.tpe.Underlying as Field
              val fieldExpr: Expr[Field] = Expr.quote {
                Expr
                  .splice(ectx.value)
                  .asInstanceOf[Product]
                  .productElement(Expr.splice(Expr(param.index)))
                  .asInstanceOf[Field]
              }
              Log.namedScope(s"Encoding named tuple field $fName: ${Type[Field].prettyPrint}") {
                deriveEncoderRecursively[Field](using ectx.nest(fieldExpr)).map { fieldEnc =>
                  (fName, fieldEnc)
                }
              }
            }
            .map { fieldPairs =>
              fieldPairs.toList
                .map { case (fName, fieldEnc) =>
                  Expr.quote {
                    Expr
                      .splice(ectx.writer)
                      .writeKey(Expr.splice(ectx.config).fieldNameMapper(Expr.splice(Expr(fName))))
                    Expr.splice(fieldEnc)
                  }
                }
                .foldLeft(Expr.quote(()): Expr[Unit]) { (acc, field) =>
                  Expr.quote {
                    Expr.splice(acc)
                    Expr.splice(field)
                  }
                }
            }
        case None =>
          MIO.pure(Expr.quote(()): Expr[Unit])
      }

      fieldsEnc.map { innerExpr =>
        Expr.quote {
          Expr.splice(ectx.writer).writeObjectStart()
          Expr.splice(innerExpr)
          Expr.splice(ectx.writer).writeObjectEnd()
        }
      }
    }
  }

  object EncHandleAsSingletonRule extends EncoderDerivationRule("handle as singleton when possible") {

    def apply[A: EncoderCtx]: MIO[Rule.Applicability[Expr[Unit]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as a singleton") >> {
        SingletonValue.parse[A].toEither match {
          case Right(_) =>
            MIO.pure(Rule.matched(Expr.quote {
              Expr.splice(ectx.writer).writeObjectStart()
              Expr.splice(ectx.writer).writeObjectEnd()
            }))
          case Left(reason) =>
            MIO.pure(Rule.yielded(reason))
        }
      }
  }

  object EncHandleAsCaseClassRule extends EncoderDerivationRule("handle as case class when possible") {

    def apply[A: EncoderCtx]: MIO[Rule.Applicability[Expr[Unit]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as a case class") >> {
        CaseClass.parse[A].toEither match {
          case Right(caseClass) =>
            for {
              _ <- ectx.setHelper[A] { (value, writer, config) =>
                encodeCaseClassFields[A](caseClass)(using ectx.nestInCache(value, writer, config))
              }
              result <- ectx.getHelper[A].flatMap {
                case Some(helperCall) => MIO.pure(Rule.matched(helperCall(ectx.value, ectx.writer, ectx.config)))
                case None             => MIO.pure(Rule.yielded(s"Failed to build helper for ${Type[A].prettyPrint}"))
              }
            } yield result

          case Left(reason) =>
            MIO.pure(Rule.yielded(reason))
        }
      }

    /** Encode only the key-value field pairs (no writeObjectStart/writeObjectEnd). */
    @scala.annotation.nowarn("msg=is never used")
    private[compiletime] def encodeCaseClassFieldsOnly[A: EncoderCtx](
        caseClass: CaseClass[A]
    ): MIO[Expr[Unit]] = {
      implicit val StringT: Type[String] = CTypes.String
      implicit val JsonWriterT: Type[JsonWriter] = CTypes.JsonWriter
      implicit val UnitT: Type[Unit] = CTypes.Unit
      implicit val AnyT: Type[Any] = CTypes.Any
      implicit val fieldNameT: Type[fieldNameAnn] = CTypes.FieldName
      implicit val transientFieldT: Type[transientField] = CTypes.TransientField
      implicit val stringifiedT: Type[stringified] = CTypes.Stringified

      val allFields = caseClass.caseFieldValuesAt(ectx.value).toList

      // Singletons (case objects, parameterless enum cases) have no primary constructor.
      // Only access primaryConstructor when there are actual fields to process.
      val paramsByName: Map[String, Parameter] =
        if (allFields.isEmpty) Map.empty
        else caseClass.primaryConstructor.parameters.flatten.toMap

      // Validate: @transientField on fields without defaults is a compile error
      paramsByName.collectFirst {
        case (name, param) if hasAnnotationType[transientField](param) && !param.hasDefault => name
      } match {
        case Some(name) =>
          val err = CodecDerivationError.TransientFieldMissingDefault(name, Type[A].prettyPrint)
          Log.error(err.message) >> MIO.fail(err)
        case None =>
          val nonTransientFields = allFields.filter { case (name, _) =>
            paramsByName.get(name).forall(p => !hasAnnotationType[transientField](p))
          }

          NonEmptyList.fromList(nonTransientFields) match {
            case Some(nonEmptyFields) =>
              nonEmptyFields
                .parTraverse { case (fName, fieldValue) =>
                  import fieldValue.{Underlying as Field, value as fieldExpr}
                  Log.namedScope(s"Encoding field ${ectx.value.prettyPrint}.$fName: ${Type[Field].prettyPrint}") {
                    val hasStringifiedAnnotation =
                      paramsByName.get(fName).exists(p => hasAnnotationType[stringified](p))
                    val fieldEncMIO: MIO[Expr[Unit]] = if (hasStringifiedAnnotation) {
                      deriveStringifiedEncoder[Field](fieldExpr, ectx.writer) match {
                        case Some(enc) => MIO.pure(enc)
                        case None      =>
                          val err = CodecDerivationError
                            .StringifiedOnNonNumeric(fName, Type[A].prettyPrint, Type[Field].prettyPrint)
                          Log.error(err.message) >> MIO.fail(err)
                      }
                    } else {
                      // When no @stringified annotation, check if global isStringified applies
                      deriveStringifiedEncoder[Field](fieldExpr, ectx.writer) match {
                        case Some(stringifiedEnc) =>
                          // Numeric field: generate both paths, select at runtime
                          deriveEncoderRecursively[Field](using ectx.nest(fieldExpr)).map { normalEnc =>
                            Expr.quote {
                              if (Expr.splice(ectx.config).isStringified)
                                Expr.splice(stringifiedEnc)
                              else
                                Expr.splice(normalEnc)
                            }
                          }
                        case None =>
                          // Non-numeric field: always normal encoding
                          deriveEncoderRecursively[Field](using ectx.nest(fieldExpr))
                      }
                    }
                    fieldEncMIO.map { fieldEnc =>
                      val nameOverride =
                        paramsByName.get(fName).flatMap(p => getAnnotationStringArg[fieldNameAnn](p))

                      // Build the writeKey + encode expression
                      val unconditionalWrite: Expr[Unit] = nameOverride match {
                        case Some(customName) =>
                          Expr.quote {
                            Expr.splice(ectx.writer).writeKey(Expr.splice(Expr(customName)))
                            Expr.splice(fieldEnc)
                          }
                        case None =>
                          Expr.quote {
                            Expr
                              .splice(ectx.writer)
                              .writeKey(Expr.splice(ectx.config).fieldNameMapper(Expr.splice(Expr(fName))))
                            Expr.splice(fieldEnc)
                          }
                      }

                      // Determine transient skip conditions based on field type
                      val param = paramsByName.get(fName)
                      val defaultAsAnyOpt: Option[Expr[Any]] = param.filter(_.hasDefault).flatMap { p =>
                        p.defaultValue.flatMap { existentialOuter =>
                          val methodOf = existentialOuter.value
                          methodOf.value match {
                            case noInstance: Method.NoInstance[?] =>
                              import noInstance.Returned
                              noInstance(Map.empty).toOption.map(_.upcast[Any])
                            case _ => None
                          }
                        }
                      }

                      val isOptionField = Type[Field] match {
                        case IsOption(_) => true
                        case _           => false
                      }
                      val isStringField = Type[Field] =:= CTypes.String
                      val isCollectionField = !isOptionField && {
                        val isMap = Type[Field] match { case IsMap(_) => true; case _ => false }
                        val isColl = Type[Field] match { case IsCollection(_) => true; case _ => false }
                        isMap || isColl
                      }
                      val isEmptyCapable = isStringField || isCollectionField

                      // Build skip conditions (only applicable ones per field type)
                      val conditions = List.newBuilder[Expr[Boolean]]

                      defaultAsAnyOpt.foreach { defaultExpr =>
                        conditions += Expr.quote {
                          Expr.splice(ectx.config).transientDefault &&
                          Expr.splice(fieldExpr).asInstanceOf[Any] == Expr.splice(defaultExpr)
                        }
                      }

                      if (isOptionField) {
                        conditions += Expr.quote {
                          Expr.splice(ectx.config).transientNone &&
                          !Expr.splice(fieldExpr).asInstanceOf[Option[Any]].isDefined
                        }
                      }

                      if (isEmptyCapable) {
                        if (isStringField) {
                          conditions += Expr.quote {
                            Expr.splice(ectx.config).transientEmpty &&
                            Expr.splice(fieldExpr).asInstanceOf[String].isEmpty
                          }
                        } else {
                          // Collection or Map — both extend Iterable
                          conditions += Expr.quote {
                            Expr.splice(ectx.config).transientEmpty &&
                            Expr.splice(fieldExpr).asInstanceOf[Iterable[Any]].isEmpty
                          }
                        }
                      }

                      val condList = conditions.result()
                      if (condList.isEmpty) {
                        unconditionalWrite
                      } else {
                        val skipExpr = condList.reduce { (a, b) =>
                          Expr.quote(Expr.splice(a) || Expr.splice(b))
                        }
                        Expr.quote {
                          if (!Expr.splice(skipExpr)) {
                            Expr.splice(unconditionalWrite)
                          }
                        }
                      }
                    }
                  }
                }
                .map { fieldExprs =>
                  fieldExprs.toList
                    .foldLeft(Expr.quote(()): Expr[Unit]) { (acc, field) =>
                      Expr.quote {
                        Expr.splice(acc)
                        Expr.splice(field)
                      }
                    }
                }

            case None =>
              MIO.pure(Expr.quote(()): Expr[Unit])
          }
      }
    }

    @scala.annotation.nowarn("msg=is never used")
    private def deriveStringifiedEncoder[F: Type](value: Expr[F], writer: Expr[JsonWriter])(implicit
        JsonWriterT: Type[JsonWriter],
        UnitT: Type[Unit]
    ): Option[Expr[Unit]] =
      if (Type[F] =:= CTypes.Int)
        Some(Expr.quote {
          JsoniterDerivationUtils.writeStringifiedInt(Expr.splice(writer), Expr.splice(value).asInstanceOf[Int])
        })
      else if (Type[F] =:= CTypes.Long)
        Some(Expr.quote {
          JsoniterDerivationUtils.writeStringifiedLong(Expr.splice(writer), Expr.splice(value).asInstanceOf[Long])
        })
      else if (Type[F] =:= CTypes.Double)
        Some(Expr.quote {
          JsoniterDerivationUtils.writeStringifiedDouble(Expr.splice(writer), Expr.splice(value).asInstanceOf[Double])
        })
      else if (Type[F] =:= CTypes.Float)
        Some(Expr.quote {
          JsoniterDerivationUtils.writeStringifiedFloat(Expr.splice(writer), Expr.splice(value).asInstanceOf[Float])
        })
      else if (Type[F] =:= CTypes.Short)
        Some(Expr.quote {
          JsoniterDerivationUtils.writeStringifiedShort(Expr.splice(writer), Expr.splice(value).asInstanceOf[Short])
        })
      else if (Type[F] =:= CTypes.Byte)
        Some(Expr.quote {
          JsoniterDerivationUtils.writeStringifiedByte(Expr.splice(writer), Expr.splice(value).asInstanceOf[Byte])
        })
      else if (Type[F] =:= CTypes.BigDecimal)
        Some(Expr.quote {
          JsoniterDerivationUtils
            .writeStringifiedBigDecimal(Expr.splice(writer), Expr.splice(value).asInstanceOf[BigDecimal])
        })
      else if (Type[F] =:= CTypes.BigInt)
        Some(Expr.quote {
          JsoniterDerivationUtils
            .writeStringifiedBigInt(Expr.splice(writer), Expr.splice(value).asInstanceOf[BigInt])
        })
      else None

    /** Encode a full JSON object: writeObjectStart + fields + writeObjectEnd. */
    @scala.annotation.nowarn("msg=is never used")
    private def encodeCaseClassFields[A: EncoderCtx](
        caseClass: CaseClass[A]
    ): MIO[Expr[Unit]] =
      encodeCaseClassFieldsOnly(caseClass).map { fieldsExpr =>
        Expr.quote {
          Expr.splice(ectx.writer).writeObjectStart()
          Expr.splice(fieldsExpr)
          Expr.splice(ectx.writer).writeObjectEnd()
        }
      }
  }

  object EncHandleAsEnumRule extends EncoderDerivationRule("handle as enum when possible") {

    def apply[A: EncoderCtx]: MIO[Rule.Applicability[Expr[Unit]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as an enum") >> {
        Enum.parse[A].toEither match {
          case Right(enumm) =>
            for {
              _ <- ectx.setHelper[A] { (value, writer, config) =>
                encodeEnumCases[A](enumm)(using ectx.nestInCache(value, writer, config))
              }
              result <- ectx.getHelper[A].flatMap {
                case Some(helperCall) => MIO.pure(Rule.matched(helperCall(ectx.value, ectx.writer, ectx.config)))
                case None             => MIO.pure(Rule.yielded(s"Failed to build helper for ${Type[A].prettyPrint}"))
              }
            } yield result
          case Left(reason) =>
            MIO.pure(Rule.yielded(reason))
        }
      }

    @scala.annotation.nowarn("msg=is never used")
    private def encodeEnumCases[A: EncoderCtx](
        enumm: Enum[A]
    ): MIO[Expr[Unit]] = {
      implicit val UnitT: Type[Unit] = CTypes.Unit
      implicit val JsonWriterT: Type[JsonWriter] = CTypes.JsonWriter
      implicit val StringT: Type[String] = CTypes.String

      // Check at compile time if all children are singletons (case objects with no fields)
      val childrenList = enumm.directChildren.toList
      val isEnumerationOrJavaEnum = Type[A].isEnumeration || Type[A].isJavaEnum
      val allCaseObjects = isEnumerationOrJavaEnum || childrenList.forall { case (_, child) =>
        SingletonValue.unapply(child.Underlying).isDefined
      }

      enumm
        .parMatchOn[MIO, Unit](ectx.value) { matched =>
          import matched.{value as enumCaseValue, Underlying as EnumCase}
          Log.namedScope(s"Encoding enum case ${enumCaseValue.prettyPrint}: ${EnumCase.prettyPrint}") {
            val caseName: String = childrenList
              .find { case (_, child) =>
                import child.Underlying as ChildType
                Type[EnumCase] <:< Type[ChildType]
              }
              .map(_._1)
              .getOrElse(Type[EnumCase].shortName)

            // For discriminator mode, we need fields-only encoding to avoid double wrapping.
            // Parse child as case class to get field-level access.
            val fieldsOnlyMIO: MIO[Expr[Unit]] =
              if (isEnumerationOrJavaEnum) MIO.pure(Expr.quote(()): Expr[Unit])
              else
                SingletonValue.unapply(Type[EnumCase]) match {
                  case Some(_) =>
                    // Singleton — no fields
                    MIO.pure(Expr.quote(()): Expr[Unit])
                  case None =>
                    CaseClass.parse[EnumCase].toOption match {
                      case Some(caseClass) =>
                        EncHandleAsCaseClassRule.encodeCaseClassFieldsOnly[EnumCase](caseClass)(using
                          ectx.nest(enumCaseValue)
                        )
                      case None =>
                        // Not a case class (e.g. case object) — no fields
                        MIO.pure(Expr.quote(()): Expr[Unit])
                    }
                }

            // Also derive the full encoding for wrapper mode
            val fullEncMIO: MIO[Expr[Unit]] =
              if (isEnumerationOrJavaEnum) MIO.pure(Expr.quote(()): Expr[Unit])
              else deriveEncoderRecursively[EnumCase](using ectx.nest(enumCaseValue))

            for {
              fieldsOnly <- fieldsOnlyMIO
              fullEnc <- fullEncMIO
            } yield
              if (allCaseObjects) {
                if (isEnumerationOrJavaEnum)
                  Expr.quote {
                    val config = Expr.splice(ectx.config)
                    if (config.useScalaEnumValueId)
                      JsoniterDerivationUtils.writeScalaEnumValueId(
                        Expr.splice(ectx.writer),
                        Expr.splice(enumCaseValue)
                      )
                    else {
                      val name = config.adtLeafClassNameMapper(Expr.splice(Expr(caseName)))
                      if (config.enumAsStrings)
                        JsoniterDerivationUtils.writeEnumAsString(Expr.splice(ectx.writer), name)
                      else
                        config.discriminatorFieldName match {
                          case Some(discriminatorField) =>
                            Expr.splice(ectx.writer).writeObjectStart()
                            Expr.splice(ectx.writer).writeKey(discriminatorField)
                            Expr.splice(ectx.writer).writeVal(name)
                            Expr.splice(ectx.writer).writeObjectEnd()
                          case None =>
                            JsoniterDerivationUtils.writeWrapped(Expr.splice(ectx.writer), name) {}
                        }
                    }
                  }
                else
                  Expr.quote {
                    val config = Expr.splice(ectx.config)
                    val name = config.adtLeafClassNameMapper(Expr.splice(Expr(caseName)))
                    if (config.enumAsStrings) {
                      JsoniterDerivationUtils.writeEnumAsString(Expr.splice(ectx.writer), name)
                    } else {
                      config.discriminatorFieldName match {
                        case Some(discriminatorField) =>
                          Expr.splice(ectx.writer).writeObjectStart()
                          Expr.splice(ectx.writer).writeKey(discriminatorField)
                          Expr.splice(ectx.writer).writeVal(name)
                          Expr.splice(fieldsOnly)
                          Expr.splice(ectx.writer).writeObjectEnd()
                        case None =>
                          JsoniterDerivationUtils.writeWrapped(Expr.splice(ectx.writer), name) {
                            Expr.splice(fullEnc)
                          }
                      }
                    }
                  }
              } else {
                val isSingletonCase = SingletonValue.unapply(Type[EnumCase]).isDefined
                Expr.quote {
                  val config = Expr.splice(ectx.config)
                  val name = config.adtLeafClassNameMapper(Expr.splice(Expr(caseName)))
                  if (config.circeLikeObjectEncoding && Expr.splice(Expr(isSingletonCase)))
                    JsoniterDerivationUtils.writeEnumAsString(Expr.splice(ectx.writer), name)
                  else
                    config.discriminatorFieldName match {
                      case Some(discriminatorField) =>
                        Expr.splice(ectx.writer).writeObjectStart()
                        Expr.splice(ectx.writer).writeKey(discriminatorField)
                        Expr.splice(ectx.writer).writeVal(name)
                        Expr.splice(fieldsOnly)
                        Expr.splice(ectx.writer).writeObjectEnd()
                      case None =>
                        JsoniterDerivationUtils.writeWrapped(Expr.splice(ectx.writer), name) {
                          Expr.splice(fullEnc)
                        }
                    }
                }
              }
          }
        }
        .flatMap {
          case Some(result) => MIO.pure(result)
          case None         =>
            val err = CodecDerivationError.NoChildrenInSealedTrait(Type[A].prettyPrint)
            Log.error(err.message) >> MIO.fail(err)
        }
    }
  }

  // Decoder Context

  final case class DecoderCtx[A](
      tpe: Type[A],
      reader: Expr[JsonReader],
      config: Expr[JsoniterConfig],
      cache: MLocal[ValDefsCache],
      derivedType: Option[??]
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
        derivedType: Option[??]
    ): DecoderCtx[A] = DecoderCtx(
      tpe = Type[A],
      reader = reader,
      config = config,
      cache = cache,
      derivedType = derivedType
    )
  }

  def dctx[A](implicit A: DecoderCtx[A]): DecoderCtx[A] = A

  implicit def currentDecoderValueType[A: DecoderCtx]: Type[A] = dctx.tpe

  abstract class DecoderDerivationRule(val name: String) extends Rule {
    def apply[A: DecoderCtx]: MIO[Rule.Applicability[Expr[A]]]
  }

  // Decoder derivation

  def deriveDecoderRecursively[A: DecoderCtx]: MIO[Expr[A]] =
    Log
      .namedScope(s"Deriving decoder for type ${Type[A].prettyPrint}") {
        Rules(
          DecUseCachedDefWhenAvailableRule,
          DecHandleAsLiteralTypeRule,
          DecUseImplicitWhenAvailableRule,
          DecHandleAsBuiltInRule,
          DecHandleAsValueTypeRule,
          DecHandleAsOptionRule,
          DecHandleAsMapRule,
          DecHandleAsCollectionRule,
          DecHandleAsNamedTupleRule,
          DecHandleAsSingletonRule,
          DecHandleAsCaseClassRule,
          DecHandleAsEnumRule
        )(_[A]).flatMap {
          case Right(result) =>
            Log.info(s"Derived decoder for ${Type[A].prettyPrint}: ${result.prettyPrint}") >>
              MIO.pure(result)
          case Left(reasons) =>
            val reasonsStrings = reasons.toListMap
              .removed(DecUseCachedDefWhenAvailableRule)
              .view
              .map { case (rule, reasons) =>
                if (reasons.isEmpty) s"The rule ${rule.name} was not applicable"
                else
                  s" - The rule ${rule.name} was not applicable, for the following reasons: ${reasons.mkString(", ")}"
              }
              .toList
            val err = CodecDerivationError.UnsupportedType(Type[A].prettyPrint, reasonsStrings)
            Log.error(err.message) >> MIO.fail(err)
        }
      }

  // Decoder Rules

  object DecUseCachedDefWhenAvailableRule extends DecoderDerivationRule("use cached def when available") {

    def apply[A: DecoderCtx]: MIO[Rule.Applicability[Expr[A]]] =
      Log.info(s"Attempting to use cached decoder for ${Type[A].prettyPrint}") >>
        dctx.getInstance[A].flatMap {
          case Some(instance) => callCachedInstance[A](instance)
          case None           =>
            dctx.getHelper[A].flatMap {
              case Some(helperCall) => callCachedHelper[A](helperCall)
              case None             => yieldUnsupported[A]
            }
        }

    private def callCachedInstance[A: DecoderCtx](
        instance: Expr[JsonValueCodec[A]]
    ): MIO[Rule.Applicability[Expr[A]]] =
      Log.info(s"Found cached codec instance for ${Type[A].prettyPrint}") >> MIO.pure(
        Rule.matched(Expr.quote {
          Expr.splice(instance).decodeValue(Expr.splice(dctx.reader), Expr.splice(instance).nullValue)
        })
      )

    private def callCachedHelper[A: DecoderCtx](
        helperCall: (Expr[JsonReader], Expr[JsoniterConfig]) => Expr[A]
    ): MIO[Rule.Applicability[Expr[A]]] =
      Log.info(s"Found cached decoder helper for ${Type[A].prettyPrint}") >> MIO.pure(
        Rule.matched(helperCall(dctx.reader, dctx.config))
      )

    private def yieldUnsupported[A: DecoderCtx]: MIO[Rule.Applicability[Expr[A]]] =
      MIO.pure(Rule.yielded(s"The type ${Type[A].prettyPrint} does not have a cached decoder"))
  }

  object DecUseImplicitWhenAvailableRule extends DecoderDerivationRule("use implicit when available") {

    lazy val ignoredImplicits: Seq[UntypedMethod] = {
      val ours = Type.of[KindlingsJsonValueCodec.type].methods.collect {
        case method if method.value.name == "derived" => method.value.asUntyped
      }
      ours
    }

    def apply[A: DecoderCtx]: MIO[Rule.Applicability[Expr[A]]] =
      Log.info(s"Attempting to use implicit JsonValueCodec for ${Type[A].prettyPrint}") >> {
        if (dctx.derivedType.exists(_.Underlying =:= Type[A]))
          MIO.pure(
            Rule.yielded(s"The type ${Type[A].prettyPrint} is the type being derived, skipping implicit search")
          )
        else
          CTypes.JsonValueCodec[A].summonExprIgnoring(ignoredImplicits*).toEither match {
            case Right(instanceExpr) => cacheAndUse[A](instanceExpr)
            case Left(reason)        => yieldUnsupported[A](reason)
          }
      }

    private def cacheAndUse[A: DecoderCtx](
        instanceExpr: Expr[JsonValueCodec[A]]
    ): MIO[Rule.Applicability[Expr[A]]] =
      Log.info(s"Found implicit codec ${instanceExpr.prettyPrint}, using directly") >>
        MIO.pure(Rule.matched(Expr.quote {
          Expr.splice(instanceExpr).decodeValue(Expr.splice(dctx.reader), Expr.splice(instanceExpr).nullValue)
        }))

    private def yieldUnsupported[A: DecoderCtx](
        reason: String
    ): MIO[Rule.Applicability[Expr[A]]] =
      MIO.pure(
        Rule.yielded(
          s"The type ${Type[A].prettyPrint} does not have an implicit JsonValueCodec instance: $reason"
        )
      )
  }

  object DecHandleAsLiteralTypeRule extends DecoderDerivationRule("handle as literal type when possible") {

    def apply[A: DecoderCtx]: MIO[Rule.Applicability[Expr[A]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as a literal type") >> {
        extractLiteralDecoder[A] match {
          case Some(expr) => MIO.pure(Rule.matched(expr))
          case None       => MIO.pure(Rule.yielded(s"The type ${Type[A].prettyPrint} is not a literal type"))
        }
      }

    private def decodeLiteral[A: DecoderCtx, U](
        codec: TypeCodec[U],
        read: Expr[JsonReader] => Expr[U]
    )(implicit exprCodec: ExprCodec[U], ut: Type[U]): Option[Expr[A]] =
      codec.fromType(Type[A]).map { e =>
        val constant: U = e.value
        Expr.quote {
          val actual = Expr.splice(read(dctx.reader))
          if (actual != Expr.splice(Expr(constant)))
            Expr
              .splice(dctx.reader)
              .decodeError(s"Expected literal value " + Expr.splice(Expr(constant)) + " but got " + actual)
          actual.asInstanceOf[A]
        }
      }

    private def extractLiteralDecoder[A: DecoderCtx]: Option[Expr[A]] = {
      implicit val StringT: Type[String] = CTypes.String
      implicit val IntT: Type[Int] = CTypes.Int
      implicit val LongT: Type[Long] = CTypes.Long
      implicit val DoubleT: Type[Double] = CTypes.Double
      implicit val BooleanT: Type[Boolean] = CTypes.Boolean
      decodeLiteral(Type.StringCodec, r => Expr.quote(Expr.splice(r).readString(null)))
        .orElse(decodeLiteral(Type.IntCodec, r => Expr.quote(Expr.splice(r).readInt())))
        .orElse(decodeLiteral(Type.LongCodec, r => Expr.quote(Expr.splice(r).readLong())))
        .orElse(decodeLiteral(Type.BooleanCodec, r => Expr.quote(Expr.splice(r).readBoolean())))
        .orElse(decodeLiteral(Type.DoubleCodec, r => Expr.quote(Expr.splice(r).readDouble())))
    }
  }

  @scala.annotation.nowarn("msg=is never used")
  object DecHandleAsBuiltInRule extends DecoderDerivationRule("handle as built-in type") {

    def apply[A: DecoderCtx]: MIO[Rule.Applicability[Expr[A]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as a built-in type") >> {
        val reader = dctx.reader

        val result: Option[Expr[A]] =
          if (Type[A] =:= Type.of[Int])
            Some(Expr.quote(Expr.splice(reader).readInt().asInstanceOf[A]))
          else if (Type[A] =:= Type.of[Long])
            Some(Expr.quote(Expr.splice(reader).readLong().asInstanceOf[A]))
          else if (Type[A] =:= Type.of[Double])
            Some(Expr.quote(Expr.splice(reader).readDouble().asInstanceOf[A]))
          else if (Type[A] =:= Type.of[Float])
            Some(Expr.quote(Expr.splice(reader).readFloat().asInstanceOf[A]))
          else if (Type[A] =:= Type.of[Boolean])
            Some(Expr.quote(Expr.splice(reader).readBoolean().asInstanceOf[A]))
          else if (Type[A] =:= Type.of[String])
            Some(Expr.quote(Expr.splice(reader).readString(null).asInstanceOf[A]))
          else if (Type[A] =:= Type.of[Byte])
            Some(Expr.quote(Expr.splice(reader).readByte().asInstanceOf[A]))
          else if (Type[A] =:= Type.of[Short])
            Some(Expr.quote(Expr.splice(reader).readShort().asInstanceOf[A]))
          else if (Type[A] =:= Type.of[Char])
            Some(Expr.quote(Expr.splice(reader).readChar().asInstanceOf[A]))
          else if (Type[A] =:= Type.of[BigDecimal])
            Some(Expr.quote {
              JsoniterDerivationUtils
                .validateBigDecimal(
                  Expr.splice(reader),
                  Expr.splice(reader).readBigDecimal(null),
                  Expr.splice(dctx.config).bigDecimalPrecision,
                  Expr.splice(dctx.config).bigDecimalScaleLimit,
                  Expr.splice(dctx.config).bigDecimalDigitsLimit
                )
                .asInstanceOf[A]
            })
          else if (Type[A] =:= Type.of[BigInt])
            Some(Expr.quote {
              JsoniterDerivationUtils
                .validateBigInt(
                  Expr.splice(reader),
                  Expr.splice(reader).readBigInt(null),
                  Expr.splice(dctx.config).bigDecimalDigitsLimit
                )
                .asInstanceOf[A]
            })
          else if (Type[A] =:= CTypes.Instant)
            Some(Expr.quote(JsoniterDerivationUtils.readInstant(Expr.splice(reader)).asInstanceOf[A]))
          else if (Type[A] =:= CTypes.LocalDate)
            Some(Expr.quote(JsoniterDerivationUtils.readLocalDate(Expr.splice(reader)).asInstanceOf[A]))
          else if (Type[A] =:= CTypes.LocalTime)
            Some(Expr.quote(JsoniterDerivationUtils.readLocalTime(Expr.splice(reader)).asInstanceOf[A]))
          else if (Type[A] =:= CTypes.LocalDateTime)
            Some(Expr.quote(JsoniterDerivationUtils.readLocalDateTime(Expr.splice(reader)).asInstanceOf[A]))
          else if (Type[A] =:= CTypes.OffsetDateTime)
            Some(Expr.quote(JsoniterDerivationUtils.readOffsetDateTime(Expr.splice(reader)).asInstanceOf[A]))
          else if (Type[A] =:= CTypes.ZonedDateTime)
            Some(Expr.quote(JsoniterDerivationUtils.readZonedDateTime(Expr.splice(reader)).asInstanceOf[A]))
          else if (Type[A] =:= CTypes.Duration)
            Some(Expr.quote(JsoniterDerivationUtils.readDuration(Expr.splice(reader)).asInstanceOf[A]))
          else if (Type[A] =:= CTypes.Period)
            Some(Expr.quote(JsoniterDerivationUtils.readPeriod(Expr.splice(reader)).asInstanceOf[A]))
          else
            None

        MIO.pure(result match {
          case Some(expr) => Rule.matched(expr)
          case None       => Rule.yielded(s"The type ${Type[A].prettyPrint} is not a built-in type")
        })
      }
  }

  object DecHandleAsValueTypeRule extends DecoderDerivationRule("handle as value type when possible") {

    def apply[A: DecoderCtx]: MIO[Rule.Applicability[Expr[A]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as a value type") >> {
        Type[A] match {
          case IsValueType(isValueType) =>
            import isValueType.Underlying as Inner

            // Build wrap lambda outside quotes to avoid staging issues with wrap.Result type
            LambdaBuilder
              .of1[Inner]("inner")
              .traverse { innerExpr =>
                MIO.pure(isValueType.value.wrap.apply(innerExpr).asInstanceOf[Expr[A]])
              }
              .flatMap { builder =>
                val wrapLambda = builder.build[A]
                // Try implicit first, fall back to recursive derivation (includes built-in types)
                CTypes
                  .JsonValueCodec[Inner]
                  .summonExprIgnoring(DecUseImplicitWhenAvailableRule.ignoredImplicits*)
                  .toEither match {
                  case Right(innerCodec) =>
                    MIO.pure(Rule.matched(Expr.quote {
                      Expr
                        .splice(wrapLambda)
                        .apply(
                          Expr
                            .splice(innerCodec)
                            .decodeValue(Expr.splice(dctx.reader), Expr.splice(innerCodec).nullValue)
                        )
                    }))
                  case Left(_) =>
                    // No implicit — derive via recursive rules (includes built-in types)
                    deriveDecoderRecursively[Inner](using dctx.nest[Inner](dctx.reader)).map { innerDecoded =>
                      Rule.matched(Expr.quote {
                        Expr.splice(wrapLambda).apply(Expr.splice(innerDecoded))
                      })
                    }
                }
              }

          case _ =>
            MIO.pure(Rule.yielded(s"The type ${Type[A].prettyPrint} is not a value type"))
        }
      }
  }

  object DecHandleAsOptionRule extends DecoderDerivationRule("handle as Option when possible") {

    def apply[A: DecoderCtx]: MIO[Rule.Applicability[Expr[A]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as Option") >> {
        Type[A] match {
          case IsOption(isOption) =>
            import isOption.Underlying as Inner
            implicit val JsonReaderT: Type[JsonReader] = CTypes.JsonReader

            LambdaBuilder
              .of1[JsonReader]("innerReader")
              .traverse { innerReaderExpr =>
                deriveDecoderRecursively[Inner](using dctx.nest[Inner](innerReaderExpr))
              }
              .map { builder =>
                val decodeFn = builder.build[Inner]
                Rule.matched(Expr.quote {
                  JsoniterDerivationUtils
                    .readOption(Expr.splice(dctx.reader))(Expr.splice(decodeFn))
                    .asInstanceOf[A]
                })
              }

          case _ =>
            MIO.pure(Rule.yielded(s"The type ${Type[A].prettyPrint} is not an Option"))
        }
      }
  }

  @scala.annotation.nowarn("msg=Infinite loop")
  object DecHandleAsMapRule extends DecoderDerivationRule("handle as map when possible") {

    def apply[A: DecoderCtx]: MIO[Rule.Applicability[Expr[A]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as a map") >> {
        Type[A] match {
          case IsMap(isMap) =>
            import isMap.Underlying as Pair
            decodeMapEntries[A, Pair](isMap.value)
          case _ =>
            MIO.pure(Rule.yielded(s"The type ${Type[A].prettyPrint} is not a map"))
        }
      }

    private def decodeMapEntries[A: DecoderCtx, Pair: Type](
        isMap: IsMapOf[A, Pair]
    ): MIO[Rule.Applicability[Expr[A]]] = {
      import isMap.{Key, Value, CtorResult}
      implicit val StringT: Type[String] = CTypes.String
      implicit val JsonReaderT: Type[JsonReader] = CTypes.JsonReader
      @scala.annotation.nowarn("msg=is never used")
      implicit val JsoniterConfigT: Type[JsoniterConfig] = CTypes.JsoniterConfig

      if (Key <:< Type[String]) {
        // String keys — derive value decoder, plus key-as-value decoder for mapAsArray
        LambdaBuilder
          .of1[JsonReader]("valueReader")
          .traverse { valueReaderExpr =>
            deriveDecoderRecursively[Value](using dctx.nest[Value](valueReaderExpr))
          }
          .flatMap { valueBuilder =>
            val decodeFn = valueBuilder.build[Value]
            // Derive key-as-value decoder for String (for mapAsArray mode)
            LambdaBuilder
              .of1[JsonReader]("keyValueReader")
              .traverse { keyReaderExpr =>
                // String keys: read as string value
                MIO.pure(Expr.quote {
                  Expr.splice(keyReaderExpr).readString(null).asInstanceOf[Key]
                })
              }
              .map { keyValueBuilder =>
                val keyValueDecodeFn = keyValueBuilder.build[Key]
                val factoryExpr = isMap.factory
                Rule.matched(Expr.quote {
                  if (Expr.splice(dctx.config).mapAsArray) {
                    JsoniterDerivationUtils
                      .readMapAsArray[Key, Value, A](
                        Expr.splice(dctx.reader),
                        Expr.splice(keyValueDecodeFn),
                        Expr.splice(decodeFn),
                        Expr.splice(factoryExpr).asInstanceOf[scala.collection.Factory[(Key, Value), A]],
                        Expr.splice(dctx.config).mapMaxInsertNumber
                      )
                      .asInstanceOf[A]
                  } else {
                    JsoniterDerivationUtils
                      .readMap[Value, A](
                        Expr.splice(dctx.reader),
                        Expr.splice(decodeFn),
                        Expr.splice(factoryExpr).asInstanceOf[scala.collection.Factory[(String, Value), A]],
                        Expr.splice(dctx.config).mapMaxInsertNumber
                      )
                      .asInstanceOf[A]
                  }
                })
              }
          }
      } else {
        // Non-String keys — try to derive key decoding for object mode
        deriveKeyDecoding[Key].flatMap {
          case Some(keyDecoderLambda) =>
            // Derive value decoder
            LambdaBuilder
              .of1[JsonReader]("valueReader")
              .traverse { valueReaderExpr =>
                deriveDecoderRecursively[Value](using dctx.nest[Value](valueReaderExpr))
              }
              .flatMap { valueBuilder =>
                val decodeFn = valueBuilder.build[Value]
                // Derive key-as-value decoder for mapAsArray mode
                LambdaBuilder
                  .of1[JsonReader]("keyValueReader")
                  .traverse { keyReaderExpr =>
                    deriveDecoderRecursively[Key](using dctx.nest[Key](keyReaderExpr))
                  }
                  .map { keyValueBuilder =>
                    val keyValueDecodeFn = keyValueBuilder.build[Key]
                    val factoryExpr = isMap.factory
                    Rule.matched(Expr.quote {
                      if (Expr.splice(dctx.config).mapAsArray) {
                        JsoniterDerivationUtils
                          .readMapAsArray[Key, Value, A](
                            Expr.splice(dctx.reader),
                            Expr.splice(keyValueDecodeFn),
                            Expr.splice(decodeFn),
                            Expr.splice(factoryExpr).asInstanceOf[scala.collection.Factory[(Key, Value), A]],
                            Expr.splice(dctx.config).mapMaxInsertNumber
                          )
                          .asInstanceOf[A]
                      } else {
                        JsoniterDerivationUtils
                          .readMapWithKeyDecoder[Key, Value, A](
                            Expr.splice(dctx.reader),
                            Expr.splice(keyDecoderLambda),
                            Expr.splice(decodeFn),
                            Expr.splice(factoryExpr).asInstanceOf[scala.collection.Factory[(Key, Value), A]],
                            Expr.splice(dctx.config).mapMaxInsertNumber
                          )
                          .asInstanceOf[A]
                      }
                    })
                  }
              }
          case None =>
            // No key decoder — try value-level decoding for mapAsArray-only support
            LambdaBuilder
              .of1[JsonReader]("valueReader")
              .traverse { valueReaderExpr =>
                deriveDecoderRecursively[Value](using dctx.nest[Value](valueReaderExpr))
              }
              .flatMap { valueBuilder =>
                val decodeFn = valueBuilder.build[Value]
                LambdaBuilder
                  .of1[JsonReader]("keyValueReader")
                  .traverse { keyReaderExpr =>
                    deriveDecoderRecursively[Key](using dctx.nest[Key](keyReaderExpr))
                  }
                  .map { keyValueBuilder =>
                    val keyValueDecodeFn = keyValueBuilder.build[Key]
                    val factoryExpr = isMap.factory
                    Rule.matched(Expr.quote {
                      if (Expr.splice(dctx.config).mapAsArray) {
                        JsoniterDerivationUtils
                          .readMapAsArray[Key, Value, A](
                            Expr.splice(dctx.reader),
                            Expr.splice(keyValueDecodeFn),
                            Expr.splice(decodeFn),
                            Expr.splice(factoryExpr).asInstanceOf[scala.collection.Factory[(Key, Value), A]],
                            Expr.splice(dctx.config).mapMaxInsertNumber
                          )
                          .asInstanceOf[A]
                      } else {
                        throw new IllegalArgumentException(
                          "Map key type " + Expr.splice(Expr(Key.prettyPrint)) +
                            " cannot be used as JSON object key. Use mapAsArray config option."
                        )
                      }
                    })
                  }
              }
              .recoverWith { _ =>
                MIO.pure(
                  Rule.yielded(
                    s"Map key type ${Key.prettyPrint} is not String and no key decoder could be derived"
                  )
                )
              }
        }
      }
    }

    /** Try to derive a JsonReader => K function for map key decoding. Returns None if derivation fails. */
    @scala.annotation.nowarn("msg=is never used")
    private[compiletime] def deriveKeyDecoding[K: Type](implicit
        ctx: DecoderCtx[?]
    ): MIO[Option[Expr[JsonReader => K]]] = {
      implicit val JsonReaderT: Type[JsonReader] = CTypes.JsonReader
      implicit val StringT: Type[String] = CTypes.String

      Log.info(s"Attempting to derive key decoder for ${Type[K].prettyPrint}") >> {
        // 1. Built-in types with native readKeyAs* overloads
        val builtIn: Option[MIO[Option[Expr[JsonReader => K]]]] = {
          def makeKeyDecoder(body: Expr[JsonReader] => Expr[K]): MIO[Option[Expr[JsonReader => K]]] =
            LambdaBuilder
              .of1[JsonReader]("reader")
              .traverse { readerExpr =>
                MIO.pure(body(readerExpr))
              }
              .map(builder => Some(builder.build[K]): Option[Expr[JsonReader => K]])

          if (Type[K] =:= Type.of[Int])
            Some(makeKeyDecoder(r => Expr.quote(Expr.splice(r).readKeyAsInt().asInstanceOf[K])))
          else if (Type[K] =:= Type.of[Long])
            Some(makeKeyDecoder(r => Expr.quote(Expr.splice(r).readKeyAsLong().asInstanceOf[K])))
          else if (Type[K] =:= Type.of[Double])
            Some(makeKeyDecoder(r => Expr.quote(Expr.splice(r).readKeyAsDouble().asInstanceOf[K])))
          else if (Type[K] =:= Type.of[Float])
            Some(makeKeyDecoder(r => Expr.quote(Expr.splice(r).readKeyAsFloat().asInstanceOf[K])))
          else if (Type[K] =:= Type.of[Short])
            Some(makeKeyDecoder(r => Expr.quote(Expr.splice(r).readKeyAsShort().asInstanceOf[K])))
          else if (Type[K] =:= Type.of[Boolean])
            Some(makeKeyDecoder(r => Expr.quote(Expr.splice(r).readKeyAsBoolean().asInstanceOf[K])))
          else if (Type[K] =:= Type.of[BigDecimal])
            Some(makeKeyDecoder(r => Expr.quote(Expr.splice(r).readKeyAsBigDecimal().asInstanceOf[K])))
          else if (Type[K] =:= Type.of[BigInt])
            Some(makeKeyDecoder(r => Expr.quote(Expr.splice(r).readKeyAsBigInt().asInstanceOf[K])))
          else
            None
        }

        builtIn.getOrElse {
          // 2. Try summoning user-provided JsonKeyCodec[K]
          CTypes.JsonKeyCodec[K].summonExprIgnoring().toEither match {
            case Right(keyCodecExpr) =>
              Log.info(s"Found implicit JsonKeyCodec[${Type[K].prettyPrint}]") >>
                LambdaBuilder
                  .of1[JsonReader]("reader")
                  .traverse { readerExpr =>
                    MIO.pure(Expr.quote {
                      Expr.splice(keyCodecExpr).decodeKey(Expr.splice(readerExpr))
                    })
                  }
                  .map(builder => Some(builder.build[K]): Option[Expr[JsonReader => K]])
            case Left(_) =>
              // 3. Value type — unwrap to inner, recurse
              Type[K] match {
                case IsValueType(isValueType) =>
                  import isValueType.Underlying as Inner
                  // Build wrap lambda outside quotes
                  LambdaBuilder
                    .of1[Inner]("inner")
                    .traverse { innerExpr =>
                      MIO.pure(isValueType.value.wrap.apply(innerExpr).asInstanceOf[Expr[K]])
                    }
                    .flatMap { wrapBuilder =>
                      val wrapLambda = wrapBuilder.build[K]
                      deriveKeyDecoding[Inner].flatMap {
                        case Some(innerKeyDecoder) =>
                          LambdaBuilder
                            .of1[JsonReader]("reader")
                            .traverse { readerExpr =>
                              MIO.pure(Expr.quote {
                                Expr
                                  .splice(wrapLambda)
                                  .apply(Expr.splice(innerKeyDecoder).apply(Expr.splice(readerExpr)))
                              })
                            }
                            .map(builder => Some(builder.build[K]): Option[Expr[JsonReader => K]])
                        case None => MIO.pure(None)
                      }
                    }
                case _ =>
                  // 4. Enum (all case objects) — read key as string, match against case names
                  Enum.parse[K].toOption match {
                    case Some(enumm) =>
                      val childrenList = enumm.directChildren.toList
                      val allCaseObjects = Type[K].isEnumeration || Type[K].isJavaEnum || childrenList.forall {
                        case (_, child) =>
                          SingletonValue.unapply(child.Underlying).isDefined
                      }
                      if (allCaseObjects) {
                        NonEmptyList.fromList(childrenList) match {
                          case None           => MIO.pure(None)
                          case Some(children) =>
                            // Build a Map[String, K] lookup expression from singletons, then use it
                            // inside the lambda to dispatch readKeyAsString() results.
                            children
                              .parTraverse { case (childName, child) =>
                                import child.Underlying as ChildType
                                SingletonValue.unapply(Type[ChildType]) match {
                                  case Some(sv) =>
                                    MIO.pure((childName, sv.singletonExpr.asInstanceOf[Expr[K]]))
                                  case None =>
                                    CaseClass.parse[ChildType].toOption match {
                                      case Some(cc) =>
                                        cc.construct[MIO](new CaseClass.ConstructField[MIO] {
                                          def apply(field: Parameter): MIO[Expr[field.tpe.Underlying]] =
                                            MIO.fail(new RuntimeException("Unexpected parameter in enum singleton"))
                                        }).flatMap {
                                          case Some(expr) => MIO.pure((childName, expr.asInstanceOf[Expr[K]]))
                                          case None       =>
                                            MIO.fail(new RuntimeException(s"Cannot construct enum case $childName"))
                                        }
                                      case None =>
                                        MIO.fail(new RuntimeException(s"Cannot construct enum case $childName"))
                                    }
                                }
                              }
                              .flatMap { casesNel =>
                                // Build Map[String, K] expression
                                val mapEntries: List[Expr[(String, K)]] = casesNel.toList.map { case (name, caseExpr) =>
                                  Expr.quote {
                                    (Expr.splice(Expr(name)), Expr.splice(caseExpr))
                                  }
                                }
                                val lookupMapExpr: Expr[Map[String, K]] = mapEntries.foldLeft(
                                  Expr.quote(Map.empty[String, K])
                                ) { (accExpr, entryExpr) =>
                                  Expr.quote(Expr.splice(accExpr) + Expr.splice(entryExpr))
                                }
                                LambdaBuilder
                                  .of1[JsonReader]("reader")
                                  .traverse { readerExpr =>
                                    MIO.pure(Expr.quote {
                                      val keyStr = Expr.splice(readerExpr).readKeyAsString()
                                      Expr
                                        .splice(lookupMapExpr)
                                        .getOrElse(
                                          keyStr,
                                          Expr.splice(readerExpr).decodeError("unknown enum key: " + keyStr): K
                                        )
                                    })
                                  }
                                  .map(builder => Some(builder.build[K]): Option[Expr[JsonReader => K]])
                              }
                        }
                      } else MIO.pure(None)
                    case None => MIO.pure(None)
                  }
              }
          }
        }
      }
    }
  }

  object DecHandleAsCollectionRule extends DecoderDerivationRule("handle as collection when possible") {

    def apply[A: DecoderCtx]: MIO[Rule.Applicability[Expr[A]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as a collection") >> {
        Type[A] match {
          case IsCollection(isCollection) =>
            import isCollection.Underlying as Item
            import isCollection.value.CtorResult
            implicit val JsonReaderT: Type[JsonReader] = CTypes.JsonReader

            LambdaBuilder
              .of1[JsonReader]("itemReader")
              .traverse { itemReaderExpr =>
                deriveDecoderRecursively[Item](using dctx.nest[Item](itemReaderExpr))
              }
              .map { builder =>
                val decodeFn = builder.build[Item]
                val factoryExpr = isCollection.value.factory
                Rule.matched(Expr.quote {
                  JsoniterDerivationUtils
                    .readCollection[Item, A](
                      Expr.splice(dctx.reader),
                      Expr.splice(decodeFn),
                      Expr.splice(factoryExpr).asInstanceOf[scala.collection.Factory[Item, A]],
                      Expr.splice(dctx.config).setMaxInsertNumber
                    )
                    .asInstanceOf[A]
                })
              }

          case _ =>
            MIO.pure(Rule.yielded(s"The type ${Type[A].prettyPrint} is not a collection"))
        }
      }
  }

  object DecHandleAsNamedTupleRule extends DecoderDerivationRule("handle as named tuple when possible") {

    def apply[A: DecoderCtx]: MIO[Rule.Applicability[Expr[A]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as a named tuple") >> {
        NamedTuple.parse[A].toEither match {
          case Right(namedTuple) =>
            for {
              _ <- dctx.setHelper[A] { (reader, config) =>
                decodeNamedTupleFields[A](namedTuple.primaryConstructor)(using dctx.nestInCache(reader, config))
              }
              result <- dctx.getHelper[A].flatMap {
                case Some(helperCall) =>
                  MIO.pure(Rule.matched(helperCall(dctx.reader, dctx.config)))
                case None =>
                  MIO.pure(Rule.yielded(s"Failed to build helper for ${Type[A].prettyPrint}"))
              }
            } yield result
          case Left(reason) =>
            MIO.pure(Rule.yielded(reason))
        }
      }

    @scala.annotation.nowarn("msg=is never used|unused explicit parameter")
    private def decodeNamedTupleFields[A: DecoderCtx](
        constructor: Method.NoInstance[A]
    ): MIO[Expr[A]] = {
      implicit val StringT: Type[String] = CTypes.String
      implicit val JsonReaderT: Type[JsonReader] = CTypes.JsonReader
      implicit val AnyT: Type[Any] = CTypes.Any
      implicit val ArrayAnyT: Type[Array[Any]] = CTypes.ArrayAny

      val fieldsList = constructor.parameters.flatten.toList
      val indexedFields = fieldsList.zipWithIndex

      NonEmptyList.fromList(indexedFields) match {
        case None =>
          constructor(Map.empty) match {
            case Right(constructExpr) =>
              MIO.pure(Expr.quote {
                JsoniterDerivationUtils.readEmptyObject(Expr.splice(dctx.reader))
                Expr.splice(constructExpr)
              })
            case Left(error) =>
              val err = CodecDerivationError.CannotConstructType(Type[A].prettyPrint, isSingleton = false, Some(error))
              Log.error(err.message) >> MIO.fail(err)
          }

        case Some(fields) =>
          fields
            .parTraverse { case ((fName, param), index) =>
              import param.tpe.Underlying as Field
              Log.namedScope(s"Deriving decoder for named tuple field $fName: ${Type[Field].prettyPrint}") {
                deriveFieldDecoder[Field].map { decodeFn =>
                  val decodeFnErased: Expr[JsonReader => Any] = Expr.quote { (r: JsonReader) =>
                    Expr.splice(decodeFn).apply(r).asInstanceOf[Any]
                  }
                  val makeAccessor: Expr[Array[Any]] => (String, Expr_??) = { arrExpr =>
                    val typedExpr = Expr.quote {
                      JsoniterDerivationUtils.unsafeCast(
                        Expr.splice(arrExpr)(Expr.splice(Expr(index))),
                        Expr.splice(decodeFn)
                      )
                    }
                    (fName, typedExpr.as_??)
                  }
                  (fName, index, decodeFnErased, makeAccessor)
                }
              }
            }
            .flatMap { fieldData =>
              val fieldDataList = fieldData.toList

              LambdaBuilder
                .of1[Array[Any]]("decodedValues")
                .traverse { decodedValuesExpr =>
                  val fieldMap: Map[String, Expr_??] =
                    fieldDataList.map(_._4(decodedValuesExpr)).toMap
                  constructor(fieldMap) match {
                    case Right(constructExpr) => MIO.pure(constructExpr)
                    case Left(error)          =>
                      val err = CodecDerivationError.CannotConstructType(
                        Type[A].prettyPrint,
                        isSingleton = false,
                        Some(error)
                      )
                      Log.error(err.message) >> MIO.fail(err)
                  }
                }
                .map { builder =>
                  val constructLambda = builder.build[A]

                  val fieldMappings = fieldDataList.map { case (name, index, decodeFnErased, _) =>
                    (name, index, decodeFnErased)
                  }

                  Expr.quote {
                    JsoniterDerivationUtils.readObject[A](
                      Expr.splice(dctx.reader),
                      Expr.splice(Expr(fieldMappings.size)),
                      Expr.splice(constructLambda)
                    ) { case (fieldName, arr, reader) =>
                      Expr.splice {
                        fieldMappings.foldRight(Expr.quote {
                          if (Expr.splice(dctx.config).skipUnexpectedFields) reader.skip()
                          else reader.decodeError("unexpected field: " + fieldName)
                        }: Expr[Unit]) { case ((name, index, decodeFnErased), elseExpr) =>
                          Expr.quote {
                            if (fieldName == Expr.splice(dctx.config).fieldNameMapper(Expr.splice(Expr(name)))) {
                              arr(Expr.splice(Expr(index))) = Expr.splice(decodeFnErased).apply(reader)
                            } else Expr.splice(elseExpr)
                          }
                        }
                      }
                    }
                  }
                }
            }
      }
    }
  }

  object DecHandleAsSingletonRule extends DecoderDerivationRule("handle as singleton when possible") {

    @scala.annotation.nowarn("msg=is never used")
    def apply[A: DecoderCtx]: MIO[Rule.Applicability[Expr[A]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as a singleton") >> {
        SingletonValue.parse[A].toEither match {
          case Right(sv) =>
            implicit val JsonReaderT: Type[JsonReader] = CTypes.JsonReader
            MIO.pure(Rule.matched(Expr.quote {
              JsoniterDerivationUtils.readEmptyObject(Expr.splice(dctx.reader))
              Expr.splice(sv.singletonExpr)
            }))
          case Left(reason) =>
            MIO.pure(Rule.yielded(reason))
        }
      }
  }

  object DecHandleAsCaseClassRule extends DecoderDerivationRule("handle as case class when possible") {

    def apply[A: DecoderCtx]: MIO[Rule.Applicability[Expr[A]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as a case class") >> {
        CaseClass.parse[A].toEither match {
          case Right(caseClass) =>
            for {
              _ <- dctx.setHelper[A] { (reader, config) =>
                decodeCaseClassFields[A](caseClass)(using dctx.nestInCache(reader, config))
              }
              result <- dctx.getHelper[A].flatMap {
                case Some(helperCall) =>
                  MIO.pure(Rule.matched(helperCall(dctx.reader, dctx.config)))
                case None =>
                  MIO.pure(Rule.yielded(s"Failed to build helper for ${Type[A].prettyPrint}"))
              }
            } yield result

          case Left(reason) =>
            MIO.pure(Rule.yielded(reason))
        }
      }

    @scala.annotation.nowarn("msg=is never used|unused explicit parameter|Non local returns")
    private def decodeCaseClassFields[A: DecoderCtx](
        caseClass: CaseClass[A]
    ): MIO[Expr[A]] = {
      implicit val StringT: Type[String] = CTypes.String
      implicit val JsonReaderT: Type[JsonReader] = CTypes.JsonReader
      implicit val fieldNameT: Type[fieldNameAnn] = CTypes.FieldName
      implicit val transientFieldT: Type[transientField] = CTypes.TransientField
      implicit val stringifiedT: Type[stringified] = CTypes.Stringified

      val constructor = caseClass.primaryConstructor
      val fieldsList = constructor.parameters.flatten.toList

      // Validate: @transientField on fields without defaults is a compile error
      fieldsList
        .collectFirst {
          case (name, param) if hasAnnotationType[transientField](param) && !param.hasDefault => name
        }
        .foreach { name =>
          val err = CodecDerivationError.TransientFieldMissingDefault(name, Type[A].prettyPrint)
          return Log.error(err.message) >> MIO.fail(err)
        }

      // Separate transient from non-transient fields
      val nonTransientFields = fieldsList.filterNot { case (_, p) => hasAnnotationType[transientField](p) }

      NonEmptyList.fromList(nonTransientFields) match {
        case None =>
          // Zero non-transient fields (either zero-param or all-transient): construct directly
          caseClass
            .construct[MIO](new CaseClass.ConstructField[MIO] {
              def apply(field: Parameter): MIO[Expr[field.tpe.Underlying]] = {
                val err = CodecDerivationError.CannotConstructType(
                  Type[A].prettyPrint,
                  isSingleton = false,
                  Some("Unexpected parameter in zero-argument case class")
                )
                Log.error(err.message) >> MIO.fail(err)
              }
            })
            .flatMap {
              case Some(expr) =>
                // Still need to read the empty object from the reader
                MIO.pure(Expr.quote {
                  JsoniterDerivationUtils.readEmptyObject(Expr.splice(dctx.reader))
                  Expr.splice(expr)
                })
              case None =>
                val err = CodecDerivationError.CannotConstructType(Type[A].prettyPrint, isSingleton = false)
                Log.error(err.message) >> MIO.fail(err)
            }

        case Some(fields) =>
          implicit val AnyT: Type[Any] = CTypes.Any
          implicit val ArrayAnyT: Type[Array[Any]] = CTypes.ArrayAny

          // Build transient field default values for the constructor
          val transientDefaults: Map[String, Expr_??] = fieldsList
            .filter { case (_, p) => hasAnnotationType[transientField](p) }
            .flatMap { case (fName, param) =>
              param.defaultValue.flatMap { existentialOuter =>
                val methodOf = existentialOuter.value
                methodOf.value match {
                  case noInstance: Method.NoInstance[?] =>
                    import noInstance.Returned
                    noInstance(Map.empty).toOption.map(expr => (fName, expr.as_??))
                  case _ => None
                }
              }
            }
            .toMap

          // Re-index non-transient fields for the array (0, 1, 2, ...)
          val nonTransientWithIndex = nonTransientFields.zipWithIndex

          // Step 1: For each non-transient field, derive a decoder and build dispatch/accessor.
          fields
            .parTraverse { case (fName, param) =>
              import param.tpe.Underlying as Field
              val nameOverride = getAnnotationStringArg[fieldNameAnn](param)
              val arrayIndex = nonTransientWithIndex.find(_._1._1 == fName).map(_._2).getOrElse(param.index)
              val hasStringifiedAnnotation = hasAnnotationType[stringified](param)
              Log.namedScope(s"Deriving decoder for field $fName: ${Type[Field].prettyPrint}") {
                val decodeFnMIO: MIO[Expr[JsonReader => Field]] = if (hasStringifiedAnnotation) {
                  deriveStringifiedDecoder[Field] match {
                    case Some(dec) => MIO.pure(dec)
                    case None      =>
                      val err = CodecDerivationError
                        .StringifiedOnNonNumeric(fName, Type[A].prettyPrint, Type[Field].prettyPrint)
                      Log.error(err.message) >> MIO.fail(err)
                  }
                } else {
                  deriveStringifiedDecoder[Field] match {
                    case Some(stringifiedDec) =>
                      deriveFieldDecoder[Field].map { normalDec =>
                        Expr.quote { (r: JsonReader) =>
                          if (Expr.splice(dctx.config).isStringified)
                            Expr.splice(stringifiedDec).apply(r)
                          else
                            Expr.splice(normalDec).apply(r)
                        }
                      }
                    case None =>
                      deriveFieldDecoder[Field]
                  }
                }
                decodeFnMIO.map { decodeFn =>
                  val decodeFnErased: Expr[JsonReader => Any] = Expr.quote { (r: JsonReader) =>
                    Expr.splice(decodeFn).apply(r).asInstanceOf[Any]
                  }
                  val makeAccessor: Expr[Array[Any]] => (String, Expr_??) = { arrExpr =>
                    val typedExpr = Expr.quote {
                      JsoniterDerivationUtils.unsafeCast(
                        Expr.splice(arrExpr)(Expr.splice(Expr(arrayIndex))),
                        Expr.splice(decodeFn)
                      )
                    }
                    (fName, typedExpr.as_??)
                  }
                  (fName, arrayIndex, decodeFnErased, makeAccessor, nameOverride)
                }
              }
            }
            .flatMap { fieldData =>
              val fieldDataList = fieldData.toList

              // Step 2: Build the constructor lambda using LambdaBuilder + primaryConstructor
              LambdaBuilder
                .of1[Array[Any]]("decodedValues")
                .traverse { decodedValuesExpr =>
                  // Build require check expressions: validate required fields before transient init
                  val requireCheckExprs: List[Expr[Unit]] =
                    nonTransientWithIndex.flatMap { case ((fName, param), idx) =>
                      import param.tpe.Underlying as Field
                      val checks = List.newBuilder[Expr[Unit]]

                      // requireDefaultFields: fields with defaults must be explicitly present
                      if (param.hasDefault) {
                        checks += Expr.quote {
                          if (
                            Expr.splice(dctx.config).requireDefaultFields &&
                            Expr.splice(decodedValuesExpr)(Expr.splice(Expr(idx))) == null
                          )
                            JsoniterDerivationUtils.throwMissingField(Expr.splice(Expr(fName)))
                        }
                      }

                      // requireCollectionFields: collection/map fields must be explicitly present
                      val isCollOrMap = Type[Field] match {
                        case IsMap(_)        => true
                        case IsCollection(_) => true
                        case _               => false
                      }
                      if (isCollOrMap) {
                        checks += Expr.quote {
                          if (
                            Expr.splice(dctx.config).requireCollectionFields &&
                            Expr.splice(decodedValuesExpr)(Expr.splice(Expr(idx))) == null
                          )
                            JsoniterDerivationUtils.throwMissingField(Expr.splice(Expr(fName)))
                        }
                      }

                      checks.result()
                    }

                  val requireCheckAll: Expr[Unit] =
                    requireCheckExprs.foldLeft(Expr.quote(()): Expr[Unit]) { (acc, step) =>
                      Expr.quote {
                        Expr.splice(acc)
                        Expr.splice(step)
                      }
                    }

                  // Build transient init expressions: fill null slots for absent fields
                  val transientInitExprs: List[Expr[Unit]] =
                    nonTransientWithIndex.flatMap { case ((fName, param), idx) =>
                      import param.tpe.Underlying as Field
                      val initSteps = List.newBuilder[Expr[Unit]]

                      // transientDefault: use field's default value
                      if (param.hasDefault) {
                        param.defaultValue
                          .flatMap { existentialOuter =>
                            val methodOf = existentialOuter.value
                            methodOf.value match {
                              case noInstance: Method.NoInstance[?] =>
                                import noInstance.Returned
                                noInstance(Map.empty).toOption.map(_.upcast[Any])
                              case _ => None
                            }
                          }
                          .foreach { defaultExpr =>
                            initSteps += Expr.quote {
                              if (
                                Expr.splice(dctx.config).transientDefault &&
                                Expr.splice(decodedValuesExpr)(Expr.splice(Expr(idx))) == null
                              )
                                Expr.splice(decodedValuesExpr)(Expr.splice(Expr(idx))) = Expr.splice(defaultExpr)
                            }
                          }
                      }

                      // transientNone: use None for Option fields
                      val isOpt = Type[Field] match {
                        case IsOption(_) => true; case _ => false
                      }
                      if (isOpt) {
                        initSteps += Expr.quote {
                          if (
                            Expr.splice(dctx.config).transientNone &&
                            Expr.splice(decodedValuesExpr)(Expr.splice(Expr(idx))) == null
                          )
                            Expr.splice(decodedValuesExpr)(Expr.splice(Expr(idx))) = (None: Any)
                        }
                      }

                      // transientEmpty: use "" for String fields
                      if (Type[Field] =:= CTypes.String) {
                        initSteps += Expr.quote {
                          if (
                            Expr.splice(dctx.config).transientEmpty &&
                            Expr.splice(decodedValuesExpr)(Expr.splice(Expr(idx))) == null
                          )
                            Expr.splice(decodedValuesExpr)(Expr.splice(Expr(idx))) = ("": Any)
                        }
                      }

                      initSteps.result()
                    }

                  val transientInitAll: Expr[Unit] =
                    transientInitExprs.foldLeft(Expr.quote(()): Expr[Unit]) { (acc, step) =>
                      Expr.quote {
                        Expr.splice(acc)
                        Expr.splice(step)
                      }
                    }

                  // Non-transient fields read from the array
                  val nonTransientFieldMap: Map[String, Expr_??] =
                    fieldDataList.map(_._4(decodedValuesExpr)).toMap
                  // Merge with transient defaults
                  val fieldMap = nonTransientFieldMap ++ transientDefaults
                  caseClass.primaryConstructor(fieldMap) match {
                    case Right(constructExpr) =>
                      MIO.pure(Expr.quote {
                        Expr.splice(requireCheckAll)
                        Expr.splice(transientInitAll)
                        Expr.splice(constructExpr)
                      })
                    case Left(error) =>
                      val err =
                        CodecDerivationError.CannotConstructType(Type[A].prettyPrint, isSingleton = false, Some(error))
                      Log.error(err.message) >> MIO.fail(err)
                  }
                }
                .map { builder =>
                  val constructLambda = builder.build[A]

                  // Step 3: Build the field dispatch - if-else chain matching mapped field names.
                  val fieldMappings = fieldDataList.map { case (name, index, decodeFnErased, _, nameOverride) =>
                    (name, index, decodeFnErased, nameOverride)
                  }

                  Expr.quote {
                    val _seen: Array[Boolean] =
                      if (Expr.splice(dctx.config).checkFieldDuplication)
                        new Array[Boolean](Expr.splice(Expr(fieldMappings.size)))
                      else null
                    JsoniterDerivationUtils.readObject[A](
                      Expr.splice(dctx.reader),
                      Expr.splice(Expr(fieldMappings.size)),
                      Expr.splice(constructLambda)
                    ) { case (fieldName, arr, reader) =>
                      Expr.splice {
                        fieldMappings.foldRight(Expr.quote {
                          if (Expr.splice(dctx.config).skipUnexpectedFields) reader.skip()
                          else reader.decodeError("unexpected field: " + fieldName)
                        }: Expr[Unit]) {
                          case ((name, index, decodeFnErased, Some(customName)), elseExpr) =>
                            Expr.quote {
                              if (fieldName == Expr.splice(Expr(customName))) {
                                if (_seen != null) {
                                  if (_seen(Expr.splice(Expr(index))))
                                    JsoniterDerivationUtils.throwDuplicateField(reader, fieldName)
                                  _seen(Expr.splice(Expr(index))) = true
                                }
                                arr(Expr.splice(Expr(index))) = Expr.splice(decodeFnErased).apply(reader)
                              } else Expr.splice(elseExpr)
                            }
                          case ((name, index, decodeFnErased, None), elseExpr) =>
                            Expr.quote {
                              if (fieldName == Expr.splice(dctx.config).fieldNameMapper(Expr.splice(Expr(name)))) {
                                if (_seen != null) {
                                  if (_seen(Expr.splice(Expr(index))))
                                    JsoniterDerivationUtils.throwDuplicateField(reader, fieldName)
                                  _seen(Expr.splice(Expr(index))) = true
                                }
                                arr(Expr.splice(Expr(index))) = Expr.splice(decodeFnErased).apply(reader)
                              } else Expr.splice(elseExpr)
                            }
                        }
                      }
                    }
                  }
                }
            }
      }
    }

    /** Decode case class fields from an already-opened JSON object (for discriminator mode). The object's `{` and
      * discriminator key-value have already been read. Returns Expr[A] that reads remaining fields via
      * readObjectInline.
      */
    @scala.annotation.nowarn("msg=is never used|unused explicit parameter|Non local returns")
    private[compiletime] def decodeCaseClassFieldsInline[A: DecoderCtx](
        caseClass: CaseClass[A]
    ): MIO[Expr[A]] = {
      implicit val StringT: Type[String] = CTypes.String
      implicit val JsonReaderT: Type[JsonReader] = CTypes.JsonReader
      implicit val fieldNameT: Type[fieldNameAnn] = CTypes.FieldName
      implicit val transientFieldT: Type[transientField] = CTypes.TransientField
      implicit val stringifiedT: Type[stringified] = CTypes.Stringified

      val constructor = caseClass.primaryConstructor
      val fieldsList = constructor.parameters.flatten.toList

      // Validate: @transientField on fields without defaults
      fieldsList
        .collectFirst {
          case (name, param) if hasAnnotationType[transientField](param) && !param.hasDefault => name
        }
        .foreach { name =>
          val err = CodecDerivationError.TransientFieldMissingDefault(name, Type[A].prettyPrint)
          return Log.error(err.message) >> MIO.fail(err)
        }

      val nonTransientFields = fieldsList.filterNot { case (_, p) => hasAnnotationType[transientField](p) }

      NonEmptyList.fromList(nonTransientFields) match {
        case None =>
          // Zero non-transient fields: just read closing `}`
          caseClass
            .construct[MIO](new CaseClass.ConstructField[MIO] {
              def apply(field: Parameter): MIO[Expr[field.tpe.Underlying]] = {
                val err = CodecDerivationError.CannotConstructType(
                  Type[A].prettyPrint,
                  isSingleton = false,
                  Some("Unexpected field in zero-arg case class")
                )
                Log.error(err.message) >> MIO.fail(err)
              }
            })
            .flatMap {
              case Some(expr) =>
                MIO.pure(Expr.quote {
                  val reader = Expr.splice(dctx.reader)
                  if (!reader.isNextToken('}'.toByte)) {
                    if (reader.isCurrentToken(','.toByte)) {
                      reader.rollbackToken()
                      while (reader.isNextToken(','.toByte)) {
                        val _ = reader.readKeyAsString()
                        reader.skip()
                      }
                    }
                  }
                  Expr.splice(expr)
                })
              case None =>
                val err = CodecDerivationError.CannotConstructType(Type[A].prettyPrint, isSingleton = false)
                Log.error(err.message) >> MIO.fail(err)
            }

        case Some(fields) =>
          implicit val AnyT: Type[Any] = CTypes.Any
          implicit val ArrayAnyT: Type[Array[Any]] = CTypes.ArrayAny

          // Build transient field default values
          val transientDefaults: Map[String, Expr_??] = fieldsList
            .filter { case (_, p) => hasAnnotationType[transientField](p) }
            .flatMap { case (fName, param) =>
              param.defaultValue.flatMap { existentialOuter =>
                val methodOf = existentialOuter.value
                methodOf.value match {
                  case noInstance: Method.NoInstance[?] =>
                    import noInstance.Returned
                    noInstance(Map.empty).toOption.map(expr => (fName, expr.as_??))
                  case _ => None
                }
              }
            }
            .toMap

          val nonTransientWithIndex = nonTransientFields.zipWithIndex

          fields
            .parTraverse { case (fName, param) =>
              import param.tpe.Underlying as Field
              val nameOverride = getAnnotationStringArg[fieldNameAnn](param)
              val arrayIndex = nonTransientWithIndex.find(_._1._1 == fName).map(_._2).getOrElse(param.index)
              val hasStringifiedAnnotation = hasAnnotationType[stringified](param)
              Log.namedScope(s"Deriving decoder for field $fName: ${Type[Field].prettyPrint}") {
                val decodeFnMIO: MIO[Expr[JsonReader => Field]] = if (hasStringifiedAnnotation) {
                  deriveStringifiedDecoder[Field] match {
                    case Some(dec) => MIO.pure(dec)
                    case None      =>
                      val err = CodecDerivationError
                        .StringifiedOnNonNumeric(fName, Type[A].prettyPrint, Type[Field].prettyPrint)
                      Log.error(err.message) >> MIO.fail(err)
                  }
                } else {
                  deriveStringifiedDecoder[Field] match {
                    case Some(stringifiedDec) =>
                      deriveFieldDecoder[Field].map { normalDec =>
                        Expr.quote { (r: JsonReader) =>
                          if (Expr.splice(dctx.config).isStringified)
                            Expr.splice(stringifiedDec).apply(r)
                          else
                            Expr.splice(normalDec).apply(r)
                        }
                      }
                    case None =>
                      deriveFieldDecoder[Field]
                  }
                }
                decodeFnMIO.map { decodeFn =>
                  val decodeFnErased: Expr[JsonReader => Any] = Expr.quote { (r: JsonReader) =>
                    Expr.splice(decodeFn).apply(r).asInstanceOf[Any]
                  }
                  val makeAccessor: Expr[Array[Any]] => (String, Expr_??) = { arrExpr =>
                    val typedExpr = Expr.quote {
                      JsoniterDerivationUtils.unsafeCast(
                        Expr.splice(arrExpr)(Expr.splice(Expr(arrayIndex))),
                        Expr.splice(decodeFn)
                      )
                    }
                    (fName, typedExpr.as_??)
                  }
                  (fName, arrayIndex, decodeFnErased, makeAccessor, nameOverride)
                }
              }
            }
            .flatMap { fieldData =>
              val fieldDataList = fieldData.toList

              LambdaBuilder
                .of1[Array[Any]]("decodedValues")
                .traverse { decodedValuesExpr =>
                  // Build require check expressions: validate required fields before transient init
                  val requireCheckExprs0: List[Expr[Unit]] =
                    nonTransientWithIndex.flatMap { case ((fName0, param0), idx0) =>
                      import param0.tpe.Underlying as Field0
                      val checks = List.newBuilder[Expr[Unit]]

                      if (param0.hasDefault) {
                        checks += Expr.quote {
                          if (
                            Expr.splice(dctx.config).requireDefaultFields &&
                            Expr.splice(decodedValuesExpr)(Expr.splice(Expr(idx0))) == null
                          )
                            JsoniterDerivationUtils.throwMissingField(Expr.splice(Expr(fName0)))
                        }
                      }

                      val isCollOrMap0 = Type[Field0] match {
                        case IsMap(_)        => true
                        case IsCollection(_) => true
                        case _               => false
                      }
                      if (isCollOrMap0) {
                        checks += Expr.quote {
                          if (
                            Expr.splice(dctx.config).requireCollectionFields &&
                            Expr.splice(decodedValuesExpr)(Expr.splice(Expr(idx0))) == null
                          )
                            JsoniterDerivationUtils.throwMissingField(Expr.splice(Expr(fName0)))
                        }
                      }

                      checks.result()
                    }

                  val requireCheckAll0: Expr[Unit] =
                    requireCheckExprs0.foldLeft(Expr.quote(()): Expr[Unit]) { (acc, step) =>
                      Expr.quote {
                        Expr.splice(acc)
                        Expr.splice(step)
                      }
                    }

                  // Build transient init expressions: fill null slots for absent fields
                  val transientInitExprs: List[Expr[Unit]] =
                    nonTransientWithIndex.flatMap { case ((fName0, param0), idx0) =>
                      import param0.tpe.Underlying as Field0
                      val initSteps = List.newBuilder[Expr[Unit]]

                      if (param0.hasDefault) {
                        param0.defaultValue
                          .flatMap { existentialOuter =>
                            val methodOf = existentialOuter.value
                            methodOf.value match {
                              case noInstance: Method.NoInstance[?] =>
                                import noInstance.Returned
                                noInstance(Map.empty).toOption.map(_.upcast[Any])
                              case _ => None
                            }
                          }
                          .foreach { defaultExpr =>
                            initSteps += Expr.quote {
                              if (
                                Expr.splice(dctx.config).transientDefault &&
                                Expr.splice(decodedValuesExpr)(Expr.splice(Expr(idx0))) == null
                              )
                                Expr.splice(decodedValuesExpr)(Expr.splice(Expr(idx0))) = Expr.splice(defaultExpr)
                            }
                          }
                      }

                      val isOpt0 = Type[Field0] match {
                        case IsOption(_) => true; case _ => false
                      }
                      if (isOpt0) {
                        initSteps += Expr.quote {
                          if (
                            Expr.splice(dctx.config).transientNone &&
                            Expr.splice(decodedValuesExpr)(Expr.splice(Expr(idx0))) == null
                          )
                            Expr.splice(decodedValuesExpr)(Expr.splice(Expr(idx0))) = (None: Any)
                        }
                      }

                      if (Type[Field0] =:= CTypes.String) {
                        initSteps += Expr.quote {
                          if (
                            Expr.splice(dctx.config).transientEmpty &&
                            Expr.splice(decodedValuesExpr)(Expr.splice(Expr(idx0))) == null
                          )
                            Expr.splice(decodedValuesExpr)(Expr.splice(Expr(idx0))) = ("": Any)
                        }
                      }

                      initSteps.result()
                    }

                  val transientInitAll: Expr[Unit] =
                    transientInitExprs.foldLeft(Expr.quote(()): Expr[Unit]) { (acc, step) =>
                      Expr.quote {
                        Expr.splice(acc)
                        Expr.splice(step)
                      }
                    }

                  val nonTransientFieldMap: Map[String, Expr_??] =
                    fieldDataList.map(_._4(decodedValuesExpr)).toMap
                  val fieldMap = nonTransientFieldMap ++ transientDefaults
                  caseClass.primaryConstructor(fieldMap) match {
                    case Right(constructExpr) =>
                      MIO.pure(Expr.quote {
                        Expr.splice(requireCheckAll0)
                        Expr.splice(transientInitAll)
                        Expr.splice(constructExpr)
                      })
                    case Left(error) =>
                      val err =
                        CodecDerivationError.CannotConstructType(Type[A].prettyPrint, isSingleton = false, Some(error))
                      Log.error(err.message) >> MIO.fail(err)
                  }
                }
                .map { builder =>
                  val constructLambda = builder.build[A]

                  val fieldMappings = fieldDataList.map { case (name, index, decodeFnErased, _, nameOverride) =>
                    (name, index, decodeFnErased, nameOverride)
                  }

                  Expr.quote {
                    val _seen: Array[Boolean] =
                      if (Expr.splice(dctx.config).checkFieldDuplication)
                        new Array[Boolean](Expr.splice(Expr(fieldMappings.size)))
                      else null
                    JsoniterDerivationUtils.readObjectInline[A](
                      Expr.splice(dctx.reader),
                      Expr.splice(Expr(fieldMappings.size)),
                      Expr.splice(constructLambda)
                    ) { case (fieldName, arr, reader) =>
                      Expr.splice {
                        fieldMappings.foldRight(Expr.quote {
                          if (Expr.splice(dctx.config).skipUnexpectedFields) reader.skip()
                          else reader.decodeError("unexpected field: " + fieldName)
                        }: Expr[Unit]) {
                          case ((name, index, decodeFnErased, Some(customName)), elseExpr) =>
                            Expr.quote {
                              if (fieldName == Expr.splice(Expr(customName))) {
                                if (_seen != null) {
                                  if (_seen(Expr.splice(Expr(index))))
                                    JsoniterDerivationUtils.throwDuplicateField(reader, fieldName)
                                  _seen(Expr.splice(Expr(index))) = true
                                }
                                arr(Expr.splice(Expr(index))) = Expr.splice(decodeFnErased).apply(reader)
                              } else Expr.splice(elseExpr)
                            }
                          case ((name, index, decodeFnErased, None), elseExpr) =>
                            Expr.quote {
                              if (fieldName == Expr.splice(dctx.config).fieldNameMapper(Expr.splice(Expr(name)))) {
                                if (_seen != null) {
                                  if (_seen(Expr.splice(Expr(index))))
                                    JsoniterDerivationUtils.throwDuplicateField(reader, fieldName)
                                  _seen(Expr.splice(Expr(index))) = true
                                }
                                arr(Expr.splice(Expr(index))) = Expr.splice(decodeFnErased).apply(reader)
                              } else Expr.splice(elseExpr)
                            }
                        }
                      }
                    }
                  }
                }
            }
      }
    }

  }

  /** Derive a decode function for a case class field. Tries implicit summoning first, falls back to recursive
    * derivation via the full rule chain.
    */
  @scala.annotation.nowarn("msg=is never used|unused explicit parameter")
  private def deriveFieldDecoder[Field: Type](implicit ctx: DecoderCtx[?]): MIO[Expr[JsonReader => Field]] = {
    implicit val JsonReaderT: Type[JsonReader] = CTypes.JsonReader

    CTypes
      .JsonValueCodec[Field]
      .summonExprIgnoring(DecUseImplicitWhenAvailableRule.ignoredImplicits*)
      .toEither match {
      case Right(codecExpr) =>
        Log.info(s"Found implicit JsonValueCodec[${Type[Field].prettyPrint}]") >> MIO.pure(
          Expr.quote { (r: JsonReader) =>
            Expr.splice(codecExpr).decodeValue(r, Expr.splice(codecExpr).nullValue)
          }
        )
      case Left(_) =>
        Log.info(s"Building decoder for ${Type[Field].prettyPrint} via recursive derivation") >>
          LambdaBuilder
            .of1[JsonReader]("fieldReader")
            .traverse { fieldReaderExpr =>
              deriveDecoderRecursively[Field](using ctx.nest[Field](fieldReaderExpr))
            }
            .map { builder =>
              builder.build[Field]
            }
    }
  }

  @scala.annotation.nowarn("msg=is never used")
  private def deriveStringifiedDecoder[F: Type](implicit
      JsonReaderT: Type[JsonReader]
  ): Option[Expr[JsonReader => F]] =
    if (Type[F] =:= CTypes.Int)
      Some(Expr.quote { (r: JsonReader) =>
        JsoniterDerivationUtils.readStringifiedInt(r).asInstanceOf[F]
      })
    else if (Type[F] =:= CTypes.Long)
      Some(Expr.quote { (r: JsonReader) =>
        JsoniterDerivationUtils.readStringifiedLong(r).asInstanceOf[F]
      })
    else if (Type[F] =:= CTypes.Double)
      Some(Expr.quote { (r: JsonReader) =>
        JsoniterDerivationUtils.readStringifiedDouble(r).asInstanceOf[F]
      })
    else if (Type[F] =:= CTypes.Float)
      Some(Expr.quote { (r: JsonReader) =>
        JsoniterDerivationUtils.readStringifiedFloat(r).asInstanceOf[F]
      })
    else if (Type[F] =:= CTypes.Short)
      Some(Expr.quote { (r: JsonReader) =>
        JsoniterDerivationUtils.readStringifiedShort(r).asInstanceOf[F]
      })
    else if (Type[F] =:= CTypes.Byte)
      Some(Expr.quote { (r: JsonReader) =>
        JsoniterDerivationUtils.readStringifiedByte(r).asInstanceOf[F]
      })
    else if (Type[F] =:= CTypes.BigDecimal)
      Some(Expr.quote { (r: JsonReader) =>
        JsoniterDerivationUtils.readStringifiedBigDecimal(r).asInstanceOf[F]
      })
    else if (Type[F] =:= CTypes.BigInt)
      Some(Expr.quote { (r: JsonReader) =>
        JsoniterDerivationUtils.readStringifiedBigInt(r).asInstanceOf[F]
      })
    else None

  object DecHandleAsEnumRule extends DecoderDerivationRule("handle as enum when possible") {

    def apply[A: DecoderCtx]: MIO[Rule.Applicability[Expr[A]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as an enum") >> {
        Enum.parse[A].toEither match {
          case Right(enumm) =>
            for {
              _ <- dctx.setHelper[A] { (reader, config) =>
                decodeEnumCases[A](enumm)(using dctx.nestInCache(reader, config))
              }
              result <- dctx.getHelper[A].flatMap {
                case Some(helperCall) =>
                  MIO.pure(Rule.matched(helperCall(dctx.reader, dctx.config)))
                case None =>
                  MIO.pure(Rule.yielded(s"Failed to build helper for ${Type[A].prettyPrint}"))
              }
            } yield result
          case Left(reason) =>
            MIO.pure(Rule.yielded(reason))
        }
      }

    @scala.annotation.nowarn("msg=is never used|unused explicit parameter")
    private def decodeEnumCases[A: DecoderCtx](
        enumm: Enum[A]
    ): MIO[Expr[A]] = {
      implicit val JsonReaderT: Type[JsonReader] = CTypes.JsonReader
      implicit val StringT: Type[String] = CTypes.String
      implicit val ListStringT: Type[List[String]] = CTypes.ListString

      val childrenList = enumm.directChildren.toList

      // Check at compile time if all children are singletons (case objects with no fields)
      val allCaseObjects = Type[A].isEnumeration || Type[A].isJavaEnum ||
        childrenList.forall { case (_, child) =>
          SingletonValue.unapply(child.Underlying).isDefined
        }

      NonEmptyList.fromList(childrenList) match {
        case None =>
          MIO.pure(Expr.quote {
            Expr
              .splice(dctx.reader)
              .decodeError(
                "Enum " + Expr.splice(Expr(Type[A].prettyPrint)) + " has no subtypes"
              ): A
          })

        case Some(children) =>
          val knownNames: List[String] = children.toList.map(_._1)

          val isScalaEnumeration = Type[A].isEnumeration

          // For each child, derive BOTH wrapper-mode and inline (discriminator-mode) decoders,
          // and optionally string-enum dispatchers for case-object children
          children
            .parTraverse { case (childName, child) =>
              import child.Underlying as ChildType
              val isSingleton = SingletonValue.unapply(Type[ChildType]).isDefined || isScalaEnumeration ||
                Type[A].isJavaEnum
              Log.namedScope(s"Deriving decoder for enum case $childName: ${Type[ChildType].prettyPrint}") {
                for {
                  wrapper <- deriveChildDecoder[A, ChildType](childName)
                  inline <- deriveChildDecoderInline[A, ChildType](childName)
                  stringEnum <-
                    if (allCaseObjects || isSingleton)
                      deriveChildDecoderStringEnum[A, ChildType](childName).map(Some(_))
                    else MIO.pure(None)
                  enumId <-
                    if (allCaseObjects && isScalaEnumeration)
                      deriveChildDecoderEnumId[A, ChildType](childName).map(Some(_))
                    else MIO.pure(None)
                } yield (wrapper, inline, stringEnum, enumId)
              }
            }
            .flatMap { allDispatchers =>
              val wrapperDispatchers = allDispatchers.toList.map(_._1)
              val inlineDispatchers = allDispatchers.toList.map(_._2)

              implicit val IntT: Type[Int] = CTypes.Int

              def buildErrorExpr(typeNameExpr: Expr[String]): Expr[A] = Expr.quote {
                Expr
                  .splice(dctx.reader)
                  .decodeError(
                    "Unknown type discriminator: " + Expr.splice(typeNameExpr) +
                      ". Expected one of: " + Expr.splice(Expr(knownNames)).mkString(", ")
                  ): A
              }

              def buildDispatchLambda(
                  dispatchers: List[(Expr[String], Expr[JsonReader], Expr[A]) => Expr[A]]
              ): MIO[Expr[String => A]] =
                LambdaBuilder
                  .of1[String]("typeName")
                  .traverse { typeNameExpr =>
                    MIO.pure(dispatchers.foldRight(buildErrorExpr(typeNameExpr)) { case (dispatcher, elseExpr) =>
                      dispatcher(typeNameExpr, dctx.reader, elseExpr)
                    })
                  }
                  .map(_.build[A])

              def buildIntErrorExpr(idExpr: Expr[Int]): Expr[A] = Expr.quote {
                Expr
                  .splice(dctx.reader)
                  .decodeError(
                    "Unknown enum value id: " + Expr.splice(idExpr)
                  ): A
              }

              def buildIntDispatchLambda(
                  dispatchers: List[(Expr[Int], Expr[JsonReader], Expr[A]) => Expr[A]]
              ): MIO[Expr[Int => A]] =
                LambdaBuilder
                  .of1[Int]("enumId")
                  .traverse { idExpr =>
                    MIO.pure(dispatchers.foldRight(buildIntErrorExpr(idExpr)) { case (dispatcher, elseExpr) =>
                      dispatcher(idExpr, dctx.reader, elseExpr)
                    })
                  }
                  .map(_.build[A])

              for {
                wrapperDispatchFn <- buildDispatchLambda(wrapperDispatchers)
                inlineDispatchFn <- buildDispatchLambda(inlineDispatchers)
                result <-
                  if (allCaseObjects) {
                    val stringEnumDispatchers = allDispatchers.toList.flatMap(_._3)
                    val enumIdDispatchers = allDispatchers.toList.flatMap(_._4)
                    for {
                      stringEnumDispatchFn <- buildDispatchLambda(stringEnumDispatchers)
                      enumIdDispatchFnOpt <-
                        if (enumIdDispatchers.nonEmpty) buildIntDispatchLambda(enumIdDispatchers).map(Some(_))
                        else MIO.pure(None)
                    } yield enumIdDispatchFnOpt match {
                      case Some(enumIdDispatchFn) =>
                        // Scala Enumeration with useScalaEnumValueId support
                        Expr.quote {
                          val config = Expr.splice(dctx.config)
                          val reader = Expr.splice(dctx.reader)
                          if (config.useScalaEnumValueId)
                            JsoniterDerivationUtils.readScalaEnumValueId[A](reader)(
                              Expr.splice(enumIdDispatchFn)
                            )
                          else if (config.enumAsStrings)
                            JsoniterDerivationUtils.readEnumAsString[A](reader)(
                              Expr.splice(stringEnumDispatchFn)
                            )
                          else
                            config.discriminatorFieldName match {
                              case Some(field) =>
                                JsoniterDerivationUtils.readWithDiscriminator[A](reader, field)(
                                  Expr.splice(inlineDispatchFn)
                                )
                              case None =>
                                JsoniterDerivationUtils.readWrapped[A](reader)(Expr.splice(wrapperDispatchFn))
                            }
                        }
                      case None =>
                        // Non-Enumeration all-case-objects enum
                        Expr.quote {
                          val config = Expr.splice(dctx.config)
                          val reader = Expr.splice(dctx.reader)
                          if (config.enumAsStrings)
                            JsoniterDerivationUtils.readEnumAsString[A](reader)(
                              Expr.splice(stringEnumDispatchFn)
                            )
                          else
                            config.discriminatorFieldName match {
                              case Some(field) =>
                                JsoniterDerivationUtils.readWithDiscriminator[A](reader, field)(
                                  Expr.splice(inlineDispatchFn)
                                )
                              case None =>
                                JsoniterDerivationUtils.readWrapped[A](reader)(Expr.splice(wrapperDispatchFn))
                            }
                        }
                    }
                  } else {
                    // Mixed enum (case objects + case classes)
                    // Build string-enum dispatchers for singleton children (for circeLikeObjectEncoding)
                    val singletonStringEnumDispatchers = allDispatchers.toList.flatMap(_._3)
                    (if (singletonStringEnumDispatchers.nonEmpty)
                       buildDispatchLambda(singletonStringEnumDispatchers).map(Some(_))
                     else MIO.pure(None)).map { singletonDispatchFnOpt =>
                      singletonDispatchFnOpt match {
                        case Some(singletonDispatchFn) =>
                          Expr.quote {
                            val config = Expr.splice(dctx.config)
                            val reader = Expr.splice(dctx.reader)
                            if (config.circeLikeObjectEncoding)
                              JsoniterDerivationUtils.readCirceLikeObject[A](reader)(
                                Expr.splice(singletonDispatchFn),
                                Expr.splice(wrapperDispatchFn)
                              )
                            else
                              config.discriminatorFieldName match {
                                case Some(field) =>
                                  JsoniterDerivationUtils.readWithDiscriminator[A](reader, field)(
                                    Expr.splice(inlineDispatchFn)
                                  )
                                case None =>
                                  JsoniterDerivationUtils.readWrapped[A](reader)(Expr.splice(wrapperDispatchFn))
                              }
                          }
                        case None =>
                          Expr.quote {
                            val config = Expr.splice(dctx.config)
                            val reader = Expr.splice(dctx.reader)
                            config.discriminatorFieldName match {
                              case Some(field) =>
                                JsoniterDerivationUtils.readWithDiscriminator[A](reader, field)(
                                  Expr.splice(inlineDispatchFn)
                                )
                              case None =>
                                JsoniterDerivationUtils.readWrapped[A](reader)(Expr.splice(wrapperDispatchFn))
                            }
                          }
                      }
                    }
                  }
              } yield result
            }
      }
    }

    @scala.annotation.nowarn("msg=is never used|unused explicit parameter")
    private def deriveChildDecoder[A: DecoderCtx, ChildType: Type](
        childName: String
    ): MIO[(Expr[String], Expr[JsonReader], Expr[A]) => Expr[A]] = {
      implicit val JsonReaderT: Type[JsonReader] = CTypes.JsonReader

      CTypes
        .JsonValueCodec[ChildType]
        .summonExprIgnoring(DecUseImplicitWhenAvailableRule.ignoredImplicits*)
        .toEither match {
        case Right(codecExpr) =>
          Log.info(s"Found implicit JsonValueCodec[$childName], using it") >>
            MIO.pure { (typeNameExpr: Expr[String], readerExpr: Expr[JsonReader], elseExpr: Expr[A]) =>
              Expr.quote {
                if (
                  Expr.splice(dctx.config).adtLeafClassNameMapper(Expr.splice(Expr(childName))) == Expr
                    .splice(typeNameExpr)
                )
                  Expr
                    .splice(codecExpr)
                    .decodeValue(Expr.splice(readerExpr), Expr.splice(codecExpr).nullValue)
                    .asInstanceOf[A]
                else
                  Expr.splice(elseExpr)
              }
            }

        case Left(_) =>
          // Try singletonOf first — handles Enumeration values, Java enum values, case objects
          Expr.singletonOf[ChildType] match {
            case Some(singleton) =>
              Log.info(s"Using singleton for $childName") >>
                MIO.pure { (typeNameExpr: Expr[String], _: Expr[JsonReader], elseExpr: Expr[A]) =>
                  Expr.quote {
                    if (
                      Expr.splice(dctx.config).adtLeafClassNameMapper(Expr.splice(Expr(childName))) == Expr
                        .splice(typeNameExpr)
                    )
                      Expr.splice(singleton).asInstanceOf[A]
                    else
                      Expr.splice(elseExpr)
                  }
                }
            case None =>
              // No singleton - derive via full rules chain
              deriveDecoderRecursively[ChildType](using dctx.nest[ChildType](dctx.reader)).flatMap { decodedExpr =>
                dctx.getHelper[ChildType].map {
                  case Some(helper) =>
                    (typeNameExpr: Expr[String], readerExpr: Expr[JsonReader], elseExpr: Expr[A]) => {
                      val helperCallExpr = helper(readerExpr, dctx.config)
                      Expr.quote {
                        if (
                          Expr.splice(dctx.config).adtLeafClassNameMapper(Expr.splice(Expr(childName))) == Expr
                            .splice(typeNameExpr)
                        )
                          Expr.splice(helperCallExpr).asInstanceOf[A]
                        else
                          Expr.splice(elseExpr)
                      }
                    }

                  case None =>
                    // No helper registered (e.g., built-in types like String, Int) — use the derived expression directly
                    (typeNameExpr: Expr[String], _: Expr[JsonReader], elseExpr: Expr[A]) =>
                      Expr.quote {
                        if (
                          Expr.splice(dctx.config).adtLeafClassNameMapper(Expr.splice(Expr(childName))) == Expr
                            .splice(typeNameExpr)
                        )
                          Expr.splice(decodedExpr).asInstanceOf[A]
                        else
                          Expr.splice(elseExpr)
                      }
                }
              }
          }
      }
    }

    /** Derive an inline child decoder for discriminator mode. Uses decodeCaseClassFieldsInline to read fields from an
      * already-opened object.
      */
    @scala.annotation.nowarn("msg=is never used|unused explicit parameter")
    private def deriveChildDecoderInline[A: DecoderCtx, ChildType: Type](
        childName: String
    ): MIO[(Expr[String], Expr[JsonReader], Expr[A]) => Expr[A]] = {
      implicit val JsonReaderT: Type[JsonReader] = CTypes.JsonReader

      CaseClass.parse[ChildType].toOption match {
        case Some(cc) =>
          DecHandleAsCaseClassRule
            .decodeCaseClassFieldsInline[ChildType](cc)(using dctx.nest[ChildType](dctx.reader))
            .map { inlineExpr => (typeNameExpr: Expr[String], _: Expr[JsonReader], elseExpr: Expr[A]) =>
              Expr.quote {
                if (
                  Expr.splice(dctx.config).adtLeafClassNameMapper(Expr.splice(Expr(childName))) == Expr
                    .splice(typeNameExpr)
                )
                  Expr.splice(inlineExpr).asInstanceOf[A]
                else
                  Expr.splice(elseExpr)
              }
            }

        case None =>
          // Not a case class (e.g., case object) — fall back to wrapper-style decoder
          deriveChildDecoder[A, ChildType](childName)
      }
    }

    /** Derive a string-enum child decoder that returns the singleton instance directly without reading from the reader.
      * Used when all enum children are case objects and enumAsStrings is enabled.
      */
    @scala.annotation.nowarn("msg=is never used|unused explicit parameter")
    private def deriveChildDecoderStringEnum[A: DecoderCtx, ChildType: Type](
        childName: String
    ): MIO[(Expr[String], Expr[JsonReader], Expr[A]) => Expr[A]] = {
      implicit val JsonReaderT: Type[JsonReader] = CTypes.JsonReader

      // Try SingletonValue first — handles Enumeration values, Java enum values, case objects
      SingletonValue.unapply(Type[ChildType]) match {
        case Some(sv) =>
          Log.info(s"Using singleton for string enum child $childName") >>
            MIO.pure { (typeNameExpr: Expr[String], _: Expr[JsonReader], elseExpr: Expr[A]) =>
              Expr.quote {
                if (
                  Expr.splice(dctx.config).adtLeafClassNameMapper(Expr.splice(Expr(childName))) == Expr
                    .splice(typeNameExpr)
                )
                  Expr.splice(sv.singletonExpr).asInstanceOf[A]
                else
                  Expr.splice(elseExpr)
              }
            }
        case None =>
          // Fall back to CaseClass.construct for zero-arg case classes
          CaseClass.parse[ChildType].toOption match {
            case Some(cc) if cc.primaryConstructor.parameters.flatten.isEmpty =>
              val constructMIO: MIO[Option[Expr[ChildType]]] =
                cc.construct[MIO](new CaseClass.ConstructField[MIO] {
                  def apply(field: Parameter): MIO[Expr[field.tpe.Underlying]] = {
                    val err = CodecDerivationError.UnexpectedParameterInSingleton(
                      Type[ChildType].prettyPrint,
                      "Unexpected parameter in singleton"
                    )
                    Log.error(err.message) >> MIO.fail(err)
                  }
                })
              constructMIO.flatMap {
                case Some(instanceExpr) =>
                  MIO.pure { (typeNameExpr: Expr[String], _: Expr[JsonReader], elseExpr: Expr[A]) =>
                    Expr.quote {
                      if (
                        Expr.splice(dctx.config).adtLeafClassNameMapper(Expr.splice(Expr(childName))) == Expr
                          .splice(typeNameExpr)
                      )
                        Expr.splice(instanceExpr).asInstanceOf[A]
                      else
                        Expr.splice(elseExpr)
                    }
                  }
                case None =>
                  val err = CodecDerivationError.CannotConstructType(Type[ChildType].prettyPrint, isSingleton = true)
                  Log.error(err.message) >> MIO.fail(err)
              }

            case _ =>
              // Not a zero-param case class — shouldn't happen when allCaseObjects is true
              val err = CodecDerivationError.UnexpectedParameterInSingleton(
                Type[ChildType].prettyPrint,
                "Expected singleton/case object for string enum but got"
              )
              Log.error(err.message) >> MIO.fail(err)
          }
      }
    }

    /** Derive an Int-based enum-id child decoder for useScalaEnumValueId support. Matches the child's .id at runtime.
      */
    @scala.annotation.nowarn("msg=is never used|unused explicit parameter")
    private def deriveChildDecoderEnumId[A: DecoderCtx, ChildType: Type](
        childName: String
    ): MIO[(Expr[Int], Expr[JsonReader], Expr[A]) => Expr[A]] = {
      implicit val IntT: Type[Int] = CTypes.Int
      SingletonValue.unapply(Type[ChildType]) match {
        case Some(sv) =>
          Log.info(s"Using singleton for enum-id child $childName") >>
            MIO.pure { (idExpr: Expr[Int], _: Expr[JsonReader], elseExpr: Expr[A]) =>
              Expr.quote {
                if (Expr.splice(idExpr) == JsoniterDerivationUtils.scalaEnumValueId(Expr.splice(sv.singletonExpr)))
                  Expr.splice(sv.singletonExpr).asInstanceOf[A]
                else
                  Expr.splice(elseExpr)
              }
            }
        case None =>
          MIO.pure { (_: Expr[Int], _: Expr[JsonReader], elseExpr: Expr[A]) =>
            elseExpr
          }
      }
    }
  }

  // Types

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
    val FieldName: Type[fieldNameAnn] = Type.of[fieldNameAnn]
    val TransientField: Type[transientField] = Type.of[transientField]
    val Stringified: Type[stringified] = Type.of[stringified]
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
    val Period: Type[java.time.Period] = Type.of[java.time.Period]
  }
}

sealed private[compiletime] trait CodecDerivationError
    extends util.control.NoStackTrace
    with Product
    with Serializable {
  def message: String
  override def getMessage(): String = message
}
private[compiletime] object CodecDerivationError {
  final case class UnsupportedType(tpeName: String, reasons: List[String]) extends CodecDerivationError {
    override def message: String =
      s"The type $tpeName was not handled by any codec derivation rule:\n${reasons.mkString("\n")}"
  }
  final case class TransientFieldMissingDefault(fieldName: String, tpeName: String) extends CodecDerivationError {
    override def message: String =
      s"@transientField on field '$fieldName' of $tpeName requires a default value"
  }
  final case class NoChildrenInSealedTrait(tpeName: String) extends CodecDerivationError {
    override def message: String =
      s"The type $tpeName does not have any children!"
  }
  final case class CannotConstructType(tpeName: String, isSingleton: Boolean, constructorError: Option[String] = None)
      extends CodecDerivationError {
    override def message: String = {
      val prefix =
        if (isSingleton) s"Cannot construct singleton $tpeName" else s"Cannot construct $tpeName"
      constructorError.fold(prefix)(err => s"$prefix: $err")
    }
  }
  final case class UnexpectedParameterInSingleton(tpeName: String, context: String) extends CodecDerivationError {
    override def message: String = s"$context: $tpeName"
  }
  final case class StringifiedOnNonNumeric(fieldName: String, tpeName: String, fieldTypeName: String)
      extends CodecDerivationError {
    override def message: String =
      s"@stringified on field '$fieldName' of $tpeName requires a numeric type, but found $fieldTypeName"
  }
}
