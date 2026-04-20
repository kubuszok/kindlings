package hearth.kindlings.ubjsonderivation.internal.compiletime

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.fp.syntax.*
import hearth.std.*

import hearth.kindlings.ubjsonderivation.{UBJsonConfig, UBJsonReader, UBJsonValueCodec, UBJsonWriter}
import hearth.kindlings.ubjsonderivation.annotations.{fieldName as fieldNameAnn, stringified, transientField}

trait CodecMacrosImpl
    extends hearth.kindlings.derivation.compiletime.DerivationTimeout
    with rules.EncoderUseCachedDefWhenAvailableRuleImpl
    with rules.EncoderUseImplicitWhenAvailableRuleImpl
    with rules.EncoderHandleAsBuiltInRuleImpl
    with rules.EncoderHandleAsValueTypeRuleImpl
    with rules.EncoderHandleAsOptionRuleImpl
    with rules.EncoderHandleAsMapRuleImpl
    with rules.EncoderHandleAsCollectionRuleImpl
    with rules.EncoderHandleAsSingletonRuleImpl
    with rules.EncoderHandleAsCaseClassRuleImpl
    with rules.EncoderHandleAsEnumRuleImpl
    with rules.DecoderUseCachedDefWhenAvailableRuleImpl
    with rules.DecoderUseImplicitWhenAvailableRuleImpl
    with rules.DecoderHandleAsBuiltInRuleImpl
    with rules.DecoderHandleAsValueTypeRuleImpl
    with rules.DecoderHandleAsOptionRuleImpl
    with rules.DecoderHandleAsMapRuleImpl
    with rules.DecoderHandleAsCollectionRuleImpl
    with rules.DecoderHandleAsSingletonRuleImpl
    with rules.DecoderHandleAsCaseClassRuleImpl
    with rules.DecoderHandleAsEnumRuleImpl { this: MacroCommons & StdExtensions & AnnotationSupport =>

  override protected def derivationSettingsNamespace: String = "ubjsonDerivation"

  // Entrypoints

  /** Derive a combined UBJsonValueCodec for type A. */
  @scala.annotation.nowarn("msg=is never used")
  def deriveCodecTypeClass[A: Type](configExpr: Expr[UBJsonConfig]): Expr[UBJsonValueCodec[A]] = {
    implicit val CodecA: Type[UBJsonValueCodec[A]] = CTypes.UBJsonValueCodec[A]
    implicit val ConfigT: Type[UBJsonConfig] = CTypes.UBJsonConfig
    implicit val UBJsonReaderT: Type[UBJsonReader] = CTypes.UBJsonReader
    implicit val UBJsonWriterT: Type[UBJsonWriter] = CTypes.UBJsonWriter
    implicit val UnitT: Type[Unit] = CTypes.Unit

    deriveCodecFromCtxAndAdaptForEntrypoint[A, UBJsonValueCodec[A]]("UBJsonValueCodec.derived") {
      case (encodeFn, decodeFn, nullValueExpr) =>
        Expr.quote {
          new UBJsonValueCodec[A] {
            def nullValue: A = Expr.splice(nullValueExpr)
            def decode(reader: UBJsonReader): A =
              Expr.splice(decodeFn(Expr.quote(reader), configExpr))
            def encode(writer: UBJsonWriter, value: A): Unit =
              Expr.splice(encodeFn(Expr.quote(value), Expr.quote(writer), configExpr))
          }
        }
    }
  }

  // Handles logging, error reporting and prepending "cached" defs and vals to the result.

  def deriveCodecFromCtxAndAdaptForEntrypoint[A: Type, Out: Type](macroName: String)(
      provideCtxAndAdapt: (
          (Expr[A], Expr[UBJsonWriter], Expr[UBJsonConfig]) => Expr[Unit],
          (Expr[UBJsonReader], Expr[UBJsonConfig]) => Expr[A],
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
        implicit val ConfigT: Type[UBJsonConfig] = CTypes.UBJsonConfig
        implicit val UBJsonReaderT: Type[UBJsonReader] = CTypes.UBJsonReader
        implicit val UBJsonWriterT: Type[UBJsonWriter] = CTypes.UBJsonWriter
        implicit val UnitT: Type[Unit] = CTypes.Unit

        MIO.scoped { runSafe =>
          val cache = ValDefsCache.mlocal
          val selfType: Option[??] = Some(Type[A].as_??)

          // Encoder
          val encMIO: MIO[(Expr[A], Expr[UBJsonWriter], Expr[UBJsonConfig]) => Expr[Unit]] = {
            val defBuilder =
              ValDefBuilder.ofDef3[A, UBJsonWriter, UBJsonConfig, Unit](s"codec_encode_${Type[A].shortName}")
            for {
              _ <- Log.info(s"Forward-declaring codec encode body for ${Type[A].prettyPrint}")
              _ <- cache.forwardDeclare("codec-encode-body", defBuilder)
              builtEnc <- defBuilder.traverse { case (_, (v, w, c)) =>
                deriveEncoderRecursively[A](using EncoderCtx.from(v, w, c, cache, selfType))
              }
              encCache <- cache.get
              _ <- cache.set(builtEnc.buildCached(encCache, "codec-encode-body"))
              _ <- Log.info(s"Defined codec encode body for ${Type[A].prettyPrint}")
              fn <- cache.get3Ary[A, UBJsonWriter, UBJsonConfig, Unit]("codec-encode-body")
            } yield fn.get
          }

          // Decoder
          val decMIO: MIO[(Expr[UBJsonReader], Expr[UBJsonConfig]) => Expr[A]] = {
            val defBuilder =
              ValDefBuilder.ofDef2[UBJsonReader, UBJsonConfig, A](s"codec_decode_${Type[A].shortName}")
            for {
              _ <- Log.info(s"Forward-declaring codec decode body for ${Type[A].prettyPrint}")
              _ <- cache.forwardDeclare("codec-decode-body", defBuilder)
              builtDec <- defBuilder.traverse { case (_, (r, c)) =>
                deriveDecoderRecursively[A](using DecoderCtx.from(r, c, cache, selfType))
              }
              decCache <- cache.get
              _ <- cache.set(builtDec.buildCached(decCache, "codec-decode-body"))
              _ <- Log.info(s"Defined codec decode body for ${Type[A].prettyPrint}")
              fn <- cache.get2Ary[UBJsonReader, UBJsonConfig, A]("codec-decode-body")
            } yield fn.get
          }

          // Null value
          val nullMIO: MIO[Expr[A]] = deriveNullValue[A]

          // Combine
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
        errorRendering = if (shouldWeLogCodecDerivation) RenderFrom(Log.Level.Info) else DontRender,
        timeout = derivationTimeout
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
          "Enable debug logging with: import hearth.kindlings.ubjsonderivation.debug.logDerivationForUBJsonValueCodec or scalac option -Xmacro-settings:ubjsonDerivation.logDerivation=true"
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
    implicit val LogDerivation: Type[UBJsonValueCodec.LogDerivation] = CTypes.CodecLogDerivation
    def logDerivationImported = Expr.summonImplicit[UBJsonValueCodec.LogDerivation].isDefined

    def logDerivationSetGlobally = (for {
      data <- Environment.typedSettings.toOption
      ubjsonDerivation <- data.get("ubjsonDerivation")
      shouldLog <- ubjsonDerivation.get("logDerivation").flatMap(_.asBoolean)
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
      writer: Expr[UBJsonWriter],
      config: Expr[UBJsonConfig],
      cache: MLocal[ValDefsCache],
      derivedType: Option[??]
  ) {

    def nest[B: Type](newValue: Expr[B]): EncoderCtx[B] = copy[B](
      tpe = Type[B],
      value = newValue
    )

    def nestInCache(
        newValue: Expr[A],
        newWriter: Expr[UBJsonWriter],
        newConfig: Expr[UBJsonConfig]
    ): EncoderCtx[A] = copy(
      value = newValue,
      writer = newWriter,
      config = newConfig
    )

    def getInstance[B: Type]: MIO[Option[Expr[UBJsonValueCodec[B]]]] = {
      implicit val CodecB: Type[UBJsonValueCodec[B]] = CTypes.UBJsonValueCodec[B]
      cache.get0Ary[UBJsonValueCodec[B]]("cached-codec-instance")
    }
    def setInstance[B: Type](instance: Expr[UBJsonValueCodec[B]]): MIO[Unit] = {
      implicit val CodecB: Type[UBJsonValueCodec[B]] = CTypes.UBJsonValueCodec[B]
      Log.info(s"Caching UBJsonValueCodec instance for ${Type[B].prettyPrint}") >>
        cache.buildCachedWith(
          "cached-codec-instance",
          ValDefBuilder.ofLazy[UBJsonValueCodec[B]](s"codec_${Type[B].shortName}")
        )(_ => instance)
    }

    def getHelper[B: Type]: MIO[Option[(Expr[B], Expr[UBJsonWriter], Expr[UBJsonConfig]) => Expr[Unit]]] = {
      implicit val UnitT: Type[Unit] = CTypes.Unit
      implicit val UBJsonWriterT: Type[UBJsonWriter] = CTypes.UBJsonWriter
      implicit val ConfigT: Type[UBJsonConfig] = CTypes.UBJsonConfig
      cache.get3Ary[B, UBJsonWriter, UBJsonConfig, Unit]("cached-encode-method")
    }
    def setHelper[B: Type](
        helper: (Expr[B], Expr[UBJsonWriter], Expr[UBJsonConfig]) => MIO[Expr[Unit]]
    ): MIO[Unit] = {
      implicit val UnitT: Type[Unit] = CTypes.Unit
      implicit val UBJsonWriterT: Type[UBJsonWriter] = CTypes.UBJsonWriter
      implicit val ConfigT: Type[UBJsonConfig] = CTypes.UBJsonConfig
      val defBuilder =
        ValDefBuilder.ofDef3[B, UBJsonWriter, UBJsonConfig, Unit](s"encode_${Type[B].shortName}")
      for {
        _ <- Log.info(s"Forward-declaring encode helper for ${Type[B].prettyPrint}")
        _ <- cache.forwardDeclare("cached-encode-method", defBuilder)
        builtBuilder <- defBuilder.traverse { case (_, (value, writer, config)) =>
          helper(value, writer, config)
        }
        currentCache <- cache.get
        _ <- cache.set(builtBuilder.buildCached(currentCache, "cached-encode-method"))
        _ <- Log.info(s"Defined encode helper for ${Type[B].prettyPrint}")
      } yield ()
    }

    override def toString: String =
      s"encode[${tpe.prettyPrint}](value = ${value.prettyPrint}, writer = ${writer.prettyPrint}, config = ${config.prettyPrint})"
  }
  object EncoderCtx {

    def from[A: Type](
        value: Expr[A],
        writer: Expr[UBJsonWriter],
        config: Expr[UBJsonConfig],
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
    // Cache ALL types (including built-ins, options, collections) as helper defs.
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
          EncoderUseImplicitWhenAvailableRule,
          EncoderHandleAsBuiltInRule,
          EncoderHandleAsValueTypeRule,
          EncoderHandleAsOptionRule,
          EncoderHandleAsMapRule,
          EncoderHandleAsCollectionRule,
          EncoderHandleAsSingletonRule,
          EncoderHandleAsCaseClassRule,
          EncoderHandleAsEnumRule
        )(_[A]).flatMap {
          case Right(result) =>
            Log.info(s"Derived encoder for ${Type[A].prettyPrint}: ${result.prettyPrint}") >>
              MIO.pure(result)
          case Left(reasons) =>
            val reasonsStrings = reasons.toListMap
              // .removed(EncoderUseCachedDefWhenAvailableRule)
              .view.map { case (rule, reasons) =>
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
      reader: Expr[UBJsonReader],
      config: Expr[UBJsonConfig],
      cache: MLocal[ValDefsCache],
      derivedType: Option[??]
  ) {

    def nest[B: Type](newReader: Expr[UBJsonReader]): DecoderCtx[B] = copy[B](
      tpe = Type[B],
      reader = newReader
    )

    def nestInCache(
        newReader: Expr[UBJsonReader],
        newConfig: Expr[UBJsonConfig]
    ): DecoderCtx[A] = copy(
      reader = newReader,
      config = newConfig
    )

    def getInstance[B: Type]: MIO[Option[Expr[UBJsonValueCodec[B]]]] = {
      implicit val CodecB: Type[UBJsonValueCodec[B]] = CTypes.UBJsonValueCodec[B]
      cache.get0Ary[UBJsonValueCodec[B]]("cached-codec-instance")
    }
    def setInstance[B: Type](instance: Expr[UBJsonValueCodec[B]]): MIO[Unit] = {
      implicit val CodecB: Type[UBJsonValueCodec[B]] = CTypes.UBJsonValueCodec[B]
      Log.info(s"Caching UBJsonValueCodec instance for ${Type[B].prettyPrint}") >>
        cache.buildCachedWith(
          "cached-codec-instance",
          ValDefBuilder.ofLazy[UBJsonValueCodec[B]](s"codec_${Type[B].shortName}")
        )(_ => instance)
    }

    def getHelper[B: Type]: MIO[Option[(Expr[UBJsonReader], Expr[UBJsonConfig]) => Expr[B]]] = {
      implicit val UBJsonReaderT: Type[UBJsonReader] = CTypes.UBJsonReader
      implicit val ConfigT: Type[UBJsonConfig] = CTypes.UBJsonConfig
      cache.get2Ary[UBJsonReader, UBJsonConfig, B]("cached-decode-method")
    }
    def setHelper[B: Type](
        helper: (Expr[UBJsonReader], Expr[UBJsonConfig]) => MIO[Expr[B]]
    ): MIO[Unit] = {
      implicit val UBJsonReaderT: Type[UBJsonReader] = CTypes.UBJsonReader
      implicit val ConfigT: Type[UBJsonConfig] = CTypes.UBJsonConfig
      val defBuilder =
        ValDefBuilder.ofDef2[UBJsonReader, UBJsonConfig, B](s"decode_${Type[B].shortName}")
      for {
        _ <- Log.info(s"Forward-declaring decode helper for ${Type[B].prettyPrint}")
        _ <- cache.forwardDeclare("cached-decode-method", defBuilder)
        builtBuilder <- defBuilder.traverse { case (_, (reader, config)) =>
          helper(reader, config)
        }
        currentCache <- cache.get
        _ <- cache.set(builtBuilder.buildCached(currentCache, "cached-decode-method"))
        _ <- Log.info(s"Defined decode helper for ${Type[B].prettyPrint}")
      } yield ()
    }

    override def toString: String =
      s"decode[${tpe.prettyPrint}](reader = ${reader.prettyPrint}, config = ${config.prettyPrint})"
  }
  object DecoderCtx {

    def from[A: Type](
        reader: Expr[UBJsonReader],
        config: Expr[UBJsonConfig],
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
    // Cache ALL types (including built-ins, options, collections) as helper defs.
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

  private def deriveDecoderViaRules[A: DecoderCtx]: MIO[Expr[A]] =
    Log
      .namedScope(s"Deriving decoder for type ${Type[A].prettyPrint}") {
        Rules(
          DecoderUseImplicitWhenAvailableRule,
          DecoderHandleAsBuiltInRule,
          DecoderHandleAsValueTypeRule,
          DecoderHandleAsOptionRule,
          DecoderHandleAsMapRule,
          DecoderHandleAsCollectionRule,
          DecoderHandleAsSingletonRule,
          DecoderHandleAsCaseClassRule,
          DecoderHandleAsEnumRule
        )(_[A]).flatMap {
          case Right(result) =>
            Log.info(s"Derived decoder for ${Type[A].prettyPrint}: ${result.prettyPrint}") >>
              MIO.pure(result)
          case Left(reasons) =>
            val reasonsStrings = reasons.toListMap
              // .removed(DecoderUseCachedDefWhenAvailableRule)
              .view.map { case (rule, reasons) =>
                if (reasons.isEmpty) s"The rule ${rule.name} was not applicable"
                else
                  s" - The rule ${rule.name} was not applicable, for the following reasons: ${reasons.mkString(", ")}"
              }.toList
            val err = CodecDerivationError.UnsupportedType(Type[A].prettyPrint, reasonsStrings)
            Log.error(err.message) >> MIO.fail(err)
        }
      }

  // Types

  private[compiletime] object CTypes {

    def UBJsonValueCodec: Type.Ctor1[UBJsonValueCodec] = Type.Ctor1.of[UBJsonValueCodec]
    val CodecLogDerivation: Type[hearth.kindlings.ubjsonderivation.UBJsonValueCodec.LogDerivation] =
      Type.of[hearth.kindlings.ubjsonderivation.UBJsonValueCodec.LogDerivation]
    val UBJsonConfig: Type[UBJsonConfig] = Type.of[UBJsonConfig]
    val UBJsonReader: Type[UBJsonReader] = Type.of[UBJsonReader]
    val UBJsonWriter: Type[UBJsonWriter] = Type.of[UBJsonWriter]
    val String: Type[String] = Type.of[String]
    val Unit: Type[Unit] = Type.of[Unit]
    val Any: Type[Any] = Type.of[Any]
    val ArrayAny: Type[Array[Any]] = Type.of[Array[Any]]
    val ListString: Type[List[String]] = Type.of[List[String]]
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
  }
}
