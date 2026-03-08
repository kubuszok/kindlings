package hearth.kindlings.ubjsonderivation.internal.compiletime

import hearth.MacroCommons
import hearth.fp.data.NonEmptyList
import hearth.fp.effect.*
import hearth.fp.syntax.*
import hearth.std.*

import hearth.kindlings.ubjsonderivation.{KindlingsUBJsonValueCodec, UBJsonConfig, UBJsonReader, UBJsonValueCodec, UBJsonWriter}
import hearth.kindlings.ubjsonderivation.annotations.{fieldName as fieldNameAnn, stringified, transientField}
import hearth.kindlings.ubjsonderivation.internal.runtime.UBJsonDerivationUtils

trait CodecMacrosImpl { this: MacroCommons & StdExtensions & AnnotationSupport =>

  // Entrypoints

  /** Derive a combined UBJsonValueCodec for type A. */
  @scala.annotation.nowarn("msg=is never used")
  def deriveCodecTypeClass[A: Type](configExpr: Expr[UBJsonConfig]): Expr[KindlingsUBJsonValueCodec[A]] = {
    implicit val CodecA: Type[UBJsonValueCodec[A]] = CTypes.UBJsonValueCodec[A]
    implicit val KindlingsCodecA: Type[KindlingsUBJsonValueCodec[A]] = CTypes.KindlingsUBJsonValueCodec[A]
    implicit val ConfigT: Type[UBJsonConfig] = CTypes.UBJsonConfig
    implicit val UBJsonReaderT: Type[UBJsonReader] = CTypes.UBJsonReader
    implicit val UBJsonWriterT: Type[UBJsonWriter] = CTypes.UBJsonWriter
    implicit val UnitT: Type[Unit] = CTypes.Unit

    deriveCodecFromCtxAndAdaptForEntrypoint[A, KindlingsUBJsonValueCodec[A]]("KindlingsUBJsonValueCodec.derived") {
      case (encodeFn, decodeFn, nullValueExpr) =>
        Expr.quote {
          new KindlingsUBJsonValueCodec[A] {
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
              _ <- MIO.scoped { rs =>
                rs(cache.buildCachedWith("codec-encode-body", defBuilder) { case (_, (v, w, c)) =>
                  rs(deriveEncoderRecursively[A](using EncoderCtx.from(v, w, c, cache, selfType)))
                })
              }
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
              _ <- MIO.scoped { rs =>
                rs(cache.buildCachedWith("codec-decode-body", defBuilder) { case (_, (r, c)) =>
                  rs(deriveDecoderRecursively[A](using DecoderCtx.from(r, c, cache, selfType)))
                })
              }
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
          "Enable debug logging with: import hearth.kindlings.ubjsonderivation.debug.logDerivationForKindlingsUBJsonValueCodec or scalac option -Xmacro-settings:ubjsonDerivation.logDerivation=true"
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
    implicit val LogDerivation: Type[KindlingsUBJsonValueCodec.LogDerivation] = CTypes.CodecLogDerivation
    def logDerivationImported = Expr.summonImplicit[KindlingsUBJsonValueCodec.LogDerivation].isDefined

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
    Log
      .namedScope(s"Deriving encoder for type ${Type[A].prettyPrint}") {
        Rules(
          EncUseCachedDefWhenAvailableRule,
          EncUseImplicitWhenAvailableRule,
          EncHandleAsBuiltInRule,
          EncHandleAsValueTypeRule,
          EncHandleAsOptionRule,
          EncHandleAsMapRule,
          EncHandleAsCollectionRule,
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

    @scala.annotation.nowarn("msg=is never used")
    private def callCachedInstance[A: EncoderCtx](
        instance: Expr[UBJsonValueCodec[A]]
    ): MIO[Rule.Applicability[Expr[Unit]]] =
      Log.info(s"Found cached codec instance for ${Type[A].prettyPrint}") >> MIO.pure(Rule.matched(Expr.quote {
        Expr.splice(instance).encode(Expr.splice(ectx.writer), Expr.splice(ectx.value))
      }))

    private def callCachedHelper[A: EncoderCtx](
        helperCall: (Expr[A], Expr[UBJsonWriter], Expr[UBJsonConfig]) => Expr[Unit]
    ): MIO[Rule.Applicability[Expr[Unit]]] =
      Log.info(s"Found cached encoder helper for ${Type[A].prettyPrint}") >> MIO.pure(
        Rule.matched(helperCall(ectx.value, ectx.writer, ectx.config))
      )

    private def yieldUnsupported[A: EncoderCtx]: MIO[Rule.Applicability[Expr[Unit]]] =
      MIO.pure(Rule.yielded(s"The type ${Type[A].prettyPrint} does not have a cached encoder"))
  }

  object EncUseImplicitWhenAvailableRule extends EncoderDerivationRule("use implicit when available") {

    lazy val ignoredImplicits: Seq[UntypedMethod] = {
      val ours = Type.of[KindlingsUBJsonValueCodec.type].methods.collect {
        case method if method.value.name == "derived" => method.value.asUntyped
      }
      ours
    }

    @scala.annotation.nowarn("msg=is never used")
    def apply[A: EncoderCtx]: MIO[Rule.Applicability[Expr[Unit]]] =
      Log.info(s"Attempting to use implicit UBJsonValueCodec for ${Type[A].prettyPrint}") >> {
        if (ectx.derivedType.exists(_.Underlying =:= Type[A]))
          MIO.pure(
            Rule.yielded(s"The type ${Type[A].prettyPrint} is the type being derived, skipping implicit search")
          )
        else
          CTypes.UBJsonValueCodec[A].summonExprIgnoring(ignoredImplicits*).toEither match {
            case Right(instanceExpr) =>
              Log.info(s"Found implicit codec ${instanceExpr.prettyPrint}, using directly") >>
                MIO.pure(Rule.matched(Expr.quote {
                  Expr.splice(instanceExpr).encode(Expr.splice(ectx.writer), Expr.splice(ectx.value))
                }))
            case Left(reason) =>
              MIO.pure(
                Rule.yielded(
                  s"The type ${Type[A].prettyPrint} does not have an implicit UBJsonValueCodec instance: $reason"
                )
              )
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
            Some(Expr.quote(Expr.splice(writer).writeInt(Expr.splice(value).asInstanceOf[Int])))
          else if (Type[A] =:= Type.of[Long])
            Some(Expr.quote(Expr.splice(writer).writeLong(Expr.splice(value).asInstanceOf[Long])))
          else if (Type[A] =:= Type.of[Double])
            Some(Expr.quote(Expr.splice(writer).writeDouble(Expr.splice(value).asInstanceOf[Double])))
          else if (Type[A] =:= Type.of[Float])
            Some(Expr.quote(Expr.splice(writer).writeFloat(Expr.splice(value).asInstanceOf[Float])))
          else if (Type[A] =:= Type.of[Boolean])
            Some(Expr.quote(Expr.splice(writer).writeBoolean(Expr.splice(value).asInstanceOf[Boolean])))
          else if (Type[A] =:= Type.of[String])
            Some(Expr.quote(Expr.splice(writer).writeString(Expr.splice(value).asInstanceOf[String])))
          else if (Type[A] =:= Type.of[Byte])
            Some(Expr.quote(Expr.splice(writer).writeInt8(Expr.splice(value).asInstanceOf[Byte])))
          else if (Type[A] =:= Type.of[Short])
            Some(Expr.quote(Expr.splice(writer).writeInt(Expr.splice(value).asInstanceOf[Short].toInt)))
          else if (Type[A] =:= Type.of[Char])
            Some(Expr.quote(Expr.splice(writer).writeChar(Expr.splice(value).asInstanceOf[Char])))
          else if (Type[A] =:= Type.of[BigDecimal])
            Some(Expr.quote(Expr.splice(writer).writeBigDecimal(Expr.splice(value).asInstanceOf[BigDecimal])))
          else if (Type[A] =:= Type.of[BigInt])
            Some(Expr.quote(Expr.splice(writer).writeBigDecimal(BigDecimal(Expr.splice(value).asInstanceOf[BigInt]))))
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
    implicit val UBJsonWriterT: Type[UBJsonWriter] = CTypes.UBJsonWriter

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

    @scala.annotation.nowarn("msg=is never used")
    private def deriveMapEntries[A: EncoderCtx, Pair: Type](
        isMap: IsMapOf[A, Pair]
    ): MIO[Rule.Applicability[Expr[Unit]]] = {
      import isMap.{Key, Value}
      if (Key <:< Type[String]) {
        LambdaBuilder
          .of1[Value]("mapValue")
          .traverse { valueExpr =>
            deriveEncoderRecursively[Value](using ectx.nest(valueExpr))
          }
          .map { valueBuilder =>
            val valueLambda = valueBuilder.build[Unit]
            val iterableExpr = isMap.asIterable(ectx.value)
            Rule.matched(Expr.quote {
              UBJsonDerivationUtils.writeMapStringKeyed[Value](
                Expr.splice(ectx.writer),
                Expr.splice(iterableExpr).asInstanceOf[Iterable[(String, Value)]],
                Expr.splice(valueLambda)
              )
            })
          }
      } else {
        // Non-String keys — encode key via .toString
        LambdaBuilder
          .of1[Value]("mapValue")
          .traverse { valueExpr =>
            deriveEncoderRecursively[Value](using ectx.nest(valueExpr))
          }
          .map { valueBuilder =>
            val valueLambda = valueBuilder.build[Unit]
            val iterableExpr = isMap.asIterable(ectx.value)
            Rule.matched(Expr.quote {
              UBJsonDerivationUtils.writeMapWithKeyEncoder[Key, Value](
                Expr.splice(ectx.writer),
                Expr.splice(iterableExpr).asInstanceOf[Iterable[(Key, Value)]],
                (k: Key) => k.toString,
                Expr.splice(valueLambda)
              )
            })
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
                  UBJsonDerivationUtils.writeArray[Item](
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
      implicit val UBJsonWriterT: Type[UBJsonWriter] = CTypes.UBJsonWriter
      implicit val UnitT: Type[Unit] = CTypes.Unit
      implicit val AnyT: Type[Any] = CTypes.Any
      implicit val fieldNameT: Type[fieldNameAnn] = CTypes.FieldName
      implicit val transientFieldT: Type[transientField] = CTypes.TransientField

      val allFields = caseClass.caseFieldValuesAt(ectx.value).toList

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
                    deriveEncoderRecursively[Field](using ectx.nest(fieldExpr)).map { fieldEnc =>
                      val nameOverride =
                        paramsByName.get(fName).flatMap(p => getAnnotationStringArg[fieldNameAnn](p))

                      val unconditionalWrite: Expr[Unit] = nameOverride match {
                        case Some(customName) =>
                          Expr.quote {
                            Expr.splice(ectx.writer).writeFieldName(Expr.splice(Expr(customName)))
                            Expr.splice(fieldEnc)
                          }
                        case None =>
                          Expr.quote {
                            Expr
                              .splice(ectx.writer)
                              .writeFieldName(Expr.splice(ectx.config).fieldNameMapper(Expr.splice(Expr(fName))))
                            Expr.splice(fieldEnc)
                          }
                      }

                      // Determine transient skip conditions
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
                        val isMapF = Type[Field] match { case IsMap(_) => true; case _ => false }
                        val isCollF = Type[Field] match { case IsCollection(_) => true; case _ => false }
                        isMapF || isCollF
                      }
                      val isEmptyCapable = isStringField || isCollectionField

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
      implicit val UBJsonWriterT: Type[UBJsonWriter] = CTypes.UBJsonWriter
      implicit val StringT: Type[String] = CTypes.String

      val childrenList = enumm.directChildren.toList
      val allCaseObjects = Type[A].isEnumeration || Type[A].isJavaEnum || childrenList.forall { case (_, child) =>
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

            // Fields-only encoding for discriminator mode
            val fieldsOnlyMIO: MIO[Expr[Unit]] =
              if (Type[A].isEnumeration || Type[A].isJavaEnum) MIO.pure(Expr.quote(()): Expr[Unit])
              else
                SingletonValue.unapply(Type[EnumCase]) match {
                  case Some(_) => MIO.pure(Expr.quote(()): Expr[Unit])
                  case None =>
                    CaseClass.parse[EnumCase].toOption match {
                      case Some(caseClass) =>
                        EncHandleAsCaseClassRule.encodeCaseClassFieldsOnly[EnumCase](caseClass)(using
                          ectx.nest(enumCaseValue)
                        )
                      case None => MIO.pure(Expr.quote(()): Expr[Unit])
                    }
                }

            // Full encoding for wrapper mode
            val fullEncMIO: MIO[Expr[Unit]] =
              if (Type[A].isEnumeration || Type[A].isJavaEnum) MIO.pure(Expr.quote(()): Expr[Unit])
              else deriveEncoderRecursively[EnumCase](using ectx.nest(enumCaseValue))

            for {
              fieldsOnly <- fieldsOnlyMIO
              fullEnc <- fullEncMIO
            } yield
              if (allCaseObjects) {
                Expr.quote {
                  val config = Expr.splice(ectx.config)
                  val name = config.adtLeafClassNameMapper(Expr.splice(Expr(caseName)))
                  if (config.enumAsStrings) {
                    UBJsonDerivationUtils.writeEnumAsString(Expr.splice(ectx.writer), name)
                  } else {
                    config.discriminatorFieldName match {
                      case Some(discriminatorField) =>
                        Expr.splice(ectx.writer).writeObjectStart()
                        Expr.splice(ectx.writer).writeFieldName(discriminatorField)
                        Expr.splice(ectx.writer).writeString(name)
                        Expr.splice(fieldsOnly)
                        Expr.splice(ectx.writer).writeObjectEnd()
                      case None =>
                        UBJsonDerivationUtils.writeWrapped(Expr.splice(ectx.writer), name) {
                          Expr.splice(fullEnc)
                        }
                    }
                  }
                }
              } else {
                Expr.quote {
                  val config = Expr.splice(ectx.config)
                  val name = config.adtLeafClassNameMapper(Expr.splice(Expr(caseName)))
                  config.discriminatorFieldName match {
                    case Some(discriminatorField) =>
                      Expr.splice(ectx.writer).writeObjectStart()
                      Expr.splice(ectx.writer).writeFieldName(discriminatorField)
                      Expr.splice(ectx.writer).writeString(name)
                      Expr.splice(fieldsOnly)
                      Expr.splice(ectx.writer).writeObjectEnd()
                    case None =>
                      UBJsonDerivationUtils.writeWrapped(Expr.splice(ectx.writer), name) {
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
    Log
      .namedScope(s"Deriving decoder for type ${Type[A].prettyPrint}") {
        Rules(
          DecUseCachedDefWhenAvailableRule,
          DecUseImplicitWhenAvailableRule,
          DecHandleAsBuiltInRule,
          DecHandleAsValueTypeRule,
          DecHandleAsOptionRule,
          DecHandleAsMapRule,
          DecHandleAsCollectionRule,
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
          case Some(instance) =>
            Log.info(s"Found cached codec instance for ${Type[A].prettyPrint}") >> MIO.pure(
              Rule.matched(Expr.quote {
                Expr.splice(instance).decode(Expr.splice(dctx.reader))
              })
            )
          case None =>
            dctx.getHelper[A].flatMap {
              case Some(helperCall) =>
                Log.info(s"Found cached decoder helper for ${Type[A].prettyPrint}") >> MIO.pure(
                  Rule.matched(helperCall(dctx.reader, dctx.config))
                )
              case None =>
                MIO.pure(Rule.yielded(s"The type ${Type[A].prettyPrint} does not have a cached decoder"))
            }
        }
  }

  object DecUseImplicitWhenAvailableRule extends DecoderDerivationRule("use implicit when available") {

    lazy val ignoredImplicits: Seq[UntypedMethod] = {
      val ours = Type.of[KindlingsUBJsonValueCodec.type].methods.collect {
        case method if method.value.name == "derived" => method.value.asUntyped
      }
      ours
    }

    @scala.annotation.nowarn("msg=is never used")
    def apply[A: DecoderCtx]: MIO[Rule.Applicability[Expr[A]]] =
      Log.info(s"Attempting to use implicit UBJsonValueCodec for ${Type[A].prettyPrint}") >> {
        if (dctx.derivedType.exists(_.Underlying =:= Type[A]))
          MIO.pure(
            Rule.yielded(s"The type ${Type[A].prettyPrint} is the type being derived, skipping implicit search")
          )
        else
          CTypes.UBJsonValueCodec[A].summonExprIgnoring(ignoredImplicits*).toEither match {
            case Right(instanceExpr) =>
              Log.info(s"Found implicit codec ${instanceExpr.prettyPrint}, using directly") >>
                MIO.pure(Rule.matched(Expr.quote {
                  Expr.splice(instanceExpr).decode(Expr.splice(dctx.reader))
                }))
            case Left(reason) =>
              MIO.pure(
                Rule.yielded(
                  s"The type ${Type[A].prettyPrint} does not have an implicit UBJsonValueCodec instance: $reason"
                )
              )
          }
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
            Some(Expr.quote(Expr.splice(reader).readString().asInstanceOf[A]))
          else if (Type[A] =:= Type.of[Byte])
            Some(Expr.quote(Expr.splice(reader).readByte().asInstanceOf[A]))
          else if (Type[A] =:= Type.of[Short])
            Some(Expr.quote(Expr.splice(reader).readShort().asInstanceOf[A]))
          else if (Type[A] =:= Type.of[Char])
            Some(Expr.quote(Expr.splice(reader).readChar().asInstanceOf[A]))
          else if (Type[A] =:= Type.of[BigDecimal])
            Some(Expr.quote {
              UBJsonDerivationUtils
                .validateBigDecimal(
                  Expr.splice(reader),
                  Expr.splice(reader).readBigDecimal(),
                  Expr.splice(dctx.config).bigDecimalPrecision,
                  Expr.splice(dctx.config).bigDecimalScaleLimit,
                  Expr.splice(dctx.config).bigDecimalDigitsLimit
                )
                .asInstanceOf[A]
            })
          else if (Type[A] =:= Type.of[BigInt])
            Some(Expr.quote {
              val bd = Expr.splice(reader).readBigDecimal()
              UBJsonDerivationUtils
                .validateBigInt(
                  Expr.splice(reader),
                  bd.toBigInt,
                  Expr.splice(dctx.config).bigDecimalDigitsLimit
                )
                .asInstanceOf[A]
            })
          else
            None

        MIO.pure(result match {
          case Some(expr) => Rule.matched(expr)
          case None       => Rule.yielded(s"The type ${Type[A].prettyPrint} is not a built-in type")
        })
      }
  }

  object DecHandleAsValueTypeRule extends DecoderDerivationRule("handle as value type when possible") {

    @scala.annotation.nowarn("msg=is never used")
    def apply[A: DecoderCtx]: MIO[Rule.Applicability[Expr[A]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as a value type") >> {
        Type[A] match {
          case IsValueType(isValueType) =>
            import isValueType.Underlying as Inner

            LambdaBuilder
              .of1[Inner]("inner")
              .traverse { innerExpr =>
                isValueType.value.wrap match {
                  case _: CtorLikeOf.EitherStringOrValue[?, ?] =>
                    val wrapResult = isValueType.value.wrap.apply(innerExpr).asInstanceOf[Expr[Either[String, A]]]
                    MIO.pure(Expr.quote {
                      Expr.splice(wrapResult) match {
                        case scala.Right(v)  => v
                        case scala.Left(msg) => Expr.splice(dctx.reader).decodeError(msg)
                      }
                    })
                  case _ =>
                    MIO.pure(isValueType.value.wrap.apply(innerExpr).asInstanceOf[Expr[A]])
                }
              }
              .flatMap { builder =>
                val wrapLambda = builder.build[A]
                deriveDecoderRecursively[Inner](using dctx.nest[Inner](dctx.reader)).map { innerDecoded =>
                  Rule.matched(Expr.quote {
                    Expr.splice(wrapLambda).apply(Expr.splice(innerDecoded))
                  })
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
            implicit val UBJsonReaderT: Type[UBJsonReader] = CTypes.UBJsonReader

            LambdaBuilder
              .of1[UBJsonReader]("innerReader")
              .traverse { innerReaderExpr =>
                deriveDecoderRecursively[Inner](using dctx.nest[Inner](innerReaderExpr))
              }
              .map { builder =>
                val decodeFn = builder.build[Inner]
                Rule.matched(Expr.quote {
                  UBJsonDerivationUtils
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

    @scala.annotation.nowarn("msg=is never used")
    private def decodeMapEntries[A: DecoderCtx, Pair: Type](
        isMap: IsMapOf[A, Pair]
    ): MIO[Rule.Applicability[Expr[A]]] = {
      import isMap.{Key, Value, CtorResult}
      implicit val StringT: Type[String] = CTypes.String
      implicit val UBJsonReaderT: Type[UBJsonReader] = CTypes.UBJsonReader

      if (Key <:< Type[String]) {
        LambdaBuilder
          .of1[UBJsonReader]("valueReader")
          .traverse { valueReaderExpr =>
            deriveDecoderRecursively[Value](using dctx.nest[Value](valueReaderExpr))
          }
          .map { valueBuilder =>
            val decodeFn = valueBuilder.build[Value]
            val factoryExpr = isMap.factory
            Rule.matched(Expr.quote {
              UBJsonDerivationUtils
                .readMap[Value, A](
                  Expr.splice(dctx.reader),
                  Expr.splice(decodeFn),
                  Expr.splice(factoryExpr).asInstanceOf[scala.collection.Factory[(String, Value), A]],
                  Expr.splice(dctx.config).mapMaxInsertNumber
                )
                .asInstanceOf[A]
            })
          }
      } else {
        // Non-String keys — parse from string
        LambdaBuilder
          .of1[UBJsonReader]("valueReader")
          .traverse { valueReaderExpr =>
            deriveDecoderRecursively[Value](using dctx.nest[Value](valueReaderExpr))
          }
          .map { valueBuilder =>
            val decodeFn = valueBuilder.build[Value]
            val factoryExpr = isMap.factory

            // Build a String => Key function based on the key type
            val keyDecoder: Expr[String => Key] =
              if (Key =:= Type.of[Int]) Expr.quote((s: String) => s.toInt.asInstanceOf[Key])
              else if (Key =:= Type.of[Long]) Expr.quote((s: String) => s.toLong.asInstanceOf[Key])
              else if (Key =:= Type.of[Double]) Expr.quote((s: String) => s.toDouble.asInstanceOf[Key])
              else Expr.quote((s: String) => s.asInstanceOf[Key])

            Rule.matched(Expr.quote {
              UBJsonDerivationUtils
                .readMapWithKeyDecoder[Key, Value, A](
                  Expr.splice(dctx.reader),
                  Expr.splice(keyDecoder),
                  Expr.splice(decodeFn),
                  Expr.splice(factoryExpr).asInstanceOf[scala.collection.Factory[(Key, Value), A]],
                  Expr.splice(dctx.config).mapMaxInsertNumber
                )
                .asInstanceOf[A]
            })
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
            implicit val UBJsonReaderT: Type[UBJsonReader] = CTypes.UBJsonReader

            LambdaBuilder
              .of1[UBJsonReader]("itemReader")
              .traverse { itemReaderExpr =>
                deriveDecoderRecursively[Item](using dctx.nest[Item](itemReaderExpr))
              }
              .map { builder =>
                val decodeFn = builder.build[Item]
                val factoryExpr = isCollection.value.factory
                Rule.matched(Expr.quote {
                  UBJsonDerivationUtils
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

  object DecHandleAsSingletonRule extends DecoderDerivationRule("handle as singleton when possible") {

    @scala.annotation.nowarn("msg=is never used")
    def apply[A: DecoderCtx]: MIO[Rule.Applicability[Expr[A]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as a singleton") >> {
        SingletonValue.parse[A].toEither match {
          case Right(sv) =>
            implicit val UBJsonReaderT: Type[UBJsonReader] = CTypes.UBJsonReader
            MIO.pure(Rule.matched(Expr.quote {
              UBJsonDerivationUtils.readEmptyObject(Expr.splice(dctx.reader))
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
      implicit val UBJsonReaderT: Type[UBJsonReader] = CTypes.UBJsonReader
      implicit val fieldNameT: Type[fieldNameAnn] = CTypes.FieldName
      implicit val transientFieldT: Type[transientField] = CTypes.TransientField

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
                MIO.pure(Expr.quote {
                  UBJsonDerivationUtils.readEmptyObject(Expr.splice(dctx.reader))
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
              Log.namedScope(s"Deriving decoder for field $fName: ${Type[Field].prettyPrint}") {
                deriveFieldDecoder[Field].map { decodeFn =>
                  val decodeFnErased: Expr[UBJsonReader => Any] = Expr.quote { (r: UBJsonReader) =>
                    Expr.splice(decodeFn).apply(r).asInstanceOf[Any]
                  }
                  val makeAccessor: Expr[Array[Any]] => (String, Expr_??) = { arrExpr =>
                    val typedExpr = Expr.quote {
                      UBJsonDerivationUtils.unsafeCast(
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
                  // Require check
                  val requireCheckExprs: List[Expr[Unit]] =
                    nonTransientWithIndex.flatMap { case ((fName, param), idx) =>
                      import param.tpe.Underlying as Field
                      val checks = List.newBuilder[Expr[Unit]]

                      if (param.hasDefault) {
                        checks += Expr.quote {
                          if (
                            Expr.splice(dctx.config).requireDefaultFields &&
                            Expr.splice(decodedValuesExpr)(Expr.splice(Expr(idx))) == null
                          )
                            UBJsonDerivationUtils.throwMissingField(Expr.splice(Expr(fName)))
                        }
                      }

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
                            UBJsonDerivationUtils.throwMissingField(Expr.splice(Expr(fName)))
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

                  // Transient init
                  val transientInitExprs: List[Expr[Unit]] =
                    nonTransientWithIndex.flatMap { case ((fName, param), idx) =>
                      import param.tpe.Underlying as Field
                      val initSteps = List.newBuilder[Expr[Unit]]

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

                  val nonTransientFieldMap: Map[String, Expr_??] =
                    fieldDataList.map(_._4(decodedValuesExpr)).toMap
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

                  val fieldMappings = fieldDataList.map { case (name, index, decodeFnErased, _, nameOverride) =>
                    (name, index, decodeFnErased, nameOverride)
                  }

                  Expr.quote {
                    val _seen: Array[Boolean] =
                      if (Expr.splice(dctx.config).checkFieldDuplication)
                        new Array[Boolean](Expr.splice(Expr(fieldMappings.size)))
                      else null
                    UBJsonDerivationUtils.readObject[A](
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
                                    UBJsonDerivationUtils.throwDuplicateField(reader, fieldName)
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
                                    UBJsonDerivationUtils.throwDuplicateField(reader, fieldName)
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

    /** Decode fields from an already-opened object (for discriminator mode). */
    @scala.annotation.nowarn("msg=is never used|unused explicit parameter|Non local returns")
    private[compiletime] def decodeCaseClassFieldsInline[A: DecoderCtx](
        caseClass: CaseClass[A]
    ): MIO[Expr[A]] = {
      implicit val StringT: Type[String] = CTypes.String
      implicit val UBJsonReaderT: Type[UBJsonReader] = CTypes.UBJsonReader
      implicit val fieldNameT: Type[fieldNameAnn] = CTypes.FieldName
      implicit val transientFieldT: Type[transientField] = CTypes.TransientField

      val constructor = caseClass.primaryConstructor
      val fieldsList = constructor.parameters.flatten.toList

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
                  while (!reader.isObjectEnd()) {
                    val _ = reader.readFieldName()
                    reader.skip()
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
              Log.namedScope(s"Deriving decoder for field $fName: ${Type[Field].prettyPrint}") {
                deriveFieldDecoder[Field].map { decodeFn =>
                  val decodeFnErased: Expr[UBJsonReader => Any] = Expr.quote { (r: UBJsonReader) =>
                    Expr.splice(decodeFn).apply(r).asInstanceOf[Any]
                  }
                  val makeAccessor: Expr[Array[Any]] => (String, Expr_??) = { arrExpr =>
                    val typedExpr = Expr.quote {
                      UBJsonDerivationUtils.unsafeCast(
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
                  val nonTransientFieldMap: Map[String, Expr_??] =
                    fieldDataList.map(_._4(decodedValuesExpr)).toMap
                  val fieldMap = nonTransientFieldMap ++ transientDefaults
                  caseClass.primaryConstructor(fieldMap) match {
                    case Right(constructExpr) => MIO.pure(constructExpr)
                    case Left(error)          =>
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
                    UBJsonDerivationUtils.readObjectInline[A](
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
                                arr(Expr.splice(Expr(index))) = Expr.splice(decodeFnErased).apply(reader)
                              } else Expr.splice(elseExpr)
                            }
                          case ((name, index, decodeFnErased, None), elseExpr) =>
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

  @scala.annotation.nowarn("msg=is never used|unused explicit parameter")
  private def deriveFieldDecoder[Field: Type](implicit ctx: DecoderCtx[?]): MIO[Expr[UBJsonReader => Field]] = {
    implicit val UBJsonReaderT: Type[UBJsonReader] = CTypes.UBJsonReader

    CTypes
      .UBJsonValueCodec[Field]
      .summonExprIgnoring(DecUseImplicitWhenAvailableRule.ignoredImplicits*)
      .toEither match {
      case Right(codecExpr) =>
        Log.info(s"Found implicit UBJsonValueCodec[${Type[Field].prettyPrint}]") >> MIO.pure(
          Expr.quote { (r: UBJsonReader) =>
            Expr.splice(codecExpr).decode(r)
          }
        )
      case Left(_) =>
        Log.info(s"Building decoder for ${Type[Field].prettyPrint} via recursive derivation") >>
          LambdaBuilder
            .of1[UBJsonReader]("fieldReader")
            .traverse { fieldReaderExpr =>
              deriveDecoderRecursively[Field](using ctx.nest[Field](fieldReaderExpr))
            }
            .map { builder =>
              builder.build[Field]
            }
    }
  }

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
      implicit val UBJsonReaderT: Type[UBJsonReader] = CTypes.UBJsonReader
      implicit val StringT: Type[String] = CTypes.String
      implicit val ListStringT: Type[List[String]] = CTypes.ListString

      val childrenList = enumm.directChildren.toList

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

          // For each child, derive wrapper-mode and inline (discriminator) decoders
          children
            .parTraverse { case (childName, child) =>
              import child.Underlying as ChildType
              Log.namedScope(s"Deriving decoder for enum case $childName: ${Type[ChildType].prettyPrint}") {
                for {
                  wrapper <- deriveChildDecoder[A, ChildType](childName)
                  inline <- deriveChildDecoderInline[A, ChildType](childName)
                  stringEnum <-
                    if (allCaseObjects)
                      deriveChildDecoderStringEnum[A, ChildType](childName).map(Some(_))
                    else MIO.pure(None)
                } yield (wrapper, inline, stringEnum)
              }
            }
            .flatMap { allDispatchers =>
              val wrapperDispatchers = allDispatchers.toList.map(_._1)
              val inlineDispatchers = allDispatchers.toList.map(_._2)

              def buildErrorExpr(typeNameExpr: Expr[String]): Expr[A] = Expr.quote {
                Expr
                  .splice(dctx.reader)
                  .decodeError(
                    "Unknown type discriminator: " + Expr.splice(typeNameExpr) +
                      ". Expected one of: " + Expr.splice(Expr(knownNames)).mkString(", ")
                  ): A
              }

              def buildDispatchLambda(
                  dispatchers: List[(Expr[String], Expr[UBJsonReader], Expr[A]) => Expr[A]]
              ): MIO[Expr[String => A]] =
                LambdaBuilder
                  .of1[String]("typeName")
                  .traverse { typeNameExpr =>
                    MIO.pure(dispatchers.foldRight(buildErrorExpr(typeNameExpr)) { case (dispatcher, elseExpr) =>
                      dispatcher(typeNameExpr, dctx.reader, elseExpr)
                    })
                  }
                  .map(_.build[A])

              for {
                wrapperDispatchFn <- buildDispatchLambda(wrapperDispatchers)
                inlineDispatchFn <- buildDispatchLambda(inlineDispatchers)
                result <-
                  if (allCaseObjects) {
                    val stringEnumDispatchers = allDispatchers.toList.flatMap(_._3)
                    buildDispatchLambda(stringEnumDispatchers).map { stringEnumDispatchFn =>
                      Expr.quote {
                        val config = Expr.splice(dctx.config)
                        val reader = Expr.splice(dctx.reader)
                        if (config.enumAsStrings)
                          UBJsonDerivationUtils.readEnumAsString[A](reader)(
                            Expr.splice(stringEnumDispatchFn)
                          )
                        else
                          config.discriminatorFieldName match {
                            case Some(field) =>
                              UBJsonDerivationUtils.readWithDiscriminator[A](reader, field)(
                                Expr.splice(inlineDispatchFn)
                              )
                            case None =>
                              UBJsonDerivationUtils.readWrapped[A](reader)(Expr.splice(wrapperDispatchFn))
                          }
                      }
                    }
                  } else {
                    MIO.pure {
                      Expr.quote {
                        val config = Expr.splice(dctx.config)
                        val reader = Expr.splice(dctx.reader)
                        config.discriminatorFieldName match {
                          case Some(field) =>
                            UBJsonDerivationUtils.readWithDiscriminator[A](reader, field)(
                              Expr.splice(inlineDispatchFn)
                            )
                          case None =>
                            UBJsonDerivationUtils.readWrapped[A](reader)(Expr.splice(wrapperDispatchFn))
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
    ): MIO[(Expr[String], Expr[UBJsonReader], Expr[A]) => Expr[A]] = {
      implicit val UBJsonReaderT: Type[UBJsonReader] = CTypes.UBJsonReader

      Expr.singletonOf[ChildType] match {
        case Some(singleton) =>
          Log.info(s"Using singleton for $childName") >>
            MIO.pure { (typeNameExpr: Expr[String], _: Expr[UBJsonReader], elseExpr: Expr[A]) =>
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
          deriveDecoderRecursively[ChildType](using dctx.nest[ChildType](dctx.reader)).flatMap { decodedExpr =>
            dctx.getHelper[ChildType].map {
              case Some(helper) =>
                (typeNameExpr: Expr[String], readerExpr: Expr[UBJsonReader], elseExpr: Expr[A]) => {
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
                (typeNameExpr: Expr[String], _: Expr[UBJsonReader], elseExpr: Expr[A]) =>
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

    @scala.annotation.nowarn("msg=is never used|unused explicit parameter")
    private def deriveChildDecoderInline[A: DecoderCtx, ChildType: Type](
        childName: String
    ): MIO[(Expr[String], Expr[UBJsonReader], Expr[A]) => Expr[A]] = {
      implicit val UBJsonReaderT: Type[UBJsonReader] = CTypes.UBJsonReader

      CaseClass.parse[ChildType].toOption match {
        case Some(cc) =>
          DecHandleAsCaseClassRule
            .decodeCaseClassFieldsInline[ChildType](cc)(using dctx.nest[ChildType](dctx.reader))
            .map { inlineExpr => (typeNameExpr: Expr[String], _: Expr[UBJsonReader], elseExpr: Expr[A]) =>
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
          deriveChildDecoder[A, ChildType](childName)
      }
    }

    @scala.annotation.nowarn("msg=is never used|unused explicit parameter")
    private def deriveChildDecoderStringEnum[A: DecoderCtx, ChildType: Type](
        childName: String
    ): MIO[(Expr[String], Expr[UBJsonReader], Expr[A]) => Expr[A]] = {
      implicit val UBJsonReaderT: Type[UBJsonReader] = CTypes.UBJsonReader

      SingletonValue.unapply(Type[ChildType]) match {
        case Some(sv) =>
          Log.info(s"Using singleton for string enum child $childName") >>
            MIO.pure { (typeNameExpr: Expr[String], _: Expr[UBJsonReader], elseExpr: Expr[A]) =>
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
                  MIO.pure { (typeNameExpr: Expr[String], _: Expr[UBJsonReader], elseExpr: Expr[A]) =>
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
              val err = CodecDerivationError.UnexpectedParameterInSingleton(
                Type[ChildType].prettyPrint,
                "Expected singleton/case object for string enum but got"
              )
              Log.error(err.message) >> MIO.fail(err)
          }
      }
    }
  }

  // Types

  private[compiletime] object CTypes {

    def UBJsonValueCodec: Type.Ctor1[UBJsonValueCodec] = Type.Ctor1.of[UBJsonValueCodec]
    def KindlingsUBJsonValueCodec: Type.Ctor1[KindlingsUBJsonValueCodec] =
      Type.Ctor1.of[KindlingsUBJsonValueCodec]
    val CodecLogDerivation: Type[hearth.kindlings.ubjsonderivation.KindlingsUBJsonValueCodec.LogDerivation] =
      Type.of[hearth.kindlings.ubjsonderivation.KindlingsUBJsonValueCodec.LogDerivation]
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
}
