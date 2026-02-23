package hearth.kindlings.jsoniterderivation.internal.compiletime

import hearth.MacroCommons
import hearth.fp.data.NonEmptyList
import hearth.fp.effect.*
import hearth.fp.syntax.*
import hearth.std.*

import hearth.kindlings.jsoniterderivation.{JsoniterConfig, KindlingsJsonValueCodec}
import hearth.kindlings.jsoniterderivation.internal.runtime.JsoniterDerivationUtils
import com.github.plokhotnyuk.jsoniter_scala.core.{JsonReader, JsonValueCodec, JsonWriter}

trait CodecMacrosImpl { this: MacroCommons & StdExtensions =>

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
              Expr.splice(decodeFn(Expr.quote(in), configExpr))
            }
            def encodeValue(x: A, out: JsonWriter): Unit =
              Expr.splice(encodeFn(Expr.quote(x), Expr.quote(out), configExpr))
          }
        }
    }
  }

  // Handles logging, error reporting and prepending "cached" defs and vals to the result.

  def deriveCodecFromCtxAndAdaptForEntrypoint[A: Type, Out: Type](macroName: String)(
      provideCtxAndAdapt: (
          (Expr[A], Expr[JsonWriter], Expr[JsoniterConfig]) => Expr[Unit],
          (Expr[JsonReader], Expr[JsoniterConfig]) => Expr[A],
          Expr[A]
      ) => Expr[Out]
  ): Expr[Out] = Log
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
            _ <- cache.forwardDeclare("codec-encode-body", defBuilder)
            _ <- MIO.scoped { rs =>
              rs(cache.buildCachedWith("codec-encode-body", defBuilder) { case (_, (v, w, c)) =>
                rs(deriveEncoderRecursively[A](using EncoderCtx.from(v, w, c, cache, selfType)))
              })
            }
            fn <- cache.get3Ary[A, JsonWriter, JsoniterConfig, Unit]("codec-encode-body")
          } yield fn.get
        }

        // Decoder: same pattern with ofDef2
        val decMIO: MIO[(Expr[JsonReader], Expr[JsoniterConfig]) => Expr[A]] = {
          val defBuilder =
            ValDefBuilder.ofDef2[JsonReader, JsoniterConfig, A](s"codec_decode_${Type[A].shortName}")
          for {
            _ <- cache.forwardDeclare("codec-decode-body", defBuilder)
            _ <- MIO.scoped { rs =>
              rs(cache.buildCachedWith("codec-decode-body", defBuilder) { case (_, (r, c)) =>
                rs(deriveDecoderRecursively[A](using DecoderCtx.from(r, c, cache, selfType)))
              })
            }
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
      errorRendering = RenderFrom(Log.Level.Info)
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
        _ <- cache.forwardDeclare("cached-encode-method", defBuilder)
        _ <- MIO.scoped { runSafe =>
          runSafe(cache.buildCachedWith("cached-encode-method", defBuilder) { case (_, (value, writer, config)) =>
            runSafe(helper(value, writer, config))
          })
        }
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
          EncUseImplicitWhenAvailableRule,
          EncHandleAsBuiltInRule,
          EncHandleAsValueTypeRule,
          EncHandleAsOptionRule,
          EncHandleAsMapRule,
          EncHandleAsCollectionRule,
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
            Log.info(s"Failed to derive encoder for ${Type[A].prettyPrint}:\n${reasonsStrings.mkString("\n")}") >>
              MIO.fail(CodecDerivationError.UnsupportedType(Type[A].prettyPrint, reasonsStrings))
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
      if (!(Key <:< Type[String]))
        MIO.pure(Rule.yielded(s"Map key type ${Key.prettyPrint} is not String"))
      else {
        LambdaBuilder
          .of1[Value]("mapValue")
          .traverse { valueExpr =>
            deriveEncoderRecursively[Value](using ectx.nest(valueExpr))
          }
          .map { builder =>
            val valueLambda = builder.build[Unit]
            val iterableExpr = isMap.asIterable(ectx.value)
            Rule.matched(Expr.quote {
              JsoniterDerivationUtils.writeMapStringKeyed[Value](
                Expr.splice(ectx.writer),
                Expr.splice(iterableExpr).asInstanceOf[Iterable[(String, Value)]],
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

  object EncHandleAsCaseClassRule extends EncoderDerivationRule("handle as case class when possible") {

    def apply[A: EncoderCtx]: MIO[Rule.Applicability[Expr[Unit]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as a case class") >> {
        CaseClass.parse[A] match {
          case Some(caseClass) =>
            for {
              _ <- ectx.setHelper[A] { (value, writer, config) =>
                encodeCaseClassFields[A](caseClass)(using ectx.nestInCache(value, writer, config))
              }
              result <- ectx.getHelper[A].flatMap {
                case Some(helperCall) => MIO.pure(Rule.matched(helperCall(ectx.value, ectx.writer, ectx.config)))
                case None             => MIO.pure(Rule.yielded(s"Failed to build helper for ${Type[A].prettyPrint}"))
              }
            } yield result

          case None =>
            MIO.pure(Rule.yielded(s"The type ${Type[A].prettyPrint} is not a case class"))
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

      val fields = caseClass.caseFieldValuesAt(ectx.value).toList

      NonEmptyList.fromList(fields) match {
        case Some(nonEmptyFields) =>
          nonEmptyFields
            .parTraverse { case (fieldName, fieldValue) =>
              import fieldValue.{Underlying as Field, value as fieldExpr}
              Log.namedScope(s"Encoding field ${ectx.value.prettyPrint}.$fieldName: ${Type[Field].prettyPrint}") {
                deriveEncoderRecursively[Field](using ectx.nest(fieldExpr)).map { fieldEnc =>
                  (fieldName, fieldEnc)
                }
              }
            }
            .map { fieldPairs =>
              fieldPairs.toList
                .map { case (fieldName, fieldEnc) =>
                  Expr.quote {
                    Expr
                      .splice(ectx.writer)
                      .writeKey(Expr.splice(ectx.config).fieldNameMapper(Expr.splice(Expr(fieldName))))
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
    }

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
        Enum.parse[A] match {
          case Some(enumm) =>
            for {
              _ <- ectx.setHelper[A] { (value, writer, config) =>
                encodeEnumCases[A](enumm)(using ectx.nestInCache(value, writer, config))
              }
              result <- ectx.getHelper[A].flatMap {
                case Some(helperCall) => MIO.pure(Rule.matched(helperCall(ectx.value, ectx.writer, ectx.config)))
                case None             => MIO.pure(Rule.yielded(s"Failed to build helper for ${Type[A].prettyPrint}"))
              }
            } yield result
          case None =>
            MIO.pure(Rule.yielded(s"The type ${Type[A].prettyPrint} is not an enum"))
        }
      }

    @scala.annotation.nowarn("msg=is never used")
    private def encodeEnumCases[A: EncoderCtx](
        enumm: Enum[A]
    ): MIO[Expr[Unit]] = {
      implicit val UnitT: Type[Unit] = CTypes.Unit
      implicit val JsonWriterT: Type[JsonWriter] = CTypes.JsonWriter
      implicit val StringT: Type[String] = CTypes.String

      enumm
        .parMatchOn[MIO, Unit](ectx.value) { matched =>
          import matched.{value as enumCaseValue, Underlying as EnumCase}
          Log.namedScope(s"Encoding enum case ${enumCaseValue.prettyPrint}: ${EnumCase.prettyPrint}") {
            val caseName = Type[EnumCase].shortName

            // For discriminator mode, we need fields-only encoding to avoid double wrapping.
            // Parse child as case class to get field-level access.
            val fieldsOnlyMIO: MIO[Expr[Unit]] = CaseClass.parse[EnumCase] match {
              case Some(caseClass) =>
                EncHandleAsCaseClassRule.encodeCaseClassFieldsOnly[EnumCase](caseClass)(using ectx.nest(enumCaseValue))
              case None =>
                // Not a case class (e.g. case object) — no fields
                MIO.pure(Expr.quote(()): Expr[Unit])
            }

            // Also derive the full encoding for wrapper mode
            val fullEncMIO: MIO[Expr[Unit]] =
              deriveEncoderRecursively[EnumCase](using ectx.nest(enumCaseValue))

            for {
              fieldsOnly <- fieldsOnlyMIO
              fullEnc <- fullEncMIO
            } yield Expr.quote {
              val name = Expr.splice(ectx.config).adtLeafClassNameMapper(Expr.splice(Expr(caseName)))
              Expr.splice(ectx.config).discriminatorFieldName match {
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
        .flatMap {
          case Some(result) => MIO.pure(result)
          case None         =>
            MIO.fail(new RuntimeException(s"The type ${Type[A].prettyPrint} does not have any children!"))
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
        _ <- cache.forwardDeclare("cached-decode-method", defBuilder)
        _ <- MIO.scoped { runSafe =>
          runSafe(cache.buildCachedWith("cached-decode-method", defBuilder) { case (_, (reader, config)) =>
            runSafe(helper(reader, config))
          })
        }
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
          DecUseImplicitWhenAvailableRule,
          DecHandleAsBuiltInRule,
          DecHandleAsValueTypeRule,
          DecHandleAsOptionRule,
          DecHandleAsMapRule,
          DecHandleAsCollectionRule,
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
            Log.info(s"Failed to derive decoder for ${Type[A].prettyPrint}:\n${reasonsStrings.mkString("\n")}") >>
              MIO.fail(CodecDerivationError.UnsupportedType(Type[A].prettyPrint, reasonsStrings))
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
            Some(Expr.quote(Expr.splice(reader).readBigDecimal(null).asInstanceOf[A]))
          else if (Type[A] =:= Type.of[BigInt])
            Some(Expr.quote(Expr.splice(reader).readBigInt(null).asInstanceOf[A]))
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

      if (!(Key <:< Type[String]))
        MIO.pure(Rule.yielded(s"Map key type ${Key.prettyPrint} is not String"))
      else {
        LambdaBuilder
          .of1[JsonReader]("valueReader")
          .traverse { valueReaderExpr =>
            deriveDecoderRecursively[Value](using dctx.nest[Value](valueReaderExpr))
          }
          .map { builder =>
            val decodeFn = builder.build[Value]
            val factoryExpr = isMap.factory
            Rule.matched(Expr.quote {
              JsoniterDerivationUtils
                .readMap[Value, A](
                  Expr.splice(dctx.reader),
                  Expr.splice(decodeFn),
                  Expr.splice(factoryExpr).asInstanceOf[scala.collection.Factory[(String, Value), A]]
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
                      Expr.splice(factoryExpr).asInstanceOf[scala.collection.Factory[Item, A]]
                    )
                    .asInstanceOf[A]
                })
              }

          case _ =>
            MIO.pure(Rule.yielded(s"The type ${Type[A].prettyPrint} is not a collection"))
        }
      }
  }

  object DecHandleAsCaseClassRule extends DecoderDerivationRule("handle as case class when possible") {

    def apply[A: DecoderCtx]: MIO[Rule.Applicability[Expr[A]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as a case class") >> {
        CaseClass.parse[A] match {
          case Some(caseClass) =>
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

          case None =>
            MIO.pure(Rule.yielded(s"The type ${Type[A].prettyPrint} is not a case class"))
        }
      }

    @scala.annotation.nowarn("msg=is never used|unused explicit parameter")
    private def decodeCaseClassFields[A: DecoderCtx](
        caseClass: CaseClass[A]
    ): MIO[Expr[A]] = {
      implicit val StringT: Type[String] = CTypes.String
      implicit val JsonReaderT: Type[JsonReader] = CTypes.JsonReader

      val constructor = caseClass.primaryConstructor
      val fieldsList = constructor.parameters.flatten.toList

      NonEmptyList.fromList(fieldsList) match {
        case None =>
          // Zero-parameter case class: construct directly
          caseClass
            .construct[MIO](new CaseClass.ConstructField[MIO] {
              def apply(field: Parameter): MIO[Expr[field.tpe.Underlying]] =
                MIO.fail(
                  new RuntimeException(s"Unexpected parameter in zero-argument case class ${Type[A].prettyPrint}")
                )
            })
            .flatMap {
              case Some(expr) =>
                // Still need to read the empty object from the reader
                MIO.pure(Expr.quote {
                  JsoniterDerivationUtils.readEmptyObject(Expr.splice(dctx.reader))
                  Expr.splice(expr)
                })
              case None =>
                MIO.fail(new RuntimeException(s"Cannot construct ${Type[A].prettyPrint}"))
            }

        case Some(fields) =>
          implicit val AnyT: Type[Any] = CTypes.Any
          implicit val ArrayAnyT: Type[Array[Any]] = CTypes.ArrayAny

          // Step 1: For each field, derive a decoder (implicit or recursive) and build
          // decode + accessor expressions. Uses unsafeCast with the decoder as type witness
          // to avoid path-dependent type aliases in Expr.quote (Scala 2 compatibility).
          fields
            .parTraverse { case (fieldName, param) =>
              import param.tpe.Underlying as Field
              Log.namedScope(s"Deriving decoder for field $fieldName: ${Type[Field].prettyPrint}") {
                deriveFieldDecoder[Field].map { decodeFn =>
                  // Build an erased version of the decode function for the dispatch
                  val decodeFnErased: Expr[JsonReader => Any] = Expr.quote { (r: JsonReader) =>
                    Expr.splice(decodeFn).apply(r).asInstanceOf[Any]
                  }
                  val makeAccessor: Expr[Array[Any]] => (String, Expr_??) = { arrExpr =>
                    val typedExpr = Expr.quote {
                      JsoniterDerivationUtils.unsafeCast(
                        Expr.splice(arrExpr)(Expr.splice(Expr(param.index))),
                        Expr.splice(decodeFn)
                      )
                    }
                    (fieldName, typedExpr.as_??)
                  }
                  (fieldName, param.index, decodeFnErased, makeAccessor)
                }
              }
            }
            .flatMap { fieldData =>
              val fieldDataList = fieldData.toList

              // Step 2: Build the constructor lambda using LambdaBuilder + primaryConstructor
              LambdaBuilder
                .of1[Array[Any]]("decodedValues")
                .traverse { decodedValuesExpr =>
                  val fieldMap: Map[String, Expr_??] =
                    fieldDataList.map(_._4(decodedValuesExpr)).toMap
                  caseClass.primaryConstructor(fieldMap) match {
                    case Right(constructExpr) => MIO.pure(constructExpr)
                    case Left(error)          =>
                      MIO.fail(new RuntimeException(s"Cannot construct ${Type[A].prettyPrint}: $error"))
                  }
                }
                .map { builder =>
                  val constructLambda = builder.build[A]

                  // Step 3: Build the field dispatch - if-else chain matching mapped field names.
                  // Uses the erased decode functions (JsonReader => Any) to avoid path-dependent types.
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

    /** Decode case class fields from an already-opened JSON object (for discriminator mode). The object's `{` and
      * discriminator key-value have already been read. Returns Expr[A] that reads remaining fields via
      * readObjectInline.
      */
    @scala.annotation.nowarn("msg=is never used|unused explicit parameter")
    private[compiletime] def decodeCaseClassFieldsInline[A: DecoderCtx](
        caseClass: CaseClass[A]
    ): MIO[Expr[A]] = {
      implicit val StringT: Type[String] = CTypes.String
      implicit val JsonReaderT: Type[JsonReader] = CTypes.JsonReader

      val constructor = caseClass.primaryConstructor
      val fieldsList = constructor.parameters.flatten.toList

      NonEmptyList.fromList(fieldsList) match {
        case None =>
          // Zero-parameter case class: just read closing `}`
          caseClass
            .construct[MIO](new CaseClass.ConstructField[MIO] {
              def apply(field: Parameter): MIO[Expr[field.tpe.Underlying]] =
                MIO.fail(new RuntimeException(s"Unexpected field in zero-arg case class"))
            })
            .flatMap {
              case Some(expr) =>
                // Read just the `}` — object was already opened, discriminator already consumed
                MIO.pure(Expr.quote {
                  // After discriminator, if next token is `}`, there are no more fields
                  // If next token is `,`, there are unexpected extra fields — just skip to `}`
                  val reader = Expr.splice(dctx.reader)
                  if (!reader.isNextToken('}'.toByte)) {
                    if (reader.isCurrentToken(','.toByte)) {
                      // skip remaining fields
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
                MIO.fail(new RuntimeException(s"Cannot construct ${Type[A].prettyPrint}"))
            }

        case Some(fields) =>
          implicit val AnyT: Type[Any] = CTypes.Any
          implicit val ArrayAnyT: Type[Array[Any]] = CTypes.ArrayAny

          fields
            .parTraverse { case (fieldName, param) =>
              import param.tpe.Underlying as Field
              Log.namedScope(s"Deriving decoder for field $fieldName: ${Type[Field].prettyPrint}") {
                deriveFieldDecoder[Field].map { decodeFn =>
                  val decodeFnErased: Expr[JsonReader => Any] = Expr.quote { (r: JsonReader) =>
                    Expr.splice(decodeFn).apply(r).asInstanceOf[Any]
                  }
                  val makeAccessor: Expr[Array[Any]] => (String, Expr_??) = { arrExpr =>
                    val typedExpr = Expr.quote {
                      JsoniterDerivationUtils.unsafeCast(
                        Expr.splice(arrExpr)(Expr.splice(Expr(param.index))),
                        Expr.splice(decodeFn)
                      )
                    }
                    (fieldName, typedExpr.as_??)
                  }
                  (fieldName, param.index, decodeFnErased, makeAccessor)
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
                  caseClass.primaryConstructor(fieldMap) match {
                    case Right(constructExpr) => MIO.pure(constructExpr)
                    case Left(error)          =>
                      MIO.fail(new RuntimeException(s"Cannot construct ${Type[A].prettyPrint}: $error"))
                  }
                }
                .map { builder =>
                  val constructLambda = builder.build[A]

                  val fieldMappings = fieldDataList.map { case (name, index, decodeFnErased, _) =>
                    (name, index, decodeFnErased)
                  }

                  Expr.quote {
                    JsoniterDerivationUtils.readObjectInline[A](
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
  }

  object DecHandleAsEnumRule extends DecoderDerivationRule("handle as enum when possible") {

    def apply[A: DecoderCtx]: MIO[Rule.Applicability[Expr[A]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as an enum") >> {
        Enum.parse[A] match {
          case Some(enumm) =>
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
          case None =>
            MIO.pure(Rule.yielded(s"The type ${Type[A].prettyPrint} is not an enum"))
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

          // For each child, derive BOTH wrapper-mode and inline (discriminator-mode) decoders
          children
            .parTraverse { case (childName, child) =>
              import child.Underlying as ChildType
              Log.namedScope(s"Deriving decoder for enum case $childName: ${Type[ChildType].prettyPrint}") {
                for {
                  wrapper <- deriveChildDecoder[A, ChildType](childName)
                  inline <- deriveChildDecoderInline[A, ChildType](childName)
                } yield (wrapper, inline)
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

              for {
                wrapperDispatchFn <- buildDispatchLambda(wrapperDispatchers)
                inlineDispatchFn <- buildDispatchLambda(inlineDispatchers)
              } yield Expr.quote {
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
          // No implicit - derive via full rules chain
          deriveDecoderRecursively[ChildType](using dctx.nest[ChildType](dctx.reader)).flatMap { _ =>
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
                (typeNameExpr: Expr[String], readerExpr: Expr[JsonReader], elseExpr: Expr[A]) => elseExpr
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

      CaseClass.parse[ChildType] match {
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
  }

  // Types

  private[compiletime] object CTypes {

    def JsonValueCodec: Type.Ctor1[JsonValueCodec] = Type.Ctor1.of[JsonValueCodec]
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
}
