package hearth.kindlings.circederivation.internal.compiletime

import hearth.MacroCommons
import hearth.fp.data.NonEmptyList
import hearth.fp.effect.*
import hearth.fp.syntax.*
import hearth.std.*

import hearth.kindlings.circederivation.{Configuration, KindlingsEncoder, KindlingsEncoderAsObject}
import hearth.kindlings.circederivation.annotations.{fieldName, transientField}
import hearth.kindlings.circederivation.internal.runtime.CirceDerivationUtils
import io.circe.{Encoder, Json, JsonObject, KeyEncoder}

trait EncoderMacrosImpl { this: MacroCommons & StdExtensions & AnnotationSupport =>

  // Entrypoints

  def deriveInlineEncode[A: Type](valueExpr: Expr[A], configExpr: Expr[Configuration]): Expr[Json] = {
    implicit val JsonT: Type[Json] = Types.Json
    implicit val ConfigT: Type[Configuration] = Types.Configuration

    deriveEncoderFromCtxAndAdaptForEntrypoint[A, Json]("KindlingsEncoder.encode") { fromCtx =>
      ValDefs.createVal[A](valueExpr).use { valueVal =>
        ValDefs.createVal[Configuration](configExpr).use { configVal =>
          Expr.quote {
            val _ = Expr.splice(valueVal)
            val _ = Expr.splice(configVal)
            Expr.splice(fromCtx(EncoderCtx.from(valueVal, configVal, derivedType = None)))
          }
        }
      }
    }
  }

  @scala.annotation.nowarn("msg=is never used")
  def deriveEncoderTypeClass[A: Type](configExpr: Expr[Configuration]): Expr[KindlingsEncoder[A]] = {
    implicit val EncoderA: Type[Encoder[A]] = Types.Encoder[A]
    implicit val KindlingsEncoderA: Type[KindlingsEncoder[A]] = Types.KindlingsEncoder[A]
    implicit val JsonT: Type[Json] = Types.Json
    implicit val ConfigT: Type[Configuration] = Types.Configuration
    val selfType: Option[??] = Some(Type[A].as_??)

    deriveEncoderFromCtxAndAdaptForEntrypoint[A, KindlingsEncoder[A]]("KindlingsEncoder.derived") { fromCtx =>
      ValDefs.createVal[Configuration](configExpr).use { configVal =>
        Expr.quote {
          val cfg = Expr.splice(configVal)
          new KindlingsEncoder[A] {
            def apply(a: A): Json = {
              val _ = a
              Expr.splice {
                fromCtx(EncoderCtx.from(Expr.quote(a), Expr.quote(cfg), derivedType = selfType))
              }
            }
          }
        }
      }
    }
  }

  @scala.annotation.nowarn("msg=is never used")
  def deriveEncoderAsObjectTypeClass[A: Type](configExpr: Expr[Configuration]): Expr[KindlingsEncoderAsObject[A]] = {
    // Compile-time validation: only case classes, named tuples, sealed traits produce objects.
    // Note: value types (e.g., case class Foo(x: Int) extends AnyVal) ARE case classes and pass this check,
    // but produce non-object JSON at runtime — the runtime guard in encodeObject handles this.
    val isCaseClass = CaseClass.parse[A].toOption.isDefined
    val isNamedTuple = NamedTuple.parse[A].toOption.isDefined
    val isEnum = Enum.parse[A].toOption.isDefined
    if (!isCaseClass && !isNamedTuple && !isEnum)
      Environment.reportErrorAndAbort(
        s"KindlingsEncoder.deriveAsObject: ${Type[A].prettyPrint} is not a case class, sealed trait, or named tuple. " +
          "Use KindlingsEncoder.derive instead."
      )

    implicit val KindlingsEncoderAsObjectA: Type[KindlingsEncoderAsObject[A]] = Types.KindlingsEncoderAsObject[A]
    implicit val JsonObjectT: Type[JsonObject] = Types.JsonObject
    implicit val JsonT: Type[Json] = Types.Json
    implicit val ConfigT: Type[Configuration] = Types.Configuration
    val selfType: Option[??] = Some(Type[A].as_??)

    deriveEncoderFromCtxAndAdaptForEntrypoint[A, KindlingsEncoderAsObject[A]]("KindlingsEncoder.deriveAsObject") {
      fromCtx =>
        ValDefs.createVal[Configuration](configExpr).use { configVal =>
          Expr.quote {
            val cfg = Expr.splice(configVal)
            new KindlingsEncoderAsObject[A] {
              def encodeObject(a: A): JsonObject = {
                val _ = a
                val json: Json = Expr.splice {
                  fromCtx(EncoderCtx.from(Expr.quote(a), Expr.quote(cfg), derivedType = selfType))
                }
                json.asObject match {
                  case Some(obj) => obj
                  case None      =>
                    throw new IllegalStateException(
                      "Encoder.AsObject: produced non-object JSON. This can happen when using enumAsStrings=true " +
                        "with a sealed trait of case objects. Use KindlingsEncoder.derive instead."
                    )
                }
              }
            }
          }
        }
    }
  }

  // Handles logging, error reporting and prepending "cached" defs and vals to the result.

  def deriveEncoderFromCtxAndAdaptForEntrypoint[A: Type, Out: Type](macroName: String)(
      provideCtxAndAdapt: (EncoderCtx[A] => Expr[Json]) => Expr[Out]
  ): Expr[Out] = {
    if (Type[A] =:= Type.of[Nothing].asInstanceOf[Type[A]] || Type[A] =:= Type.of[Any].asInstanceOf[Type[A]])
      Environment.reportErrorAndAbort(
        s"$macroName: type parameter was inferred as ${Type[A].prettyPrint}, which is likely unintended.\n" +
          s"Provide an explicit type parameter, e.g.: $macroName[MyType](...)\n" +
          "or add a type ascription to the result variable."
      )
    Log
      .namedScope(
        s"Deriving encoder for ${Type[A].prettyPrint} at: ${Environment.currentPosition.prettyPrint}"
      ) {
        MIO.scoped { runSafe =>
          val fromCtx: (EncoderCtx[A] => Expr[Json]) = (ctx: EncoderCtx[A]) =>
            runSafe {
              for {
                _ <- Environment.loadStandardExtensions().toMIO(allowFailures = false)
                result <- deriveEncoderRecursively[A](using ctx)
                cache <- ctx.cache.get
              } yield cache.toValDefs.use(_ => result)
            }

          provideCtxAndAdapt(fromCtx)
        }
      }
      .flatTap { result =>
        Log.info(s"Derived final encoder result: ${result.prettyPrint}")
      }
      .runToExprOrFail(
        macroName,
        infoRendering = if (shouldWeLogEncoderDerivation) RenderFrom(Log.Level.Info) else DontRender,
        errorRendering = if (shouldWeLogEncoderDerivation) RenderFrom(Log.Level.Info) else DontRender
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
          "Enable debug logging with: import hearth.kindlings.circederivation.debug.logDerivationForKindlingsEncoder or scalac option -Xmacro-settings:circeDerivation.logDerivation=true"
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

  def shouldWeLogEncoderDerivation: Boolean = {
    implicit val LogDerivation: Type[KindlingsEncoder.LogDerivation] = Types.EncoderLogDerivation
    def logDerivationImported = Expr.summonImplicit[KindlingsEncoder.LogDerivation].isDefined

    def logDerivationSetGlobally = (for {
      data <- Environment.typedSettings.toOption
      circeDerivation <- data.get("circeDerivation")
      shouldLog <- circeDerivation.get("logDerivation").flatMap(_.asBoolean)
    } yield shouldLog).getOrElse(false)

    logDerivationImported || logDerivationSetGlobally
  }

  // Context

  final case class EncoderCtx[A](
      tpe: Type[A],
      value: Expr[A],
      config: Expr[Configuration],
      cache: MLocal[ValDefsCache],
      derivedType: Option[??]
  ) {

    def nest[B: Type](newValue: Expr[B]): EncoderCtx[B] = copy[B](
      tpe = Type[B],
      value = newValue
    )

    def nestInCache(
        newValue: Expr[A],
        newConfig: Expr[Configuration]
    ): EncoderCtx[A] = copy(
      value = newValue,
      config = newConfig
    )

    def getInstance[B: Type]: MIO[Option[Expr[Encoder[B]]]] = {
      implicit val EncoderB: Type[Encoder[B]] = Types.Encoder[B]
      cache.get0Ary[Encoder[B]]("cached-encoder-instance")
    }
    def setInstance[B: Type](instance: Expr[Encoder[B]]): MIO[Unit] = {
      implicit val EncoderB: Type[Encoder[B]] = Types.Encoder[B]
      Log.info(s"Caching Encoder instance for ${Type[B].prettyPrint}") >>
        cache.buildCachedWith(
          "cached-encoder-instance",
          ValDefBuilder.ofLazy[Encoder[B]](s"encoder_${Type[B].shortName}")
        )(_ => instance)
    }

    def getHelper[B: Type]: MIO[Option[(Expr[B], Expr[Configuration]) => Expr[Json]]] = {
      implicit val JsonT: Type[Json] = Types.Json
      implicit val ConfigT: Type[Configuration] = Types.Configuration
      cache.get2Ary[B, Configuration, Json]("cached-encode-method")
    }
    def setHelper[B: Type](
        helper: (Expr[B], Expr[Configuration]) => MIO[Expr[Json]]
    ): MIO[Unit] = {
      implicit val JsonT: Type[Json] = Types.Json
      implicit val ConfigT: Type[Configuration] = Types.Configuration
      val defBuilder =
        ValDefBuilder.ofDef2[B, Configuration, Json](s"encode_${Type[B].shortName}")
      for {
        _ <- Log.info(s"Forward-declaring encode helper for ${Type[B].prettyPrint}")
        _ <- cache.forwardDeclare("cached-encode-method", defBuilder)
        _ <- MIO.scoped { runSafe =>
          runSafe(cache.buildCachedWith("cached-encode-method", defBuilder) { case (_, (value, config)) =>
            runSafe(helper(value, config))
          })
        }
        _ <- Log.info(s"Defined encode helper for ${Type[B].prettyPrint}")
      } yield ()
    }

    override def toString: String =
      s"encode[${tpe.prettyPrint}](value = ${value.prettyPrint}, config = ${config.prettyPrint})"
  }
  object EncoderCtx {

    def from[A: Type](
        value: Expr[A],
        config: Expr[Configuration],
        derivedType: Option[??]
    ): EncoderCtx[A] = EncoderCtx(
      tpe = Type[A],
      value = value,
      config = config,
      cache = ValDefsCache.mlocal,
      derivedType = derivedType
    )
  }

  def ectx[A](implicit A: EncoderCtx[A]): EncoderCtx[A] = A

  implicit def currentEncoderValueType[A: EncoderCtx]: Type[A] = ectx.tpe

  abstract class EncoderDerivationRule(val name: String) extends Rule {
    def apply[A: EncoderCtx]: MIO[Rule.Applicability[Expr[Json]]]
  }

  // The actual derivation logic

  def deriveEncoderRecursively[A: EncoderCtx]: MIO[Expr[Json]] =
    Log
      .namedScope(s"Deriving encoder for type ${Type[A].prettyPrint}") {
        Rules(
          EncUseCachedDefWhenAvailableRule,
          EncHandleAsLiteralTypeRule,
          EncUseImplicitWhenAvailableRule,
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
            val err = EncoderDerivationError.UnsupportedType(Type[A].prettyPrint, reasonsStrings)
            Log.error(err.message) >> MIO.fail(err)
        }
      }

  // Rules

  object EncUseCachedDefWhenAvailableRule extends EncoderDerivationRule("use cached def when available") {

    def apply[A: EncoderCtx]: MIO[Rule.Applicability[Expr[Json]]] =
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
        instance: Expr[Encoder[A]]
    ): MIO[Rule.Applicability[Expr[Json]]] =
      Log.info(s"Found cached encoder instance for ${Type[A].prettyPrint}") >> MIO.pure(Rule.matched(Expr.quote {
        Expr.splice(instance).apply(Expr.splice(ectx.value))
      }))

    private def callCachedHelper[A: EncoderCtx](
        helperCall: (Expr[A], Expr[Configuration]) => Expr[Json]
    ): MIO[Rule.Applicability[Expr[Json]]] =
      Log.info(s"Found cached encoder helper for ${Type[A].prettyPrint}") >> MIO.pure(
        Rule.matched(helperCall(ectx.value, ectx.config))
      )

    private def yieldUnsupported[A: EncoderCtx]: MIO[Rule.Applicability[Expr[Json]]] =
      MIO.pure(Rule.yielded(s"The type ${Type[A].prettyPrint} does not have a cached encoder"))
  }

  object EncUseImplicitWhenAvailableRule extends EncoderDerivationRule("use implicit when available") {

    lazy val ignoredImplicits: Seq[UntypedMethod] = {
      val ours = Type.of[KindlingsEncoder.type].methods.collect {
        case method if method.value.name == "derived" => method.value.asUntyped
      }
      val circeEncoder = Type.of[Encoder.type].methods.collect {
        case method if method.value.name == "derived" || method.value.name.startsWith("encodeLiteral") =>
          method.value.asUntyped
      }
      ours ++ circeEncoder
    }

    def apply[A: EncoderCtx]: MIO[Rule.Applicability[Expr[Json]]] =
      Log.info(s"Attempting to use implicit Encoder for ${Type[A].prettyPrint}") >> {
        // Skip implicit search for the self type being derived to prevent self-referential loops
        // (e.g., `implicit val enc: Encoder[X] = KindlingsEncoder.derived[X]` would otherwise
        // find `enc` itself during macro expansion, generating code that calls itself infinitely).
        if (ectx.derivedType.exists(_.Underlying =:= Type[A]))
          MIO.pure(
            Rule.yielded(s"The type ${Type[A].prettyPrint} is the type being derived, skipping implicit search")
          )
        else
          Types.Encoder[A].summonExprIgnoring(ignoredImplicits*).toEither match {
            case Right(instanceExpr) => cacheAndUse[A](instanceExpr)
            case Left(reason)        => yieldUnsupported[A](reason)
          }
      }

    private def cacheAndUse[A: EncoderCtx](
        instanceExpr: Expr[Encoder[A]]
    ): MIO[Rule.Applicability[Expr[Json]]] =
      Log.info(s"Found implicit encoder ${instanceExpr.prettyPrint}, using directly") >>
        MIO.pure(Rule.matched(Expr.quote {
          Expr.splice(instanceExpr).apply(Expr.splice(ectx.value))
        }))

    private def yieldUnsupported[A: EncoderCtx](reason: String): MIO[Rule.Applicability[Expr[Json]]] =
      MIO.pure(
        Rule.yielded(
          s"The type ${Type[A].prettyPrint} does not have an implicit Encoder instance: $reason"
        )
      )
  }

  object EncHandleAsLiteralTypeRule extends EncoderDerivationRule("handle as literal type when possible") {

    def apply[A: EncoderCtx]: MIO[Rule.Applicability[Expr[Json]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as a literal type") >> {
        extractLiteralJson[A] match {
          case Some(jsonExpr) => MIO.pure(Rule.matched(jsonExpr))
          case None           => MIO.pure(Rule.yielded(s"The type ${Type[A].prettyPrint} is not a literal type"))
        }
      }

    private def extractLiteralJson[A: EncoderCtx]: Option[Expr[Json]] = {
      implicit val JsonT: Type[Json] = Types.Json

      Type.StringCodec.fromType(Type[A]).map { e =>
        val v: String = e.value; Expr.quote(Json.fromString(Expr.splice(Expr(v))))
      } orElse Type.IntCodec.fromType(Type[A]).map { e =>
        val v: Int = e.value; Expr.quote(Json.fromInt(Expr.splice(Expr(v))))
      } orElse Type.LongCodec.fromType(Type[A]).map { e =>
        val v: Long = e.value; Expr.quote(Json.fromLong(Expr.splice(Expr(v))))
      } orElse Type.DoubleCodec.fromType(Type[A]).map { e =>
        val v: Double = e.value; Expr.quote(Json.fromDoubleOrNull(Expr.splice(Expr(v))))
      } orElse Type.FloatCodec.fromType(Type[A]).map { e =>
        val v: Float = e.value; Expr.quote(Json.fromFloatOrNull(Expr.splice(Expr(v))))
      } orElse Type.BooleanCodec.fromType(Type[A]).map { e =>
        val v: Boolean = e.value; Expr.quote(Json.fromBoolean(Expr.splice(Expr(v))))
      } orElse Type.ShortCodec.fromType(Type[A]).map { e =>
        val v: Short = e.value; Expr.quote(Json.fromInt(Expr.splice(Expr(v)).toInt))
      } orElse Type.ByteCodec.fromType(Type[A]).map { e =>
        val v: Byte = e.value; Expr.quote(Json.fromInt(Expr.splice(Expr(v)).toInt))
      } orElse Type.CharCodec.fromType(Type[A]).map { e =>
        val v: Char = e.value; Expr.quote(Json.fromString(Expr.splice(Expr(v)).toString))
      }
    }
  }

  object EncHandleAsValueTypeRule extends EncoderDerivationRule("handle as value type when possible") {

    def apply[A: EncoderCtx]: MIO[Rule.Applicability[Expr[Json]]] =
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
    implicit val JsonT: Type[Json] = Types.Json

    def apply[A: EncoderCtx]: MIO[Rule.Applicability[Expr[Json]]] =
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
                val lambda = builder.build[Json]
                Rule.matched(
                  isOption.value.fold[Json](ectx.value)(
                    onEmpty = Expr.quote(Json.Null),
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
    implicit val JsonT: Type[Json] = Types.Json
    implicit val StringT: Type[String] = Types.String
    implicit val StringJsonPairT: Type[(String, Json)] = Type.of[(String, Json)]

    def apply[A: EncoderCtx]: MIO[Rule.Applicability[Expr[Json]]] =
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
    ): MIO[Rule.Applicability[Expr[Json]]] = {
      import isMap.{Key, Value}
      if (Key <:< Type[String]) {
        // String keys — use existing fast path
        LambdaBuilder
          .of1[Pair]("pair")
          .traverse { pairExpr =>
            val keyExpr = isMap.key(pairExpr)
            val valueExpr = isMap.value(pairExpr)
            deriveEncoderRecursively[Value](using ectx.nest(valueExpr)).map { valueJson =>
              Expr.quote {
                (Expr.splice(keyExpr).asInstanceOf[String], Expr.splice(valueJson))
              }
            }
          }
          .map { builder =>
            val pairLambda = builder.build[(String, Json)]
            val iterableExpr = isMap.asIterable(ectx.value)
            Rule.matched(Expr.quote {
              CirceDerivationUtils.jsonFromMappedPairs[Pair](
                Expr.splice(iterableExpr),
                Expr.splice(pairLambda)
              )
            })
          }
      } else {
        // Non-String keys — try to derive a key-to-String function
        deriveKeyEncoder[Key].flatMap {
          case Some(keyEncoderLambda) =>
            LambdaBuilder
              .of1[Pair]("pair")
              .traverse { pairExpr =>
                val keyExpr = isMap.key(pairExpr)
                val valueExpr = isMap.value(pairExpr)
                deriveEncoderRecursively[Value](using ectx.nest(valueExpr)).map { valueJson =>
                  Expr.quote {
                    (Expr.splice(keyEncoderLambda).apply(Expr.splice(keyExpr)), Expr.splice(valueJson))
                  }
                }
              }
              .map { builder =>
                val pairLambda = builder.build[(String, Json)]
                val iterableExpr = isMap.asIterable(ectx.value)
                Rule.matched(Expr.quote {
                  CirceDerivationUtils.jsonFromMappedPairs[Pair](
                    Expr.splice(iterableExpr),
                    Expr.splice(pairLambda)
                  )
                })
              }
          case None =>
            MIO.pure(Rule.yielded(s"Map key type ${Key.prettyPrint} is not String and no key encoder could be derived"))
        }
      }
    }

    /** Try to derive a K => String function for map keys. Returns None if derivation fails. */
    @scala.annotation.nowarn("msg=is never used")
    private def deriveKeyEncoder[K: Type](implicit ctx: EncoderCtx[?]): MIO[Option[Expr[K => String]]] =
      Log.info(s"Attempting to derive key encoder for ${Type[K].prettyPrint}") >> {
        // 1. Built-in types — inline key.toString (same as circe's KeyEncoder[Int] etc.)
        val builtIn: Option[MIO[Option[Expr[K => String]]]] = {
          def makeToStringKeyEncoder: MIO[Option[Expr[K => String]]] =
            LambdaBuilder
              .of1[K]("key")
              .traverse { keyExpr =>
                MIO.pure(Expr.quote(Expr.splice(keyExpr).toString))
              }
              .map(builder => Some(builder.build[String]): Option[Expr[K => String]])

          if (Type[K] =:= Type.of[Int]) Some(makeToStringKeyEncoder)
          else if (Type[K] =:= Type.of[Long]) Some(makeToStringKeyEncoder)
          else if (Type[K] =:= Type.of[Double]) Some(makeToStringKeyEncoder)
          else if (Type[K] =:= Type.of[Short]) Some(makeToStringKeyEncoder)
          else if (Type[K] =:= Type.of[Byte]) Some(makeToStringKeyEncoder)
          else None
        }

        builtIn.getOrElse {
          // 2. Try summoning user-provided KeyEncoder[K]
          Types.KeyEncoder[K].summonExprIgnoring().toEither match {
            case Right(keyEncoderExpr) =>
              Log.info(s"Found implicit KeyEncoder[${Type[K].prettyPrint}]") >>
                LambdaBuilder
                  .of1[K]("key")
                  .traverse { keyExpr =>
                    MIO.pure(Expr.quote {
                      Expr.splice(keyEncoderExpr).apply(Expr.splice(keyExpr))
                    })
                  }
                  .map { builder =>
                    Some(builder.build[String]): Option[Expr[K => String]]
                  }
            case Left(_) =>
              // 3. Try value type — unwrap to inner, recurse
              Type[K] match {
                case IsValueType(isValueType) =>
                  import isValueType.Underlying as Inner
                  deriveKeyEncoder[Inner].flatMap {
                    case Some(innerKeyEncoder) =>
                      LambdaBuilder
                        .of1[K]("key")
                        .traverse { keyExpr =>
                          val unwrapped = isValueType.value.unwrap(keyExpr)
                          MIO.pure(Expr.quote {
                            Expr.splice(innerKeyEncoder).apply(Expr.splice(unwrapped))
                          })
                        }
                        .map { builder =>
                          Some(builder.build[String]): Option[Expr[K => String]]
                        }
                    case None => MIO.pure(None)
                  }
                case _ =>
                  // 4. Try enum (all case objects) — use toString for key
                  // Uses .toString to avoid Scala 3 staging issues with matchOn inside LambdaBuilder
                  Enum.parse[K].toOption match {
                    case Some(enumm) =>
                      val childrenList = enumm.directChildren.toList
                      val allCaseObjects = Type[K].isEnumeration || Type[K].isJavaEnum || childrenList.forall {
                        case (_, child) =>
                          SingletonValue.unapply(child.Underlying).isDefined
                      }
                      if (allCaseObjects) {
                        LambdaBuilder
                          .of1[K]("key")
                          .traverse { keyExpr =>
                            MIO.pure(Expr.quote {
                              Expr.splice(ctx.config).transformConstructorNames(Expr.splice(keyExpr).toString)
                            })
                          }
                          .map { builder =>
                            Some(builder.build[String]): Option[Expr[K => String]]
                          }
                      } else MIO.pure(None)
                    case None => MIO.pure(None)
                  }
              }
          }
        }
      }
  }

  object EncHandleAsCollectionRule extends EncoderDerivationRule("handle as collection when possible") {
    implicit val JsonT: Type[Json] = Types.Json

    def apply[A: EncoderCtx]: MIO[Rule.Applicability[Expr[Json]]] =
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
                val lambda = builder.build[Json]
                val iterableExpr = isCollection.value.asIterable(ectx.value)
                Rule.matched(Expr.quote {
                  CirceDerivationUtils.encodeIterable[Item](
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

    def apply[A: EncoderCtx]: MIO[Rule.Applicability[Expr[Json]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as a named tuple") >> {
        NamedTuple.parse[A].toEither match {
          case Right(namedTuple) =>
            for {
              _ <- ectx.setHelper[A] { (value, config) =>
                encodeNamedTupleFields[A](namedTuple.primaryConstructor)(using ectx.nestInCache(value, config))
              }
              result <- ectx.getHelper[A].flatMap {
                case Some(helperCall) => MIO.pure(Rule.matched(helperCall(ectx.value, ectx.config)))
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
    ): MIO[Expr[Json]] = {
      implicit val JsonT: Type[Json] = Types.Json
      implicit val StringT: Type[String] = Types.String
      implicit val ProductType: Type[Product] = Types.Product
      implicit val IntType: Type[Int] = Types.Int

      val fields = constructor.parameters.flatten.toList

      NonEmptyList.fromList(fields) match {
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
                deriveEncoderRecursively[Field](using ectx.nest(fieldExpr)).map { fieldJson =>
                  (fName, fieldJson)
                }
              }
            }
            .map { fieldPairs =>
              fieldPairs.toList.foldRight(Expr.quote(List.empty[(String, Json)])) { case ((fName, fieldJson), acc) =>
                Expr.quote {
                  (
                    Expr.splice(ectx.config).transformMemberNames(Expr.splice(Expr(fName))),
                    Expr.splice(fieldJson)
                  ) :: Expr.splice(acc)
                }
              }
            }
            .map { fieldsListExpr =>
              Expr.quote {
                CirceDerivationUtils.jsonFromFields(Expr.splice(fieldsListExpr))
              }
            }
        case None =>
          MIO.pure(Expr.quote {
            CirceDerivationUtils.jsonFromFields(Nil)
          })
      }
    }
  }

  object EncHandleAsSingletonRule extends EncoderDerivationRule("handle as singleton when possible") {

    def apply[A: EncoderCtx]: MIO[Rule.Applicability[Expr[Json]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as a singleton") >> {
        SingletonValue.parse[A].toEither match {
          case Right(_) =>
            MIO.pure(Rule.matched(Expr.quote {
              CirceDerivationUtils.jsonFromFields(Nil)
            }))
          case Left(reason) =>
            MIO.pure(Rule.yielded(reason))
        }
      }
  }

  object EncHandleAsCaseClassRule extends EncoderDerivationRule("handle as case class when possible") {

    def apply[A: EncoderCtx]: MIO[Rule.Applicability[Expr[Json]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as a case class") >> {
        CaseClass.parse[A].toEither match {
          case Right(caseClass) =>
            for {
              _ <- ectx.setHelper[A] { (value, config) =>
                encodeCaseClassFields[A](caseClass)(using ectx.nestInCache(value, config))
              }
              result <- ectx.getHelper[A].flatMap {
                case Some(helperCall) => MIO.pure(Rule.matched(helperCall(ectx.value, ectx.config)))
                case None             => MIO.pure(Rule.yielded(s"Failed to build helper for ${Type[A].prettyPrint}"))
              }
            } yield result

          case Left(reason) =>
            MIO.pure(Rule.yielded(reason))
        }
      }

    @scala.annotation.nowarn("msg=is never used")
    private def encodeCaseClassFields[A: EncoderCtx](
        caseClass: CaseClass[A]
    ): MIO[Expr[Json]] = {
      implicit val JsonT: Type[Json] = Types.Json
      implicit val StringT: Type[String] = Types.String
      implicit val fieldNameT: Type[fieldName] = Types.FieldName
      implicit val transientFieldT: Type[transientField] = Types.TransientField

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
          val err = EncoderDerivationError.TransientFieldMissingDefault(name, Type[A].prettyPrint)
          Log.error(err.message) >> MIO.fail(err)
        case None =>
          val nonTransientFields = allFields.filter { case (name, _) =>
            paramsByName.get(name).forall(p => !hasAnnotationType[transientField](p))
          }

          NonEmptyList.fromList(nonTransientFields) match {
            case Some(fields) =>
              fields
                .parTraverse { case (fName, fieldValue) =>
                  import fieldValue.{Underlying as Field, value as fieldExpr}
                  Log.namedScope(s"Encoding field ${ectx.value.prettyPrint}.$fName: ${Type[Field].prettyPrint}") {
                    deriveEncoderRecursively[Field](using ectx.nest(fieldExpr)).map { fieldJson =>
                      val nameOverride =
                        paramsByName.get(fName).flatMap(p => getAnnotationStringArg[fieldName](p))
                      (fName, fieldJson, nameOverride)
                    }
                  }
                }
                .map { fieldPairs =>
                  fieldPairs.toList.foldRight(Expr.quote(List.empty[(String, Json)])) {
                    case ((fName, fieldJson, Some(customName)), acc) =>
                      Expr.quote {
                        (
                          Expr.splice(Expr(customName)),
                          Expr.splice(fieldJson)
                        ) ::
                          Expr.splice(acc)
                      }
                    case ((fName, fieldJson, None), acc) =>
                      Expr.quote {
                        (
                          Expr.splice(ectx.config).transformMemberNames(Expr.splice(Expr(fName))),
                          Expr.splice(fieldJson)
                        ) ::
                          Expr.splice(acc)
                      }
                  }
                }
                .map { fieldsListExpr =>
                  Expr.quote {
                    CirceDerivationUtils.jsonFromFields(Expr.splice(fieldsListExpr))
                  }
                }
            case None =>
              MIO.pure(Expr.quote {
                CirceDerivationUtils.jsonFromFields(Nil)
              })
          }
      }
    }
  }

  object EncHandleAsEnumRule extends EncoderDerivationRule("handle as enum when possible") {

    def apply[A: EncoderCtx]: MIO[Rule.Applicability[Expr[Json]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as an enum") >> {
        Enum.parse[A].toEither match {
          case Right(enumm) =>
            for {
              _ <- ectx.setHelper[A] { (value, config) =>
                encodeEnumCases[A](enumm)(using ectx.nestInCache(value, config))
              }
              result <- ectx.getHelper[A].flatMap {
                case Some(helperCall) => MIO.pure(Rule.matched(helperCall(ectx.value, ectx.config)))
                case None             => MIO.pure(Rule.yielded(s"Failed to build helper for ${Type[A].prettyPrint}"))
              }
            } yield result
          case Left(reason) =>
            MIO.pure(Rule.yielded(reason))
        }
      }

    private def encodeEnumCases[A: EncoderCtx](
        enumm: Enum[A]
    ): MIO[Expr[Json]] = {
      implicit val JsonT: Type[Json] = Types.Json

      // Check at compile time if all children are singletons (case objects with no fields)
      val childrenList = enumm.directChildren.toList
      val isEnumerationOrJavaEnum = Type[A].isEnumeration || Type[A].isJavaEnum
      val allCaseObjects = isEnumerationOrJavaEnum || childrenList.forall { case (_, child) =>
        SingletonValue.unapply(child.Underlying).isDefined
      }

      enumm
        .parMatchOn[MIO, Json](ectx.value) { matched =>
          import matched.{value as enumCaseValue, Underlying as EnumCase}
          Log.namedScope(s"Encoding enum case ${enumCaseValue.prettyPrint}: ${EnumCase.prettyPrint}") {
            val caseJsonMIO: MIO[Expr[Json]] =
              if (isEnumerationOrJavaEnum) MIO.pure(Expr.quote(Json.obj()))
              else deriveEncoderRecursively[EnumCase](using ectx.nest(enumCaseValue))
            caseJsonMIO.map { caseJson =>
              val caseName: String = childrenList
                .find { case (_, child) =>
                  import child.Underlying as ChildType
                  Type[EnumCase] <:< Type[ChildType]
                }
                .map(_._1)
                .getOrElse(Type[EnumCase].shortName)
              if (allCaseObjects) {
                Expr.quote {
                  val config = Expr.splice(ectx.config)
                  val name = config.transformConstructorNames(Expr.splice(Expr(caseName)))
                  if (config.enumAsStrings) {
                    CirceDerivationUtils.encodeEnumAsString(name)
                  } else {
                    val json = Expr.splice(caseJson)
                    config.discriminator match {
                      case Some(discriminatorField) =>
                        CirceDerivationUtils.addDiscriminator(discriminatorField, name, json)
                      case None =>
                        CirceDerivationUtils.wrapWithTypeName(name, json)
                    }
                  }
                }
              } else {
                Expr.quote {
                  val name = Expr.splice(ectx.config).transformConstructorNames(Expr.splice(Expr(caseName)))
                  val json = Expr.splice(caseJson)
                  Expr.splice(ectx.config).discriminator match {
                    case Some(discriminatorField) =>
                      CirceDerivationUtils.addDiscriminator(discriminatorField, name, json)
                    case None =>
                      CirceDerivationUtils.wrapWithTypeName(name, json)
                  }
                }
              }
            }
          }
        }
        .flatMap {
          case Some(result) => MIO.pure(result)
          case None         =>
            val err = EncoderDerivationError.NoChildrenInSealedTrait(Type[A].prettyPrint)
            Log.error(err.message) >> MIO.fail(err)
        }
    }
  }

  // Types

  private[compiletime] object Types {

    def Encoder: Type.Ctor1[Encoder] = Type.Ctor1.of[Encoder]
    def KeyEncoder: Type.Ctor1[KeyEncoder] = Type.Ctor1.of[KeyEncoder]
    def KindlingsEncoder: Type.Ctor1[KindlingsEncoder] = Type.Ctor1.of[KindlingsEncoder]
    def KindlingsEncoderAsObject: Type.Ctor1[KindlingsEncoderAsObject] = Type.Ctor1.of[KindlingsEncoderAsObject]
    val EncoderLogDerivation: Type[hearth.kindlings.circederivation.KindlingsEncoder.LogDerivation] =
      Type.of[hearth.kindlings.circederivation.KindlingsEncoder.LogDerivation]
    val Json: Type[Json] = Type.of[Json]
    val JsonObject: Type[JsonObject] = Type.of[JsonObject]
    val Configuration: Type[Configuration] = Type.of[Configuration]
    val String: Type[String] = Type.of[String]
    val FieldName: Type[fieldName] = Type.of[fieldName]
    val TransientField: Type[transientField] = Type.of[transientField]
    val Int: Type[Int] = Type.of[Int]
    val Product: Type[Product] = Type.of[Product]
  }
}

sealed private[compiletime] trait EncoderDerivationError
    extends util.control.NoStackTrace
    with Product
    with Serializable {
  def message: String
  override def getMessage(): String = message
}
private[compiletime] object EncoderDerivationError {
  final case class UnsupportedType(tpeName: String, reasons: List[String]) extends EncoderDerivationError {
    override def message: String =
      s"The type $tpeName was not handled by any encoder derivation rule:\n${reasons.mkString("\n")}"
  }
  final case class TransientFieldMissingDefault(fieldName: String, tpeName: String) extends EncoderDerivationError {
    override def message: String =
      s"@transientField on field '$fieldName' of $tpeName requires a default value"
  }
  final case class NoChildrenInSealedTrait(tpeName: String) extends EncoderDerivationError {
    override def message: String =
      s"The type $tpeName does not have any children!"
  }
}
