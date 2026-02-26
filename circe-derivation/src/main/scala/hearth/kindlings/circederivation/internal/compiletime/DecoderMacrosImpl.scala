package hearth.kindlings.circederivation.internal.compiletime

import hearth.MacroCommons
import hearth.fp.data.NonEmptyList
import hearth.fp.effect.*
import hearth.fp.syntax.*
import hearth.std.*

import hearth.kindlings.circederivation.{Configuration, KindlingsDecoder}
import hearth.kindlings.circederivation.annotations.{fieldName, transientField}
import hearth.kindlings.circederivation.internal.runtime.CirceDerivationUtils
import cats.data.{Validated, ValidatedNel}
import io.circe.{Decoder, DecodingFailure, HCursor, Json, KeyDecoder}

trait DecoderMacrosImpl { this: MacroCommons & StdExtensions & AnnotationSupport =>

  // Entrypoints

  @scala.annotation.nowarn("msg=is never used")
  def deriveInlineDecode[A: Type](
      jsonExpr: Expr[Json],
      configExpr: Expr[Configuration]
  ): Expr[Either[DecodingFailure, A]] = {
    implicit val EitherT: Type[Either[DecodingFailure, A]] = DTypes.DecoderResult[A]
    implicit val JsonT: Type[Json] = DTypes.Json
    implicit val HCursorT: Type[HCursor] = DTypes.HCursor
    implicit val ConfigT: Type[Configuration] = DTypes.Configuration
    implicit val DecodingFailureT: Type[DecodingFailure] = DTypes.DecodingFailure

    deriveDecoderFromCtxAndAdaptForEntrypoint[A, Either[DecodingFailure, A]]("KindlingsDecoder.decode") { fromCtx =>
      ValDefs.createVal[Json](jsonExpr).use { jsonVal =>
        ValDefs.createVal[Configuration](configExpr).use { configVal =>
          Expr.quote {
            val _ = Expr.splice(jsonVal)
            val _ = Expr.splice(configVal)
            Expr.splice {
              val cursorExpr: Expr[HCursor] = Expr.quote(Expr.splice(jsonVal).hcursor)
              fromCtx(DecoderCtx.from(cursorExpr, configVal, Expr.quote(true), derivedType = None))
            }
          }
        }
      }
    }
  }

  @scala.annotation.nowarn("msg=is never used")
  def deriveDecoderTypeClass[A: Type](configExpr: Expr[Configuration]): Expr[KindlingsDecoder[A]] = {
    implicit val DecoderA: Type[Decoder[A]] = DTypes.Decoder[A]
    implicit val KindlingsDecoderA: Type[KindlingsDecoder[A]] = DTypes.KindlingsDecoder[A]
    implicit val EitherT: Type[Either[DecodingFailure, A]] = DTypes.DecoderResult[A]
    implicit val HCursorT: Type[HCursor] = DTypes.HCursor
    implicit val ConfigT: Type[Configuration] = DTypes.Configuration
    implicit val DecodingFailureT: Type[DecodingFailure] = DTypes.DecodingFailure
    implicit val BooleanT: Type[Boolean] = DTypes.Boolean
    implicit val AnyT: Type[Any] = DTypes.Any
    implicit val ValidatedNelDFA: Type[ValidatedNel[DecodingFailure, A]] = DTypes.ValidatedNelDF[A]
    val selfType: Option[??] = Some(Type[A].as_??)

    if (Type[A] =:= Type.of[Nothing].asInstanceOf[Type[A]] || Type[A] =:= Type.of[Any].asInstanceOf[Type[A]])
      Environment.reportErrorAndAbort(
        s"KindlingsDecoder.derived: type parameter was inferred as ${Type[A].prettyPrint}, which is likely unintended.\n" +
          "Provide an explicit type parameter, e.g.: KindlingsDecoder.derived[MyType]\n" +
          "or add a type ascription to the result variable."
      )

    Log
      .namedScope(
        s"Deriving decoder for ${Type[A].prettyPrint} at: ${Environment.currentPosition.prettyPrint}"
      ) {
        MIO.scoped { runSafe =>
          // Step 1: Run derivation once (populates cache with the cached def)
          val ctx = DecoderCtx.from[A](
            cursor = Expr.quote(null.asInstanceOf[HCursor]), // placeholder — real cursor comes from cached def params
            config = Expr.quote(null.asInstanceOf[Configuration]), // placeholder
            failFast = Expr.quote(true), // placeholder
            derivedType = selfType
          )
          runSafe {
            for {
              _ <- Environment.loadStandardExtensions().toMIO(allowFailures = false)
              _ <- deriveDecoderRecursively[A](using ctx)
            } yield ()
          }

          // Step 2: Get the cached helper reference and cache
          val helperOpt = runSafe(ctx.getHelper[A])
          val cache = runSafe(ctx.cache.get)

          // Step 2b: For types handled without a cached helper (value types, options, collections, maps),
          // build a direct Decoder[A] that we can delegate to in the KindlingsDecoder body.
          val directDecoderOpt: Option[Expr[Decoder[A]]] =
            if (helperOpt.isDefined) None
            else {
              Some(runSafe {
                LambdaBuilder
                  .of1[HCursor]("directCursor")
                  .traverse { cursorExpr =>
                    val freshCtx = DecoderCtx.from[A](cursorExpr, configExpr, Expr.quote(true), derivedType = selfType)
                    for {
                      _ <- Environment.loadStandardExtensions().toMIO(allowFailures = false)
                      result <- deriveDecoderRecursively[A](using freshCtx)
                      freshCache <- freshCtx.cache.get
                    } yield freshCache.toValDefs.use(_ => result)
                  }
                  .map { builder =>
                    val decodeFn = builder.build[Either[DecodingFailure, A]]
                    Expr.quote(CirceDerivationUtils.decoderFromFn(Expr.splice(decodeFn)))
                  }
              })
            }

          // Step 3: Wrap cached defs around the entire KindlingsDecoder block
          // Both apply and decodeAccumulating call the same cached def with different failFast values
          cache.toValDefs.use { _ =>
            ValDefs.createVal[Configuration](configExpr).use { configVal =>
              Expr.quote {
                val cfg = Expr.splice(configVal)
                new KindlingsDecoder[A] {
                  def apply(c: HCursor): Decoder.Result[A] = {
                    val _ = c
                    Expr.splice {
                      helperOpt match {
                        case Some(helper) =>
                          Expr.quote {
                            Expr
                              .splice(helper(Expr.quote(c), Expr.quote(cfg), Expr.quote(true)))
                              .asInstanceOf[Either[DecodingFailure, A]]
                          }
                        case None =>
                          // Fallback for types handled without a cached helper (value types,
                          // options, collections, maps). Delegate to a direct decoder.
                          Expr.quote {
                            Expr.splice(directDecoderOpt.get).apply(c)
                          }
                      }
                    }
                  }
                  override def decodeAccumulating(c: HCursor): Decoder.AccumulatingResult[A] = {
                    val _ = c
                    Expr.splice {
                      helperOpt match {
                        case Some(helper) =>
                          Expr.quote {
                            Expr
                              .splice(helper(Expr.quote(c), Expr.quote(cfg), Expr.quote(false)))
                              .asInstanceOf[ValidatedNel[DecodingFailure, A]]
                          }
                        case None =>
                          // Fallback: delegate to the direct decoder's decodeAccumulating
                          Expr.quote {
                            Expr.splice(directDecoderOpt.get).decodeAccumulating(c)
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
      .flatTap { result =>
        Log.info(s"Derived final decoder result: ${result.prettyPrint}")
      }
      .runToExprOrFail(
        "KindlingsDecoder.derived",
        infoRendering = if (shouldWeLogDecoderDerivation) RenderFrom(Log.Level.Info) else DontRender,
        errorRendering = if (shouldWeLogDecoderDerivation) RenderFrom(Log.Level.Info) else DontRender
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
          "Enable debug logging with: import hearth.kindlings.circederivation.debug.logDerivationForKindlingsDecoder or scalac option -Xmacro-settings:circeDerivation.logDerivation=true"
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

  def deriveDecoderFromCtxAndAdaptForEntrypoint[A: Type, Out: Type](macroName: String)(
      provideCtxAndAdapt: (DecoderCtx[A] => Expr[Either[DecodingFailure, A]]) => Expr[Out]
  ): Expr[Out] = {
    if (Type[A] =:= Type.of[Nothing].asInstanceOf[Type[A]] || Type[A] =:= Type.of[Any].asInstanceOf[Type[A]])
      Environment.reportErrorAndAbort(
        s"$macroName: type parameter was inferred as Nothing, which is likely unintended.\n" +
          s"Provide an explicit type parameter, e.g.: $macroName[MyType](...)\n" +
          "or add a type ascription to the result variable."
      )
    Log
      .namedScope(
        s"Deriving decoder for ${Type[A].prettyPrint} at: ${Environment.currentPosition.prettyPrint}"
      ) {
        MIO.scoped { runSafe =>
          val fromCtx: (DecoderCtx[A] => Expr[Either[DecodingFailure, A]]) =
            (ctx: DecoderCtx[A]) =>
              runSafe {
                for {
                  _ <- Environment.loadStandardExtensions().toMIO(allowFailures = false)
                  result <- deriveDecoderRecursively[A](using ctx)
                  cache <- ctx.cache.get
                } yield cache.toValDefs.use(_ => result)
              }

          provideCtxAndAdapt(fromCtx)
        }
      }
      .flatTap { result =>
        Log.info(s"Derived final decoder result: ${result.prettyPrint}")
      }
      .runToExprOrFail(
        macroName,
        infoRendering = if (shouldWeLogDecoderDerivation) RenderFrom(Log.Level.Info) else DontRender,
        errorRendering = if (shouldWeLogDecoderDerivation) RenderFrom(Log.Level.Info) else DontRender
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
          "Enable debug logging with: import hearth.kindlings.circederivation.debug.logDerivationForKindlingsDecoder or scalac option -Xmacro-settings:circeDerivation.logDerivation=true"
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

  def shouldWeLogDecoderDerivation: Boolean = {
    implicit val LogDerivation: Type[KindlingsDecoder.LogDerivation] = DTypes.DecoderLogDerivation
    def logDerivationImported = Expr.summonImplicit[KindlingsDecoder.LogDerivation].isDefined

    def logDerivationSetGlobally = (for {
      data <- Environment.typedSettings.toOption
      circeDerivation <- data.get("circeDerivation")
      shouldLog <- circeDerivation.get("logDerivation").flatMap(_.asBoolean)
    } yield shouldLog).getOrElse(false)

    logDerivationImported || logDerivationSetGlobally
  }

  // Context

  final case class DecoderCtx[A](
      tpe: Type[A],
      cursor: Expr[HCursor],
      config: Expr[Configuration],
      failFast: Expr[Boolean],
      cache: MLocal[ValDefsCache],
      derivedType: Option[??]
  ) {

    def nest[B: Type](newCursor: Expr[HCursor]): DecoderCtx[B] = copy[B](
      tpe = Type[B],
      cursor = newCursor,
      failFast = Expr.quote(true)
    )

    def nestInCache(
        newCursor: Expr[HCursor],
        newConfig: Expr[Configuration],
        newFailFast: Expr[Boolean]
    ): DecoderCtx[A] = copy(
      cursor = newCursor,
      config = newConfig,
      failFast = newFailFast
    )

    def getInstance[B: Type]: MIO[Option[Expr[Decoder[B]]]] = {
      implicit val DecoderB: Type[Decoder[B]] = DTypes.Decoder[B]
      cache.get0Ary[Decoder[B]]("cached-decoder-instance")
    }
    def setInstance[B: Type](instance: Expr[Decoder[B]]): MIO[Unit] = {
      implicit val DecoderB: Type[Decoder[B]] = DTypes.Decoder[B]
      Log.info(s"Caching Decoder instance for ${Type[B].prettyPrint}") >>
        cache.buildCachedWith(
          "cached-decoder-instance",
          ValDefBuilder.ofLazy[Decoder[B]](s"decoder_${Type[B].shortName}")
        )(_ => instance)
    }

    // The cached decode helper returns `Any` because the actual return type depends on `failFast`:
    //   - failFast=true  → Either[DecodingFailure, B]
    //   - failFast=false → ValidatedNel[DecodingFailure, B]
    //
    // Ideally we'd express this as a match type:
    //   type Result[FailFast <: Boolean] = FailFast match {
    //     case true  => [A] =>> Either[DecodingFailure, A]
    //     case false => [A] =>> ValidatedNel[DecodingFailure, A]
    //   }
    //   def decode_Foo(cursor: HCursor, config: Configuration, failFast: Boolean): Result[failFast.type][Foo]
    //
    // but writing a reusable macro utility (ValDefBuilder, ValDefsCache) that works with
    // match types would be very difficult — even without Scala 2.13 support. So we use `Any`
    // as the return type and cast at each call site where the actual type is known.
    //
    // The cache key includes Type[B].prettyPrint to prevent collisions: ValDefsCache keys by
    // (String, arg types, return type), and since the return type is always `Any`, the string
    // must disambiguate different types.
    private def helperCacheKey[B: Type]: String = s"cached-decode-method:${Type[B].prettyPrint}"

    def getHelper[B: Type]: MIO[Option[(Expr[HCursor], Expr[Configuration], Expr[Boolean]) => Expr[Any]]] = {
      implicit val AnyT: Type[Any] = DTypes.Any
      implicit val HCursorT: Type[HCursor] = DTypes.HCursor
      implicit val ConfigT: Type[Configuration] = DTypes.Configuration
      implicit val BooleanT: Type[Boolean] = DTypes.Boolean
      cache.get3Ary[HCursor, Configuration, Boolean, Any](helperCacheKey[B])
    }
    def setHelper[B: Type](
        helper: (Expr[HCursor], Expr[Configuration], Expr[Boolean]) => MIO[Expr[Any]]
    ): MIO[Unit] = {
      implicit val AnyT: Type[Any] = DTypes.Any
      implicit val HCursorT: Type[HCursor] = DTypes.HCursor
      implicit val ConfigT: Type[Configuration] = DTypes.Configuration
      implicit val BooleanT: Type[Boolean] = DTypes.Boolean
      val defBuilder =
        ValDefBuilder.ofDef3[HCursor, Configuration, Boolean, Any](s"decode_${Type[B].shortName}")
      for {
        _ <- Log.info(s"Forward-declaring decode helper for ${Type[B].prettyPrint}")
        _ <- cache.forwardDeclare(helperCacheKey[B], defBuilder)
        _ <- MIO.scoped { runSafe =>
          runSafe(cache.buildCachedWith(helperCacheKey[B], defBuilder) { case (_, (cursor, config, failFast)) =>
            runSafe(helper(cursor, config, failFast))
          })
        }
        _ <- Log.info(s"Defined decode helper for ${Type[B].prettyPrint}")
      } yield ()
    }

    override def toString: String =
      s"decode[${tpe.prettyPrint}](cursor = ${cursor.prettyPrint}, config = ${config.prettyPrint})"
  }
  object DecoderCtx {

    def from[A: Type](
        cursor: Expr[HCursor],
        config: Expr[Configuration],
        failFast: Expr[Boolean],
        derivedType: Option[??]
    ): DecoderCtx[A] = DecoderCtx(
      tpe = Type[A],
      cursor = cursor,
      config = config,
      failFast = failFast,
      cache = ValDefsCache.mlocal,
      derivedType = derivedType
    )
  }

  def dctx[A](implicit A: DecoderCtx[A]): DecoderCtx[A] = A

  implicit def currentDecoderValueType[A: DecoderCtx]: Type[A] = dctx.tpe

  abstract class DecoderDerivationRule(val name: String) extends Rule {
    def apply[A: DecoderCtx]: MIO[Rule.Applicability[Expr[Either[DecodingFailure, A]]]]
  }

  // The actual derivation logic

  def deriveDecoderRecursively[A: DecoderCtx]: MIO[Expr[Either[DecodingFailure, A]]] =
    Log
      .namedScope(s"Deriving decoder for type ${Type[A].prettyPrint}") {
        Rules(
          DecUseCachedDefWhenAvailableRule,
          DecUseImplicitWhenAvailableRule,
          DecHandleAsValueTypeRule,
          DecHandleAsOptionRule,
          DecHandleAsMapRule,
          DecHandleAsCollectionRule,
          DecHandleAsNamedTupleRule,
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
            val err = DecoderDerivationError.UnsupportedType(Type[A].prettyPrint, reasonsStrings)
            Log.error(err.message) >> MIO.fail(err)
        }
      }

  // Rules

  object DecUseCachedDefWhenAvailableRule extends DecoderDerivationRule("use cached def when available") {

    def apply[A: DecoderCtx]: MIO[Rule.Applicability[Expr[Either[DecodingFailure, A]]]] =
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
        instance: Expr[Decoder[A]]
    ): MIO[Rule.Applicability[Expr[Either[DecodingFailure, A]]]] =
      Log.info(s"Found cached decoder instance for ${Type[A].prettyPrint}") >> MIO.pure(
        Rule.matched(Expr.quote {
          Expr.splice(instance).apply(Expr.splice(dctx.cursor))
        })
      )

    private def callCachedHelper[A: DecoderCtx](
        helperCall: (Expr[HCursor], Expr[Configuration], Expr[Boolean]) => Expr[Any]
    ): MIO[Rule.Applicability[Expr[Either[DecodingFailure, A]]]] = {
      implicit val EitherDFA: Type[Either[DecodingFailure, A]] = DTypes.DecoderResult[A]
      Log.info(s"Found cached decoder helper for ${Type[A].prettyPrint}") >> MIO.pure(
        Rule.matched(Expr.quote {
          Expr.splice(helperCall(dctx.cursor, dctx.config, dctx.failFast)).asInstanceOf[Either[DecodingFailure, A]]
        })
      )
    }

    private def yieldUnsupported[A: DecoderCtx]: MIO[Rule.Applicability[Expr[Either[DecodingFailure, A]]]] =
      MIO.pure(Rule.yielded(s"The type ${Type[A].prettyPrint} does not have a cached decoder"))
  }

  object DecUseImplicitWhenAvailableRule extends DecoderDerivationRule("use implicit when available") {

    lazy val ignoredImplicits: Seq[UntypedMethod] = {
      val ours = Type.of[KindlingsDecoder.type].methods.collect {
        case method if method.value.name == "derived" => method.value.asUntyped
      }
      val circeDecoder = Type.of[Decoder.type].methods.collect {
        case method if method.value.name == "derived" => method.value.asUntyped
      }
      ours ++ circeDecoder
    }

    def apply[A: DecoderCtx]: MIO[Rule.Applicability[Expr[Either[DecodingFailure, A]]]] =
      Log.info(s"Attempting to use implicit Decoder for ${Type[A].prettyPrint}") >> {
        // Skip implicit search for the self type being derived to prevent self-referential loops
        // (e.g., `implicit val dec: Decoder[X] = KindlingsDecoder.derived[X]` would otherwise
        // find `dec` itself during macro expansion, generating code that calls itself infinitely).
        if (dctx.derivedType.exists(_.Underlying =:= Type[A]))
          MIO.pure(
            Rule.yielded(s"The type ${Type[A].prettyPrint} is the type being derived, skipping implicit search")
          )
        else
          DTypes.Decoder[A].summonExprIgnoring(ignoredImplicits*).toEither match {
            case Right(instanceExpr) => cacheAndUse[A](instanceExpr)
            case Left(reason)        => yieldUnsupported[A](reason)
          }
      }

    private def cacheAndUse[A: DecoderCtx](
        instanceExpr: Expr[Decoder[A]]
    ): MIO[Rule.Applicability[Expr[Either[DecodingFailure, A]]]] =
      Log.info(s"Found implicit decoder ${instanceExpr.prettyPrint}, using directly") >>
        MIO.pure(Rule.matched(Expr.quote {
          Expr.splice(instanceExpr).apply(Expr.splice(dctx.cursor))
        }))

    private def yieldUnsupported[A: DecoderCtx](
        reason: String
    ): MIO[Rule.Applicability[Expr[Either[DecodingFailure, A]]]] =
      MIO.pure(
        Rule.yielded(
          s"The type ${Type[A].prettyPrint} does not have an implicit Decoder instance: $reason"
        )
      )
  }

  object DecHandleAsValueTypeRule extends DecoderDerivationRule("handle as value type when possible") {

    def apply[A: DecoderCtx]: MIO[Rule.Applicability[Expr[Either[DecodingFailure, A]]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as a value type") >> {
        Type[A] match {
          // IArray is an opaque type on Scala 3, so IsValueType matches it before IsCollection.
          // Skip it here — IsCollectionProviderForIArray handles it correctly.
          case _ if Type[A].isIArray =>
            MIO.pure(Rule.yielded(s"The type ${Type[A].prettyPrint} is IArray, handled as collection instead"))

          case IsValueType(isValueType) =>
            import isValueType.Underlying as Inner

            // Summon a Decoder[Inner]
            DTypes.Decoder[Inner].summonExprIgnoring(DecUseImplicitWhenAvailableRule.ignoredImplicits*).toEither match {
              case Right(innerDecoder) =>
                // Build wrap lambda outside quotes to avoid staging issues with wrap.Result type
                LambdaBuilder
                  .of1[Inner]("inner")
                  .traverse { innerExpr =>
                    MIO.pure(isValueType.value.wrap.apply(innerExpr).asInstanceOf[Expr[A]])
                  }
                  .map { builder =>
                    val wrapLambda = builder.build[A]
                    Rule.matched(Expr.quote {
                      Expr.splice(innerDecoder).apply(Expr.splice(dctx.cursor)).map(Expr.splice(wrapLambda))
                    })
                  }
              case Left(reason) =>
                MIO.pure(Rule.yielded(s"Value type inner ${Type[Inner].prettyPrint} has no Decoder: $reason"))
            }

          case _ =>
            MIO.pure(Rule.yielded(s"The type ${Type[A].prettyPrint} is not a value type"))
        }
      }
  }

  object DecHandleAsOptionRule extends DecoderDerivationRule("handle as Option when possible") {

    def apply[A: DecoderCtx]: MIO[Rule.Applicability[Expr[Either[DecodingFailure, A]]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as Option") >> {
        Type[A] match {
          case IsOption(isOption) =>
            import isOption.Underlying as Inner
            implicit val HCursorT: Type[HCursor] = DTypes.HCursor
            implicit val EitherDFInner: Type[Either[DecodingFailure, Inner]] = DTypes.DecoderResult[Inner]

            LambdaBuilder
              .of1[HCursor]("innerCursor")
              .traverse { innerCursorExpr =>
                deriveDecoderRecursively[Inner](using dctx.nest[Inner](innerCursorExpr))
              }
              .map { builder =>
                val decodeFn = builder.build[Either[DecodingFailure, Inner]]
                Rule.matched(Expr.quote {
                  CirceDerivationUtils
                    .decodeOptionFromFn(
                      Expr.splice(dctx.cursor),
                      Expr.splice(decodeFn)
                    )
                    .asInstanceOf[Either[DecodingFailure, A]]
                })
              }

          case _ =>
            MIO.pure(Rule.yielded(s"The type ${Type[A].prettyPrint} is not an Option"))
        }
      }
  }

  @scala.annotation.nowarn("msg=Infinite loop")
  object DecHandleAsMapRule extends DecoderDerivationRule("handle as map when possible") {

    def apply[A: DecoderCtx]: MIO[Rule.Applicability[Expr[Either[DecodingFailure, A]]]] =
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
    ): MIO[Rule.Applicability[Expr[Either[DecodingFailure, A]]]] = {
      import isMap.{Key, Value, CtorResult}
      implicit val StringT: Type[String] = DTypes.String
      implicit val HCursorT: Type[HCursor] = DTypes.HCursor
      implicit val EitherDFValue: Type[Either[DecodingFailure, Value]] = DTypes.DecoderResult[Value]

      if (Key <:< Type[String]) {
        // String keys — use existing fast path
        LambdaBuilder
          .of1[HCursor]("valueCursor")
          .traverse { valueCursorExpr =>
            deriveDecoderRecursively[Value](using dctx.nest[Value](valueCursorExpr))
          }
          .map { builder =>
            val decodeFn = builder.build[Either[DecodingFailure, Value]]
            val factoryExpr = isMap.factory
            Rule.matched(Expr.quote {
              CirceDerivationUtils
                .decodeMapWith(
                  Expr.splice(dctx.cursor),
                  CirceDerivationUtils.decoderFromFn(Expr.splice(decodeFn)),
                  Expr
                    .splice(factoryExpr)
                    .asInstanceOf[scala.collection.Factory[(String, Value), A]]
                )
                .asInstanceOf[Either[DecodingFailure, A]]
            })
          }
      } else {
        // Non-String keys — try to derive a key decoder
        deriveKeyDecoder[Key].flatMap {
          case Some(keyDecoderLambda) =>
            LambdaBuilder
              .of1[HCursor]("valueCursor")
              .traverse { valueCursorExpr =>
                deriveDecoderRecursively[Value](using dctx.nest[Value](valueCursorExpr))
              }
              .map { builder =>
                val decodeFn = builder.build[Either[DecodingFailure, Value]]
                val factoryExpr = isMap.factory
                Rule.matched(Expr.quote {
                  CirceDerivationUtils
                    .decodeMapWithKeyDecoder[Key, Value, A](
                      Expr.splice(dctx.cursor),
                      Expr.splice(keyDecoderLambda),
                      CirceDerivationUtils.decoderFromFn(Expr.splice(decodeFn)),
                      Expr
                        .splice(factoryExpr)
                        .asInstanceOf[scala.collection.Factory[(Key, Value), A]]
                    )
                    .asInstanceOf[Either[DecodingFailure, A]]
                })
              }
          case None =>
            MIO.pure(
              Rule.yielded(s"Map key type ${Key.prettyPrint} is not String and no key decoder could be derived")
            )
        }
      }
    }

    /** Try to derive a String => Either[DecodingFailure, K] function for map keys. Returns None if derivation fails. */
    @scala.annotation.nowarn("msg=is never used")
    private def deriveKeyDecoder[K: Type](implicit
        ctx: DecoderCtx[?]
    ): MIO[Option[Expr[String => Either[DecodingFailure, K]]]] = {
      implicit val StringT: Type[String] = DTypes.String
      implicit val DecodingFailureT: Type[DecodingFailure] = DTypes.DecodingFailure
      implicit val EitherDFK: Type[Either[DecodingFailure, K]] = DTypes.DecoderResult[K]

      Log.info(s"Attempting to derive key decoder for ${Type[K].prettyPrint}") >> {
        // 1. Built-in types — inline parsing via runtime helpers
        val builtIn: Option[MIO[Option[Expr[String => Either[DecodingFailure, K]]]]] = {
          def makeBuiltInKeyDecoder(
              body: Expr[String] => Expr[Either[DecodingFailure, K]]
          ): MIO[Option[Expr[String => Either[DecodingFailure, K]]]] =
            LambdaBuilder
              .of1[String]("keyStr")
              .traverse { keyStrExpr =>
                MIO.pure(body(keyStrExpr))
              }
              .map { builder =>
                Some(builder.build[Either[DecodingFailure, K]]): Option[
                  Expr[String => Either[DecodingFailure, K]]
                ]
              }

          if (Type[K] =:= Type.of[Int])
            Some(
              makeBuiltInKeyDecoder(s =>
                Expr.quote(CirceDerivationUtils.decodeKeyInt(Expr.splice(s)).asInstanceOf[Either[DecodingFailure, K]])
              )
            )
          else if (Type[K] =:= Type.of[Long])
            Some(
              makeBuiltInKeyDecoder(s =>
                Expr.quote(CirceDerivationUtils.decodeKeyLong(Expr.splice(s)).asInstanceOf[Either[DecodingFailure, K]])
              )
            )
          else if (Type[K] =:= Type.of[Double])
            Some(
              makeBuiltInKeyDecoder(s =>
                Expr.quote(
                  CirceDerivationUtils.decodeKeyDouble(Expr.splice(s)).asInstanceOf[Either[DecodingFailure, K]]
                )
              )
            )
          else if (Type[K] =:= Type.of[Short])
            Some(
              makeBuiltInKeyDecoder(s =>
                Expr.quote(CirceDerivationUtils.decodeKeyShort(Expr.splice(s)).asInstanceOf[Either[DecodingFailure, K]])
              )
            )
          else if (Type[K] =:= Type.of[Byte])
            Some(
              makeBuiltInKeyDecoder(s =>
                Expr.quote(CirceDerivationUtils.decodeKeyByte(Expr.splice(s)).asInstanceOf[Either[DecodingFailure, K]])
              )
            )
          else
            None
        }

        builtIn.getOrElse {
          // 2. Try summoning user-provided KeyDecoder[K]
          DTypes.KeyDecoder[K].summonExprIgnoring().toEither match {
            case Right(keyDecoderExpr) =>
              Log.info(s"Found implicit KeyDecoder[${Type[K].prettyPrint}]") >>
                LambdaBuilder
                  .of1[String]("keyStr")
                  .traverse { keyStrExpr =>
                    MIO.pure(Expr.quote {
                      Expr.splice(keyDecoderExpr).apply(Expr.splice(keyStrExpr)) match {
                        case Some(k) => Right(k): Either[DecodingFailure, K]
                        case None    =>
                          Left(
                            DecodingFailure(
                              "Failed to decode map key: " + Expr.splice(keyStrExpr),
                              Nil
                            )
                          ): Either[DecodingFailure, K]
                      }
                    })
                  }
                  .map { builder =>
                    Some(builder.build[Either[DecodingFailure, K]]): Option[
                      Expr[String => Either[DecodingFailure, K]]
                    ]
                  }
            case Left(_) =>
              // 3. Try value type — unwrap to inner, recurse
              Type[K] match {
                case IsValueType(isValueType) =>
                  import isValueType.Underlying as Inner
                  deriveKeyDecoder[Inner].flatMap {
                    case Some(innerKeyDecoder) =>
                      // Build wrap lambda outside quotes
                      LambdaBuilder
                        .of1[Inner]("inner")
                        .traverse { innerExpr =>
                          MIO.pure(isValueType.value.wrap.apply(innerExpr).asInstanceOf[Expr[K]])
                        }
                        .flatMap { wrapBuilder =>
                          val wrapLambda = wrapBuilder.build[K]
                          LambdaBuilder
                            .of1[String]("keyStr")
                            .traverse { keyStrExpr =>
                              MIO.pure(Expr.quote {
                                Expr.splice(innerKeyDecoder).apply(Expr.splice(keyStrExpr)).map(Expr.splice(wrapLambda))
                              })
                            }
                            .map { builder =>
                              Some(builder.build[Either[DecodingFailure, K]]): Option[
                                Expr[String => Either[DecodingFailure, K]]
                              ]
                            }
                        }
                    case None => MIO.pure(None)
                  }
                case _ =>
                  // 4. Try enum (all case objects) — build lookup Map[String, K] and use runtime helper
                  // Uses runtime dispatch to avoid Scala 3 staging issues with singleton expressions in LambdaBuilder
                  Enum.parse[K] match {
                    case Some(enumm) =>
                      val childrenList = enumm.directChildren.toList
                      val allCaseObjects = Type[K].isEnumeration || Type[K].isJavaEnum || childrenList.forall {
                        case (_, child) =>
                          Type.isVal(using child.Underlying) ||
                          CaseClass
                            .parse(using child.Underlying)
                            .exists(_.primaryConstructor.parameters.flatten.isEmpty)
                      }
                      if (allCaseObjects) {
                        NonEmptyList.fromList(childrenList) match {
                          case Some(children) =>
                            // Build singleton expressions for each child
                            children
                              .parTraverse { case (childName, child) =>
                                import child.Underlying as ChildType
                                Expr.singletonOf[ChildType] match {
                                  case Some(singleton) =>
                                    MIO.pure((childName, singleton.asInstanceOf[Expr[K]]))
                                  case None =>
                                    CaseClass.parse[ChildType] match {
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
                                // Build a Map[String, K] expression: Map(config.transformConstructorNames("Name") -> singleton, ...)
                                val lookupMapExpr: Expr[Map[String, K]] = casesNel.toList.foldRight(
                                  Expr.quote(Map.empty[String, K])
                                ) { case ((caseName, caseExpr), acc) =>
                                  Expr.quote {
                                    Expr.splice(acc) + (
                                      Expr.splice(ctx.config).transformConstructorNames(Expr.splice(Expr(caseName)))
                                        -> Expr.splice(caseExpr)
                                    )
                                  }
                                }
                                // Build the key decoder lambda using the runtime helper
                                LambdaBuilder
                                  .of1[String]("keyStr")
                                  .traverse { keyStrExpr =>
                                    MIO.pure(Expr.quote {
                                      CirceDerivationUtils.decodeEnumKey[K](
                                        Expr.splice(keyStrExpr),
                                        Expr.splice(lookupMapExpr)
                                      )
                                    })
                                  }
                                  .map { builder =>
                                    Some(builder.build[Either[DecodingFailure, K]]): Option[
                                      Expr[String => Either[DecodingFailure, K]]
                                    ]
                                  }
                              }
                          case None => MIO.pure(None)
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

    def apply[A: DecoderCtx]: MIO[Rule.Applicability[Expr[Either[DecodingFailure, A]]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as a collection") >> {
        Type[A] match {
          case IsCollection(isCollection) =>
            import isCollection.Underlying as Item
            import isCollection.value.CtorResult
            implicit val HCursorT: Type[HCursor] = DTypes.HCursor
            implicit val EitherDFItem: Type[Either[DecodingFailure, Item]] = DTypes.DecoderResult[Item]

            LambdaBuilder
              .of1[HCursor]("itemCursor")
              .traverse { itemCursorExpr =>
                deriveDecoderRecursively[Item](using dctx.nest[Item](itemCursorExpr))
              }
              .map { builder =>
                val decodeFn = builder.build[Either[DecodingFailure, Item]]
                val factoryExpr = isCollection.value.factory
                Rule.matched(Expr.quote {
                  CirceDerivationUtils
                    .decodeCollectionWith(
                      Expr.splice(dctx.cursor),
                      CirceDerivationUtils.decoderFromFn(Expr.splice(decodeFn)),
                      Expr.splice(factoryExpr).asInstanceOf[scala.collection.Factory[Item, A]]
                    )
                    .asInstanceOf[Either[DecodingFailure, A]]
                })
              }

          case _ =>
            MIO.pure(Rule.yielded(s"The type ${Type[A].prettyPrint} is not a collection"))
        }
      }
  }

  object DecHandleAsNamedTupleRule extends DecoderDerivationRule("handle as named tuple when possible") {

    def apply[A: DecoderCtx]: MIO[Rule.Applicability[Expr[Either[DecodingFailure, A]]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as a named tuple") >> {
        if (!Type[A].isNamedTuple)
          MIO.pure(Rule.yielded(s"The type ${Type[A].prettyPrint} is not a named tuple"))
        else
          Type[A].primaryConstructor match {
            case Some(constructor) =>
              for {
                _ <- dctx.setHelper[A] { (cursor, config, failFast) =>
                  @scala.annotation.nowarn("msg=is never used")
                  implicit val AnyT: Type[Any] = DTypes.Any
                  @scala.annotation.nowarn("msg=is never used")
                  implicit val BooleanT: Type[Boolean] = DTypes.Boolean
                  decodeNamedTupleFields[A](constructor)(using dctx.nestInCache(cursor, config, failFast))
                    .map { eitherExpr =>
                      // Named tuples always use fail-fast decode; convert to ValidatedNel when failFast=false
                      Expr.quote {
                        (if (Expr.splice(failFast)) Expr.splice(eitherExpr)
                         else
                           Validated.fromEither(Expr.splice(eitherExpr)).leftMap(cats.data.NonEmptyList.one(_))): Any
                      }
                    }
                }
                result <- dctx.getHelper[A].flatMap {
                  case Some(helperCall) =>
                    implicit val EitherDFA: Type[Either[DecodingFailure, A]] = DTypes.DecoderResult[A]
                    MIO.pure(Rule.matched(Expr.quote {
                      Expr
                        .splice(helperCall(dctx.cursor, dctx.config, dctx.failFast))
                        .asInstanceOf[Either[DecodingFailure, A]]
                    }))
                  case None =>
                    MIO.pure(Rule.yielded(s"Failed to build helper for ${Type[A].prettyPrint}"))
                }
              } yield result
            case None =>
              MIO.pure(Rule.yielded(s"Named tuple ${Type[A].prettyPrint} has no primary constructor"))
          }
      }

    @scala.annotation.nowarn("msg=is never used|unused explicit parameter")
    private def decodeNamedTupleFields[A: DecoderCtx](
        constructor: Method.NoInstance[A]
    ): MIO[Expr[Either[DecodingFailure, A]]] = {
      implicit val StringT: Type[String] = DTypes.String
      implicit val HCursorT: Type[HCursor] = DTypes.HCursor
      implicit val DecodingFailureT: Type[DecodingFailure] = DTypes.DecodingFailure
      implicit val AnyT: Type[Any] = DTypes.Any
      implicit val EitherDFAnyT: Type[Either[DecodingFailure, Any]] = DTypes.EitherDFAny
      implicit val ArrayAnyT: Type[Array[Any]] = DTypes.ArrayAny
      implicit val ListEitherT: Type[List[Either[DecodingFailure, Any]]] = DTypes.ListEitherDFAny

      val fieldsList = constructor.parameters.flatten.toList

      NonEmptyList.fromList(fieldsList) match {
        case None =>
          // Empty named tuple: return Right(empty tuple)
          constructor(Map.empty) match {
            case Right(constructExpr) =>
              MIO.pure(Expr.quote {
                Right(Expr.splice(constructExpr)): Either[DecodingFailure, A]
              })
            case Left(error) =>
              val err =
                DecoderDerivationError.CannotConstructType(Type[A].prettyPrint, isSingleton = false, Some(error))
              Log.error(err.message) >> MIO.fail(err)
          }

        case Some(fields) =>
          // Step 1: For each field, derive a decoder and build decode + accessor expressions
          fields
            .parTraverse { case (fName, param) =>
              import param.tpe.Underlying as Field
              Log.namedScope(s"Deriving decoder for named tuple field $fName: ${Type[Field].prettyPrint}") {
                deriveFieldDecoder[Field].map { decoderExpr =>
                  val decodeExpr: Expr[Either[DecodingFailure, Any]] = Expr.quote {
                    Expr
                      .splice(dctx.cursor)
                      .downField(
                        Expr.splice(dctx.config).transformMemberNames(Expr.splice(Expr(fName)))
                      )
                      .as(Expr.splice(decoderExpr))
                      .asInstanceOf[Either[DecodingFailure, Any]]
                  }

                  val makeAccessor: Expr[Array[Any]] => (String, Expr_??) = { arrExpr =>
                    val typedExpr = Expr.quote {
                      CirceDerivationUtils.unsafeCast(
                        Expr.splice(arrExpr)(Expr.splice(Expr(param.index))),
                        Expr.splice(decoderExpr)
                      )
                    }
                    (fName, typedExpr.as_??)
                  }
                  (decodeExpr, makeAccessor)
                }
              }
            }
            .flatMap { fieldData =>
              val decodeExprs = fieldData.toList.map(_._1)
              val makeAccessors = fieldData.toList.map(_._2)

              // Step 2: Build List literal from the decode expressions
              val listExpr: Expr[List[Either[DecodingFailure, Any]]] =
                decodeExprs.foldRight(Expr.quote(List.empty[Either[DecodingFailure, Any]])) { (elem, acc) =>
                  Expr.quote(Expr.splice(elem) :: Expr.splice(acc))
                }

              // Step 3: Build the constructor lambda using LambdaBuilder + primaryConstructor
              LambdaBuilder
                .of1[Array[Any]]("decodedValues")
                .traverse { decodedValuesExpr =>
                  val fieldMap: Map[String, Expr_??] =
                    makeAccessors.map(_(decodedValuesExpr)).toMap
                  constructor(fieldMap) match {
                    case Right(constructExpr) => MIO.pure(constructExpr)
                    case Left(error)          =>
                      val err = DecoderDerivationError.CannotConstructType(
                        Type[A].prettyPrint,
                        isSingleton = false,
                        Some(error)
                      )
                      Log.error(err.message) >> MIO.fail(err)
                  }
                }
                .map { builder =>
                  val constructLambda = builder.build[A]
                  Expr.quote {
                    CirceDerivationUtils.sequenceDecodeResults(Expr.splice(listExpr)).map { arr =>
                      Expr.splice(constructLambda).apply(arr)
                    }
                  }
                }
            }
      }
    }
  }

  object DecHandleAsCaseClassRule extends DecoderDerivationRule("handle as case class when possible") {

    def apply[A: DecoderCtx]: MIO[Rule.Applicability[Expr[Either[DecodingFailure, A]]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as a case class") >> {
        CaseClass.parse[A] match {
          case Some(caseClass) =>
            for {
              _ <- dctx.setHelper[A] { (cursor, config, failFast) =>
                decodeCaseClassFields[A](caseClass)(using dctx.nestInCache(cursor, config, failFast))
              }
              result <- dctx.getHelper[A].flatMap {
                case Some(helperCall) =>
                  implicit val EitherDFA: Type[Either[DecodingFailure, A]] = DTypes.DecoderResult[A]
                  MIO.pure(Rule.matched(Expr.quote {
                    Expr
                      .splice(helperCall(dctx.cursor, dctx.config, dctx.failFast))
                      .asInstanceOf[Either[DecodingFailure, A]]
                  }))
                case None =>
                  MIO.pure(Rule.yielded(s"Failed to build helper for ${Type[A].prettyPrint}"))
              }
            } yield result

          case None =>
            MIO.pure(Rule.yielded(s"The type ${Type[A].prettyPrint} is not a case class"))
        }
      }

    @scala.annotation.nowarn("msg=is never used|unused explicit parameter|Non local returns")
    private def decodeCaseClassFields[A: DecoderCtx](
        caseClass: CaseClass[A]
    ): MIO[Expr[Any]] = {
      implicit val StringT: Type[String] = DTypes.String
      implicit val HCursorT: Type[HCursor] = DTypes.HCursor
      implicit val DecodingFailureT: Type[DecodingFailure] = DTypes.DecodingFailure
      implicit val UnitT: Type[Unit] = DTypes.Unit
      implicit val EitherDFUnitT: Type[Either[DecodingFailure, Unit]] = DTypes.EitherDFUnit
      implicit val SetStringT: Type[Set[String]] = DTypes.SetString
      implicit val ConfigT: Type[Configuration] = DTypes.Configuration
      implicit val BooleanT: Type[Boolean] = DTypes.Boolean
      implicit val fieldNameT: Type[fieldName] = DTypes.FieldName
      implicit val transientFieldT: Type[transientField] = DTypes.TransientField

      // Singletons (case objects, parameterless enum cases) have no primary constructor.
      // Use construct directly, which returns the singleton reference via Expr.singletonOf.
      if (caseClass.isSingleton) {
        return caseClass
          .construct[MIO](new CaseClass.ConstructField[MIO] {
            def apply(field: Parameter): MIO[Expr[field.tpe.Underlying]] = {
              val err = DecoderDerivationError.CannotConstructType(
                Type[A].prettyPrint,
                isSingleton = true,
                Some(s"Unexpected parameter in singleton")
              )
              Log.error(err.message) >> MIO.fail(err)
            }
          })
          .flatMap {
            case Some(expr) =>
              implicit val ValidatedNelDFA: Type[ValidatedNel[DecodingFailure, A]] = DTypes.ValidatedNelDF[A]
              implicit val ValidatedNelDFUnitT: Type[ValidatedNel[DecodingFailure, Unit]] = DTypes.ValidatedNelDFUnit
              MIO.pure(Expr.quote {
                val config = Expr.splice(dctx.config)
                (if (Expr.splice(dctx.failFast)) {
                   if (config.strictDecoding)
                     CirceDerivationUtils
                       .checkStrictDecoding(Expr.splice(dctx.cursor), Set.empty[String])
                       .map(_ => Expr.splice(expr))
                   else
                     Right(Expr.splice(expr)): Either[DecodingFailure, A]
                 } else {
                   if (config.strictDecoding)
                     CirceDerivationUtils
                       .checkStrictDecodingAccumulating(Expr.splice(dctx.cursor), Set.empty[String])
                       .map(_ => Expr.splice(expr))
                   else
                     Validated.valid(Expr.splice(expr)): ValidatedNel[DecodingFailure, A]
                 }): Any
              })
            case None =>
              val err = DecoderDerivationError.CannotConstructType(Type[A].prettyPrint, isSingleton = true)
              Log.error(err.message) >> MIO.fail(err)
          }
      }

      val constructor = caseClass.primaryConstructor
      val fieldsList = constructor.parameters.flatten.toList

      // Validate: @transientField on fields without defaults is a compile error
      fieldsList
        .collectFirst {
          case (name, param) if hasAnnotationType[transientField](param) && !param.hasDefault => name
        }
        .foreach { name =>
          val err = DecoderDerivationError.TransientFieldMissingDefault(name, Type[A].prettyPrint)
          return Log.error(err.message) >> MIO.fail(err)
        }

      // Build a List[String] expression of the field names (accounting for @fieldName overrides)
      // for strict decoding — only non-transient fields
      val fieldNamesListExpr: Expr[List[String]] =
        fieldsList
          .filterNot { case (_, param) => hasAnnotationType[transientField](param) }
          .map { case (name, param) =>
            getAnnotationStringArg[fieldName](param).getOrElse(name)
          }
          .foldRight(Expr.quote(List.empty[String])) { (name, acc) =>
            Expr.quote(Expr.splice(Expr(name)) :: Expr.splice(acc))
          }

      // For strict decoding with @fieldName, the names are already resolved at compile time
      // so we need a version that doesn't apply config.transformMemberNames for those
      val hasAnyFieldNameAnnotation = fieldsList.exists { case (_, param) =>
        getAnnotationStringArg[fieldName](param).isDefined
      }

      NonEmptyList.fromList(fieldsList) match {
        case None =>
          // Zero-parameter case class: construct directly, but check strictDecoding
          caseClass
            .construct[MIO](new CaseClass.ConstructField[MIO] {
              def apply(field: Parameter): MIO[Expr[field.tpe.Underlying]] = {
                val err = DecoderDerivationError.CannotConstructType(
                  Type[A].prettyPrint,
                  isSingleton = false,
                  Some(s"Unexpected parameter in zero-argument case class")
                )
                Log.error(err.message) >> MIO.fail(err)
              }
            })
            .flatMap {
              case Some(expr) =>
                implicit val ValidatedNelDFA: Type[ValidatedNel[DecodingFailure, A]] = DTypes.ValidatedNelDF[A]
                implicit val ValidatedNelDFUnitT: Type[ValidatedNel[DecodingFailure, Unit]] =
                  DTypes.ValidatedNelDFUnit
                MIO.pure(Expr.quote {
                  (if (Expr.splice(dctx.failFast)) {
                     CirceDerivationUtils.checkIsObject(Expr.splice(dctx.cursor)).flatMap { _ =>
                       val config = Expr.splice(dctx.config)
                       if (config.strictDecoding)
                         CirceDerivationUtils
                           .checkStrictDecoding(Expr.splice(dctx.cursor), Set.empty[String])
                           .map(_ => Expr.splice(expr))
                       else
                         Right(Expr.splice(expr)): Either[DecodingFailure, A]
                     }
                   } else {
                     CirceDerivationUtils
                       .checkIsObjectAccumulating(Expr.splice(dctx.cursor))
                       .andThen { _ =>
                         if (Expr.splice(dctx.config).strictDecoding)
                           CirceDerivationUtils
                             .checkStrictDecodingAccumulating(Expr.splice(dctx.cursor), Set.empty[String])
                             .map(_ => Expr.splice(expr))
                         else
                           Validated.valid(Expr.splice(expr)): ValidatedNel[DecodingFailure, A]
                       }
                   }): Any
                })
              case None =>
                val err = DecoderDerivationError.CannotConstructType(Type[A].prettyPrint, isSingleton = false)
                Log.error(err.message) >> MIO.fail(err)
            }

        case Some(fields) =>
          implicit val AnyT: Type[Any] = DTypes.Any
          implicit val EitherDFAnyT: Type[Either[DecodingFailure, Any]] = DTypes.EitherDFAny
          implicit val ArrayAnyT: Type[Array[Any]] = DTypes.ArrayAny
          implicit val ListEitherT: Type[List[Either[DecodingFailure, Any]]] = DTypes.ListEitherDFAny
          implicit val ValidatedNelDFAnyT: Type[ValidatedNel[DecodingFailure, Any]] = DTypes.ValidatedNelDFAny
          implicit val ListValidatedNelT: Type[List[ValidatedNel[DecodingFailure, Any]]] = DTypes.ListValidatedNelDFAny
          implicit val ValidatedNelDFArrayAnyT: Type[ValidatedNel[DecodingFailure, Array[Any]]] =
            DTypes.ValidatedNelDFArrayAny

          // Step 1: For each field, derive a decoder and build BOTH fail-fast and accumulating
          // decode expressions, plus accessor. The failFast branching is done at the wrapping level.
          fields
            .parTraverse { case (fName, param) =>
              import param.tpe.Underlying as Field
              val isTransient = hasAnnotationType[transientField](param)
              val nameOverride = getAnnotationStringArg[fieldName](param)
              Log.namedScope(s"Deriving decoder for field $fName: ${Type[Field].prettyPrint}") {
                deriveFieldDecoder[Field].map { decoderExpr =>
                  val defaultAsAnyOpt: Option[Expr[Any]] =
                    if (param.hasDefault)
                      param.defaultValue.flatMap { existentialOuter =>
                        val methodOf = existentialOuter.value
                        methodOf.value match {
                          case noInstance: Method.NoInstance[?] =>
                            import noInstance.Returned
                            noInstance(Map.empty).toOption.map(_.upcast[Any])
                          case _ => None
                        }
                      }
                    else None

                  // --- Fail-fast decode expression ---
                  val ffExpr: Expr[Either[DecodingFailure, Any]] =
                    if (isTransient) {
                      defaultAsAnyOpt match {
                        case Some(d) => Expr.quote(Right(Expr.splice(d)): Either[DecodingFailure, Any])
                        case None    => Expr.quote(Right(null): Either[DecodingFailure, Any])
                      }
                    } else {
                      nameOverride match {
                        case Some(customName) =>
                          defaultAsAnyOpt match {
                            case Some(defaultAnyExpr) =>
                              Expr.quote {
                                val config = Expr.splice(dctx.config)
                                if (config.useDefaults) {
                                  val field = Expr.splice(dctx.cursor).downField(Expr.splice(Expr(customName)))
                                  if (field.failed)
                                    Right(Expr.splice(defaultAnyExpr)): Either[DecodingFailure, Any]
                                  else
                                    field.as(Expr.splice(decoderExpr)).asInstanceOf[Either[DecodingFailure, Any]]
                                } else
                                  Expr
                                    .splice(dctx.cursor)
                                    .downField(Expr.splice(Expr(customName)))
                                    .as(Expr.splice(decoderExpr))
                                    .asInstanceOf[Either[DecodingFailure, Any]]
                              }
                            case None =>
                              Expr.quote {
                                Expr
                                  .splice(dctx.cursor)
                                  .downField(Expr.splice(Expr(customName)))
                                  .as(Expr.splice(decoderExpr))
                                  .asInstanceOf[Either[DecodingFailure, Any]]
                              }
                          }
                        case None =>
                          defaultAsAnyOpt match {
                            case Some(defaultAnyExpr) =>
                              Expr.quote {
                                val config = Expr.splice(dctx.config)
                                val fn = config.transformMemberNames(Expr.splice(Expr(fName)))
                                if (config.useDefaults) {
                                  val field = Expr.splice(dctx.cursor).downField(fn)
                                  if (field.failed)
                                    Right(Expr.splice(defaultAnyExpr)): Either[DecodingFailure, Any]
                                  else
                                    field.as(Expr.splice(decoderExpr)).asInstanceOf[Either[DecodingFailure, Any]]
                                } else
                                  Expr
                                    .splice(dctx.cursor)
                                    .downField(fn)
                                    .as(Expr.splice(decoderExpr))
                                    .asInstanceOf[Either[DecodingFailure, Any]]
                              }
                            case None =>
                              Expr.quote {
                                Expr
                                  .splice(dctx.cursor)
                                  .downField(Expr.splice(dctx.config).transformMemberNames(Expr.splice(Expr(fName))))
                                  .as(Expr.splice(decoderExpr))
                                  .asInstanceOf[Either[DecodingFailure, Any]]
                              }
                          }
                      }
                    }

                  // --- Accumulating decode expression ---
                  val accExpr: Expr[ValidatedNel[DecodingFailure, Any]] =
                    if (isTransient) {
                      defaultAsAnyOpt match {
                        case Some(d) =>
                          Expr.quote(Validated.valid(Expr.splice(d)): ValidatedNel[DecodingFailure, Any])
                        case None =>
                          Expr.quote(Validated.valid(null): ValidatedNel[DecodingFailure, Any])
                      }
                    } else {
                      nameOverride match {
                        case Some(customName) =>
                          defaultAsAnyOpt match {
                            case Some(defaultAnyExpr) =>
                              Expr.quote {
                                val config = Expr.splice(dctx.config)
                                if (config.useDefaults)
                                  CirceDerivationUtils.decodeFieldWithDefaultAccumulating(
                                    Expr.splice(dctx.cursor),
                                    Expr.splice(Expr(customName)),
                                    Expr.splice(decoderExpr),
                                    Expr.splice(defaultAnyExpr)
                                  )
                                else
                                  Expr
                                    .splice(decoderExpr)
                                    .tryDecodeAccumulating(
                                      Expr.splice(dctx.cursor).downField(Expr.splice(Expr(customName)))
                                    )
                                    .map(x => x: Any)
                              }
                            case None =>
                              Expr.quote {
                                Expr
                                  .splice(decoderExpr)
                                  .tryDecodeAccumulating(
                                    Expr.splice(dctx.cursor).downField(Expr.splice(Expr(customName)))
                                  )
                                  .map(x => x: Any)
                              }
                          }
                        case None =>
                          defaultAsAnyOpt match {
                            case Some(defaultAnyExpr) =>
                              Expr.quote {
                                val config = Expr.splice(dctx.config)
                                val fn = config.transformMemberNames(Expr.splice(Expr(fName)))
                                if (config.useDefaults)
                                  CirceDerivationUtils.decodeFieldWithDefaultAccumulating(
                                    Expr.splice(dctx.cursor),
                                    fn,
                                    Expr.splice(decoderExpr),
                                    Expr.splice(defaultAnyExpr)
                                  )
                                else
                                  Expr
                                    .splice(decoderExpr)
                                    .tryDecodeAccumulating(Expr.splice(dctx.cursor).downField(fn))
                                    .map(x => x: Any)
                              }
                            case None =>
                              Expr.quote {
                                Expr
                                  .splice(decoderExpr)
                                  .tryDecodeAccumulating(
                                    Expr
                                      .splice(dctx.cursor)
                                      .downField(
                                        Expr.splice(dctx.config).transformMemberNames(Expr.splice(Expr(fName)))
                                      )
                                  )
                                  .map(x => x: Any)
                              }
                          }
                      }
                    }

                  val makeAccessor: Expr[Array[Any]] => (String, Expr_??) = { arrExpr =>
                    val typedExpr = Expr.quote {
                      CirceDerivationUtils.unsafeCast(
                        Expr.splice(arrExpr)(Expr.splice(Expr(param.index))),
                        Expr.splice(decoderExpr)
                      )
                    }
                    (fName, typedExpr.as_??)
                  }
                  (ffExpr, accExpr, makeAccessor)
                }
              }
            }
            .flatMap { fieldData =>
              val ffExprs = fieldData.toList.map(_._1)
              val accExprs = fieldData.toList.map(_._2)
              val makeAccessors = fieldData.toList.map(_._3)

              // Step 2: Build List literals for both paths
              val ffListExpr: Expr[List[Either[DecodingFailure, Any]]] =
                ffExprs.foldRight(Expr.quote(List.empty[Either[DecodingFailure, Any]])) { (elem, acc) =>
                  Expr.quote(Expr.splice(elem) :: Expr.splice(acc))
                }
              val accListExpr: Expr[List[ValidatedNel[DecodingFailure, Any]]] =
                accExprs.foldRight(Expr.quote(List.empty[ValidatedNel[DecodingFailure, Any]])) { (elem, acc) =>
                  Expr.quote(Expr.splice(elem) :: Expr.splice(acc))
                }

              // Step 3: Build the constructor lambda using LambdaBuilder + primaryConstructor
              LambdaBuilder
                .of1[Array[Any]]("decodedValues")
                .traverse { decodedValuesExpr =>
                  val fieldMap: Map[String, Expr_??] =
                    makeAccessors.map(_(decodedValuesExpr)).toMap
                  caseClass.primaryConstructor(fieldMap) match {
                    case Right(constructExpr) => MIO.pure(constructExpr)
                    case Left(error)          =>
                      val err = DecoderDerivationError.CannotConstructType(
                        Type[A].prettyPrint,
                        isSingleton = false,
                        Some(error)
                      )
                      Log.error(err.message) >> MIO.fail(err)
                  }
                }
                .map { builder =>
                  val constructLambda = builder.build[A]
                  // Step 4: Wrap with if(failFast) branching and strictDecoding check
                  val resolvedFieldNames: Option[Expr[Set[String]]] =
                    if (hasAnyFieldNameAnnotation) {
                      val nameExprs = fieldsList
                        .filterNot { case (_, p) => hasAnnotationType[transientField](p) }
                        .map { case (name, p) =>
                          getAnnotationStringArg[fieldName](p) match {
                            case Some(custom) => (custom, true)
                            case None         => (name, false)
                          }
                        }
                      val resolvedList = nameExprs.foldRight(Expr.quote(List.empty[String])) {
                        case ((name, true), acc) =>
                          Expr.quote(Expr.splice(Expr(name)) :: Expr.splice(acc))
                        case ((name, false), acc) =>
                          Expr.quote {
                            Expr.splice(dctx.config).transformMemberNames(Expr.splice(Expr(name))) ::
                              Expr.splice(acc)
                          }
                      }
                      Some(Expr.quote(Expr.splice(resolvedList).toSet))
                    } else None

                  Expr.quote {
                    val config = Expr.splice(dctx.config)
                    (if (Expr.splice(dctx.failFast)) {
                       // --- Fail-fast path ---
                       val decoded = CirceDerivationUtils
                         .sequenceDecodeResults(Expr.splice(ffListExpr))
                         .map(Expr.splice(constructLambda))
                       if (config.strictDecoding) {
                         val expectedFields = Expr.splice(
                           resolvedFieldNames.getOrElse(
                             Expr.quote {
                               Expr.splice(fieldNamesListExpr).map(config.transformMemberNames).toSet
                             }
                           )
                         )
                         CirceDerivationUtils
                           .checkStrictDecoding(Expr.splice(dctx.cursor), expectedFields)
                           .flatMap(_ => decoded)
                       } else decoded
                     } else {
                       // --- Accumulating path ---
                       val decoded = CirceDerivationUtils
                         .sequenceDecodeResultsAccumulating(Expr.splice(accListExpr))
                         .map(Expr.splice(constructLambda))
                       if (config.strictDecoding) {
                         val expectedFields = Expr.splice(
                           resolvedFieldNames.getOrElse(
                             Expr.quote {
                               Expr.splice(fieldNamesListExpr).map(config.transformMemberNames).toSet
                             }
                           )
                         )
                         CirceDerivationUtils
                           .checkStrictDecodingAccumulating(Expr.splice(dctx.cursor), expectedFields)
                           .andThen(_ => decoded)
                       } else decoded
                     }): Any
                  }
                }
            }
      }
    }

  }

  /** Derive a Decoder[Field] for a case class field. Tries implicit summoning first, falls back to recursive derivation
    * via the full rule chain.
    */
  @scala.annotation.nowarn("msg=is never used|unused explicit parameter")
  private def deriveFieldDecoder[Field: Type](implicit ctx: DecoderCtx[?]): MIO[Expr[Decoder[Field]]] = {
    implicit val HCursorT: Type[HCursor] = DTypes.HCursor
    implicit val EitherDFField: Type[Either[DecodingFailure, Field]] = DTypes.DecoderResult[Field]

    DTypes.Decoder[Field].summonExprIgnoring(DecUseImplicitWhenAvailableRule.ignoredImplicits*).toEither match {
      case Right(decoderExpr) =>
        Log.info(s"Found implicit Decoder[${Type[Field].prettyPrint}]") >> MIO.pure(decoderExpr)
      case Left(_) =>
        Log.info(s"Building Decoder[${Type[Field].prettyPrint}] via recursive derivation") >>
          LambdaBuilder
            .of1[HCursor]("fieldCursor")
            .traverse { fieldCursorExpr =>
              deriveDecoderRecursively[Field](using ctx.nest[Field](fieldCursorExpr))
            }
            .map { builder =>
              val decodeFn = builder.build[Either[DecodingFailure, Field]]
              Expr.quote(CirceDerivationUtils.decoderFromFn(Expr.splice(decodeFn)))
            }
    }
  }

  object DecHandleAsEnumRule extends DecoderDerivationRule("handle as enum when possible") {

    def apply[A: DecoderCtx]: MIO[Rule.Applicability[Expr[Either[DecodingFailure, A]]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as an enum") >> {
        Enum.parse[A] match {
          case Some(enumm) =>
            for {
              _ <- dctx.setHelper[A] { (cursor, config, failFast) =>
                decodeEnumCases[A](enumm)(using dctx.nestInCache(cursor, config, failFast))
              }
              result <- dctx.getHelper[A].flatMap {
                case Some(helperCall) =>
                  implicit val EitherDFA: Type[Either[DecodingFailure, A]] = DTypes.DecoderResult[A]
                  MIO.pure(Rule.matched(Expr.quote {
                    Expr
                      .splice(helperCall(dctx.cursor, dctx.config, dctx.failFast))
                      .asInstanceOf[Either[DecodingFailure, A]]
                  }))
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
    ): MIO[Expr[Any]] = {
      implicit val HCursorT: Type[HCursor] = DTypes.HCursor
      implicit val DecodingFailureT: Type[DecodingFailure] = DTypes.DecodingFailure
      implicit val StringT: Type[String] = DTypes.String
      implicit val ListStringT: Type[List[String]] = DTypes.ListString
      implicit val TupleT: Type[(String, HCursor)] = DTypes.StringHCursorTuple
      implicit val EitherDFA: Type[Either[DecodingFailure, A]] = DTypes.DecoderResult[A]
      implicit val AnyT: Type[Any] = DTypes.Any
      implicit val BooleanT: Type[Boolean] = DTypes.Boolean
      implicit val ValidatedNelDFA: Type[ValidatedNel[DecodingFailure, A]] = DTypes.ValidatedNelDF[A]

      val childrenList = enumm.directChildren.toList

      // Check at compile time if all children are singletons (case objects with no fields)
      val allCaseObjects = Type[A].isEnumeration || Type[A].isJavaEnum ||
        childrenList.forall { case (_, child) =>
          Type.isVal(using child.Underlying) ||
          CaseClass.parse(using child.Underlying).exists(_.primaryConstructor.parameters.flatten.isEmpty)
        }

      NonEmptyList.fromList(childrenList) match {
        case None =>
          MIO.pure(Expr.quote {
            val err = DecodingFailure(
              s"Enum ${Expr.splice(Expr(Type[A].prettyPrint))} has no subtypes",
              Expr.splice(dctx.cursor).history
            )
            (if (Expr.splice(dctx.failFast))
               Left(err): Either[DecodingFailure, A]
             else
               Validated.invalidNel(err): ValidatedNel[DecodingFailure, A]): Any
          })

        case Some(children) =>
          val knownNames: List[String] = children.toList.map(_._1)

          // For each child, derive a decoder and produce a dispatch function
          // that takes (typeNameExpr, innerCursorExpr, elseExpr) and returns Any
          // (Either when failFast=true, ValidatedNel when failFast=false)
          children
            .parTraverse { case (childName, child) =>
              import child.Underlying as ChildType
              Log.namedScope(s"Deriving decoder for enum case $childName: ${Type[ChildType].prettyPrint}") {
                deriveChildDecoder[A, ChildType](childName)
              }
            }
            .flatMap { childDispatchers =>
              // Build a dispatch lambda: (String, HCursor) => Any
              LambdaBuilder
                .of1[(String, HCursor)]("readResult")
                .traverse { readResultExpr =>
                  // Extract typeName and innerCursor from the tuple
                  val typeNameExpr: Expr[String] = Expr.quote(Expr.splice(readResultExpr)._1)
                  val innerCursorExpr: Expr[HCursor] = Expr.quote(Expr.splice(readResultExpr)._2)

                  // Build the if-else dispatch chain (foldRight to get correct order)
                  val errorExpr: Expr[Any] = Expr.quote {
                    val failure = CirceDerivationUtils.failedToMatchSubtype(
                      Expr.splice(typeNameExpr),
                      Expr.splice(innerCursorExpr),
                      Expr.splice(Expr(knownNames))
                    )
                    (if (Expr.splice(dctx.failFast))
                       Left(failure): Either[DecodingFailure, A]
                     else
                       Validated.invalidNel(failure): ValidatedNel[DecodingFailure, A]): Any
                  }

                  MIO.pure(childDispatchers.toList.foldRight(errorExpr) { case (dispatcher, elseExpr) =>
                    dispatcher(typeNameExpr, innerCursorExpr, elseExpr)
                  })
                }
                .map { builder =>
                  val dispatchFn = builder.build[Any]
                  Expr.quote {
                    val config = Expr.splice(dctx.config)
                    val cursor = Expr.splice(dctx.cursor)
                    val failFast = Expr.splice(dctx.failFast)
                    if (Expr.splice(Expr(allCaseObjects)) && config.enumAsStrings) {
                      // String enum decode path: read plain string, dispatch on name
                      cursor.as[String](io.circe.Decoder.decodeString) match {
                        case Right(typeName) =>
                          Expr.splice(dispatchFn)((typeName, cursor))
                        case Left(_) =>
                          val err = DecodingFailure("Expected a JSON string for enum value", cursor.history)
                          (if (failFast)
                             Left(err): Either[DecodingFailure, A]
                           else
                             Validated.invalidNel(err): ValidatedNel[DecodingFailure, A]): Any
                      }
                    } else {
                      val readResult: Either[DecodingFailure, (String, HCursor)] =
                        config.discriminator match {
                          case Some(field) => CirceDerivationUtils.decodeDiscriminator(cursor, field)
                          case None        => CirceDerivationUtils.decodeWrapped(cursor)
                        }
                      if (failFast) {
                        readResult.flatMap { r =>
                          Expr.splice(dispatchFn)(r).asInstanceOf[Either[DecodingFailure, A]]
                        }: Any
                      } else {
                        (readResult match {
                          case Right(r) => Expr.splice(dispatchFn)(r)
                          case Left(e)  => Validated.invalidNel(e): ValidatedNel[DecodingFailure, A]
                        }): Any
                      }
                    }
                  }
                }
            }
      }
    }

    /** Derives a decoder for a single enum child type and returns a dispatch function. The dispatch function takes
      * (typeNameExpr, innerCursorExpr, elseExpr) and produces an if-else expression that checks if the type name
      * matches and decodes accordingly. Returns Any (Either when failFast=true, ValidatedNel when failFast=false).
      */
    @scala.annotation.nowarn("msg=is never used|unused explicit parameter")
    private def deriveChildDecoder[A: DecoderCtx, ChildType: Type](
        childName: String
    ): MIO[(Expr[String], Expr[HCursor], Expr[Any]) => Expr[Any]] = {
      implicit val HCursorT: Type[HCursor] = DTypes.HCursor
      implicit val DecodingFailureT: Type[DecodingFailure] = DTypes.DecodingFailure
      implicit val StringT: Type[String] = DTypes.String
      implicit val AnyT: Type[Any] = DTypes.Any
      implicit val BooleanT: Type[Boolean] = DTypes.Boolean
      implicit val EitherDFA: Type[Either[DecodingFailure, A]] = DTypes.DecoderResult[A]
      implicit val ValidatedNelDFA: Type[ValidatedNel[DecodingFailure, A]] = DTypes.ValidatedNelDF[A]

      // Try to summon implicit Decoder[ChildType] first
      DTypes
        .Decoder[ChildType]
        .summonExprIgnoring(DecUseImplicitWhenAvailableRule.ignoredImplicits*)
        .toEither match {
        case Right(decoderExpr) =>
          Log.info(s"Found implicit Decoder[$childName], using it") >>
            MIO.pure { (typeNameExpr: Expr[String], innerCursorExpr: Expr[HCursor], elseExpr: Expr[Any]) =>
              Expr.quote {
                if (
                  Expr.splice(dctx.config).transformConstructorNames(Expr.splice(Expr(childName))) == Expr
                    .splice(typeNameExpr)
                )
                  (if (Expr.splice(dctx.failFast))
                     Expr
                       .splice(decoderExpr)
                       .apply(Expr.splice(innerCursorExpr))
                       .asInstanceOf[Either[DecodingFailure, A]]
                   else
                     (Expr
                       .splice(decoderExpr)
                       .decodeAccumulating(Expr.splice(innerCursorExpr)): Any)
                       .asInstanceOf[ValidatedNel[DecodingFailure, A]]): Any
                else
                  Expr.splice(elseExpr)
              }
            }

        case Left(_) =>
          // Try singletonOf first — handles Enumeration values, Java enum values, case objects
          Expr.singletonOf[ChildType] match {
            case Some(singleton) =>
              Log.info(s"Using singleton for $childName") >>
                MIO.pure { (typeNameExpr: Expr[String], _: Expr[HCursor], elseExpr: Expr[Any]) =>
                  Expr.quote {
                    if (
                      Expr.splice(dctx.config).transformConstructorNames(Expr.splice(Expr(childName))) == Expr
                        .splice(typeNameExpr)
                    )
                      (if (Expr.splice(dctx.failFast))
                         Right(Expr.splice(singleton).asInstanceOf[A]): Either[DecodingFailure, A]
                       else
                         Validated.valid(Expr.splice(singleton).asInstanceOf[A]): ValidatedNel[DecodingFailure, A]): Any
                    else
                      Expr.splice(elseExpr)
                  }
                }
            case None =>
              // No singleton - derive via full rules chain (this sets up a helper in cache)
              deriveDecoderRecursively[ChildType](using dctx.nest[ChildType](dctx.cursor)).flatMap { _ =>
                dctx.getHelper[ChildType].map {
                  case Some(helper) =>
                    (typeNameExpr: Expr[String], innerCursorExpr: Expr[HCursor], elseExpr: Expr[Any]) => {
                      val helperCallExpr = helper(innerCursorExpr, dctx.config, dctx.failFast)
                      Expr.quote {
                        if (
                          Expr.splice(dctx.config).transformConstructorNames(Expr.splice(Expr(childName))) == Expr
                            .splice(typeNameExpr)
                        )
                          Expr.splice(helperCallExpr) // already returns Any (Either or ValidatedNel based on failFast)
                        else
                          Expr.splice(elseExpr)
                      }
                    }

                  case None =>
                    // No helper - the child was handled by implicit or value type rule
                    // This shouldn't normally happen since we checked implicit above
                    (_: Expr[String], _: Expr[HCursor], elseExpr: Expr[Any]) => elseExpr
                }
              }
          }
      }
    }
  }

  // Types

  private[compiletime] object DTypes {

    def Decoder: Type.Ctor1[Decoder] = Type.Ctor1.of[Decoder]
    def KeyDecoder: Type.Ctor1[KeyDecoder] = Type.Ctor1.of[KeyDecoder]
    def KindlingsDecoder: Type.Ctor1[KindlingsDecoder] = Type.Ctor1.of[KindlingsDecoder]
    val DecoderLogDerivation: Type[hearth.kindlings.circederivation.KindlingsDecoder.LogDerivation] =
      Type.of[hearth.kindlings.circederivation.KindlingsDecoder.LogDerivation]
    val Json: Type[Json] = Type.of[Json]
    val HCursor: Type[HCursor] = Type.of[HCursor]
    val DecodingFailure: Type[DecodingFailure] = Type.of[DecodingFailure]
    val Configuration: Type[Configuration] = Type.of[Configuration]
    val String: Type[String] = Type.of[String]
    val Boolean: Type[Boolean] = Type.of[Boolean]
    val Any: Type[Any] = Type.of[Any]
    val Unit: Type[Unit] = Type.of[Unit]
    val ArrayAny: Type[Array[Any]] = Type.of[Array[Any]]
    val EitherDFAny: Type[Either[DecodingFailure, Any]] = Type.of[Either[DecodingFailure, Any]]
    val EitherDFUnit: Type[Either[DecodingFailure, Unit]] = Type.of[Either[DecodingFailure, Unit]]
    val ListEitherDFAny: Type[List[Either[DecodingFailure, Any]]] =
      Type.of[List[Either[DecodingFailure, Any]]]
    val ListString: Type[List[String]] = Type.of[List[String]]
    val SetString: Type[Set[String]] = Type.of[Set[String]]
    val StringHCursorTuple: Type[(String, HCursor)] = Type.of[(String, HCursor)]
    val FieldName: Type[fieldName] = Type.of[fieldName]
    val TransientField: Type[transientField] = Type.of[transientField]

    def DecoderResult[A: Type]: Type[Either[DecodingFailure, A]] =
      Type.of[Either[DecodingFailure, A]]

    // Accumulating types
    def ValidatedNelDF[A: Type]: Type[ValidatedNel[DecodingFailure, A]] =
      Type.of[ValidatedNel[DecodingFailure, A]]
    val ValidatedNelDFAny: Type[ValidatedNel[DecodingFailure, Any]] =
      Type.of[ValidatedNel[DecodingFailure, Any]]
    val ValidatedNelDFUnit: Type[ValidatedNel[DecodingFailure, Unit]] =
      Type.of[ValidatedNel[DecodingFailure, Unit]]
    val ValidatedNelDFArrayAny: Type[ValidatedNel[DecodingFailure, Array[Any]]] =
      Type.of[ValidatedNel[DecodingFailure, Array[Any]]]
    val ListValidatedNelDFAny: Type[List[ValidatedNel[DecodingFailure, Any]]] =
      Type.of[List[ValidatedNel[DecodingFailure, Any]]]
  }
}

sealed private[compiletime] trait DecoderDerivationError
    extends util.control.NoStackTrace
    with Product
    with Serializable {
  def message: String
  override def getMessage(): String = message
}
private[compiletime] object DecoderDerivationError {
  final case class UnsupportedType(tpeName: String, reasons: List[String]) extends DecoderDerivationError {
    override def message: String =
      s"The type $tpeName was not handled by any decoder derivation rule:\n${reasons.mkString("\n")}"
  }
  final case class TransientFieldMissingDefault(fieldName: String, tpeName: String) extends DecoderDerivationError {
    override def message: String =
      s"@transientField on field '$fieldName' of $tpeName requires a default value"
  }
  final case class CannotConstructType(tpeName: String, isSingleton: Boolean, constructorError: Option[String] = None)
      extends DecoderDerivationError {
    override def message: String = {
      val prefix =
        if (isSingleton) s"Cannot construct singleton $tpeName" else s"Cannot construct $tpeName"
      constructorError.fold(prefix)(err => s"$prefix: $err")
    }
  }
}
