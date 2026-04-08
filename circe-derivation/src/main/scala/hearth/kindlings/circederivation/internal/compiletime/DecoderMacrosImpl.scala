package hearth.kindlings.circederivation.internal.compiletime

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.fp.syntax.*
import hearth.std.*

import hearth.kindlings.circederivation.{Configuration, KindlingsDecoder}
import hearth.kindlings.circederivation.annotations.{fieldName, transientField}
import hearth.kindlings.circederivation.internal.runtime.CirceDerivationUtils
import cats.data.ValidatedNel
import io.circe.{Decoder, DecodingFailure, HCursor, Json, KeyDecoder}

trait DecoderMacrosImpl
    extends rules.DecoderUseCachedDefWhenAvailableRuleImpl
    with rules.DecoderUseImplicitWhenAvailableRuleImpl
    with rules.DecoderHandleAsLiteralTypeRuleImpl
    with rules.DecoderHandleAsValueTypeRuleImpl
    with rules.DecoderHandleAsOptionRuleImpl
    with rules.DecoderHandleAsMapRuleImpl
    with rules.DecoderHandleAsCollectionRuleImpl
    with rules.DecoderHandleAsNamedTupleRuleImpl
    with rules.DecoderHandleAsSingletonRuleImpl
    with rules.DecoderHandleAsCaseClassRuleImpl
    with rules.DecoderHandleAsEnumRuleImpl {
  this: MacroCommons & StdExtensions & AnnotationSupport & LoadStandardExtensionsOnce =>

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
              _ <- ensureStandardExtensionsLoaded()
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
                      _ <- ensureStandardExtensionsLoaded()
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
                  _ <- ensureStandardExtensionsLoaded()
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
          DecoderUseCachedDefWhenAvailableRule,
          DecoderHandleAsLiteralTypeRule,
          DecoderUseImplicitWhenAvailableRule,
          DecoderHandleAsValueTypeRule,
          DecoderHandleAsOptionRule,
          DecoderHandleAsMapRule,
          DecoderHandleAsCollectionRule,
          DecoderHandleAsNamedTupleRule,
          DecoderHandleAsSingletonRule,
          DecoderHandleAsCaseClassRule,
          DecoderHandleAsEnumRule
        )(_[A]).flatMap {
          case Right(result) =>
            Log.info(s"Derived decoder for ${Type[A].prettyPrint}: ${result.prettyPrint}") >>
              MIO.pure(result)
          case Left(reasons) =>
            val reasonsStrings = reasons.toListMap
              .removed(DecoderUseCachedDefWhenAvailableRule)
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

  /** Derive a Decoder[Field] for a case class field. Tries implicit summoning first, falls back to recursive derivation
    * via the full rule chain.
    */
  @scala.annotation.nowarn("msg=is never used|unused explicit parameter")
  protected def deriveFieldDecoder[Field: Type](implicit ctx: DecoderCtx[?]): MIO[Expr[Decoder[Field]]] = {
    implicit val HCursorT: Type[HCursor] = DTypes.HCursor
    implicit val EitherDFField: Type[Either[DecodingFailure, Field]] = DTypes.DecoderResult[Field]

    DTypes
      .Decoder[Field]
      .summonExprIgnoring(DecoderUseImplicitWhenAvailableRule.ignoredImplicits*)
      .toEither match {
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
    val Int: Type[Int] = Type.of[Int]
    val Long: Type[Long] = Type.of[Long]
    val Double: Type[Double] = Type.of[Double]
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
