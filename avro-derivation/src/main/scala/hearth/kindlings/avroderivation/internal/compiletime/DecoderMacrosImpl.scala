package hearth.kindlings.avroderivation.internal.compiletime

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

import hearth.kindlings.avroderivation.{AvroConfig, AvroDecoder, DecimalConfig}
import hearth.kindlings.avroderivation.annotations.{avroFixed, fieldName, transientField}
import org.apache.avro.Schema

trait DecoderMacrosImpl
    extends AvroDerivationTimeout
    with rules.AvroDecoderUseCachedDefWhenAvailableRuleImpl
    with rules.AvroDecoderUseImplicitWhenAvailableRuleImpl
    with rules.AvroDecoderHandleAsLiteralTypeRuleImpl
    with rules.AvroDecoderUseBuiltInSupportRuleImpl
    with rules.AvroDecoderHandleAsValueTypeRuleImpl
    with rules.AvroDecoderHandleAsOptionRuleImpl
    with rules.AvroDecoderHandleAsEitherRuleImpl
    with rules.AvroDecoderHandleAsMapRuleImpl
    with rules.AvroDecoderHandleAsCollectionRuleImpl
    with rules.AvroDecoderHandleAsNamedTupleRuleImpl
    with rules.AvroDecoderHandleAsSingletonRuleImpl
    with rules.AvroDecoderHandleAsCaseClassRuleImpl
    with rules.AvroDecoderHandleAsEnumRuleImpl {
  this: MacroCommons & StdExtensions & SchemaForMacrosImpl & AnnotationSupport & LoadStandardExtensionsOnce =>

  // Entrypoints

  @scala.annotation.nowarn("msg=is never used")
  def deriveInlineDecode[A: Type](
      avroValueExpr: Expr[Any],
      configExpr: Expr[AvroConfig]
  ): Expr[A] = {
    implicit val AnyT: Type[Any] = DecTypes.Any
    implicit val ConfigT: Type[AvroConfig] = DecTypes.AvroConfig

    deriveDecoderFromCtxAndAdaptForEntrypoint[A, A]("AvroDecoder.decode") { fromCtx =>
      ValDefs.createVal[Any](avroValueExpr).use { avroVal =>
        ValDefs.createVal[AvroConfig](configExpr).use { configVal =>
          Expr.quote {
            val _ = Expr.splice(avroVal)
            val _ = Expr.splice(configVal)
            Expr.splice(fromCtx(DecoderCtx.from(avroVal, configVal, derivedType = None)))
          }
        }
      }
    }
  }

  @scala.annotation.nowarn("msg=is never used")
  def deriveDecoderTypeClass[A: Type](configExpr: Expr[AvroConfig]): Expr[AvroDecoder[A]] = {
    implicit val AvroDecoderA: Type[AvroDecoder[A]] = DecTypes.AvroDecoder[A]
    implicit val AnyT: Type[Any] = DecTypes.Any
    implicit val SchemaT: Type[Schema] = DecTypes.Schema
    implicit val ConfigT: Type[AvroConfig] = DecTypes.AvroConfig
    val selfType: Option[??] = Some(Type[A].as_??)

    if (Type[A] =:= Type.of[Nothing].asInstanceOf[Type[A]] || Type[A] =:= Type.of[Any].asInstanceOf[Type[A]])
      Environment.reportErrorAndAbort(
        s"AvroDecoder.derived: type parameter was inferred as ${Type[A].prettyPrint}, which is likely unintended.\n" +
          "Provide an explicit type parameter, e.g.: AvroDecoder.derived[MyType]\n" +
          "or add a type ascription to the result variable."
      )

    // Schema and decoder are derived in the same MIO.scoped block to avoid Scala 3 splice isolation issues.
    Log
      .namedScope(
        s"Deriving decoder for ${Type[A].prettyPrint} at: ${Environment.currentPosition.prettyPrint}"
      ) {
        MIO.scoped { runSafe =>
          // Derive schema with its own cache (self-contained)
          val schemaExpr: Expr[Schema] = runSafe {
            deriveSelfContainedSchema[A](configExpr)
          }

          // Create decoder derivation callback
          val fromCtx: (DecoderCtx[A] => Expr[A]) = (ctx: DecoderCtx[A]) =>
            runSafe {
              for {
                _ <- ensureStandardExtensionsLoaded()
                result <- deriveDecoderRecursively[A](using ctx)
                cache <- ctx.cache.get
              } yield cache.toValDefs.use(_ => result)
            }

          // Assemble the type class instance
          ValDefs.createVal[AvroConfig](configExpr).use { configVal =>
            Expr.quote {
              val cfg = Expr.splice(configVal)
              val sch = Expr.splice(schemaExpr)
              (new AvroDecoder[A] {
                val schema: Schema = sch
                def decode(value: Any): A = {
                  val _ = value
                  val _ = cfg
                  Expr.splice {
                    fromCtx(DecoderCtx.from(Expr.quote(value), Expr.quote(cfg), derivedType = selfType))
                  }
                }
              }): AvroDecoder[A]
            }
          }
        }
      }
      .flatTap { result =>
        Log.info(s"Derived final decoder result: ${result.prettyPrint}")
      }
      .runToExprOrFail(
        "AvroDecoder.derived",
        infoRendering = if (shouldWeLogDecoderDerivation) RenderFrom(Log.Level.Info) else DontRender,
        errorRendering = if (shouldWeLogDecoderDerivation) RenderFrom(Log.Level.Info) else DontRender,
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
          "Enable debug logging with: import hearth.kindlings.avroderivation.debug.logDerivationForAvroDecoder or scalac option -Xmacro-settings:avroDerivation.logDerivation=true"
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
      provideCtxAndAdapt: (DecoderCtx[A] => Expr[A]) => Expr[Out]
  ): Expr[Out] = {
    if (Type[A] =:= Type.of[Nothing].asInstanceOf[Type[A]] || Type[A] =:= Type.of[Any].asInstanceOf[Type[A]])
      Environment.reportErrorAndAbort(
        s"$macroName: type parameter was inferred as ${Type[A].prettyPrint}, which is likely unintended.\n" +
          s"Provide an explicit type parameter, e.g.: $macroName[MyType](...)\n" +
          "or add a type ascription to the result variable."
      )
    Log
      .namedScope(
        s"Deriving decoder for ${Type[A].prettyPrint} at: ${Environment.currentPosition.prettyPrint}"
      ) {
        MIO.scoped { runSafe =>
          val fromCtx: (DecoderCtx[A] => Expr[A]) = (ctx: DecoderCtx[A]) =>
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
        errorRendering = if (shouldWeLogDecoderDerivation) RenderFrom(Log.Level.Info) else DontRender,
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
          "Enable debug logging with: import hearth.kindlings.avroderivation.debug.logDerivationForAvroDecoder or scalac option -Xmacro-settings:avroDerivation.logDerivation=true"
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
    implicit val LogDerivation: Type[AvroDecoder.LogDerivation] = DecTypes.DecoderLogDerivation
    def logDerivationImported = Expr.summonImplicit[AvroDecoder.LogDerivation].isDefined

    def logDerivationSetGlobally = (for {
      data <- Environment.typedSettings.toOption
      avroDerivation <- data.get("avroDerivation")
      shouldLog <- avroDerivation.get("logDerivation").flatMap(_.asBoolean)
    } yield shouldLog).getOrElse(false)

    logDerivationImported || logDerivationSetGlobally
  }

  // Context

  final case class DecoderCtx[A](
      tpe: Type[A],
      avroValue: Expr[Any],
      config: Expr[AvroConfig],
      cache: MLocal[ValDefsCache],
      derivedType: Option[??]
  ) {

    def nest[B: Type](newValue: Expr[Any]): DecoderCtx[B] = copy[B](
      tpe = Type[B],
      avroValue = newValue
    )

    def nestInCache(
        newValue: Expr[Any],
        newConfig: Expr[AvroConfig]
    ): DecoderCtx[A] = copy(
      avroValue = newValue,
      config = newConfig
    )

    def getInstance[B: Type]: MIO[Option[Expr[AvroDecoder[B]]]] = {
      implicit val DecoderB: Type[AvroDecoder[B]] = DecTypes.AvroDecoder[B]
      cache.get0Ary[AvroDecoder[B]]("cached-decoder-instance")
    }
    def setInstance[B: Type](instance: Expr[AvroDecoder[B]]): MIO[Unit] = {
      implicit val DecoderB: Type[AvroDecoder[B]] = DecTypes.AvroDecoder[B]
      Log.info(s"Caching AvroDecoder instance for ${Type[B].prettyPrint}") >>
        cache.buildCachedWith(
          "cached-decoder-instance",
          ValDefBuilder.ofLazy[AvroDecoder[B]](s"decoder_${Type[B].shortName}")
        )(_ => instance)
    }

    def getHelper[B: Type]: MIO[Option[(Expr[Any], Expr[AvroConfig]) => Expr[B]]] = {
      implicit val ConfigT: Type[AvroConfig] = DecTypes.AvroConfig
      implicit val AnyT: Type[Any] = DecTypes.Any
      cache.get2Ary[Any, AvroConfig, B]("cached-decode-method")
    }
    def setHelper[B: Type](
        helper: (Expr[Any], Expr[AvroConfig]) => MIO[Expr[B]]
    ): MIO[Unit] = {
      implicit val ConfigT: Type[AvroConfig] = DecTypes.AvroConfig
      implicit val AnyT: Type[Any] = DecTypes.Any
      val defBuilder =
        ValDefBuilder.ofDef2[Any, AvroConfig, B](s"decode_${Type[B].shortName}")
      for {
        _ <- Log.info(s"Forward-declaring decode helper for ${Type[B].prettyPrint}")
        _ <- cache.forwardDeclare("cached-decode-method", defBuilder)
        _ <- MIO.scoped { runSafe =>
          runSafe(cache.buildCachedWith("cached-decode-method", defBuilder) { case (_, (value, config)) =>
            runSafe(helper(value, config))
          })
        }
        _ <- Log.info(s"Defined decode helper for ${Type[B].prettyPrint}")
      } yield ()
    }

    override def toString: String =
      s"decode[${tpe.prettyPrint}](avroValue = ${avroValue.prettyPrint}, config = ${config.prettyPrint})"
  }
  object DecoderCtx {

    def from[A: Type](
        avroValue: Expr[Any],
        config: Expr[AvroConfig],
        derivedType: Option[??]
    ): DecoderCtx[A] = DecoderCtx(
      tpe = Type[A],
      avroValue = avroValue,
      config = config,
      cache = ValDefsCache.mlocal,
      derivedType = derivedType
    )
  }

  def dctx[A](implicit A: DecoderCtx[A]): DecoderCtx[A] = A

  implicit def currentDecoderValueType[A: DecoderCtx]: Type[A] = dctx.tpe

  abstract class DecoderDerivationRule(val name: String) extends Rule {
    def apply[A: DecoderCtx]: MIO[Rule.Applicability[Expr[A]]]
  }

  // The actual derivation logic

  def deriveDecoderRecursively[A: DecoderCtx]: MIO[Expr[A]] =
    dctx.getHelper[A].flatMap {
      case Some(helperCall) =>
        Log.info(s"Found cached decoder helper for ${Type[A].prettyPrint}") >>
          MIO.pure(helperCall(dctx.avroValue, dctx.config))
      case None =>
        dctx.getInstance[A].flatMap {
          case Some(instance) =>
            Log.info(s"Found cached decoder instance for ${Type[A].prettyPrint}") >>
              MIO.pure(Expr.quote {
                Expr.splice(instance).decode(Expr.splice(dctx.avroValue))
              })
          case None =>
            dctx.setHelper[A] { (value, config) =>
              deriveDecoderRecursivelyViaRules[A](using dctx.nestInCache(value, config))
            } >> dctx.getHelper[A].flatMap {
              case Some(helperCall) => MIO.pure(helperCall(dctx.avroValue, dctx.config))
              case None             =>
                MIO.fail(new Exception(s"Failed to build decoder helper for ${Type[A].prettyPrint}"))
            }
        }
    }

  private def deriveDecoderRecursivelyViaRules[A: DecoderCtx]: MIO[Expr[A]] =
    Log
      .namedScope(s"Deriving decoder for type ${Type[A].prettyPrint}") {
        Rules(
          AvroDecoderHandleAsLiteralTypeRule,
          AvroDecoderUseImplicitWhenAvailableRule,
          AvroDecoderUseBuiltInSupportRule,
          AvroDecoderHandleAsValueTypeRule,
          AvroDecoderHandleAsOptionRule,
          AvroDecoderHandleAsEitherRule,
          AvroDecoderHandleAsMapRule,
          AvroDecoderHandleAsCollectionRule,
          AvroDecoderHandleAsNamedTupleRule,
          AvroDecoderHandleAsSingletonRule,
          AvroDecoderHandleAsCaseClassRule,
          AvroDecoderHandleAsEnumRule
        )(_[A]).flatMap {
          case Right(result) =>
            Log.info(s"Derived decoder for ${Type[A].prettyPrint}: ${result.prettyPrint}") >>
              MIO.pure(result)
          case Left(reasons) =>
            val reasonsStrings = reasons.toListMap
              // .removed(AvroDecoderUseCachedDefWhenAvailableRule)
              .view.map { case (rule, reasons) =>
                if (reasons.isEmpty) s"The rule ${rule.name} was not applicable"
                else
                  s" - The rule ${rule.name} was not applicable, for the following reasons: ${reasons.mkString(", ")}"
              }.toList
            val err = DecoderDerivationError.UnsupportedType(Type[A].prettyPrint, reasonsStrings)
            Log.error(err.message) >> MIO.fail(err)
        }
      }

  // Shared field-level helper used by CaseClass and NamedTuple rules

  /** Derive an [[AvroDecoder]] instance for a single case-class / named-tuple field.
    *
    * Per project rule 5, [[LambdaBuilder]] is reserved for collection / Optional iteration lambdas. This site uses the
    * def-caching pattern instead (see `docs/contributing/def-caching-skill.md`):
    *
    *   1. Drive `deriveDecoderRecursively[Field]` once. That recursive call internally goes through `setHelper`
    *      (defined on [[DecoderCtx]] and called from the rule chain) which forward-declares and builds a
    *      `def decode_Field(value: Any, config: AvroConfig): Field` against the shared [[ValDefsCache]] — exactly the
    *      same `cached-decode-method` entry the case-class rule uses for self-references. No second cache key needed.
    *   2. Retrieve the helper-call function via [[DecoderCtx.getHelper]].
    *   3. Wrap it in a fresh `new AvroDecoder[Field] { def decode(value) = helper(value, ctx.config) }`. The
    *      closed-over `ctx.config` is the call site's config — typically the outer cached body's `config` parameter,
    *      which is in scope at the call site (where the wrapping `AvroDecoder` is spliced) and gets forwarded into the
    *      cached field-decode `def`.
    */
  protected def deriveFieldDecoder[Field: Type](implicit ctx: DecoderCtx[?]): MIO[Expr[AvroDecoder[Field]]] = {
    implicit val AnyT: Type[Any] = DecTypes.Any
    @scala.annotation.nowarn("msg=is never used")
    implicit val AvroConfigT: Type[AvroConfig] = DecTypes.AvroConfig
    @scala.annotation.nowarn("msg=is never used")
    implicit val SchemaT: Type[Schema] = DecTypes.Schema

    DecTypes
      .AvroDecoder[Field]
      .summonExprIgnoring(AvroDecoderUseImplicitWhenAvailableRule.ignoredImplicits*)
      .toEither match {
      case Right(decoderExpr) =>
        Log.info(s"Found implicit AvroDecoder[${Type[Field].prettyPrint}]") >> MIO.pure(decoderExpr)
      case Left(_) =>
        Log.info(s"Building AvroDecoder[${Type[Field].prettyPrint}] via cached def") >> {
          // Placeholder Expr[Any] for the field's value slot — the rule chain (via setHelper)
          // immediately overrides it with the cached def's parameter Exprs inside its body builder.
          val placeholderValue: Expr[Any] = Expr.quote(null.asInstanceOf[Any])
          for {
            // NOTE: kept SEQUENTIAL deliberately. Parallelizing decoder + schema derivation
            // via `parTuple` was attempted but failed Scala 3 parameterized-enum tests —
            // `parTuple` forks `MLocal` state per branch and the schema branch does not
            // observe state populated by the decoder branch (or vice versa) for nested
            // types within the same expansion. The two derivations are not entirely
            // independent: both go through the rule chain and `deriveSelfContainedSchema`
            // shares some Hearth-internal MLocal state with `deriveDecoderRecursively`.
            //
            // For multi-METHOD aggregation (e.g. Hash's hash + eqv), parTuple is correct
            // and useful — see HashMacrosImpl. Here the two operations are aspects of one
            // field handler, not independent methods, so the value of error aggregation is
            // smaller and the breakage is real. Sequential is the safe choice.
            _ <- deriveDecoderRecursively[Field](using ctx.nest[Field](placeholderValue))
            helperOpt <- ctx.getHelper[Field]
            schemaExpr <- deriveSelfContainedSchema[Field](ctx.config)
          } yield {
            val callerFn = helperOpt.get
            val configExpr: Expr[AvroConfig] = ctx.config
            Expr.quote {
              val sch = Expr.splice(schemaExpr)
              new AvroDecoder[Field] {
                val schema: Schema = sch
                def decode(value: Any): Field = {
                  val _ = value
                  Expr.splice(callerFn(Expr.quote(value), configExpr))
                }
              }
            }
          }
        }
    }
  }

  // Types

  private[compiletime] object DecTypes {

    def AvroDecoder: Type.Ctor1[AvroDecoder] = Type.Ctor1.of[AvroDecoder]
    val DecoderLogDerivation: Type[hearth.kindlings.avroderivation.AvroDecoder.LogDerivation] =
      Type.of[hearth.kindlings.avroderivation.AvroDecoder.LogDerivation]
    val Schema: Type[Schema] = Type.of[Schema]
    val AvroConfig: Type[AvroConfig] = Type.of[AvroConfig]
    val DecimalConfig: Type[DecimalConfig] = Type.of[DecimalConfig]
    val String: Type[String] = Type.of[String]
    val Int: Type[Int] = Type.of[Int]
    val Long: Type[Long] = Type.of[Long]
    val Double: Type[Double] = Type.of[Double]
    val Boolean: Type[Boolean] = Type.of[Boolean]
    val Any: Type[Any] = Type.of[Any]
    val ArrayAny: Type[Array[Any]] = Type.of[Array[Any]]
    val ArrayByte: Type[Array[Byte]] = Type.of[Array[Byte]]
    val FieldName: Type[fieldName] = Type.of[fieldName]
    val TransientField: Type[transientField] = Type.of[transientField]
    val AvroFixed: Type[avroFixed] = Type.of[avroFixed]
  }
}
