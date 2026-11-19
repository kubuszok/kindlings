package hearth.kindlings.yamlderivation.internal.compiletime

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.fp.syntax.*
import hearth.std.*

import hearth.kindlings.yamlderivation.{KindlingsYamlDecoder, YamlConfig}
import hearth.kindlings.yamlderivation.annotations.{fieldName, transientField}
import hearth.kindlings.yamlderivation.internal.runtime.YamlDerivationUtils
import org.virtuslab.yaml.{ConstructError, Node, YamlDecoder, YamlError}

trait DecoderMacrosImpl
    extends YamlDerivationTimeout
    with SubtypeNamesSupport
    with YamlDerivationPolicy
    with hearth.kindlings.derivation.compiletime.MethodFolds
    with hearth.kindlings.derivation.compiletime.FlatFieldsConstruct
    with rules.DecoderUseCachedDefWhenAvailableRuleImpl
    with rules.DecoderUseImplicitWhenAvailableRuleImpl
    with rules.DecoderHandleAsBuiltInRuleImpl
    with rules.DecoderHandleAsLiteralTypeRuleImpl
    with rules.DecoderHandleAsValueTypeRuleImpl
    with rules.DecoderHandleAsOptionRuleImpl
    with rules.DecoderHandleAsMapRuleImpl
    with rules.DecoderHandleAsCollectionRuleImpl
    with rules.DecoderHandleAsNamedTupleRuleImpl
    with rules.DecoderHandleAsSingletonRuleImpl
    with rules.DecoderHandleAsCaseClassRuleImpl
    with rules.DecoderHandleAsEnumRuleImpl
    with rules.DecoderDerivationPolicyRuleImpl {
  this: MacroCommons & StdExtensions & AnnotationSupport & LoadStandardExtensionsOnce =>

  // $COVERAGE-OFF$
  override protected def derivationPolicyTypeClassName: String = "KindlingsYamlDecoder"
  // $COVERAGE-ON$

  // Entrypoints

  @scala.annotation.nowarn("msg=is never used")
  def deriveInlineDecode[A: Type](
      nodeExpr: Expr[Node],
      configExpr: Expr[YamlConfig]
  ): Expr[Either[ConstructError, A]] = {
    implicit val EitherT: Type[Either[ConstructError, A]] = DTypes.DecoderResult[A]
    implicit val NodeT: Type[Node] = DTypes.Node
    implicit val ConfigT: Type[YamlConfig] = DTypes.YamlConfig
    implicit val ConstructErrorT: Type[ConstructError] = DTypes.ConstructError
    val evConfig: Option[YamlConfig] = configExpr.semiEval.toOption

    deriveDecoderFromCtxAndAdaptForEntrypoint[A, Either[ConstructError, A]]("KindlingsYamlDecoder.decode") { fromCtx =>
      ValDefs.createVal[Node](nodeExpr).use { nodeVal =>
        ValDefs.createVal[YamlConfig](configExpr).use { configVal =>
          Expr.quote {
            val _ = Expr.splice(nodeVal)
            val _ = Expr.splice(configVal)
            Expr.splice {
              fromCtx(perCallDecoderCtx[A](nodeVal, configVal, evConfig))
            }
          }
        }
      }
    }
  }

  @scala.annotation.nowarn("msg=is never used")
  def deriveInlineFromYamlString[A: Type](
      yamlExpr: Expr[String],
      configExpr: Expr[YamlConfig]
  ): Expr[Either[YamlError, A]] = {
    implicit val EitherCEA: Type[Either[ConstructError, A]] = DTypes.DecoderResult[A]
    implicit val EitherYEA: Type[Either[YamlError, A]] = DTypes.YamlErrorResult[A]
    implicit val NodeT: Type[Node] = DTypes.Node
    implicit val ConfigT: Type[YamlConfig] = DTypes.YamlConfig
    implicit val ConstructErrorT: Type[ConstructError] = DTypes.ConstructError
    implicit val StringT: Type[String] = DTypes.String
    val evConfig: Option[YamlConfig] = configExpr.semiEval.toOption

    deriveDecoderFromCtxAndAdaptForEntrypoint[A, Either[YamlError, A]]("KindlingsYamlDecoder.fromYamlString") {
      fromCtx =>
        ValDefs.createVal[String](yamlExpr).use { yamlVal =>
          ValDefs.createVal[YamlConfig](configExpr).use { configVal =>
            Expr.quote {
              val y = Expr.splice(yamlVal)
              val cfg = Expr.splice(configVal)
              YamlDerivationUtils.parseAndDecode[A](
                y,
                (node: Node) => {
                  val _ = node
                  val _ = cfg
                  Expr.splice {
                    fromCtx(perCallDecoderCtx[A](Expr.quote(node), Expr.quote(cfg), evConfig))
                  }
                }
              )
            }
          }
        }
    }
  }

  @scala.annotation.nowarn("msg=is never used")
  def deriveDecoderTypeClass[A: Type](configExpr: Expr[YamlConfig]): Expr[KindlingsYamlDecoder[A]] = {
    implicit val DecoderA: Type[YamlDecoder[A]] = DTypes.YamlDecoder[A]
    implicit val KindlingsDecoderA: Type[KindlingsYamlDecoder[A]] = DTypes.KindlingsYamlDecoder[A]
    implicit val EitherT: Type[Either[ConstructError, A]] = DTypes.DecoderResult[A]
    implicit val NodeT: Type[Node] = DTypes.Node
    implicit val ConfigT: Type[YamlConfig] = DTypes.YamlConfig
    implicit val ConstructErrorT: Type[ConstructError] = DTypes.ConstructError
    val selfType: Option[??] = Some(Type[A].as_??)
    val evConfig: Option[YamlConfig] = configExpr.semiEval.toOption

    // The configuration is a val around the whole instance and so are the cached definitions (the decode helpers and
    // the lazy vals of the field decoders): they are created once per instance, not on every decode.
    deriveDecoderInstanceForEntrypoint[A, KindlingsYamlDecoder[A]]("KindlingsYamlDecoder.derived") { deriveHelper =>
      ValDefs.createVal[YamlConfig](configExpr).use { configVal =>
        val (decodeCall, cache) = deriveHelper(
          DecoderCtx.from[A](Expr.quote(null.asInstanceOf[Node]), configVal, derivedType = selfType, evConfig)
        )
        cache.toValDefs.use { _ =>
          Expr.quote {
            hearth.kindlings.yamlderivation.internal.runtime.YamlDerivationFactories.decoderInstance[A] { (node: Node) =>
              Expr.splice(decodeCall(Expr.quote(node)))
            }
          }
        }
      }
    }
  }

  /** The context of the inline entry points (see `DecoderCtx.perCall`). */
  private def perCallDecoderCtx[A: Type](
      node: Expr[Node],
      config: Expr[YamlConfig],
      evConfig: Option[YamlConfig]
  ): DecoderCtx[A] =
    DecoderCtx.from[A](node, config, derivedType = None, evConfig).copy(perCall = true)

  // Handles logging, error reporting and prepending "cached" defs and vals to the result.

  def deriveDecoderFromCtxAndAdaptForEntrypoint[A: Type, Out: Type](macroName: String)(
      provideCtxAndAdapt: (DecoderCtx[A] => Expr[Either[ConstructError, A]]) => Expr[Out]
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
          val fromCtx: (DecoderCtx[A] => Expr[Either[ConstructError, A]]) =
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
        errorRendering = if (shouldWeLogDecoderDerivation) RenderFrom(Log.Level.Info) else DontRender,
        timeout = derivationTimeout
      )(renderDecoderDerivationErrors)
  }

  /** As [[deriveDecoderFromCtxAndAdaptForEntrypoint]], for an instance: the function given to `provideCtxAndAdapt`
    * derives the decoder of `A` as a cached helper and returns the helper's call (for a node) and the cache, whose
    * definitions the instance wraps itself (once per instance, not inside its per-call decode function).
    */
  def deriveDecoderInstanceForEntrypoint[A: Type, Out: Type](macroName: String)(
      provideCtxAndAdapt: (DecoderCtx[A] => (Expr[Node] => Expr[Either[ConstructError, A]], ValDefsCache)) => Expr[Out]
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
          val deriveHelper: DecoderCtx[A] => (Expr[Node] => Expr[Either[ConstructError, A]], ValDefsCache) =
            (ctx: DecoderCtx[A]) =>
              runSafe {
                for {
                  _ <- ensureStandardExtensionsLoaded()
                  _ <- deriveDecoderRecursively[A](using ctx)
                  helper <- ctx.getHelper[A]
                  cache <- ctx.cache.get
                } yield ((node: Expr[Node]) => helper.get(node, ctx.config), cache)
              }
          provideCtxAndAdapt(deriveHelper)
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
      )(renderDecoderDerivationErrors)
  }

  private def renderDecoderDerivationErrors(
      errorLogs: String,
      errors: hearth.fp.data.NonEmptyVector[Throwable]
  ): String = {
    val errorsRendered =
      hearth.kindlings.derivation.compiletime.DerivationErrorRendering.renderErrors(errors.toVector)
    val hint =
      "Enable debug logging with: import hearth.kindlings.yamlderivation.debug.logDerivationForKindlingsYamlDecoder or scalac option -Xmacro-settings:yamlDerivation.logDerivation=true"
    if (errorLogs.nonEmpty)
      s"""Macro derivation failed with the following errors:
         |$errorsRendered
         |and the following logs:
         |${hearth.kindlings.derivation.compiletime.DerivationErrorRendering.withoutAnsiCodes(errorLogs)}
         |$hint""".stripMargin
    else
      s"""Macro derivation failed with the following errors:
         |$errorsRendered
         |$hint""".stripMargin
  }

  def shouldWeLogDecoderDerivation: Boolean = {
    implicit val LogDerivation: Type[KindlingsYamlDecoder.LogDerivation] = DTypes.DecoderLogDerivation
    def logDerivationImported = Expr.summonImplicit[KindlingsYamlDecoder.LogDerivation].isDefined

    def logDerivationSetGlobally = (for {
      data <- Environment.typedSettings.toOption
      yamlDerivation <- data.get("yamlDerivation")
      shouldLog <- yamlDerivation.get("logDerivation").flatMap(_.asBoolean)
    } yield shouldLog).getOrElse(false)

    logDerivationImported || logDerivationSetGlobally
  }

  // Context

  final case class DecoderCtx[A](
      tpe: Type[A],
      node: Expr[Node],
      config: Expr[YamlConfig],
      cache: MLocal[ValDefsCache],
      derivedType: Option[??],
      evaluatedConfig: Option[YamlConfig] = None,
      // The inline entry points run the cached definitions on every call: there a cached field decoder is a `def`
      // (evaluated where it is used), not a `lazy val` (a lazy cell allocated per call).
      perCall: Boolean = false
  ) {

    def nest[B: Type](newNode: Expr[Node]): DecoderCtx[B] = copy[B](
      tpe = Type[B],
      node = newNode
    )

    /** The context of a cached helper's body. The configuration is NOT replaced by the helper's `config` parameter:
      * every entry point binds the configuration to a value around all the cached definitions and every helper call
      * passes that value, so referring to it directly lets the cached field decoders (lazy vals of the instance, see
      * [[getFieldDecoder]]) be built from code that does not depend on a helper's parameters.
      */
    @scala.annotation.nowarn("msg=is never used|unused explicit parameter")
    def nestInCache(
        newNode: Expr[Node],
        newConfig: Expr[YamlConfig]
    ): DecoderCtx[A] = copy(
      node = newNode
    )

    /** A field's decoder, cached as a lazy val of the instance (evaluated once, not on every decode), or as a def when
      * the definitions are evaluated per call ([[perCall]]).
      */
    def getFieldDecoder[B: Type]: MIO[Option[Expr[YamlDecoder[B]]]] = {
      implicit val DecoderB: Type[YamlDecoder[B]] = DTypes.YamlDecoder[B]
      cache.get0Ary[YamlDecoder[B]]("cached-field-decoder")
    }
    def setFieldDecoder[B: Type](decoder: Expr[YamlDecoder[B]]): MIO[Unit] = {
      implicit val DecoderB: Type[YamlDecoder[B]] = DTypes.YamlDecoder[B]
      if (perCall)
        cache.buildCachedWith(
          "cached-field-decoder",
          ValDefBuilder.ofDef0[YamlDecoder[B]](s"fieldDecoder_${Type[B].shortName}")
        )(_ => decoder)
      else
        cache.buildCachedWith(
          "cached-field-decoder",
          ValDefBuilder.ofLazy[YamlDecoder[B]](s"fieldDecoder_${Type[B].shortName}")
        )(_ => decoder)
    }

    /** A summoned instance as a lazy val of the derived instance (resolved once, not on every decode); used as it is in
      * the inline entry points ([[perCall]]).
      */
    def cachedInstance[B: Type](instance: Expr[YamlDecoder[B]]): MIO[Expr[YamlDecoder[B]]] =
      if (perCall) MIO.pure(instance)
      else
        getInstance[B].flatMap {
          case Some(cached) => MIO.pure(cached)
          case None         => setInstance[B](instance) >> getInstance[B].map(_.get)
        }

    /** The subtype names `names` transformed by a configuration only known at runtime: computed once per instance (a
      * lazy val holding an array), or where they are used in the inline entry points.
      */
    def transformedConstructorNames(names: List[String]): MIO[String => Expr[String]] =
      transformedNamesOf(names, config, perCall, cache)

    def getInstance[B: Type]: MIO[Option[Expr[YamlDecoder[B]]]] = {
      implicit val DecoderB: Type[YamlDecoder[B]] = DTypes.YamlDecoder[B]
      cache.get0Ary[YamlDecoder[B]]("cached-decoder-instance")
    }
    def setInstance[B: Type](instance: Expr[YamlDecoder[B]]): MIO[Unit] = {
      implicit val DecoderB: Type[YamlDecoder[B]] = DTypes.YamlDecoder[B]
      Log.info(s"Caching YamlDecoder instance for ${Type[B].prettyPrint}") >>
        cache.buildCachedWith(
          "cached-decoder-instance",
          ValDefBuilder.ofLazy[YamlDecoder[B]](s"decoder_${Type[B].shortName}")
        )(_ => instance)
    }

    def getHelper[B: Type]: MIO[Option[(Expr[Node], Expr[YamlConfig]) => Expr[Either[ConstructError, B]]]] = {
      implicit val ResultB: Type[Either[ConstructError, B]] = DTypes.DecoderResult[B]
      implicit val NodeT: Type[Node] = DTypes.Node
      implicit val ConfigT: Type[YamlConfig] = DTypes.YamlConfig
      cache.get2Ary[Node, YamlConfig, Either[ConstructError, B]]("cached-decode-method")
    }
    def setHelper[B: Type](
        helper: (Expr[Node], Expr[YamlConfig]) => MIO[Expr[Either[ConstructError, B]]]
    ): MIO[Unit] = {
      implicit val ResultB: Type[Either[ConstructError, B]] = DTypes.DecoderResult[B]
      implicit val NodeT: Type[Node] = DTypes.Node
      implicit val ConfigT: Type[YamlConfig] = DTypes.YamlConfig
      val defBuilder =
        ValDefBuilder.ofDef2[Node, YamlConfig, Either[ConstructError, B]](s"decode_${Type[B].shortName}")
      for {
        _ <- Log.info(s"Forward-declaring decode helper for ${Type[B].prettyPrint}")
        _ <- cache.forwardDeclare("cached-decode-method", defBuilder)
        _ <- MIO.scoped { runSafe =>
          runSafe(cache.buildCachedWith("cached-decode-method", defBuilder) { case (_, (node, config)) =>
            runSafe(helper(node, config))
          })
        }
        _ <- Log.info(s"Defined decode helper for ${Type[B].prettyPrint}")
      } yield ()
    }

    override def toString: String =
      s"decode[${tpe.prettyPrint}](node = ${node.prettyPrint}, config = ${config.prettyPrint})"
  }
  object DecoderCtx {

    def from[A: Type](
        node: Expr[Node],
        config: Expr[YamlConfig],
        derivedType: Option[??],
        evaluatedConfig: Option[YamlConfig] = None
    ): DecoderCtx[A] = DecoderCtx(
      tpe = Type[A],
      node = node,
      config = config,
      cache = ValDefsCache.mlocal,
      derivedType = derivedType,
      evaluatedConfig = evaluatedConfig
    )
  }

  def dctx[A](implicit A: DecoderCtx[A]): DecoderCtx[A] = A

  implicit def currentDecoderValueType[A: DecoderCtx]: Type[A] = dctx.tpe

  abstract class DecoderDerivationRule(val name: String) extends Rule {
    def apply[A: DecoderCtx]: MIO[Rule.Applicability[Expr[Either[ConstructError, A]]]]
  }

  // The actual derivation logic

  def deriveDecoderRecursively[A: DecoderCtx]: MIO[Expr[Either[ConstructError, A]]] =
    Log
      .namedScope(s"Deriving decoder for type ${Type[A].prettyPrint}") {
        dctx.getHelper[A].flatMap {
          case Some(helperCall) =>
            Log.info(s"Using cached decoder helper for ${Type[A].prettyPrint}") >>
              MIO.pure(helperCall(dctx.node, dctx.config))
          case None =>
            dctx.setHelper[A] { (node, config) =>
              deriveDecoderViaRules[A](using dctx.nestInCache(node, config))
            } >> dctx.getHelper[A].flatMap {
              case Some(helperCall) =>
                MIO.pure(helperCall(dctx.node, dctx.config))
              case None =>
                deriveDecoderViaRules[A]
            }
        }
      }

  private def deriveDecoderViaRules[A: DecoderCtx]: MIO[Expr[Either[ConstructError, A]]] =
    Log
      .namedScope(s"Deriving decoder via rules for type ${Type[A].prettyPrint}") {
        Rules(
          DecoderHandleAsLiteralTypeRule,
          DecoderUseImplicitWhenAvailableRule,
          DecoderDerivationPolicyRule,
          DecoderHandleAsBuiltInRule,
          DecoderHandleAsValueTypeRule,
          DecoderHandleAsOptionRule,
          // DecoderHandleAsCollectionRule now handles maps too (single IsCollection parse + IsMapOf dispatch),
          // so the standalone DecoderHandleAsMapRule is no longer in the chain (its object is kept for
          // decodeMapEntries).
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
              // .removed(DecoderUseCachedDefWhenAvailableRule)
              .view.map { case (rule, reasons) =>
                if (reasons.isEmpty) s"The rule ${rule.name} was not applicable"
                else
                  s" - The rule ${rule.name} was not applicable, for the following reasons: ${reasons.mkString(", ")}"
              }.toList
            val err = DecoderDerivationError.UnsupportedType(Type[A].prettyPrint, reasonsStrings)
            Log.error(err.message) >> MIO.fail(err)
        }
      }

  /** The decoder of a case class field: cached as a lazy val of the instance (created once, not on every decode, see
    * `DecoderCtx.getFieldDecoder`) and shared by all the fields of this type.
    */
  @scala.annotation.nowarn("msg=is never used|unused explicit parameter")
  def deriveFieldDecoder[Field: Type](implicit ctx: DecoderCtx[?]): MIO[Expr[YamlDecoder[Field]]] = {
    implicit val NodeT: Type[Node] = DTypes.Node
    implicit val EitherCEField: Type[Either[ConstructError, Field]] = DTypes.DecoderResult[Field]

    ctx.getFieldDecoder[Field].flatMap {
      case Some(cached) => MIO.pure(cached)
      case None         =>
        val decoder: MIO[Expr[YamlDecoder[Field]]] =
          DTypes
            .YamlDecoder[Field]
            .summonExprIgnoring(DecoderUseImplicitWhenAvailableRule.ignoredImplicits*)
            .toEither match {
            case Right(decoderExpr) =>
              Log.info(s"Found implicit YamlDecoder[${Type[Field].prettyPrint}]") >> MIO.pure(decoderExpr)
            case Left(_) =>
              Log.info(s"Building YamlDecoder[${Type[Field].prettyPrint}] via recursive derivation") >>
                LambdaBuilder
                  .of1[Node]("fieldNode")
                  .traverse { fieldNodeExpr =>
                    deriveDecoderRecursively[Field](using ctx.nest[Field](fieldNodeExpr))
                  }
                  .map { builder =>
                    val decodeFn = builder.build[Either[ConstructError, Field]]
                    Expr.quote(YamlDerivationUtils.decoderFromFn(Expr.splice(decodeFn)))
                  }
          }
        for {
          decoderExpr <- decoder
          // a recursive field type may have cached it while it was being derived
          cached <- ctx.getFieldDecoder[Field]
          result <- cached match {
            case Some(existing) => MIO.pure(existing)
            case None           => ctx.setFieldDecoder[Field](decoderExpr) >> ctx.getFieldDecoder[Field].map(_.get)
          }
        } yield result
    }
  }

  // Types

  private[compiletime] object DTypes {

    def YamlDecoder: Type.Ctor1[YamlDecoder] = Type.Ctor1.of[YamlDecoder]
    def KindlingsYamlDecoder: Type.Ctor1[KindlingsYamlDecoder] = Type.Ctor1.of[KindlingsYamlDecoder]
    val DecoderLogDerivation: Type[hearth.kindlings.yamlderivation.KindlingsYamlDecoder.LogDerivation] =
      Type.of[hearth.kindlings.yamlderivation.KindlingsYamlDecoder.LogDerivation]
    val Node: Type.Lazy[Node] = Type.Lazy(Type.of[Node])
    val ConstructError: Type.Lazy[ConstructError] = Type.Lazy(Type.of[ConstructError])
    val YamlConfig: Type.Lazy[YamlConfig] = Type.Lazy(Type.of[YamlConfig])
    val String: Type.Lazy[String] = Type.Lazy(Type.of[String])
    val Int: Type.Lazy[Int] = Type.Lazy(Type.of[Int])
    val Long: Type.Lazy[Long] = Type.Lazy(Type.of[Long])
    val Double: Type.Lazy[Double] = Type.Lazy(Type.of[Double])
    val Float: Type.Lazy[Float] = Type.Lazy(Type.of[Float])
    val Boolean: Type.Lazy[Boolean] = Type.Lazy(Type.of[Boolean])
    val Short: Type.Lazy[Short] = Type.Lazy(Type.of[Short])
    val Byte: Type.Lazy[Byte] = Type.Lazy(Type.of[Byte])
    val Char: Type.Lazy[Char] = Type.Lazy(Type.of[Char])
    val BigDecimal: Type.Lazy[BigDecimal] = Type.Lazy(Type.of[BigDecimal])
    val BigInt: Type.Lazy[BigInt] = Type.Lazy(Type.of[BigInt])
    val Any: Type.Lazy[Any] = Type.Lazy(Type.of[Any])
    val ArrayAny: Type.Lazy[Array[Any]] = Type.Lazy(Type.of[Array[Any]])
    val EitherCEAny: Type.Lazy[Either[ConstructError, Any]] = Type.Lazy(Type.of[Either[ConstructError, Any]])
    val FieldNodes: Type[java.util.HashMap[String, Node]] = Type.of[java.util.HashMap[String, Node]]
    val EitherCEFieldNodes: Type[Either[ConstructError, java.util.HashMap[String, Node]]] =
      Type.of[Either[ConstructError, java.util.HashMap[String, Node]]]
    val ListEitherCEAny: Type[List[Either[ConstructError, Any]]] =
      Type.of[List[Either[ConstructError, Any]]]
    val ListString: Type.Lazy[List[String]] = Type.Lazy(Type.of[List[String]])
    val StringNodeTuple: Type.Lazy[(String, Node)] = Type.Lazy(Type.of[(String, Node)])
    val FieldName: Type.Lazy[fieldName] = Type.Lazy(Type.of[fieldName])
    val TransientField: Type.Lazy[transientField] = Type.Lazy(Type.of[transientField])

    def DecoderResult[A: Type]: Type[Either[ConstructError, A]] =
      Type.of[Either[ConstructError, A]]

    def YamlErrorResult[A: Type]: Type[Either[YamlError, A]] =
      Type.of[Either[YamlError, A]]
  }
}
