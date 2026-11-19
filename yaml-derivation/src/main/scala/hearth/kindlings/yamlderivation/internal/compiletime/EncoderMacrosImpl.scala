package hearth.kindlings.yamlderivation.internal.compiletime

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

import hearth.kindlings.yamlderivation.{KindlingsYamlEncoder, YamlConfig}
import hearth.kindlings.yamlderivation.annotations.{fieldName, transientField}
import hearth.kindlings.yamlderivation.internal.runtime.YamlDerivationUtils
import org.virtuslab.yaml.{Node, YamlEncoder}

trait EncoderMacrosImpl
    extends YamlDerivationTimeout
    with SubtypeNamesSupport
    with YamlDerivationPolicy
    with rules.EncoderUseCachedDefWhenAvailableRuleImpl
    with rules.EncoderUseImplicitWhenAvailableRuleImpl
    with rules.EncoderHandleAsBuiltInRuleImpl
    with rules.EncoderHandleAsLiteralTypeRuleImpl
    with rules.EncoderHandleAsValueTypeRuleImpl
    with rules.EncoderHandleAsOptionRuleImpl
    with rules.EncoderHandleAsMapRuleImpl
    with rules.EncoderHandleAsCollectionRuleImpl
    with rules.EncoderHandleAsNamedTupleRuleImpl
    with rules.EncoderHandleAsSingletonRuleImpl
    with rules.EncoderHandleAsCaseClassRuleImpl
    with rules.EncoderHandleAsEnumRuleImpl
    with rules.EncoderDerivationPolicyRuleImpl {
  this: MacroCommons & StdExtensions & AnnotationSupport & LoadStandardExtensionsOnce =>

  // $COVERAGE-OFF$
  override protected def derivationPolicyTypeClassName: String = "KindlingsYamlEncoder"
  // $COVERAGE-ON$

  // Entrypoints

  def deriveInlineEncode[A: Type](valueExpr: Expr[A], configExpr: Expr[YamlConfig]): Expr[Node] = {
    implicit val NodeT: Type[Node] = Types.Node
    implicit val ConfigT: Type[YamlConfig] = Types.YamlConfig
    val evConfig: Option[YamlConfig] = configExpr.semiEval.toOption

    deriveEncoderFromCtxAndAdaptForEntrypoint[A, Node]("KindlingsYamlEncoder.encode") { fromCtx =>
      ValDefs.createVal[A](valueExpr).use { valueVal =>
        ValDefs.createVal[YamlConfig](configExpr).use { configVal =>
          // the definitions are evaluated on every call: see `EncoderCtx.perCall`
          val encoded = fromCtx(perCallEncoderCtx[A](valueVal, configVal, evConfig))
          Expr.quote {
            val _ = Expr.splice(valueVal)
            val _ = Expr.splice(configVal)
            Expr.splice(encoded)
          }
        }
      }
    }
  }

  @scala.annotation.nowarn("msg=is never used")
  def deriveInlineToYamlString[A: Type](valueExpr: Expr[A], configExpr: Expr[YamlConfig]): Expr[String] = {
    implicit val ConfigT: Type[YamlConfig] = Types.YamlConfig
    implicit val StringT: Type[String] = Types.String
    val evConfig: Option[YamlConfig] = configExpr.semiEval.toOption

    deriveEncoderFromCtxAndAdaptForEntrypoint[A, String]("KindlingsYamlEncoder.toYamlString") { fromCtx =>
      ValDefs.createVal[A](valueExpr).use { valueVal =>
        ValDefs.createVal[YamlConfig](configExpr).use { configVal =>
          // the definitions are evaluated on every call: see `EncoderCtx.perCall`
          val encoded = fromCtx(perCallEncoderCtx[A](valueVal, configVal, evConfig))
          Expr.quote {
            val _ = Expr.splice(valueVal)
            val _ = Expr.splice(configVal)
            val node = Expr.splice(encoded)
            YamlDerivationUtils.nodeToYaml(node, Expr.splice(configVal).emptyCollectionsAsFlow)
          }
        }
      }
    }
  }

  @scala.annotation.nowarn("msg=is never used")
  def deriveEncoderTypeClass[A: Type](configExpr: Expr[YamlConfig]): Expr[KindlingsYamlEncoder[A]] = {
    implicit val EncoderA: Type[YamlEncoder[A]] = Types.YamlEncoder[A]
    implicit val KindlingsEncoderA: Type[KindlingsYamlEncoder[A]] = Types.KindlingsYamlEncoder[A]
    implicit val NodeT: Type[Node] = Types.Node
    implicit val ConfigT: Type[YamlConfig] = Types.YamlConfig
    val selfType: Option[??] = Some(Type[A].as_??)
    val evConfig: Option[YamlConfig] = configExpr.semiEval.toOption

    // The configuration is a val around the whole instance and so are the cached definitions (the encode helpers and
    // the lazy vals of summoned instances and runtime-transformed names): they are created once per instance, not on
    // every encode.
    deriveEncoderInstanceForEntrypoint[A, KindlingsYamlEncoder[A]](
      "KindlingsYamlEncoder.derived",
      configExpr,
      selfType,
      evConfig
    ) { encode =>
      Expr.quote {
        hearth.kindlings.yamlderivation.internal.runtime.YamlDerivationFactories.encoderInstance[A] { (obj: A) =>
          val _ = obj
          Expr.splice(encode(Expr.quote(obj)))
        }
      }
    }
  }

  /** The context of the inline entry points (see `EncoderCtx.perCall`). */
  private def perCallEncoderCtx[A: Type](
      value: Expr[A],
      config: Expr[YamlConfig],
      evConfig: Option[YamlConfig]
  ): EncoderCtx[A] =
    EncoderCtx.from[A](value, config, derivedType = None, evConfig).copy(perCall = true)

  // Handles logging, error reporting and prepending "cached" defs and vals to the result.

  /** The inline entry points: the cached definitions are put around the encoding of a single value (they are evaluated
    * on every call, see `EncoderCtx.perCall`).
    */
  def deriveEncoderFromCtxAndAdaptForEntrypoint[A: Type, Out: Type](macroName: String)(
      provideCtxAndAdapt: (EncoderCtx[A] => Expr[Node]) => Expr[Out]
  ): Expr[Out] =
    runEncoderEntrypoint[A, Out](macroName) {
      MIO.scoped { runSafe =>
        val fromCtx: (EncoderCtx[A] => Expr[Node]) = (ctx: EncoderCtx[A]) =>
          runSafe {
            for {
              _ <- ensureStandardExtensionsLoaded()
              result <- deriveEncoderRecursively[A](using ctx)
              cache <- ctx.cache.get
            } yield cache.toValDefs.use(_ => result)
          }

        provideCtxAndAdapt(fromCtx)
      }
    }

  /** A type class instance: the configuration is a val around the whole instance, and the encoding of `A` is derived
    * once into a cached def, with the cached definitions around the whole instance too, so that they are created once
    * per instance, not on every call. `adapt` builds the instance from the call of that def.
    */
  def deriveEncoderInstanceForEntrypoint[A: Type, Out: Type](
      macroName: String,
      configExpr: Expr[YamlConfig],
      derivedType: Option[??],
      evConfig: Option[YamlConfig]
  )(
      adapt: (Expr[A] => Expr[Node]) => Expr[Out]
  ): Expr[Out] = {
    implicit val ConfigT: Type[YamlConfig] = Types.YamlConfig
    runEncoderEntrypoint[A, Out](macroName) {
      MIO.scoped { runSafe =>
        ValDefs.createVal[YamlConfig](configExpr).use { configVal =>
          // the value is a placeholder: the encoding is derived into a cached def, called with the instance's argument
          val ctx = EncoderCtx.from[A](Expr.quote(null.asInstanceOf[A]), configVal, derivedType, evConfig)
          runSafe {
            for {
              _ <- ensureStandardExtensionsLoaded()
              _ <- deriveEncoderRecursively[A](using ctx)
            } yield ()
          }
          val encode = runSafe(ctx.getHelper[A]).getOrElse {
            Environment.reportErrorAndAbort(s"$macroName: no encoding was derived for ${Type[A].prettyPrint}")
          }
          val cache = runSafe(ctx.cache.get)
          cache.toValDefs.use(_ => adapt((value: Expr[A]) => encode(value, configVal)))
        }
      }
    }
  }

  private def runEncoderEntrypoint[A: Type, Out: Type](macroName: String)(derive: => MIO[Expr[Out]]): Expr[Out] = {
    if (Type[A] =:= Type.of[Nothing].asInstanceOf[Type[A]] || Type[A] =:= Type.of[Any].asInstanceOf[Type[A]])
      Environment.reportErrorAndAbort(
        s"$macroName: type parameter was inferred as ${Type[A].prettyPrint}, which is likely unintended.\n" +
          s"Provide an explicit type parameter, e.g.: $macroName[MyType](...)\n" +
          "or add a type ascription to the result variable."
      )
    Log
      .namedScope(
        s"Deriving encoder for ${Type[A].prettyPrint} at: ${Environment.currentPosition.prettyPrint}"
      )(derive)
      .flatTap { result =>
        Log.info(s"Derived final encoder result: ${result.prettyPrint}")
      }
      .runToExprOrFail(
        macroName,
        infoRendering = if (shouldWeLogEncoderDerivation) RenderFrom(Log.Level.Info) else DontRender,
        errorRendering = if (shouldWeLogEncoderDerivation) RenderFrom(Log.Level.Info) else DontRender,
        timeout = derivationTimeout
      ) { (errorLogs, errors) =>
        val errorsRendered =
          hearth.kindlings.derivation.compiletime.DerivationErrorRendering.renderErrors(errors.toVector)
        val hint =
          "Enable debug logging with: import hearth.kindlings.yamlderivation.debug.logDerivationForKindlingsYamlEncoder or scalac option -Xmacro-settings:yamlDerivation.logDerivation=true"
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
  }

  def shouldWeLogEncoderDerivation: Boolean = {
    implicit val LogDerivation: Type[KindlingsYamlEncoder.LogDerivation] = Types.EncoderLogDerivation
    def logDerivationImported = Expr.summonImplicit[KindlingsYamlEncoder.LogDerivation].isDefined

    def logDerivationSetGlobally = (for {
      data <- Environment.typedSettings.toOption
      yamlDerivation <- data.get("yamlDerivation")
      shouldLog <- yamlDerivation.get("logDerivation").flatMap(_.asBoolean)
    } yield shouldLog).getOrElse(false)

    logDerivationImported || logDerivationSetGlobally
  }

  // Context

  final case class EncoderCtx[A](
      tpe: Type[A],
      value: Expr[A],
      config: Expr[YamlConfig],
      cache: MLocal[ValDefsCache],
      derivedType: Option[??],
      evaluatedConfig: Option[YamlConfig] = None,
      // The inline entry points evaluate the cached definitions on every call, so summoned instances and transformed
      // names are not cached in them (a lazy val would be a lazy cell allocated per call).
      perCall: Boolean = false
  ) {

    def nest[B: Type](newValue: Expr[B]): EncoderCtx[B] = copy[B](
      tpe = Type[B],
      value = newValue
    )

    /** The context of a cached helper's body. The configuration is NOT replaced by the helper's `config` parameter:
      * every entry point binds the configuration to a value around all the cached definitions and every helper call
      * passes that value, so referring to it directly lets the cached values (lazy vals of the instance, see
      * [[cachedLazy]]) be built from code that does not depend on a helper's parameters.
      */
    @scala.annotation.nowarn("msg=is never used|unused explicit parameter")
    def nestInCache(
        newValue: Expr[A],
        newConfig: Expr[YamlConfig]
    ): EncoderCtx[A] = copy(
      value = newValue
    )

    /** `value` as a lazy val of the derived instance (evaluated once, not on every encode), shared under `key` (and its
      * type); in the inline entry points ([[perCall]]) it is used as it is.
      */
    def cachedLazy[T: Type](key: String, name: String, value: => Expr[T]): MIO[Expr[T]] =
      if (perCall) MIO.pure(value)
      else
        cache.get0Ary[T](key).flatMap {
          case Some(cached) => MIO.pure(cached)
          case None         =>
            cache.buildCachedWith(key, ValDefBuilder.ofLazy[T](name))(_ => value) >>
              cache.get0Ary[T](key).map(_.get)
        }

    /** A summoned instance as a lazy val of the derived instance: resolved once, not on every encode. */
    def cachedInstance[B: Type](instance: Expr[YamlEncoder[B]]): MIO[Expr[YamlEncoder[B]]] = {
      implicit val EncoderB: Type[YamlEncoder[B]] = Types.YamlEncoder[B]
      cachedLazy[YamlEncoder[B]]("cached-encoder-instance", s"encoder_${Type[B].shortName}", instance)
    }

    /** The subtype names `names` transformed by a configuration only known at runtime (`transformConstructorNames`):
      * computed once per instance (a lazy val holding an array), or where they are used in the inline entry points.
      */
    def transformedConstructorNames(names: List[String]): MIO[String => Expr[String]] =
      transformedNamesOf(names, config, perCall, cache)

    def getInstance[B: Type]: MIO[Option[Expr[YamlEncoder[B]]]] = {
      implicit val EncoderB: Type[YamlEncoder[B]] = Types.YamlEncoder[B]
      cache.get0Ary[YamlEncoder[B]]("cached-encoder-instance")
    }
    def setInstance[B: Type](instance: Expr[YamlEncoder[B]]): MIO[Unit] = {
      implicit val EncoderB: Type[YamlEncoder[B]] = Types.YamlEncoder[B]
      Log.info(s"Caching YamlEncoder instance for ${Type[B].prettyPrint}") >>
        cache.buildCachedWith(
          "cached-encoder-instance",
          ValDefBuilder.ofLazy[YamlEncoder[B]](s"encoder_${Type[B].shortName}")
        )(_ => instance)
    }

    def getHelper[B: Type]: MIO[Option[(Expr[B], Expr[YamlConfig]) => Expr[Node]]] = {
      implicit val NodeT: Type[Node] = Types.Node
      implicit val ConfigT: Type[YamlConfig] = Types.YamlConfig
      cache.get2Ary[B, YamlConfig, Node]("cached-encode-method")
    }
    def setHelper[B: Type](
        helper: (Expr[B], Expr[YamlConfig]) => MIO[Expr[Node]]
    ): MIO[Unit] = {
      implicit val NodeT: Type[Node] = Types.Node
      implicit val ConfigT: Type[YamlConfig] = Types.YamlConfig
      val defBuilder =
        ValDefBuilder.ofDef2[B, YamlConfig, Node](s"encode_${Type[B].shortName}")
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
        config: Expr[YamlConfig],
        derivedType: Option[??],
        evaluatedConfig: Option[YamlConfig] = None
    ): EncoderCtx[A] = EncoderCtx(
      tpe = Type[A],
      value = value,
      config = config,
      cache = ValDefsCache.mlocal,
      derivedType = derivedType,
      evaluatedConfig = evaluatedConfig
    )
  }

  def ectx[A](implicit A: EncoderCtx[A]): EncoderCtx[A] = A

  implicit def currentEncoderValueType[A: EncoderCtx]: Type[A] = ectx.tpe

  abstract class EncoderDerivationRule(val name: String) extends Rule {
    def apply[A: EncoderCtx]: MIO[Rule.Applicability[Expr[Node]]]
  }

  // The actual derivation logic

  def deriveEncoderRecursively[A: EncoderCtx]: MIO[Expr[Node]] =
    Log
      .namedScope(s"Deriving encoder for type ${Type[A].prettyPrint}") {
        ectx.getHelper[A].flatMap {
          case Some(helperCall) =>
            Log.info(s"Using cached encoder helper for ${Type[A].prettyPrint}") >>
              MIO.pure(helperCall(ectx.value, ectx.config))
          case None =>
            ectx.setHelper[A] { (value, config) =>
              deriveEncoderViaRules[A](using ectx.nestInCache(value, config))
            } >> ectx.getHelper[A].flatMap {
              case Some(helperCall) =>
                MIO.pure(helperCall(ectx.value, ectx.config))
              case None =>
                deriveEncoderViaRules[A]
            }
        }
      }

  private def deriveEncoderViaRules[A: EncoderCtx]: MIO[Expr[Node]] =
    Log
      .namedScope(s"Deriving encoder via rules for type ${Type[A].prettyPrint}") {
        Rules(
          EncoderHandleAsLiteralTypeRule,
          EncoderUseImplicitWhenAvailableRule,
          EncoderDerivationPolicyRule,
          EncoderHandleAsBuiltInRule,
          EncoderHandleAsValueTypeRule,
          EncoderHandleAsOptionRule,
          // EncoderHandleAsCollectionRule now handles maps too (single IsCollection parse + IsMapOf dispatch),
          // so the standalone EncoderHandleAsMapRule is no longer in the chain (its object is kept for
          // deriveMapEntries).
          EncoderHandleAsCollectionRule,
          EncoderHandleAsNamedTupleRule,
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
            val err = EncoderDerivationError.UnsupportedType(Type[A].prettyPrint, reasonsStrings)
            Log.error(err.message) >> MIO.fail(err)
        }
      }

  // Types

  private[compiletime] object Types {

    def YamlEncoder: Type.Ctor1[YamlEncoder] = Type.Ctor1.of[YamlEncoder]
    def KindlingsYamlEncoder: Type.Ctor1[KindlingsYamlEncoder] = Type.Ctor1.of[KindlingsYamlEncoder]
    val EncoderLogDerivation: Type[hearth.kindlings.yamlderivation.KindlingsYamlEncoder.LogDerivation] =
      Type.of[hearth.kindlings.yamlderivation.KindlingsYamlEncoder.LogDerivation]
    val Node: Type.Lazy[Node] = Type.Lazy(Type.of[Node])
    val YamlConfig: Type.Lazy[YamlConfig] = Type.Lazy(Type.of[YamlConfig])
    val String: Type.Lazy[String] = Type.Lazy(Type.of[String])
    val FieldName: Type.Lazy[fieldName] = Type.Lazy(Type.of[fieldName])
    val TransientField: Type.Lazy[transientField] = Type.Lazy(Type.of[transientField])
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
    val Product: Type.Lazy[Product] = Type.Lazy(Type.of[Product])
  }
}
