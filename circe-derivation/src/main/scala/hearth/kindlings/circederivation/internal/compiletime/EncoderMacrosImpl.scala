package hearth.kindlings.circederivation.internal.compiletime

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

import hearth.kindlings.circederivation.{Configuration, KindlingsEncoder, KindlingsEncoderAsObject}
import hearth.kindlings.circederivation.annotations.{fieldName, transientField}
import io.circe.{Encoder, Json, JsonObject, KeyEncoder}

trait EncoderMacrosImpl
    extends CirceDerivationTimeout
    with CirceDerivationPolicy
    with SealedHierarchyFlattening
    with rules.EncoderUseCachedDefWhenAvailableRuleImpl
    with rules.EncoderUseImplicitWhenAvailableRuleImpl
    with rules.EncoderHandleAsLiteralTypeRuleImpl
    with rules.EncoderHandleAsBuiltInTypeRuleImpl
    with rules.EncoderHandleAsValueTypeRuleImpl
    with rules.EncoderHandleAsOptionRuleImpl
    with rules.EncoderHandleAsMapRuleImpl
    with rules.EncoderHandleAsCollectionRuleImpl
    with rules.EncoderHandleAsNamedTupleRuleImpl
    with rules.EncoderHandleAsTupleRuleImpl
    with rules.EncoderHandleAsSingletonRuleImpl
    with rules.EncoderHandleAsCaseClassRuleImpl
    with rules.EncoderHandleAsEnumRuleImpl
    with rules.EncoderDerivationPolicyRuleImpl {
  this: MacroCommons & StdExtensions & AnnotationSupport & LoadStandardExtensionsOnce =>

  // $COVERAGE-OFF$
  override protected def derivationPolicyTypeClassName: String = "KindlingsEncoder"
  // $COVERAGE-ON$

  // Entrypoints

  def deriveInlineEncode[A: Type](valueExpr: Expr[A], configExpr: Expr[Configuration]): Expr[Json] = {
    implicit val JsonT: Type[Json] = Types.Json
    implicit val ConfigT: Type[Configuration] = Types.Configuration
    val evConfig: Option[Configuration] = semiEvalConfig(configExpr)
    evaluatedConfigFlags = evConfig

    deriveEncoderFromCtxAndAdaptForEntrypoint[A, Json]("KindlingsEncoder.encode") { fromCtx =>
      ValDefs.createVal[A](valueExpr).use { valueVal =>
        ValDefs.createVal[Configuration](configExpr).use { configVal =>
          Expr.quote {
            val _ = Expr.splice(valueVal)
            val _ = Expr.splice(configVal)
            Expr.splice(fromCtx(EncoderCtx.from(valueVal, configVal, derivedType = None, evConfig)))
          }
        }
      }
    }
  }

  @scala.annotation.nowarn("msg=is never used|Non local returns")
  def deriveEncoderTypeClass[A: Type](configExpr: Expr[Configuration]): Expr[KindlingsEncoder[A]] = {
    implicit val EncoderA: Type[Encoder[A]] = Types.Encoder[A]
    implicit val KindlingsEncoderA: Type[KindlingsEncoder[A]] = Types.KindlingsEncoder[A]
    implicit val JsonT: Type[Json] = Types.Json
    implicit val ConfigT: Type[Configuration] = Types.Configuration
    val selfType: Option[??] = Some(Type[A].as_??)
    val evConfig: Option[Configuration] = semiEvalConfig(configExpr)
    evaluatedConfigFlags = evConfig

    // Like circe's semiauto/`derives` (which return `Encoder.AsObject` for products and sums), return an
    // `Encoder.AsObject` instance whenever the type is known to encode to a JSON object.
    if (nonObjectReason[A](evaluatedConfigFlags).isEmpty) {
      implicit val KindlingsEncoderAsObjectA: Type[KindlingsEncoderAsObject[A]] = Types.KindlingsEncoderAsObject[A]
      return deriveEncoderAsObjectTypeClass[A](configExpr).upcast[KindlingsEncoder[A]]
    }

    // A derived sum is marked as a KindlingsSumEncoder (flattened when used for an intermediate sealed trait).
    val markedSum: Boolean = isMarkedSum[A]

    deriveEncoderFromCtxAndAdaptForEntrypoint[A, KindlingsEncoder[A]]("KindlingsEncoder.derived") { fromCtx =>
      ValDefs.createVal[Configuration](configExpr).use { configVal =>
        Expr.quote {
          val cfg = Expr.splice(configVal)
          Expr.splice {
            if (markedSum)
              Expr.quote {
                hearth.kindlings.circederivation.internal.runtime.CirceDerivationFactories.sumEncoderInstance[A] {
                  (a: A) =>
                    val _ = a
                    Expr.splice {
                      fromCtx(EncoderCtx.from(Expr.quote(a), Expr.quote(cfg), derivedType = selfType, evConfig))
                    }
                }
              }
            else
              Expr.quote {
                hearth.kindlings.circederivation.internal.runtime.CirceDerivationFactories.encoderInstance[A] {
                  (a: A) =>
                    val _ = a
                    Expr.splice {
                      fromCtx(EncoderCtx.from(Expr.quote(a), Expr.quote(cfg), derivedType = selfType, evConfig))
                    }
                }
              }
          }
        }
      }
    }
  }

  @scala.annotation.nowarn("msg=is never used")
  def deriveEncoderAsObjectTypeClass[A: Type](configExpr: Expr[Configuration]): Expr[KindlingsEncoderAsObject[A]] = {
    // Compile-time validation: like circe's semiauto/`derives`, only products and sums encode to JSON objects.
    val evConfigForCheck: Option[Configuration] = semiEvalConfig(configExpr)
    evaluatedConfigFlags = evConfigForCheck
    // A sum of singletons with a configuration only known at runtime is allowed (enumAsStrings fails at runtime).
    nonObjectReason[A](evConfigForCheck).filterNot(_ == maybeObject).foreach { reason =>
      Environment.reportErrorAndAbort(
        s"KindlingsEncoder.deriveAsObject: ${Type[A].prettyPrint} does not encode to a JSON object ($reason). " +
          "Use KindlingsEncoder.derived instead."
      )
    }

    implicit val KindlingsEncoderAsObjectA: Type[KindlingsEncoderAsObject[A]] = Types.KindlingsEncoderAsObject[A]
    implicit val JsonObjectT: Type[JsonObject] = Types.JsonObject
    implicit val JsonT: Type[Json] = Types.Json
    implicit val ConfigT: Type[Configuration] = Types.Configuration
    val selfType: Option[??] = Some(Type[A].as_??)
    val evConfig: Option[Configuration] = semiEvalConfig(configExpr)
    evaluatedConfigFlags = evConfig

    deriveEncoderFromCtxAndAdaptForEntrypoint[A, KindlingsEncoderAsObject[A]]("KindlingsEncoder.deriveAsObject") {
      fromCtx =>
        ValDefs.createVal[Configuration](configExpr).use { configVal =>
          Expr.quote {
            val cfg = Expr.splice(configVal)
            // (no local `val` around the splice: lambdas built inside it, e.g. for an `Option` of a value class, would
            // be owned by a different symbol than the `val`, which Scala 3 rejects)
            val encodeObject: A => JsonObject = (a: A) => {
              val _ = a
              hearth.kindlings.circederivation.internal.runtime.CirceDerivationUtils.asJsonObjectOrThrow(Expr.splice {
                fromCtx(EncoderCtx.from(Expr.quote(a), Expr.quote(cfg), derivedType = selfType, evConfig))
              })
            }
            // A derived sum is marked as a KindlingsSumEncoder (flattened when used for an intermediate sealed trait).
            if (Expr.splice(Expr(isMarkedSum[A])))
              hearth.kindlings.circederivation.internal.runtime.CirceDerivationFactories
                .sumEncoderAsObjectInstance[A](encodeObject)
            else
              hearth.kindlings.circederivation.internal.runtime.CirceDerivationFactories
                .encoderAsObjectInstance[A](encodeObject)
          }
        }
    }
  }

  /** `KindlingsEncoder.deriveUnwrapped[A]`: like generic-extras' `deriveUnwrappedEncoder`, a single-field case class is
    * encoded as its field's JSON.
    */
  @scala.annotation.nowarn("msg=is never used")
  def deriveUnwrappedEncoderTypeClass[A: Type](configExpr: Expr[Configuration]): Expr[KindlingsEncoder[A]] = {
    val singleField: (CaseClass[A], Parameter) = singleFieldCaseClass[A]("KindlingsEncoder.deriveUnwrapped")
    val caseClass: CaseClass[A] = singleField._1
    val param: Parameter = singleField._2
    import param.tpe.Underlying as Field
    val evConfig: Option[Configuration] = semiEvalConfig(configExpr)
    evaluatedConfigFlags = evConfig

    unwrappedEncoderExpr[A, Field](caseClass, param.name, configExpr, evConfig)
  }

  @scala.annotation.nowarn("msg=is never used")
  private def unwrappedEncoderExpr[A: Type, Field: Type](
      caseClass: CaseClass[A],
      fieldName: String,
      configExpr: Expr[Configuration],
      evConfig: Option[Configuration]
  ): Expr[KindlingsEncoder[A]] = {
    implicit val KindlingsEncoderA: Type[KindlingsEncoder[A]] = Types.KindlingsEncoder[A]
    implicit val ConfigT: Type[Configuration] = Types.Configuration
    def fieldOf(a: Expr[A]): Expr[Field] =
      caseClass
        .caseFieldValuesAt(a)
        .collectFirst { case (name, v) if name == fieldName => v.value.asInstanceOf[Expr[Field]] }
        .getOrElse(Environment.reportErrorAndAbort(s"Cannot access field $fieldName"))
    deriveEncoderFromCtxAndAdaptForEntrypoint[Field, KindlingsEncoder[A]]("KindlingsEncoder.deriveUnwrapped") {
      fromCtx =>
        def encode(a: Expr[A], cfg: Expr[Configuration]): Expr[Json] =
          fromCtx(EncoderCtx.from[Field](fieldOf(a), cfg, derivedType = None, evConfig))
        ValDefs.createVal[Configuration](configExpr).use { configVal =>
          Expr.quote {
            val cfg = Expr.splice(configVal)
            hearth.kindlings.circederivation.internal.runtime.CirceDerivationFactories.encoderInstance[A] { (a: A) =>
              val _ = a
              Expr.splice(encode(Expr.quote(a), Expr.quote(cfg)))
            }
          }
        }
    }
  }

  /** Reason returned by [[nonObjectReason]] when it depends on a configuration only known at runtime. */
  final protected val maybeObject: String =
    "enumAsStrings may encode the cases as JSON strings and the configuration is not known at compile time"

  /** Why `A` may not encode to a JSON object, or `None` when it always does (a product or a sum, as in circe). */
  protected def nonObjectReason[A: Type](evConfig: Option[Configuration]): Option[String] = {
    // IsOption/IsCollection/IsValueType need the standard extensions; this runs before the derivation's MIO run.
    ensureStandardExtensionsLoaded().unsafe.runSync: Unit
    if (Type[A].isTuple && !Type[A].isNamedTuple) Some("a tuple is encoded as a JSON array")
    else if (Type[A].isNamedTuple) None
    else
      Type[A] match {
        case IsOption(_)     => Some("an Option is encoded as its value or null")
        case IsCollection(_) => Some("a collection or map is not a product or a sum")
        case IsValueType(_)  =>
          if (evConfig.exists(!_.unwrapValueClasses) && anyValCaseClass[A].isDefined) None
          else Some("a value class is encoded as its underlying value")
        case _ =>
          if (Type[A].isEnumeration || Type[A].isJavaEnum)
            Some("an Enumeration/Java enum is not a sealed product hierarchy")
          else if (CaseClass.parse[A].toOption.isDefined && Enum.parse[A].toOption.isEmpty) None
          else
            Enum.parse[A].toOption match {
              case Some(enumm) =>
                val leaves = flattenedChildren[A](enumm)(_ => false)
                val allSingletons = leaves.forall { case (_, c) => SingletonValue.unapply(c.Underlying).isDefined }
                if (!allSingletons || evConfig.exists(!_.enumAsStrings)) None
                else if (evConfig.exists(_.enumAsStrings)) Some("enumAsStrings encodes the cases as JSON strings")
                else Some(maybeObject)
              case None =>
                if (CaseClass.parse[A].toOption.isDefined) None
                else Some("it is not a case class, sealed trait or named tuple")
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
                _ <- ensureStandardExtensionsLoaded()
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
        errorRendering = if (shouldWeLogEncoderDerivation) RenderFrom(Log.Level.Info) else DontRender,
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
      derivedType: Option[??],
      evaluatedConfig: Option[Configuration] = None
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
        derivedType: Option[??],
        evaluatedConfig: Option[Configuration] = None
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
    def apply[A: EncoderCtx]: MIO[Rule.Applicability[Expr[Json]]]
  }

  // The actual derivation logic

  def deriveEncoderRecursively[A: EncoderCtx]: MIO[Expr[Json]] =
    // Cache ALL types (including built-ins, options, collections) as helper defs.
    // Without this, built-in types are re-derived for every field reference, causing
    // O(n*m) compilation time for large type graphs (n types × m fields).
    // The forward-declaration also breaks recursive self-references (e.g., Node → List[Node]).
    ectx.getHelper[A].flatMap {
      case Some(helperCall) =>
        Log.info(s"Using cached encoder for ${Type[A].prettyPrint}") >>
          MIO.pure(helperCall(ectx.value, ectx.config))
      case None =>
        for {
          _ <- ectx.setHelper[A] { (value, config) =>
            deriveEncoderViaRules[A](using ectx.nestInCache(value, config))
          }
          helper <- ectx.getHelper[A]
        } yield helper.get(ectx.value, ectx.config)
    }

  private def deriveEncoderViaRules[A: EncoderCtx]: MIO[Expr[Json]] =
    Log
      .namedScope(s"Deriving encoder for type ${Type[A].prettyPrint}") {
        Rules(
          // Note: EncoderUseCachedDefWhenAvailableRule is NOT included here because
          // caching is handled by deriveEncoderRecursively (which wraps in setHelper).
          // Including it would match the forward-declared helper and create a self-referential loop.
          EncoderHandleAsLiteralTypeRule,
          EncoderUseImplicitWhenAvailableRule,
          EncoderDerivationPolicyRule,
          EncoderHandleAsBuiltInTypeRule,
          EncoderHandleAsValueTypeRule,
          EncoderHandleAsOptionRule,
          // EncoderHandleAsCollectionRule now handles maps too (single IsCollection parse + IsMapOf dispatch),
          // so the standalone EncoderHandleAsMapRule is no longer in the chain (its object is kept for
          // deriveMapEntries / deriveKeyEncoder).
          EncoderHandleAsCollectionRule,
          EncoderHandleAsNamedTupleRule,
          EncoderHandleAsTupleRule,
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

    def Encoder: Type.Ctor1[Encoder] = Type.Ctor1.of[Encoder]
    def KeyEncoder: Type.Ctor1[KeyEncoder] = Type.Ctor1.of[KeyEncoder]
    def KindlingsEncoder: Type.Ctor1[KindlingsEncoder] = Type.Ctor1.of[KindlingsEncoder]
    def KindlingsEncoderAsObject: Type.Ctor1[KindlingsEncoderAsObject] = Type.Ctor1.of[KindlingsEncoderAsObject]
    val EncoderLogDerivation: Type[hearth.kindlings.circederivation.KindlingsEncoder.LogDerivation] =
      Type.of[hearth.kindlings.circederivation.KindlingsEncoder.LogDerivation]
    val Json: Type.Lazy[Json] = Type.Lazy(Type.of[Json])
    val JsonObject: Type.Lazy[JsonObject] = Type.Lazy(Type.of[JsonObject])
    val Configuration: Type.Lazy[Configuration] = Type.Lazy(Type.of[Configuration])
    val String: Type.Lazy[String] = Type.Lazy(Type.of[String])
    val FieldName: Type.Lazy[fieldName] = Type.Lazy(Type.of[fieldName])
    val TransientField: Type.Lazy[transientField] = Type.Lazy(Type.of[transientField])
    val Int: Type.Lazy[Int] = Type.Lazy(Type.of[Int])
    val Product: Type.Lazy[Product] = Type.Lazy(Type.of[Product])
  }
}
