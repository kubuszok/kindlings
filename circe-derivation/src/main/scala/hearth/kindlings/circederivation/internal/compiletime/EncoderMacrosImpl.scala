package hearth.kindlings.circederivation.internal.compiletime

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

import hearth.kindlings.circederivation.{Configuration, KindlingsEncoder, KindlingsEncoderAsObject}
import hearth.kindlings.circederivation.annotations.{fieldName, transientField}
import io.circe.{Encoder, Json, JsonObject, KeyEncoder}

trait EncoderMacrosImpl
    extends rules.EncoderUseCachedDefWhenAvailableRuleImpl
    with rules.EncoderUseImplicitWhenAvailableRuleImpl
    with rules.EncoderHandleAsLiteralTypeRuleImpl
    with rules.EncoderHandleAsValueTypeRuleImpl
    with rules.EncoderHandleAsOptionRuleImpl
    with rules.EncoderHandleAsMapRuleImpl
    with rules.EncoderHandleAsCollectionRuleImpl
    with rules.EncoderHandleAsNamedTupleRuleImpl
    with rules.EncoderHandleAsSingletonRuleImpl
    with rules.EncoderHandleAsCaseClassRuleImpl
    with rules.EncoderHandleAsEnumRuleImpl { this: MacroCommons & StdExtensions & AnnotationSupport =>

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
          EncoderHandleAsValueTypeRule,
          EncoderHandleAsOptionRule,
          EncoderHandleAsMapRule,
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
