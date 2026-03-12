package hearth.kindlings.avroderivation.internal.compiletime

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

import hearth.kindlings.avroderivation.{AvroConfig, AvroEncoder, DecimalConfig}
import hearth.kindlings.avroderivation.annotations.{avroFixed, fieldName, transientField}
import org.apache.avro.Schema

trait EncoderMacrosImpl
    extends rules.AvroEncoderUseCachedDefWhenAvailableRuleImpl
    with rules.AvroEncoderUseImplicitWhenAvailableRuleImpl
    with rules.AvroEncoderHandleAsLiteralTypeRuleImpl
    with rules.AvroEncoderUseBuiltInSupportRuleImpl
    with rules.AvroEncoderHandleAsValueTypeRuleImpl
    with rules.AvroEncoderHandleAsOptionRuleImpl
    with rules.AvroEncoderHandleAsEitherRuleImpl
    with rules.AvroEncoderHandleAsMapRuleImpl
    with rules.AvroEncoderHandleAsCollectionRuleImpl
    with rules.AvroEncoderHandleAsNamedTupleRuleImpl
    with rules.AvroEncoderHandleAsSingletonRuleImpl
    with rules.AvroEncoderHandleAsCaseClassRuleImpl
    with rules.AvroEncoderHandleAsEnumRuleImpl {
  this: MacroCommons & StdExtensions & SchemaForMacrosImpl & AnnotationSupport =>

  // Entrypoints

  def deriveInlineEncode[A: Type](valueExpr: Expr[A], configExpr: Expr[AvroConfig]): Expr[Any] = {
    implicit val AnyT: Type[Any] = EncTypes.Any
    implicit val ConfigT: Type[AvroConfig] = EncTypes.AvroConfig

    deriveEncoderFromCtxAndAdaptForEntrypoint[A, Any]("AvroEncoder.encode") { fromCtx =>
      ValDefs.createVal[A](valueExpr).use { valueVal =>
        ValDefs.createVal[AvroConfig](configExpr).use { configVal =>
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
  def deriveEncoderTypeClass[A: Type](configExpr: Expr[AvroConfig]): Expr[AvroEncoder[A]] = {
    implicit val AvroEncoderA: Type[AvroEncoder[A]] = EncTypes.AvroEncoder[A]
    implicit val AnyT: Type[Any] = EncTypes.Any
    implicit val SchemaT: Type[Schema] = EncTypes.Schema
    implicit val ConfigT: Type[AvroConfig] = EncTypes.AvroConfig
    val selfType: Option[??] = Some(Type[A].as_??)

    if (Type[A] =:= Type.of[Nothing].asInstanceOf[Type[A]] || Type[A] =:= Type.of[Any].asInstanceOf[Type[A]])
      Environment.reportErrorAndAbort(
        s"AvroEncoder.derived: type parameter was inferred as ${Type[A].prettyPrint}, which is likely unintended.\n" +
          "Provide an explicit type parameter, e.g.: AvroEncoder.derived[MyType]\n" +
          "or add a type ascription to the result variable."
      )

    // Schema and encoder are derived in the same MIO.scoped block to avoid Scala 3 splice isolation issues.
    Log
      .namedScope(
        s"Deriving encoder for ${Type[A].prettyPrint} at: ${Environment.currentPosition.prettyPrint}"
      ) {
        MIO.scoped { runSafe =>
          // Derive schema with its own cache (self-contained)
          val schemaExpr: Expr[Schema] = runSafe {
            deriveSelfContainedSchema[A](configExpr)
          }

          // Create encoder derivation callback
          val fromCtx: (EncoderCtx[A] => Expr[Any]) = (ctx: EncoderCtx[A]) =>
            runSafe {
              for {
                _ <- Environment.loadStandardExtensions().toMIO(allowFailures = false)
                result <- deriveEncoderRecursively[A](using ctx)
                cache <- ctx.cache.get
              } yield cache.toValDefs.use(_ => result)
            }

          // Assemble the type class instance
          ValDefs.createVal[AvroConfig](configExpr).use { configVal =>
            Expr.quote {
              val cfg = Expr.splice(configVal)
              val sch = Expr.splice(schemaExpr)
              (new AvroEncoder[A] {
                val schema: Schema = sch
                def encode(value: A): Any = {
                  val _ = value
                  val _ = cfg
                  Expr.splice {
                    fromCtx(EncoderCtx.from(Expr.quote(value), Expr.quote(cfg), derivedType = selfType))
                  }
                }
              }): AvroEncoder[A]
            }
          }
        }
      }
      .flatTap { result =>
        Log.info(s"Derived final encoder result: ${result.prettyPrint}")
      }
      .runToExprOrFail(
        "AvroEncoder.derived",
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
          "Enable debug logging with: import hearth.kindlings.avroderivation.debug.logDerivationForAvroEncoder or scalac option -Xmacro-settings:avroDerivation.logDerivation=true"
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

  def deriveEncoderFromCtxAndAdaptForEntrypoint[A: Type, Out: Type](macroName: String)(
      provideCtxAndAdapt: (EncoderCtx[A] => Expr[Any]) => Expr[Out]
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
          val fromCtx: (EncoderCtx[A] => Expr[Any]) = (ctx: EncoderCtx[A]) =>
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
          "Enable debug logging with: import hearth.kindlings.avroderivation.debug.logDerivationForAvroEncoder or scalac option -Xmacro-settings:avroDerivation.logDerivation=true"
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
    implicit val LogDerivation: Type[AvroEncoder.LogDerivation] = EncTypes.EncoderLogDerivation
    def logDerivationImported = Expr.summonImplicit[AvroEncoder.LogDerivation].isDefined

    def logDerivationSetGlobally = (for {
      data <- Environment.typedSettings.toOption
      avroDerivation <- data.get("avroDerivation")
      shouldLog <- avroDerivation.get("logDerivation").flatMap(_.asBoolean)
    } yield shouldLog).getOrElse(false)

    logDerivationImported || logDerivationSetGlobally
  }

  // Context

  final case class EncoderCtx[A](
      tpe: Type[A],
      value: Expr[A],
      config: Expr[AvroConfig],
      cache: MLocal[ValDefsCache],
      derivedType: Option[??]
  ) {

    def nest[B: Type](newValue: Expr[B]): EncoderCtx[B] = copy[B](
      tpe = Type[B],
      value = newValue
    )

    def nestInCache(
        newValue: Expr[A],
        newConfig: Expr[AvroConfig]
    ): EncoderCtx[A] = copy(
      value = newValue,
      config = newConfig
    )

    def getInstance[B: Type]: MIO[Option[Expr[AvroEncoder[B]]]] = {
      implicit val EncoderB: Type[AvroEncoder[B]] = EncTypes.AvroEncoder[B]
      cache.get0Ary[AvroEncoder[B]]("cached-encoder-instance")
    }
    def setInstance[B: Type](instance: Expr[AvroEncoder[B]]): MIO[Unit] = {
      implicit val EncoderB: Type[AvroEncoder[B]] = EncTypes.AvroEncoder[B]
      Log.info(s"Caching AvroEncoder instance for ${Type[B].prettyPrint}") >>
        cache.buildCachedWith(
          "cached-encoder-instance",
          ValDefBuilder.ofLazy[AvroEncoder[B]](s"encoder_${Type[B].shortName}")
        )(_ => instance)
    }

    def getHelper[B: Type]: MIO[Option[(Expr[B], Expr[AvroConfig]) => Expr[Any]]] = {
      implicit val AnyT: Type[Any] = EncTypes.Any
      implicit val ConfigT: Type[AvroConfig] = EncTypes.AvroConfig
      cache.get2Ary[B, AvroConfig, Any]("cached-encode-method")
    }
    def setHelper[B: Type](
        helper: (Expr[B], Expr[AvroConfig]) => MIO[Expr[Any]]
    ): MIO[Unit] = {
      implicit val AnyT: Type[Any] = EncTypes.Any
      implicit val ConfigT: Type[AvroConfig] = EncTypes.AvroConfig
      val defBuilder =
        ValDefBuilder.ofDef2[B, AvroConfig, Any](s"encode_${Type[B].shortName}")
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
        config: Expr[AvroConfig],
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
    def apply[A: EncoderCtx]: MIO[Rule.Applicability[Expr[Any]]]
  }

  // The actual derivation logic

  def deriveEncoderRecursively[A: EncoderCtx]: MIO[Expr[Any]] =
    Log
      .namedScope(s"Deriving encoder for type ${Type[A].prettyPrint}") {
        Rules(
          AvroEncoderUseCachedDefWhenAvailableRule,
          AvroEncoderHandleAsLiteralTypeRule,
          AvroEncoderUseImplicitWhenAvailableRule,
          AvroEncoderUseBuiltInSupportRule,
          AvroEncoderHandleAsValueTypeRule,
          AvroEncoderHandleAsOptionRule,
          AvroEncoderHandleAsEitherRule,
          AvroEncoderHandleAsMapRule,
          AvroEncoderHandleAsCollectionRule,
          AvroEncoderHandleAsNamedTupleRule,
          AvroEncoderHandleAsSingletonRule,
          AvroEncoderHandleAsCaseClassRule,
          AvroEncoderHandleAsEnumRule
        )(_[A]).flatMap {
          case Right(result) =>
            Log.info(s"Derived encoder for ${Type[A].prettyPrint}: ${result.prettyPrint}") >>
              MIO.pure(result)
          case Left(reasons) =>
            val reasonsStrings = reasons.toListMap
              .removed(AvroEncoderUseCachedDefWhenAvailableRule)
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

  // Types

  private[compiletime] object EncTypes {

    def AvroEncoder: Type.Ctor1[AvroEncoder] = Type.Ctor1.of[AvroEncoder]
    val EncoderLogDerivation: Type[hearth.kindlings.avroderivation.AvroEncoder.LogDerivation] =
      Type.of[hearth.kindlings.avroderivation.AvroEncoder.LogDerivation]
    val Schema: Type[Schema] = Type.of[Schema]
    val AvroConfig: Type[AvroConfig] = Type.of[AvroConfig]
    val DecimalConfig: Type[DecimalConfig] = Type.of[DecimalConfig]
    val String: Type[String] = Type.of[String]
    val Any: Type[Any] = Type.of[Any]
    val Int: Type[Int] = Type.of[Int]
    val Product: Type[Product] = Type.of[Product]
    val FieldName: Type[fieldName] = Type.of[fieldName]
    val TransientField: Type[transientField] = Type.of[transientField]
    val AvroFixed: Type[avroFixed] = Type.of[avroFixed]
  }
}
