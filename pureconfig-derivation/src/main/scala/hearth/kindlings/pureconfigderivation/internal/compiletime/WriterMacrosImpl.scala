package hearth.kindlings.pureconfigderivation.internal.compiletime

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

import com.typesafe.config.ConfigValue
import hearth.kindlings.pureconfigderivation.{
  KindlingsConfigWriter,
  KindlingsCoproductHint,
  KindlingsProductHint,
  PureConfig
}
import hearth.kindlings.pureconfigderivation.annotations.{configKey, transientField}
import pureconfig.ConfigWriter

trait WriterMacrosImpl
    extends rules.WriterUseCachedDefWhenAvailableRuleImpl
    with rules.WriterUseImplicitWhenAvailableRuleImpl
    with rules.WriterHandleAsValueTypeRuleImpl
    with rules.WriterHandleAsOptionRuleImpl
    with rules.WriterHandleAsMapRuleImpl
    with rules.WriterHandleAsCollectionRuleImpl
    with rules.WriterHandleAsNamedTupleRuleImpl
    with rules.WriterHandleAsSingletonRuleImpl
    with rules.WriterHandleAsCaseClassRuleImpl
    with rules.WriterHandleAsEnumRuleImpl {
  this: MacroCommons & StdExtensions & AnnotationSupport & LoadStandardExtensionsOnce =>

  // Entrypoints

  @scala.annotation.nowarn("msg=is never used")
  def deriveWriterTypeClass[A: Type](configExpr: Expr[PureConfig]): Expr[KindlingsConfigWriter[A]] = {
    implicit val WriterA: Type[ConfigWriter[A]] = WTypes.ConfigWriter[A]
    implicit val KindlingsWriterA: Type[KindlingsConfigWriter[A]] = WTypes.KindlingsConfigWriter[A]
    implicit val ConfigValueT: Type[ConfigValue] = WTypes.ConfigValue
    implicit val PureConfigT: Type[PureConfig] = WTypes.PureConfig
    val selfType: Option[??] = Some(Type[A].as_??)

    deriveWriterFromCtxAndAdaptForEntrypoint[A, KindlingsConfigWriter[A]]("KindlingsConfigWriter.derived") { fromCtx =>
      ValDefs.createVal[PureConfig](configExpr).use { configVal =>
        Expr.quote {
          val cfg = Expr.splice(configVal)
          new KindlingsConfigWriter[A] {
            def to(a: A): ConfigValue = {
              val _ = a
              Expr.splice {
                fromCtx(WriterCtx.from(Expr.quote(a), Expr.quote(cfg), derivedType = selfType))
              }
            }
          }
        }
      }
    }
  }

  // Handles logging, error reporting and prepending "cached" defs and vals to the result.

  def deriveWriterFromCtxAndAdaptForEntrypoint[A: Type, Out: Type](macroName: String)(
      provideCtxAndAdapt: (WriterCtx[A] => Expr[ConfigValue]) => Expr[Out]
  ): Expr[Out] = {
    if (Type[A] =:= Type.of[Nothing].asInstanceOf[Type[A]] || Type[A] =:= Type.of[Any].asInstanceOf[Type[A]])
      Environment.reportErrorAndAbort(
        s"$macroName: type parameter was inferred as ${Type[A].prettyPrint}, which is likely unintended.\n" +
          s"Provide an explicit type parameter, e.g.: $macroName[MyType](...)\n" +
          "or add a type ascription to the result variable."
      )
    Log
      .namedScope(
        s"Deriving writer for ${Type[A].prettyPrint} at: ${Environment.currentPosition.prettyPrint}"
      ) {
        MIO.scoped { runSafe =>
          val fromCtx: WriterCtx[A] => Expr[ConfigValue] = (ctx: WriterCtx[A]) =>
            runSafe {
              for {
                _ <- ensureStandardExtensionsLoaded()
                result <- deriveWriterRecursively[A](using ctx)
                cache <- ctx.cache.get
              } yield cache.toValDefs.use(_ => result)
            }

          provideCtxAndAdapt(fromCtx)
        }
      }
      .flatTap { result =>
        Log.info(s"Derived final writer result: ${result.prettyPrint}")
      }
      .runToExprOrFail(
        macroName,
        infoRendering = if (shouldWeLogWriterDerivation) RenderFrom(Log.Level.Info) else DontRender,
        errorRendering = if (shouldWeLogWriterDerivation) RenderFrom(Log.Level.Info) else DontRender
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
          "Enable debug logging with: import hearth.kindlings.pureconfigderivation.debug.logDerivationForKindlingsConfigWriter or scalac option -Xmacro-settings:pureconfigDerivation.logDerivation=true"
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

  def shouldWeLogWriterDerivation: Boolean = {
    implicit val LogDerivationT: Type[KindlingsConfigWriter.LogDerivation] = WTypes.WriterLogDerivation
    def logDerivationImported = Expr.summonImplicit[KindlingsConfigWriter.LogDerivation].isDefined

    def logDerivationSetGlobally = (for {
      data <- Environment.typedSettings.toOption
      pcDerivation <- data.get("pureconfigDerivation")
      shouldLog <- pcDerivation.get("logDerivation").flatMap(_.asBoolean)
    } yield shouldLog).getOrElse(false)

    logDerivationImported || logDerivationSetGlobally
  }

  // Context

  final case class WriterCtx[A](
      tpe: Type[A],
      value: Expr[A],
      config: Expr[PureConfig],
      cache: MLocal[ValDefsCache],
      derivedType: Option[??]
  ) {

    def nest[B: Type](newValue: Expr[B]): WriterCtx[B] = copy[B](
      tpe = Type[B],
      value = newValue
    )

    def nestInCache(
        newValue: Expr[A],
        newConfig: Expr[PureConfig]
    ): WriterCtx[A] = copy(
      value = newValue,
      config = newConfig
    )

    def getInstance[B: Type]: MIO[Option[Expr[ConfigWriter[B]]]] = {
      implicit val WriterB: Type[ConfigWriter[B]] = WTypes.ConfigWriter[B]
      cache.get0Ary[ConfigWriter[B]]("cached-writer-instance")
    }
    def setInstance[B: Type](instance: Expr[ConfigWriter[B]]): MIO[Unit] = {
      implicit val WriterB: Type[ConfigWriter[B]] = WTypes.ConfigWriter[B]
      Log.info(s"Caching ConfigWriter instance for ${Type[B].prettyPrint}") >>
        cache.buildCachedWith(
          "cached-writer-instance",
          ValDefBuilder.ofLazy[ConfigWriter[B]](s"writer_${Type[B].shortName}")
        )(_ => instance)
    }

    def getHelper[B: Type]: MIO[Option[(Expr[B], Expr[PureConfig]) => Expr[ConfigValue]]] = {
      implicit val ConfigValueT: Type[ConfigValue] = WTypes.ConfigValue
      implicit val PureConfigT: Type[PureConfig] = WTypes.PureConfig
      cache.get2Ary[B, PureConfig, ConfigValue]("cached-write-method")
    }
    def setHelper[B: Type](
        helper: (Expr[B], Expr[PureConfig]) => MIO[Expr[ConfigValue]]
    ): MIO[Unit] = {
      implicit val ConfigValueT: Type[ConfigValue] = WTypes.ConfigValue
      implicit val PureConfigT: Type[PureConfig] = WTypes.PureConfig
      val defBuilder =
        ValDefBuilder.ofDef2[B, PureConfig, ConfigValue](s"write_${Type[B].shortName}")
      for {
        _ <- Log.info(s"Forward-declaring write helper for ${Type[B].prettyPrint}")
        _ <- cache.forwardDeclare("cached-write-method", defBuilder)
        _ <- MIO.scoped { runSafe =>
          runSafe(cache.buildCachedWith("cached-write-method", defBuilder) { case (_, (value, config)) =>
            runSafe(helper(value, config))
          })
        }
        _ <- Log.info(s"Defined write helper for ${Type[B].prettyPrint}")
      } yield ()
    }

    override def toString: String =
      s"write[${tpe.prettyPrint}](value = ${value.prettyPrint}, config = ${config.prettyPrint})"
  }
  object WriterCtx {

    def from[A: Type](
        value: Expr[A],
        config: Expr[PureConfig],
        derivedType: Option[??]
    ): WriterCtx[A] = WriterCtx(
      tpe = Type[A],
      value = value,
      config = config,
      cache = ValDefsCache.mlocal,
      derivedType = derivedType
    )
  }

  def wctx[A](implicit A: WriterCtx[A]): WriterCtx[A] = A

  implicit def currentWriterValueType[A: WriterCtx]: Type[A] = wctx.tpe

  abstract class WriterDerivationRule(val name: String) extends Rule {
    def apply[A: WriterCtx]: MIO[Rule.Applicability[Expr[ConfigValue]]]
  }

  // The actual derivation logic

  def deriveWriterRecursively[A: WriterCtx]: MIO[Expr[ConfigValue]] =
    Log
      .namedScope(s"Deriving writer for type ${Type[A].prettyPrint}") {
        wctx.getHelper[A].flatMap {
          case Some(helperCall) =>
            Log.info(s"Using cached writer helper for ${Type[A].prettyPrint}") >>
              MIO.pure(helperCall(wctx.value, wctx.config))
          case None =>
            wctx.setHelper[A] { (value, config) =>
              deriveWriterViaRules[A](using wctx.nestInCache(value, config))
            } >> wctx.getHelper[A].flatMap {
              case Some(helperCall) =>
                MIO.pure(helperCall(wctx.value, wctx.config))
              case None =>
                deriveWriterViaRules[A]
            }
        }
      }

  private def deriveWriterViaRules[A: WriterCtx]: MIO[Expr[ConfigValue]] =
    Log
      .namedScope(s"Deriving writer via rules for type ${Type[A].prettyPrint}") {
        Rules(
          WriterUseImplicitWhenAvailableRule,
          WriterHandleAsValueTypeRule,
          WriterHandleAsOptionRule,
          WriterHandleAsMapRule,
          WriterHandleAsCollectionRule,
          WriterHandleAsNamedTupleRule,
          WriterHandleAsSingletonRule,
          WriterHandleAsCaseClassRule,
          WriterHandleAsEnumRule
        )(_[A]).flatMap {
          case Right(result) =>
            Log.info(s"Derived writer for ${Type[A].prettyPrint}: ${result.prettyPrint}") >>
              MIO.pure(result)
          case Left(reasons) =>
            val reasonsStrings = reasons.toListMap.view.map { case (rule, reasons) =>
              if (reasons.isEmpty) s"The rule ${rule.name} was not applicable"
              else
                s" - The rule ${rule.name} was not applicable, for the following reasons: ${reasons.mkString(", ")}"
            }.toList
            val err = WriterDerivationError.UnsupportedType(Type[A].prettyPrint, reasonsStrings)
            Log.error(err.message) >> MIO.fail(err)
        }
      }

  // Types

  private[compiletime] object WTypes {

    def ConfigWriter: Type.Ctor1[ConfigWriter] = Type.Ctor1.of[ConfigWriter]
    def KindlingsConfigWriter: Type.Ctor1[KindlingsConfigWriter] = Type.Ctor1.of[KindlingsConfigWriter]
    val WriterLogDerivation: Type[hearth.kindlings.pureconfigderivation.KindlingsConfigWriter.LogDerivation] =
      Type.of[hearth.kindlings.pureconfigderivation.KindlingsConfigWriter.LogDerivation]
    val ConfigValue: Type[ConfigValue] = Type.of[ConfigValue]
    val PureConfig: Type[PureConfig] = Type.of[PureConfig]
    val String: Type[String] = Type.of[String]
    val StringToString: Type[String => String] = Type.of[String => String]
    val OptionString: Type[Option[String]] = Type.of[Option[String]]
    val ConfigKey: Type[configKey] = Type.of[configKey]
    val TransientField: Type[transientField] = Type.of[transientField]
    val Int: Type[Int] = Type.of[Int]
    val Product: Type[Product] = Type.of[Product]

    def kindlingsProductHintType[A: Type]: Type[KindlingsProductHint[A]] =
      Type.of[KindlingsProductHint[A]]

    def fieldCoproductHintType[A: Type]: Type[KindlingsCoproductHint.Field[A]] =
      Type.of[KindlingsCoproductHint.Field[A]]

    def wrappedCoproductHintType[A: Type]: Type[KindlingsCoproductHint.Wrapped[A]] =
      Type.of[KindlingsCoproductHint.Wrapped[A]]
  }
}
