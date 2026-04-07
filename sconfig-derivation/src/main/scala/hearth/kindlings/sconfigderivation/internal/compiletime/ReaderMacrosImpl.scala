package hearth.kindlings.sconfigderivation.internal.compiletime

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

import hearth.kindlings.sconfigderivation.{
  ConfigDecodingError,
  ConfigReader,
  CoproductHint,
  ProductHint,
  SConfig
}
import hearth.kindlings.sconfigderivation.annotations.{configKey, transientField}
import org.ekrich.config.ConfigValue

trait ReaderMacrosImpl
    extends rules.ReaderUseCachedDefWhenAvailableRuleImpl
    with rules.ReaderUseImplicitWhenAvailableRuleImpl
    with rules.ReaderHandleAsValueTypeRuleImpl
    with rules.ReaderHandleAsOptionRuleImpl
    with rules.ReaderHandleAsMapRuleImpl
    with rules.ReaderHandleAsCollectionRuleImpl
    with rules.ReaderHandleAsNamedTupleRuleImpl
    with rules.ReaderHandleAsSingletonRuleImpl
    with rules.ReaderHandleAsCaseClassRuleImpl
    with rules.ReaderHandleAsEnumRuleImpl {
  this: MacroCommons & StdExtensions & AnnotationSupport & LoadStandardExtensionsOnce =>

  // Entrypoints

  @scala.annotation.nowarn("msg=is never used")
  def deriveReaderTypeClass[A: Type](configExpr: Expr[SConfig]): Expr[ConfigReader[A]] = {
    implicit val ConfigReaderA: Type[ConfigReader[A]] = RTypes.ConfigReader[A]
    implicit val ConfigValueT: Type[ConfigValue] = RTypes.ConfigValue
    implicit val SConfigT: Type[SConfig] = RTypes.SConfig
    implicit val ErrorT: Type[ConfigDecodingError] = RTypes.ConfigDecodingError
    implicit val ResultA: Type[Either[ConfigDecodingError, A]] = RTypes.ReaderResult[A]
    val selfType: Option[??] = Some(Type[A].as_??)

    deriveReaderFromCtxAndAdaptForEntrypoint[A, ConfigReader[A]]("ConfigReader.derived") { fromCtx =>
      ValDefs.createVal[SConfig](configExpr).use { configVal =>
        Expr.quote {
          val cfg = Expr.splice(configVal)
          new ConfigReader[A] {
            def from(value: ConfigValue): Either[ConfigDecodingError, A] = {
              val _ = value
              Expr.splice {
                fromCtx(ReaderCtx.from(Expr.quote(value), Expr.quote(cfg), derivedType = selfType))
              }
            }
          }
        }
      }
    }
  }

  def deriveReaderFromCtxAndAdaptForEntrypoint[A: Type, Out: Type](macroName: String)(
      provideCtxAndAdapt: (ReaderCtx[A] => Expr[Either[ConfigDecodingError, A]]) => Expr[Out]
  ): Expr[Out] = {
    if (Type[A] =:= Type.of[Nothing].asInstanceOf[Type[A]] || Type[A] =:= Type.of[Any].asInstanceOf[Type[A]])
      Environment.reportErrorAndAbort(
        s"$macroName: type parameter was inferred as ${Type[A].prettyPrint}, which is likely unintended.\n" +
          s"Provide an explicit type parameter, e.g.: $macroName[MyType](...)"
      )
    Log
      .namedScope(s"Deriving reader for ${Type[A].prettyPrint} at: ${Environment.currentPosition.prettyPrint}") {
        MIO.scoped { runSafe =>
          val fromCtx: ReaderCtx[A] => Expr[Either[ConfigDecodingError, A]] = (ctx: ReaderCtx[A]) =>
            runSafe {
              for {
                _ <- ensureStandardExtensionsLoaded()
                result <- deriveReaderRecursively[A](using ctx)
                cache <- ctx.cache.get
              } yield cache.toValDefs.use(_ => result)
            }
          provideCtxAndAdapt(fromCtx)
        }
      }
      .flatTap { result =>
        Log.info(s"Derived final reader result: ${result.prettyPrint}")
      }
      .runToExprOrFail(
        macroName,
        infoRendering = if (shouldWeLogReaderDerivation) RenderFrom(Log.Level.Info) else DontRender,
        errorRendering = if (shouldWeLogReaderDerivation) RenderFrom(Log.Level.Info) else DontRender
      ) { (errorLogs, errors) =>
        val errorsRendered = errors.map { e =>
          e.getMessage.split("\n").toList match {
            case head :: tail => (("  - " + head) :: tail.map("    " + _)).mkString("\n")
            case _            => "  - " + e.getMessage
          }
        }.mkString("\n")
        val hint =
          "Enable debug logging with: import hearth.kindlings.sconfigderivation.debug.logDerivationForConfigReader or scalac option -Xmacro-settings:sconfigDerivation.logDerivation=true"
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

  def shouldWeLogReaderDerivation: Boolean = {
    implicit val LogDerivationT: Type[ConfigReader.LogDerivation] = RTypes.ReaderLogDerivation
    def logDerivationImported = Expr.summonImplicit[ConfigReader.LogDerivation].isDefined
    def logDerivationSetGlobally = (for {
      data <- Environment.typedSettings.toOption
      sub <- data.get("sconfigDerivation")
      shouldLog <- sub.get("logDerivation").flatMap(_.asBoolean)
    } yield shouldLog).getOrElse(false)
    logDerivationImported || logDerivationSetGlobally
  }

  // Context

  final case class ReaderCtx[A](
      tpe: Type[A],
      value: Expr[ConfigValue],
      config: Expr[SConfig],
      cache: MLocal[ValDefsCache],
      derivedType: Option[??]
  ) {

    def nest[B: Type](newValue: Expr[ConfigValue]): ReaderCtx[B] = copy[B](
      tpe = Type[B],
      value = newValue
    )

    def nestInCache(
        newValue: Expr[ConfigValue],
        newConfig: Expr[SConfig]
    ): ReaderCtx[A] = copy(
      value = newValue,
      config = newConfig
    )

    def getInstance[B: Type]: MIO[Option[Expr[ConfigReader[B]]]] = {
      implicit val ReaderB: Type[ConfigReader[B]] = RTypes.ConfigReader[B]
      cache.get0Ary[ConfigReader[B]]("cached-reader-instance")
    }

    def setInstance[B: Type](instance: Expr[ConfigReader[B]]): MIO[Unit] = {
      implicit val ReaderB: Type[ConfigReader[B]] = RTypes.ConfigReader[B]
      Log.info(s"Caching ConfigReader instance for ${Type[B].prettyPrint}") >>
        cache.buildCachedWith(
          "cached-reader-instance",
          ValDefBuilder.ofLazy[ConfigReader[B]](s"reader_${Type[B].shortName}")
        )(_ => instance)
    }

    def getHelper[B: Type]
        : MIO[Option[(Expr[ConfigValue], Expr[SConfig]) => Expr[Either[ConfigDecodingError, B]]]] = {
      implicit val ResultB: Type[Either[ConfigDecodingError, B]] = RTypes.ReaderResult[B]
      implicit val ConfigValueT: Type[ConfigValue] = RTypes.ConfigValue
      implicit val SConfigT: Type[SConfig] = RTypes.SConfig
      cache.get2Ary[ConfigValue, SConfig, Either[ConfigDecodingError, B]]("cached-read-method")
    }

    def setHelper[B: Type](
        helper: (Expr[ConfigValue], Expr[SConfig]) => MIO[Expr[Either[ConfigDecodingError, B]]]
    ): MIO[Unit] = {
      implicit val ResultB: Type[Either[ConfigDecodingError, B]] = RTypes.ReaderResult[B]
      implicit val ConfigValueT: Type[ConfigValue] = RTypes.ConfigValue
      implicit val SConfigT: Type[SConfig] = RTypes.SConfig
      val defBuilder = ValDefBuilder
        .ofDef2[ConfigValue, SConfig, Either[ConfigDecodingError, B]](s"read_${Type[B].shortName}")
      for {
        _ <- Log.info(s"Forward-declaring read helper for ${Type[B].prettyPrint}")
        _ <- cache.forwardDeclare("cached-read-method", defBuilder)
        _ <- MIO.scoped { runSafe =>
          runSafe(cache.buildCachedWith("cached-read-method", defBuilder) { case (_, (v, config)) =>
            runSafe(helper(v, config))
          })
        }
        _ <- Log.info(s"Defined read helper for ${Type[B].prettyPrint}")
      } yield ()
    }

    override def toString: String =
      s"read[${tpe.prettyPrint}](value = ${value.prettyPrint}, config = ${config.prettyPrint})"
  }
  object ReaderCtx {
    def from[A: Type](
        value: Expr[ConfigValue],
        config: Expr[SConfig],
        derivedType: Option[??]
    ): ReaderCtx[A] = ReaderCtx(
      tpe = Type[A],
      value = value,
      config = config,
      cache = ValDefsCache.mlocal,
      derivedType = derivedType
    )
  }

  def rctx[A](implicit A: ReaderCtx[A]): ReaderCtx[A] = A

  implicit def currentReaderValueType[A: ReaderCtx]: Type[A] = rctx.tpe

  abstract class ReaderDerivationRule(val name: String) extends Rule {
    def apply[A: ReaderCtx]: MIO[Rule.Applicability[Expr[Either[ConfigDecodingError, A]]]]
  }

  // The actual derivation logic

  def deriveReaderRecursively[A: ReaderCtx]: MIO[Expr[Either[ConfigDecodingError, A]]] =
    Log
      .namedScope(s"Deriving reader for type ${Type[A].prettyPrint}") {
        rctx.getHelper[A].flatMap {
          case Some(helperCall) =>
            Log.info(s"Using cached reader helper for ${Type[A].prettyPrint}") >>
              MIO.pure(helperCall(rctx.value, rctx.config))
          case None =>
            rctx.setHelper[A] { (value, config) =>
              deriveReaderViaRules[A](using rctx.nestInCache(value, config))
            } >> rctx.getHelper[A].flatMap {
              case Some(helperCall) =>
                MIO.pure(helperCall(rctx.value, rctx.config))
              case None =>
                deriveReaderViaRules[A]
            }
        }
      }

  private def deriveReaderViaRules[A: ReaderCtx]: MIO[Expr[Either[ConfigDecodingError, A]]] =
    Log
      .namedScope(s"Deriving reader via rules for type ${Type[A].prettyPrint}") {
        Rules(
          ReaderUseImplicitWhenAvailableRule,
          ReaderHandleAsValueTypeRule,
          ReaderHandleAsOptionRule,
          ReaderHandleAsMapRule,
          ReaderHandleAsCollectionRule,
          ReaderHandleAsNamedTupleRule,
          ReaderHandleAsSingletonRule,
          ReaderHandleAsCaseClassRule,
          ReaderHandleAsEnumRule
        )(_[A]).flatMap {
          case Right(result) =>
            Log.info(s"Derived reader for ${Type[A].prettyPrint}: ${result.prettyPrint}") >>
              MIO.pure(result)
          case Left(reasons) =>
            val reasonsStrings = reasons.toListMap.view.map { case (rule, reasons) =>
              if (reasons.isEmpty) s"The rule ${rule.name} was not applicable"
              else
                s" - The rule ${rule.name} was not applicable, for the following reasons: ${reasons.mkString(", ")}"
            }.toList
            val err = ReaderDerivationError.UnsupportedType(Type[A].prettyPrint, reasonsStrings)
            Log.error(err.message) >> MIO.fail(err)
        }
      }

  // Types

  private[compiletime] object RTypes {

    def ConfigReader: Type.Ctor1[ConfigReader] = Type.Ctor1.of[ConfigReader]
    val ReaderLogDerivation: Type[hearth.kindlings.sconfigderivation.ConfigReader.LogDerivation] =
      Type.of[hearth.kindlings.sconfigderivation.ConfigReader.LogDerivation]
    val ConfigValue: Type[ConfigValue] = Type.of[ConfigValue]
    val ConfigDecodingError: Type[ConfigDecodingError] = Type.of[ConfigDecodingError]
    val SConfig: Type[SConfig] = Type.of[SConfig]
    val String: Type[String] = Type.of[String]
    val Int: Type[Int] = Type.of[Int]
    val Long: Type[Long] = Type.of[Long]
    val Double: Type[Double] = Type.of[Double]
    val Boolean: Type[Boolean] = Type.of[Boolean]
    val Any: Type[Any] = Type.of[Any]
    val ArrayAny: Type[Array[Any]] = Type.of[Array[Any]]
    val EitherErrorAny: Type[Either[ConfigDecodingError, Any]] = Type.of[Either[ConfigDecodingError, Any]]
    val ListEitherErrorAny: Type[List[Either[ConfigDecodingError, Any]]] =
      Type.of[List[Either[ConfigDecodingError, Any]]]
    val ListString: Type[List[String]] = Type.of[List[String]]
    val SetString: Type[Set[String]] = Type.of[Set[String]]
    val OptionString: Type[Option[String]] = Type.of[Option[String]]
    val StringToString: Type[String => String] = Type.of[String => String]
    val ConfigKey: Type[configKey] = Type.of[configKey]
    val TransientField: Type[transientField] = Type.of[transientField]

    def ReaderResult[A: Type]: Type[Either[ConfigDecodingError, A]] =
      Type.of[Either[ConfigDecodingError, A]]

    def eitherStringType[A: Type]: Type[Either[String, A]] = Type.of[Either[String, A]]

    def optionType[A: Type]: Type[Option[A]] = Type.of[Option[A]]

    def productHintType[A: Type]: Type[ProductHint[A]] =
      Type.of[ProductHint[A]]

    def fieldCoproductHintType[A: Type]: Type[CoproductHint.Field[A]] =
      Type.of[CoproductHint.Field[A]]

    def wrappedCoproductHintType[A: Type]: Type[CoproductHint.Wrapped[A]] =
      Type.of[CoproductHint.Wrapped[A]]
  }
}
