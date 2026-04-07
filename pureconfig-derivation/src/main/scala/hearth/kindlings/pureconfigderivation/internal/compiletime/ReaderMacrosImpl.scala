package hearth.kindlings.pureconfigderivation.internal.compiletime

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

import hearth.kindlings.pureconfigderivation.{KindlingsConfigReader, KindlingsCoproductHint, KindlingsProductHint, PureConfig}
import hearth.kindlings.pureconfigderivation.annotations.{configKey, transientField}
import pureconfig.ConfigReader
import pureconfig.ConfigCursor
import pureconfig.error.ConfigReaderFailures

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
  def deriveReaderTypeClass[A: Type](configExpr: Expr[PureConfig]): Expr[KindlingsConfigReader[A]] = {
    implicit val ConfigReaderA: Type[ConfigReader[A]] = RTypes.ConfigReader[A]
    implicit val KindlingsConfigReaderA: Type[KindlingsConfigReader[A]] = RTypes.KindlingsConfigReader[A]
    implicit val ConfigCursorT: Type[ConfigCursor] = RTypes.ConfigCursor
    implicit val PureConfigT: Type[PureConfig] = RTypes.PureConfig
    implicit val FailuresT: Type[ConfigReaderFailures] = RTypes.ConfigReaderFailures
    implicit val ResultA: Type[Either[ConfigReaderFailures, A]] = RTypes.ReaderResult[A]
    val selfType: Option[??] = Some(Type[A].as_??)

    deriveReaderFromCtxAndAdaptForEntrypoint[A, KindlingsConfigReader[A]]("KindlingsConfigReader.derived") { fromCtx =>
      ValDefs.createVal[PureConfig](configExpr).use { configVal =>
        Expr.quote {
          val cfg = Expr.splice(configVal)
          new KindlingsConfigReader[A] {
            def from(cur: ConfigCursor): Either[ConfigReaderFailures, A] = {
              val _ = cur
              Expr.splice {
                fromCtx(ReaderCtx.from(Expr.quote(cur), Expr.quote(cfg), derivedType = selfType))
              }
            }
          }
        }
      }
    }
  }

  // Handles logging, error reporting and prepending "cached" defs and vals to the result.

  def deriveReaderFromCtxAndAdaptForEntrypoint[A: Type, Out: Type](macroName: String)(
      provideCtxAndAdapt: (ReaderCtx[A] => Expr[Either[ConfigReaderFailures, A]]) => Expr[Out]
  ): Expr[Out] = {
    if (Type[A] =:= Type.of[Nothing].asInstanceOf[Type[A]] || Type[A] =:= Type.of[Any].asInstanceOf[Type[A]])
      Environment.reportErrorAndAbort(
        s"$macroName: type parameter was inferred as ${Type[A].prettyPrint}, which is likely unintended.\n" +
          s"Provide an explicit type parameter, e.g.: $macroName[MyType](...)\n" +
          "or add a type ascription to the result variable."
      )
    Log
      .namedScope(
        s"Deriving reader for ${Type[A].prettyPrint} at: ${Environment.currentPosition.prettyPrint}"
      ) {
        MIO.scoped { runSafe =>
          val fromCtx: ReaderCtx[A] => Expr[Either[ConfigReaderFailures, A]] = (ctx: ReaderCtx[A]) =>
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
        val errorsRendered = errors
          .map { e =>
            e.getMessage.split("\n").toList match {
              case head :: tail => (("  - " + head) :: tail.map("    " + _)).mkString("\n")
              case _            => "  - " + e.getMessage
            }
          }
          .mkString("\n")
        val hint =
          "Enable debug logging with: import hearth.kindlings.pureconfigderivation.debug.logDerivationForKindlingsConfigReader or scalac option -Xmacro-settings:pureconfigDerivation.logDerivation=true"
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
    implicit val LogDerivationT: Type[KindlingsConfigReader.LogDerivation] = RTypes.ReaderLogDerivation
    def logDerivationImported = Expr.summonImplicit[KindlingsConfigReader.LogDerivation].isDefined

    def logDerivationSetGlobally = (for {
      data <- Environment.typedSettings.toOption
      pcDerivation <- data.get("pureconfigDerivation")
      shouldLog <- pcDerivation.get("logDerivation").flatMap(_.asBoolean)
    } yield shouldLog).getOrElse(false)

    logDerivationImported || logDerivationSetGlobally
  }

  // Context

  final case class ReaderCtx[A](
      tpe: Type[A],
      cursor: Expr[ConfigCursor],
      config: Expr[PureConfig],
      cache: MLocal[ValDefsCache],
      derivedType: Option[??]
  ) {

    def nest[B: Type](newCursor: Expr[ConfigCursor]): ReaderCtx[B] = copy[B](
      tpe = Type[B],
      cursor = newCursor
    )

    def nestInCache(
        newCursor: Expr[ConfigCursor],
        newConfig: Expr[PureConfig]
    ): ReaderCtx[A] = copy(
      cursor = newCursor,
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
        : MIO[Option[(Expr[ConfigCursor], Expr[PureConfig]) => Expr[Either[ConfigReaderFailures, B]]]] = {
      implicit val ResultB: Type[Either[ConfigReaderFailures, B]] = RTypes.ReaderResult[B]
      implicit val ConfigCursorT: Type[ConfigCursor] = RTypes.ConfigCursor
      implicit val PureConfigT: Type[PureConfig] = RTypes.PureConfig
      cache.get2Ary[ConfigCursor, PureConfig, Either[ConfigReaderFailures, B]]("cached-read-method")
    }
    def setHelper[B: Type](
        helper: (Expr[ConfigCursor], Expr[PureConfig]) => MIO[Expr[Either[ConfigReaderFailures, B]]]
    ): MIO[Unit] = {
      implicit val ResultB: Type[Either[ConfigReaderFailures, B]] = RTypes.ReaderResult[B]
      implicit val ConfigCursorT: Type[ConfigCursor] = RTypes.ConfigCursor
      implicit val PureConfigT: Type[PureConfig] = RTypes.PureConfig
      val defBuilder = ValDefBuilder
        .ofDef2[ConfigCursor, PureConfig, Either[ConfigReaderFailures, B]](s"read_${Type[B].shortName}")
      for {
        _ <- Log.info(s"Forward-declaring read helper for ${Type[B].prettyPrint}")
        _ <- cache.forwardDeclare("cached-read-method", defBuilder)
        _ <- MIO.scoped { runSafe =>
          runSafe(cache.buildCachedWith("cached-read-method", defBuilder) { case (_, (cur, config)) =>
            runSafe(helper(cur, config))
          })
        }
        _ <- Log.info(s"Defined read helper for ${Type[B].prettyPrint}")
      } yield ()
    }

    override def toString: String =
      s"read[${tpe.prettyPrint}](cursor = ${cursor.prettyPrint}, config = ${config.prettyPrint})"
  }
  object ReaderCtx {

    def from[A: Type](
        cursor: Expr[ConfigCursor],
        config: Expr[PureConfig],
        derivedType: Option[??]
    ): ReaderCtx[A] = ReaderCtx(
      tpe = Type[A],
      cursor = cursor,
      config = config,
      cache = ValDefsCache.mlocal,
      derivedType = derivedType
    )
  }

  def rctx[A](implicit A: ReaderCtx[A]): ReaderCtx[A] = A

  implicit def currentReaderValueType[A: ReaderCtx]: Type[A] = rctx.tpe

  abstract class ReaderDerivationRule(val name: String) extends Rule {
    def apply[A: ReaderCtx]: MIO[Rule.Applicability[Expr[Either[ConfigReaderFailures, A]]]]
  }

  // The actual derivation logic

  def deriveReaderRecursively[A: ReaderCtx]: MIO[Expr[Either[ConfigReaderFailures, A]]] =
    Log
      .namedScope(s"Deriving reader for type ${Type[A].prettyPrint}") {
        rctx.getHelper[A].flatMap {
          case Some(helperCall) =>
            Log.info(s"Using cached reader helper for ${Type[A].prettyPrint}") >>
              MIO.pure(helperCall(rctx.cursor, rctx.config))
          case None =>
            rctx.setHelper[A] { (cur, config) =>
              deriveReaderViaRules[A](using rctx.nestInCache(cur, config))
            } >> rctx.getHelper[A].flatMap {
              case Some(helperCall) =>
                MIO.pure(helperCall(rctx.cursor, rctx.config))
              case None =>
                deriveReaderViaRules[A]
            }
        }
      }

  private def deriveReaderViaRules[A: ReaderCtx]: MIO[Expr[Either[ConfigReaderFailures, A]]] =
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
    def KindlingsConfigReader: Type.Ctor1[KindlingsConfigReader] = Type.Ctor1.of[KindlingsConfigReader]
    val ReaderLogDerivation: Type[hearth.kindlings.pureconfigderivation.KindlingsConfigReader.LogDerivation] =
      Type.of[hearth.kindlings.pureconfigderivation.KindlingsConfigReader.LogDerivation]
    val ConfigCursor: Type[ConfigCursor] = Type.of[ConfigCursor]
    val ConfigObjectCursor: Type[pureconfig.ConfigObjectCursor] = Type.of[pureconfig.ConfigObjectCursor]
    val ConfigListCursor: Type[pureconfig.ConfigListCursor] = Type.of[pureconfig.ConfigListCursor]
    val ConfigReaderFailures: Type[ConfigReaderFailures] = Type.of[ConfigReaderFailures]
    val PureConfig: Type[PureConfig] = Type.of[PureConfig]
    val String: Type[String] = Type.of[String]
    val Int: Type[Int] = Type.of[Int]
    val Long: Type[Long] = Type.of[Long]
    val Double: Type[Double] = Type.of[Double]
    val Boolean: Type[Boolean] = Type.of[Boolean]
    val Any: Type[Any] = Type.of[Any]
    val ArrayAny: Type[Array[Any]] = Type.of[Array[Any]]
    val EitherFailuresAny: Type[Either[ConfigReaderFailures, Any]] = Type.of[Either[ConfigReaderFailures, Any]]
    val ListEitherFailuresAny: Type[List[Either[ConfigReaderFailures, Any]]] =
      Type.of[List[Either[ConfigReaderFailures, Any]]]
    val ListString: Type[List[String]] = Type.of[List[String]]
    val SetString: Type[Set[String]] = Type.of[Set[String]]
    val OptionString: Type[Option[String]] = Type.of[Option[String]]
    val StringToString: Type[String => String] = Type.of[String => String]
    val ConfigKey: Type[configKey] = Type.of[configKey]
    val TransientField: Type[transientField] = Type.of[transientField]

    def ReaderResult[A: Type]: Type[Either[ConfigReaderFailures, A]] =
      Type.of[Either[ConfigReaderFailures, A]]

    def eitherStringType[A: Type]: Type[Either[String, A]] = Type.of[Either[String, A]]

    def optionType[A: Type]: Type[Option[A]] = Type.of[Option[A]]

    def kindlingsProductHintType[A: Type]: Type[KindlingsProductHint[A]] =
      Type.of[KindlingsProductHint[A]]

    def fieldCoproductHintType[A: Type]: Type[KindlingsCoproductHint.Field[A]] =
      Type.of[KindlingsCoproductHint.Field[A]]

    def wrappedCoproductHintType[A: Type]: Type[KindlingsCoproductHint.Wrapped[A]] =
      Type.of[KindlingsCoproductHint.Wrapped[A]]

    def firstSuccessCoproductHintType[A: Type]: Type[KindlingsCoproductHint.FirstSuccess[A]] =
      Type.of[KindlingsCoproductHint.FirstSuccess[A]]
  }
}
