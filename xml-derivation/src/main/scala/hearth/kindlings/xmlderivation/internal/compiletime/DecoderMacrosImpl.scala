package hearth.kindlings.xmlderivation.internal.compiletime

import hearth.MacroCommons
import hearth.fp.data.NonEmptyList
import hearth.fp.effect.*
import hearth.fp.syntax.*
import hearth.std.*

import hearth.kindlings.xmlderivation.{KindlingsXmlDecoder, XmlConfig, XmlDecodingError}
import hearth.kindlings.xmlderivation.annotations.{transientField, xmlAttribute, xmlContent, xmlName}
import hearth.kindlings.xmlderivation.internal.runtime.XmlDerivationUtils

trait DecoderMacrosImpl { this: MacroCommons & StdExtensions & AnnotationSupport =>

  // Entrypoints

  @scala.annotation.nowarn("msg=is never used")
  def deriveInlineDecode[A: Type](
      elemExpr: Expr[scala.xml.Elem],
      configExpr: Expr[XmlConfig]
  ): Expr[Either[XmlDecodingError, A]] = {
    implicit val EitherT: Type[Either[XmlDecodingError, A]] = DTypes.DecoderResult[A]
    implicit val ElemT: Type[scala.xml.Elem] = DTypes.Elem
    implicit val ConfigT: Type[XmlConfig] = DTypes.XmlConfig
    implicit val XmlDecodingErrorT: Type[XmlDecodingError] = DTypes.XmlDecodingError

    deriveDecoderFromCtxAndAdaptForEntrypoint[A, Either[XmlDecodingError, A]]("KindlingsXmlDecoder.decode") { fromCtx =>
      ValDefs.createVal[scala.xml.Elem](elemExpr).use { elemVal =>
        ValDefs.createVal[XmlConfig](configExpr).use { configVal =>
          Expr.quote {
            val _ = Expr.splice(elemVal)
            val _ = Expr.splice(configVal)
            Expr.splice {
              fromCtx(DecoderCtx.from(elemVal, configVal, derivedType = None))
            }
          }
        }
      }
    }
  }

  @scala.annotation.nowarn("msg=is never used")
  def deriveInlineFromXmlString[A: Type](
      xmlExpr: Expr[String],
      configExpr: Expr[XmlConfig]
  ): Expr[Either[XmlDecodingError, A]] = {
    implicit val EitherT: Type[Either[XmlDecodingError, A]] = DTypes.DecoderResult[A]
    implicit val ElemT: Type[scala.xml.Elem] = DTypes.Elem
    implicit val ConfigT: Type[XmlConfig] = DTypes.XmlConfig
    implicit val XmlDecodingErrorT: Type[XmlDecodingError] = DTypes.XmlDecodingError
    implicit val StringT: Type[String] = DTypes.String

    deriveDecoderFromCtxAndAdaptForEntrypoint[A, Either[XmlDecodingError, A]](
      "KindlingsXmlDecoder.fromXmlString"
    ) { fromCtx =>
      ValDefs.createVal[String](xmlExpr).use { xmlVal =>
        ValDefs.createVal[XmlConfig](configExpr).use { configVal =>
          Expr.quote {
            val x = Expr.splice(xmlVal)
            val cfg = Expr.splice(configVal)
            XmlDerivationUtils.parseAndDecode[A](
              x,
              (elem: scala.xml.Elem) => {
                val _ = elem
                val _ = cfg
                Expr.splice {
                  fromCtx(DecoderCtx.from(Expr.quote(elem), Expr.quote(cfg), derivedType = None))
                }
              }
            )
          }
        }
      }
    }
  }

  @scala.annotation.nowarn("msg=is never used")
  def deriveDecoderTypeClass[A: Type](configExpr: Expr[XmlConfig]): Expr[KindlingsXmlDecoder[A]] = {
    implicit val DecoderA: Type[XmlDecoder[A]] = DTypes.XmlDecoder[A]
    implicit val KindlingsDecoderA: Type[KindlingsXmlDecoder[A]] = DTypes.KindlingsXmlDecoder[A]
    implicit val EitherT: Type[Either[XmlDecodingError, A]] = DTypes.DecoderResult[A]
    implicit val ElemT: Type[scala.xml.Elem] = DTypes.Elem
    implicit val ConfigT: Type[XmlConfig] = DTypes.XmlConfig
    implicit val XmlDecodingErrorT: Type[XmlDecodingError] = DTypes.XmlDecodingError
    val selfType: Option[??] = Some(Type[A].as_??)

    if (Type[A] =:= Type.of[Nothing].asInstanceOf[Type[A]] || Type[A] =:= Type.of[Any].asInstanceOf[Type[A]])
      Environment.reportErrorAndAbort(
        s"KindlingsXmlDecoder.derived: type parameter was inferred as ${Type[A].prettyPrint}, which is likely unintended.\n" +
          "Provide an explicit type parameter, e.g.: KindlingsXmlDecoder.derived[MyType]\n" +
          "or add a type ascription to the result variable."
      )

    Log
      .namedScope(
        s"Deriving XML decoder for ${Type[A].prettyPrint} at: ${Environment.currentPosition.prettyPrint}"
      ) {
        MIO.scoped { runSafe =>
          implicit val AnyT: Type[Any] = DTypes.Any

          // Step 1: Run derivation with placeholder context to populate cache
          val placeholderCtx = DecoderCtx.from[A](
            elem = Expr.quote(null.asInstanceOf[scala.xml.Elem]),
            config = Expr.quote(null.asInstanceOf[XmlConfig]),
            derivedType = selfType
          )
          runSafe {
            for {
              _ <- Environment.loadStandardExtensions().toMIO(allowFailures = false)
              _ <- deriveDecoderRecursively[A](using placeholderCtx)
            } yield ()
          }

          // Step 2: Extract cached helper and cache
          val helperOpt = runSafe(placeholderCtx.getHelper[A])
          val cache = runSafe(placeholderCtx.cache.get)

          helperOpt match {
            case Some(helper) =>
              // Type has a cached def — call it directly (no lambda allocation)
              cache.toValDefs.use { _ =>
                Expr.quote {
                  new KindlingsXmlDecoder[A] {
                    def decode(elem: scala.xml.Elem): Either[XmlDecodingError, A] =
                      Expr.splice(helper(Expr.quote(elem), configExpr))
                  }
                }
              }
            case None =>
              // Simple type without cached helper (value types, options, collections) —
              // derive directly with real context. Define fromCtx outside the quote to avoid
              // ClassTag leaks from loadStandardExtensions into the reified context.
              val fromCtx: DecoderCtx[A] => Expr[Either[XmlDecodingError, A]] =
                ctx =>
                  runSafe {
                    for {
                      _ <- Environment.loadStandardExtensions().toMIO(allowFailures = false)
                      result <- deriveDecoderRecursively[A](using ctx)
                      freshCache <- ctx.cache.get
                    } yield freshCache.toValDefs.use(_ => result)
                  }
              Expr.quote {
                new KindlingsXmlDecoder[A] {
                  def decode(elem: scala.xml.Elem): Either[XmlDecodingError, A] = {
                    val _ = elem
                    Expr.splice {
                      fromCtx(DecoderCtx.from[A](Expr.quote(elem), configExpr, derivedType = selfType))
                    }
                  }
                }
              }
          }
        }
      }
      .flatTap { result =>
        Log.info(s"Derived final XML decoder result: ${result.prettyPrint}")
      }
      .runToExprOrFail(
        "KindlingsXmlDecoder.derived",
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
          "Enable debug logging with: import hearth.kindlings.xmlderivation.debug.logDerivationForKindlingsXmlDecoder or scalac option -Xmacro-settings:xmlDerivation.logDerivation=true"
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
      provideCtxAndAdapt: (DecoderCtx[A] => Expr[Either[XmlDecodingError, A]]) => Expr[Out]
  ): Expr[Out] = {
    if (Type[A] =:= Type.of[Nothing].asInstanceOf[Type[A]] || Type[A] =:= Type.of[Any].asInstanceOf[Type[A]])
      Environment.reportErrorAndAbort(
        s"$macroName: type parameter was inferred as ${Type[A].prettyPrint}, which is likely unintended.\n" +
          s"Provide an explicit type parameter, e.g.: $macroName[MyType](...)\n" +
          "or add a type ascription to the result variable."
      )
    Log
      .namedScope(
        s"Deriving XML decoder for ${Type[A].prettyPrint} at: ${Environment.currentPosition.prettyPrint}"
      ) {
        MIO.scoped { runSafe =>
          val fromCtx: (DecoderCtx[A] => Expr[Either[XmlDecodingError, A]]) =
            (ctx: DecoderCtx[A]) =>
              runSafe {
                for {
                  _ <- Environment.loadStandardExtensions().toMIO(allowFailures = false)
                  result <- deriveDecoderRecursively[A](using ctx)
                  cache <- ctx.cache.get
                } yield cache.toValDefs.use(_ => result)
              }

          provideCtxAndAdapt(fromCtx)
        }
      }
      .flatTap { result =>
        Log.info(s"Derived final XML decoder result: ${result.prettyPrint}")
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
          "Enable debug logging with: import hearth.kindlings.xmlderivation.debug.logDerivationForKindlingsXmlDecoder or scalac option -Xmacro-settings:xmlDerivation.logDerivation=true"
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
    implicit val LogDerivation: Type[KindlingsXmlDecoder.LogDerivation] = DTypes.DecoderLogDerivation
    def logDerivationImported = Expr.summonImplicit[KindlingsXmlDecoder.LogDerivation].isDefined

    def logDerivationSetGlobally = (for {
      data <- Environment.typedSettings.toOption
      xmlDerivation <- data.get("xmlDerivation")
      shouldLog <- xmlDerivation.get("logDerivation").flatMap(_.asBoolean)
    } yield shouldLog).getOrElse(false)

    logDerivationImported || logDerivationSetGlobally
  }

  // Context

  final case class DecoderCtx[A](
      tpe: Type[A],
      elem: Expr[scala.xml.Elem],
      config: Expr[XmlConfig],
      cache: MLocal[ValDefsCache],
      derivedType: Option[??]
  ) {

    def nest[B: Type](newElem: Expr[scala.xml.Elem]): DecoderCtx[B] = copy[B](
      tpe = Type[B],
      elem = newElem
    )

    def nestInCache(
        newElem: Expr[scala.xml.Elem],
        newConfig: Expr[XmlConfig]
    ): DecoderCtx[A] = copy(
      elem = newElem,
      config = newConfig
    )

    def getInstance[B: Type]: MIO[Option[Expr[XmlDecoder[B]]]] = {
      implicit val DecoderB: Type[XmlDecoder[B]] = DTypes.XmlDecoder[B]
      cache.get0Ary[XmlDecoder[B]]("cached-decoder-instance")
    }
    def setInstance[B: Type](instance: Expr[XmlDecoder[B]]): MIO[Unit] = {
      implicit val DecoderB: Type[XmlDecoder[B]] = DTypes.XmlDecoder[B]
      Log.info(s"Caching XmlDecoder instance for ${Type[B].prettyPrint}") >>
        cache.buildCachedWith(
          "cached-decoder-instance",
          ValDefBuilder.ofLazy[XmlDecoder[B]](s"decoder_${Type[B].shortName}")
        )(_ => instance)
    }

    def getHelper[B: Type]
        : MIO[Option[(Expr[scala.xml.Elem], Expr[XmlConfig]) => Expr[Either[XmlDecodingError, B]]]] = {
      implicit val EitherT: Type[Either[XmlDecodingError, B]] = DTypes.DecoderResult[B]
      implicit val ConfigT: Type[XmlConfig] = DTypes.XmlConfig
      implicit val ElemT: Type[scala.xml.Elem] = DTypes.Elem
      cache.get2Ary[scala.xml.Elem, XmlConfig, Either[XmlDecodingError, B]]("cached-decode-method")
    }
    def setHelper[B: Type](
        helper: (Expr[scala.xml.Elem], Expr[XmlConfig]) => MIO[Expr[Either[XmlDecodingError, B]]]
    ): MIO[Unit] = {
      implicit val EitherT: Type[Either[XmlDecodingError, B]] = DTypes.DecoderResult[B]
      implicit val ConfigT: Type[XmlConfig] = DTypes.XmlConfig
      implicit val ElemT: Type[scala.xml.Elem] = DTypes.Elem
      val defBuilder =
        ValDefBuilder.ofDef2[scala.xml.Elem, XmlConfig, Either[XmlDecodingError, B]](s"decode_${Type[B].shortName}")
      for {
        _ <- Log.info(s"Forward-declaring decode helper for ${Type[B].prettyPrint}")
        _ <- cache.forwardDeclare("cached-decode-method", defBuilder)
        _ <- MIO.scoped { runSafe =>
          runSafe(cache.buildCachedWith("cached-decode-method", defBuilder) { case (_, (elem, config)) =>
            runSafe(helper(elem, config))
          })
        }
        _ <- Log.info(s"Defined decode helper for ${Type[B].prettyPrint}")
      } yield ()
    }

    override def toString: String =
      s"decode[${tpe.prettyPrint}](elem = ${elem.prettyPrint}, config = ${config.prettyPrint})"
  }
  object DecoderCtx {

    def from[A: Type](
        elem: Expr[scala.xml.Elem],
        config: Expr[XmlConfig],
        derivedType: Option[??]
    ): DecoderCtx[A] = DecoderCtx(
      tpe = Type[A],
      elem = elem,
      config = config,
      cache = ValDefsCache.mlocal,
      derivedType = derivedType
    )
  }

  def dctx[A](implicit A: DecoderCtx[A]): DecoderCtx[A] = A

  implicit def currentDecoderValueType[A: DecoderCtx]: Type[A] = dctx.tpe

  abstract class DecoderDerivationRule(val name: String) extends Rule {
    def apply[A: DecoderCtx]: MIO[Rule.Applicability[Expr[Either[XmlDecodingError, A]]]]
  }

  // The actual derivation logic

  def deriveDecoderRecursively[A: DecoderCtx]: MIO[Expr[Either[XmlDecodingError, A]]] =
    Log
      .namedScope(s"Deriving XML decoder for type ${Type[A].prettyPrint}") {
        Rules(
          DecUseCachedDefWhenAvailableRule,
          DecUseImplicitWhenAvailableRule,
          DecHandleAsBuiltInRule,
          DecHandleAsValueTypeRule,
          DecHandleAsOptionRule,
          DecHandleAsMapRule,
          DecHandleAsCollectionRule,
          DecHandleAsSingletonRule,
          DecHandleAsCaseClassRule,
          DecHandleAsEnumRule
        )(_[A]).flatMap {
          case Right(result) =>
            Log.info(s"Derived XML decoder for ${Type[A].prettyPrint}: ${result.prettyPrint}") >>
              MIO.pure(result)
          case Left(reasons) =>
            val reasonsStrings = reasons.toListMap
              .removed(DecUseCachedDefWhenAvailableRule)
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

  // Rules

  object DecUseCachedDefWhenAvailableRule extends DecoderDerivationRule("use cached def when available") {

    def apply[A: DecoderCtx]: MIO[Rule.Applicability[Expr[Either[XmlDecodingError, A]]]] =
      Log.info(s"Attempting to use cached XML decoder for ${Type[A].prettyPrint}") >>
        dctx.getInstance[A].flatMap {
          case Some(instance) => callCachedInstance[A](instance)
          case None           =>
            dctx.getHelper[A].flatMap {
              case Some(helperCall) => callCachedHelper[A](helperCall)
              case None             => yieldUnsupported[A]
            }
        }

    @scala.annotation.nowarn("msg=is never used")
    private def callCachedInstance[A: DecoderCtx](
        instance: Expr[XmlDecoder[A]]
    ): MIO[Rule.Applicability[Expr[Either[XmlDecodingError, A]]]] = {
      implicit val EitherT: Type[Either[XmlDecodingError, A]] = DTypes.DecoderResult[A]
      implicit val XmlDecodingErrorT: Type[XmlDecodingError] = DTypes.XmlDecodingError
      implicit val ElemT: Type[scala.xml.Elem] = DTypes.Elem
      implicit val DecoderAT: Type[hearth.kindlings.xmlderivation.XmlDecoder[A]] = DTypes.XmlDecoder[A]
      val publicInstance: Expr[hearth.kindlings.xmlderivation.XmlDecoder[A]] =
        instance.upcast[hearth.kindlings.xmlderivation.XmlDecoder[A]]
      Log.info(s"Found cached XML decoder instance for ${Type[A].prettyPrint}") >> MIO.pure(Rule.matched(Expr.quote {
        Expr.splice(publicInstance).decode(Expr.splice(dctx.elem))
      }))
    }

    private def callCachedHelper[A: DecoderCtx](
        helperCall: (Expr[scala.xml.Elem], Expr[XmlConfig]) => Expr[Either[XmlDecodingError, A]]
    ): MIO[Rule.Applicability[Expr[Either[XmlDecodingError, A]]]] =
      Log.info(s"Found cached XML decoder helper for ${Type[A].prettyPrint}") >> MIO.pure(
        Rule.matched(helperCall(dctx.elem, dctx.config))
      )

    private def yieldUnsupported[A: DecoderCtx]: MIO[Rule.Applicability[Expr[Either[XmlDecodingError, A]]]] =
      MIO.pure(Rule.yielded(s"The type ${Type[A].prettyPrint} does not have a cached XML decoder"))
  }

  object DecUseImplicitWhenAvailableRule extends DecoderDerivationRule("use implicit when available") {

    lazy val ignoredImplicits: Seq[UntypedMethod] = {
      val ours = Type.of[KindlingsXmlDecoder.type].methods.collect {
        case method if method.value.name == "derived" => method.value.asUntyped
      }
      ours
    }

    def apply[A: DecoderCtx]: MIO[Rule.Applicability[Expr[Either[XmlDecodingError, A]]]] =
      Log.info(s"Attempting to use implicit XmlDecoder for ${Type[A].prettyPrint}") >> {
        if (dctx.derivedType.exists(_.Underlying =:= Type[A]))
          MIO.pure(
            Rule.yielded(s"The type ${Type[A].prettyPrint} is the type being derived, skipping implicit search")
          )
        else
          DTypes.XmlDecoder[A].summonExprIgnoring(ignoredImplicits*).toEither match {
            case Right(instanceExpr) => cacheAndUse[A](instanceExpr)
            case Left(reason)        => yieldUnsupported[A](reason)
          }
      }

    @scala.annotation.nowarn("msg=is never used")
    private def cacheAndUse[A: DecoderCtx](
        instanceExpr: Expr[XmlDecoder[A]]
    ): MIO[Rule.Applicability[Expr[Either[XmlDecodingError, A]]]] = {
      implicit val EitherT: Type[Either[XmlDecodingError, A]] = DTypes.DecoderResult[A]
      implicit val XmlDecodingErrorT: Type[XmlDecodingError] = DTypes.XmlDecodingError
      implicit val ElemT: Type[scala.xml.Elem] = DTypes.Elem
      implicit val DecoderAT: Type[hearth.kindlings.xmlderivation.XmlDecoder[A]] = DTypes.XmlDecoder[A]
      val publicInstance: Expr[hearth.kindlings.xmlderivation.XmlDecoder[A]] =
        instanceExpr.upcast[hearth.kindlings.xmlderivation.XmlDecoder[A]]
      Log.info(s"Found implicit XML decoder ${instanceExpr.prettyPrint}, using directly") >>
        MIO.pure(Rule.matched(Expr.quote {
          Expr.splice(publicInstance).decode(Expr.splice(dctx.elem))
        }))
    }

    private def yieldUnsupported[A: DecoderCtx](
        reason: String
    ): MIO[Rule.Applicability[Expr[Either[XmlDecodingError, A]]]] =
      MIO.pure(
        Rule.yielded(
          s"The type ${Type[A].prettyPrint} does not have an implicit XmlDecoder instance: $reason"
        )
      )
  }

  @scala.annotation.nowarn("msg=is never used")
  object DecHandleAsBuiltInRule extends DecoderDerivationRule("handle as built-in primitive type") {

    implicit val ElemT: Type[scala.xml.Elem] = DTypes.Elem
    implicit val XmlDecodingErrorT: Type[XmlDecodingError] = DTypes.XmlDecodingError
    implicit val StringT: Type[String] = DTypes.String
    implicit val BooleanT: Type[Boolean] = DTypes.Boolean
    implicit val ByteT: Type[Byte] = DTypes.Byte
    implicit val ShortT: Type[Short] = DTypes.Short
    implicit val IntT: Type[Int] = DTypes.Int
    implicit val LongT: Type[Long] = DTypes.Long
    implicit val FloatT: Type[Float] = DTypes.Float
    implicit val DoubleT: Type[Double] = DTypes.Double
    implicit val CharT: Type[Char] = DTypes.Char
    implicit val BigDecimalT: Type[BigDecimal] = DTypes.BigDecimal
    implicit val BigIntT: Type[BigInt] = DTypes.BigInt

    def apply[A: DecoderCtx]: MIO[Rule.Applicability[Expr[Either[XmlDecodingError, A]]]] =
      Log.info(s"Attempting to use built-in support for ${Type[A].prettyPrint}") >> MIO {
        implicit val EitherT: Type[Either[XmlDecodingError, A]] = DTypes.DecoderResult[A]
        val elem = dctx.elem

        if (Type[A] <:< Type[String]) {
          implicit val ResultT: Type[Either[XmlDecodingError, String]] = DTypes.DecoderResult[String]
          Rule.matched(
            Expr
              .quote {
                XmlDerivationUtils
                  .getTextContent(Expr.splice(elem))
                  .flatMap(v => XmlDerivationUtils.parseString(v, Expr.splice(elem).label))
              }
              .asInstanceOf[Expr[Either[XmlDecodingError, A]]]
          )
        } else if (Type[A] <:< Type[Boolean]) {
          implicit val ResultT: Type[Either[XmlDecodingError, Boolean]] = DTypes.DecoderResult[Boolean]
          Rule.matched(
            Expr
              .quote {
                XmlDerivationUtils
                  .getTextContent(Expr.splice(elem))
                  .flatMap(v => XmlDerivationUtils.parseBoolean(v, Expr.splice(elem).label))
              }
              .asInstanceOf[Expr[Either[XmlDecodingError, A]]]
          )
        } else if (Type[A] <:< Type[Int]) {
          implicit val ResultT: Type[Either[XmlDecodingError, Int]] = DTypes.DecoderResult[Int]
          Rule.matched(
            Expr
              .quote {
                XmlDerivationUtils
                  .getTextContent(Expr.splice(elem))
                  .flatMap(v => XmlDerivationUtils.parseInt(v, Expr.splice(elem).label))
              }
              .asInstanceOf[Expr[Either[XmlDecodingError, A]]]
          )
        } else if (Type[A] <:< Type[Long]) {
          implicit val ResultT: Type[Either[XmlDecodingError, Long]] = DTypes.DecoderResult[Long]
          Rule.matched(
            Expr
              .quote {
                XmlDerivationUtils
                  .getTextContent(Expr.splice(elem))
                  .flatMap(v => XmlDerivationUtils.parseLong(v, Expr.splice(elem).label))
              }
              .asInstanceOf[Expr[Either[XmlDecodingError, A]]]
          )
        } else if (Type[A] <:< Type[Double]) {
          implicit val ResultT: Type[Either[XmlDecodingError, Double]] = DTypes.DecoderResult[Double]
          Rule.matched(
            Expr
              .quote {
                XmlDerivationUtils
                  .getTextContent(Expr.splice(elem))
                  .flatMap(v => XmlDerivationUtils.parseDouble(v, Expr.splice(elem).label))
              }
              .asInstanceOf[Expr[Either[XmlDecodingError, A]]]
          )
        } else if (Type[A] <:< Type[Float]) {
          implicit val ResultT: Type[Either[XmlDecodingError, Float]] = DTypes.DecoderResult[Float]
          Rule.matched(
            Expr
              .quote {
                XmlDerivationUtils
                  .getTextContent(Expr.splice(elem))
                  .flatMap(v => XmlDerivationUtils.parseFloat(v, Expr.splice(elem).label))
              }
              .asInstanceOf[Expr[Either[XmlDecodingError, A]]]
          )
        } else if (Type[A] <:< Type[Short]) {
          implicit val ResultT: Type[Either[XmlDecodingError, Short]] = DTypes.DecoderResult[Short]
          Rule.matched(
            Expr
              .quote {
                XmlDerivationUtils
                  .getTextContent(Expr.splice(elem))
                  .flatMap(v => XmlDerivationUtils.parseShort(v, Expr.splice(elem).label))
              }
              .asInstanceOf[Expr[Either[XmlDecodingError, A]]]
          )
        } else if (Type[A] <:< Type[Byte]) {
          implicit val ResultT: Type[Either[XmlDecodingError, Byte]] = DTypes.DecoderResult[Byte]
          Rule.matched(
            Expr
              .quote {
                XmlDerivationUtils
                  .getTextContent(Expr.splice(elem))
                  .flatMap(v => XmlDerivationUtils.parseByte(v, Expr.splice(elem).label))
              }
              .asInstanceOf[Expr[Either[XmlDecodingError, A]]]
          )
        } else if (Type[A] <:< Type[Char]) {
          implicit val ResultT: Type[Either[XmlDecodingError, Char]] = DTypes.DecoderResult[Char]
          Rule.matched(
            Expr
              .quote {
                XmlDerivationUtils
                  .getTextContent(Expr.splice(elem))
                  .flatMap(v => XmlDerivationUtils.parseChar(v, Expr.splice(elem).label))
              }
              .asInstanceOf[Expr[Either[XmlDecodingError, A]]]
          )
        } else if (Type[A] <:< Type[BigDecimal]) {
          implicit val ResultT: Type[Either[XmlDecodingError, BigDecimal]] = DTypes.DecoderResult[BigDecimal]
          Rule.matched(
            Expr
              .quote {
                XmlDerivationUtils
                  .getTextContent(Expr.splice(elem))
                  .flatMap(v => XmlDerivationUtils.parseBigDecimal(v, Expr.splice(elem).label))
              }
              .asInstanceOf[Expr[Either[XmlDecodingError, A]]]
          )
        } else if (Type[A] <:< Type[BigInt]) {
          implicit val ResultT: Type[Either[XmlDecodingError, BigInt]] = DTypes.DecoderResult[BigInt]
          Rule.matched(
            Expr
              .quote {
                XmlDerivationUtils
                  .getTextContent(Expr.splice(elem))
                  .flatMap(v => XmlDerivationUtils.parseBigInt(v, Expr.splice(elem).label))
              }
              .asInstanceOf[Expr[Either[XmlDecodingError, A]]]
          )
        } else Rule.yielded(s"The type ${Type[A].prettyPrint} is not a built-in primitive type")
      }
  }

  @scala.annotation.nowarn("msg=is never used")
  object DecHandleAsValueTypeRule extends DecoderDerivationRule("handle as value type when possible") {

    def apply[A: DecoderCtx]: MIO[Rule.Applicability[Expr[Either[XmlDecodingError, A]]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as a value type") >> {
        implicit val XmlDecodingErrorT: Type[XmlDecodingError] = DTypes.XmlDecodingError
        implicit val EitherT: Type[Either[XmlDecodingError, A]] = DTypes.DecoderResult[A]
        Type[A] match {
          case IsValueType(isValueType) =>
            import isValueType.Underlying as Inner
            implicit val EitherInnerT: Type[Either[XmlDecodingError, Inner]] = DTypes.DecoderResult[Inner]
            isValueType.value.wrap match {
              case _: CtorLikeOf.EitherStringOrValue[?, ?] =>
                // Wrap returns Either[String, A] — convert Left(String) to Left(XmlDecodingError)
                implicit val StringT: Type[String] = DTypes.String
                implicit val EitherStringA: Type[Either[String, A]] = DTypes.eitherStringType[A]
                LambdaBuilder
                  .of1[Inner]("inner")
                  .traverse { innerExpr =>
                    val wrapResult = isValueType.value.wrap.apply(innerExpr).asInstanceOf[Expr[Either[String, A]]]
                    MIO.pure(Expr.quote {
                      Expr.splice(wrapResult).left.map { (msg: String) =>
                        XmlDecodingError.General(msg): XmlDecodingError
                      }
                    })
                  }
                  .flatMap { builder =>
                    val wrapLambda = builder.build[Either[XmlDecodingError, A]]
                    for {
                      innerResult <- deriveDecoderRecursively[Inner](using dctx.nest[Inner](dctx.elem))
                    } yield Rule.matched(Expr.quote {
                      Expr.splice(innerResult).flatMap(Expr.splice(wrapLambda))
                    })
                  }

              case _ =>
                // Wrap returns A directly (identity or simple constructor)
                LambdaBuilder
                  .of1[Inner]("inner")
                  .traverse { innerExpr =>
                    MIO.pure(isValueType.value.wrap.apply(innerExpr).asInstanceOf[Expr[A]])
                  }
                  .flatMap { builder =>
                    val wrapLambda = builder.build[A]
                    for {
                      innerResult <- deriveDecoderRecursively[Inner](using dctx.nest[Inner](dctx.elem))
                    } yield Rule.matched(Expr.quote {
                      Expr.splice(innerResult).map(Expr.splice(wrapLambda))
                    })
                  }
            }

          case _ =>
            MIO.pure(Rule.yielded(s"The type ${Type[A].prettyPrint} is not a value type"))
        }
      }
  }

  @scala.annotation.nowarn("msg=is never used")
  object DecHandleAsOptionRule extends DecoderDerivationRule("handle as Option when possible") {

    def apply[A: DecoderCtx]: MIO[Rule.Applicability[Expr[Either[XmlDecodingError, A]]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as Option") >> {
        implicit val EitherT: Type[Either[XmlDecodingError, A]] = DTypes.DecoderResult[A]
        implicit val XmlDecodingErrorT: Type[XmlDecodingError] = DTypes.XmlDecodingError
        implicit val ElemT: Type[scala.xml.Elem] = DTypes.Elem
        Type[A] match {
          case IsOption(isOption) =>
            import isOption.Underlying as Inner
            implicit val EitherInnerT: Type[Either[XmlDecodingError, Inner]] = DTypes.DecoderResult[Inner]
            // For Option, we try to decode and wrap in Some; if the element is effectively empty, return None
            LambdaBuilder
              .of1[scala.xml.Elem]("optElem")
              .traverse { elemExpr =>
                deriveDecoderRecursively[Inner](using dctx.nest[Inner](elemExpr))
              }
              .map { builder =>
                val lambda = builder.build[Either[XmlDecodingError, Inner]]
                Rule.matched(Expr.quote {
                  val elem = Expr.splice(dctx.elem)
                  if (elem.child.isEmpty && elem.attributes.isEmpty && elem.text.trim.isEmpty)
                    Right(None.asInstanceOf[A])
                  else
                    Expr.splice(lambda).apply(elem).map(v => Some(v).asInstanceOf[A])
                })
              }

          case _ =>
            MIO.pure(Rule.yielded(s"The type ${Type[A].prettyPrint} is not an Option"))
        }
      }
  }

  @scala.annotation.nowarn("msg=is never used")
  @scala.annotation.nowarn("msg=is never used")
  object DecHandleAsMapRule extends DecoderDerivationRule("handle as map when possible") {

    def apply[A: DecoderCtx]: MIO[Rule.Applicability[Expr[Either[XmlDecodingError, A]]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as a map") >> {
        Type[A] match {
          case IsMap(isMap) =>
            import isMap.Underlying as Pair
            decodeMapEntries[A, Pair](isMap.value)

          case _ =>
            MIO.pure(Rule.yielded(s"The type ${Type[A].prettyPrint} is not a map"))
        }
      }

    private def decodeMapEntries[A: DecoderCtx, Pair: Type](
        isMap: IsMapOf[A, Pair]
    ): MIO[Rule.Applicability[Expr[Either[XmlDecodingError, A]]]] = {
      import isMap.{Key, Value, CtorResult}
      implicit val EitherT: Type[Either[XmlDecodingError, A]] = DTypes.DecoderResult[A]
      implicit val XmlDecodingErrorT: Type[XmlDecodingError] = DTypes.XmlDecodingError
      implicit val ElemT: Type[scala.xml.Elem] = DTypes.Elem
      implicit val StringT: Type[String] = DTypes.String
      if (!(Key <:< Type[String]))
        MIO.pure(Rule.yielded(s"Map key type ${Key.prettyPrint} is not String"))
      else {
        LambdaBuilder
          .of1[scala.xml.Elem]("mapEntryElem")
          .traverse { entryElemExpr =>
            deriveDecoderRecursively[Value](using dctx.nest[Value](entryElemExpr))
          }
          .map { builder =>
            implicit val EitherValueT: Type[Either[XmlDecodingError, Value]] = DTypes.DecoderResult[Value]
            val lambda = builder.build[Either[XmlDecodingError, Value]]
            Rule.matched(Expr.quote {
              val parentElem = Expr.splice(dctx.elem)
              val entries = XmlDerivationUtils.getChildElems(parentElem, "entry")
              val results: List[Either[XmlDecodingError, (String, Value)]] = entries.map { entryElem =>
                XmlDerivationUtils.getAttribute(entryElem, "key").flatMap { key =>
                  val valueElems = (entryElem \ "value").collect { case e: scala.xml.Elem => e }.toList
                  valueElems.headOption match {
                    case Some(valueElem) =>
                      Expr.splice(lambda).apply(valueElem).map(v => (key, v))
                    case None =>
                      Left(XmlDecodingError.MissingElement("value", entryElem.label))
                  }
                }
              }
              val errors = results.collect { case Left(e) => e }
              if (errors.nonEmpty) Left(XmlDecodingError.Multiple(errors))
              else {
                val pairs = results.collect { case Right(p) => p }
                Right(pairs.toMap.asInstanceOf[A])
              }
            })
          }
      }
    }
  }

  @scala.annotation.nowarn("msg=is never used")
  object DecHandleAsCollectionRule extends DecoderDerivationRule("handle as collection when possible") {

    def apply[A: DecoderCtx]: MIO[Rule.Applicability[Expr[Either[XmlDecodingError, A]]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as a collection") >> {
        implicit val EitherT: Type[Either[XmlDecodingError, A]] = DTypes.DecoderResult[A]
        implicit val XmlDecodingErrorT: Type[XmlDecodingError] = DTypes.XmlDecodingError
        implicit val ElemT: Type[scala.xml.Elem] = DTypes.Elem
        Type[A] match {
          case IsCollection(isCollection) =>
            import isCollection.Underlying as Item
            implicit val EitherItemT: Type[Either[XmlDecodingError, Item]] = DTypes.DecoderResult[Item]
            LambdaBuilder
              .of1[scala.xml.Elem]("collItemElem")
              .traverse { itemElemExpr =>
                deriveDecoderRecursively[Item](using dctx.nest[Item](itemElemExpr))
              }
              .map { builder =>
                val lambda = builder.build[Either[XmlDecodingError, Item]]
                Rule.matched(Expr.quote {
                  val parentElem = Expr.splice(dctx.elem)
                  val childElems = parentElem.child.collect { case e: scala.xml.Elem => e }.toList
                  val results: List[Either[XmlDecodingError, Item]] = childElems.map { childElem =>
                    Expr.splice(lambda).apply(childElem)
                  }
                  val errors = results.collect { case Left(e) => e }
                  if (errors.nonEmpty) Left(XmlDecodingError.Multiple(errors).asInstanceOf[XmlDecodingError])
                  else Right(results.collect { case Right(v) => v }.asInstanceOf[A])
                })
              }

          case _ =>
            MIO.pure(Rule.yielded(s"The type ${Type[A].prettyPrint} is not a collection"))
        }
      }
  }

  @scala.annotation.nowarn("msg=is never used")
  object DecHandleAsSingletonRule extends DecoderDerivationRule("handle as singleton when possible") {

    def apply[A: DecoderCtx]: MIO[Rule.Applicability[Expr[Either[XmlDecodingError, A]]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as a singleton") >> {
        implicit val EitherT: Type[Either[XmlDecodingError, A]] = DTypes.DecoderResult[A]
        implicit val XmlDecodingErrorT: Type[XmlDecodingError] = DTypes.XmlDecodingError
        Expr.singletonOf[A] match {
          case Some(_) =>
            // Use setHelper/getHelper so the singleton Expr is created inside the helper's Quotes scope,
            // avoiding splice isolation errors on Scala 3
            for {
              _ <- dctx.setHelper[A] { (_, _) =>
                Expr.singletonOf[A] match {
                  case Some(singletonExpr) =>
                    MIO.pure(Expr.quote(Right(Expr.splice(singletonExpr))))
                  case None =>
                    MIO.fail(new RuntimeException(s"Singleton disappeared for ${Type[A].prettyPrint}"))
                }
              }
              result <- dctx.getHelper[A].flatMap {
                case Some(helperCall) =>
                  MIO.pure(Rule.matched(helperCall(dctx.elem, dctx.config)))
                case None =>
                  MIO.pure(Rule.yielded(s"Failed to build helper for singleton ${Type[A].prettyPrint}"))
              }
            } yield result
          case None =>
            MIO.pure(Rule.yielded(s"The type ${Type[A].prettyPrint} is not a singleton"))
        }
      }
  }

  object DecHandleAsCaseClassRule extends DecoderDerivationRule("handle as case class when possible") {

    def apply[A: DecoderCtx]: MIO[Rule.Applicability[Expr[Either[XmlDecodingError, A]]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as a case class") >> {
        CaseClass.parse[A].toEither match {
          case Right(caseClass) =>
            for {
              _ <- dctx.setHelper[A] { (elem, config) =>
                decodeCaseClassFields[A](caseClass, caseClass.primaryConstructor.parameters.flatten.toList)(
                  using dctx.nestInCache(elem, config)
                )
              }
              result <- dctx.getHelper[A].flatMap {
                case Some(helperCall) =>
                  MIO.pure(Rule.matched(helperCall(dctx.elem, dctx.config)))
                case None => MIO.pure(Rule.yielded(s"Failed to build helper for ${Type[A].prettyPrint}"))
              }
            } yield result
          case Left(reason) =>
            MIO.pure(Rule.yielded(reason))
        }
      }

    @scala.annotation.nowarn("msg=is never used")
    private def decodeCaseClassFields[A: DecoderCtx](
        caseClass: CaseClass[A],
        fields: List[(String, Parameter)]
    ): MIO[Expr[Either[XmlDecodingError, A]]] = {
      implicit val EitherT: Type[Either[XmlDecodingError, A]] = DTypes.DecoderResult[A]
      implicit val XmlDecodingErrorT: Type[XmlDecodingError] = DTypes.XmlDecodingError
      implicit val ElemT: Type[scala.xml.Elem] = DTypes.Elem
      implicit val StringT: Type[String] = DTypes.String
      implicit val AnyT: Type[Any] = DTypes.Any
      implicit val transientFieldT: Type[transientField] = DTypes.TransientField
      implicit val xmlNameT: Type[xmlName] = DTypes.XmlNameAnnotation
      implicit val xmlAttributeT: Type[xmlAttribute] = DTypes.XmlAttributeAnnotation
      implicit val xmlContentT: Type[xmlContent] = DTypes.XmlContentAnnotation

      val constructor = caseClass.primaryConstructor

      NonEmptyList.fromList(fields) match {
        case Some(fieldValues) =>
          fieldValues
            .traverse { case (fName, param) =>
              import param.tpe.Underlying as FieldType
              val isTransient = hasAnnotationType[transientField](param)
              val customName = getAnnotationStringArg[xmlName](param)
              val isAttrAnnotated = hasAnnotationType[xmlAttribute](param)
              val isContentAnnotated = hasAnnotationType[xmlContent](param)
              val xmlFieldName = customName.getOrElse(fName)

              if (isTransient) {
                val defaultValue: Expr[Any] = param.defaultValue match {
                  case Some(existentialOuter) =>
                    val methodOf = existentialOuter.value
                    methodOf.value match {
                      case noInstance: Method.NoInstance[?] =>
                        noInstance(Map.empty) match {
                          case Right(expr) => expr.asInstanceOf[Expr[Any]]
                          case Left(_)     =>
                            Environment.reportErrorAndAbort(
                              s"Field '$fName' is annotated with @transientField but its default value could not be resolved"
                            )
                        }
                      case _ =>
                        Environment.reportErrorAndAbort(
                          s"Field '$fName' is annotated with @transientField but has no default value"
                        )
                    }
                  case None =>
                    Environment.reportErrorAndAbort(
                      s"Field '$fName' is annotated with @transientField but has no default value"
                    )
                }
                MIO.pure(FieldDecoding.Default(defaultValue))
              } else if (isContentAnnotated) {
                // Decode from text content
                deriveDecoderRecursively[FieldType](using dctx.nest[FieldType](dctx.elem)).map { decodedExpr =>
                  FieldDecoding.FromContent(decodedExpr.asInstanceOf[Expr[Either[XmlDecodingError, Any]]])
                }
              } else if (isAttrAnnotated) {
                // Decode from attribute
                MIO.pure(FieldDecoding.FromAttribute(xmlFieldName, param.tpe))
              } else {
                // Decode from child element (default)
                MIO.pure(FieldDecoding.FromChildElement(xmlFieldName, param.tpe))
              }
            }
            .flatMap { decodings =>
              buildDecodeExpr[A](caseClass, constructor, fields, decodings.toList)
            }
        case None =>
          caseClass
            .construct[MIO](new CaseClass.ConstructField[MIO] {
              def apply(field: Parameter): MIO[Expr[field.tpe.Underlying]] =
                MIO.fail(
                  new RuntimeException(
                    s"Unexpected parameter in zero-argument case class ${Type[A].prettyPrint}"
                  )
                )
            })
            .flatMap {
              case Some(constructExpr) =>
                MIO.pure(Expr.quote {
                  Right(Expr.splice(constructExpr))
                })
              case None =>
                MIO.fail(new RuntimeException(s"Cannot construct ${Type[A].prettyPrint}"))
            }
      }
    }

    @scala.annotation.nowarn("msg=is never used|unused explicit parameter")
    private def buildDecodeExpr[A: DecoderCtx](
        caseClass: CaseClass[A],
        constructor: Method.NoInstance[A],
        fields: List[(String, Parameter)],
        decodings: List[FieldDecoding]
    ): MIO[Expr[Either[XmlDecodingError, A]]] = {
      implicit val EitherT: Type[Either[XmlDecodingError, A]] = DTypes.DecoderResult[A]
      implicit val XmlDecodingErrorT: Type[XmlDecodingError] = DTypes.XmlDecodingError
      implicit val ElemT: Type[scala.xml.Elem] = DTypes.Elem
      implicit val StringT: Type[String] = DTypes.String
      implicit val AnyT: Type[Any] = DTypes.Any
      implicit val EitherAnyT: Type[Either[XmlDecodingError, Any]] = DTypes.DecoderResultAny

      // Build accessor types needed later
      implicit val ArrayAnyT: Type[Array[Any]] = DTypes.ArrayAny
      implicit val IntT: Type[Int] = DTypes.Int

      // Build decode expressions, child lambdas, and accessor functions in a single pass.
      // This avoids path-dependent type issues with Expr.quote on Scala 2 (the old code used
      // `import param.tpe.Underlying as FieldT` inside Expr.quote's asInstanceOf, which leaked
      // the path `param` into generated code). Now we use unsafeCastWithFn with the child lambda
      // for type inference, keeping path-dependent types out of Expr.quote.
      fields
        .zip(decodings)
        .zipWithIndex
        .foldLeft(MIO.pure(List.empty[(Expr[Either[XmlDecodingError, Any]], Expr[Array[Any]] => (String, Expr_??))])) {
          case (accMIO, (((fName, param), decoding), idx)) =>
            import param.tpe.Underlying as FieldT
            accMIO.flatMap { acc =>
              decoding match {
                case FieldDecoding.Default(defaultExpr) =>
                  // Build a decode lambda purely for type inference in the accessor.
                  // This avoids using path-dependent FieldT inside Expr.quote.
                  implicit val eitherFieldT: Type[Either[XmlDecodingError, FieldT]] = DTypes.DecoderResult[FieldT]
                  val decodeExpr: Expr[Either[XmlDecodingError, Any]] =
                    Expr.quote(Right(Expr.splice(defaultExpr)): Either[XmlDecodingError, Any])
                  LambdaBuilder
                    .of1[scala.xml.Elem]("dummyElem")
                    .traverse { dummyElemExpr =>
                      deriveDecoderRecursively[FieldT](using dctx.nest[FieldT](dummyElemExpr))
                    }
                    .map { builder =>
                      val castLambda = builder
                        .build[Either[XmlDecodingError, FieldT]]
                        .asInstanceOf[Expr[scala.xml.Elem => Either[XmlDecodingError, FieldT]]]
                      val makeAccessor: Expr[Array[Any]] => (String, Expr_??) = { arrExpr =>
                        val typedExpr: Expr[FieldT] = Expr.quote {
                          XmlDerivationUtils.unsafeCastWithFn(
                            Expr.splice(arrExpr)(Expr.splice(Expr(idx))),
                            Expr.splice(castLambda)
                          )
                        }
                        (fName, typedExpr.as_??)
                      }
                      acc :+ (decodeExpr, makeAccessor)
                    }

                case FieldDecoding.FromContent(decodedExpr) =>
                  implicit val eitherFieldT: Type[Either[XmlDecodingError, FieldT]] = DTypes.DecoderResult[FieldT]
                  LambdaBuilder
                    .of1[scala.xml.Elem]("contentElem")
                    .traverse { contentElemExpr =>
                      deriveDecoderRecursively[FieldT](using dctx.nest[FieldT](contentElemExpr))
                    }
                    .map { builder =>
                      val castLambda = builder
                        .build[Either[XmlDecodingError, FieldT]]
                        .asInstanceOf[Expr[scala.xml.Elem => Either[XmlDecodingError, FieldT]]]
                      val makeAccessor: Expr[Array[Any]] => (String, Expr_??) = { arrExpr =>
                        val typedExpr: Expr[FieldT] = Expr.quote {
                          XmlDerivationUtils.unsafeCastWithFn(
                            Expr.splice(arrExpr)(Expr.splice(Expr(idx))),
                            Expr.splice(castLambda)
                          )
                        }
                        (fName, typedExpr.as_??)
                      }
                      acc :+ (decodedExpr, makeAccessor)
                    }

                case FieldDecoding.FromAttribute(attrName, _) =>
                  // Build a decode lambda for type inference in the accessor.
                  implicit val eitherFieldT: Type[Either[XmlDecodingError, FieldT]] = DTypes.DecoderResult[FieldT]
                  val decodeExpr: Expr[Either[XmlDecodingError, Any]] = Expr.quote {
                    XmlDerivationUtils
                      .getAttribute(Expr.splice(dctx.elem), Expr.splice(Expr(attrName)))
                      .asInstanceOf[Either[XmlDecodingError, Any]]
                  }
                  LambdaBuilder
                    .of1[scala.xml.Elem]("attrElem")
                    .traverse { attrElemExpr =>
                      deriveDecoderRecursively[FieldT](using dctx.nest[FieldT](attrElemExpr))
                    }
                    .map { builder =>
                      val castLambda = builder
                        .build[Either[XmlDecodingError, FieldT]]
                        .asInstanceOf[Expr[scala.xml.Elem => Either[XmlDecodingError, FieldT]]]
                      val makeAccessor: Expr[Array[Any]] => (String, Expr_??) = { arrExpr =>
                        val typedExpr: Expr[FieldT] = Expr.quote {
                          XmlDerivationUtils.unsafeCastWithFn(
                            Expr.splice(arrExpr)(Expr.splice(Expr(idx))),
                            Expr.splice(castLambda)
                          )
                        }
                        (fName, typedExpr.as_??)
                      }
                      acc :+ (decodeExpr, makeAccessor)
                    }

                case FieldDecoding.FromChildElement(childName, _) =>
                  implicit val eitherFieldT: Type[Either[XmlDecodingError, FieldT]] = DTypes.DecoderResult[FieldT]
                  LambdaBuilder
                    .of1[scala.xml.Elem]("childElem")
                    .traverse { childElemExpr =>
                      deriveDecoderRecursively[FieldT](using dctx.nest[FieldT](childElemExpr))
                    }
                    .map { builder =>
                      val childLambda = builder.build[Either[XmlDecodingError, FieldT]]
                      val childLambdaAsAny = childLambda
                        .asInstanceOf[Expr[scala.xml.Elem => Either[XmlDecodingError, Any]]]
                      val childLambdaForCast = childLambda
                        .asInstanceOf[Expr[scala.xml.Elem => Either[XmlDecodingError, FieldT]]]
                      val decodeExpr: Expr[Either[XmlDecodingError, Any]] = Expr.quote {
                        XmlDerivationUtils.getChildElem(Expr.splice(dctx.elem), Expr.splice(Expr(childName))).flatMap {
                          childElem =>
                            Expr.splice(childLambdaAsAny).apply(childElem)
                        }
                      }
                      val makeAccessor: Expr[Array[Any]] => (String, Expr_??) = { arrExpr =>
                        val typedExpr: Expr[FieldT] = Expr.quote {
                          XmlDerivationUtils.unsafeCastWithFn(
                            Expr.splice(arrExpr)(Expr.splice(Expr(idx))),
                            Expr.splice(childLambdaForCast)
                          )
                        }
                        (fName, typedExpr.as_??)
                      }
                      acc :+ (decodeExpr, makeAccessor)
                    }
              }
            }
        }
        .flatMap { fieldData =>
          val fieldDecodeExprs = fieldData.map(_._1)
          val makeAccessors = fieldData.map(_._2)

          // Build the result list at compile time using foldRight
          val listExpr: Expr[List[Either[XmlDecodingError, Any]]] =
            fieldDecodeExprs.foldRight(Expr.quote(List.empty[Either[XmlDecodingError, Any]])) { (elem, acc) =>
              Expr.quote(Expr.splice(elem) :: Expr.splice(acc))
            }

          // Build the constructor lambda using LambdaBuilder + primaryConstructor
          LambdaBuilder
            .of1[Array[Any]]("decodedValues")
            .traverse { decodedValuesExpr =>
              val fieldMap: Map[String, Expr_??] =
                makeAccessors.map(_(decodedValuesExpr)).toMap
              constructor(fieldMap) match {
                case Right(constructExpr) => MIO.pure(constructExpr)
                case Left(error)          =>
                  MIO.fail(new RuntimeException(s"Cannot construct ${Type[A].prettyPrint}: $error"))
              }
            }
            .map { builder =>
              val constructLambda = builder.build[A]
              Expr.quote {
                val fieldResults: List[Either[XmlDecodingError, Any]] = Expr.splice(listExpr)
                XmlDerivationUtils.sequenceDecodeResults(fieldResults).map { arr =>
                  Expr.splice(constructLambda).apply(arr)
                }
              }
            }
        }
    }
  }

  @scala.annotation.nowarn("msg=is never used")
  object DecHandleAsEnumRule extends DecoderDerivationRule("handle as sealed trait/enum when possible") {

    def apply[A: DecoderCtx]: MIO[Rule.Applicability[Expr[Either[XmlDecodingError, A]]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as a sealed trait/enum") >> {
        Enum.parse[A].toEither match {
          case Right(parsedEnum) =>
            for {
              _ <- dctx.setHelper[A] { (elem, config) =>
                decodeEnumCases[A](parsedEnum)(using dctx.nestInCache(elem, config))
              }
              result <- dctx.getHelper[A].flatMap {
                case Some(helperCall) =>
                  MIO.pure(Rule.matched(helperCall(dctx.elem, dctx.config)))
                case None =>
                  MIO.pure(Rule.yielded(s"Failed to build helper for ${Type[A].prettyPrint}"))
              }
            } yield result
          case Left(reason) =>
            MIO.pure(Rule.yielded(reason))
        }
      }

    @scala.annotation.nowarn("msg=is never used")
    private def decodeEnumCases[A: DecoderCtx](
        enumType: Enum[A]
    ): MIO[Expr[Either[XmlDecodingError, A]]] = {
      implicit val EitherT: Type[Either[XmlDecodingError, A]] = DTypes.DecoderResult[A]
      implicit val XmlDecodingErrorT: Type[XmlDecodingError] = DTypes.XmlDecodingError
      implicit val ElemT: Type[scala.xml.Elem] = DTypes.Elem
      implicit val StringT: Type[String] = DTypes.String
      implicit val AnyT: Type[Any] = DTypes.Any
      implicit val EitherAnyT: Type[Either[XmlDecodingError, Any]] = DTypes.DecoderResultAny
      val childrenList = enumType.directChildren.toList

      NonEmptyList.fromList(childrenList) match {
        case None =>
          val knownNames: List[String] = Nil
          MIO.pure(Expr.quote {
            Left(
              XmlDerivationUtils.failedToMatchSubtype("", Expr.splice(Expr(knownNames)))
            ): Either[XmlDecodingError, A]
          })

        case Some(children) =>
          // Derive all child decoders, collecting their helpers as lambdas
          children
            .traverse { case (childName, child) =>
              import child.Underlying as ChildType
              deriveChildDecoderLambda[A, ChildType](childName)
            }
            .flatMap { childLambdas =>
              val childNames: List[String] = childLambdas.toList.map(_._1)
              val childDecoderExprs: List[Expr[scala.xml.Elem => Either[XmlDecodingError, Any]]] =
                childLambdas.toList.map(_._2)

              // Wrap the final assembly in a LambdaBuilder to get a proper runSafe context
              // for creating Expr(childNames) — this avoids Scala 3 splice isolation issues
              LambdaBuilder
                .of1[scala.xml.Elem]("dispatchElem")
                .traverse { elemExpr =>
                  // Create the names list inside runSafe context
                  val namesListExpr: Expr[List[String]] = Expr(childNames)

                  // Build the decoders list expression using foldRight
                  val decodersListExpr: Expr[List[scala.xml.Elem => Either[XmlDecodingError, Any]]] =
                    childDecoderExprs.foldRight(
                      Expr.quote(List.empty[scala.xml.Elem => Either[XmlDecodingError, Any]])
                    ) { (decoder, acc) =>
                      Expr.quote(Expr.splice(decoder) :: Expr.splice(acc))
                    }

                  MIO.pure(Expr.quote {
                    XmlDerivationUtils.getAttribute(Expr.splice(elemExpr), "type") match {
                      case Right(typeName) =>
                        XmlDerivationUtils.dispatchByName[A](
                          typeName,
                          Expr.splice(elemExpr),
                          Expr.splice(namesListExpr),
                          Expr.splice(decodersListExpr)
                        )
                      case Left(_) =>
                        Left(XmlDecodingError.MissingDiscriminator("type", Expr.splice(elemExpr).label))
                    }
                  })
                }
                .map { builder =>
                  val dispatchLambda = builder.build[Either[XmlDecodingError, A]]
                  Expr.quote {
                    Expr.splice(dispatchLambda)(Expr.splice(dctx.elem))
                  }
                }
            }
      }
    }

    @scala.annotation.nowarn("msg=is never used")
    private def deriveChildDecoderLambda[A: DecoderCtx, ChildType: Type](
        childName: String
    ): MIO[(String, Expr[scala.xml.Elem => Either[XmlDecodingError, Any]])] = {
      implicit val XmlDecodingErrorT: Type[XmlDecodingError] = DTypes.XmlDecodingError
      implicit val ElemT: Type[scala.xml.Elem] = DTypes.Elem
      implicit val AnyT: Type[Any] = DTypes.Any
      implicit val EitherAnyT: Type[Either[XmlDecodingError, Any]] = DTypes.DecoderResultAny

      // Derive via full rules chain (singletons use setHelper too), then use cached helper
      implicit val EitherChildT: Type[Either[XmlDecodingError, ChildType]] = DTypes.DecoderResult[ChildType]
      implicit val ConfigT: Type[XmlConfig] = DTypes.XmlConfig
      deriveDecoderRecursively[ChildType](using dctx.nest[ChildType](dctx.elem)).flatMap { _ =>
        dctx.getHelper[ChildType].flatMap {
          case Some(helper) =>
            // Build a lambda that calls the cached helper
            LambdaBuilder
              .of1[scala.xml.Elem]("childElem")
              .traverse { childElemExpr =>
                val helperCallExpr: Expr[Either[XmlDecodingError, ChildType]] =
                  helper(childElemExpr, dctx.config)
                MIO.pure(Expr.quote {
                  Expr.splice(helperCallExpr).asInstanceOf[Either[XmlDecodingError, Any]]
                })
              }
              .map { builder =>
                val lambda = builder.build[Either[XmlDecodingError, Any]]
                (childName, lambda)
              }
          case None =>
            MIO.fail(
              new RuntimeException(s"No helper found for enum case ${Type[ChildType].prettyPrint}")
            )
        }
      }
    }
  }

  // Field decoding types
  sealed trait FieldDecoding
  object FieldDecoding {
    case class Default(value: Expr[Any]) extends FieldDecoding
    case class FromAttribute(name: String, fieldType: ??) extends FieldDecoding
    case class FromChildElement(name: String, fieldType: ??) extends FieldDecoding
    case class FromContent(decoded: Expr[Either[XmlDecodingError, Any]]) extends FieldDecoding
  }

  // Error types

  sealed abstract class DecoderDerivationError(val message: String) extends Exception(message)
  object DecoderDerivationError {
    final case class UnsupportedType(typeName: String, reasons: List[String])
        extends DecoderDerivationError(
          s"Cannot derive XmlDecoder for type $typeName:\n${reasons.mkString("\n")}"
        )
  }

  // Types

  private[compiletime] object DTypes {
    lazy val Elem: Type[scala.xml.Elem] = Type.of[scala.xml.Elem]
    lazy val XmlConfig: Type[hearth.kindlings.xmlderivation.XmlConfig] =
      Type.of[hearth.kindlings.xmlderivation.XmlConfig]
    lazy val String: Type[String] = Type.of[String]
    lazy val Boolean: Type[Boolean] = Type.of[Boolean]
    lazy val Byte: Type[Byte] = Type.of[Byte]
    lazy val Short: Type[Short] = Type.of[Short]
    lazy val Long: Type[Long] = Type.of[Long]
    lazy val Float: Type[Float] = Type.of[Float]
    lazy val Double: Type[Double] = Type.of[Double]
    lazy val Char: Type[Char] = Type.of[Char]
    lazy val BigDecimal: Type[BigDecimal] = Type.of[BigDecimal]
    lazy val BigInt: Type[BigInt] = Type.of[BigInt]
    lazy val XmlDecodingError: Type[hearth.kindlings.xmlderivation.XmlDecodingError] =
      Type.of[hearth.kindlings.xmlderivation.XmlDecodingError]
    def DecoderResult[A: Type]: Type[Either[hearth.kindlings.xmlderivation.XmlDecodingError, A]] =
      Type.of[Either[hearth.kindlings.xmlderivation.XmlDecodingError, A]]
    lazy val DecoderResultAny: Type[Either[hearth.kindlings.xmlderivation.XmlDecodingError, Any]] =
      Type.of[Either[hearth.kindlings.xmlderivation.XmlDecodingError, Any]]
    lazy val Any: Type[Any] = Type.of[Any]
    lazy val Int: Type[Int] = Type.of[Int]
    lazy val ArrayAny: Type[Array[Any]] = Type.of[Array[Any]]
    def eitherStringType[A: Type]: Type[Either[String, A]] = Type.of[Either[String, A]]
    def TransientField: Type[hearth.kindlings.xmlderivation.annotations.transientField] =
      Type.of[hearth.kindlings.xmlderivation.annotations.transientField]
    def XmlNameAnnotation: Type[hearth.kindlings.xmlderivation.annotations.xmlName] =
      Type.of[hearth.kindlings.xmlderivation.annotations.xmlName]
    def XmlAttributeAnnotation: Type[hearth.kindlings.xmlderivation.annotations.xmlAttribute] =
      Type.of[hearth.kindlings.xmlderivation.annotations.xmlAttribute]
    def XmlContentAnnotation: Type[hearth.kindlings.xmlderivation.annotations.xmlContent] =
      Type.of[hearth.kindlings.xmlderivation.annotations.xmlContent]
    def XmlDecoder[A: Type]: Type[hearth.kindlings.xmlderivation.XmlDecoder[A]] =
      Type.of[hearth.kindlings.xmlderivation.XmlDecoder[A]]
    def KindlingsXmlDecoder[A: Type]: Type[hearth.kindlings.xmlderivation.KindlingsXmlDecoder[A]] =
      Type.of[hearth.kindlings.xmlderivation.KindlingsXmlDecoder[A]]
    val DecoderLogDerivation: Type[hearth.kindlings.xmlderivation.KindlingsXmlDecoder.LogDerivation] =
      Type.of[hearth.kindlings.xmlderivation.KindlingsXmlDecoder.LogDerivation]
  }

  private type XmlDecoder[A] = hearth.kindlings.xmlderivation.XmlDecoder[A]
}
