package hearth.kindlings.xmlderivation.internal.compiletime

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

import hearth.kindlings.xmlderivation.{KindlingsXmlDecoder, XmlConfig, XmlDecodingError}
import hearth.kindlings.xmlderivation.internal.runtime.XmlDerivationUtils

trait DecoderMacrosImpl
    extends XmlDerivationTimeout
    with rules.DecoderUseCachedDefWhenAvailableRuleImpl
    with rules.DecoderUseImplicitWhenAvailableRuleImpl
    with rules.DecoderHandleAsBuiltInRuleImpl
    with rules.DecoderHandleAsValueTypeRuleImpl
    with rules.DecoderHandleAsOptionRuleImpl
    with rules.DecoderHandleAsMapRuleImpl
    with rules.DecoderHandleAsCollectionRuleImpl
    with rules.DecoderHandleAsSingletonRuleImpl
    with rules.DecoderHandleAsCaseClassRuleImpl
    with rules.DecoderHandleAsEnumRuleImpl { this: MacroCommons & StdExtensions & AnnotationSupport =>

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
        dctx.getHelper[A].flatMap {
          case Some(helperCall) =>
            Log.info(s"Using cached decoder helper for ${Type[A].prettyPrint}") >>
              MIO.pure(helperCall(dctx.elem, dctx.config))
          case None =>
            dctx.setHelper[A] { (elem, config) =>
              deriveDecoderViaRules[A](using dctx.nestInCache(elem, config))
            } >> dctx.getHelper[A].flatMap {
              case Some(helperCall) =>
                MIO.pure(helperCall(dctx.elem, dctx.config))
              case None =>
                deriveDecoderViaRules[A]
            }
        }
      }

  private def deriveDecoderViaRules[A: DecoderCtx]: MIO[Expr[Either[XmlDecodingError, A]]] =
    Log
      .namedScope(s"Deriving XML decoder via rules for type ${Type[A].prettyPrint}") {
        Rules(
          DecoderUseImplicitWhenAvailableRule,
          DecoderHandleAsBuiltInRule,
          DecoderHandleAsValueTypeRule,
          DecoderHandleAsOptionRule,
          DecoderHandleAsMapRule,
          DecoderHandleAsCollectionRule,
          DecoderHandleAsSingletonRule,
          DecoderHandleAsCaseClassRule,
          DecoderHandleAsEnumRule
        )(_[A]).flatMap {
          case Right(result) =>
            Log.info(s"Derived XML decoder for ${Type[A].prettyPrint}: ${result.prettyPrint}") >>
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

  // Field decoding types
  sealed trait FieldDecoding
  object FieldDecoding {
    case class Default(value: Expr[Any]) extends FieldDecoding
    case class FromAttribute(name: String, fieldType: ??) extends FieldDecoding
    case class FromChildElement(name: String, fieldType: ??) extends FieldDecoding
    case class FromContent(decoded: Expr[Either[XmlDecodingError, Any]]) extends FieldDecoding
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
