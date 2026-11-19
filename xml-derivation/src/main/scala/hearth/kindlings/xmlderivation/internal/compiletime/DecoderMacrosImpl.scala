package hearth.kindlings.xmlderivation.internal.compiletime

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

import hearth.kindlings.xmlderivation.{KindlingsXmlDecoder, XmlConfig, XmlDecodingError}
import hearth.kindlings.xmlderivation.internal.runtime.XmlDerivationUtils

trait DecoderMacrosImpl
    extends XmlDerivationTimeout
    with XmlDerivationPolicy
    with XmlFieldLayouts
    with hearth.kindlings.derivation.compiletime.LoadStandardExtensionsOnce
    with hearth.kindlings.derivation.compiletime.MethodFolds
    with hearth.kindlings.derivation.compiletime.FlatFieldsConstruct
    with rules.DecoderUseCachedDefWhenAvailableRuleImpl
    with rules.DecoderUseImplicitWhenAvailableRuleImpl
    with rules.DecoderHandleAsBuiltInRuleImpl
    with rules.DecoderHandleAsValueTypeRuleImpl
    with rules.DecoderHandleAsOptionRuleImpl
    with rules.DecoderHandleAsMapRuleImpl
    with rules.DecoderHandleAsCollectionRuleImpl
    with rules.DecoderHandleAsSingletonRuleImpl
    with rules.DecoderHandleAsCaseClassRuleImpl
    with rules.DecoderHandleAsEnumRuleImpl
    with rules.DecoderDerivationPolicyRuleImpl { this: MacroCommons & StdExtensions & AnnotationSupport =>

  // $COVERAGE-OFF$
  override protected def derivationPolicyTypeClassName: String = "KindlingsXmlDecoder"
  // $COVERAGE-ON$

  // Entrypoints

  @scala.annotation.nowarn("msg=is never used")
  def deriveInlineDecode[A: Type](
      elemExpr: Expr[scala.xml.Elem],
      configExpr: Expr[XmlConfig]
  ): Expr[Either[XmlDecodingError, A]] = {
    implicit val EitherT: Type[Either[XmlDecodingError, A]] = DTypes.DecoderResult[A]

    deriveDecoderAndAdaptForEntrypoint[A, Either[XmlDecodingError, A]](
      "KindlingsXmlDecoder.decode",
      configExpr,
      derivedType = None
    )(decode => decode(elemExpr, configExpr))
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

    deriveDecoderAndAdaptForEntrypoint[A, Either[XmlDecodingError, A]](
      "KindlingsXmlDecoder.fromXmlString",
      configExpr,
      derivedType = None
    ) { decode =>
      Expr.quote {
        val cfg = Expr.splice(configExpr)
        XmlDerivationUtils.parseAndDecode[A](
          Expr.splice(xmlExpr),
          (elem: scala.xml.Elem) => Expr.splice(decode(Expr.quote(elem), Expr.quote(cfg)))
        )
      }
    }
  }

  @scala.annotation.nowarn("msg=is never used")
  def deriveDecoderTypeClass[A: Type](configExpr: Expr[XmlConfig]): Expr[KindlingsXmlDecoder[A]] = {
    implicit val KindlingsDecoderA: Type[KindlingsXmlDecoder[A]] = DTypes.KindlingsXmlDecoder[A]
    implicit val EitherT: Type[Either[XmlDecodingError, A]] = DTypes.DecoderResult[A]
    implicit val ElemT: Type[scala.xml.Elem] = DTypes.Elem
    implicit val ConfigT: Type[XmlConfig] = DTypes.XmlConfig
    implicit val XmlDecodingErrorT: Type[XmlDecodingError] = DTypes.XmlDecodingError

    deriveDecoderAndAdaptForEntrypoint[A, KindlingsXmlDecoder[A]](
      "KindlingsXmlDecoder.derived",
      configExpr,
      derivedType = Some(Type[A].as_??)
    ) { decode =>
      Expr.quote {
        val cfg = Expr.splice(configExpr)
        hearth.kindlings.xmlderivation.internal.runtime.XmlDerivationFactories.decoderInstance[A] {
          (elem: scala.xml.Elem) => Expr.splice(decode(Expr.quote(elem), Expr.quote(cfg)))
        }
      }
    }
  }

  /** Derives the decoder of `A` once, at the top level of the expansion (the cached defs of every type it needs, put
    * around the result), and lets the entry point call it: `(element, config) => result`. Handles logging and error
    * reporting.
    *
    * (Deriving at the top level, not inside the quote that uses the result: on Scala 3 every splice has its own
    * `Quotes`, and expressions created by a derivation running inside one cannot be used by cached defs.)
    */
  @scala.annotation.nowarn("msg=is never used")
  def deriveDecoderAndAdaptForEntrypoint[A: Type, Out: Type](
      macroName: String,
      configExpr: Expr[XmlConfig],
      derivedType: Option[??]
  )(
      adapt: ((Expr[scala.xml.Elem], Expr[XmlConfig]) => Expr[Either[XmlDecodingError, A]]) => Expr[Out]
  ): Expr[Out] = {
    implicit val ElemT: Type[scala.xml.Elem] = DTypes.Elem
    implicit val ConfigT: Type[XmlConfig] = DTypes.XmlConfig
    val evConfig: Option[XmlConfig] = configExpr.semiEval.toOption

    if (Type[A] =:= Type.of[Nothing].asInstanceOf[Type[A]] || Type[A] =:= Type.of[Any].asInstanceOf[Type[A]])
      Environment.reportErrorAndAbort(
        s"$macroName: type parameter was inferred as ${Type[A].prettyPrint}, which is likely unintended.\n" +
          s"Provide an explicit type parameter, e.g.: $macroName[MyType]\n" +
          "or add a type ascription to the result variable."
      )

    Log
      .namedScope(
        s"Deriving XML decoder for ${Type[A].prettyPrint} at: ${Environment.currentPosition.prettyPrint}"
      ) {
        // Run the derivation with a placeholder context to populate the cache: every type gets a cached def
        val placeholderCtx = DecoderCtx.from[A](
          elem = Expr.quote(null.asInstanceOf[scala.xml.Elem]),
          config = Expr.quote(null.asInstanceOf[XmlConfig]),
          derivedType = derivedType,
          evaluatedConfig = evConfig
        )
        for {
          _ <- ensureStandardExtensionsLoaded()
          _ <- deriveDecoderRecursively[A](using placeholderCtx)
          helperOpt <- placeholderCtx.getHelper[A]
          cache <- placeholderCtx.cache.get
          helper <- helperOpt match {
            case Some(helper) => MIO.pure(helper)
            case None         => MIO.fail(new RuntimeException(s"No decode helper for ${Type[A].prettyPrint}"))
          }
        } yield cache.toValDefs.use(_ => adapt(helper))
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
        val errorsRendered =
          hearth.kindlings.derivation.compiletime.DerivationErrorRendering.renderErrors(errors.toVector)
        val hint =
          "Enable debug logging with: import hearth.kindlings.xmlderivation.debug.logDerivationForKindlingsXmlDecoder or scalac option -Xmacro-settings:xmlDerivation.logDerivation=true"
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
      derivedType: Option[??],
      evaluatedConfig: Option[XmlConfig] = None
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
            val bodyExpr = runSafe(helper(elem, config))
            Expr.quote {
              Expr.splice(bodyExpr).asInstanceOf[Either[XmlDecodingError, B]]
            }
          })
        }
        _ <- Log.info(s"Defined decode helper for ${Type[B].prettyPrint}")
      } yield ()
    }

    /** The cached `(case name, config) => value` helper of an enum of singletons. */
    def getEnumFromNameHelper[B: Type]
        : MIO[Option[(Expr[String], Expr[XmlConfig]) => Expr[Either[XmlDecodingError, B]]]] = {
      implicit val EitherT: Type[Either[XmlDecodingError, B]] = DTypes.DecoderResult[B]
      implicit val ConfigT: Type[XmlConfig] = DTypes.XmlConfig
      implicit val StringT: Type[String] = DTypes.String
      cache.get2Ary[String, XmlConfig, Either[XmlDecodingError, B]]("cached-enum-from-name-method")
    }
    def setEnumFromNameHelper[B: Type](
        helper: (Expr[String], Expr[XmlConfig]) => MIO[Expr[Either[XmlDecodingError, B]]]
    ): MIO[Unit] = {
      implicit val EitherT: Type[Either[XmlDecodingError, B]] = DTypes.DecoderResult[B]
      implicit val ConfigT: Type[XmlConfig] = DTypes.XmlConfig
      implicit val StringT: Type[String] = DTypes.String
      val defBuilder =
        ValDefBuilder.ofDef2[String, XmlConfig, Either[XmlDecodingError, B]](s"enumFromName_${Type[B].shortName}")
      for {
        _ <- cache.forwardDeclare("cached-enum-from-name-method", defBuilder)
        _ <- MIO.scoped { runSafe =>
          runSafe(cache.buildCachedWith("cached-enum-from-name-method", defBuilder) { case (_, (name, config)) =>
            runSafe(helper(name, config))
          })
        }
      } yield ()
    }

    override def toString: String =
      s"decode[${tpe.prettyPrint}](elem = ${elem.prettyPrint}, config = ${config.prettyPrint})"
  }
  object DecoderCtx {

    def from[A: Type](
        elem: Expr[scala.xml.Elem],
        config: Expr[XmlConfig],
        derivedType: Option[??],
        evaluatedConfig: Option[XmlConfig] = None
    ): DecoderCtx[A] = DecoderCtx(
      tpe = Type[A],
      elem = elem,
      config = config,
      cache = ValDefsCache.mlocal,
      derivedType = derivedType,
      evaluatedConfig = evaluatedConfig
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
          DecoderDerivationPolicyRule,
          DecoderHandleAsValueTypeRule,
          DecoderHandleAsBuiltInRule,
          DecoderHandleAsOptionRule,
          // DecoderHandleAsCollectionRule now handles maps too (single IsCollection parse + IsMapOf dispatch),
          // so the standalone DecoderHandleAsMapRule is no longer in the chain (its object is kept for
          // decodeMapEntries).
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
    case class FromAttribute(nameExpr: Expr[String], shape: XmlTextShape) extends FieldDecoding
    case class FromOptionalAttribute(nameExpr: Expr[String], shape: XmlTextShape, whenAbsent: Expr[Any])
        extends FieldDecoding
    case class FromAttributeWithDefault(nameExpr: Expr[String], shape: XmlTextShape, defaultValue: Expr[Any])
        extends FieldDecoding
    case class FromAttributeWithRuntimeDefault(nameExpr: Expr[String], shape: XmlTextShape, defaultValue: Expr[Any])
        extends FieldDecoding

    /** The configuration is only known at runtime: read from an attribute when `defaultFieldMode` is `Attribute` (and,
      * with `needsEnumAsStrings`, `enumAsStrings` is set), else from an element.
      */
    case class DynamicMode(asAttribute: FieldDecoding, asElement: FieldDecoding, needsEnumAsStrings: Boolean)
        extends FieldDecoding
    case class FromChildElement(nameExpr: Expr[String]) extends FieldDecoding
    case class FromChildElementOrEmpty(nameExpr: Expr[String]) extends FieldDecoding
    case class FromChildElementWithDefault(nameExpr: Expr[String], defaultValue: Expr[Any]) extends FieldDecoding
    case class FromChildElementWithRuntimeDefault(nameExpr: Expr[String], defaultValue: Expr[Any]) extends FieldDecoding

    /** `@xmlContent`: the text content of the element. */
    case class FromContent(shape: XmlAttrShape) extends FieldDecoding

    /** `@xmlUnwrapped`: every child element with that name is an item. Without any, `defaultWhenAbsent` is the value
      * (only when `useDefaults` is set, if that is only known at runtime: `defaultNeedsUseDefaults`).
      */
    case class FromRepeatedChildren(
        nameExpr: Expr[String],
        defaultWhenAbsent: Option[Expr[Any]],
        defaultNeedsUseDefaults: Boolean
    ) extends FieldDecoding

    /** `@xmlWrapper`: `inner` read from the wrapper element (from an empty one when it is absent, unless
      * `defaultWhenAbsent` applies).
      */
    case class FromWrapper(
        wrapperName: String,
        inner: FieldDecoding,
        defaultWhenAbsent: Option[Expr[Any]],
        defaultNeedsUseDefaults: Boolean
    ) extends FieldDecoding
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
    def XmlElementAnnotation: Type[hearth.kindlings.xmlderivation.annotations.xmlElement] =
      Type.of[hearth.kindlings.xmlderivation.annotations.xmlElement]
    lazy val Unit: Type[Unit] = Type.of[Unit]
    def XmlDecoder[A: Type]: Type[hearth.kindlings.xmlderivation.XmlDecoder[A]] =
      Type.of[hearth.kindlings.xmlderivation.XmlDecoder[A]]
    def KindlingsXmlDecoder[A: Type]: Type[hearth.kindlings.xmlderivation.KindlingsXmlDecoder[A]] =
      Type.of[hearth.kindlings.xmlderivation.KindlingsXmlDecoder[A]]
    val DecoderLogDerivation: Type[hearth.kindlings.xmlderivation.KindlingsXmlDecoder.LogDerivation] =
      Type.of[hearth.kindlings.xmlderivation.KindlingsXmlDecoder.LogDerivation]
  }

  private type XmlDecoder[A] = hearth.kindlings.xmlderivation.XmlDecoder[A]
}
