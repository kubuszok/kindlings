package hearth.kindlings.xmlderivation.internal.compiletime

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

import hearth.kindlings.xmlderivation.{KindlingsXmlEncoder, XmlConfig}
import hearth.kindlings.xmlderivation.internal.runtime.XmlDerivationUtils

trait EncoderMacrosImpl
    extends XmlDerivationTimeout
    with rules.EncoderUseCachedDefWhenAvailableRuleImpl
    with rules.EncoderUseImplicitWhenAvailableRuleImpl
    with rules.EncoderHandleAsBuiltInRuleImpl
    with rules.EncoderHandleAsValueTypeRuleImpl
    with rules.EncoderHandleAsOptionRuleImpl
    with rules.EncoderHandleAsMapRuleImpl
    with rules.EncoderHandleAsCollectionRuleImpl
    with rules.EncoderHandleAsSingletonRuleImpl
    with rules.EncoderHandleAsCaseClassRuleImpl
    with rules.EncoderHandleAsEnumRuleImpl { this: MacroCommons & StdExtensions & AnnotationSupport =>

  // Entrypoints

  def deriveInlineEncode[A: Type](
      valueExpr: Expr[A],
      elementNameExpr: Expr[String],
      configExpr: Expr[XmlConfig]
  ): Expr[scala.xml.Elem] = {
    implicit val ElemT: Type[scala.xml.Elem] = Types.Elem
    implicit val ConfigT: Type[XmlConfig] = Types.XmlConfig
    implicit val StringT: Type[String] = Types.String

    deriveEncoderFromCtxAndAdaptForEntrypoint[A, scala.xml.Elem]("KindlingsXmlEncoder.encode") { fromCtx =>
      ValDefs.createVal[A](valueExpr).use { valueVal =>
        ValDefs.createVal[XmlConfig](configExpr).use { configVal =>
          ValDefs.createVal[String](elementNameExpr).use { nameVal =>
            Expr.quote {
              val _ = Expr.splice(valueVal)
              val _ = Expr.splice(configVal)
              val _ = Expr.splice(nameVal)
              Expr.splice(fromCtx(EncoderCtx.from(valueVal, nameVal, configVal, derivedType = None)))
            }
          }
        }
      }
    }
  }

  @scala.annotation.nowarn("msg=is never used")
  def deriveInlineToXmlString[A: Type](
      valueExpr: Expr[A],
      elementNameExpr: Expr[String],
      configExpr: Expr[XmlConfig]
  ): Expr[String] = {
    implicit val ElemT: Type[scala.xml.Elem] = Types.Elem
    implicit val ConfigT: Type[XmlConfig] = Types.XmlConfig
    implicit val StringT: Type[String] = Types.String

    deriveEncoderFromCtxAndAdaptForEntrypoint[A, String]("KindlingsXmlEncoder.toXmlString") { fromCtx =>
      ValDefs.createVal[A](valueExpr).use { valueVal =>
        ValDefs.createVal[XmlConfig](configExpr).use { configVal =>
          ValDefs.createVal[String](elementNameExpr).use { nameVal =>
            Expr.quote {
              val _ = Expr.splice(valueVal)
              val _ = Expr.splice(configVal)
              val _ = Expr.splice(nameVal)
              XmlDerivationUtils.elemToString(
                Expr.splice(fromCtx(EncoderCtx.from(valueVal, nameVal, configVal, derivedType = None)))
              )
            }
          }
        }
      }
    }
  }

  @scala.annotation.nowarn("msg=is never used")
  def deriveEncoderTypeClass[A: Type](configExpr: Expr[XmlConfig]): Expr[KindlingsXmlEncoder[A]] = {
    implicit val EncoderA: Type[XmlEncoder[A]] = Types.XmlEncoder[A]
    implicit val KindlingsEncoderA: Type[KindlingsXmlEncoder[A]] = Types.KindlingsXmlEncoder[A]
    implicit val ElemT: Type[scala.xml.Elem] = Types.Elem
    implicit val ConfigT: Type[XmlConfig] = Types.XmlConfig
    implicit val StringT: Type[String] = Types.String
    val selfType: Option[??] = Some(Type[A].as_??)

    deriveEncoderFromCtxAndAdaptForEntrypoint[A, KindlingsXmlEncoder[A]]("KindlingsXmlEncoder.derived") { fromCtx =>
      ValDefs.createVal[XmlConfig](configExpr).use { configVal =>
        Expr.quote {
          val cfg = Expr.splice(configVal)
          new KindlingsXmlEncoder[A] {
            def encode(value: A, elementName: String): scala.xml.Elem = {
              val _ = value
              val _ = elementName
              val _ = cfg
              Expr.splice {
                fromCtx(
                  EncoderCtx.from(Expr.quote(value), Expr.quote(elementName), Expr.quote(cfg), derivedType = selfType)
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
      provideCtxAndAdapt: (EncoderCtx[A] => Expr[scala.xml.Elem]) => Expr[Out]
  ): Expr[Out] = {
    if (Type[A] =:= Type.of[Nothing].asInstanceOf[Type[A]] || Type[A] =:= Type.of[Any].asInstanceOf[Type[A]])
      Environment.reportErrorAndAbort(
        s"$macroName: type parameter was inferred as ${Type[A].prettyPrint}, which is likely unintended.\n" +
          s"Provide an explicit type parameter, e.g.: $macroName[MyType](...)\n" +
          "or add a type ascription to the result variable."
      )
    Log
      .namedScope(
        s"Deriving XML encoder for ${Type[A].prettyPrint} at: ${Environment.currentPosition.prettyPrint}"
      ) {
        MIO.scoped { runSafe =>
          val fromCtx: (EncoderCtx[A] => Expr[scala.xml.Elem]) = (ctx: EncoderCtx[A]) =>
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
        Log.info(s"Derived final XML encoder result: ${result.prettyPrint}")
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
          "Enable debug logging with: import hearth.kindlings.xmlderivation.debug.logDerivationForKindlingsXmlEncoder or scalac option -Xmacro-settings:xmlDerivation.logDerivation=true"
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
    implicit val LogDerivation: Type[KindlingsXmlEncoder.LogDerivation] = Types.EncoderLogDerivation
    def logDerivationImported = Expr.summonImplicit[KindlingsXmlEncoder.LogDerivation].isDefined

    def logDerivationSetGlobally = (for {
      data <- Environment.typedSettings.toOption
      xmlDerivation <- data.get("xmlDerivation")
      shouldLog <- xmlDerivation.get("logDerivation").flatMap(_.asBoolean)
    } yield shouldLog).getOrElse(false)

    logDerivationImported || logDerivationSetGlobally
  }

  // Context

  final case class EncoderCtx[A](
      tpe: Type[A],
      value: Expr[A],
      elementName: Expr[String],
      config: Expr[XmlConfig],
      cache: MLocal[ValDefsCache],
      derivedType: Option[??]
  ) {

    def nest[B: Type](newValue: Expr[B]): EncoderCtx[B] = copy[B](
      tpe = Type[B],
      value = newValue
    )

    def nestInCache(
        newValue: Expr[A],
        newElementName: Expr[String],
        newConfig: Expr[XmlConfig]
    ): EncoderCtx[A] = copy(
      value = newValue,
      elementName = newElementName,
      config = newConfig
    )

    def getInstance[B: Type]: MIO[Option[Expr[XmlEncoder[B]]]] = {
      implicit val EncoderB: Type[XmlEncoder[B]] = Types.XmlEncoder[B]
      cache.get0Ary[XmlEncoder[B]]("cached-encoder-instance")
    }
    def setInstance[B: Type](instance: Expr[XmlEncoder[B]]): MIO[Unit] = {
      implicit val EncoderB: Type[XmlEncoder[B]] = Types.XmlEncoder[B]
      Log.info(s"Caching XmlEncoder instance for ${Type[B].prettyPrint}") >>
        cache.buildCachedWith(
          "cached-encoder-instance",
          ValDefBuilder.ofLazy[XmlEncoder[B]](s"encoder_${Type[B].shortName}")
        )(_ => instance)
    }

    def getHelper[B: Type]: MIO[Option[(Expr[B], Expr[String], Expr[XmlConfig]) => Expr[scala.xml.Elem]]] = {
      implicit val ElemT: Type[scala.xml.Elem] = Types.Elem
      implicit val ConfigT: Type[XmlConfig] = Types.XmlConfig
      implicit val StringT: Type[String] = Types.String
      cache.get3Ary[B, String, XmlConfig, scala.xml.Elem]("cached-encode-method")
    }
    def setHelper[B: Type](
        helper: (Expr[B], Expr[String], Expr[XmlConfig]) => MIO[Expr[scala.xml.Elem]]
    ): MIO[Unit] = {
      implicit val ElemT: Type[scala.xml.Elem] = Types.Elem
      implicit val ConfigT: Type[XmlConfig] = Types.XmlConfig
      implicit val StringT: Type[String] = Types.String
      val defBuilder =
        ValDefBuilder.ofDef3[B, String, XmlConfig, scala.xml.Elem](s"encode_${Type[B].shortName}")
      for {
        _ <- Log.info(s"Forward-declaring encode helper for ${Type[B].prettyPrint}")
        _ <- cache.forwardDeclare("cached-encode-method", defBuilder)
        _ <- MIO.scoped { runSafe =>
          runSafe(cache.buildCachedWith("cached-encode-method", defBuilder) { case (_, (value, name, config)) =>
            runSafe(helper(value, name, config))
          })
        }
        _ <- Log.info(s"Defined encode helper for ${Type[B].prettyPrint}")
      } yield ()
    }

    override def toString: String =
      s"encode[${tpe.prettyPrint}](value = ${value.prettyPrint}, elementName = ${elementName.prettyPrint}, config = ${config.prettyPrint})"
  }
  object EncoderCtx {

    def from[A: Type](
        value: Expr[A],
        elementName: Expr[String],
        config: Expr[XmlConfig],
        derivedType: Option[??]
    ): EncoderCtx[A] = EncoderCtx(
      tpe = Type[A],
      value = value,
      elementName = elementName,
      config = config,
      cache = ValDefsCache.mlocal,
      derivedType = derivedType
    )
  }

  def ectx[A](implicit A: EncoderCtx[A]): EncoderCtx[A] = A

  implicit def currentEncoderValueType[A: EncoderCtx]: Type[A] = ectx.tpe

  abstract class EncoderDerivationRule(val name: String) extends Rule {
    def apply[A: EncoderCtx]: MIO[Rule.Applicability[Expr[scala.xml.Elem]]]
  }

  // The actual derivation logic

  def deriveEncoderRecursively[A: EncoderCtx]: MIO[Expr[scala.xml.Elem]] =
    Log
      .namedScope(s"Deriving XML encoder for type ${Type[A].prettyPrint}") {
        ectx.getHelper[A].flatMap {
          case Some(helperCall) =>
            Log.info(s"Using cached encoder helper for ${Type[A].prettyPrint}") >>
              MIO.pure(helperCall(ectx.value, ectx.elementName, ectx.config))
          case None =>
            ectx.setHelper[A] { (value, name, config) =>
              deriveEncoderViaRules[A](using ectx.nestInCache(value, name, config))
            } >> ectx.getHelper[A].flatMap {
              case Some(helperCall) =>
                MIO.pure(helperCall(ectx.value, ectx.elementName, ectx.config))
              case None =>
                deriveEncoderViaRules[A]
            }
        }
      }

  private def deriveEncoderViaRules[A: EncoderCtx]: MIO[Expr[scala.xml.Elem]] =
    Log
      .namedScope(s"Deriving XML encoder via rules for type ${Type[A].prettyPrint}") {
        Rules(
          EncoderUseImplicitWhenAvailableRule,
          EncoderHandleAsBuiltInRule,
          EncoderHandleAsValueTypeRule,
          EncoderHandleAsOptionRule,
          EncoderHandleAsMapRule,
          EncoderHandleAsCollectionRule,
          EncoderHandleAsSingletonRule,
          EncoderHandleAsCaseClassRule,
          EncoderHandleAsEnumRule
        )(_[A]).flatMap {
          case Right(result) =>
            Log.info(s"Derived XML encoder for ${Type[A].prettyPrint}: ${result.prettyPrint}") >>
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

  // Field encoding result type
  sealed trait FieldEncoding
  object FieldEncoding {
    case object Skip extends FieldEncoding
    case class Attr(name: String, value: Expr[Any]) extends FieldEncoding
    case class Child(elem: Expr[scala.xml.Elem]) extends FieldEncoding
    case class WrappedChild(wrapperName: String, elem: Expr[scala.xml.Elem]) extends FieldEncoding
    case class Content(elem: Expr[scala.xml.Elem]) extends FieldEncoding
  }

  // Types

  private[compiletime] object Types {
    def Elem: Type[scala.xml.Elem] = Type.of[scala.xml.Elem]
    def Node: Type[scala.xml.Node] = Type.of[scala.xml.Node]
    def XmlConfig: Type[hearth.kindlings.xmlderivation.XmlConfig] = Type.of[hearth.kindlings.xmlderivation.XmlConfig]
    def XmlFieldMode: Type[hearth.kindlings.xmlderivation.XmlFieldMode] =
      Type.of[hearth.kindlings.xmlderivation.XmlFieldMode]
    def String: Type[String] = Type.of[String]
    def Boolean: Type[Boolean] = Type.of[Boolean]
    def Byte: Type[Byte] = Type.of[Byte]
    def Short: Type[Short] = Type.of[Short]
    def Int: Type[Int] = Type.of[Int]
    def Long: Type[Long] = Type.of[Long]
    def Float: Type[Float] = Type.of[Float]
    def Double: Type[Double] = Type.of[Double]
    def Char: Type[Char] = Type.of[Char]
    def BigDecimal: Type[BigDecimal] = Type.of[BigDecimal]
    def BigInt: Type[BigInt] = Type.of[BigInt]
    def Any: Type[Any] = Type.of[Any]
    def Product: Type[Product] = Type.of[Product]
    def TransientField: Type[hearth.kindlings.xmlderivation.annotations.transientField] =
      Type.of[hearth.kindlings.xmlderivation.annotations.transientField]
    def XmlNameAnnotation: Type[hearth.kindlings.xmlderivation.annotations.xmlName] =
      Type.of[hearth.kindlings.xmlderivation.annotations.xmlName]
    def XmlAttributeAnnotation: Type[hearth.kindlings.xmlderivation.annotations.xmlAttribute] =
      Type.of[hearth.kindlings.xmlderivation.annotations.xmlAttribute]
    def XmlElementAnnotation: Type[hearth.kindlings.xmlderivation.annotations.xmlElement] =
      Type.of[hearth.kindlings.xmlderivation.annotations.xmlElement]
    def XmlContentAnnotation: Type[hearth.kindlings.xmlderivation.annotations.xmlContent] =
      Type.of[hearth.kindlings.xmlderivation.annotations.xmlContent]
    def XmlWrapperAnnotation: Type[hearth.kindlings.xmlderivation.annotations.xmlWrapper] =
      Type.of[hearth.kindlings.xmlderivation.annotations.xmlWrapper]
    def XmlEncoder[A: Type]: Type[hearth.kindlings.xmlderivation.XmlEncoder[A]] =
      Type.of[hearth.kindlings.xmlderivation.XmlEncoder[A]]
    def KindlingsXmlEncoder[A: Type]: Type[hearth.kindlings.xmlderivation.KindlingsXmlEncoder[A]] =
      Type.of[hearth.kindlings.xmlderivation.KindlingsXmlEncoder[A]]
    val EncoderLogDerivation: Type[hearth.kindlings.xmlderivation.KindlingsXmlEncoder.LogDerivation] =
      Type.of[hearth.kindlings.xmlderivation.KindlingsXmlEncoder.LogDerivation]
  }

  private type XmlEncoder[A] = hearth.kindlings.xmlderivation.XmlEncoder[A]
}
