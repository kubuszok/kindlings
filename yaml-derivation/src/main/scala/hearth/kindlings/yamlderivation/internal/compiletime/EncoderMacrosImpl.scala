package hearth.kindlings.yamlderivation.internal.compiletime

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

import hearth.kindlings.yamlderivation.{KindlingsYamlEncoder, YamlConfig}
import hearth.kindlings.yamlderivation.annotations.{fieldName, transientField}
import hearth.kindlings.yamlderivation.internal.runtime.YamlDerivationUtils
import org.virtuslab.yaml.{Node, YamlEncoder}

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

  def deriveInlineEncode[A: Type](valueExpr: Expr[A], configExpr: Expr[YamlConfig]): Expr[Node] = {
    implicit val NodeT: Type[Node] = Types.Node
    implicit val ConfigT: Type[YamlConfig] = Types.YamlConfig

    deriveEncoderFromCtxAndAdaptForEntrypoint[A, Node]("KindlingsYamlEncoder.encode") { fromCtx =>
      ValDefs.createVal[A](valueExpr).use { valueVal =>
        ValDefs.createVal[YamlConfig](configExpr).use { configVal =>
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
  def deriveInlineToYamlString[A: Type](valueExpr: Expr[A], configExpr: Expr[YamlConfig]): Expr[String] = {
    implicit val ConfigT: Type[YamlConfig] = Types.YamlConfig
    implicit val StringT: Type[String] = Types.String

    deriveEncoderFromCtxAndAdaptForEntrypoint[A, String]("KindlingsYamlEncoder.toYamlString") { fromCtx =>
      ValDefs.createVal[A](valueExpr).use { valueVal =>
        ValDefs.createVal[YamlConfig](configExpr).use { configVal =>
          Expr.quote {
            val _ = Expr.splice(valueVal)
            val _ = Expr.splice(configVal)
            val node = Expr.splice(fromCtx(EncoderCtx.from(valueVal, configVal, derivedType = None)))
            YamlDerivationUtils.nodeToYaml(node)
          }
        }
      }
    }
  }

  @scala.annotation.nowarn("msg=is never used")
  def deriveEncoderTypeClass[A: Type](configExpr: Expr[YamlConfig]): Expr[KindlingsYamlEncoder[A]] = {
    implicit val EncoderA: Type[YamlEncoder[A]] = Types.YamlEncoder[A]
    implicit val KindlingsEncoderA: Type[KindlingsYamlEncoder[A]] = Types.KindlingsYamlEncoder[A]
    implicit val NodeT: Type[Node] = Types.Node
    implicit val ConfigT: Type[YamlConfig] = Types.YamlConfig
    val selfType: Option[??] = Some(Type[A].as_??)

    deriveEncoderFromCtxAndAdaptForEntrypoint[A, KindlingsYamlEncoder[A]]("KindlingsYamlEncoder.derived") { fromCtx =>
      ValDefs.createVal[YamlConfig](configExpr).use { configVal =>
        Expr.quote {
          val cfg = Expr.splice(configVal)
          new KindlingsYamlEncoder[A] {
            def asNode(obj: A): Node = {
              val _ = obj
              Expr.splice {
                fromCtx(EncoderCtx.from(Expr.quote(obj), Expr.quote(cfg), derivedType = selfType))
              }
            }
          }
        }
      }
    }
  }

  // Handles logging, error reporting and prepending "cached" defs and vals to the result.

  def deriveEncoderFromCtxAndAdaptForEntrypoint[A: Type, Out: Type](macroName: String)(
      provideCtxAndAdapt: (EncoderCtx[A] => Expr[Node]) => Expr[Out]
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
          val fromCtx: (EncoderCtx[A] => Expr[Node]) = (ctx: EncoderCtx[A]) =>
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
          "Enable debug logging with: import hearth.kindlings.yamlderivation.debug.logDerivationForKindlingsYamlEncoder or scalac option -Xmacro-settings:yamlDerivation.logDerivation=true"
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
    implicit val LogDerivation: Type[KindlingsYamlEncoder.LogDerivation] = Types.EncoderLogDerivation
    def logDerivationImported = Expr.summonImplicit[KindlingsYamlEncoder.LogDerivation].isDefined

    def logDerivationSetGlobally = (for {
      data <- Environment.typedSettings.toOption
      yamlDerivation <- data.get("yamlDerivation")
      shouldLog <- yamlDerivation.get("logDerivation").flatMap(_.asBoolean)
    } yield shouldLog).getOrElse(false)

    logDerivationImported || logDerivationSetGlobally
  }

  // Context

  final case class EncoderCtx[A](
      tpe: Type[A],
      value: Expr[A],
      config: Expr[YamlConfig],
      cache: MLocal[ValDefsCache],
      derivedType: Option[??]
  ) {

    def nest[B: Type](newValue: Expr[B]): EncoderCtx[B] = copy[B](
      tpe = Type[B],
      value = newValue
    )

    def nestInCache(
        newValue: Expr[A],
        newConfig: Expr[YamlConfig]
    ): EncoderCtx[A] = copy(
      value = newValue,
      config = newConfig
    )

    def getInstance[B: Type]: MIO[Option[Expr[YamlEncoder[B]]]] = {
      implicit val EncoderB: Type[YamlEncoder[B]] = Types.YamlEncoder[B]
      cache.get0Ary[YamlEncoder[B]]("cached-encoder-instance")
    }
    def setInstance[B: Type](instance: Expr[YamlEncoder[B]]): MIO[Unit] = {
      implicit val EncoderB: Type[YamlEncoder[B]] = Types.YamlEncoder[B]
      Log.info(s"Caching YamlEncoder instance for ${Type[B].prettyPrint}") >>
        cache.buildCachedWith(
          "cached-encoder-instance",
          ValDefBuilder.ofLazy[YamlEncoder[B]](s"encoder_${Type[B].shortName}")
        )(_ => instance)
    }

    def getHelper[B: Type]: MIO[Option[(Expr[B], Expr[YamlConfig]) => Expr[Node]]] = {
      implicit val NodeT: Type[Node] = Types.Node
      implicit val ConfigT: Type[YamlConfig] = Types.YamlConfig
      cache.get2Ary[B, YamlConfig, Node]("cached-encode-method")
    }
    def setHelper[B: Type](
        helper: (Expr[B], Expr[YamlConfig]) => MIO[Expr[Node]]
    ): MIO[Unit] = {
      implicit val NodeT: Type[Node] = Types.Node
      implicit val ConfigT: Type[YamlConfig] = Types.YamlConfig
      val defBuilder =
        ValDefBuilder.ofDef2[B, YamlConfig, Node](s"encode_${Type[B].shortName}")
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
        config: Expr[YamlConfig],
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
    def apply[A: EncoderCtx]: MIO[Rule.Applicability[Expr[Node]]]
  }

  // The actual derivation logic

  def deriveEncoderRecursively[A: EncoderCtx]: MIO[Expr[Node]] =
    Log
      .namedScope(s"Deriving encoder for type ${Type[A].prettyPrint}") {
        ectx.getHelper[A].flatMap {
          case Some(helperCall) =>
            Log.info(s"Using cached encoder helper for ${Type[A].prettyPrint}") >>
              MIO.pure(helperCall(ectx.value, ectx.config))
          case None =>
            ectx.setHelper[A] { (value, config) =>
              deriveEncoderViaRules[A](using ectx.nestInCache(value, config))
            } >> ectx.getHelper[A].flatMap {
              case Some(helperCall) =>
                MIO.pure(helperCall(ectx.value, ectx.config))
              case None =>
                deriveEncoderViaRules[A]
            }
        }
      }

  private def deriveEncoderViaRules[A: EncoderCtx]: MIO[Expr[Node]] =
    Log
      .namedScope(s"Deriving encoder via rules for type ${Type[A].prettyPrint}") {
        Rules(
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

    def YamlEncoder: Type.Ctor1[YamlEncoder] = Type.Ctor1.of[YamlEncoder]
    def KindlingsYamlEncoder: Type.Ctor1[KindlingsYamlEncoder] = Type.Ctor1.of[KindlingsYamlEncoder]
    val EncoderLogDerivation: Type[hearth.kindlings.yamlderivation.KindlingsYamlEncoder.LogDerivation] =
      Type.of[hearth.kindlings.yamlderivation.KindlingsYamlEncoder.LogDerivation]
    val Node: Type[Node] = Type.of[Node]
    val YamlConfig: Type[YamlConfig] = Type.of[YamlConfig]
    val String: Type[String] = Type.of[String]
    val FieldName: Type[fieldName] = Type.of[fieldName]
    val TransientField: Type[transientField] = Type.of[transientField]
    val Int: Type[Int] = Type.of[Int]
    val Product: Type[Product] = Type.of[Product]
  }
}
