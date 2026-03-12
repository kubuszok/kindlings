package hearth.kindlings.yamlderivation.internal.compiletime

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.fp.syntax.*
import hearth.std.*

import hearth.kindlings.yamlderivation.{KindlingsYamlDecoder, YamlConfig}
import hearth.kindlings.yamlderivation.annotations.{fieldName, transientField}
import hearth.kindlings.yamlderivation.internal.runtime.YamlDerivationUtils
import org.virtuslab.yaml.{ConstructError, Node, YamlDecoder, YamlError}

trait DecoderMacrosImpl
    extends rules.DecoderUseCachedDefWhenAvailableRuleImpl
    with rules.DecoderUseImplicitWhenAvailableRuleImpl
    with rules.DecoderHandleAsLiteralTypeRuleImpl
    with rules.DecoderHandleAsValueTypeRuleImpl
    with rules.DecoderHandleAsOptionRuleImpl
    with rules.DecoderHandleAsMapRuleImpl
    with rules.DecoderHandleAsCollectionRuleImpl
    with rules.DecoderHandleAsNamedTupleRuleImpl
    with rules.DecoderHandleAsSingletonRuleImpl
    with rules.DecoderHandleAsCaseClassRuleImpl
    with rules.DecoderHandleAsEnumRuleImpl { this: MacroCommons & StdExtensions & AnnotationSupport =>

  // Entrypoints

  @scala.annotation.nowarn("msg=is never used")
  def deriveInlineDecode[A: Type](
      nodeExpr: Expr[Node],
      configExpr: Expr[YamlConfig]
  ): Expr[Either[ConstructError, A]] = {
    implicit val EitherT: Type[Either[ConstructError, A]] = DTypes.DecoderResult[A]
    implicit val NodeT: Type[Node] = DTypes.Node
    implicit val ConfigT: Type[YamlConfig] = DTypes.YamlConfig
    implicit val ConstructErrorT: Type[ConstructError] = DTypes.ConstructError

    deriveDecoderFromCtxAndAdaptForEntrypoint[A, Either[ConstructError, A]]("KindlingsYamlDecoder.decode") { fromCtx =>
      ValDefs.createVal[Node](nodeExpr).use { nodeVal =>
        ValDefs.createVal[YamlConfig](configExpr).use { configVal =>
          Expr.quote {
            val _ = Expr.splice(nodeVal)
            val _ = Expr.splice(configVal)
            Expr.splice {
              fromCtx(DecoderCtx.from(nodeVal, configVal, derivedType = None))
            }
          }
        }
      }
    }
  }

  @scala.annotation.nowarn("msg=is never used")
  def deriveInlineFromYamlString[A: Type](
      yamlExpr: Expr[String],
      configExpr: Expr[YamlConfig]
  ): Expr[Either[YamlError, A]] = {
    implicit val EitherCEA: Type[Either[ConstructError, A]] = DTypes.DecoderResult[A]
    implicit val EitherYEA: Type[Either[YamlError, A]] = DTypes.YamlErrorResult[A]
    implicit val NodeT: Type[Node] = DTypes.Node
    implicit val ConfigT: Type[YamlConfig] = DTypes.YamlConfig
    implicit val ConstructErrorT: Type[ConstructError] = DTypes.ConstructError
    implicit val StringT: Type[String] = DTypes.String

    deriveDecoderFromCtxAndAdaptForEntrypoint[A, Either[YamlError, A]]("KindlingsYamlDecoder.fromYamlString") {
      fromCtx =>
        ValDefs.createVal[String](yamlExpr).use { yamlVal =>
          ValDefs.createVal[YamlConfig](configExpr).use { configVal =>
            Expr.quote {
              val y = Expr.splice(yamlVal)
              val cfg = Expr.splice(configVal)
              YamlDerivationUtils.parseAndDecode[A](
                y,
                (node: Node) => {
                  val _ = node
                  val _ = cfg
                  Expr.splice {
                    fromCtx(DecoderCtx.from(Expr.quote(node), Expr.quote(cfg), derivedType = None))
                  }
                }
              )
            }
          }
        }
    }
  }

  @scala.annotation.nowarn("msg=is never used")
  def deriveDecoderTypeClass[A: Type](configExpr: Expr[YamlConfig]): Expr[KindlingsYamlDecoder[A]] = {
    implicit val DecoderA: Type[YamlDecoder[A]] = DTypes.YamlDecoder[A]
    implicit val KindlingsDecoderA: Type[KindlingsYamlDecoder[A]] = DTypes.KindlingsYamlDecoder[A]
    implicit val EitherT: Type[Either[ConstructError, A]] = DTypes.DecoderResult[A]
    implicit val NodeT: Type[Node] = DTypes.Node
    implicit val ConfigT: Type[YamlConfig] = DTypes.YamlConfig
    implicit val ConstructErrorT: Type[ConstructError] = DTypes.ConstructError
    val selfType: Option[??] = Some(Type[A].as_??)

    deriveDecoderFromCtxAndAdaptForEntrypoint[A, KindlingsYamlDecoder[A]]("KindlingsYamlDecoder.derived") { fromCtx =>
      ValDefs.createVal[YamlConfig](configExpr).use { configVal =>
        Expr.quote {
          val cfg = Expr.splice(configVal)
          new KindlingsYamlDecoder[A] {
            def construct(
                node: Node
            )(implicit
                settings: org.virtuslab.yaml.LoadSettings = org.virtuslab.yaml.LoadSettings.empty
            ): Either[ConstructError, A] = {
              val _ = node
              val _ = settings
              Expr.splice {
                fromCtx(DecoderCtx.from(Expr.quote(node), Expr.quote(cfg), derivedType = selfType))
              }
            }
          }
        }
      }
    }
  }

  // Handles logging, error reporting and prepending "cached" defs and vals to the result.

  def deriveDecoderFromCtxAndAdaptForEntrypoint[A: Type, Out: Type](macroName: String)(
      provideCtxAndAdapt: (DecoderCtx[A] => Expr[Either[ConstructError, A]]) => Expr[Out]
  ): Expr[Out] = {
    if (Type[A] =:= Type.of[Nothing].asInstanceOf[Type[A]] || Type[A] =:= Type.of[Any].asInstanceOf[Type[A]])
      Environment.reportErrorAndAbort(
        s"$macroName: type parameter was inferred as ${Type[A].prettyPrint}, which is likely unintended.\n" +
          s"Provide an explicit type parameter, e.g.: $macroName[MyType](...)\n" +
          "or add a type ascription to the result variable."
      )
    Log
      .namedScope(
        s"Deriving decoder for ${Type[A].prettyPrint} at: ${Environment.currentPosition.prettyPrint}"
      ) {
        MIO.scoped { runSafe =>
          val fromCtx: (DecoderCtx[A] => Expr[Either[ConstructError, A]]) =
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
        Log.info(s"Derived final decoder result: ${result.prettyPrint}")
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
          "Enable debug logging with: import hearth.kindlings.yamlderivation.debug.logDerivationForKindlingsYamlDecoder or scalac option -Xmacro-settings:yamlDerivation.logDerivation=true"
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
    implicit val LogDerivation: Type[KindlingsYamlDecoder.LogDerivation] = DTypes.DecoderLogDerivation
    def logDerivationImported = Expr.summonImplicit[KindlingsYamlDecoder.LogDerivation].isDefined

    def logDerivationSetGlobally = (for {
      data <- Environment.typedSettings.toOption
      yamlDerivation <- data.get("yamlDerivation")
      shouldLog <- yamlDerivation.get("logDerivation").flatMap(_.asBoolean)
    } yield shouldLog).getOrElse(false)

    logDerivationImported || logDerivationSetGlobally
  }

  // Context

  final case class DecoderCtx[A](
      tpe: Type[A],
      node: Expr[Node],
      config: Expr[YamlConfig],
      cache: MLocal[ValDefsCache],
      derivedType: Option[??]
  ) {

    def nest[B: Type](newNode: Expr[Node]): DecoderCtx[B] = copy[B](
      tpe = Type[B],
      node = newNode
    )

    def nestInCache(
        newNode: Expr[Node],
        newConfig: Expr[YamlConfig]
    ): DecoderCtx[A] = copy(
      node = newNode,
      config = newConfig
    )

    def getInstance[B: Type]: MIO[Option[Expr[YamlDecoder[B]]]] = {
      implicit val DecoderB: Type[YamlDecoder[B]] = DTypes.YamlDecoder[B]
      cache.get0Ary[YamlDecoder[B]]("cached-decoder-instance")
    }
    def setInstance[B: Type](instance: Expr[YamlDecoder[B]]): MIO[Unit] = {
      implicit val DecoderB: Type[YamlDecoder[B]] = DTypes.YamlDecoder[B]
      Log.info(s"Caching YamlDecoder instance for ${Type[B].prettyPrint}") >>
        cache.buildCachedWith(
          "cached-decoder-instance",
          ValDefBuilder.ofLazy[YamlDecoder[B]](s"decoder_${Type[B].shortName}")
        )(_ => instance)
    }

    def getHelper[B: Type]: MIO[Option[(Expr[Node], Expr[YamlConfig]) => Expr[Either[ConstructError, B]]]] = {
      implicit val ResultB: Type[Either[ConstructError, B]] = DTypes.DecoderResult[B]
      implicit val NodeT: Type[Node] = DTypes.Node
      implicit val ConfigT: Type[YamlConfig] = DTypes.YamlConfig
      cache.get2Ary[Node, YamlConfig, Either[ConstructError, B]]("cached-decode-method")
    }
    def setHelper[B: Type](
        helper: (Expr[Node], Expr[YamlConfig]) => MIO[Expr[Either[ConstructError, B]]]
    ): MIO[Unit] = {
      implicit val ResultB: Type[Either[ConstructError, B]] = DTypes.DecoderResult[B]
      implicit val NodeT: Type[Node] = DTypes.Node
      implicit val ConfigT: Type[YamlConfig] = DTypes.YamlConfig
      val defBuilder =
        ValDefBuilder.ofDef2[Node, YamlConfig, Either[ConstructError, B]](s"decode_${Type[B].shortName}")
      for {
        _ <- Log.info(s"Forward-declaring decode helper for ${Type[B].prettyPrint}")
        _ <- cache.forwardDeclare("cached-decode-method", defBuilder)
        _ <- MIO.scoped { runSafe =>
          runSafe(cache.buildCachedWith("cached-decode-method", defBuilder) { case (_, (node, config)) =>
            runSafe(helper(node, config))
          })
        }
        _ <- Log.info(s"Defined decode helper for ${Type[B].prettyPrint}")
      } yield ()
    }

    override def toString: String =
      s"decode[${tpe.prettyPrint}](node = ${node.prettyPrint}, config = ${config.prettyPrint})"
  }
  object DecoderCtx {

    def from[A: Type](
        node: Expr[Node],
        config: Expr[YamlConfig],
        derivedType: Option[??]
    ): DecoderCtx[A] = DecoderCtx(
      tpe = Type[A],
      node = node,
      config = config,
      cache = ValDefsCache.mlocal,
      derivedType = derivedType
    )
  }

  def dctx[A](implicit A: DecoderCtx[A]): DecoderCtx[A] = A

  implicit def currentDecoderValueType[A: DecoderCtx]: Type[A] = dctx.tpe

  abstract class DecoderDerivationRule(val name: String) extends Rule {
    def apply[A: DecoderCtx]: MIO[Rule.Applicability[Expr[Either[ConstructError, A]]]]
  }

  // The actual derivation logic

  def deriveDecoderRecursively[A: DecoderCtx]: MIO[Expr[Either[ConstructError, A]]] =
    Log
      .namedScope(s"Deriving decoder for type ${Type[A].prettyPrint}") {
        Rules(
          DecoderUseCachedDefWhenAvailableRule,
          DecoderHandleAsLiteralTypeRule,
          DecoderUseImplicitWhenAvailableRule,
          DecoderHandleAsValueTypeRule,
          DecoderHandleAsOptionRule,
          DecoderHandleAsMapRule,
          DecoderHandleAsCollectionRule,
          DecoderHandleAsNamedTupleRule,
          DecoderHandleAsSingletonRule,
          DecoderHandleAsCaseClassRule,
          DecoderHandleAsEnumRule
        )(_[A]).flatMap {
          case Right(result) =>
            Log.info(s"Derived decoder for ${Type[A].prettyPrint}: ${result.prettyPrint}") >>
              MIO.pure(result)
          case Left(reasons) =>
            val reasonsStrings = reasons.toListMap
              .removed(DecoderUseCachedDefWhenAvailableRule)
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

  @scala.annotation.nowarn("msg=is never used|unused explicit parameter")
  def deriveFieldDecoder[Field: Type](implicit ctx: DecoderCtx[?]): MIO[Expr[YamlDecoder[Field]]] = {
    implicit val NodeT: Type[Node] = DTypes.Node
    implicit val EitherCEField: Type[Either[ConstructError, Field]] = DTypes.DecoderResult[Field]

    DTypes.YamlDecoder[Field].summonExprIgnoring(DecoderUseImplicitWhenAvailableRule.ignoredImplicits*).toEither match {
      case Right(decoderExpr) =>
        Log.info(s"Found implicit YamlDecoder[${Type[Field].prettyPrint}]") >> MIO.pure(decoderExpr)
      case Left(_) =>
        Log.info(s"Building YamlDecoder[${Type[Field].prettyPrint}] via recursive derivation") >>
          LambdaBuilder
            .of1[Node]("fieldNode")
            .traverse { fieldNodeExpr =>
              deriveDecoderRecursively[Field](using ctx.nest[Field](fieldNodeExpr))
            }
            .map { builder =>
              val decodeFn = builder.build[Either[ConstructError, Field]]
              Expr.quote(YamlDerivationUtils.decoderFromFn(Expr.splice(decodeFn)))
            }
    }
  }

  // Types

  private[compiletime] object DTypes {

    def YamlDecoder: Type.Ctor1[YamlDecoder] = Type.Ctor1.of[YamlDecoder]
    def KindlingsYamlDecoder: Type.Ctor1[KindlingsYamlDecoder] = Type.Ctor1.of[KindlingsYamlDecoder]
    val DecoderLogDerivation: Type[hearth.kindlings.yamlderivation.KindlingsYamlDecoder.LogDerivation] =
      Type.of[hearth.kindlings.yamlderivation.KindlingsYamlDecoder.LogDerivation]
    val Node: Type[Node] = Type.of[Node]
    val ConstructError: Type[ConstructError] = Type.of[ConstructError]
    val YamlConfig: Type[YamlConfig] = Type.of[YamlConfig]
    val String: Type[String] = Type.of[String]
    val Int: Type[Int] = Type.of[Int]
    val Long: Type[Long] = Type.of[Long]
    val Double: Type[Double] = Type.of[Double]
    val Boolean: Type[Boolean] = Type.of[Boolean]
    val Any: Type[Any] = Type.of[Any]
    val ArrayAny: Type[Array[Any]] = Type.of[Array[Any]]
    val EitherCEAny: Type[Either[ConstructError, Any]] = Type.of[Either[ConstructError, Any]]
    val ListEitherCEAny: Type[List[Either[ConstructError, Any]]] =
      Type.of[List[Either[ConstructError, Any]]]
    val ListString: Type[List[String]] = Type.of[List[String]]
    val StringNodeTuple: Type[(String, Node)] = Type.of[(String, Node)]
    val FieldName: Type[fieldName] = Type.of[fieldName]
    val TransientField: Type[transientField] = Type.of[transientField]

    def DecoderResult[A: Type]: Type[Either[ConstructError, A]] =
      Type.of[Either[ConstructError, A]]

    def YamlErrorResult[A: Type]: Type[Either[YamlError, A]] =
      Type.of[Either[YamlError, A]]
  }
}
