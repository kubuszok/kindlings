package hearth.kindlings.yamlderivation.internal.compiletime

import hearth.MacroCommons
import hearth.fp.data.NonEmptyList
import hearth.fp.effect.*
import hearth.fp.syntax.*
import hearth.std.*

import hearth.kindlings.yamlderivation.{KindlingsYamlDecoder, YamlConfig}
import hearth.kindlings.yamlderivation.annotations.{fieldName, transientField}
import hearth.kindlings.yamlderivation.internal.runtime.YamlDerivationUtils
import org.virtuslab.yaml.{ConstructError, Node, YamlDecoder, YamlError}

trait DecoderMacrosImpl { this: MacroCommons & StdExtensions & AnnotationSupport =>

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
  ): Expr[Out] = Log
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
        _ <- cache.forwardDeclare("cached-decode-method", defBuilder)
        _ <- MIO.scoped { runSafe =>
          runSafe(cache.buildCachedWith("cached-decode-method", defBuilder) { case (_, (node, config)) =>
            runSafe(helper(node, config))
          })
        }
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
          DecUseCachedDefWhenAvailableRule,
          DecUseImplicitWhenAvailableRule,
          DecHandleAsValueTypeRule,
          DecHandleAsOptionRule,
          DecHandleAsMapRule,
          DecHandleAsCollectionRule,
          DecHandleAsCaseClassRule,
          DecHandleAsEnumRule
        )(_[A]).flatMap {
          case Right(result) =>
            Log.info(s"Derived decoder for ${Type[A].prettyPrint}: ${result.prettyPrint}") >>
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

    def apply[A: DecoderCtx]: MIO[Rule.Applicability[Expr[Either[ConstructError, A]]]] =
      Log.info(s"Attempting to use cached decoder for ${Type[A].prettyPrint}") >>
        dctx.getInstance[A].flatMap {
          case Some(instance) => callCachedInstance[A](instance)
          case None           =>
            dctx.getHelper[A].flatMap {
              case Some(helperCall) => callCachedHelper[A](helperCall)
              case None             => yieldUnsupported[A]
            }
        }

    private def callCachedInstance[A: DecoderCtx](
        instance: Expr[YamlDecoder[A]]
    ): MIO[Rule.Applicability[Expr[Either[ConstructError, A]]]] =
      Log.info(s"Found cached decoder instance for ${Type[A].prettyPrint}") >> MIO.pure(
        Rule.matched(Expr.quote {
          Expr.splice(instance).construct(Expr.splice(dctx.node))(org.virtuslab.yaml.LoadSettings.empty)
        })
      )

    private def callCachedHelper[A: DecoderCtx](
        helperCall: (Expr[Node], Expr[YamlConfig]) => Expr[Either[ConstructError, A]]
    ): MIO[Rule.Applicability[Expr[Either[ConstructError, A]]]] =
      Log.info(s"Found cached decoder helper for ${Type[A].prettyPrint}") >> MIO.pure(
        Rule.matched(helperCall(dctx.node, dctx.config))
      )

    private def yieldUnsupported[A: DecoderCtx]: MIO[Rule.Applicability[Expr[Either[ConstructError, A]]]] =
      MIO.pure(Rule.yielded(s"The type ${Type[A].prettyPrint} does not have a cached decoder"))
  }

  object DecUseImplicitWhenAvailableRule extends DecoderDerivationRule("use implicit when available") {

    lazy val ignoredImplicits: Seq[UntypedMethod] = {
      val ours = Type.of[KindlingsYamlDecoder.type].methods.collect {
        case method if method.value.name == "derived" => method.value.asUntyped
      }
      ours
    }

    def apply[A: DecoderCtx]: MIO[Rule.Applicability[Expr[Either[ConstructError, A]]]] =
      Log.info(s"Attempting to use implicit YamlDecoder for ${Type[A].prettyPrint}") >> {
        if (dctx.derivedType.exists(_.Underlying =:= Type[A]))
          MIO.pure(
            Rule.yielded(s"The type ${Type[A].prettyPrint} is the type being derived, skipping implicit search")
          )
        else
          DTypes.YamlDecoder[A].summonExprIgnoring(ignoredImplicits*).toEither match {
            case Right(instanceExpr) => cacheAndUse[A](instanceExpr)
            case Left(reason)        => yieldUnsupported[A](reason)
          }
      }

    private def cacheAndUse[A: DecoderCtx](
        instanceExpr: Expr[YamlDecoder[A]]
    ): MIO[Rule.Applicability[Expr[Either[ConstructError, A]]]] =
      Log.info(s"Found implicit decoder ${instanceExpr.prettyPrint}, using directly") >>
        MIO.pure(Rule.matched(Expr.quote {
          Expr.splice(instanceExpr).construct(Expr.splice(dctx.node))(org.virtuslab.yaml.LoadSettings.empty)
        }))

    private def yieldUnsupported[A: DecoderCtx](
        reason: String
    ): MIO[Rule.Applicability[Expr[Either[ConstructError, A]]]] =
      MIO.pure(
        Rule.yielded(
          s"The type ${Type[A].prettyPrint} does not have an implicit YamlDecoder instance: $reason"
        )
      )
  }

  object DecHandleAsValueTypeRule extends DecoderDerivationRule("handle as value type when possible") {

    def apply[A: DecoderCtx]: MIO[Rule.Applicability[Expr[Either[ConstructError, A]]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as a value type") >> {
        Type[A] match {
          case IsValueType(isValueType) =>
            import isValueType.Underlying as Inner

            DTypes
              .YamlDecoder[Inner]
              .summonExprIgnoring(DecUseImplicitWhenAvailableRule.ignoredImplicits*)
              .toEither match {
              case Right(innerDecoder) =>
                LambdaBuilder
                  .of1[Inner]("inner")
                  .traverse { innerExpr =>
                    MIO.pure(isValueType.value.wrap.apply(innerExpr).asInstanceOf[Expr[A]])
                  }
                  .map { builder =>
                    val wrapLambda = builder.build[A]
                    Rule.matched(Expr.quote {
                      Expr
                        .splice(innerDecoder)
                        .construct(Expr.splice(dctx.node))(org.virtuslab.yaml.LoadSettings.empty)
                        .map(Expr.splice(wrapLambda))
                    })
                  }
              case Left(reason) =>
                MIO.pure(Rule.yielded(s"Value type inner ${Type[Inner].prettyPrint} has no YamlDecoder: $reason"))
            }

          case _ =>
            MIO.pure(Rule.yielded(s"The type ${Type[A].prettyPrint} is not a value type"))
        }
      }
  }

  object DecHandleAsOptionRule extends DecoderDerivationRule("handle as Option when possible") {

    def apply[A: DecoderCtx]: MIO[Rule.Applicability[Expr[Either[ConstructError, A]]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as Option") >> {
        Type[A] match {
          case IsOption(isOption) =>
            import isOption.Underlying as Inner
            implicit val NodeT: Type[Node] = DTypes.Node
            implicit val EitherCEInner: Type[Either[ConstructError, Inner]] = DTypes.DecoderResult[Inner]

            LambdaBuilder
              .of1[Node]("innerNode")
              .traverse { innerNodeExpr =>
                deriveDecoderRecursively[Inner](using dctx.nest[Inner](innerNodeExpr))
              }
              .map { builder =>
                val decodeFn = builder.build[Either[ConstructError, Inner]]
                Rule.matched(Expr.quote {
                  YamlDerivationUtils
                    .decodeOptionFromFn(
                      Expr.splice(dctx.node),
                      Expr.splice(decodeFn)
                    )
                    .asInstanceOf[Either[ConstructError, A]]
                })
              }

          case _ =>
            MIO.pure(Rule.yielded(s"The type ${Type[A].prettyPrint} is not an Option"))
        }
      }
  }

  @scala.annotation.nowarn("msg=Infinite loop")
  object DecHandleAsMapRule extends DecoderDerivationRule("handle as map when possible") {

    def apply[A: DecoderCtx]: MIO[Rule.Applicability[Expr[Either[ConstructError, A]]]] =
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
    ): MIO[Rule.Applicability[Expr[Either[ConstructError, A]]]] = {
      import isMap.{Key, Value, CtorResult}
      implicit val StringT: Type[String] = DTypes.String
      implicit val NodeT: Type[Node] = DTypes.Node
      implicit val EitherCEValue: Type[Either[ConstructError, Value]] = DTypes.DecoderResult[Value]

      if (!(Key <:< Type[String]))
        MIO.pure(Rule.yielded(s"Map key type ${Key.prettyPrint} is not String"))
      else {
        LambdaBuilder
          .of1[Node]("valueNode")
          .traverse { valueNodeExpr =>
            deriveDecoderRecursively[Value](using dctx.nest[Value](valueNodeExpr))
          }
          .map { builder =>
            val decodeFn = builder.build[Either[ConstructError, Value]]
            val factoryExpr = isMap.factory
            Rule.matched(Expr.quote {
              YamlDerivationUtils
                .decodeMapWith(
                  Expr.splice(dctx.node),
                  YamlDerivationUtils.decoderFromFn(Expr.splice(decodeFn)),
                  Expr
                    .splice(factoryExpr)
                    .asInstanceOf[scala.collection.Factory[(String, Value), A]]
                )
                .asInstanceOf[Either[ConstructError, A]]
            })
          }
      }
    }
  }

  object DecHandleAsCollectionRule extends DecoderDerivationRule("handle as collection when possible") {

    def apply[A: DecoderCtx]: MIO[Rule.Applicability[Expr[Either[ConstructError, A]]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as a collection") >> {
        Type[A] match {
          case IsCollection(isCollection) =>
            import isCollection.Underlying as Item
            import isCollection.value.CtorResult
            implicit val NodeT: Type[Node] = DTypes.Node
            implicit val EitherCEItem: Type[Either[ConstructError, Item]] = DTypes.DecoderResult[Item]

            LambdaBuilder
              .of1[Node]("itemNode")
              .traverse { itemNodeExpr =>
                deriveDecoderRecursively[Item](using dctx.nest[Item](itemNodeExpr))
              }
              .map { builder =>
                val decodeFn = builder.build[Either[ConstructError, Item]]
                val factoryExpr = isCollection.value.factory
                Rule.matched(Expr.quote {
                  YamlDerivationUtils
                    .decodeCollectionWith(
                      Expr.splice(dctx.node),
                      YamlDerivationUtils.decoderFromFn(Expr.splice(decodeFn)),
                      Expr.splice(factoryExpr).asInstanceOf[scala.collection.Factory[Item, A]]
                    )
                    .asInstanceOf[Either[ConstructError, A]]
                })
              }

          case _ =>
            MIO.pure(Rule.yielded(s"The type ${Type[A].prettyPrint} is not a collection"))
        }
      }
  }

  object DecHandleAsCaseClassRule extends DecoderDerivationRule("handle as case class when possible") {

    def apply[A: DecoderCtx]: MIO[Rule.Applicability[Expr[Either[ConstructError, A]]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as a case class") >> {
        CaseClass.parse[A] match {
          case Some(caseClass) =>
            for {
              _ <- dctx.setHelper[A] { (node, config) =>
                decodeCaseClassFields[A](caseClass)(using dctx.nestInCache(node, config))
              }
              result <- dctx.getHelper[A].flatMap {
                case Some(helperCall) =>
                  MIO.pure(Rule.matched(helperCall(dctx.node, dctx.config)))
                case None =>
                  MIO.pure(Rule.yielded(s"Failed to build helper for ${Type[A].prettyPrint}"))
              }
            } yield result

          case None =>
            MIO.pure(Rule.yielded(s"The type ${Type[A].prettyPrint} is not a case class"))
        }
      }

    @scala.annotation.nowarn("msg=is never used|unused explicit parameter")
    private def decodeCaseClassFields[A: DecoderCtx](
        caseClass: CaseClass[A]
    ): MIO[Expr[Either[ConstructError, A]]] = {
      implicit val StringT: Type[String] = DTypes.String
      implicit val NodeT: Type[Node] = DTypes.Node
      implicit val ConstructErrorT: Type[ConstructError] = DTypes.ConstructError
      implicit val fieldNameT: Type[fieldName] = DTypes.FieldName
      implicit val transientFieldT: Type[transientField] = DTypes.TransientField

      // Singletons (case objects, parameterless enum cases) have no primary constructor
      if (caseClass.isSingleton) {
        return caseClass
          .construct[MIO](new CaseClass.ConstructField[MIO] {
            def apply(field: Parameter): MIO[Expr[field.tpe.Underlying]] = {
              val err = DecoderDerivationError.CannotConstructType(
                Type[A].prettyPrint,
                isSingleton = true,
                Some(s"Unexpected parameter in singleton")
              )
              Log.error(err.message) >> MIO.fail(err)
            }
          })
          .flatMap {
            case Some(expr) =>
              MIO.pure(Expr.quote(Right(Expr.splice(expr)): Either[ConstructError, A]))
            case None =>
              val err = DecoderDerivationError.CannotConstructType(Type[A].prettyPrint, isSingleton = true)
              Log.error(err.message) >> MIO.fail(err)
          }
      }

      val constructor = caseClass.primaryConstructor
      val fieldsList = constructor.parameters.flatten.toList

      // Validate: @transientField on fields without defaults is a compile error
      fieldsList.collectFirst {
        case (name, param) if hasAnnotationType[transientField](param) && !param.hasDefault => name
      } match {
        case Some(name) =>
          val err = DecoderDerivationError.TransientFieldMissingDefault(name, Type[A].prettyPrint)
          return Log.error(err.message) >> MIO.fail(err)
        case None => // OK
      }

      // Separate transient and non-transient fields
      val transientFields = fieldsList.filter { case (_, param) => hasAnnotationType[transientField](param) }
      val nonTransientFields = fieldsList.filter { case (_, param) => !hasAnnotationType[transientField](param) }

      // Build transient defaults map
      val transientDefaults: Map[String, Expr_??] = transientFields.flatMap { case (fName, param) =>
        param.defaultValue.flatMap { existentialOuter =>
          val methodOf = existentialOuter.value
          methodOf.value match {
            case noInstance: Method.NoInstance[?] =>
              import noInstance.Returned
              noInstance(Map.empty).toOption.map { defaultExpr =>
                (fName, defaultExpr.as_??)
              }
            case _ => None
          }
        }
      }.toMap

      NonEmptyList.fromList(nonTransientFields) match {
        case None =>
          // All fields are transient or there are no fields — construct with defaults
          // Validate that input is a mapping node
          caseClass
            .construct[MIO](new CaseClass.ConstructField[MIO] {
              def apply(field: Parameter): MIO[Expr[field.tpe.Underlying]] =
                transientDefaults.get(field.name) match {
                  case Some(defaultExpr) =>
                    MIO.pure(defaultExpr.value.asInstanceOf[Expr[field.tpe.Underlying]])
                  case None =>
                    val err = DecoderDerivationError.CannotConstructType(
                      Type[A].prettyPrint,
                      isSingleton = false,
                      Some(s"Unexpected parameter in zero-argument case class")
                    )
                    Log.error(err.message) >> MIO.fail(err)
                }
            })
            .flatMap {
              case Some(expr) =>
                MIO.pure(Expr.quote {
                  YamlDerivationUtils.checkIsMapping(Expr.splice(dctx.node)).map(_ => Expr.splice(expr))
                })
              case None =>
                val err = DecoderDerivationError.CannotConstructType(Type[A].prettyPrint, isSingleton = false)
                Log.error(err.message) >> MIO.fail(err)
            }

        case Some(fields) =>
          implicit val AnyT: Type[Any] = DTypes.Any
          implicit val EitherCEAnyT: Type[Either[ConstructError, Any]] = DTypes.EitherCEAny
          implicit val ArrayAnyT: Type[Array[Any]] = DTypes.ArrayAny
          implicit val ListEitherT: Type[List[Either[ConstructError, Any]]] = DTypes.ListEitherCEAny

          val indexedFields = fields.toList.zipWithIndex
          NonEmptyList
            .fromList(indexedFields)
            .get
            .parTraverse { case ((fName, param), reindex) =>
              import param.tpe.Underlying as Field
              val nameOverride = getAnnotationStringArg[fieldName](param)
              Log.namedScope(s"Deriving decoder for field $fName: ${Type[Field].prettyPrint}") {
                deriveFieldDecoder[Field].map { decoderExpr =>
                  val decodeExpr: Expr[Either[ConstructError, Any]] = nameOverride match {
                    case Some(customName) =>
                      Expr.quote {
                        YamlDerivationUtils
                          .getField(
                            Expr.splice(dctx.node),
                            Expr.splice(Expr(customName))
                          )
                          .flatMap(fieldNode =>
                            Expr.splice(decoderExpr).construct(fieldNode)(org.virtuslab.yaml.LoadSettings.empty)
                          )
                          .asInstanceOf[Either[ConstructError, Any]]
                      }
                    case None =>
                      Expr.quote {
                        YamlDerivationUtils
                          .getField(
                            Expr.splice(dctx.node),
                            Expr.splice(dctx.config).transformMemberNames(Expr.splice(Expr(fName)))
                          )
                          .flatMap(fieldNode =>
                            Expr.splice(decoderExpr).construct(fieldNode)(org.virtuslab.yaml.LoadSettings.empty)
                          )
                          .asInstanceOf[Either[ConstructError, Any]]
                      }
                  }
                  val makeAccessor: Expr[Array[Any]] => (String, Expr_??) = { arrExpr =>
                    val typedExpr = Expr.quote {
                      YamlDerivationUtils.unsafeCast(
                        Expr.splice(arrExpr)(Expr.splice(Expr(reindex))),
                        Expr.splice(decoderExpr)
                      )
                    }
                    (fName, typedExpr.as_??)
                  }
                  (decodeExpr, makeAccessor)
                }
              }
            }
            .flatMap { fieldData =>
              val decodeExprs = fieldData.toList.map(_._1)
              val makeAccessors = fieldData.toList.map(_._2)

              val listExpr: Expr[List[Either[ConstructError, Any]]] =
                decodeExprs.foldRight(Expr.quote(List.empty[Either[ConstructError, Any]])) { (elem, acc) =>
                  Expr.quote(Expr.splice(elem) :: Expr.splice(acc))
                }

              LambdaBuilder
                .of1[Array[Any]]("decodedValues")
                .traverse { decodedValuesExpr =>
                  val nonTransientFieldMap: Map[String, Expr_??] =
                    makeAccessors.map(_(decodedValuesExpr)).toMap
                  val fieldMap: Map[String, Expr_??] = nonTransientFieldMap ++ transientDefaults
                  caseClass.primaryConstructor(fieldMap) match {
                    case Right(constructExpr) => MIO.pure(constructExpr)
                    case Left(error)          =>
                      val err = DecoderDerivationError.CannotConstructType(
                        Type[A].prettyPrint,
                        isSingleton = false,
                        Some(error)
                      )
                      Log.error(err.message) >> MIO.fail(err)
                  }
                }
                .map { builder =>
                  val constructLambda = builder.build[A]
                  Expr.quote {
                    YamlDerivationUtils
                      .sequenceDecodeResults(Expr.splice(listExpr))
                      .map(Expr.splice(constructLambda))
                  }
                }
            }
      }
    }

    @scala.annotation.nowarn("msg=is never used|unused explicit parameter")
    private def deriveFieldDecoder[Field: Type](implicit ctx: DecoderCtx[?]): MIO[Expr[YamlDecoder[Field]]] = {
      implicit val NodeT: Type[Node] = DTypes.Node
      implicit val EitherCEField: Type[Either[ConstructError, Field]] = DTypes.DecoderResult[Field]

      DTypes.YamlDecoder[Field].summonExprIgnoring(DecUseImplicitWhenAvailableRule.ignoredImplicits*).toEither match {
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

  }

  object DecHandleAsEnumRule extends DecoderDerivationRule("handle as enum when possible") {

    def apply[A: DecoderCtx]: MIO[Rule.Applicability[Expr[Either[ConstructError, A]]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as an enum") >> {
        Enum.parse[A] match {
          case Some(enumm) =>
            for {
              _ <- dctx.setHelper[A] { (node, config) =>
                decodeEnumCases[A](enumm)(using dctx.nestInCache(node, config))
              }
              result <- dctx.getHelper[A].flatMap {
                case Some(helperCall) =>
                  MIO.pure(Rule.matched(helperCall(dctx.node, dctx.config)))
                case None =>
                  MIO.pure(Rule.yielded(s"Failed to build helper for ${Type[A].prettyPrint}"))
              }
            } yield result
          case None =>
            MIO.pure(Rule.yielded(s"The type ${Type[A].prettyPrint} is not an enum"))
        }
      }

    @scala.annotation.nowarn("msg=is never used|unused explicit parameter")
    private def decodeEnumCases[A: DecoderCtx](
        enumm: Enum[A]
    ): MIO[Expr[Either[ConstructError, A]]] = {
      implicit val NodeT: Type[Node] = DTypes.Node
      implicit val ConstructErrorT: Type[ConstructError] = DTypes.ConstructError
      implicit val StringT: Type[String] = DTypes.String
      implicit val ListStringT: Type[List[String]] = DTypes.ListString
      implicit val TupleT: Type[(String, Node)] = DTypes.StringNodeTuple
      implicit val EitherCEA: Type[Either[ConstructError, A]] = DTypes.DecoderResult[A]

      val childrenList = enumm.directChildren.toList

      // Check at compile time if all children are singletons (case objects with no fields)
      val allCaseObjects = childrenList.forall { case (_, child) =>
        Type.isVal(using child.Underlying) ||
        CaseClass.parse(using child.Underlying).exists(_.primaryConstructor.parameters.flatten.isEmpty)
      }

      NonEmptyList.fromList(childrenList) match {
        case None =>
          MIO.pure(Expr.quote {
            Left(
              ConstructError.from(
                s"Enum ${Expr.splice(Expr(Type[A].prettyPrint))} has no subtypes"
              )
            ): Either[ConstructError, A]
          })

        case Some(children) =>
          val knownNames: List[String] = children.toList.map(_._1)

          children
            .parTraverse { case (childName, child) =>
              import child.Underlying as ChildType
              Log.namedScope(s"Deriving decoder for enum case $childName: ${Type[ChildType].prettyPrint}") {
                deriveChildDecoder[A, ChildType](childName)
              }
            }
            .flatMap { childDispatchers =>
              LambdaBuilder
                .of1[(String, Node)]("readResult")
                .traverse { readResultExpr =>
                  val typeNameExpr: Expr[String] = Expr.quote(Expr.splice(readResultExpr)._1)
                  val innerNodeExpr: Expr[Node] = Expr.quote(Expr.splice(readResultExpr)._2)

                  val errorExpr: Expr[Either[ConstructError, A]] = Expr.quote {
                    Left(
                      YamlDerivationUtils.failedToMatchSubtype(
                        Expr.splice(typeNameExpr),
                        Expr.splice(innerNodeExpr),
                        Expr.splice(Expr(knownNames))
                      )
                    ): Either[ConstructError, A]
                  }

                  MIO.pure(childDispatchers.toList.foldRight(errorExpr) { case (dispatcher, elseExpr) =>
                    dispatcher(typeNameExpr, innerNodeExpr, elseExpr)
                  })
                }
                .map { builder =>
                  val dispatchFn = builder.build[Either[ConstructError, A]]
                  Expr.quote {
                    val config = Expr.splice(dctx.config)
                    val node = Expr.splice(dctx.node)
                    if (Expr.splice(Expr(allCaseObjects)) && config.enumAsStrings) {
                      // String enum decode path: read scalar string, dispatch on name
                      YamlDerivationUtils.decodeEnumFromString[A](node, Expr.splice(Expr(knownNames))) { typeName =>
                        Expr.splice(dispatchFn)((typeName, node))
                      }
                    } else {
                      val readResult: Either[ConstructError, (String, Node)] =
                        config.discriminator match {
                          case Some(field) => YamlDerivationUtils.decodeDiscriminator(node, field)
                          case None        => YamlDerivationUtils.decodeWrapped(node)
                        }
                      readResult.flatMap(Expr.splice(dispatchFn))
                    }
                  }
                }
            }
      }
    }

    @scala.annotation.nowarn("msg=is never used|unused explicit parameter")
    private def deriveChildDecoder[A: DecoderCtx, ChildType: Type](
        childName: String
    ): MIO[(Expr[String], Expr[Node], Expr[Either[ConstructError, A]]) => Expr[Either[ConstructError, A]]] = {
      implicit val NodeT: Type[Node] = DTypes.Node
      implicit val ConstructErrorT: Type[ConstructError] = DTypes.ConstructError
      implicit val StringT: Type[String] = DTypes.String

      DTypes
        .YamlDecoder[ChildType]
        .summonExprIgnoring(DecUseImplicitWhenAvailableRule.ignoredImplicits*)
        .toEither match {
        case Right(decoderExpr) =>
          Log.info(s"Found implicit YamlDecoder[$childName], using it") >>
            MIO.pure {
              (
                  typeNameExpr: Expr[String],
                  innerNodeExpr: Expr[Node],
                  elseExpr: Expr[Either[ConstructError, A]]
              ) =>
                Expr.quote {
                  if (
                    Expr.splice(dctx.config).transformConstructorNames(Expr.splice(Expr(childName))) == Expr
                      .splice(typeNameExpr)
                  )
                    Expr
                      .splice(decoderExpr)
                      .construct(Expr.splice(innerNodeExpr))(org.virtuslab.yaml.LoadSettings.empty)
                      .asInstanceOf[Either[ConstructError, A]]
                  else
                    Expr.splice(elseExpr)
                }
            }

        case Left(_) =>
          deriveDecoderRecursively[ChildType](using dctx.nest[ChildType](dctx.node)).flatMap { _ =>
            dctx.getHelper[ChildType].map {
              case Some(helper) =>
                (
                    typeNameExpr: Expr[String],
                    innerNodeExpr: Expr[Node],
                    elseExpr: Expr[Either[ConstructError, A]]
                ) => {
                  val helperCallExpr = helper(innerNodeExpr, dctx.config)
                  Expr.quote {
                    if (
                      Expr.splice(dctx.config).transformConstructorNames(Expr.splice(Expr(childName))) == Expr
                        .splice(typeNameExpr)
                    )
                      Expr.splice(helperCallExpr).asInstanceOf[Either[ConstructError, A]]
                    else
                      Expr.splice(elseExpr)
                  }
                }

              case None =>
                (
                    _: Expr[String],
                    _: Expr[Node],
                    elseExpr: Expr[Either[ConstructError, A]]
                ) => elseExpr
            }
          }
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

sealed private[compiletime] trait DecoderDerivationError
    extends util.control.NoStackTrace
    with Product
    with Serializable {
  def message: String
  override def getMessage(): String = message
}
private[compiletime] object DecoderDerivationError {
  final case class UnsupportedType(tpeName: String, reasons: List[String]) extends DecoderDerivationError {
    override def message: String =
      s"The type $tpeName was not handled by any decoder derivation rule:\n${reasons.mkString("\n")}"
  }
  final case class TransientFieldMissingDefault(fieldName: String, tpeName: String) extends DecoderDerivationError {
    override def message: String =
      s"@transientField on field '$fieldName' of $tpeName requires a default value"
  }
  final case class CannotConstructType(tpeName: String, isSingleton: Boolean, constructorError: Option[String] = None)
      extends DecoderDerivationError {
    override def message: String = {
      val prefix =
        if (isSingleton) s"Cannot construct singleton $tpeName" else s"Cannot construct $tpeName"
      constructorError.fold(prefix)(err => s"$prefix: $err")
    }
  }
}
