package hearth.kindlings.yamlderivation.internal.compiletime

import hearth.MacroCommons
import hearth.fp.data.NonEmptyList
import hearth.fp.effect.*
import hearth.fp.syntax.*
import hearth.std.*

import hearth.kindlings.yamlderivation.{KindlingsYamlEncoder, YamlConfig}
import hearth.kindlings.yamlderivation.annotations.{fieldName, transientField}
import hearth.kindlings.yamlderivation.internal.runtime.YamlDerivationUtils
import org.virtuslab.yaml.{Node, YamlEncoder}

trait EncoderMacrosImpl { this: MacroCommons & StdExtensions & AnnotationSupport =>

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
  ): Expr[Out] = Log
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
        _ <- cache.forwardDeclare("cached-encode-method", defBuilder)
        _ <- MIO.scoped { runSafe =>
          runSafe(cache.buildCachedWith("cached-encode-method", defBuilder) { case (_, (value, config)) =>
            runSafe(helper(value, config))
          })
        }
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
        Rules(
          EncUseCachedDefWhenAvailableRule,
          EncUseImplicitWhenAvailableRule,
          EncHandleAsValueTypeRule,
          EncHandleAsOptionRule,
          EncHandleAsMapRule,
          EncHandleAsCollectionRule,
          EncHandleAsCaseClassRule,
          EncHandleAsEnumRule
        )(_[A]).flatMap {
          case Right(result) =>
            Log.info(s"Derived encoder for ${Type[A].prettyPrint}: ${result.prettyPrint}") >>
              MIO.pure(result)
          case Left(reasons) =>
            val reasonsStrings = reasons.toListMap
              .removed(EncUseCachedDefWhenAvailableRule)
              .view
              .map { case (rule, reasons) =>
                if (reasons.isEmpty) s"The rule ${rule.name} was not applicable"
                else
                  s" - The rule ${rule.name} was not applicable, for the following reasons: ${reasons.mkString(", ")}"
              }
              .toList
            val err = EncoderDerivationError.UnsupportedType(Type[A].prettyPrint, reasonsStrings)
            Log.error(err.message) >> MIO.fail(err)
        }
      }

  // Rules

  object EncUseCachedDefWhenAvailableRule extends EncoderDerivationRule("use cached def when available") {

    def apply[A: EncoderCtx]: MIO[Rule.Applicability[Expr[Node]]] =
      Log.info(s"Attempting to use cached encoder for ${Type[A].prettyPrint}") >>
        ectx.getInstance[A].flatMap {
          case Some(instance) => callCachedInstance[A](instance)
          case None           =>
            ectx.getHelper[A].flatMap {
              case Some(helperCall) => callCachedHelper[A](helperCall)
              case None             => yieldUnsupported[A]
            }
        }

    private def callCachedInstance[A: EncoderCtx](
        instance: Expr[YamlEncoder[A]]
    ): MIO[Rule.Applicability[Expr[Node]]] =
      Log.info(s"Found cached encoder instance for ${Type[A].prettyPrint}") >> MIO.pure(Rule.matched(Expr.quote {
        Expr.splice(instance).asNode(Expr.splice(ectx.value))
      }))

    private def callCachedHelper[A: EncoderCtx](
        helperCall: (Expr[A], Expr[YamlConfig]) => Expr[Node]
    ): MIO[Rule.Applicability[Expr[Node]]] =
      Log.info(s"Found cached encoder helper for ${Type[A].prettyPrint}") >> MIO.pure(
        Rule.matched(helperCall(ectx.value, ectx.config))
      )

    private def yieldUnsupported[A: EncoderCtx]: MIO[Rule.Applicability[Expr[Node]]] =
      MIO.pure(Rule.yielded(s"The type ${Type[A].prettyPrint} does not have a cached encoder"))
  }

  object EncUseImplicitWhenAvailableRule extends EncoderDerivationRule("use implicit when available") {

    lazy val ignoredImplicits: Seq[UntypedMethod] = {
      val ours = Type.of[KindlingsYamlEncoder.type].methods.collect {
        case method if method.value.name == "derived" => method.value.asUntyped
      }
      ours
    }

    def apply[A: EncoderCtx]: MIO[Rule.Applicability[Expr[Node]]] =
      Log.info(s"Attempting to use implicit YamlEncoder for ${Type[A].prettyPrint}") >> {
        if (ectx.derivedType.exists(_.Underlying =:= Type[A]))
          MIO.pure(
            Rule.yielded(s"The type ${Type[A].prettyPrint} is the type being derived, skipping implicit search")
          )
        else
          Types.YamlEncoder[A].summonExprIgnoring(ignoredImplicits*).toEither match {
            case Right(instanceExpr) => cacheAndUse[A](instanceExpr)
            case Left(reason)        => yieldUnsupported[A](reason)
          }
      }

    private def cacheAndUse[A: EncoderCtx](
        instanceExpr: Expr[YamlEncoder[A]]
    ): MIO[Rule.Applicability[Expr[Node]]] =
      Log.info(s"Found implicit encoder ${instanceExpr.prettyPrint}, using directly") >>
        MIO.pure(Rule.matched(Expr.quote {
          Expr.splice(instanceExpr).asNode(Expr.splice(ectx.value))
        }))

    private def yieldUnsupported[A: EncoderCtx](reason: String): MIO[Rule.Applicability[Expr[Node]]] =
      MIO.pure(
        Rule.yielded(
          s"The type ${Type[A].prettyPrint} does not have an implicit YamlEncoder instance: $reason"
        )
      )
  }

  object EncHandleAsValueTypeRule extends EncoderDerivationRule("handle as value type when possible") {

    def apply[A: EncoderCtx]: MIO[Rule.Applicability[Expr[Node]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as a value type") >> {
        Type[A] match {
          case IsValueType(isValueType) =>
            import isValueType.Underlying as Inner
            val unwrappedExpr = isValueType.value.unwrap(ectx.value)
            for {
              innerResult <- deriveEncoderRecursively[Inner](using ectx.nest(unwrappedExpr))
            } yield Rule.matched(innerResult)

          case _ =>
            MIO.pure(Rule.yielded(s"The type ${Type[A].prettyPrint} is not a value type"))
        }
      }
  }

  object EncHandleAsOptionRule extends EncoderDerivationRule("handle as Option when possible") {
    implicit val NodeT: Type[Node] = Types.Node

    def apply[A: EncoderCtx]: MIO[Rule.Applicability[Expr[Node]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as Option") >> {
        Type[A] match {
          case IsOption(isOption) =>
            import isOption.Underlying as Inner
            LambdaBuilder
              .of1[Inner]("inner")
              .traverse { innerExpr =>
                deriveEncoderRecursively[Inner](using ectx.nest(innerExpr))
              }
              .map { builder =>
                val lambda = builder.build[Node]
                Rule.matched(
                  isOption.value.fold[Node](ectx.value)(
                    onEmpty = Expr.quote(YamlDerivationUtils.nodeNull),
                    onSome = innerExpr =>
                      Expr.quote {
                        Expr.splice(lambda).apply(Expr.splice(innerExpr))
                      }
                  )
                )
              }

          case _ =>
            MIO.pure(Rule.yielded(s"The type ${Type[A].prettyPrint} is not an Option"))
        }
      }
  }

  @scala.annotation.nowarn("msg=Infinite loop")
  object EncHandleAsMapRule extends EncoderDerivationRule("handle as map when possible") {
    implicit val NodeT: Type[Node] = Types.Node
    implicit val StringT: Type[String] = Types.String
    implicit val StringNodePairT: Type[(String, Node)] = Type.of[(String, Node)]

    def apply[A: EncoderCtx]: MIO[Rule.Applicability[Expr[Node]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as a map") >> {
        Type[A] match {
          case IsMap(isMap) =>
            import isMap.Underlying as Pair
            deriveMapEntries[A, Pair](isMap.value)

          case _ =>
            MIO.pure(Rule.yielded(s"The type ${Type[A].prettyPrint} is not a map"))
        }
      }

    private def deriveMapEntries[A: EncoderCtx, Pair: Type](
        isMap: IsMapOf[A, Pair]
    ): MIO[Rule.Applicability[Expr[Node]]] = {
      import isMap.{Key, Value}
      if (!(Key <:< Type[String]))
        MIO.pure(Rule.yielded(s"Map key type ${Key.prettyPrint} is not String"))
      else {
        LambdaBuilder
          .of1[Pair]("pair")
          .traverse { pairExpr =>
            val keyExpr = isMap.key(pairExpr)
            val valueExpr = isMap.value(pairExpr)
            deriveEncoderRecursively[Value](using ectx.nest(valueExpr)).map { valueNode =>
              Expr.quote {
                (Expr.splice(keyExpr).asInstanceOf[String], Expr.splice(valueNode))
              }
            }
          }
          .map { builder =>
            val pairLambda = builder.build[(String, Node)]
            val iterableExpr = isMap.asIterable(ectx.value)
            Rule.matched(Expr.quote {
              YamlDerivationUtils.encodeMappedPairs[Pair](
                Expr.splice(iterableExpr),
                Expr.splice(pairLambda)
              )
            })
          }
      }
    }
  }

  object EncHandleAsCollectionRule extends EncoderDerivationRule("handle as collection when possible") {
    implicit val NodeT: Type[Node] = Types.Node

    def apply[A: EncoderCtx]: MIO[Rule.Applicability[Expr[Node]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as a collection") >> {
        Type[A] match {
          case IsCollection(isCollection) =>
            import isCollection.Underlying as Item
            LambdaBuilder
              .of1[Item]("item")
              .traverse { itemExpr =>
                deriveEncoderRecursively[Item](using ectx.nest(itemExpr))
              }
              .map { builder =>
                val lambda = builder.build[Node]
                val iterableExpr = isCollection.value.asIterable(ectx.value)
                Rule.matched(Expr.quote {
                  YamlDerivationUtils.encodeIterable[Item](
                    Expr.splice(iterableExpr),
                    Expr.splice(lambda)
                  )
                })
              }

          case _ =>
            MIO.pure(Rule.yielded(s"The type ${Type[A].prettyPrint} is not a collection"))
        }
      }
  }

  object EncHandleAsCaseClassRule extends EncoderDerivationRule("handle as case class when possible") {

    def apply[A: EncoderCtx]: MIO[Rule.Applicability[Expr[Node]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as a case class") >> {
        CaseClass.parse[A] match {
          case Some(caseClass) =>
            for {
              _ <- ectx.setHelper[A] { (value, config) =>
                encodeCaseClassFields[A](caseClass)(using ectx.nestInCache(value, config))
              }
              result <- ectx.getHelper[A].flatMap {
                case Some(helperCall) => MIO.pure(Rule.matched(helperCall(ectx.value, ectx.config)))
                case None             => MIO.pure(Rule.yielded(s"Failed to build helper for ${Type[A].prettyPrint}"))
              }
            } yield result

          case None =>
            MIO.pure(Rule.yielded(s"The type ${Type[A].prettyPrint} is not a case class"))
        }
      }

    @scala.annotation.nowarn("msg=is never used")
    private def encodeCaseClassFields[A: EncoderCtx](
        caseClass: CaseClass[A]
    ): MIO[Expr[Node]] = {
      implicit val NodeT: Type[Node] = Types.Node
      implicit val StringT: Type[String] = Types.String
      implicit val fieldNameT: Type[fieldName] = Types.FieldName
      implicit val transientFieldT: Type[transientField] = Types.TransientField

      val allFields = caseClass.caseFieldValuesAt(ectx.value).toList

      // Singletons (case objects, parameterless enum cases) have no primary constructor.
      // Only access primaryConstructor when there are actual fields to process.
      val paramsByName: Map[String, Parameter] =
        if (allFields.isEmpty) Map.empty
        else caseClass.primaryConstructor.parameters.flatten.toMap

      // Validate: @transientField on fields without defaults is a compile error
      paramsByName.collectFirst {
        case (name, param) if hasAnnotationType[transientField](param) && !param.hasDefault => name
      } match {
        case Some(name) =>
          val err = EncoderDerivationError.TransientFieldMissingDefault(name, Type[A].prettyPrint)
          Log.error(err.message) >> MIO.fail(err)
        case None =>
          val nonTransientFields = allFields.filter { case (name, _) =>
            paramsByName.get(name).forall(p => !hasAnnotationType[transientField](p))
          }

          NonEmptyList.fromList(nonTransientFields) match {
            case Some(fields) =>
              fields
                .parTraverse { case (fName, fieldValue) =>
                  import fieldValue.{Underlying as Field, value as fieldExpr}
                  Log.namedScope(s"Encoding field ${ectx.value.prettyPrint}.$fName: ${Type[Field].prettyPrint}") {
                    deriveEncoderRecursively[Field](using ectx.nest(fieldExpr)).map { fieldNode =>
                      val nameOverride =
                        paramsByName.get(fName).flatMap(p => getAnnotationStringArg[fieldName](p))
                      (fName, fieldNode, nameOverride)
                    }
                  }
                }
                .map { fieldPairs =>
                  fieldPairs.toList.foldRight(Expr.quote(List.empty[(String, Node)])) {
                    case ((fName, fieldNode, Some(customName)), acc) =>
                      Expr.quote {
                        (
                          Expr.splice(Expr(customName)),
                          Expr.splice(fieldNode)
                        ) ::
                          Expr.splice(acc)
                      }
                    case ((fName, fieldNode, None), acc) =>
                      Expr.quote {
                        (
                          Expr.splice(ectx.config).transformMemberNames(Expr.splice(Expr(fName))),
                          Expr.splice(fieldNode)
                        ) ::
                          Expr.splice(acc)
                      }
                  }
                }
                .map { fieldsListExpr =>
                  Expr.quote {
                    YamlDerivationUtils.nodeFromFields(Expr.splice(fieldsListExpr))
                  }
                }
            case None =>
              MIO.pure(Expr.quote {
                YamlDerivationUtils.nodeFromFields(Nil)
              })
          }
      }
    }
  }

  object EncHandleAsEnumRule extends EncoderDerivationRule("handle as enum when possible") {

    def apply[A: EncoderCtx]: MIO[Rule.Applicability[Expr[Node]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as an enum") >> {
        Enum.parse[A] match {
          case Some(enumm) =>
            for {
              _ <- ectx.setHelper[A] { (value, config) =>
                encodeEnumCases[A](enumm)(using ectx.nestInCache(value, config))
              }
              result <- ectx.getHelper[A].flatMap {
                case Some(helperCall) => MIO.pure(Rule.matched(helperCall(ectx.value, ectx.config)))
                case None             => MIO.pure(Rule.yielded(s"Failed to build helper for ${Type[A].prettyPrint}"))
              }
            } yield result
          case None =>
            MIO.pure(Rule.yielded(s"The type ${Type[A].prettyPrint} is not an enum"))
        }
      }

    private def encodeEnumCases[A: EncoderCtx](
        enumm: Enum[A]
    ): MIO[Expr[Node]] = {
      implicit val NodeT: Type[Node] = Types.Node

      // Check at compile time if all children are singletons (case objects with no fields)
      val allCaseObjects = enumm.directChildren.toList.forall { case (_, child) =>
        Type.isVal(using child.Underlying) ||
        CaseClass.parse(using child.Underlying).exists(_.primaryConstructor.parameters.flatten.isEmpty)
      }

      enumm
        .parMatchOn[MIO, Node](ectx.value) { matched =>
          import matched.{value as enumCaseValue, Underlying as EnumCase}
          Log.namedScope(s"Encoding enum case ${enumCaseValue.prettyPrint}: ${EnumCase.prettyPrint}") {
            deriveEncoderRecursively[EnumCase](using ectx.nest(enumCaseValue)).map { caseNode =>
              val caseName = Type[EnumCase].shortName
              if (allCaseObjects) {
                Expr.quote {
                  val config = Expr.splice(ectx.config)
                  val name = config.transformConstructorNames(Expr.splice(Expr(caseName)))
                  if (config.enumAsStrings) {
                    YamlDerivationUtils.encodeEnumAsString(name)
                  } else {
                    val node = Expr.splice(caseNode)
                    config.discriminator match {
                      case Some(discriminatorField) =>
                        YamlDerivationUtils.addDiscriminator(discriminatorField, name, node)
                      case None =>
                        YamlDerivationUtils.wrapWithTypeName(name, node)
                    }
                  }
                }
              } else {
                Expr.quote {
                  val name = Expr.splice(ectx.config).transformConstructorNames(Expr.splice(Expr(caseName)))
                  val node = Expr.splice(caseNode)
                  Expr.splice(ectx.config).discriminator match {
                    case Some(discriminatorField) =>
                      YamlDerivationUtils.addDiscriminator(discriminatorField, name, node)
                    case None =>
                      YamlDerivationUtils.wrapWithTypeName(name, node)
                  }
                }
              }
            }
          }
        }
        .flatMap {
          case Some(result) => MIO.pure(result)
          case None         =>
            val err = EncoderDerivationError.NoChildrenInSealedTrait(Type[A].prettyPrint)
            Log.error(err.message) >> MIO.fail(err)
        }
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
  }
}

sealed private[compiletime] trait EncoderDerivationError
    extends util.control.NoStackTrace
    with Product
    with Serializable {
  def message: String
  override def getMessage(): String = message
}
private[compiletime] object EncoderDerivationError {
  final case class UnsupportedType(tpeName: String, reasons: List[String]) extends EncoderDerivationError {
    override def message: String =
      s"The type $tpeName was not handled by any encoder derivation rule:\n${reasons.mkString("\n")}"
  }
  final case class TransientFieldMissingDefault(fieldName: String, tpeName: String) extends EncoderDerivationError {
    override def message: String =
      s"@transientField on field '$fieldName' of $tpeName requires a default value"
  }
  final case class NoChildrenInSealedTrait(tpeName: String) extends EncoderDerivationError {
    override def message: String =
      s"The type $tpeName does not have any children!"
  }
}
