package hearth.kindlings.circederivation.internal.compiletime

import hearth.MacroCommons
import hearth.fp.data.NonEmptyList
import hearth.fp.effect.*
import hearth.fp.syntax.*
import hearth.std.*

import hearth.kindlings.circederivation.{Configuration, KindlingsEncoder}
import hearth.kindlings.circederivation.internal.runtime.CirceDerivationUtils
import io.circe.{Encoder, Json}

trait EncoderMacrosImpl { this: MacroCommons & StdExtensions =>

  // Entrypoints

  def deriveInlineEncode[A: Type](valueExpr: Expr[A], configExpr: Expr[Configuration]): Expr[Json] = {
    implicit val JsonT: Type[Json] = Types.Json
    implicit val ConfigT: Type[Configuration] = Types.Configuration

    deriveEncoderFromCtxAndAdaptForEntrypoint[A, Json]("KindlingsEncoder.encode") { fromCtx =>
      ValDefs.createVal[A](valueExpr).use { valueVal =>
        ValDefs.createVal[Configuration](configExpr).use { configVal =>
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
  def deriveEncoderTypeClass[A: Type](configExpr: Expr[Configuration]): Expr[KindlingsEncoder[A]] = {
    implicit val EncoderA: Type[Encoder[A]] = Types.Encoder[A]
    implicit val KindlingsEncoderA: Type[KindlingsEncoder[A]] = Types.KindlingsEncoder[A]
    implicit val JsonT: Type[Json] = Types.Json
    implicit val ConfigT: Type[Configuration] = Types.Configuration
    val selfType: Option[??] = Some(Type[A].as_??)

    deriveEncoderFromCtxAndAdaptForEntrypoint[A, KindlingsEncoder[A]]("KindlingsEncoder.derived") { fromCtx =>
      ValDefs.createVal[Configuration](configExpr).use { configVal =>
        Expr.quote {
          val cfg = Expr.splice(configVal)
          new KindlingsEncoder[A] {
            def apply(a: A): Json = {
              val _ = a
              Expr.splice {
                fromCtx(EncoderCtx.from(Expr.quote(a), Expr.quote(cfg), derivedType = selfType))
              }
            }
          }
        }
      }
    }
  }

  // Handles logging, error reporting and prepending "cached" defs and vals to the result.

  def deriveEncoderFromCtxAndAdaptForEntrypoint[A: Type, Out: Type](macroName: String)(
      provideCtxAndAdapt: (EncoderCtx[A] => Expr[Json]) => Expr[Out]
  ): Expr[Out] = Log
    .namedScope(
      s"Deriving encoder for ${Type[A].prettyPrint} at: ${Environment.currentPosition.prettyPrint}"
    ) {
      MIO.scoped { runSafe =>
        val fromCtx: (EncoderCtx[A] => Expr[Json]) = (ctx: EncoderCtx[A]) =>
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
      errorRendering = RenderFrom(Log.Level.Info)
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
        "Enable debug logging with: import hearth.kindlings.circederivation.debug.logDerivationForKindlingsEncoder or scalac option -Xmacro-settings:circeDerivation.logDerivation=true"
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
    implicit val LogDerivation: Type[KindlingsEncoder.LogDerivation] = Types.EncoderLogDerivation
    def logDerivationImported = Expr.summonImplicit[KindlingsEncoder.LogDerivation].isDefined

    def logDerivationSetGlobally = (for {
      data <- Environment.typedSettings.toOption
      circeDerivation <- data.get("circeDerivation")
      shouldLog <- circeDerivation.get("logDerivation").flatMap(_.asBoolean)
    } yield shouldLog).getOrElse(false)

    logDerivationImported || logDerivationSetGlobally
  }

  // Context

  final case class EncoderCtx[A](
      tpe: Type[A],
      value: Expr[A],
      config: Expr[Configuration],
      cache: MLocal[ValDefsCache],
      derivedType: Option[??]
  ) {

    def nest[B: Type](newValue: Expr[B]): EncoderCtx[B] = copy[B](
      tpe = Type[B],
      value = newValue
    )

    def nestInCache(
        newValue: Expr[A],
        newConfig: Expr[Configuration]
    ): EncoderCtx[A] = copy(
      value = newValue,
      config = newConfig
    )

    def getInstance[B: Type]: MIO[Option[Expr[Encoder[B]]]] = {
      implicit val EncoderB: Type[Encoder[B]] = Types.Encoder[B]
      cache.get0Ary[Encoder[B]]("cached-encoder-instance")
    }
    def setInstance[B: Type](instance: Expr[Encoder[B]]): MIO[Unit] = {
      implicit val EncoderB: Type[Encoder[B]] = Types.Encoder[B]
      cache.buildCachedWith(
        "cached-encoder-instance",
        ValDefBuilder.ofLazy[Encoder[B]](s"encoder_${Type[B].shortName}")
      )(_ => instance)
    }

    def getHelper[B: Type]: MIO[Option[(Expr[B], Expr[Configuration]) => Expr[Json]]] = {
      implicit val JsonT: Type[Json] = Types.Json
      implicit val ConfigT: Type[Configuration] = Types.Configuration
      cache.get2Ary[B, Configuration, Json]("cached-encode-method")
    }
    def setHelper[B: Type](
        helper: (Expr[B], Expr[Configuration]) => MIO[Expr[Json]]
    ): MIO[Unit] = {
      implicit val JsonT: Type[Json] = Types.Json
      implicit val ConfigT: Type[Configuration] = Types.Configuration
      val defBuilder =
        ValDefBuilder.ofDef2[B, Configuration, Json](s"encode_${Type[B].shortName}")
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
        config: Expr[Configuration],
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
    def apply[A: EncoderCtx]: MIO[Rule.Applicability[Expr[Json]]]
  }

  // The actual derivation logic

  def deriveEncoderRecursively[A: EncoderCtx]: MIO[Expr[Json]] =
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
            Log.info(s"Failed to derive encoder for ${Type[A].prettyPrint}:\n${reasonsStrings.mkString("\n")}") >>
              MIO.fail(EncoderDerivationError.UnsupportedType(Type[A].prettyPrint, reasonsStrings))
        }
      }

  // Rules

  object EncUseCachedDefWhenAvailableRule extends EncoderDerivationRule("use cached def when available") {

    def apply[A: EncoderCtx]: MIO[Rule.Applicability[Expr[Json]]] =
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
        instance: Expr[Encoder[A]]
    ): MIO[Rule.Applicability[Expr[Json]]] =
      Log.info(s"Found cached encoder instance for ${Type[A].prettyPrint}") >> MIO.pure(Rule.matched(Expr.quote {
        Expr.splice(instance).apply(Expr.splice(ectx.value))
      }))

    private def callCachedHelper[A: EncoderCtx](
        helperCall: (Expr[A], Expr[Configuration]) => Expr[Json]
    ): MIO[Rule.Applicability[Expr[Json]]] =
      Log.info(s"Found cached encoder helper for ${Type[A].prettyPrint}") >> MIO.pure(
        Rule.matched(helperCall(ectx.value, ectx.config))
      )

    private def yieldUnsupported[A: EncoderCtx]: MIO[Rule.Applicability[Expr[Json]]] =
      MIO.pure(Rule.yielded(s"The type ${Type[A].prettyPrint} does not have a cached encoder"))
  }

  object EncUseImplicitWhenAvailableRule extends EncoderDerivationRule("use implicit when available") {

    lazy val ignoredImplicits: Seq[UntypedMethod] = {
      val ours = Type.of[KindlingsEncoder.type].methods.collect {
        case method if method.value.name == "derived" => method.value.asUntyped
      }
      val circeEncoder = Type.of[Encoder.type].methods.collect {
        case method if method.value.name == "derived" => method.value.asUntyped
      }
      ours ++ circeEncoder
    }

    def apply[A: EncoderCtx]: MIO[Rule.Applicability[Expr[Json]]] =
      Log.info(s"Attempting to use implicit Encoder for ${Type[A].prettyPrint}") >> {
        // Skip implicit search for the self type being derived to prevent self-referential loops
        // (e.g., `implicit val enc: Encoder[X] = KindlingsEncoder.derived[X]` would otherwise
        // find `enc` itself during macro expansion, generating code that calls itself infinitely).
        if (ectx.derivedType.exists(_.Underlying =:= Type[A]))
          MIO.pure(
            Rule.yielded(s"The type ${Type[A].prettyPrint} is the type being derived, skipping implicit search")
          )
        else
          Types.Encoder[A].summonExprIgnoring(ignoredImplicits*).toEither match {
            case Right(instanceExpr) => cacheAndUse[A](instanceExpr)
            case Left(reason)        => yieldUnsupported[A](reason)
          }
      }

    private def cacheAndUse[A: EncoderCtx](
        instanceExpr: Expr[Encoder[A]]
    ): MIO[Rule.Applicability[Expr[Json]]] =
      Log.info(s"Found implicit encoder ${instanceExpr.prettyPrint}, using directly") >>
        MIO.pure(Rule.matched(Expr.quote {
          Expr.splice(instanceExpr).apply(Expr.splice(ectx.value))
        }))

    private def yieldUnsupported[A: EncoderCtx](reason: String): MIO[Rule.Applicability[Expr[Json]]] =
      MIO.pure(
        Rule.yielded(
          s"The type ${Type[A].prettyPrint} does not have an implicit Encoder instance: $reason"
        )
      )
  }

  object EncHandleAsValueTypeRule extends EncoderDerivationRule("handle as value type when possible") {

    def apply[A: EncoderCtx]: MIO[Rule.Applicability[Expr[Json]]] =
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
    implicit val JsonT: Type[Json] = Types.Json

    def apply[A: EncoderCtx]: MIO[Rule.Applicability[Expr[Json]]] =
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
                val lambda = builder.build[Json]
                Rule.matched(
                  isOption.value.fold[Json](ectx.value)(
                    onEmpty = Expr.quote(Json.Null),
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
    implicit val JsonT: Type[Json] = Types.Json
    implicit val StringT: Type[String] = Types.String
    implicit val StringJsonPairT: Type[(String, Json)] = Type.of[(String, Json)]

    def apply[A: EncoderCtx]: MIO[Rule.Applicability[Expr[Json]]] =
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
    ): MIO[Rule.Applicability[Expr[Json]]] = {
      import isMap.{Key, Value}
      if (!(Key <:< Type[String]))
        MIO.pure(Rule.yielded(s"Map key type ${Key.prettyPrint} is not String"))
      else {
        LambdaBuilder
          .of1[Pair]("pair")
          .traverse { pairExpr =>
            val keyExpr = isMap.key(pairExpr)
            val valueExpr = isMap.value(pairExpr)
            deriveEncoderRecursively[Value](using ectx.nest(valueExpr)).map { valueJson =>
              Expr.quote {
                (Expr.splice(keyExpr).asInstanceOf[String], Expr.splice(valueJson))
              }
            }
          }
          .map { builder =>
            val pairLambda = builder.build[(String, Json)]
            val iterableExpr = isMap.asIterable(ectx.value)
            Rule.matched(Expr.quote {
              CirceDerivationUtils.jsonFromMappedPairs[Pair](
                Expr.splice(iterableExpr),
                Expr.splice(pairLambda)
              )
            })
          }
      }
    }
  }

  object EncHandleAsCollectionRule extends EncoderDerivationRule("handle as collection when possible") {
    implicit val JsonT: Type[Json] = Types.Json

    def apply[A: EncoderCtx]: MIO[Rule.Applicability[Expr[Json]]] =
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
                val lambda = builder.build[Json]
                val iterableExpr = isCollection.value.asIterable(ectx.value)
                Rule.matched(Expr.quote {
                  CirceDerivationUtils.encodeIterable[Item](
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

    def apply[A: EncoderCtx]: MIO[Rule.Applicability[Expr[Json]]] =
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
    ): MIO[Expr[Json]] = {
      implicit val JsonT: Type[Json] = Types.Json
      implicit val StringT: Type[String] = Types.String

      NonEmptyList.fromList(caseClass.caseFieldValuesAt(ectx.value).toList) match {
        case Some(fields) =>
          fields
            .parTraverse { case (fieldName, fieldValue) =>
              import fieldValue.{Underlying as Field, value as fieldExpr}
              Log.namedScope(s"Encoding field ${ectx.value.prettyPrint}.$fieldName: ${Type[Field].prettyPrint}") {
                deriveEncoderRecursively[Field](using ectx.nest(fieldExpr)).map { fieldJson =>
                  (fieldName, fieldJson)
                }
              }
            }
            .map { fieldPairs =>
              fieldPairs.toList.foldRight(Expr.quote(List.empty[(String, Json)])) {
                case ((fieldName, fieldJson), acc) =>
                  Expr.quote {
                    (
                      Expr.splice(ectx.config).transformMemberNames(Expr.splice(Expr(fieldName))),
                      Expr
                        .splice(fieldJson)
                    ) ::
                      Expr.splice(acc)
                  }
              }
            }
            .map { fieldsListExpr =>
              Expr.quote {
                CirceDerivationUtils.jsonFromFields(Expr.splice(fieldsListExpr))
              }
            }
        case None =>
          MIO.pure(Expr.quote {
            CirceDerivationUtils.jsonFromFields(Nil)
          })
      }
    }
  }

  object EncHandleAsEnumRule extends EncoderDerivationRule("handle as enum when possible") {

    def apply[A: EncoderCtx]: MIO[Rule.Applicability[Expr[Json]]] =
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
    ): MIO[Expr[Json]] = {
      implicit val JsonT: Type[Json] = Types.Json

      // Check at compile time if all children are singletons (case objects with no fields)
      val allCaseObjects = enumm.directChildren.toList.forall { case (_, child) =>
        Type.isVal(using child.Underlying) ||
        CaseClass.parse(using child.Underlying).exists(_.primaryConstructor.parameters.flatten.isEmpty)
      }

      enumm
        .parMatchOn[MIO, Json](ectx.value) { matched =>
          import matched.{value as enumCaseValue, Underlying as EnumCase}
          Log.namedScope(s"Encoding enum case ${enumCaseValue.prettyPrint}: ${EnumCase.prettyPrint}") {
            deriveEncoderRecursively[EnumCase](using ectx.nest(enumCaseValue)).map { caseJson =>
              val caseName = Type[EnumCase].shortName
              if (allCaseObjects) {
                Expr.quote {
                  val config = Expr.splice(ectx.config)
                  val name = config.transformConstructorNames(Expr.splice(Expr(caseName)))
                  if (config.enumAsStrings) {
                    CirceDerivationUtils.encodeEnumAsString(name)
                  } else {
                    val json = Expr.splice(caseJson)
                    config.discriminator match {
                      case Some(discriminatorField) =>
                        CirceDerivationUtils.addDiscriminator(discriminatorField, name, json)
                      case None =>
                        CirceDerivationUtils.wrapWithTypeName(name, json)
                    }
                  }
                }
              } else {
                Expr.quote {
                  val name = Expr.splice(ectx.config).transformConstructorNames(Expr.splice(Expr(caseName)))
                  val json = Expr.splice(caseJson)
                  Expr.splice(ectx.config).discriminator match {
                    case Some(discriminatorField) =>
                      CirceDerivationUtils.addDiscriminator(discriminatorField, name, json)
                    case None =>
                      CirceDerivationUtils.wrapWithTypeName(name, json)
                  }
                }
              }
            }
          }
        }
        .flatMap {
          case Some(result) => MIO.pure(result)
          case None         =>
            MIO.fail(new RuntimeException(s"The type ${Type[A].prettyPrint} does not have any children!"))
        }
    }
  }

  // Types

  private[compiletime] object Types {

    def Encoder: Type.Ctor1[Encoder] = Type.Ctor1.of[Encoder]
    def KindlingsEncoder: Type.Ctor1[KindlingsEncoder] = Type.Ctor1.of[KindlingsEncoder]
    val EncoderLogDerivation: Type[hearth.kindlings.circederivation.KindlingsEncoder.LogDerivation] =
      Type.of[hearth.kindlings.circederivation.KindlingsEncoder.LogDerivation]
    val Json: Type[Json] = Type.of[Json]
    val Configuration: Type[Configuration] = Type.of[Configuration]
    val String: Type[String] = Type.of[String]
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
}
