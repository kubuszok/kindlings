package hearth.kindlings.avroderivation.internal.compiletime

import hearth.MacroCommons
import hearth.fp.data.NonEmptyList
import hearth.fp.effect.*
import hearth.fp.syntax.*
import hearth.std.*

import hearth.kindlings.avroderivation.{AvroConfig, AvroEncoder}
import hearth.kindlings.avroderivation.internal.runtime.AvroDerivationUtils
import org.apache.avro.Schema
import org.apache.avro.generic.GenericData

trait EncoderMacrosImpl { this: MacroCommons & StdExtensions & SchemaForMacrosImpl =>

  // Entrypoints

  def deriveInlineEncode[A: Type](valueExpr: Expr[A], configExpr: Expr[AvroConfig]): Expr[Any] = {
    implicit val AnyT: Type[Any] = EncTypes.Any
    implicit val ConfigT: Type[AvroConfig] = EncTypes.AvroConfig

    deriveEncoderFromCtxAndAdaptForEntrypoint[A, Any]("AvroEncoder.encode") { fromCtx =>
      ValDefs.createVal[A](valueExpr).use { valueVal =>
        ValDefs.createVal[AvroConfig](configExpr).use { configVal =>
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
  def deriveEncoderTypeClass[A: Type](configExpr: Expr[AvroConfig]): Expr[AvroEncoder[A]] = {
    implicit val AvroEncoderA: Type[AvroEncoder[A]] = EncTypes.AvroEncoder[A]
    implicit val AnyT: Type[Any] = EncTypes.Any
    implicit val SchemaT: Type[Schema] = EncTypes.Schema
    implicit val ConfigT: Type[AvroConfig] = EncTypes.AvroConfig
    val selfType: Option[??] = Some(Type[A].as_??)

    // Schema and encoder are derived in the same MIO.scoped block to avoid Scala 3 splice isolation issues.
    Log
      .namedScope(
        s"Deriving encoder for ${Type[A].prettyPrint} at: ${Environment.currentPosition.prettyPrint}"
      ) {
        MIO.scoped { runSafe =>
          // Derive schema with its own cache (self-contained)
          val schemaExpr: Expr[Schema] = runSafe {
            deriveSelfContainedSchema[A](configExpr)
          }

          // Create encoder derivation callback
          val fromCtx: (EncoderCtx[A] => Expr[Any]) = (ctx: EncoderCtx[A]) =>
            runSafe {
              for {
                _ <- Environment.loadStandardExtensions().toMIO(allowFailures = false)
                result <- deriveEncoderRecursively[A](using ctx)
                cache <- ctx.cache.get
              } yield cache.toValDefs.use(_ => result)
            }

          // Assemble the type class instance
          ValDefs.createVal[AvroConfig](configExpr).use { configVal =>
            Expr.quote {
              val cfg = Expr.splice(configVal)
              val sch = Expr.splice(schemaExpr)
              (new AvroEncoder[A] {
                val schema: Schema = sch
                def encode(value: A): Any = {
                  val _ = value
                  val _ = cfg
                  Expr.splice {
                    fromCtx(EncoderCtx.from(Expr.quote(value), Expr.quote(cfg), derivedType = selfType))
                  }
                }
              }): AvroEncoder[A]
            }
          }
        }
      }
      .flatTap { result =>
        Log.info(s"Derived final encoder result: ${result.prettyPrint}")
      }
      .runToExprOrFail(
        "AvroEncoder.derived",
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
          "Enable debug logging with: import hearth.kindlings.avroderivation.debug.logDerivationForAvroEncoder or scalac option -Xmacro-settings:avroDerivation.logDerivation=true"
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

  def deriveEncoderFromCtxAndAdaptForEntrypoint[A: Type, Out: Type](macroName: String)(
      provideCtxAndAdapt: (EncoderCtx[A] => Expr[Any]) => Expr[Out]
  ): Expr[Out] = Log
    .namedScope(
      s"Deriving encoder for ${Type[A].prettyPrint} at: ${Environment.currentPosition.prettyPrint}"
    ) {
      MIO.scoped { runSafe =>
        val fromCtx: (EncoderCtx[A] => Expr[Any]) = (ctx: EncoderCtx[A]) =>
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
        "Enable debug logging with: import hearth.kindlings.avroderivation.debug.logDerivationForAvroEncoder or scalac option -Xmacro-settings:avroDerivation.logDerivation=true"
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
    implicit val LogDerivation: Type[AvroEncoder.LogDerivation] = EncTypes.EncoderLogDerivation
    def logDerivationImported = Expr.summonImplicit[AvroEncoder.LogDerivation].isDefined

    def logDerivationSetGlobally = (for {
      data <- Environment.typedSettings.toOption
      avroDerivation <- data.get("avroDerivation")
      shouldLog <- avroDerivation.get("logDerivation").flatMap(_.asBoolean)
    } yield shouldLog).getOrElse(false)

    logDerivationImported || logDerivationSetGlobally
  }

  // Context

  final case class EncoderCtx[A](
      tpe: Type[A],
      value: Expr[A],
      config: Expr[AvroConfig],
      cache: MLocal[ValDefsCache],
      derivedType: Option[??]
  ) {

    def nest[B: Type](newValue: Expr[B]): EncoderCtx[B] = copy[B](
      tpe = Type[B],
      value = newValue
    )

    def nestInCache(
        newValue: Expr[A],
        newConfig: Expr[AvroConfig]
    ): EncoderCtx[A] = copy(
      value = newValue,
      config = newConfig
    )

    def getInstance[B: Type]: MIO[Option[Expr[AvroEncoder[B]]]] = {
      implicit val EncoderB: Type[AvroEncoder[B]] = EncTypes.AvroEncoder[B]
      cache.get0Ary[AvroEncoder[B]]("cached-encoder-instance")
    }
    def setInstance[B: Type](instance: Expr[AvroEncoder[B]]): MIO[Unit] = {
      implicit val EncoderB: Type[AvroEncoder[B]] = EncTypes.AvroEncoder[B]
      cache.buildCachedWith(
        "cached-encoder-instance",
        ValDefBuilder.ofLazy[AvroEncoder[B]](s"encoder_${Type[B].shortName}")
      )(_ => instance)
    }

    def getHelper[B: Type]: MIO[Option[(Expr[B], Expr[AvroConfig]) => Expr[Any]]] = {
      implicit val AnyT: Type[Any] = EncTypes.Any
      implicit val ConfigT: Type[AvroConfig] = EncTypes.AvroConfig
      cache.get2Ary[B, AvroConfig, Any]("cached-encode-method")
    }
    def setHelper[B: Type](
        helper: (Expr[B], Expr[AvroConfig]) => MIO[Expr[Any]]
    ): MIO[Unit] = {
      implicit val AnyT: Type[Any] = EncTypes.Any
      implicit val ConfigT: Type[AvroConfig] = EncTypes.AvroConfig
      val defBuilder =
        ValDefBuilder.ofDef2[B, AvroConfig, Any](s"encode_${Type[B].shortName}")
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
        config: Expr[AvroConfig],
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
    def apply[A: EncoderCtx]: MIO[Rule.Applicability[Expr[Any]]]
  }

  // The actual derivation logic

  def deriveEncoderRecursively[A: EncoderCtx]: MIO[Expr[Any]] =
    Log
      .namedScope(s"Deriving encoder for type ${Type[A].prettyPrint}") {
        Rules(
          EncUseCachedDefWhenAvailableRule,
          EncUseImplicitWhenAvailableRule,
          EncUseBuiltInSupportRule,
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

    def apply[A: EncoderCtx]: MIO[Rule.Applicability[Expr[Any]]] =
      Log.info(s"Attempting to use cached encoder for ${Type[A].prettyPrint}") >>
        ectx.getInstance[A].flatMap {
          case Some(instance) =>
            Log.info(s"Found cached encoder instance for ${Type[A].prettyPrint}") >> MIO.pure(
              Rule.matched(Expr.quote {
                Expr.splice(instance).encode(Expr.splice(ectx.value))
              })
            )
          case None =>
            ectx.getHelper[A].flatMap {
              case Some(helperCall) =>
                Log.info(s"Found cached encoder helper for ${Type[A].prettyPrint}") >> MIO.pure(
                  Rule.matched(helperCall(ectx.value, ectx.config))
                )
              case None =>
                MIO.pure(Rule.yielded(s"The type ${Type[A].prettyPrint} does not have a cached encoder"))
            }
        }
  }

  object EncUseImplicitWhenAvailableRule extends EncoderDerivationRule("use implicit when available") {

    lazy val ignoredImplicits: Seq[UntypedMethod] =
      Type.of[AvroEncoder.type].methods.collect {
        case method if method.value.name == "derived" => method.value.asUntyped
      }

    def apply[A: EncoderCtx]: MIO[Rule.Applicability[Expr[Any]]] =
      Log.info(s"Attempting to use implicit AvroEncoder for ${Type[A].prettyPrint}") >> {
        if (ectx.derivedType.exists(_.Underlying =:= Type[A]))
          MIO.pure(
            Rule.yielded(s"The type ${Type[A].prettyPrint} is the type being derived, skipping implicit search")
          )
        else
          EncTypes.AvroEncoder[A].summonExprIgnoring(ignoredImplicits*).toEither match {
            case Right(instanceExpr) =>
              Log.info(s"Found implicit encoder ${instanceExpr.prettyPrint}, using directly") >>
                MIO.pure(Rule.matched(Expr.quote {
                  Expr.splice(instanceExpr).encode(Expr.splice(ectx.value))
                }))
            case Left(reason) =>
              MIO.pure(
                Rule.yielded(
                  s"The type ${Type[A].prettyPrint} does not have an implicit AvroEncoder instance: $reason"
                )
              )
          }
      }
  }

  object EncUseBuiltInSupportRule extends EncoderDerivationRule("use built-in support for primitives") {

    def apply[A: EncoderCtx]: MIO[Rule.Applicability[Expr[Any]]] =
      Log.info(s"Attempting to use built-in encoder for ${Type[A].prettyPrint}") >> {
        builtInEncode[A] match {
          case Some(encodeExpr) =>
            Log.info(s"Found built-in encoder for ${Type[A].prettyPrint}") >>
              MIO.pure(Rule.matched(encodeExpr))
          case None =>
            MIO.pure(Rule.yielded(s"The type ${Type[A].prettyPrint} is not a built-in type"))
        }
      }

    @scala.annotation.nowarn("msg=is never used")
    private def builtInEncode[A: EncoderCtx]: Option[Expr[Any]] = {
      val tpe = Type[A]
      val value = ectx.value
      if (tpe =:= Type.of[Boolean])
        Some(Expr.quote(Expr.splice(value).asInstanceOf[Any]))
      else if (tpe =:= Type.of[Int])
        Some(Expr.quote(Expr.splice(value).asInstanceOf[Any]))
      else if (tpe =:= Type.of[Long])
        Some(Expr.quote(Expr.splice(value).asInstanceOf[Any]))
      else if (tpe =:= Type.of[Float])
        Some(Expr.quote(Expr.splice(value).asInstanceOf[Any]))
      else if (tpe =:= Type.of[Double])
        Some(Expr.quote(Expr.splice(value).asInstanceOf[Any]))
      else if (tpe =:= Type.of[String])
        Some(Expr.quote(Expr.splice(value).asInstanceOf[Any]))
      else if (tpe =:= Type.of[Byte])
        Some(Expr.quote(Expr.splice(value).asInstanceOf[Byte].toInt: Any))
      else if (tpe =:= Type.of[Short])
        Some(Expr.quote(Expr.splice(value).asInstanceOf[Short].toInt: Any))
      else if (tpe =:= Type.of[Char])
        Some(Expr.quote(Expr.splice(value).asInstanceOf[Char].toString: Any))
      else if (tpe =:= Type.of[Array[Byte]])
        Some(Expr.quote(AvroDerivationUtils.wrapByteArray(Expr.splice(value).asInstanceOf[Array[Byte]]): Any))
      else if (tpe =:= Type.of[BigDecimal])
        Some(Expr.quote(Expr.splice(value).asInstanceOf[BigDecimal].toString: Any))
      else if (tpe =:= Type.of[java.util.UUID])
        Some(Expr.quote(AvroDerivationUtils.encodeUUID(Expr.splice(value).asInstanceOf[java.util.UUID]): Any))
      else if (tpe =:= Type.of[java.time.Instant])
        Some(Expr.quote(AvroDerivationUtils.encodeInstant(Expr.splice(value).asInstanceOf[java.time.Instant]): Any))
      else if (tpe =:= Type.of[java.time.LocalDate])
        Some(Expr.quote(AvroDerivationUtils.encodeLocalDate(Expr.splice(value).asInstanceOf[java.time.LocalDate]): Any))
      else if (tpe =:= Type.of[java.time.LocalTime])
        Some(Expr.quote(AvroDerivationUtils.encodeLocalTime(Expr.splice(value).asInstanceOf[java.time.LocalTime]): Any))
      else if (tpe =:= Type.of[java.time.LocalDateTime])
        Some(
          Expr.quote(
            AvroDerivationUtils.encodeLocalDateTime(Expr.splice(value).asInstanceOf[java.time.LocalDateTime]): Any
          )
        )
      else
        None
    }
  }

  object EncHandleAsValueTypeRule extends EncoderDerivationRule("handle as value type when possible") {

    def apply[A: EncoderCtx]: MIO[Rule.Applicability[Expr[Any]]] =
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
    implicit val AnyT: Type[Any] = EncTypes.Any

    def apply[A: EncoderCtx]: MIO[Rule.Applicability[Expr[Any]]] =
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
                val lambda = builder.build[Any]
                Rule.matched(
                  isOption.value.fold[Any](ectx.value)(
                    onEmpty = Expr.quote(null: Any),
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
    implicit val AnyT: Type[Any] = EncTypes.Any
    implicit val StringT: Type[String] = EncTypes.String
    implicit val StringAnyPairT: Type[(String, Any)] = Type.of[(String, Any)]

    def apply[A: EncoderCtx]: MIO[Rule.Applicability[Expr[Any]]] =
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
    ): MIO[Rule.Applicability[Expr[Any]]] = {
      import isMap.{Key, Value}
      if (!(Key <:< Type[String]))
        MIO.pure(Rule.yielded(s"Map key type ${Key.prettyPrint} is not String"))
      else {
        LambdaBuilder
          .of1[Pair]("pair")
          .traverse { pairExpr =>
            val keyExpr = isMap.key(pairExpr)
            val valueExpr = isMap.value(pairExpr)
            deriveEncoderRecursively[Value](using ectx.nest(valueExpr)).map { valueEncoded =>
              Expr.quote {
                (Expr.splice(keyExpr).asInstanceOf[String], Expr.splice(valueEncoded))
              }
            }
          }
          .map { builder =>
            val pairLambda = builder.build[(String, Any)]
            val iterableExpr = isMap.asIterable(ectx.value)
            Rule.matched(Expr.quote {
              val map = new java.util.HashMap[String, Any]()
              Expr.splice(iterableExpr).foreach { pair =>
                val encoded = Expr.splice(pairLambda).apply(pair)
                map.put(encoded._1, encoded._2)
              }
              map: Any
            })
          }
      }
    }
  }

  object EncHandleAsCollectionRule extends EncoderDerivationRule("handle as collection when possible") {
    implicit val AnyT: Type[Any] = EncTypes.Any

    def apply[A: EncoderCtx]: MIO[Rule.Applicability[Expr[Any]]] =
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
                val lambda = builder.build[Any]
                val iterableExpr = isCollection.value.asIterable(ectx.value)
                Rule.matched(Expr.quote {
                  AvroDerivationUtils.encodeIterable[Item](
                    Expr.splice(iterableExpr),
                    (item: Item) => Expr.splice(lambda).apply(item)
                  ): Any
                })
              }

          case _ =>
            MIO.pure(Rule.yielded(s"The type ${Type[A].prettyPrint} is not a collection"))
        }
      }
  }

  object EncHandleAsCaseClassRule extends EncoderDerivationRule("handle as case class when possible") {

    def apply[A: EncoderCtx]: MIO[Rule.Applicability[Expr[Any]]] =
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
    ): MIO[Expr[Any]] = {
      implicit val AnyT: Type[Any] = EncTypes.Any
      implicit val StringT: Type[String] = EncTypes.String
      implicit val SchemaT: Type[Schema] = EncTypes.Schema

      NonEmptyList.fromList(caseClass.caseFieldValuesAt(ectx.value).toList) match {
        case Some(fields) =>
          fields
            .parTraverse { case (fieldName, fieldValue) =>
              import fieldValue.{Underlying as Field, value as fieldExpr}
              Log.namedScope(s"Encoding field ${ectx.value.prettyPrint}.$fieldName: ${Type[Field].prettyPrint}") {
                deriveEncoderRecursively[Field](using ectx.nest(fieldExpr)).map { fieldEncoded =>
                  (fieldName, fieldEncoded)
                }
              }
            }
            .flatMap { fieldPairs =>
              val fieldsListExpr = fieldPairs.toList.foldRight(
                Expr.quote(List.empty[(String, Any)])
              ) { case ((fieldName, fieldEncoded), acc) =>
                Expr.quote {
                  (
                    Expr.splice(ectx.config).transformFieldNames(Expr.splice(Expr(fieldName))),
                    Expr.splice(fieldEncoded)
                  ) :: Expr.splice(acc)
                }
              }

              deriveSelfContainedSchema[A](ectx.config).map { schemaExpr =>
                Expr.quote {
                  val schema = Expr.splice(schemaExpr)
                  val fields = Expr.splice(fieldsListExpr)
                  val record = new GenericData.Record(schema)
                  fields.foreach { case (name, value) =>
                    record.put(name, value)
                  }
                  record: Any
                }
              }
            }
        case None =>
          deriveSelfContainedSchema[A](ectx.config).map { schemaExpr =>
            Expr.quote {
              val record = new GenericData.Record(Expr.splice(schemaExpr))
              record: Any
            }
          }
      }
    }
  }

  object EncHandleAsEnumRule extends EncoderDerivationRule("handle as enum when possible") {

    def apply[A: EncoderCtx]: MIO[Rule.Applicability[Expr[Any]]] =
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

    @scala.annotation.nowarn("msg=is never used")
    private def encodeEnumCases[A: EncoderCtx](
        enumm: Enum[A]
    ): MIO[Expr[Any]] = {
      implicit val AnyT: Type[Any] = EncTypes.Any
      implicit val SchemaT: Type[Schema] = EncTypes.Schema

      val childrenList = enumm.directChildren.toList

      val allCaseObjects = childrenList.forall { case (_, child) =>
        Type.isVal(using child.Underlying) ||
        CaseClass.parse(using child.Underlying).exists(_.primaryConstructor.parameters.flatten.isEmpty)
      }

      if (allCaseObjects) {
        // Pure enum → encode as GenericData.EnumSymbol
        deriveSelfContainedSchema[A](ectx.config).flatMap { schemaExpr =>
          enumm
            .parMatchOn[MIO, Any](ectx.value) { matched =>
              import matched.Underlying as EnumCase
              val caseName = Type[EnumCase].shortName
              MIO.pure(Expr.quote {
                val name = Expr.splice(ectx.config).transformConstructorNames(Expr.splice(Expr(caseName)))
                AvroDerivationUtils.encodeEnumSymbol(Expr.splice(schemaExpr), name): Any
              })
            }
            .flatMap {
              case Some(result) => MIO.pure(result)
              case None         =>
                MIO.fail(new RuntimeException(s"The type ${Type[A].prettyPrint} does not have any children!"))
            }
        }
      } else {
        // Mixed sealed trait → encode as the appropriate record subtype
        enumm
          .parMatchOn[MIO, Any](ectx.value) { matched =>
            import matched.{value as enumCaseValue, Underlying as EnumCase}
            Log.namedScope(s"Encoding enum case ${Type[EnumCase].prettyPrint}") {
              deriveEncoderRecursively[EnumCase](using ectx.nest(enumCaseValue))
            }
          }
          .flatMap {
            case Some(result) => MIO.pure(result)
            case None         =>
              MIO.fail(new RuntimeException(s"The type ${Type[A].prettyPrint} does not have any children!"))
          }
      }
    }
  }

  // Types

  private[compiletime] object EncTypes {

    def AvroEncoder: Type.Ctor1[AvroEncoder] = Type.Ctor1.of[AvroEncoder]
    val EncoderLogDerivation: Type[hearth.kindlings.avroderivation.AvroEncoder.LogDerivation] =
      Type.of[hearth.kindlings.avroderivation.AvroEncoder.LogDerivation]
    val Schema: Type[Schema] = Type.of[Schema]
    val AvroConfig: Type[AvroConfig] = Type.of[AvroConfig]
    val String: Type[String] = Type.of[String]
    val Any: Type[Any] = Type.of[Any]
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
