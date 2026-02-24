package hearth.kindlings.avroderivation.internal.compiletime

import hearth.MacroCommons
import hearth.fp.data.NonEmptyList
import hearth.fp.effect.*
import hearth.fp.syntax.*
import hearth.std.*

import hearth.kindlings.avroderivation.{AvroConfig, AvroDecoder}
import hearth.kindlings.avroderivation.annotations.{fieldName, transientField}
import hearth.kindlings.avroderivation.internal.runtime.AvroDerivationUtils
import org.apache.avro.Schema
import org.apache.avro.generic.GenericRecord

trait DecoderMacrosImpl { this: MacroCommons & StdExtensions & SchemaForMacrosImpl & AnnotationSupport =>

  // Entrypoints

  @scala.annotation.nowarn("msg=is never used")
  def deriveInlineDecode[A: Type](
      avroValueExpr: Expr[Any],
      configExpr: Expr[AvroConfig]
  ): Expr[A] = {
    implicit val AnyT: Type[Any] = DecTypes.Any
    implicit val ConfigT: Type[AvroConfig] = DecTypes.AvroConfig

    deriveDecoderFromCtxAndAdaptForEntrypoint[A, A]("AvroDecoder.decode") { fromCtx =>
      ValDefs.createVal[Any](avroValueExpr).use { avroVal =>
        ValDefs.createVal[AvroConfig](configExpr).use { configVal =>
          Expr.quote {
            val _ = Expr.splice(avroVal)
            val _ = Expr.splice(configVal)
            Expr.splice(fromCtx(DecoderCtx.from(avroVal, configVal, derivedType = None)))
          }
        }
      }
    }
  }

  @scala.annotation.nowarn("msg=is never used")
  def deriveDecoderTypeClass[A: Type](configExpr: Expr[AvroConfig]): Expr[AvroDecoder[A]] = {
    implicit val AvroDecoderA: Type[AvroDecoder[A]] = DecTypes.AvroDecoder[A]
    implicit val AnyT: Type[Any] = DecTypes.Any
    implicit val SchemaT: Type[Schema] = DecTypes.Schema
    implicit val ConfigT: Type[AvroConfig] = DecTypes.AvroConfig
    val selfType: Option[??] = Some(Type[A].as_??)

    // Schema and decoder are derived in the same MIO.scoped block to avoid Scala 3 splice isolation issues.
    Log
      .namedScope(
        s"Deriving decoder for ${Type[A].prettyPrint} at: ${Environment.currentPosition.prettyPrint}"
      ) {
        MIO.scoped { runSafe =>
          // Derive schema with its own cache (self-contained)
          val schemaExpr: Expr[Schema] = runSafe {
            deriveSelfContainedSchema[A](configExpr)
          }

          // Create decoder derivation callback
          val fromCtx: (DecoderCtx[A] => Expr[A]) = (ctx: DecoderCtx[A]) =>
            runSafe {
              for {
                _ <- Environment.loadStandardExtensions().toMIO(allowFailures = false)
                result <- deriveDecoderRecursively[A](using ctx)
                cache <- ctx.cache.get
              } yield cache.toValDefs.use(_ => result)
            }

          // Assemble the type class instance
          ValDefs.createVal[AvroConfig](configExpr).use { configVal =>
            Expr.quote {
              val cfg = Expr.splice(configVal)
              val sch = Expr.splice(schemaExpr)
              (new AvroDecoder[A] {
                val schema: Schema = sch
                def decode(value: Any): A = {
                  val _ = value
                  val _ = cfg
                  Expr.splice {
                    fromCtx(DecoderCtx.from(Expr.quote(value), Expr.quote(cfg), derivedType = selfType))
                  }
                }
              }): AvroDecoder[A]
            }
          }
        }
      }
      .flatTap { result =>
        Log.info(s"Derived final decoder result: ${result.prettyPrint}")
      }
      .runToExprOrFail(
        "AvroDecoder.derived",
        infoRendering = if (shouldWeLogDecoderDerivation) RenderFrom(Log.Level.Info) else DontRender,
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
          "Enable debug logging with: import hearth.kindlings.avroderivation.debug.logDerivationForAvroDecoder or scalac option -Xmacro-settings:avroDerivation.logDerivation=true"
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
      provideCtxAndAdapt: (DecoderCtx[A] => Expr[A]) => Expr[Out]
  ): Expr[Out] = Log
    .namedScope(
      s"Deriving decoder for ${Type[A].prettyPrint} at: ${Environment.currentPosition.prettyPrint}"
    ) {
      MIO.scoped { runSafe =>
        val fromCtx: (DecoderCtx[A] => Expr[A]) = (ctx: DecoderCtx[A]) =>
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
        "Enable debug logging with: import hearth.kindlings.avroderivation.debug.logDerivationForAvroDecoder or scalac option -Xmacro-settings:avroDerivation.logDerivation=true"
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
    implicit val LogDerivation: Type[AvroDecoder.LogDerivation] = DecTypes.DecoderLogDerivation
    def logDerivationImported = Expr.summonImplicit[AvroDecoder.LogDerivation].isDefined

    def logDerivationSetGlobally = (for {
      data <- Environment.typedSettings.toOption
      avroDerivation <- data.get("avroDerivation")
      shouldLog <- avroDerivation.get("logDerivation").flatMap(_.asBoolean)
    } yield shouldLog).getOrElse(false)

    logDerivationImported || logDerivationSetGlobally
  }

  // Context

  final case class DecoderCtx[A](
      tpe: Type[A],
      avroValue: Expr[Any],
      config: Expr[AvroConfig],
      cache: MLocal[ValDefsCache],
      derivedType: Option[??]
  ) {

    def nest[B: Type](newValue: Expr[Any]): DecoderCtx[B] = copy[B](
      tpe = Type[B],
      avroValue = newValue
    )

    def nestInCache(
        newValue: Expr[Any],
        newConfig: Expr[AvroConfig]
    ): DecoderCtx[A] = copy(
      avroValue = newValue,
      config = newConfig
    )

    def getInstance[B: Type]: MIO[Option[Expr[AvroDecoder[B]]]] = {
      implicit val DecoderB: Type[AvroDecoder[B]] = DecTypes.AvroDecoder[B]
      cache.get0Ary[AvroDecoder[B]]("cached-decoder-instance")
    }
    def setInstance[B: Type](instance: Expr[AvroDecoder[B]]): MIO[Unit] = {
      implicit val DecoderB: Type[AvroDecoder[B]] = DecTypes.AvroDecoder[B]
      cache.buildCachedWith(
        "cached-decoder-instance",
        ValDefBuilder.ofLazy[AvroDecoder[B]](s"decoder_${Type[B].shortName}")
      )(_ => instance)
    }

    def getHelper[B: Type]: MIO[Option[(Expr[Any], Expr[AvroConfig]) => Expr[B]]] = {
      implicit val ConfigT: Type[AvroConfig] = DecTypes.AvroConfig
      implicit val AnyT: Type[Any] = DecTypes.Any
      cache.get2Ary[Any, AvroConfig, B]("cached-decode-method")
    }
    def setHelper[B: Type](
        helper: (Expr[Any], Expr[AvroConfig]) => MIO[Expr[B]]
    ): MIO[Unit] = {
      implicit val ConfigT: Type[AvroConfig] = DecTypes.AvroConfig
      implicit val AnyT: Type[Any] = DecTypes.Any
      val defBuilder =
        ValDefBuilder.ofDef2[Any, AvroConfig, B](s"decode_${Type[B].shortName}")
      for {
        _ <- cache.forwardDeclare("cached-decode-method", defBuilder)
        _ <- MIO.scoped { runSafe =>
          runSafe(cache.buildCachedWith("cached-decode-method", defBuilder) { case (_, (value, config)) =>
            runSafe(helper(value, config))
          })
        }
      } yield ()
    }

    override def toString: String =
      s"decode[${tpe.prettyPrint}](avroValue = ${avroValue.prettyPrint}, config = ${config.prettyPrint})"
  }
  object DecoderCtx {

    def from[A: Type](
        avroValue: Expr[Any],
        config: Expr[AvroConfig],
        derivedType: Option[??]
    ): DecoderCtx[A] = DecoderCtx(
      tpe = Type[A],
      avroValue = avroValue,
      config = config,
      cache = ValDefsCache.mlocal,
      derivedType = derivedType
    )
  }

  def dctx[A](implicit A: DecoderCtx[A]): DecoderCtx[A] = A

  implicit def currentDecoderValueType[A: DecoderCtx]: Type[A] = dctx.tpe

  abstract class DecoderDerivationRule(val name: String) extends Rule {
    def apply[A: DecoderCtx]: MIO[Rule.Applicability[Expr[A]]]
  }

  // The actual derivation logic

  def deriveDecoderRecursively[A: DecoderCtx]: MIO[Expr[A]] =
    Log
      .namedScope(s"Deriving decoder for type ${Type[A].prettyPrint}") {
        Rules(
          DecUseCachedDefWhenAvailableRule,
          DecUseImplicitWhenAvailableRule,
          DecUseBuiltInSupportRule,
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
            Log.info(s"Failed to derive decoder for ${Type[A].prettyPrint}:\n${reasonsStrings.mkString("\n")}") >>
              MIO.fail(DecoderDerivationError.UnsupportedType(Type[A].prettyPrint, reasonsStrings))
        }
      }

  // Rules

  object DecUseCachedDefWhenAvailableRule extends DecoderDerivationRule("use cached def when available") {

    def apply[A: DecoderCtx]: MIO[Rule.Applicability[Expr[A]]] =
      Log.info(s"Attempting to use cached decoder for ${Type[A].prettyPrint}") >>
        dctx.getInstance[A].flatMap {
          case Some(instance) =>
            Log.info(s"Found cached decoder instance for ${Type[A].prettyPrint}") >> MIO.pure(
              Rule.matched(Expr.quote {
                Expr.splice(instance).decode(Expr.splice(dctx.avroValue))
              })
            )
          case None =>
            dctx.getHelper[A].flatMap {
              case Some(helperCall) =>
                Log.info(s"Found cached decoder helper for ${Type[A].prettyPrint}") >> MIO.pure(
                  Rule.matched(helperCall(dctx.avroValue, dctx.config))
                )
              case None =>
                MIO.pure(Rule.yielded(s"The type ${Type[A].prettyPrint} does not have a cached decoder"))
            }
        }
  }

  object DecUseImplicitWhenAvailableRule extends DecoderDerivationRule("use implicit when available") {

    lazy val ignoredImplicits: Seq[UntypedMethod] =
      Type.of[AvroDecoder.type].methods.collect {
        case method if method.value.name == "derived" => method.value.asUntyped
      }

    def apply[A: DecoderCtx]: MIO[Rule.Applicability[Expr[A]]] =
      Log.info(s"Attempting to use implicit AvroDecoder for ${Type[A].prettyPrint}") >> {
        if (dctx.derivedType.exists(_.Underlying =:= Type[A]))
          MIO.pure(
            Rule.yielded(s"The type ${Type[A].prettyPrint} is the type being derived, skipping implicit search")
          )
        else
          DecTypes.AvroDecoder[A].summonExprIgnoring(ignoredImplicits*).toEither match {
            case Right(instanceExpr) =>
              Log.info(s"Found implicit decoder ${instanceExpr.prettyPrint}, using directly") >>
                MIO.pure(Rule.matched(Expr.quote {
                  Expr.splice(instanceExpr).decode(Expr.splice(dctx.avroValue))
                }))
            case Left(reason) =>
              MIO.pure(
                Rule.yielded(
                  s"The type ${Type[A].prettyPrint} does not have an implicit AvroDecoder instance: $reason"
                )
              )
          }
      }
  }

  object DecUseBuiltInSupportRule extends DecoderDerivationRule("use built-in support for primitives") {

    def apply[A: DecoderCtx]: MIO[Rule.Applicability[Expr[A]]] =
      Log.info(s"Attempting to use built-in decoder for ${Type[A].prettyPrint}") >> {
        builtInDecode[A] match {
          case Some(decodeExpr) =>
            Log.info(s"Found built-in decoder for ${Type[A].prettyPrint}") >>
              MIO.pure(Rule.matched(decodeExpr))
          case None =>
            MIO.pure(Rule.yielded(s"The type ${Type[A].prettyPrint} is not a built-in type"))
        }
      }

    @scala.annotation.nowarn("msg=is never used")
    private def builtInDecode[A: DecoderCtx]: Option[Expr[A]] = {
      val tpe = Type[A]
      val value = dctx.avroValue
      if (tpe =:= Type.of[Boolean])
        Some(Expr.quote(Expr.splice(value).asInstanceOf[Boolean].asInstanceOf[A]))
      else if (tpe =:= Type.of[Int])
        Some(Expr.quote(Expr.splice(value).asInstanceOf[Int].asInstanceOf[A]))
      else if (tpe =:= Type.of[Long])
        Some(Expr.quote(Expr.splice(value).asInstanceOf[Long].asInstanceOf[A]))
      else if (tpe =:= Type.of[Float])
        Some(Expr.quote(Expr.splice(value).asInstanceOf[Float].asInstanceOf[A]))
      else if (tpe =:= Type.of[Double])
        Some(Expr.quote(Expr.splice(value).asInstanceOf[Double].asInstanceOf[A]))
      else if (tpe =:= Type.of[String])
        Some(Expr.quote(AvroDerivationUtils.decodeCharSequence(Expr.splice(value)).asInstanceOf[A]))
      else if (tpe =:= Type.of[Byte])
        Some(Expr.quote(Expr.splice(value).asInstanceOf[Int].toByte.asInstanceOf[A]))
      else if (tpe =:= Type.of[Short])
        Some(Expr.quote(Expr.splice(value).asInstanceOf[Int].toShort.asInstanceOf[A]))
      else if (tpe =:= Type.of[Char])
        Some(Expr.quote(AvroDerivationUtils.decodeCharSequence(Expr.splice(value)).charAt(0).asInstanceOf[A]))
      else if (tpe =:= Type.of[Array[Byte]])
        Some(Expr.quote(AvroDerivationUtils.decodeByteBuffer(Expr.splice(value)).asInstanceOf[A]))
      else if (tpe =:= Type.of[BigDecimal])
        Some(Expr.quote(BigDecimal(AvroDerivationUtils.decodeCharSequence(Expr.splice(value))).asInstanceOf[A]))
      else if (tpe =:= Type.of[java.util.UUID])
        Some(Expr.quote(AvroDerivationUtils.decodeUUID(Expr.splice(value)).asInstanceOf[A]))
      else if (tpe =:= Type.of[java.time.Instant])
        Some(Expr.quote(AvroDerivationUtils.decodeInstant(Expr.splice(value)).asInstanceOf[A]))
      else if (tpe =:= Type.of[java.time.LocalDate])
        Some(Expr.quote(AvroDerivationUtils.decodeLocalDate(Expr.splice(value)).asInstanceOf[A]))
      else if (tpe =:= Type.of[java.time.LocalTime])
        Some(Expr.quote(AvroDerivationUtils.decodeLocalTime(Expr.splice(value)).asInstanceOf[A]))
      else if (tpe =:= Type.of[java.time.LocalDateTime])
        Some(Expr.quote(AvroDerivationUtils.decodeLocalDateTime(Expr.splice(value)).asInstanceOf[A]))
      else
        None
    }
  }

  object DecHandleAsValueTypeRule extends DecoderDerivationRule("handle as value type when possible") {

    def apply[A: DecoderCtx]: MIO[Rule.Applicability[Expr[A]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as a value type") >> {
        Type[A] match {
          case IsValueType(isValueType) =>
            import isValueType.Underlying as Inner
            for {
              innerResult <- deriveDecoderRecursively[Inner](using dctx.nest[Inner](dctx.avroValue))
            } yield {
              val wrapped = isValueType.value.wrap.apply(innerResult).asInstanceOf[Expr[A]]
              Rule.matched(wrapped)
            }

          case _ =>
            MIO.pure(Rule.yielded(s"The type ${Type[A].prettyPrint} is not a value type"))
        }
      }
  }

  object DecHandleAsOptionRule extends DecoderDerivationRule("handle as Option when possible") {

    def apply[A: DecoderCtx]: MIO[Rule.Applicability[Expr[A]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as Option") >> {
        Type[A] match {
          case IsOption(isOption) =>
            import isOption.Underlying as Inner
            implicit val AnyT: Type[Any] = DecTypes.Any

            LambdaBuilder
              .of1[Any]("innerValue")
              .traverse { innerValueExpr =>
                deriveDecoderRecursively[Inner](using dctx.nest[Inner](innerValueExpr))
              }
              .map { builder =>
                val decodeFn = builder.build[Inner]
                Rule.matched(Expr.quote {
                  AvroDerivationUtils
                    .decodeOption(Expr.splice(dctx.avroValue), Expr.splice(decodeFn))
                    .asInstanceOf[A]
                })
              }

          case _ =>
            MIO.pure(Rule.yielded(s"The type ${Type[A].prettyPrint} is not an Option"))
        }
      }
  }

  @scala.annotation.nowarn("msg=Infinite loop")
  object DecHandleAsMapRule extends DecoderDerivationRule("handle as map when possible") {

    def apply[A: DecoderCtx]: MIO[Rule.Applicability[Expr[A]]] =
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
    ): MIO[Rule.Applicability[Expr[A]]] = {
      import isMap.{Key, Value, CtorResult}
      implicit val StringT: Type[String] = DecTypes.String
      implicit val AnyT: Type[Any] = DecTypes.Any

      if (!(Key <:< Type[String]))
        MIO.pure(Rule.yielded(s"Map key type ${Key.prettyPrint} is not String"))
      else {
        LambdaBuilder
          .of1[Any]("valueRaw")
          .traverse { valueRawExpr =>
            deriveDecoderRecursively[Value](using dctx.nest[Value](valueRawExpr))
          }
          .map { builder =>
            val decodeFn = builder.build[Value]
            val factoryExpr = isMap.factory
            Rule.matched(Expr.quote {
              AvroDerivationUtils
                .decodeMap(
                  Expr.splice(dctx.avroValue),
                  Expr.splice(decodeFn),
                  Expr
                    .splice(factoryExpr)
                    .asInstanceOf[scala.collection.Factory[(String, Value), A]]
                )
                .asInstanceOf[A]
            })
          }
      }
    }
  }

  object DecHandleAsCollectionRule extends DecoderDerivationRule("handle as collection when possible") {

    def apply[A: DecoderCtx]: MIO[Rule.Applicability[Expr[A]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as a collection") >> {
        Type[A] match {
          case IsCollection(isCollection) =>
            import isCollection.Underlying as Item
            import isCollection.value.CtorResult
            implicit val AnyT: Type[Any] = DecTypes.Any

            LambdaBuilder
              .of1[Any]("itemRaw")
              .traverse { itemRawExpr =>
                deriveDecoderRecursively[Item](using dctx.nest[Item](itemRawExpr))
              }
              .map { builder =>
                val decodeFn = builder.build[Item]
                val factoryExpr = isCollection.value.factory
                Rule.matched(Expr.quote {
                  AvroDerivationUtils
                    .decodeCollection(
                      Expr.splice(dctx.avroValue),
                      Expr.splice(decodeFn),
                      Expr.splice(factoryExpr).asInstanceOf[scala.collection.Factory[Item, A]]
                    )
                    .asInstanceOf[A]
                })
              }

          case _ =>
            MIO.pure(Rule.yielded(s"The type ${Type[A].prettyPrint} is not a collection"))
        }
      }
  }

  object DecHandleAsCaseClassRule extends DecoderDerivationRule("handle as case class when possible") {

    def apply[A: DecoderCtx]: MIO[Rule.Applicability[Expr[A]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as a case class") >> {
        CaseClass.parse[A] match {
          case Some(caseClass) =>
            for {
              _ <- dctx.setHelper[A] { (value, config) =>
                decodeCaseClassFields[A](caseClass)(using dctx.nestInCache(value, config))
              }
              result <- dctx.getHelper[A].flatMap {
                case Some(helperCall) =>
                  MIO.pure(Rule.matched(helperCall(dctx.avroValue, dctx.config)))
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
    ): MIO[Expr[A]] = {
      implicit val StringT: Type[String] = DecTypes.String
      implicit val AnyT: Type[Any] = DecTypes.Any
      implicit val fieldNameT: Type[fieldName] = DecTypes.FieldName
      implicit val transientFieldT: Type[transientField] = DecTypes.TransientField

      // Singletons (case objects, parameterless enum cases) have no primary constructor
      if (caseClass.isSingleton) {
        return caseClass
          .construct[MIO](new CaseClass.ConstructField[MIO] {
            def apply(field: Parameter): MIO[Expr[field.tpe.Underlying]] =
              MIO.fail(
                new RuntimeException(s"Unexpected parameter in singleton ${Type[A].prettyPrint}")
              )
          })
          .flatMap {
            case Some(expr) => MIO.pure(expr)
            case None       => MIO.fail(new RuntimeException(s"Cannot construct ${Type[A].prettyPrint}"))
          }
      }

      val constructor = caseClass.primaryConstructor
      val fieldsList = constructor.parameters.flatten.toList

      // Validate: @transientField on fields without defaults is a compile error
      fieldsList.collectFirst {
        case (name, param) if hasAnnotationType[transientField](param) && !param.hasDefault =>
          s"@transientField on field '$name' of ${Type[A].prettyPrint} requires a default value"
      } match {
        case Some(msg) => return MIO.fail(new RuntimeException(msg))
        case None      => // OK
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
          caseClass
            .construct[MIO](new CaseClass.ConstructField[MIO] {
              def apply(field: Parameter): MIO[Expr[field.tpe.Underlying]] =
                transientDefaults.get(field.name) match {
                  case Some(defaultExpr) =>
                    MIO.pure(defaultExpr.value.asInstanceOf[Expr[field.tpe.Underlying]])
                  case None =>
                    MIO.fail(
                      new RuntimeException(s"Unexpected parameter in zero-argument case class ${Type[A].prettyPrint}")
                    )
                }
            })
            .flatMap {
              case Some(expr) => MIO.pure(expr)
              case None       => MIO.fail(new RuntimeException(s"Cannot construct ${Type[A].prettyPrint}"))
            }

        case Some(fields) =>
          implicit val ArrayAnyT: Type[Array[Any]] = DecTypes.ArrayAny

          val indexedFields = fields.toList.zipWithIndex

          // Step 1: For each non-transient field, derive a decode expression and build accessor
          NonEmptyList
            .fromList(indexedFields)
            .get
            .parTraverse { case ((fName, param), reindex) =>
              import param.tpe.Underlying as Field
              val nameOverride = getAnnotationStringArg[fieldName](param)
              Log.namedScope(s"Deriving decoder for field $fName: ${Type[Field].prettyPrint}") {
                deriveFieldDecoder[Field].map { decoderExpr =>
                  val decodeExpr: Expr[Any] = nameOverride match {
                    case Some(customName) =>
                      Expr.quote {
                        val record = Expr.splice(dctx.avroValue).asInstanceOf[GenericRecord]
                        val fieldValue = AvroDerivationUtils.decodeRecord(
                          record,
                          Expr.splice(Expr(customName))
                        )
                        Expr.splice(decoderExpr).decode(fieldValue): Any
                      }
                    case None =>
                      Expr.quote {
                        val record = Expr.splice(dctx.avroValue).asInstanceOf[GenericRecord]
                        val fieldValue = AvroDerivationUtils.decodeRecord(
                          record,
                          Expr.splice(dctx.config).transformFieldNames(Expr.splice(Expr(fName)))
                        )
                        Expr.splice(decoderExpr).decode(fieldValue): Any
                      }
                  }
                  val makeAccessor: Expr[Array[Any]] => (String, Expr_??) = { arrExpr =>
                    val typedExpr = Expr.quote {
                      AvroDerivationUtils.unsafeCast(
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

              // Step 2: Build List literal from the decode expressions
              val listExpr: Expr[List[Any]] =
                decodeExprs.foldRight(Expr.quote(List.empty[Any])) { (elem, acc) =>
                  Expr.quote(Expr.splice(elem) :: Expr.splice(acc))
                }

              // Step 3: Build the constructor lambda
              LambdaBuilder
                .of1[Array[Any]]("decodedValues")
                .traverse { decodedValuesExpr =>
                  val nonTransientFieldMap: Map[String, Expr_??] =
                    makeAccessors.map(_(decodedValuesExpr)).toMap
                  val fieldMap: Map[String, Expr_??] = nonTransientFieldMap ++ transientDefaults
                  caseClass.primaryConstructor(fieldMap) match {
                    case Right(constructExpr) => MIO.pure(constructExpr)
                    case Left(error)          =>
                      MIO.fail(new RuntimeException(s"Cannot construct ${Type[A].prettyPrint}: $error"))
                  }
                }
                .map { builder =>
                  val constructLambda = builder.build[A]
                  Expr.quote {
                    Expr
                      .splice(constructLambda)
                      .apply(
                        AvroDerivationUtils.sequenceDecodeResults(Expr.splice(listExpr))
                      )
                  }
                }
            }
      }
    }

    @scala.annotation.nowarn("msg=is never used|unused explicit parameter")
    private def deriveFieldDecoder[Field: Type](implicit ctx: DecoderCtx[?]): MIO[Expr[AvroDecoder[Field]]] = {
      implicit val AnyT: Type[Any] = DecTypes.Any

      DecTypes.AvroDecoder[Field].summonExprIgnoring(DecUseImplicitWhenAvailableRule.ignoredImplicits*).toEither match {
        case Right(decoderExpr) =>
          Log.info(s"Found implicit AvroDecoder[${Type[Field].prettyPrint}]") >> MIO.pure(decoderExpr)
        case Left(_) =>
          Log.info(s"Building AvroDecoder[${Type[Field].prettyPrint}] via recursive derivation") >>
            LambdaBuilder
              .of1[Any]("fieldValue")
              .traverse { fieldValueExpr =>
                deriveDecoderRecursively[Field](using ctx.nest[Field](fieldValueExpr))
              }
              .flatMap { builder =>
                val decodeFn = builder.build[Field]
                deriveSelfContainedSchema[Field](ctx.config).map { schemaExpr =>
                  Expr.quote {
                    val sch = Expr.splice(schemaExpr)
                    val fn = Expr.splice(decodeFn)
                    new AvroDecoder[Field] {
                      val schema: Schema = sch
                      def decode(value: Any): Field = fn(value)
                    }
                  }
                }
              }
      }
    }
  }

  object DecHandleAsEnumRule extends DecoderDerivationRule("handle as enum when possible") {

    def apply[A: DecoderCtx]: MIO[Rule.Applicability[Expr[A]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as an enum") >> {
        Enum.parse[A] match {
          case Some(enumm) =>
            for {
              _ <- dctx.setHelper[A] { (value, config) =>
                decodeEnumCases[A](enumm)(using dctx.nestInCache(value, config))
              }
              result <- dctx.getHelper[A].flatMap {
                case Some(helperCall) =>
                  MIO.pure(Rule.matched(helperCall(dctx.avroValue, dctx.config)))
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
    ): MIO[Expr[A]] = {
      implicit val StringT: Type[String] = DecTypes.String
      implicit val AnyT: Type[Any] = DecTypes.Any

      val childrenList = enumm.directChildren.toList

      NonEmptyList.fromList(childrenList) match {
        case None =>
          MIO.fail(new RuntimeException(s"The type ${Type[A].prettyPrint} does not have any children!"))

        case Some(children) =>
          val allCaseObjects = children.toList.forall { case (_, child) =>
            Type.isVal(using child.Underlying) ||
            CaseClass.parse(using child.Underlying).exists(_.primaryConstructor.parameters.flatten.isEmpty)
          }

          if (allCaseObjects) {
            // Pure enum → decode from GenericData.EnumSymbol string
            val knownNames: List[String] = children.toList.map(_._1)

            // Build dispatch chain: if name matches → return case object singleton
            children
              .parTraverse { case (childName, child) =>
                import child.Underlying as ChildType
                Log.namedScope(s"Deriving decoder for enum case $childName") {
                  Expr.singletonOf[ChildType] match {
                    case Some(singleton) =>
                      MIO.pure((childName, singleton.asInstanceOf[Expr[A]]))
                    case None =>
                      // Fallback to construct for non-singleton zero-arg case classes
                      CaseClass.parse[ChildType] match {
                        case Some(cc) =>
                          cc.construct[MIO](new CaseClass.ConstructField[MIO] {
                            def apply(field: Parameter): MIO[Expr[field.tpe.Underlying]] =
                              MIO.fail(
                                new RuntimeException(s"Unexpected parameter in case object $childName")
                              )
                          }).flatMap {
                            case Some(expr) => MIO.pure((childName, expr.asInstanceOf[Expr[A]]))
                            case None       => MIO.fail(new RuntimeException(s"Cannot construct $childName"))
                          }
                        case None =>
                          MIO.fail(new RuntimeException(s"$childName is not parseable as a case class"))
                      }
                  }
                }
              }
              .map { dispatchers =>
                val errorExpr: Expr[A] = Expr.quote {
                  AvroDerivationUtils.failedToMatchSubtype(
                    AvroDerivationUtils.decodeEnumSymbol(Expr.splice(dctx.avroValue)),
                    Expr.splice(Expr(knownNames))
                  )
                }

                dispatchers.toList.foldRight(errorExpr) { case ((childName, childExpr), elseExpr) =>
                  Expr.quote {
                    val name = AvroDerivationUtils.decodeEnumSymbol(Expr.splice(dctx.avroValue))
                    if (Expr.splice(dctx.config).transformConstructorNames(Expr.splice(Expr(childName))) == name)
                      Expr.splice(childExpr)
                    else
                      Expr.splice(elseExpr)
                  }
                }
              }
          } else {
            // Mixed sealed trait → dispatch based on record schema name
            val knownNames: List[String] = children.toList.map(_._1)

            children
              .parTraverse { case (childName, child) =>
                import child.Underlying as ChildType
                Log.namedScope(s"Deriving decoder for enum case $childName: ${Type[ChildType].prettyPrint}") {
                  deriveDecoderRecursively[ChildType](using dctx.nest[ChildType](dctx.avroValue)).flatMap { _ =>
                    dctx.getHelper[ChildType].map {
                      case Some(helper) =>
                        (
                          childName,
                          (valueExpr: Expr[Any], elseExpr: Expr[A]) =>
                            Expr.quote {
                              val record = Expr.splice(valueExpr).asInstanceOf[GenericRecord]
                              val recordName = record.getSchema.getName
                              if (
                                Expr
                                  .splice(dctx.config)
                                  .transformConstructorNames(
                                    Expr.splice(Expr(childName))
                                  ) == recordName
                              )
                                Expr.splice(helper(valueExpr, dctx.config)).asInstanceOf[A]
                              else
                                Expr.splice(elseExpr)
                            }
                        )
                      case None =>
                        (
                          childName,
                          (_: Expr[Any], elseExpr: Expr[A]) => elseExpr
                        )
                    }
                  }
                }
              }
              .map { dispatchers =>
                val errorExpr: Expr[A] = Expr.quote {
                  val record = Expr.splice(dctx.avroValue).asInstanceOf[GenericRecord]
                  AvroDerivationUtils.failedToMatchSubtype(
                    record.getSchema.getName,
                    Expr.splice(Expr(knownNames))
                  )
                }

                dispatchers.toList.foldRight(errorExpr) { case ((_, dispatch), elseExpr) =>
                  dispatch(dctx.avroValue, elseExpr)
                }
              }
          }
      }
    }
  }

  // Types

  private[compiletime] object DecTypes {

    def AvroDecoder: Type.Ctor1[AvroDecoder] = Type.Ctor1.of[AvroDecoder]
    val DecoderLogDerivation: Type[hearth.kindlings.avroderivation.AvroDecoder.LogDerivation] =
      Type.of[hearth.kindlings.avroderivation.AvroDecoder.LogDerivation]
    val Schema: Type[Schema] = Type.of[Schema]
    val AvroConfig: Type[AvroConfig] = Type.of[AvroConfig]
    val String: Type[String] = Type.of[String]
    val Any: Type[Any] = Type.of[Any]
    val ArrayAny: Type[Array[Any]] = Type.of[Array[Any]]
    val FieldName: Type[fieldName] = Type.of[fieldName]
    val TransientField: Type[transientField] = Type.of[transientField]
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
}
