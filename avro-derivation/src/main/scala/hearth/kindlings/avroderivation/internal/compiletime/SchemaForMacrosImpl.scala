package hearth.kindlings.avroderivation.internal.compiletime

import hearth.MacroCommons
import hearth.fp.data.NonEmptyList
import hearth.fp.effect.*
import hearth.fp.syntax.*
import hearth.std.*

import hearth.kindlings.avroderivation.{AvroConfig, AvroSchemaFor, DecimalConfig}
import hearth.kindlings.avroderivation.annotations.{
  avroAlias,
  avroDefault,
  avroDoc,
  avroError,
  avroFixed,
  avroNamespace,
  avroProp,
  avroSortPriority,
  fieldName,
  transientField
}
import hearth.kindlings.avroderivation.internal.runtime.AvroDerivationUtils
import org.apache.avro.Schema

trait SchemaForMacrosImpl { this: MacroCommons & StdExtensions & AnnotationSupport =>

  // Entrypoints

  def deriveInlineSchema[A: Type](configExpr: Expr[AvroConfig]): Expr[Schema] = {
    implicit val SchemaT: Type[Schema] = SfTypes.Schema
    implicit val ConfigT: Type[AvroConfig] = SfTypes.AvroConfig

    deriveSchemaFromCtxAndAdaptForEntrypoint[A, Schema]("AvroSchemaFor.schemaOf") { fromCtx =>
      ValDefs.createVal[AvroConfig](configExpr).use { configVal =>
        Expr.quote {
          val _ = Expr.splice(configVal)
          Expr.splice(fromCtx(SchemaForCtx.from[A](configVal, derivedType = None)))
        }
      }
    }
  }

  @scala.annotation.nowarn("msg=is never used")
  def deriveSchemaForTypeClass[A: Type](configExpr: Expr[AvroConfig]): Expr[AvroSchemaFor[A]] = {
    implicit val AvroSchemaForA: Type[AvroSchemaFor[A]] = SfTypes.AvroSchemaFor[A]
    implicit val SchemaT: Type[Schema] = SfTypes.Schema
    implicit val ConfigT: Type[AvroConfig] = SfTypes.AvroConfig
    val selfType: Option[??] = Some(Type[A].as_??)

    deriveSchemaFromCtxAndAdaptForEntrypoint[A, AvroSchemaFor[A]]("AvroSchemaFor.derived") { fromCtx =>
      ValDefs.createVal[AvroConfig](configExpr).use { configVal =>
        Expr.quote {
          val cfg = Expr.splice(configVal)
          new AvroSchemaFor[A] {
            val schema: Schema = Expr.splice {
              fromCtx(SchemaForCtx.from[A](Expr.quote(cfg), derivedType = selfType))
            }
          }
        }
      }
    }
  }

  // Handles logging, error reporting and prepending "cached" defs and vals to the result.

  def deriveSchemaFromCtxAndAdaptForEntrypoint[A: Type, Out: Type](macroName: String)(
      provideCtxAndAdapt: (SchemaForCtx[A] => Expr[Schema]) => Expr[Out]
  ): Expr[Out] = {
    if (Type[A] =:= Type.of[Nothing].asInstanceOf[Type[A]] || Type[A] =:= Type.of[Any].asInstanceOf[Type[A]])
      Environment.reportErrorAndAbort(
        s"$macroName: type parameter was inferred as ${Type[A].prettyPrint}, which is likely unintended.\n" +
          s"Provide an explicit type parameter, e.g.: $macroName[MyType](...)\n" +
          "or add a type ascription to the result variable."
      )
    Log
      .namedScope(
        s"Deriving schema for ${Type[A].prettyPrint} at: ${Environment.currentPosition.prettyPrint}"
      ) {
        MIO.scoped { runSafe =>
          val fromCtx: (SchemaForCtx[A] => Expr[Schema]) = (ctx: SchemaForCtx[A]) =>
            runSafe {
              for {
                _ <- Environment.loadStandardExtensions().toMIO(allowFailures = false)
                result <- deriveSchemaRecursively[A](using ctx)
                cache <- ctx.cache.get
              } yield cache.toValDefs.use(_ => result)
            }

          provideCtxAndAdapt(fromCtx)
        }
      }
      .flatTap { result =>
        Log.info(s"Derived final schema result: ${result.prettyPrint}")
      }
      .runToExprOrFail(
        macroName,
        infoRendering = if (shouldWeLogSchemaDerivation) RenderFrom(Log.Level.Info) else DontRender,
        errorRendering = if (shouldWeLogSchemaDerivation) RenderFrom(Log.Level.Info) else DontRender
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
          "Enable debug logging with: import hearth.kindlings.avroderivation.debug.logDerivationForAvroSchemaFor or scalac option -Xmacro-settings:avroDerivation.logDerivation=true"
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

  def shouldWeLogSchemaDerivation: Boolean = {
    implicit val LogDerivation: Type[AvroSchemaFor.LogDerivation] = SfTypes.SchemaForLogDerivation
    def logDerivationImported = Expr.summonImplicit[AvroSchemaFor.LogDerivation].isDefined

    def logDerivationSetGlobally = (for {
      data <- Environment.typedSettings.toOption
      avroDerivation <- data.get("avroDerivation")
      shouldLog <- avroDerivation.get("logDerivation").flatMap(_.asBoolean)
    } yield shouldLog).getOrElse(false)

    logDerivationImported || logDerivationSetGlobally
  }

  // Context

  final case class SchemaForCtx[A](
      tpe: Type[A],
      config: Expr[AvroConfig],
      cache: MLocal[ValDefsCache],
      derivedType: Option[??]
  ) {

    def nest[B: Type]: SchemaForCtx[B] = copy[B](
      tpe = Type[B]
    )

    def getCachedSchema[B: Type]: MIO[Option[Expr[Schema]]] = {
      implicit val SchemaT: Type[Schema] = SfTypes.Schema
      cache.get0Ary[Schema](s"cached-schema-for-${Type[B].shortName}")
    }
    def setCachedSchema[B: Type](instance: Expr[Schema]): MIO[Unit] = {
      implicit val SchemaT: Type[Schema] = SfTypes.Schema
      Log.info(s"Caching schema for ${Type[B].prettyPrint}") >>
        cache.buildCachedWith(
          s"cached-schema-for-${Type[B].shortName}",
          ValDefBuilder.ofLazy[Schema](s"schema_${Type[B].shortName}")
        )(_ => instance)
    }

    override def toString: String =
      s"schemaFor[${tpe.prettyPrint}](config = ${config.prettyPrint})"
  }
  object SchemaForCtx {

    def from[A: Type](
        config: Expr[AvroConfig],
        derivedType: Option[??]
    ): SchemaForCtx[A] = SchemaForCtx(
      tpe = Type[A],
      config = config,
      cache = ValDefsCache.mlocal,
      derivedType = derivedType
    )
  }

  def sfctx[A](implicit A: SchemaForCtx[A]): SchemaForCtx[A] = A

  implicit def currentSchemaValueType[A: SchemaForCtx]: Type[A] = sfctx.tpe

  abstract class SchemaDerivationRule(val name: String) extends Rule {
    def apply[A: SchemaForCtx]: MIO[Rule.Applicability[Expr[Schema]]]
  }

  /** Derives a schema within a shared cache, for use by encoder/decoder derivation. */
  def deriveSchemaInSharedScope[B: Type](config: Expr[AvroConfig], cache: MLocal[ValDefsCache]): MIO[Expr[Schema]] = {
    implicit val ctx: SchemaForCtx[B] = SchemaForCtx(
      tpe = Type[B],
      config = config,
      cache = cache,
      derivedType = None
    )
    deriveSchemaRecursively[B]
  }

  /** Derives schema with its own local cache, returning a self-contained expression. Use this instead of
    * deriveInlineSchema when calling from within an encoder/decoder MIO chain to avoid Scala 3 splice isolation issues.
    */
  def deriveSelfContainedSchema[B: Type](config: Expr[AvroConfig]): MIO[Expr[Schema]] = {
    val localCache = ValDefsCache.mlocal
    val ctx = SchemaForCtx[B](Type[B], config, localCache, derivedType = None)
    for {
      _ <- Environment.loadStandardExtensions().toMIO(allowFailures = false)
      result <- deriveSchemaRecursively[B](using ctx)
      cache <- localCache.get
    } yield cache.toValDefs.use(_ => result)
  }

  // The actual derivation logic

  def deriveSchemaRecursively[A: SchemaForCtx]: MIO[Expr[Schema]] =
    Log
      .namedScope(s"Deriving schema for type ${Type[A].prettyPrint}") {
        Rules(
          SfUseCachedDefWhenAvailableRule,
          SfHandleAsLiteralTypeRule,
          SfUseImplicitWhenAvailableRule,
          SfUseBuiltInSupportRule,
          SfHandleAsValueTypeRule,
          SfHandleAsOptionRule,
          SfHandleAsEitherRule,
          SfHandleAsMapRule,
          SfHandleAsCollectionRule,
          SfHandleAsNamedTupleRule,
          SfHandleAsSingletonRule,
          SfHandleAsCaseClassRule,
          SfHandleAsEnumRule
        )(_[A]).flatMap {
          case Right(result) =>
            Log.info(s"Derived schema for ${Type[A].prettyPrint}: ${result.prettyPrint}") >>
              MIO.pure(result)
          case Left(reasons) =>
            val reasonsStrings = reasons.toListMap
              .removed(SfUseCachedDefWhenAvailableRule)
              .view
              .map { case (rule, reasons) =>
                if (reasons.isEmpty) s"The rule ${rule.name} was not applicable"
                else
                  s" - The rule ${rule.name} was not applicable, for the following reasons: ${reasons.mkString(", ")}"
              }
              .toList
            val err = SchemaDerivationError.UnsupportedType(Type[A].prettyPrint, reasonsStrings)
            Log.error(err.message) >> MIO.fail(err)
        }
      }

  // Rules

  object SfUseCachedDefWhenAvailableRule extends SchemaDerivationRule("use cached def when available") {

    def apply[A: SchemaForCtx]: MIO[Rule.Applicability[Expr[Schema]]] =
      Log.info(s"Attempting to use cached schema for ${Type[A].prettyPrint}") >>
        sfctx.getCachedSchema[A].flatMap {
          case Some(cachedSchema) =>
            Log.info(s"Found cached schema for ${Type[A].prettyPrint}") >>
              MIO.pure(Rule.matched(cachedSchema))
          case None =>
            MIO.pure(Rule.yielded(s"The type ${Type[A].prettyPrint} does not have a cached schema"))
        }
  }

  object SfUseImplicitWhenAvailableRule extends SchemaDerivationRule("use implicit when available") {

    lazy val ignoredImplicits: Seq[UntypedMethod] =
      Type.of[AvroSchemaFor.type].methods.collect {
        case method if method.value.name == "derived" => method.value.asUntyped
      }

    def apply[A: SchemaForCtx]: MIO[Rule.Applicability[Expr[Schema]]] =
      Log.info(s"Attempting to use implicit AvroSchemaFor for ${Type[A].prettyPrint}") >> {
        if (sfctx.derivedType.exists(_.Underlying =:= Type[A]))
          MIO.pure(
            Rule.yielded(s"The type ${Type[A].prettyPrint} is the type being derived, skipping implicit search")
          )
        else
          SfTypes.AvroSchemaFor[A].summonExprIgnoring(ignoredImplicits*).toEither match {
            case Right(instanceExpr) =>
              Log.info(s"Found implicit AvroSchemaFor ${instanceExpr.prettyPrint}, using directly") >>
                MIO.pure(Rule.matched(Expr.quote {
                  Expr.splice(instanceExpr).schema
                }))
            case Left(reason) =>
              MIO.pure(
                Rule.yielded(
                  s"The type ${Type[A].prettyPrint} does not have an implicit AvroSchemaFor instance: $reason"
                )
              )
          }
      }
  }

  object SfHandleAsLiteralTypeRule extends SchemaDerivationRule("handle as literal type when possible") {

    def apply[A: SchemaForCtx]: MIO[Rule.Applicability[Expr[Schema]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as a literal type") >> {
        implicit val SchemaT: Type[Schema] = SfTypes.Schema
        extractLiteralSchema[A] match {
          case Some(expr) => MIO.pure(Rule.matched(expr))
          case None       => MIO.pure(Rule.yielded(s"The type ${Type[A].prettyPrint} is not a literal type"))
        }
      }

    private def extractLiteralSchema[A: SchemaForCtx](implicit SchemaT: Type[Schema]): Option[Expr[Schema]] =
      Type.StringCodec.fromType(Type[A]).map { _ =>
        Expr.quote(AvroDerivationUtils.stringSchema)
      } orElse Type.IntCodec.fromType(Type[A]).map { _ =>
        Expr.quote(AvroDerivationUtils.intSchema)
      } orElse Type.LongCodec.fromType(Type[A]).map { _ =>
        Expr.quote(AvroDerivationUtils.longSchema)
      } orElse Type.DoubleCodec.fromType(Type[A]).map { _ =>
        Expr.quote(AvroDerivationUtils.doubleSchema)
      } orElse Type.FloatCodec.fromType(Type[A]).map { _ =>
        Expr.quote(AvroDerivationUtils.floatSchema)
      } orElse Type.BooleanCodec.fromType(Type[A]).map { _ =>
        Expr.quote(AvroDerivationUtils.booleanSchema)
      } orElse Type.ShortCodec.fromType(Type[A]).map { _ =>
        Expr.quote(AvroDerivationUtils.intSchema)
      } orElse Type.ByteCodec.fromType(Type[A]).map { _ =>
        Expr.quote(AvroDerivationUtils.intSchema)
      } orElse Type.CharCodec.fromType(Type[A]).map { _ =>
        Expr.quote(AvroDerivationUtils.stringSchema)
      }
  }

  object SfUseBuiltInSupportRule extends SchemaDerivationRule("use built-in support for primitives") {
    implicit val SchemaT: Type[Schema] = SfTypes.Schema

    def apply[A: SchemaForCtx]: MIO[Rule.Applicability[Expr[Schema]]] =
      Log.info(s"Attempting to use built-in support for ${Type[A].prettyPrint}") >> {
        builtInSchema[A] match {
          case Some(schemaExpr) =>
            Log.info(s"Found built-in schema for ${Type[A].prettyPrint}") >>
              MIO.pure(Rule.matched(schemaExpr))
          case None =>
            MIO.pure(Rule.yielded(s"The type ${Type[A].prettyPrint} is not a built-in type"))
        }
      }

    @scala.annotation.nowarn("msg=is never used")
    private def builtInSchema[A: SchemaForCtx]: Option[Expr[Schema]] = {
      implicit val AvroConfigT: Type[AvroConfig] = SfTypes.AvroConfig
      implicit val DecimalConfigT: Type[DecimalConfig] = SfTypes.DecimalConfig
      val tpe = Type[A]
      if (tpe =:= Type.of[Boolean])
        Some(Expr.quote(AvroDerivationUtils.booleanSchema))
      else if (tpe =:= Type.of[Byte])
        Some(Expr.quote(AvroDerivationUtils.intSchema))
      else if (tpe =:= Type.of[Short])
        Some(Expr.quote(AvroDerivationUtils.intSchema))
      else if (tpe =:= Type.of[Int])
        Some(Expr.quote(AvroDerivationUtils.intSchema))
      else if (tpe =:= Type.of[Long])
        Some(Expr.quote(AvroDerivationUtils.longSchema))
      else if (tpe =:= Type.of[Float])
        Some(Expr.quote(AvroDerivationUtils.floatSchema))
      else if (tpe =:= Type.of[Double])
        Some(Expr.quote(AvroDerivationUtils.doubleSchema))
      else if (tpe =:= Type.of[Char])
        Some(Expr.quote(AvroDerivationUtils.stringSchema))
      else if (tpe =:= Type.of[String])
        Some(Expr.quote(AvroDerivationUtils.stringSchema))
      else if (tpe =:= Type.of[Array[Byte]])
        Some(Expr.quote(AvroDerivationUtils.bytesSchema))
      else if (tpe =:= Type.of[java.nio.ByteBuffer])
        Some(Expr.quote(AvroDerivationUtils.bytesSchema))
      else if (tpe =:= Type.of[BigDecimal])
        Some(Expr.quote {
          Expr.splice(sfctx.config).decimalConfig match {
            case Some(dc) => AvroDerivationUtils.decimalSchema(dc.precision, dc.scale)
            case None     => AvroDerivationUtils.stringSchema
          }
        })
      else if (tpe =:= Type.of[java.util.UUID])
        Some(Expr.quote(AvroDerivationUtils.uuidSchema))
      else if (tpe =:= Type.of[java.time.Instant])
        Some(Expr.quote(AvroDerivationUtils.timestampMillisSchema))
      else if (tpe =:= Type.of[java.time.LocalDate])
        Some(Expr.quote(AvroDerivationUtils.dateSchema))
      else if (tpe =:= Type.of[java.time.LocalTime])
        Some(Expr.quote(AvroDerivationUtils.timeMicrosSchema))
      else if (tpe =:= Type.of[java.time.LocalDateTime])
        Some(Expr.quote(AvroDerivationUtils.timestampMillisSchema))
      else
        None
    }
  }

  object SfHandleAsValueTypeRule extends SchemaDerivationRule("handle as value type when possible") {

    def apply[A: SchemaForCtx]: MIO[Rule.Applicability[Expr[Schema]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as a value type") >> {
        Type[A] match {
          case IsValueType(isValueType) =>
            import isValueType.Underlying as Inner
            for {
              innerResult <- deriveSchemaRecursively[Inner](using sfctx.nest[Inner])
            } yield Rule.matched(innerResult)

          case _ =>
            MIO.pure(Rule.yielded(s"The type ${Type[A].prettyPrint} is not a value type"))
        }
      }
  }

  object SfHandleAsOptionRule extends SchemaDerivationRule("handle as Option when possible") {
    implicit val SchemaT: Type[Schema] = SfTypes.Schema

    def apply[A: SchemaForCtx]: MIO[Rule.Applicability[Expr[Schema]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as Option") >> {
        Type[A] match {
          case IsOption(isOption) =>
            import isOption.Underlying as Inner
            for {
              innerSchema <- deriveSchemaRecursively[Inner](using sfctx.nest[Inner])
            } yield Rule.matched(Expr.quote {
              AvroDerivationUtils.createUnion(
                AvroDerivationUtils.nullSchema,
                Expr.splice(innerSchema)
              )
            })

          case _ =>
            MIO.pure(Rule.yielded(s"The type ${Type[A].prettyPrint} is not an Option"))
        }
      }
  }

  object SfHandleAsEitherRule extends SchemaDerivationRule("handle as Either when possible") {
    implicit val SchemaT: Type[Schema] = SfTypes.Schema

    def apply[A: SchemaForCtx]: MIO[Rule.Applicability[Expr[Schema]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as Either") >> {
        Type[A] match {
          case IsEither(isEither) =>
            import isEither.{LeftValue, RightValue}
            for {
              leftSchema <- deriveSchemaRecursively[LeftValue](using sfctx.nest[LeftValue])
              rightSchema <- deriveSchemaRecursively[RightValue](using sfctx.nest[RightValue])
            } yield Rule.matched(Expr.quote {
              Schema.createUnion(
                Expr.splice(leftSchema),
                Expr.splice(rightSchema)
              )
            })

          case _ =>
            MIO.pure(Rule.yielded(s"The type ${Type[A].prettyPrint} is not an Either"))
        }
      }
  }

  @scala.annotation.nowarn("msg=Infinite loop")
  object SfHandleAsMapRule extends SchemaDerivationRule("handle as map when possible") {
    implicit val SchemaT: Type[Schema] = SfTypes.Schema
    implicit val StringT: Type[String] = SfTypes.String

    def apply[A: SchemaForCtx]: MIO[Rule.Applicability[Expr[Schema]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as a map") >> {
        Type[A] match {
          case IsMap(isMap) =>
            import isMap.Underlying as Pair
            deriveMapSchema[A, Pair](isMap.value)

          case _ =>
            MIO.pure(Rule.yielded(s"The type ${Type[A].prettyPrint} is not a map"))
        }
      }

    private def deriveMapSchema[A: SchemaForCtx, Pair: Type](
        isMap: IsMapOf[A, Pair]
    ): MIO[Rule.Applicability[Expr[Schema]]] = {
      import isMap.{Key, Value}
      if (!(Key <:< Type[String]))
        MIO.pure(Rule.yielded(s"Map key type ${Key.prettyPrint} is not String"))
      else {
        for {
          valueSchema <- deriveSchemaRecursively[Value](using sfctx.nest[Value])
        } yield Rule.matched(Expr.quote {
          Schema.createMap(Expr.splice(valueSchema))
        })
      }
    }
  }

  object SfHandleAsCollectionRule extends SchemaDerivationRule("handle as collection when possible") {
    implicit val SchemaT: Type[Schema] = SfTypes.Schema

    def apply[A: SchemaForCtx]: MIO[Rule.Applicability[Expr[Schema]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as a collection") >> {
        Type[A] match {
          case IsCollection(isCollection) =>
            import isCollection.Underlying as Item
            for {
              itemSchema <- deriveSchemaRecursively[Item](using sfctx.nest[Item])
            } yield Rule.matched(Expr.quote {
              Schema.createArray(Expr.splice(itemSchema))
            })

          case _ =>
            MIO.pure(Rule.yielded(s"The type ${Type[A].prettyPrint} is not a collection"))
        }
      }
  }

  object SfHandleAsNamedTupleRule extends SchemaDerivationRule("handle as named tuple when possible") {

    def apply[A: SchemaForCtx]: MIO[Rule.Applicability[Expr[Schema]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as a named tuple") >> {
        NamedTuple.parse[A].toEither match {
          case Right(namedTuple) =>
            for {
              schemaExpr <- deriveNamedTupleSchema[A](namedTuple.primaryConstructor)
              _ <- sfctx.setCachedSchema[A](schemaExpr)
              result <- sfctx.getCachedSchema[A].flatMap {
                case Some(cachedSchema) => MIO.pure(Rule.matched(cachedSchema))
                case None               => MIO.pure(Rule.yielded(s"Failed to build helper for ${Type[A].prettyPrint}"))
              }
            } yield result
          case Left(reason) =>
            MIO.pure(Rule.yielded(reason))
        }
      }

    @scala.annotation.nowarn("msg=is never used")
    private def deriveNamedTupleSchema[A: SchemaForCtx](
        constructor: Method.NoInstance[A]
    ): MIO[Expr[Schema]] = {
      implicit val SchemaT: Type[Schema] = SfTypes.Schema
      implicit val StringT: Type[String] = SfTypes.String
      implicit val AvroConfigT: Type[AvroConfig] = SfTypes.AvroConfig

      val fields = constructor.parameters.flatten.toList
      val typeName = Type[A].shortName

      NonEmptyList.fromList(fields) match {
        case None =>
          MIO.pure(Expr.quote {
            AvroDerivationUtils.createRecord(
              Expr.splice(Expr(typeName)),
              Expr.splice(sfctx.config).namespace.getOrElse(""),
              java.util.Collections.emptyList[Schema.Field]()
            )
          })
        case Some(fieldValues) =>
          fieldValues
            .parTraverse { case (fName, param) =>
              import param.tpe.Underlying as Field
              Log.namedScope(s"Deriving schema for named tuple field $fName: ${Type[Field].prettyPrint}") {
                deriveSchemaRecursively[Field](using sfctx.nest[Field]).map { fieldSchema =>
                  (fName, fieldSchema)
                }
              }
            }
            .map { fieldPairs =>
              val javaFieldsExpr = fieldPairs.toList.foldRight(
                Expr.quote(List.empty[Schema.Field])
              ) { case ((fName, fieldSchema), acc) =>
                val nameExpr: Expr[String] = Expr.quote {
                  Expr.splice(sfctx.config).transformFieldNames(Expr.splice(Expr(fName)))
                }
                val fieldExpr: Expr[Schema.Field] = Expr.quote {
                  AvroDerivationUtils.createField(
                    Expr.splice(nameExpr),
                    Expr.splice(fieldSchema)
                  )
                }
                Expr.quote(Expr.splice(fieldExpr) :: Expr.splice(acc))
              }
              val fieldsExpr = Expr.quote {
                val fieldsList = Expr.splice(javaFieldsExpr)
                val javaFields = new java.util.ArrayList[Schema.Field](fieldsList.size)
                fieldsList.foreach(javaFields.add)
                (javaFields: java.util.List[Schema.Field])
              }
              Expr.quote {
                AvroDerivationUtils.createRecord(
                  Expr.splice(Expr(typeName)),
                  Expr.splice(sfctx.config).namespace.getOrElse(""),
                  Expr.splice(fieldsExpr)
                )
              }
            }
      }
    }
  }

  object SfHandleAsSingletonRule extends SchemaDerivationRule("handle as singleton when possible") {

    @scala.annotation.nowarn("msg=is never used")
    def apply[A: SchemaForCtx]: MIO[Rule.Applicability[Expr[Schema]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as a singleton") >> {
        SingletonValue.parse[A].toEither match {
          case Right(_) =>
            implicit val SchemaT: Type[Schema] = SfTypes.Schema
            implicit val StringT: Type[String] = SfTypes.String
            implicit val AvroConfigT: Type[AvroConfig] = SfTypes.AvroConfig
            val typeName = Type[A].shortName
            val schemaExpr = Expr.quote {
              AvroDerivationUtils.createRecord(
                Expr.splice(Expr(typeName)),
                Expr.splice(sfctx.config).namespace.getOrElse(""),
                java.util.Collections.emptyList[Schema.Field]()
              )
            }
            for {
              _ <- sfctx.setCachedSchema[A](schemaExpr)
              result <- sfctx.getCachedSchema[A].flatMap {
                case Some(cachedSchema) => MIO.pure(Rule.matched(cachedSchema))
                case None               => MIO.pure(Rule.yielded(s"Failed to cache schema for ${Type[A].prettyPrint}"))
              }
            } yield result
          case Left(reason) =>
            MIO.pure(Rule.yielded(reason))
        }
      }
  }

  object SfHandleAsCaseClassRule extends SchemaDerivationRule("handle as case class when possible") {

    def apply[A: SchemaForCtx]: MIO[Rule.Applicability[Expr[Schema]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as a case class") >> {
        CaseClass.parse[A].toEither match {
          case Right(caseClass) =>
            for {
              schemaExpr <- deriveCaseClassSchema[A](caseClass)
              _ <- sfctx.setCachedSchema[A](schemaExpr)
              result <- sfctx.getCachedSchema[A].flatMap {
                case Some(cachedSchema) => MIO.pure(Rule.matched(cachedSchema))
                case None               => MIO.pure(Rule.yielded(s"Failed to build helper for ${Type[A].prettyPrint}"))
              }
            } yield result

          case Left(reason) =>
            MIO.pure(Rule.yielded(reason))
        }
      }

    @scala.annotation.nowarn("msg=is never used")
    private def deriveCaseClassSchema[A: SchemaForCtx](
        caseClass: CaseClass[A]
    ): MIO[Expr[Schema]] = {
      implicit val SchemaT: Type[Schema] = SfTypes.Schema
      implicit val StringT: Type[String] = SfTypes.String
      implicit val fieldNameT: Type[fieldName] = SfTypes.FieldName
      implicit val transientFieldT: Type[transientField] = SfTypes.TransientField
      implicit val avroDocT: Type[avroDoc] = SfTypes.AvroDoc
      implicit val avroNamespaceT: Type[avroNamespace] = SfTypes.AvroNamespace
      implicit val avroDefaultT: Type[avroDefault] = SfTypes.AvroDefault
      implicit val avroFixedT: Type[avroFixed] = SfTypes.AvroFixed
      implicit val avroErrorT: Type[avroError] = SfTypes.AvroError
      implicit val avroPropT: Type[avroProp] = SfTypes.AvroProp
      implicit val avroAliasT: Type[avroAlias] = SfTypes.AvroAlias
      implicit val AvroConfigT: Type[AvroConfig] = SfTypes.AvroConfig

      // Read class-level annotations
      val classDoc: Option[String] = getTypeAnnotationStringArg[avroDoc, A]
      val classNamespace: Option[String] = getTypeAnnotationStringArg[avroNamespace, A]
      val isError: Boolean = hasTypeAnnotation[avroError, A]
      val classProps: List[(String, String)] = getAllTypeAnnotationTwoStringArgs[avroProp, A]
      val classAliases: List[String] = getAllTypeAnnotationStringArgs[avroAlias, A]

      val constructor = caseClass.primaryConstructor
      val fieldsList = constructor.parameters.flatten.toList
      val typeName = Type[A].shortName

      // Validate: @transientField on fields without defaults is a compile error
      fieldsList.collectFirst {
        case (name, param) if hasAnnotationType[transientField](param) && !param.hasDefault => name
      } match {
        case Some(name) =>
          val err = SchemaDerivationError.TransientFieldMissingDefault(name, Type[A].prettyPrint)
          return Log.error(err.message) >> MIO.fail(err)
        case None => // OK
      }

      // Filter out transient fields
      val nonTransientFields = fieldsList.filter { case (_, param) => !hasAnnotationType[transientField](param) }

      NonEmptyList.fromList(nonTransientFields) match {
        case None =>
          MIO.pure(
            createRecordExpr(
              typeName,
              classDoc,
              classNamespace,
              isError,
              classProps,
              classAliases,
              Expr.quote {
                java.util.Collections.emptyList[Schema.Field]()
              },
              sfctx.config
            )
          )
        case Some(fields) =>
          fields
            .parTraverse { case (fName, param) =>
              import param.tpe.Underlying as Field
              val nameOverride = getAnnotationStringArg[fieldName](param)
              val fieldDoc = getAnnotationStringArg[avroDoc](param)
              val fieldDefault = getAnnotationStringArg[avroDefault](param)
              val avroFixedSize = getAnnotationIntArg[avroFixed](param)
              val fieldProps = getAllAnnotationTwoStringArgs[avroProp](param)
              val fieldAliases = getAllAnnotationStringArgs[avroAlias](param)
              Log.namedScope(s"Deriving schema for field $fName: ${Type[Field].prettyPrint}") {
                avroFixedSize match {
                  case Some(_) if !(Type[Field] =:= Type.of[Array[Byte]]) =>
                    val err = SchemaDerivationError.AvroFixedOnNonByteArray(
                      fName,
                      Type[A].prettyPrint,
                      Type[Field].prettyPrint
                    )
                    Log.error(err.message) >> MIO.fail(err)
                  case Some(size) =>
                    val fixedName = nameOverride.getOrElse(fName)
                    MIO.pure {
                      val fieldSchema = Expr.quote {
                        AvroDerivationUtils.createFixed(
                          Expr.splice(Expr(fixedName)),
                          Expr.splice(sfctx.config).namespace.getOrElse(""),
                          Expr.splice(Expr(size))
                        )
                      }
                      (fName, fieldSchema, nameOverride, fieldDoc, fieldDefault, fieldProps, fieldAliases)
                    }
                  case None =>
                    deriveSchemaRecursively[Field](using sfctx.nest[Field]).map { fieldSchema =>
                      (fName, fieldSchema, nameOverride, fieldDoc, fieldDefault, fieldProps, fieldAliases)
                    }
                }
              }
            }
            .map { fieldPairs =>
              val javaFieldsExpr = fieldPairs.toList.foldRight(
                Expr.quote(List.empty[Schema.Field])
              ) {
                case (
                      (fName, fieldSchema, nameOverride, fieldDoc, fieldDefault, fieldProps, fieldAliases),
                      acc
                    ) =>
                  val nameExpr: Expr[String] = nameOverride match {
                    case Some(customName) => Expr(customName)
                    case None             =>
                      Expr.quote {
                        Expr.splice(sfctx.config).transformFieldNames(Expr.splice(Expr(fName)))
                      }
                  }
                  val baseFieldExpr: Expr[Schema.Field] = (fieldDoc, fieldDefault) match {
                    case (Some(doc), Some(default)) =>
                      Expr.quote {
                        AvroDerivationUtils.createFieldWithDocAndDefault(
                          Expr.splice(nameExpr),
                          Expr.splice(fieldSchema),
                          Expr.splice(Expr(doc)),
                          Expr.splice(Expr(default))
                        )
                      }
                    case (Some(doc), None) =>
                      Expr.quote {
                        AvroDerivationUtils.createFieldWithDoc(
                          Expr.splice(nameExpr),
                          Expr.splice(fieldSchema),
                          Expr.splice(Expr(doc))
                        )
                      }
                    case (None, Some(default)) =>
                      Expr.quote {
                        AvroDerivationUtils.createFieldWithDefault(
                          Expr.splice(nameExpr),
                          Expr.splice(fieldSchema),
                          Expr.splice(Expr(default))
                        )
                      }
                    case (None, None) =>
                      Expr.quote {
                        AvroDerivationUtils.createField(
                          Expr.splice(nameExpr),
                          Expr.splice(fieldSchema)
                        )
                      }
                  }
                  // Apply field-level @avroProp annotations
                  val fieldWithPropsExpr: Expr[Schema.Field] =
                    if (fieldProps.isEmpty) baseFieldExpr
                    else {
                      val propsListExpr = fieldProps.foldRight(Expr.quote(List.empty[(String, String)])) {
                        case ((k, v), listAcc) =>
                          Expr.quote((Expr.splice(Expr(k)), Expr.splice(Expr(v))) :: Expr.splice(listAcc))
                      }
                      Expr.quote {
                        val f = Expr.splice(baseFieldExpr)
                        Expr.splice(propsListExpr).foreach { case (k, v) =>
                          AvroDerivationUtils.addFieldProp(f, k, v)
                        }
                        f
                      }
                    }
                  // Apply field-level @avroAlias annotations
                  val fieldExpr: Expr[Schema.Field] =
                    if (fieldAliases.isEmpty) fieldWithPropsExpr
                    else {
                      val aliasesListExpr = fieldAliases.foldRight(Expr.quote(List.empty[String])) { (alias, listAcc) =>
                        Expr.quote(Expr.splice(Expr(alias)) :: Expr.splice(listAcc))
                      }
                      Expr.quote {
                        val f = Expr.splice(fieldWithPropsExpr)
                        Expr.splice(aliasesListExpr).foreach(a => AvroDerivationUtils.addFieldAlias(f, a))
                        f
                      }
                    }
                  Expr.quote(Expr.splice(fieldExpr) :: Expr.splice(acc))
              }
              val fieldsExpr = Expr.quote {
                val fieldsList = Expr.splice(javaFieldsExpr)
                val javaFields = new java.util.ArrayList[Schema.Field](fieldsList.size)
                fieldsList.foreach(javaFields.add)
                (javaFields: java.util.List[Schema.Field])
              }
              createRecordExpr(
                typeName,
                classDoc,
                classNamespace,
                isError,
                classProps,
                classAliases,
                fieldsExpr,
                sfctx.config
              )
            }
      }
    }

    private def createRecordExpr(
        typeName: String,
        classDoc: Option[String],
        classNamespace: Option[String],
        isError: Boolean,
        classProps: List[(String, String)],
        classAliases: List[String],
        fieldsExpr: Expr[java.util.List[Schema.Field]],
        configExpr: Expr[AvroConfig]
    )(implicit SchemaT: Type[Schema], StringT: Type[String], AvroConfigT: Type[AvroConfig]): Expr[Schema] = {
      val baseExpr: Expr[Schema] = (classDoc, classNamespace, isError) match {
        case (Some(doc), Some(ns), true) =>
          Expr.quote {
            AvroDerivationUtils.createRecordWithDocError(
              Expr.splice(Expr(typeName)),
              Expr.splice(Expr(ns)),
              Expr.splice(Expr(doc)),
              Expr.splice(fieldsExpr)
            )
          }
        case (Some(doc), Some(ns), false) =>
          Expr.quote {
            AvroDerivationUtils.createRecordWithDoc(
              Expr.splice(Expr(typeName)),
              Expr.splice(Expr(ns)),
              Expr.splice(Expr(doc)),
              Expr.splice(fieldsExpr)
            )
          }
        case (Some(doc), None, true) =>
          Expr.quote {
            AvroDerivationUtils.createRecordWithDocError(
              Expr.splice(Expr(typeName)),
              Expr.splice(configExpr).namespace.getOrElse(""),
              Expr.splice(Expr(doc)),
              Expr.splice(fieldsExpr)
            )
          }
        case (Some(doc), None, false) =>
          Expr.quote {
            AvroDerivationUtils.createRecordWithDoc(
              Expr.splice(Expr(typeName)),
              Expr.splice(configExpr).namespace.getOrElse(""),
              Expr.splice(Expr(doc)),
              Expr.splice(fieldsExpr)
            )
          }
        case (None, Some(ns), true) =>
          Expr.quote {
            AvroDerivationUtils.createRecordError(
              Expr.splice(Expr(typeName)),
              Expr.splice(Expr(ns)),
              Expr.splice(fieldsExpr)
            )
          }
        case (None, Some(ns), false) =>
          Expr.quote {
            AvroDerivationUtils.createRecord(
              Expr.splice(Expr(typeName)),
              Expr.splice(Expr(ns)),
              Expr.splice(fieldsExpr)
            )
          }
        case (None, None, true) =>
          Expr.quote {
            AvroDerivationUtils.createRecordError(
              Expr.splice(Expr(typeName)),
              Expr.splice(configExpr).namespace.getOrElse(""),
              Expr.splice(fieldsExpr)
            )
          }
        case (None, None, false) =>
          Expr.quote {
            AvroDerivationUtils.createRecord(
              Expr.splice(Expr(typeName)),
              Expr.splice(configExpr).namespace.getOrElse(""),
              Expr.splice(fieldsExpr)
            )
          }
      }
      // Apply class-level @avroProp annotations
      val withPropsExpr: Expr[Schema] =
        if (classProps.isEmpty) baseExpr
        else {
          val propsListExpr = classProps.foldRight(Expr.quote(List.empty[(String, String)])) { case ((k, v), listAcc) =>
            Expr.quote((Expr.splice(Expr(k)), Expr.splice(Expr(v))) :: Expr.splice(listAcc))
          }
          Expr.quote {
            val s = Expr.splice(baseExpr)
            Expr.splice(propsListExpr).foreach { case (k, v) =>
              AvroDerivationUtils.addSchemaProp(s, k, v)
            }
            s
          }
        }
      // Apply class-level @avroAlias annotations
      if (classAliases.isEmpty) withPropsExpr
      else {
        val aliasesListExpr = classAliases.foldRight(Expr.quote(List.empty[String])) { (alias, listAcc) =>
          Expr.quote(Expr.splice(Expr(alias)) :: Expr.splice(listAcc))
        }
        Expr.quote {
          val s = Expr.splice(withPropsExpr)
          Expr.splice(aliasesListExpr).foreach(a => AvroDerivationUtils.addSchemaAlias(s, a))
          s
        }
      }
    }
  }

  object SfHandleAsEnumRule extends SchemaDerivationRule("handle as enum when possible") {

    def apply[A: SchemaForCtx]: MIO[Rule.Applicability[Expr[Schema]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as an enum") >> {
        Enum.parse[A].toEither match {
          case Right(enumm) =>
            for {
              schemaExpr <- deriveEnumSchema[A](enumm)
              _ <- sfctx.setCachedSchema[A](schemaExpr)
              result <- sfctx.getCachedSchema[A].flatMap {
                case Some(cachedSchema) => MIO.pure(Rule.matched(cachedSchema))
                case None               => MIO.pure(Rule.yielded(s"Failed to build helper for ${Type[A].prettyPrint}"))
              }
            } yield result
          case Left(reason) =>
            MIO.pure(Rule.yielded(reason))
        }
      }

    @scala.annotation.nowarn("msg=is never used")
    private def deriveEnumSchema[A: SchemaForCtx](
        enumm: Enum[A]
    ): MIO[Expr[Schema]] = {
      implicit val SchemaT: Type[Schema] = SfTypes.Schema
      implicit val avroSortPriorityT: Type[avroSortPriority] = SfTypes.AvroSortPriority

      val childrenList = enumm.directChildren.toList
      val typeName = Type[A].shortName

      NonEmptyList.fromList(childrenList) match {
        case None =>
          val err = SchemaDerivationError.NoChildrenInSealedTrait(Type[A].prettyPrint)
          Log.error(err.message) >> MIO.fail(err)

        case Some(children) =>
          val allCaseObjects = Type[A].isEnumeration || Type[A].isJavaEnum ||
            children.toList.forall { case (_, child) =>
              SingletonValue.unapply(child.Underlying).isDefined
            }

          if (allCaseObjects) {
            // Pure enum of case objects → Avro ENUM schema
            // Sort by @avroSortPriority
            val symbolsWithPriority = children.toList.map { case (name, child) =>
              import child.Underlying as ChildType
              val priority = getTypeAnnotationIntArg[avroSortPriority, ChildType]
              (name, priority.getOrElse(0))
            }
            val sortedSymbolNames = symbolsWithPriority.sortBy(_._2).map(_._1)
            val symbolsListExpr = sortedSymbolNames.foldRight(
              Expr.quote(List.empty[String])
            ) { (name, acc) =>
              Expr.quote {
                Expr.splice(sfctx.config).transformConstructorNames(Expr.splice(Expr(name))) ::
                  Expr.splice(acc)
              }
            }
            MIO.pure(Expr.quote {
              val symbols = Expr.splice(symbolsListExpr)
              val javaSymbols = new java.util.ArrayList[String](symbols.size)
              symbols.foreach(javaSymbols.add)
              AvroDerivationUtils.createEnum(
                Expr.splice(Expr(typeName)),
                Expr.splice(sfctx.config).namespace.getOrElse(""),
                javaSymbols
              )
            })
          } else {
            // Mixed sealed trait → Avro UNION of record schemas
            // Sort children by @avroSortPriority
            val childrenWithPriority = children.toList.map { case (name, child) =>
              import child.Underlying as ChildType
              val priority = getTypeAnnotationIntArg[avroSortPriority, ChildType]
              (name, child, priority.getOrElse(0))
            }
            val sortedList = childrenWithPriority.sortBy(_._3).map(t => (t._1, t._2))
            val sortedChildren = NonEmptyList(sortedList.head, sortedList.tail)
            sortedChildren
              .parTraverse { case (_, child) =>
                import child.Underlying as ChildType
                Log.namedScope(s"Deriving schema for enum case ${Type[ChildType].prettyPrint}") {
                  deriveSchemaRecursively[ChildType](using sfctx.nest[ChildType])
                }
              }
              .map { childSchemas =>
                val schemasListExpr = childSchemas.toList.foldRight(
                  Expr.quote(List.empty[Schema])
                ) { (childSchema, acc) =>
                  Expr.quote(Expr.splice(childSchema) :: Expr.splice(acc))
                }
                Expr.quote {
                  val schemas = Expr.splice(schemasListExpr)
                  val javaSchemas = new java.util.ArrayList[Schema](schemas.size)
                  schemas.foreach(javaSchemas.add)
                  Schema.createUnion(javaSchemas)
                }
              }
          }
      }
    }
  }

  // Types

  private[compiletime] object SfTypes {

    def AvroSchemaFor: Type.Ctor1[AvroSchemaFor] = Type.Ctor1.of[AvroSchemaFor]
    val SchemaForLogDerivation: Type[hearth.kindlings.avroderivation.AvroSchemaFor.LogDerivation] =
      Type.of[hearth.kindlings.avroderivation.AvroSchemaFor.LogDerivation]
    val Schema: Type[Schema] = Type.of[Schema]
    val AvroConfig: Type[AvroConfig] = Type.of[AvroConfig]
    val DecimalConfig: Type[DecimalConfig] = Type.of[DecimalConfig]
    val String: Type[String] = Type.of[String]
    val FieldName: Type[fieldName] = Type.of[fieldName]
    val TransientField: Type[transientField] = Type.of[transientField]
    val AvroDoc: Type[avroDoc] = Type.of[avroDoc]
    val AvroNamespace: Type[avroNamespace] = Type.of[avroNamespace]
    val AvroDefault: Type[avroDefault] = Type.of[avroDefault]
    val AvroFixed: Type[avroFixed] = Type.of[avroFixed]
    val AvroError: Type[avroError] = Type.of[avroError]
    val AvroProp: Type[avroProp] = Type.of[avroProp]
    val AvroAlias: Type[avroAlias] = Type.of[avroAlias]
    val AvroSortPriority: Type[avroSortPriority] = Type.of[avroSortPriority]
  }
}

sealed private[compiletime] trait SchemaDerivationError
    extends util.control.NoStackTrace
    with Product
    with Serializable {
  def message: String
  override def getMessage(): String = message
}
private[compiletime] object SchemaDerivationError {
  final case class UnsupportedType(tpeName: String, reasons: List[String]) extends SchemaDerivationError {
    override def message: String =
      s"The type $tpeName was not handled by any schema derivation rule:\n${reasons.mkString("\n")}"
  }
  final case class TransientFieldMissingDefault(fieldName: String, tpeName: String) extends SchemaDerivationError {
    override def message: String =
      s"@transientField on field '$fieldName' of $tpeName requires a default value"
  }
  final case class NoChildrenInSealedTrait(tpeName: String) extends SchemaDerivationError {
    override def message: String =
      s"The type $tpeName does not have any children!"
  }
  final case class AvroFixedOnNonByteArray(fieldName: String, tpeName: String, fieldType: String)
      extends SchemaDerivationError {
    override def message: String =
      s"@avroFixed on field '$fieldName' of $tpeName requires Array[Byte], but field type is $fieldType"
  }
}
