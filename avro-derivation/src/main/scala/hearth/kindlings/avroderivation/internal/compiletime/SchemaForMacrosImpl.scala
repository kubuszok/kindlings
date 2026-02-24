package hearth.kindlings.avroderivation.internal.compiletime

import hearth.MacroCommons
import hearth.fp.data.NonEmptyList
import hearth.fp.effect.*
import hearth.fp.syntax.*
import hearth.std.*

import hearth.kindlings.avroderivation.{AvroConfig, AvroSchemaFor}
import hearth.kindlings.avroderivation.internal.runtime.AvroDerivationUtils
import org.apache.avro.Schema

trait SchemaForMacrosImpl { this: MacroCommons & StdExtensions =>

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
  ): Expr[Out] = Log
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
          SfUseImplicitWhenAvailableRule,
          SfUseBuiltInSupportRule,
          SfHandleAsValueTypeRule,
          SfHandleAsOptionRule,
          SfHandleAsMapRule,
          SfHandleAsCollectionRule,
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
            Log.info(
              s"Failed to derive schema for ${Type[A].prettyPrint}:\n${reasonsStrings.mkString("\n")}"
            ) >>
              MIO.fail(SchemaDerivationError.UnsupportedType(Type[A].prettyPrint, reasonsStrings))
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

    private def builtInSchema[A: SchemaForCtx]: Option[Expr[Schema]] = {
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
      else if (tpe =:= Type.of[BigDecimal])
        Some(Expr.quote(AvroDerivationUtils.stringSchema))
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

  object SfHandleAsCaseClassRule extends SchemaDerivationRule("handle as case class when possible") {

    def apply[A: SchemaForCtx]: MIO[Rule.Applicability[Expr[Schema]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as a case class") >> {
        CaseClass.parse[A] match {
          case Some(caseClass) =>
            for {
              schemaExpr <- deriveCaseClassSchema[A](caseClass)
              _ <- sfctx.setCachedSchema[A](schemaExpr)
              result <- sfctx.getCachedSchema[A].flatMap {
                case Some(cachedSchema) => MIO.pure(Rule.matched(cachedSchema))
                case None               => MIO.pure(Rule.yielded(s"Failed to build helper for ${Type[A].prettyPrint}"))
              }
            } yield result

          case None =>
            MIO.pure(Rule.yielded(s"The type ${Type[A].prettyPrint} is not a case class"))
        }
      }

    @scala.annotation.nowarn("msg=is never used")
    private def deriveCaseClassSchema[A: SchemaForCtx](
        caseClass: CaseClass[A]
    ): MIO[Expr[Schema]] = {
      implicit val SchemaT: Type[Schema] = SfTypes.Schema
      implicit val StringT: Type[String] = SfTypes.String

      val constructor = caseClass.primaryConstructor
      val fieldsList = constructor.parameters.flatten.toList
      val typeName = Type[A].shortName

      NonEmptyList.fromList(fieldsList) match {
        case None =>
          MIO.pure(Expr.quote {
            AvroDerivationUtils.createRecord(
              Expr.splice(Expr(typeName)),
              Expr.splice(sfctx.config).namespace.getOrElse(""),
              java.util.Collections.emptyList[Schema.Field]()
            )
          })
        case Some(fields) =>
          fields
            .parTraverse { case (fieldName, param) =>
              import param.tpe.Underlying as Field
              Log.namedScope(s"Deriving schema for field $fieldName: ${Type[Field].prettyPrint}") {
                deriveSchemaRecursively[Field](using sfctx.nest[Field]).map { fieldSchema =>
                  (fieldName, fieldSchema)
                }
              }
            }
            .map { fieldPairs =>
              val fieldsListExpr = fieldPairs.toList.foldRight(
                Expr.quote(List.empty[(String, Schema)])
              ) { case ((fieldName, fieldSchema), acc) =>
                Expr.quote {
                  (
                    Expr.splice(sfctx.config).transformFieldNames(Expr.splice(Expr(fieldName))),
                    Expr.splice(fieldSchema)
                  ) :: Expr.splice(acc)
                }
              }
              Expr.quote {
                val fields = Expr.splice(fieldsListExpr)
                val javaFields = new java.util.ArrayList[Schema.Field](fields.size)
                fields.foreach { case (name, schema) =>
                  javaFields.add(AvroDerivationUtils.createField(name, schema))
                }
                AvroDerivationUtils.createRecord(
                  Expr.splice(Expr(typeName)),
                  Expr.splice(sfctx.config).namespace.getOrElse(""),
                  javaFields
                )
              }
            }
      }
    }
  }

  object SfHandleAsEnumRule extends SchemaDerivationRule("handle as enum when possible") {

    def apply[A: SchemaForCtx]: MIO[Rule.Applicability[Expr[Schema]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as an enum") >> {
        Enum.parse[A] match {
          case Some(enumm) =>
            for {
              schemaExpr <- deriveEnumSchema[A](enumm)
              _ <- sfctx.setCachedSchema[A](schemaExpr)
              result <- sfctx.getCachedSchema[A].flatMap {
                case Some(cachedSchema) => MIO.pure(Rule.matched(cachedSchema))
                case None               => MIO.pure(Rule.yielded(s"Failed to build helper for ${Type[A].prettyPrint}"))
              }
            } yield result
          case None =>
            MIO.pure(Rule.yielded(s"The type ${Type[A].prettyPrint} is not an enum"))
        }
      }

    @scala.annotation.nowarn("msg=is never used")
    private def deriveEnumSchema[A: SchemaForCtx](
        enumm: Enum[A]
    ): MIO[Expr[Schema]] = {
      implicit val SchemaT: Type[Schema] = SfTypes.Schema

      val childrenList = enumm.directChildren.toList
      val typeName = Type[A].shortName

      NonEmptyList.fromList(childrenList) match {
        case None =>
          MIO.fail(new RuntimeException(s"The type ${Type[A].prettyPrint} does not have any children!"))

        case Some(children) =>
          val allCaseObjects = children.toList.forall { case (_, child) =>
            Type.isVal(using child.Underlying) ||
            CaseClass.parse(using child.Underlying).exists(_.primaryConstructor.parameters.flatten.isEmpty)
          }

          if (allCaseObjects) {
            // Pure enum of case objects → Avro ENUM schema
            val symbolNames = children.toList.map(_._1)
            val symbolsListExpr = symbolNames.foldRight(
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
            children
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
    val String: Type[String] = Type.of[String]
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
}
