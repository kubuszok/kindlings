package hearth.kindlings.tapirschemaderivation.internal.compiletime

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

import hearth.kindlings.jsonschemaconfigs.{JsonSchemaConfigExtension, JsonSchemaConfigs}
import hearth.kindlings.tapirschemaderivation.{KindlingsSchema, PreferSchemaConfig}
import hearth.kindlings.tapirschemaderivation.internal.runtime.TapirSchemaUtils
import sttp.tapir.{Schema, SchemaType}
import sttp.tapir.Schema.SName

trait SchemaMacrosImpl { this: MacroCommons & StdExtensions & JsonSchemaConfigs & AnnotationSupport =>

  // Type helpers

  object TsTypes {
    def TapirSchemaOf[A: Type]: Type[Schema[A]] = Type.of[Schema[A]]
    def KindlingsSchemaOf[A: Type]: Type[KindlingsSchema[A]] = Type.of[KindlingsSchema[A]]
    lazy val KindlingsSchemaLogDerivation: Type[KindlingsSchema.LogDerivation] =
      Type.of[KindlingsSchema.LogDerivation]
    lazy val SNameType: Type[SName] = Type.of[SName]
    lazy val SchemaTypeUtils: Type[TapirSchemaUtils.type] = Type.of[TapirSchemaUtils.type]
    lazy val PreferSchemaConfigCtor: Type.Ctor1[PreferSchemaConfig] = Type.Ctor1.of[PreferSchemaConfig]
    lazy val AnyType: Type[Any] = Type.of[Any]
    lazy val ListAnyType: Type[List[Any]] = Type.of[List[Any]]
    lazy val StringType: Type[String] = Type.of[String]
  }

  // Methods to ignore during implicit search — prevents triggering expensive auto-derivation

  private lazy val ignoredImplicits: Seq[UntypedMethod] = {
    val ours = Type.of[KindlingsSchema.type].methods.collect {
      case method if method.value.name == "derived" => method.value.asUntyped
    }
    val tapirSchema = Type.of[Schema.type].methods.collect {
      case method if method.value.name == "derivedSchema" => method.value.asUntyped
    }
    ours ++ tapirSchema
  }

  // Entrypoints

  def deriveSchema[A: Type]: Expr[Schema[A]] = {
    implicit val SchemaA: Type[Schema[A]] = TsTypes.TapirSchemaOf[A]

    deriveFromCtxAndAdaptForEntrypoint[A, Schema[A]]("KindlingsSchema.derive", derivedType = None) { fromCtx =>
      fromCtx()
    }
  }

  @scala.annotation.nowarn("msg=is never used")
  def deriveKindlingsSchema[A: Type]: Expr[KindlingsSchema[A]] = {
    implicit val schemaAType: Type[Schema[A]] = TsTypes.TapirSchemaOf[A]
    implicit val kindlingsSchemaAType: Type[KindlingsSchema[A]] = TsTypes.KindlingsSchemaOf[A]
    val selfType: Option[??] = Some(Type[A].as_??)

    deriveFromCtxAndAdaptForEntrypoint[A, KindlingsSchema[A]]("KindlingsSchema.derived", derivedType = selfType) {
      fromCtx =>
        Expr.quote {
          new KindlingsSchema[A] {
            val schema: Schema[A] = Expr.splice(fromCtx())
          }
        }
    }
  }

  // Core entrypoint

  private def deriveFromCtxAndAdaptForEntrypoint[A: Type, Out: Type](macroName: String, derivedType: Option[??])(
      provideCtxAndAdapt: (() => Expr[Schema[A]]) => Expr[Out]
  ): Expr[Out] = {
    if (Type[A] =:= Type.of[Nothing].asInstanceOf[Type[A]] || Type[A] =:= Type.of[Any].asInstanceOf[Type[A]])
      Environment.reportErrorAndAbort(
        s"$macroName: type parameter was inferred as ${Type[A].prettyPrint}, which is likely unintended.\n" +
          s"Provide an explicit type parameter, e.g.: $macroName[MyType]\n" +
          "or add a type ascription to the result variable."
      )
    Log
      .namedScope(
        s"Deriving tapir Schema for ${Type[A].prettyPrint} at: ${Environment.currentPosition.prettyPrint}"
      ) {
        MIO.scoped { runSafe =>
          val fromCtx: () => Expr[Schema[A]] = () => {
            val cache: MLocal[ValDefsCache] = ValDefsCache.mlocal
            val inProgress: MLocal[Set[String]] =
              MLocal(Set.empty[String])(identity)((a, b) => a ++ b)
            runSafe {
              for {
                _ <- Environment.loadStandardExtensions().toMIO(allowFailures = false)
                extensionResult <- MIO(Environment.loadMacroExtensions[JsonSchemaConfigExtension])
                _ <- extensionResult.toMIO(allowFailures = true)
                jsonCfg <- MIO(resolveJsonConfig(macroName))
                result <- deriveSchemaRecursively[A](jsonCfg, cache, inProgress, derivedType)
                cacheState <- cache.get
              } yield cacheState.toValDefs.use(_ => result)
            }
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
            val msg = Option(e.getMessage).getOrElse(e.getClass.getName)
            msg.split("\n").toList match {
              case head :: tail => (("  - " + head) :: tail.map("    " + _)).mkString("\n")
              case _            => "  - " + msg
            }
          }
          .mkString("\n")
        val hint =
          "Enable debug logging with: import hearth.kindlings.tapirschemaderivation.debug.logDerivationForKindlingsSchema or scalac option -Xmacro-settings:tapirSchemaDerivation.logDerivation=true"
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

  // Private sealed trait used as a phantom type parameter for UntypedType.toTyped
  // when probing PreferSchemaConfig[X]. This ensures no other Type[ConfigTypeWitness]
  // exists in implicit scope to conflict with our locally constructed one.
  sealed private trait ConfigTypeWitness

  private def resolveJsonConfig(macroName: String): JsonSchemaConfig =
    JsonSchemaConfig.all match {
      case Nil =>
        Environment.reportErrorAndAbort(
          s"$macroName: No JSON library configuration found in implicit scope.\n" +
            "KindlingsSchema requires a circe Configuration or jsoniter JsoniterConfig " +
            "to ensure the schema matches JSON encoding.\n" +
            "Add e.g.: implicit val config: Configuration = Configuration.default"
        )
      case List(single) => single
      case multiple     =>
        // Multiple configs found — try to disambiguate using PreferSchemaConfig[X]
        val ctor = TsTypes.PreferSchemaConfigCtor
        val preferred = multiple.filter { provider =>
          // Build Type[PreferSchemaConfig[ConfigType]] where ConfigType is the provider's config type.
          // ConfigTypeWitness is a phantom — the actual compiler type comes from provider.configType.
          implicit val cfgType: Type[ConfigTypeWitness] = UntypedType.toTyped[ConfigTypeWitness](provider.configType)
          implicit val pscType: Type[PreferSchemaConfig[ConfigTypeWitness]] = ctor[ConfigTypeWitness]
          Expr.summonImplicit[PreferSchemaConfig[ConfigTypeWitness]].isDefined
        }
        preferred match {
          case List(single) => single
          case Nil          =>
            val libs = multiple.map(_.libraryName).mkString(", ")
            Environment.reportErrorAndAbort(
              s"$macroName: Multiple JSON library configurations found: $libs.\n" +
                "Add an implicit PreferSchemaConfig to select which one to use for the schema, e.g.:\n" +
                multiple
                  .map(p => s"  implicit val prefer: PreferSchemaConfig[${p.libraryName}Config] = PreferSchemaConfig()")
                  .mkString("\n")
            )
          case tooMany =>
            val libs = tooMany.map(_.libraryName).mkString(", ")
            Environment.reportErrorAndAbort(
              s"$macroName: Multiple PreferSchemaConfig instances found for: $libs.\n" +
                "Only one PreferSchemaConfig should be in implicit scope."
            )
        }
    }

  private def shouldWeLogSchemaDerivation: Boolean = {
    implicit val LogDerivation: Type[KindlingsSchema.LogDerivation] = TsTypes.KindlingsSchemaLogDerivation
    def logDerivationImported = Expr.summonImplicit[KindlingsSchema.LogDerivation].isDefined

    def logDerivationSetGlobally = (for {
      data <- Environment.typedSettings.toOption
      tapirSchemaDerivation <- data.get("tapirSchemaDerivation")
      shouldLog <- tapirSchemaDerivation.get("logDerivation").flatMap(_.asBoolean)
    } yield shouldLog).getOrElse(false)

    logDerivationImported || logDerivationSetGlobally
  }

  // Schema derivation — main recursive entry point

  private def deriveSchemaRecursively[A: Type](
      jsonCfg: JsonSchemaConfig,
      cache: MLocal[ValDefsCache],
      inProgress: MLocal[Set[String]],
      derivedType: Option[??]
  ): MIO[Expr[Schema[A]]] =
    Log.namedScope(s"deriveSchemaRecursively[${Type[A].prettyPrint}]") {
      implicit val SchemaA: Type[Schema[A]] = TsTypes.TapirSchemaOf[A]
      val key = cacheKey[A]

      // 1. Check cache (previously derived type — reuse)
      cache.get0Ary[Schema[A]](key).flatMap {
        case Some(cached) =>
          Log.info(s"Using cached Schema for ${Type[A].prettyPrint}") >>
            MIO.pure(cached)
        case None =>
          // 2. Check in-progress set — recursive reference → SRef
          inProgress.get.flatMap { inProgressSet =>
            if (inProgressSet.contains(key)) {
              @scala.annotation.nowarn("msg=is never used")
              def emitSRef: MIO[Expr[Schema[A]]] = {
                implicit val sNameT: Type[SName] = TsTypes.SNameType
                implicit val utilsT: Type[TapirSchemaUtils.type] = TsTypes.SchemaTypeUtils
                val sNameExpr = computeSNameExpr[A](derivedType)
                MIO.pure(Expr.quote {
                  TapirSchemaUtils.refSchema[A](Expr.splice(sNameExpr))
                })
              }
              Log.info(s"Recursive reference detected for ${Type[A].prettyPrint}, emitting SRef") >>
                emitSRef
            } else {
              // 3. Try to summon existing implicit Schema[A], ignoring auto-derivation
              trySummonSchemaIgnoring[A](derivedType).flatMap {
                case Some(implicitExpr) =>
                  Log.info(s"Using summoned implicit Schema for ${Type[A].prettyPrint}") >>
                    setCachedAndGet[A](cache, implicitExpr)
                case None =>
                  // 4. Structural derivation rules
                  deriveStructurally[A](jsonCfg, cache, inProgress, inProgressSet, derivedType)
              }
            }
          }
      }
    }

  // Structural derivation — tries Option, Map, Collection, Singleton, CaseClass, Enum in order

  @scala.annotation.nowarn("msg=is never used")
  private def deriveStructurally[A: Type](
      jsonCfg: JsonSchemaConfig,
      cache: MLocal[ValDefsCache],
      inProgress: MLocal[Set[String]],
      inProgressSet: Set[String],
      derivedType: Option[??]
  ): MIO[Expr[Schema[A]]] = {
    implicit val SchemaA: Type[Schema[A]] = TsTypes.TapirSchemaOf[A]
    implicit val Utils: Type[TapirSchemaUtils.type] = TsTypes.SchemaTypeUtils
    implicit val anyType: Type[Any] = TsTypes.AnyType

    Type[A] match {
      // Option[E]
      case IsOption(isOption) =>
        import isOption.Underlying as Element
        Log.info(s"Deriving Schema for Option element: ${Type[Element].prettyPrint}") >>
          deriveSchemaRecursively[Element](jsonCfg, cache, inProgress, derivedType).map { elementSchema =>
            Expr.quote {
              TapirSchemaUtils.optionSchema[Element](Expr.splice(elementSchema)).asInstanceOf[Schema[A]]
            }
          }

      // Map[K, V] — checked before Collection because Map <: Iterable
      case IsMap(isMap) =>
        import isMap.Underlying as Pair
        deriveMapSchemaInner[A, Pair](isMap.value, jsonCfg, cache, inProgress, derivedType)

      // Collection (List, Vector, Set, Array, etc.)
      case IsCollection(isCollection) =>
        import isCollection.Underlying as Element
        Log.info(s"Deriving Schema for collection element: ${Type[Element].prettyPrint}") >>
          deriveSchemaRecursively[Element](jsonCfg, cache, inProgress, derivedType).map { elementSchema =>
            Expr.quote {
              TapirSchemaUtils.collectionSchema[Element](Expr.splice(elementSchema)).asInstanceOf[Schema[A]]
            }
          }

      case _ =>
        // Singleton (case object / val) — before CaseClass, produces product schema with no fields
        SingletonValue.parse[A].toEither match {
          case Right(_) =>
            Log.info(s"Deriving Schema for singleton ${Type[A].prettyPrint}") >>
              (for {
                _ <- inProgress.set(inProgressSet + cacheKey[A])
                result <- deriveSingletonSchema[A](jsonCfg, cache, derivedType)
                _ <- inProgress.set(inProgressSet)
              } yield result)

          case Left(_) =>
            // CaseClass
            CaseClass.parse[A].toEither match {
              case Right(cc) =>
                Log.info(s"Deriving Schema for case class ${Type[A].prettyPrint}") >>
                  (for {
                    _ <- inProgress.set(inProgressSet + cacheKey[A])
                    result <- deriveCaseClassSchema[A](cc, jsonCfg, cache, inProgress, derivedType)
                    _ <- inProgress.set(inProgressSet)
                  } yield result)

              case Left(_) =>
                // Enum / sealed trait
                Enum.parse[A].toEither match {
                  case Right(e) =>
                    Log.info(s"Deriving Schema for enum/sealed ${Type[A].prettyPrint}") >>
                      (for {
                        _ <- inProgress.set(inProgressSet + cacheKey[A])
                        result <- deriveEnumSchema[A](e, jsonCfg, cache, inProgress, derivedType)
                        _ <- inProgress.set(inProgressSet)
                      } yield result)

                  case Left(reason) =>
                    MIO.fail(
                      new Exception(
                        s"Cannot derive tapir Schema for ${Type[A].prettyPrint}: " +
                          s"no implicit Schema[T] found and type is not a singleton, case class, sealed trait/enum, Option, collection, or map. " +
                          s"Last reason: $reason"
                      )
                    )
                }
            }
        }
    }
  }

  @scala.annotation.nowarn("msg=is never used")
  private def deriveMapSchemaInner[A: Type, Pair: Type](
      isMap: IsMapOf[A, Pair],
      jsonCfg: JsonSchemaConfig,
      cache: MLocal[ValDefsCache],
      inProgress: MLocal[Set[String]],
      derivedType: Option[??]
  ): MIO[Expr[Schema[A]]] = {
    import isMap.{Key, Value}
    implicit val anyType: Type[Any] = TsTypes.AnyType
    implicit val Utils: Type[TapirSchemaUtils.type] = TsTypes.SchemaTypeUtils
    implicit val stringT: Type[String] = TsTypes.StringType

    if (!(Key <:< Type[String]))
      MIO.fail(
        new Exception(
          s"Cannot derive tapir Schema for Map with non-String key type ${Key.prettyPrint}"
        )
      )
    else {
      Log.info(s"Deriving Schema for map value: ${Type[Value].prettyPrint}") >>
        deriveSchemaRecursively[Value](jsonCfg, cache, inProgress, derivedType).map { valueSchema =>
          Expr.quote {
            TapirSchemaUtils.mapSchema[Value](Expr.splice(valueSchema)).asInstanceOf[Schema[A]]
          }
        }
    }
  }

  // Implicit schema summoning — uses summonExprIgnoring to skip auto-derivation

  private def trySummonSchemaIgnoring[A: Type](derivedType: Option[??]): MIO[Option[Expr[Schema[A]]]] = MIO {
    if (derivedType.exists(_.Underlying =:= Type[A])) None
    else {
      implicit val SchemaA: Type[Schema[A]] = TsTypes.TapirSchemaOf[A]
      Type[Schema[A]].summonExprIgnoring(ignoredImplicits*).toEither match {
        case Right(expr) => Some(expr)
        case Left(_)     => None
      }
    }
  }

  // Cache operations

  private def cacheKey[A: Type]: String =
    s"tapir-schema-for-${Type[A].prettyPrint}"

  private def cacheName[A: Type]: String =
    s"tapirSchema_${Type[A].shortName}"

  private def setCachedAndGet[A: Type](
      cache: MLocal[ValDefsCache],
      instance: Expr[Schema[A]]
  ): MIO[Expr[Schema[A]]] = {
    val key = cacheKey[A]
    val name = cacheName[A]
    implicit val SchemaA: Type[Schema[A]] = TsTypes.TapirSchemaOf[A]
    cache.buildCachedWith(key, ValDefBuilder.ofLazy[Schema[A]](name))(_ => instance) >>
      cache.get0Ary[Schema[A]](key).flatMap {
        case Some(ref) => MIO.pure(ref)
        case None      => MIO.pure(instance) // fallback, should not happen
      }
  }

  // Singleton derivation (case object / val — product schema with no fields)

  @scala.annotation.nowarn("msg=is never used|unused explicit parameter")
  private def deriveSingletonSchema[A: Type](
      jsonCfg: JsonSchemaConfig,
      cache: MLocal[ValDefsCache],
      derivedType: Option[??]
  ): MIO[Expr[Schema[A]]] = {
    implicit val SchemaA: Type[Schema[A]] = TsTypes.TapirSchemaOf[A]
    implicit val SNameT: Type[SName] = TsTypes.SNameType
    implicit val Utils: Type[TapirSchemaUtils.type] = TsTypes.SchemaTypeUtils

    val sNameExpr = computeSNameExpr[A](derivedType)
    val typeAnnsExpr: Expr[List[Any]] = typeAnnotationsExpr[A]

    val rawSchemaExpr = Expr.quote {
      TapirSchemaUtils.productSchema[A](Expr.splice(sNameExpr), TapirSchemaUtils.emptyFieldList[A])
    }
    val schemaExpr = Expr.quote {
      TapirSchemaUtils.enrichSchema[A](Expr.splice(rawSchemaExpr), Expr.splice(typeAnnsExpr))
    }
    setCachedAndGet[A](cache, schemaExpr)
  }

  // Case class derivation

  @scala.annotation.nowarn("msg=is never used")
  private def deriveCaseClassSchema[A: Type](
      cc: CaseClass[A],
      jsonCfg: JsonSchemaConfig,
      cache: MLocal[ValDefsCache],
      inProgress: MLocal[Set[String]],
      derivedType: Option[??]
  ): MIO[Expr[Schema[A]]] = {
    implicit val SchemaA: Type[Schema[A]] = TsTypes.TapirSchemaOf[A]
    implicit val SNameT: Type[SName] = TsTypes.SNameType
    implicit val Utils: Type[TapirSchemaUtils.type] = TsTypes.SchemaTypeUtils

    val paramsList = cc.primaryConstructor.parameters.flatten.toList
    val activeParams = paramsList.filterNot { case (_, p) => jsonCfg.isTransientField(p) }
    val sNameExpr = computeSNameExpr[A](derivedType)

    // Derive field schemas
    val fieldsResult: MIO[List[Expr[SchemaType.SProductField[A]]]] =
      activeParams.foldLeft(MIO.pure(List.empty[Expr[SchemaType.SProductField[A]]])) { case (acc, (fName, param)) =>
        acc.flatMap { results =>
          deriveFieldExpr[A](fName, param, jsonCfg, cache, inProgress, derivedType).map(results :+ _)
        }
      }

    val typeAnnsExpr: Expr[List[Any]] = typeAnnotationsExpr[A]

    for {
      fieldExprs <- fieldsResult
      fieldsListExpr = fieldExprs.foldRight(
        Expr.quote(TapirSchemaUtils.emptyFieldList[A])
      ) { (fieldExpr, acc) =>
        Expr.quote(Expr.splice(fieldExpr) :: Expr.splice(acc))
      }
      rawSchemaExpr = Expr.quote {
        TapirSchemaUtils.productSchema[A](Expr.splice(sNameExpr), Expr.splice(fieldsListExpr))
      }
      schemaExpr = Expr.quote {
        TapirSchemaUtils.enrichSchema[A](Expr.splice(rawSchemaExpr), Expr.splice(typeAnnsExpr))
      }
      result <- setCachedAndGet[A](cache, schemaExpr)
    } yield result
  }

  private def deriveFieldExpr[A: Type](
      fieldName: String,
      param: Parameter,
      jsonCfg: JsonSchemaConfig,
      cache: MLocal[ValDefsCache],
      inProgress: MLocal[Set[String]],
      derivedType: Option[??]
  ): MIO[Expr[SchemaType.SProductField[A]]] = {
    import param.tpe.Underlying as Field

    val fieldIndex = param.index
    val encodedNameExpr: Expr[String] = jsonCfg.resolveFieldName(param, fieldName)
    val annsExpr: Expr[List[Any]] = fieldAnnotationsExpr(param)

    for {
      fieldSchema <- deriveSchemaRecursively[Field](jsonCfg, cache, inProgress, derivedType)
    } yield Expr.quote {
      TapirSchemaUtils.productFieldWithAnnotations[A](
        Expr.splice(Expr(fieldName)),
        Expr.splice(encodedNameExpr),
        Expr.splice(fieldSchema).asInstanceOf[Schema[Any]],
        Expr.splice(Expr(fieldIndex)),
        Expr.splice(annsExpr)
      )
    }
  }

  // Enum / sealed trait derivation

  @scala.annotation.nowarn("msg=is never used")
  private def deriveEnumSchema[A: Type](
      e: Enum[A],
      jsonCfg: JsonSchemaConfig,
      cache: MLocal[ValDefsCache],
      inProgress: MLocal[Set[String]],
      derivedType: Option[??]
  ): MIO[Expr[Schema[A]]] = {
    implicit val SchemaA: Type[Schema[A]] = TsTypes.TapirSchemaOf[A]
    implicit val SNameT: Type[SName] = TsTypes.SNameType
    implicit val Utils: Type[TapirSchemaUtils.type] = TsTypes.SchemaTypeUtils

    val sNameExpr = computeSNameExpr[A](derivedType)
    val discriminatorExpr: Expr[Option[String]] = jsonCfg.discriminatorFieldName
    val children = e.directChildren.toList
    val typeAnnsExpr: Expr[List[Any]] = typeAnnotationsExpr[A]

    // Derive schemas for each child, casting to Schema[Any] inside the quote
    // because Schema is invariant and we need a homogeneous list
    val childSchemasResult: MIO[List[Expr[Schema[Any]]]] =
      children.foldLeft(MIO.pure(List.empty[Expr[Schema[Any]]])) { case (acc, (_, child)) =>
        acc.flatMap { results =>
          import child.Underlying as ChildType
          deriveSchemaRecursively[ChildType](jsonCfg, cache, inProgress, derivedType).map { childSchemaExpr =>
            results :+ childSchemaExpr.asInstanceOf[Expr[Schema[Any]]]
          }
        }
      }

    for {
      childSchemaExprs <- childSchemasResult
      // Build subtypes list
      subtypesListExpr = childSchemaExprs.foldRight(
        Expr.quote(Nil: List[Schema[Any]])
      ) { (schemaExpr, acc) =>
        Expr.quote(Expr.splice(schemaExpr).asInstanceOf[Schema[Any]] :: Expr.splice(acc))
      }
      rawSchemaExpr = Expr.quote {
        TapirSchemaUtils.coproductSchema[A](
          Expr.splice(sNameExpr),
          Expr.splice(subtypesListExpr),
          Expr.splice(discriminatorExpr)
        )
      }
      schemaExpr = Expr.quote {
        TapirSchemaUtils.enrichSchema[A](Expr.splice(rawSchemaExpr), Expr.splice(typeAnnsExpr))
      }
      result <- setCachedAndGet[A](cache, schemaExpr)
    } yield result
  }

  // SName computation

  @scala.annotation.nowarn("msg=is never used")
  private def computeSNameExpr[A: Type](derivedType: Option[??]): Expr[SName] = {
    implicit val SNameT: Type[SName] = TsTypes.SNameType
    implicit val Utils: Type[TapirSchemaUtils.type] = TsTypes.SchemaTypeUtils
    val fullNameExpr: Expr[String] = Type[A].runtimePlainPrint { tpe =>
      import tpe.Underlying
      // Guard against self-summoning:
      // 1. SName for type A can never depend on Schema[A] (we're building it)
      // 2. Don't summon Schema for the top-level derived type (prevents circular dependency
      //    when `given Schema[X] = KindlingsSchema.derived[X]` summons itself)
      if (tpe.Underlying =:= Type[A]) None
      else if (derivedType.exists(_.Underlying =:= tpe.Underlying)) None
      else {
        implicit val SchemaUnderlying: Type[Schema[tpe.Underlying]] = TsTypes.TapirSchemaOf[tpe.Underlying]
        Type[Schema[tpe.Underlying]].summonExprIgnoring(ignoredImplicits*).toEither match {
          case Right(schemaExpr) =>
            val fallback = Expr(Type.plainPrint[tpe.Underlying])
            Some(Expr.quote {
              Expr.splice(schemaExpr).name.map(_.show).getOrElse(Expr.splice(fallback))
            })
          case Left(_) => None
        }
      }
    }
    Expr.quote(TapirSchemaUtils.parseSName(Expr.splice(fullNameExpr)))
  }

  // Annotation helpers

  private def collectAnnotationsExpr(annotations: List[UntypedExpr]): Expr[List[Any]] = {
    implicit val anyType: Type[Any] = TsTypes.AnyType
    implicit val listAnyType: Type[List[Any]] = TsTypes.ListAnyType
    annotations.foldRight(Expr.quote(List.empty[Any]: List[Any])) { (ann, acc) =>
      val typedAnn: Expr[Any] = ann.asTyped[Any]
      Expr.quote(Expr.splice(typedAnn) :: Expr.splice(acc))
    }
  }

  private def fieldAnnotationsExpr(param: Parameter): Expr[List[Any]] =
    collectAnnotationsExpr(allParamAnnotations(param))

  private def typeAnnotationsExpr[A: Type]: Expr[List[Any]] =
    collectAnnotationsExpr(allTypeAnnotations[A])
}
