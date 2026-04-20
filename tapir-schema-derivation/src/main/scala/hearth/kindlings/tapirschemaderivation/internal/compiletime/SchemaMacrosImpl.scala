package hearth.kindlings.tapirschemaderivation.internal.compiletime

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

import hearth.kindlings.jsonschemaconfigs.{JsonSchemaConfigExtension, JsonSchemaConfigs}
import hearth.kindlings.tapirschemaderivation.{KindlingsSchema, PreferSchemaConfig}
import hearth.kindlings.tapirschemaderivation.internal.runtime.TapirSchemaUtils
import sttp.tapir.{Schema, SchemaType}
import sttp.tapir.Schema.SName

trait SchemaMacrosImpl
    extends hearth.kindlings.derivation.compiletime.DerivationTimeout
    with rules.SchemaUseCachedWhenAvailableRuleImpl
    with rules.SchemaUseSelfRefWhenRecursiveRuleImpl
    with rules.SchemaUseImplicitWhenAvailableRuleImpl
    with rules.SchemaHandleAsOptionRuleImpl
    with rules.SchemaHandleAsMapRuleImpl
    with rules.SchemaHandleAsCollectionRuleImpl
    with rules.SchemaHandleAsValueTypeRuleImpl
    with rules.SchemaHandleAsSingletonRuleImpl
    with rules.SchemaHandleAsCaseClassRuleImpl
    with rules.SchemaHandleAsEnumRuleImpl {
  this: MacroCommons & StdExtensions & JsonSchemaConfigs & AnnotationSupport =>

  override protected def derivationSettingsNamespace: String = "tapirSchemaDerivation"

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

  protected lazy val ignoredImplicits: Seq[UntypedMethod] = {
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
                ctx = SchemaCtx.from[A](jsonCfg, cache, inProgress, derivedType)
                result <- deriveSchemaRecursively[A](using ctx)
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
        errorRendering = if (shouldWeLogSchemaDerivation) RenderFrom(Log.Level.Info) else DontRender,
        timeout = derivationTimeout
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

  // Schema context and rules

  final case class SchemaCtx[A](
      tpe: Type[A],
      jsonCfg: JsonSchemaConfig,
      cache: MLocal[ValDefsCache],
      inProgress: MLocal[Set[String]],
      derivedType: Option[??]
  ) {
    def cacheKey: String = s"tapir-schema-for-${tpe.prettyPrint}"

    def nest[B: Type]: SchemaCtx[B] = SchemaCtx(
      tpe = Type[B],
      jsonCfg = jsonCfg,
      cache = cache,
      inProgress = inProgress,
      derivedType = derivedType
    )
  }
  object SchemaCtx {
    def from[A: Type](
        jsonCfg: JsonSchemaConfig,
        cache: MLocal[ValDefsCache],
        inProgress: MLocal[Set[String]],
        derivedType: Option[??]
    ): SchemaCtx[A] =
      SchemaCtx(
        tpe = Type[A],
        jsonCfg = jsonCfg,
        cache = cache,
        inProgress = inProgress,
        derivedType = derivedType
      )
  }

  def sctx[A](implicit A: SchemaCtx[A]): SchemaCtx[A] = A

  implicit def currentSchemaValueType[A: SchemaCtx]: Type[A] = sctx.tpe

  abstract class SchemaDerivationRule(val name: String) extends Rule {
    def apply[A: SchemaCtx]: MIO[Rule.Applicability[Expr[Schema[A]]]]
  }

  def deriveSchemaRecursively[A: SchemaCtx]: MIO[Expr[Schema[A]]] = {
    implicit val SchemaA: Type[Schema[A]] = TsTypes.TapirSchemaOf[A]
    sctx.cache.get0Ary[Schema[A]](sctx.cacheKey).flatMap {
      case Some(cached) =>
        Log.info(s"Using cached Schema for ${Type[A].prettyPrint}") >>
          MIO.pure(cached)
      case None =>
        deriveSchemaRecursivelyViaRules[A]
    }
  }

  private def deriveSchemaRecursivelyViaRules[A: SchemaCtx]: MIO[Expr[Schema[A]]] =
    Log.namedScope(s"deriveSchemaRecursively[${Type[A].prettyPrint}]") {
      Rules(
        SchemaUseSelfRefWhenRecursiveRule,
        SchemaUseImplicitWhenAvailableRule,
        SchemaHandleAsOptionRule,
        SchemaHandleAsMapRule,
        SchemaHandleAsCollectionRule,
        SchemaHandleAsValueTypeRule,
        SchemaHandleAsSingletonRule,
        SchemaHandleAsCaseClassRule,
        SchemaHandleAsEnumRule
      )(_[A]).flatMap {
        case Right(result) =>
          Log.info(s"Derived Schema for ${Type[A].prettyPrint}: ${result.prettyPrint}") >>
            MIO.pure(result)
        case Left(reasons) =>
          val reasonsStrings = reasons.toListMap
            // .removed(SchemaUseCachedWhenAvailableRule)
            .view.map { case (rule, reasonList) =>
              if (reasonList.isEmpty) s"The rule ${rule.name} was not applicable"
              else
                s" - The rule ${rule.name} was not applicable, for the following reasons: ${reasonList.mkString(", ")}"
            }.toList
          MIO.fail(
            new Exception(
              s"Cannot derive tapir Schema for ${Type[A].prettyPrint}: " +
                s"no implicit Schema[T] found and type is not a singleton, case class, sealed trait/enum, Option, collection, or map.\n" +
                reasonsStrings.mkString("\n")
            )
          )
      }
    }

  // Cache operations

  protected def cacheKey[A: Type]: String =
    s"tapir-schema-for-${Type[A].prettyPrint}"

  private def cacheName[A: Type]: String =
    s"tapirSchema_${Type[A].shortName}"

  protected def setCachedAndGet[A: Type](
      cache: MLocal[ValDefsCache],
      instance: Expr[Schema[A]]
  ): MIO[Expr[Schema[A]]] = {
    val key = cacheKey[A]
    val name = cacheName[A]
    implicit val SchemaA: Type[Schema[A]] = TsTypes.TapirSchemaOf[A]
    cache.get0Ary[Schema[A]](key).flatMap {
      case Some(ref) => MIO.pure(ref) // already cached
      case None      =>
        cache.buildCachedWith(key, ValDefBuilder.ofLazy[Schema[A]](name))(_ => instance) >>
          cache.get0Ary[Schema[A]](key).flatMap {
            case Some(ref) => MIO.pure(ref)
            case None      => MIO.pure(instance) // fallback, should not happen
          }
    }
  }

  // Singleton derivation (case object / val — product schema with no fields)

  @scala.annotation.nowarn("msg=is never used|unused explicit parameter")
  protected def deriveSingletonSchema[A: SchemaCtx]: MIO[Expr[Schema[A]]] = {
    implicit val SchemaA: Type[Schema[A]] = TsTypes.TapirSchemaOf[A]
    implicit val SNameT: Type[SName] = TsTypes.SNameType
    implicit val Utils: Type[TapirSchemaUtils.type] = TsTypes.SchemaTypeUtils

    val sNameExpr = computeSNameExpr[A](sctx.derivedType)
    val typeAnnsExpr: Expr[List[Any]] = typeAnnotationsExpr[A]

    val rawSchemaExpr = Expr.quote {
      TapirSchemaUtils.productSchema[A](Expr.splice(sNameExpr), TapirSchemaUtils.emptyFieldList[A])
    }
    val schemaExpr = Expr.quote {
      TapirSchemaUtils.enrichSchema[A](Expr.splice(rawSchemaExpr), Expr.splice(typeAnnsExpr))
    }
    setCachedAndGet[A](sctx.cache, schemaExpr)
  }

  // Case class derivation

  @scala.annotation.nowarn("msg=is never used")
  protected def deriveCaseClassSchema[A: SchemaCtx](cc: CaseClass[A]): MIO[Expr[Schema[A]]] = {
    implicit val SchemaA: Type[Schema[A]] = TsTypes.TapirSchemaOf[A]
    implicit val SNameT: Type[SName] = TsTypes.SNameType
    implicit val Utils: Type[TapirSchemaUtils.type] = TsTypes.SchemaTypeUtils

    val paramsList = cc.primaryConstructor.parameters.flatten.toList
    val activeParams = paramsList.filterNot { case (_, p) => sctx.jsonCfg.isTransientField(p) }
    val sNameExpr = computeSNameExpr[A](sctx.derivedType)

    // Derive field schemas
    val fieldsResult: MIO[List[Expr[SchemaType.SProductField[A]]]] =
      activeParams.foldLeft(MIO.pure(List.empty[Expr[SchemaType.SProductField[A]]])) { case (acc, (fName, param)) =>
        acc.flatMap { results =>
          deriveFieldExpr[A](fName, param).map(results :+ _)
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
      result <- setCachedAndGet[A](sctx.cache, schemaExpr)
    } yield result
  }

  @scala.annotation.nowarn("msg=is never used")
  private def deriveFieldExpr[A: SchemaCtx](
      fieldName: String,
      param: Parameter
  ): MIO[Expr[SchemaType.SProductField[A]]] = {
    import param.tpe.Underlying as Field
    implicit val anyType: Type[Any] = TsTypes.AnyType
    implicit val Utils: Type[TapirSchemaUtils.type] = TsTypes.SchemaTypeUtils

    val fieldIndex = param.index
    val encodedNameExpr: Expr[String] = sctx.jsonCfg.resolveFieldName(param, fieldName)
    val annsExpr: Expr[List[Any]] = fieldAnnotationsExpr(param)

    // Compile-time type checks for field enrichment (C5, C6, C8)
    val hasDefault: Boolean = param.hasDefault
    val isCollectionOrMap: Boolean = Type[Field] match {
      case IsMap(_)        => true
      case IsCollection(_) => true
      case _               => false
    }
    val isNumeric: Boolean = isNumericFieldType[Field]

    for {
      fieldSchema <- deriveSchemaRecursively[Field](using sctx.nest[Field])
    } yield {
      // Start with the field schema as Schema[Any]
      val baseSchemaExpr: Expr[Schema[Any]] = Expr.quote {
        Expr.splice(fieldSchema).asInstanceOf[Schema[Any]]
      }

      // C5: fieldsWithDefaultsAreOptional — mark fields with defaults as optional
      val afterDefault = if (hasDefault) {
        val flag = sctx.jsonCfg.fieldsWithDefaultsAreOptional
        Expr.quote(TapirSchemaUtils.markFieldOptional(Expr.splice(baseSchemaExpr), Expr.splice(flag)))
      } else baseSchemaExpr

      // C6: emptyFieldsAreOptional — mark collection/map fields as optional
      val afterEmpty = if (isCollectionOrMap) {
        val flag = sctx.jsonCfg.emptyFieldsAreOptional
        Expr.quote(TapirSchemaUtils.markFieldOptional(Expr.splice(afterDefault), Expr.splice(flag)))
      } else afterDefault

      // C8: numericFieldsAsStrings — add "string" format to numeric fields
      val enrichedSchemaExpr = if (isNumeric) {
        val flag = sctx.jsonCfg.numericFieldsAsStrings
        Expr.quote(TapirSchemaUtils.markFieldStringFormat(Expr.splice(afterEmpty), Expr.splice(flag)))
      } else afterEmpty

      Expr.quote {
        TapirSchemaUtils.productFieldWithAnnotations[A](
          Expr.splice(Expr(fieldName)),
          Expr.splice(encodedNameExpr),
          Expr.splice(enrichedSchemaExpr),
          Expr.splice(Expr(fieldIndex)),
          Expr.splice(annsExpr)
        )
      }
    }
  }

  /** Check if a type is a numeric primitive or BigDecimal/BigInt at compile time. Uses `plainPrint` (not `prettyPrint`)
    * to avoid ANSI color codes on Scala 2.
    */
  private def isNumericFieldType[F: Type]: Boolean = {
    val pp = Type.plainPrint[F]
    Set(
      "Int",
      "Long",
      "Double",
      "Float",
      "Short",
      "Byte",
      "BigDecimal",
      "BigInt",
      "scala.Int",
      "scala.Long",
      "scala.Double",
      "scala.Float",
      "scala.Short",
      "scala.Byte",
      "scala.math.BigDecimal",
      "scala.math.BigInt",
      "scala.BigDecimal",
      "scala.BigInt"
    ).contains(pp)
  }

  // Enum / sealed trait derivation

  @scala.annotation.nowarn("msg=is never used")
  protected def deriveEnumSchema[A: SchemaCtx](e: Enum[A]): MIO[Expr[Schema[A]]] = {
    implicit val SchemaA: Type[Schema[A]] = TsTypes.TapirSchemaOf[A]
    implicit val SNameT: Type[SName] = TsTypes.SNameType
    implicit val Utils: Type[TapirSchemaUtils.type] = TsTypes.SchemaTypeUtils
    implicit val stringT: Type[String] = TsTypes.StringType

    val sNameExpr = computeSNameExpr[A](sctx.derivedType)
    val discriminatorExpr: Expr[Option[String]] = sctx.jsonCfg.discriminatorFieldName
    val enumAsStringsExpr: Expr[Boolean] = sctx.jsonCfg.enumAsStrings
    val children = e.directChildren.toList
    val typeAnnsExpr: Expr[List[Any]] = typeAnnotationsExpr[A]

    // Build resolved constructor names for discriminator mapping (C3)
    val resolvedNamesListExpr: Expr[List[String]] = children
      .map { case (childName, _) =>
        sctx.jsonCfg.resolveConstructorName(childName)
      }
      .foldRight(Expr.quote(Nil: List[String])) { (nameExpr, acc) =>
        Expr.quote(Expr.splice(nameExpr) :: Expr.splice(acc))
      }

    // Check at compile time if all children are singletons (C4 — stringEnumSchema)
    val allSingletons: Boolean = children.forall { case (_, child) =>
      import child.Underlying as ChildType
      SingletonValue.parse[ChildType].toEither.isRight
    }

    // Build singleton values list for stringEnumSchema (only when all children are singletons)
    val singletonValuesOpt: Option[Expr[List[A]]] = if (allSingletons) {
      Some(children.foldRight(Expr.quote(Nil: List[A])) { case ((_, child), acc) =>
        import child.Underlying as ChildType
        val sv = SingletonValue.parse[ChildType].toEither.toOption.get
        Expr.quote(Expr.splice(sv.singletonExpr).asInstanceOf[A] :: Expr.splice(acc))
      })
    } else None

    // Derive schemas for each child, casting to Schema[Any] inside the quote
    // because Schema is invariant and we need a homogeneous list
    val childSchemasResult: MIO[List[Expr[Schema[Any]]]] =
      children.foldLeft(MIO.pure(List.empty[Expr[Schema[Any]]])) { case (acc, (_, child)) =>
        acc.flatMap { results =>
          import child.Underlying as ChildType
          deriveSchemaRecursively[ChildType](using sctx.nest[ChildType]).map { childSchemaExpr =>
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
      rawSchemaExpr = singletonValuesOpt match {
        // All children are singletons — at runtime, branch on enumAsStrings
        case Some(singletonValuesExpr) =>
          Expr.quote {
            if (Expr.splice(enumAsStringsExpr))
              TapirSchemaUtils.stringEnumSchema[A](
                Expr.splice(sNameExpr),
                Expr.splice(singletonValuesExpr),
                Expr.splice(resolvedNamesListExpr)
              )
            else
              TapirSchemaUtils.coproductSchema[A](
                Expr.splice(sNameExpr),
                Expr.splice(subtypesListExpr),
                Expr.splice(discriminatorExpr),
                Expr.splice(resolvedNamesListExpr)
              )
          }
        // Not all singletons — always coproduct
        case None =>
          Expr.quote {
            TapirSchemaUtils.coproductSchema[A](
              Expr.splice(sNameExpr),
              Expr.splice(subtypesListExpr),
              Expr.splice(discriminatorExpr),
              Expr.splice(resolvedNamesListExpr)
            )
          }
      }
      schemaExpr = Expr.quote {
        TapirSchemaUtils.enrichSchema[A](Expr.splice(rawSchemaExpr), Expr.splice(typeAnnsExpr))
      }
      result <- setCachedAndGet[A](sctx.cache, schemaExpr)
    } yield result
  }

  // SName computation

  @scala.annotation.nowarn("msg=is never used")
  protected def computeSNameExpr[A: Type](derivedType: Option[??]): Expr[SName] = {
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
