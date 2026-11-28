package hearth.kindlings.avroderivation.internal.compiletime

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

import hearth.kindlings.avroderivation.{AvroConfig, AvroSchemaFor, DecimalConfig, TimestampPrecision}
import hearth.kindlings.avroderivation.annotations.{
  avroAlias,
  avroDefault,
  avroDoc,
  avroEnumDefault,
  avroErasedName,
  avroError,
  avroFixed,
  avroFqnParamNames,
  avroName,
  avroNamespace,
  avroNoDefault,
  avroProp,
  avroScalePrecision,
  avroSortPriority,
  avroUnionPosition,
  fieldName,
  transientField
}
import hearth.kindlings.avroderivation.internal.runtime.AvroDerivationUtils
import org.apache.avro.Schema

trait SchemaForMacrosImpl
    extends AvroDerivationTimeout
    with AvroDerivationPolicy
    with rules.AvroSchemaForUseCachedDefWhenAvailableRuleImpl
    with rules.AvroSchemaForCheckSelfRecordRuleImpl
    with rules.AvroSchemaForUseImplicitWhenAvailableRuleImpl
    with rules.AvroSchemaForHandleAsLiteralTypeRuleImpl
    with rules.AvroSchemaForUseBuiltInSupportRuleImpl
    with rules.AvroSchemaForHandleAsValueTypeRuleImpl
    with rules.AvroSchemaForHandleAsOptionRuleImpl
    with rules.AvroSchemaForHandleAsEitherRuleImpl
    with rules.AvroSchemaForHandleAsMapRuleImpl
    with rules.AvroSchemaForHandleAsCollectionRuleImpl
    with rules.AvroSchemaForHandleAsNamedTupleRuleImpl
    with rules.AvroSchemaForHandleAsSingletonRuleImpl
    with rules.AvroSchemaForHandleAsCaseClassRuleImpl
    with rules.AvroSchemaForHandleAsEnumRuleImpl
    with rules.AvroSchemaForDerivationPolicyRuleImpl
    with hearth.kindlings.derivation.compiletime.MethodFolds {
  this: MacroCommons & StdExtensions & AnnotationSupport & LoadStandardExtensionsOnce =>

  // $COVERAGE-OFF$
  override protected def derivationPolicyTypeClassName: String = "AvroSchemaFor"
  // $COVERAGE-ON$

  // Entrypoints

  def deriveInlineSchema[A: Type](configExpr: Expr[AvroConfig]): Expr[Schema] = {
    implicit val SchemaT: Type[Schema] = SfTypes.Schema
    implicit val ConfigT: Type[AvroConfig] = SfTypes.AvroConfig
    // Evaluate the config at compile time when possible, so field-name mapping is a compile-time constant
    // (no per-field `config.transformFieldNames(name)` call). Falls back to the runtime call when not evaluable.
    val evConfig: Option[AvroConfig] = configExpr.semiEval.toOption

    deriveSchemaFromCtxAndAdaptForEntrypoint[A, Schema]("AvroSchemaFor.schemaOf") { fromCtx =>
      ValDefs.createVal[AvroConfig](configExpr).use { configVal =>
        Expr.quote {
          val _ = Expr.splice(configVal)
          Expr.splice(fromCtx(SchemaForCtx.from[A](configVal, derivedType = None, evaluatedConfig = evConfig)))
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
    val evConfig: Option[AvroConfig] = configExpr.semiEval.toOption

    deriveSchemaFromCtxAndAdaptForEntrypoint[A, AvroSchemaFor[A]]("AvroSchemaFor.derived") { fromCtx =>
      ValDefs.createVal[AvroConfig](configExpr).use { configVal =>
        Expr.quote {
          val cfg = Expr.splice(configVal)
          hearth.kindlings.avroderivation.internal.runtime.AvroDerivationFactories.schemaForInstance[A](
            Expr.splice {
              fromCtx(SchemaForCtx.from[A](Expr.quote(cfg), derivedType = selfType, evaluatedConfig = evConfig))
            }
          )
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
                _ <- ensureStandardExtensionsLoaded()
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
        errorRendering = if (shouldWeLogSchemaDerivation) RenderFrom(Log.Level.Info) else DontRender,
        timeout = derivationTimeout
      ) { (errorLogs, errors) =>
        val errorsRendered =
          hearth.kindlings.derivation.compiletime.DerivationErrorRendering.renderErrors(errors.toVector)
        val hint =
          "Enable debug logging with: import hearth.kindlings.avroderivation.debug.logDerivationForAvroSchemaFor or scalac option -Xmacro-settings:avroDerivation.logDerivation=true"
        if (errorLogs.nonEmpty)
          s"""Macro derivation failed with the following errors:
             |$errorsRendered
             |and the following logs:
             |${hearth.kindlings.derivation.compiletime.DerivationErrorRendering.withoutAnsiCodes(errorLogs)}
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
      derivedType: Option[??],
      namespaceOverride: Option[String] = None,
      evaluatedConfig: Option[AvroConfig] = None,
      inInstanceScope: Boolean = false
  ) {

    def nest[B: Type]: SchemaForCtx[B] = copy[B](
      tpe = Type[B]
    )

    def nestWithNamespaceOverride[B: Type](ns: String): SchemaForCtx[B] = copy[B](
      tpe = Type[B],
      namespaceOverride = Some(ns)
    )

    /** Sanitize a type's plainPrint for use as a Scala identifier in generated code. */
    private def sanitizedTypeName[B: Type]: String =
      Type[B].plainPrint.replaceAll("[^a-zA-Z0-9_]", "_")

    /** Whether `B` is derived structurally without searching for its implicit (the root of `AvroSchemaFor.derived` and
      * of the schemas encoders/decoders derive for themselves).
      */
    private def derivedIgnoringImplicit[B: Type]: Boolean = derivedType.exists(_.Underlying =:= Type[B])

    // Schemas are cached under two keys, so that one cache can be shared by every schema an instance derives:
    //  - `cached-schema-for-B`: B derived as a nested type, after its implicit search found nothing (a user-provided
    //    `AvroSchemaFor[B]` is never cached, so it is found again by every lookup),
    //  - `cached-root-schema-for-B`: B derived structurally without the implicit search (see
    //    `derivedIgnoringImplicit`), which must not be reused for a nested B: that would skip its implicit search.
    // A root lookup may reuse the nested schema (also structural), unless that one had a field-level namespace.
    // The self-records (below) are kept apart the same way: a self-record is only meant for the references to a type
    // from within its own derivation, and a cache shared by several schemas outlives those derivations.
    private def nestedKey[B: Type]: String = s"cached-schema-for-${Type[B].plainPrint}"
    private def rootKey[B: Type]: String = s"cached-root-schema-for-${Type[B].plainPrint}"
    private def plainNestedSchemaMarker[B: Type]: (Int, String) = (System.identityHashCode(cache), Type[B].plainPrint)

    def getCachedSchema[B: Type]: MIO[Option[Expr[Schema]]] = {
      implicit val SchemaT: Type[Schema] = SfTypes.Schema
      if (!derivedIgnoringImplicit[B]) cache.get0Ary[Schema](nestedKey[B])
      else
        cache.get0Ary[Schema](rootKey[B]).flatMap {
          case None if plainNestedSchemas.contains(plainNestedSchemaMarker[B]) => cache.get0Ary[Schema](nestedKey[B])
          case other                                                           => MIO.pure(other)
        }
    }
    def setCachedSchema[B: Type](instance: Expr[Schema]): MIO[Unit] = {
      implicit val SchemaT: Type[Schema] = SfTypes.Schema
      val isRoot = derivedIgnoringImplicit[B]
      if (!isRoot && namespaceOverride.isEmpty) plainNestedSchemas += plainNestedSchemaMarker[B]
      Log.info(s"Caching schema for ${Type[B].prettyPrint}") >>
        cache.buildCachedWith(
          if (isRoot) rootKey[B] else nestedKey[B],
          ValDefBuilder.ofLazy[Schema](s"schema_${sanitizedTypeName[B]}")
        )(_ => instance)
    }

    /** Registers an empty record Schema (no fields yet) in the cache. Recursive field derivation finds this reference
      * via SfCheckSelfRecordRule, breaking the cycle. After field derivation completes, the main schema lazy val calls
      * setRecordFields on the self-record.
      *
      * In the cache of an encoder or decoder instance (`inInstanceScope`) it is a `lazy val` (its initializer depends
      * on nothing recursive): a strict val between the instance's `def`s would make their forward references illegal.
      */
    private def selfRecordKey[B: Type]: String =
      if (derivedIgnoringImplicit[B]) s"self-root-record-for-${Type[B].plainPrint}"
      else s"self-record-for-${Type[B].plainPrint}"

    def setSelfRecordSchema[B: Type](emptyRecord: Expr[Schema]): MIO[Unit] = {
      implicit val SchemaT: Type[Schema] = SfTypes.Schema
      Log.info(s"Registering self-record schema for ${Type[B].prettyPrint}") >> {
        val name = s"selfRecord_${sanitizedTypeName[B]}"
        if (inInstanceScope)
          cache.buildCachedWith(selfRecordKey[B], ValDefBuilder.ofLazy[Schema](name))(_ => emptyRecord)
        else cache.buildCachedWith(selfRecordKey[B], ValDefBuilder.ofVal[Schema](name))(_ => emptyRecord)
      }
    }

    def getSelfRecordSchema[B: Type]: MIO[Option[Expr[Schema]]] = {
      implicit val SchemaT: Type[Schema] = SfTypes.Schema
      cache.get0Ary[Schema](selfRecordKey[B])
    }

    override def toString: String =
      s"schemaFor[${tpe.prettyPrint}](config = ${config.prettyPrint})"
  }
  object SchemaForCtx {

    def from[A: Type](
        config: Expr[AvroConfig],
        derivedType: Option[??],
        evaluatedConfig: Option[AvroConfig] = None
    ): SchemaForCtx[A] = SchemaForCtx(
      tpe = Type[A],
      config = config,
      cache = ValDefsCache.mlocal,
      derivedType = derivedType,
      evaluatedConfig = evaluatedConfig
    )
  }

  def sfctx[A](implicit A: SchemaForCtx[A]): SchemaForCtx[A] = A

  /** Nested schemas cached without a field-level namespace override (cache identity, type): see `getCachedSchema`. */
  private val plainNestedSchemas = scala.collection.mutable.Set.empty[(Int, String)]

  implicit def currentSchemaValueType[A: SchemaForCtx]: Type[A] = sfctx.tpe

  abstract class SchemaDerivationRule(val name: String) extends Rule {
    def apply[A: SchemaForCtx]: MIO[Rule.Applicability[Expr[Schema]]]
  }

  /** Derives a schema within a shared cache, for use by encoder/decoder derivation. */
  def deriveSchemaInSharedScope[B: Type](config: Expr[AvroConfig], cache: MLocal[ValDefsCache]): MIO[Expr[Schema]] = {
    val evConfig: Option[AvroConfig] = config.semiEval.toOption
    implicit val ctx: SchemaForCtx[B] = SchemaForCtx(
      tpe = Type[B],
      config = config,
      cache = cache,
      derivedType = None,
      evaluatedConfig = evConfig
    )
    deriveSchemaRecursively[B]
  }

  /** Derives schema with its own local cache, returning a self-contained expression. Use this instead of
    * deriveInlineSchema when calling from within an encoder/decoder MIO chain to avoid Scala 3 splice isolation issues.
    */
  def deriveSelfContainedSchema[B: Type](
      config: Expr[AvroConfig],
      evaluatedConfig: Option[AvroConfig] = None
  ): MIO[Expr[Schema]] = {
    // The caller may know the config's value even when `config` itself cannot be evaluated (e.g. it is the parameter
    // of a cached def): use it, so the schema does not fall back to reading the config at runtime.
    val evConfig: Option[AvroConfig] = evaluatedConfig.orElse(config.semiEval.toOption)
    val localCache = ValDefsCache.mlocal
    // Mark `B` as the type being derived so `AvroSchemaForUseImplicitWhenAvailableRule` skips the implicit
    // search for the *root* type and derives it structurally. Without this, a `derives AvroEncoder`/`AvroDecoder`
    // clause on `B` puts a companion `derived$AvroEncoder: AvroEncoder[B]` given in scope — and since
    // `AvroEncoder[B] <: AvroSchemaFor[B]`, summoning `AvroSchemaFor[B]` for the root finds that given and emits
    // `derived$AvroEncoder(...).schema`, i.e. the instance summons its own schema → infinite recursion (a runtime
    // StackOverflow for generic `B`). Nested field/child types still resolve implicits normally (their type differs
    // from `B`), so an in-scope `AvroEncoder`/`AvroDecoder` for a *field* is still reused as its schema source.
    // Mirrors `deriveSchemaForTypeClass`, the real `AvroSchemaFor.derived` entry, which already passes `selfType`.
    val ctx =
      SchemaForCtx[B](Type[B], config, localCache, derivedType = Some(Type[B].as_??), evaluatedConfig = evConfig)
    for {
      _ <- ensureStandardExtensionsLoaded()
      result <- deriveSchemaRecursively[B](using ctx)
      cache <- localCache.get
    } yield cache.toValDefs.use(_ => result)
  }

  /** Derives the schema of `B` (structurally, like [[deriveSelfContainedSchema]]) into the cache of the instance being
    * derived, which the caller emits around the instance: nested record schemas are then built once and shared by all
    * the schemas the instance derives (its own and those of its nested records), instead of a copy per schema.
    */
  def deriveSchemaInInstanceScope[B: Type](
      config: Expr[AvroConfig],
      evaluatedConfig: Option[AvroConfig],
      cache: MLocal[ValDefsCache]
  ): MIO[Expr[Schema]] = {
    val evConfig: Option[AvroConfig] = evaluatedConfig.orElse(config.semiEval.toOption)
    // `derivedType = B`: the root type is derived without the implicit search, see `deriveSelfContainedSchema`.
    val ctx = SchemaForCtx[B](
      Type[B],
      config,
      cache,
      derivedType = Some(Type[B].as_??),
      evaluatedConfig = evConfig,
      inInstanceScope = true
    )
    ensureStandardExtensionsLoaded() >> deriveSchemaRecursively[B](using ctx)
  }

  // The actual derivation logic

  def deriveSchemaRecursively[A: SchemaForCtx]: MIO[Expr[Schema]] =
    sfctx.getCachedSchema[A].flatMap {
      case Some(cachedSchema) =>
        Log.info(s"Found cached schema for ${Type[A].prettyPrint}") >>
          MIO.pure(cachedSchema)
      case None =>
        deriveSchemaRecursivelyViaRules[A]
    }

  private def deriveSchemaRecursivelyViaRules[A: SchemaForCtx]: MIO[Expr[Schema]] =
    Log
      .namedScope(s"Deriving schema for type ${Type[A].prettyPrint}") {
        Rules(
          AvroSchemaForCheckSelfRecordRule,
          AvroSchemaForHandleAsLiteralTypeRule,
          AvroSchemaForUseImplicitWhenAvailableRule,
          AvroSchemaForDerivationPolicyRule,
          AvroSchemaForUseBuiltInSupportRule,
          AvroSchemaForHandleAsValueTypeRule,
          AvroSchemaForHandleAsOptionRule,
          AvroSchemaForHandleAsEitherRule,
          // Map handling is folded into the collection rule (single IsCollection parse, dispatched via `.asMap`).
          AvroSchemaForHandleAsCollectionRule,
          AvroSchemaForHandleAsNamedTupleRule,
          AvroSchemaForHandleAsSingletonRule,
          AvroSchemaForHandleAsCaseClassRule,
          AvroSchemaForHandleAsEnumRule
        )(_[A]).flatMap {
          case Right(result) =>
            Log.info(s"Derived schema for ${Type[A].prettyPrint}: ${result.prettyPrint}") >>
              MIO.pure(result)
          case Left(reasons) =>
            val reasonsStrings = reasons.toListMap
              // .removed(AvroSchemaForUseCachedDefWhenAvailableRule)
              .removed(AvroSchemaForCheckSelfRecordRule)
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

  // Avro namespace computation

  /** Extracts the package namespace from a type's fully qualified name.
    *
    * For `hearth.kindlings.avroderivation.SimplePerson` this returns `"hearth.kindlings.avroderivation"`. For a
    * top-level type with no package (e.g. `Foo`) this returns `""`.
    */
  protected def extractPackageNamespace[A: Type]: String = {
    val fqcn = enumerationObjectFqn[A].getOrElse(Type[A].plainPrint.takeWhile(_ != '[').stripSuffix(".type"))
    val lastDot = fqcn.lastIndexOf('.')
    val namespace = if (lastDot < 0) "" else fqcn.substring(0, lastDot).stripSuffix(".package")
    // Scala 2 prints a type in the empty package as `<empty>`.Foo (Scala 3 as Foo): no namespace on both versions.
    namespace match {
      case "<empty>" | "`<empty>`" => ""
      case other                   => other
    }
  }

  /** For a `scala.Enumeration` value type (`Weekday.Value`), the fully qualified name of the enumeration object: avro4s
    * names the ENUM after the object (`Weekday`) in the object's package.
    */
  protected def enumerationObjectFqn[A: Type]: Option[String] =
    if (!Type[A].isEnumeration) None
    else {
      val fqcn = Type[A].plainPrint.takeWhile(_ != '[')
      val lastDot = fqcn.lastIndexOf('.')
      if (fqcn.endsWith(".type") || lastDot < 0) None else Some(fqcn.substring(0, lastDot))
    }

  /** The `@avroNamespace` of the closest sealed supertype of `A` that has one. */
  private def sealedParentNamespace[A: Type]: Option[String] = {
    implicit val avroNamespaceT: Type[avroNamespace] = SfTypes.AvroNamespace
    Type[A].baseClasses.iterator
      .flatMap { base =>
        import base.Underlying as Base
        if (Type[Base] =:= Type[A] || !Type[Base].isSealed) None
        else getTypeAnnotationStringArg[avroNamespace, Base]
      }
      .nextOption()
  }

  /** The type arguments of `A` (at any depth) that carry `@avroName`, as (printed type, name): avro4s uses the
    * annotated name of a type argument in the generic name (`Generic__renamed`).
    */
  private def renamedTypeArguments[A: Type]: List[(String, String)] = {
    implicit val avroNameT: Type[avroName] = SfTypes.AvroName
    Type.typeArguments[A].flatMap { arg =>
      import arg.Underlying as Arg
      getTypeAnnotationStringArg[avroName, Arg].map(name => Type.plainPrint[Arg] -> name).toList :::
        renamedTypeArguments[Arg]
    }
  }

  /** Computes the namespace expression for a record or enum type with the following priority:
    *   1. Field-level `@avroNamespace` override from the parent context (highest priority)
    *   2. Type-level `@avroNamespace` annotation on the type itself
    *   3. `AvroConfig.namespace` (explicit config)
    *   4. Package name extracted from the type's fully qualified name
    *   5. `""` (fallback for top-level types)
    */
  @scala.annotation.nowarn("msg=is never used")
  protected def computeNamespaceExpr[A: SchemaForCtx]: Expr[String] = {
    implicit val avroNamespaceT: Type[avroNamespace] = SfTypes.AvroNamespace
    implicit val AvroConfigT: Type[AvroConfig] = SfTypes.AvroConfig
    implicit val StringT: Type[String] = SfTypes.String

    // Field-level override takes highest priority
    sfctx.namespaceOverride match {
      case Some(ns) => Expr(ns)
      case None     =>
        // as avro4s: the type's own @avroNamespace, else the one of its sealed parent (a subtype then has the same
        // name whether its schema is derived on its own - e.g. by its encoder - or as a member of the parent's union)
        val classNamespace: Option[String] =
          getTypeAnnotationStringArg[avroNamespace, A].orElse(sealedParentNamespace[A])
        val packageNamespace: String = extractPackageNamespace[A]

        classNamespace match {
          case Some(ns) => Expr(ns)
          case None     =>
            val packageNsExpr = Expr(packageNamespace)
            Expr.quote {
              Expr.splice(sfctx.config).namespace.getOrElse(Expr.splice(packageNsExpr))
            }
        }
    }
  }

  // Decimal rounding

  /** `mode` as a constant in the generated code: read from a table by its ordinal (cross-quotes print a reference to a
    * Java enum constant by its bare name on Scala 2, which does not compile).
    */
  protected def roundingModeConstant(mode: java.math.RoundingMode): Expr[java.math.RoundingMode] = {
    implicit val RoundingModeT: Type[java.math.RoundingMode] = RoundingModeType
    Expr.quote(AvroDerivationUtils.roundingModeOf(Expr.splice(Expr(mode.ordinal))))
  }
  private lazy val RoundingModeType: Type[java.math.RoundingMode] = Type.of[java.math.RoundingMode]

  /** `@avroScalePrecision(precision, scale)` or `@avroScalePrecision(precision, scale, roundingMode)` on a field. */
  final protected case class ScalePrecision(
      precision: Int,
      scale: Int,
      roundingMode: Option[Expr[java.math.RoundingMode]]
  )

  /** The field's `@avroScalePrecision`. Its rounding mode is a constant in the generated code when the macro can
    * evaluate it; otherwise (Hearth's `semiEval` cannot read a Java enum constant such as
    * `java.math.RoundingMode.HALF_UP` on Scala 3, see `docs/research/hearth-gap-semieval-java-static-fields.md`) the
    * annotation's argument expression itself is used.
    */
  protected def getScalePrecision(param: Parameter): Option[ScalePrecision] = {
    implicit val avroScalePrecisionT: Type[avroScalePrecision] = SfTypes.AvroScalePrecision
    implicit val RoundingModeT: Type[java.math.RoundingMode] = RoundingModeType
    param.annotationsOfType[avroScalePrecision].headOption.flatMap { annotation =>
      Annotations.decodedConstructorArguments(annotation).getOrElse(Nil) match {
        case List(Right(precision: Int), Right(scale: Int))          => Some(ScalePrecision(precision, scale, None))
        case List(Right(precision: Int), Right(scale: Int), decoded) =>
          val roundingMode: Option[Expr[java.math.RoundingMode]] = decoded match {
            case Right(mode: java.math.RoundingMode) => Some(roundingModeConstant(mode))
            case _                                   =>
              Annotations.constructorArguments(annotation).getOrElse(Nil).lift(2).collect {
                case arg if arg.Underlying <:< Type[java.math.RoundingMode] =>
                  arg.value.asInstanceOf[Expr[java.math.RoundingMode]]
              }
          }
          Some(ScalePrecision(precision, scale, roundingMode))
        case _ => None
      }
    }
  }

  /** Encodes `value` as a decimal of `precision` and `scale`, with `fieldRoundingMode` (of `@avroScalePrecision`) when
    * given, otherwise with the rounding mode of the config (a constant when the config is known at compile time).
    */
  @scala.annotation.nowarn("msg=is never used")
  protected def encodeDecimalExpr(
      value: Expr[BigDecimal],
      precision: Int,
      scale: Int,
      config: Expr[AvroConfig],
      evaluatedConfig: Option[AvroConfig],
      fieldRoundingMode: Option[Expr[java.math.RoundingMode]] = None
  ): Expr[Any] = {
    implicit val AnyT: Type[Any] = Type.of[Any]
    implicit val AvroConfigT: Type[AvroConfig] = SfTypes.AvroConfig
    implicit val RoundingModeT: Type[java.math.RoundingMode] = RoundingModeType
    val roundingMode: Expr[java.math.RoundingMode] = fieldRoundingMode.getOrElse {
      evaluatedConfig.map(AvroDerivationUtils.decimalRoundingMode) match {
        case Some(mode) => roundingModeConstant(mode)
        case None       => Expr.quote(AvroDerivationUtils.decimalRoundingMode(Expr.splice(config)))
      }
    }
    Expr.quote {
      AvroDerivationUtils.encodeBigDecimal(
        Expr.splice(value),
        Expr.splice(Expr(precision)),
        Expr.splice(Expr(scale)),
        Expr.splice(roundingMode)
      ): Any
    }
  }

  // @avroFixed on a value class

  /** The size of the FIXED type a value class `A` wrapping `Inner` (bytes or a String) is annotated to be. */
  protected def fixedValueTypeSize[A: Type, Inner: Type]: Option[Int] = {
    implicit val avroFixedT: Type[avroFixed] = SfTypes.AvroFixed
    if (Type[Inner] =:= Type.of[Array[Byte]] || Type[Inner] =:= Type.of[String])
      getTypeAnnotationIntArg[avroFixed, A]
    else None
  }

  // @transientField

  /** A `@transientField` is left out of the schema and decoded as its default value; an `Option` field without a
    * default is decoded as `None` (as avro4s decodes `@AvroTransient` fields). Any other field needs a default.
    */
  protected def transientFieldIsOptional(param: Parameter): Boolean = {
    import param.tpe.Underlying as Field
    Type[Field] <:< Type.of[Option[Any]]
  }
  protected def transientFieldHasNoFallback(param: Parameter): Boolean =
    !param.hasDefault && !transientFieldIsOptional(param)

  // Avro ENUM symbols

  /** The ENUM symbol of a case object (enum value) `Child`: its `@avroName` when present (used as it is, as avro4s
    * does), otherwise the constructor name passed through `AvroConfig.transformConstructorNames`.
    */
  @scala.annotation.nowarn("msg=is never used")
  protected def enumSymbolExpr[Child: Type](
      childName: String,
      config: Expr[AvroConfig],
      evaluatedConfig: Option[AvroConfig]
  ): Expr[String] = {
    implicit val avroNameT: Type[avroName] = SfTypes.AvroName
    implicit val AvroConfigT: Type[AvroConfig] = SfTypes.AvroConfig
    implicit val StringT: Type[String] = SfTypes.String
    getTypeAnnotationStringArg[avroName, Child] match {
      case Some(explicitName) => Expr(explicitName)
      case None               =>
        evaluatedConfig match {
          case Some(cfg) => Expr(cfg.transformConstructorNames(childName))
          case None      => Expr.quote(Expr.splice(config).transformConstructorNames(Expr.splice(Expr(childName))))
        }
    }
  }

  // Avro name computation

  @scala.annotation.nowarn("msg=is never used")
  protected def computeAvroNameExpr[A: SchemaForCtx]: Expr[String] = {
    implicit val avroNameT: Type[avroName] = SfTypes.AvroName
    implicit val avroErasedNameT: Type[avroErasedName] = SfTypes.AvroErasedName
    getTypeAnnotationStringArg[avroName, A] match {
      case Some(explicitName)                        => Expr(explicitName)
      case None if enumerationObjectFqn[A].isDefined =>
        Expr(enumerationObjectFqn[A].get.split('.').last)
      case None if hasTypeAnnotation[avroErasedName, A] => Expr(Type[A].shortName)
      case None                                         =>
        implicit val SchemaT: Type[Schema] = SfTypes.Schema
        implicit val StringT: Type[String] = SfTypes.String
        implicit val avroFqnParamNamesT: Type[avroFqnParamNames] = SfTypes.AvroFqnParamNames
        val useFqn = hasTypeAnnotation[avroFqnParamNames, A]
        val ignoredImplicits = AvroSchemaForUseImplicitWhenAvailableRule.ignoredImplicits
        // whether a type argument's name is read from its (user-provided) schema at runtime
        var nameReadAtRuntime = false
        val fullNameExpr: Expr[String] = Type[A].runtimePlainPrint { tpe =>
          import tpe.Underlying
          if (tpe.Underlying =:= Type[A]) None
          else if (getTypeAnnotationStringArg[avroName, tpe.Underlying].isDefined)
            Some(Expr(getTypeAnnotationStringArg[avroName, tpe.Underlying].get))
          else if (sfctx.derivedType.exists(_.Underlying =:= tpe.Underlying)) None
          else {
            implicit val AvroSchemaForT: Type[AvroSchemaFor[tpe.Underlying]] = SfTypes.AvroSchemaFor[tpe.Underlying]
            Type[AvroSchemaFor[tpe.Underlying]].summonExprIgnoring(ignoredImplicits*).toEither match {
              case Right(schemaForExpr) =>
                val fallback = Expr(Type.plainPrint[tpe.Underlying])
                nameReadAtRuntime = true
                Some(
                  Expr.quote {
                    val s = Expr.splice(schemaForExpr).schema
                    val t = s.getType
                    if (t == Schema.Type.RECORD || t == Schema.Type.ENUM || t == Schema.Type.FIXED)
                      s.getFullName
                    else Expr.splice(fallback)
                  }
                )
              case Left(_) => None
            }
          }
        }
        if (!nameReadAtRuntime) {
          // Every part of the name is known now: compute the Avro name at compile time instead of parsing the type
          // name (with regex replacements) every time the expression is evaluated, e.g. per subtype per decode.
          // longest first, so a renamed type is replaced before any shorter name it contains
          val fullName = renamedTypeArguments[A].distinct.sortBy(-_._1.length).foldLeft(Type.plainPrint[A]) {
            case (printed, (argument, name)) =>
              val bracket = printed.indexOf('[')
              if (bracket < 0) printed
              else printed.substring(0, bracket) + printed.substring(bracket).replace(argument, name)
          }
          Expr(
            if (useFqn) AvroDerivationUtils.parseAvroNameFqn(fullName) else AvroDerivationUtils.parseAvroName(fullName)
          )
        } else if (useFqn) Expr.quote(AvroDerivationUtils.parseAvroNameFqn(Expr.splice(fullNameExpr)))
        else Expr.quote(AvroDerivationUtils.parseAvroName(Expr.splice(fullNameExpr)))
    }
  }

  // Configuration known at compile time

  // When the config is evaluated at compile time, its temporal settings become constants in the generated code
  // instead of a read of the runtime config per encoded/decoded value.

  private def precisionConstant(precision: TimestampPrecision): Expr[TimestampPrecision] = {
    implicit val TimestampPrecisionT: Type[TimestampPrecision] = SfTypes.TimestampPrecision
    precision match {
      case TimestampPrecision.Millis => Expr.quote(TimestampPrecision.Millis: TimestampPrecision)
      case TimestampPrecision.Micros => Expr.quote(TimestampPrecision.Micros: TimestampPrecision)
      case TimestampPrecision.Nanos  => Expr.quote(TimestampPrecision.Nanos: TimestampPrecision)
    }
  }

  def instantPrecisionExpr(config: Expr[AvroConfig], evaluated: Option[AvroConfig]): Expr[TimestampPrecision] =
    evaluated match {
      case Some(cfg) => precisionConstant(cfg.instantPrecision)
      case None      =>
        implicit val TimestampPrecisionT: Type[TimestampPrecision] = SfTypes.TimestampPrecision
        Expr.quote(Expr.splice(config).instantPrecision)
    }

  def localDateTimePrecisionExpr(config: Expr[AvroConfig], evaluated: Option[AvroConfig]): Expr[TimestampPrecision] =
    evaluated match {
      case Some(cfg) => precisionConstant(cfg.localDateTimePrecision)
      case None      =>
        implicit val TimestampPrecisionT: Type[TimestampPrecision] = SfTypes.TimestampPrecision
        Expr.quote(Expr.splice(config).localDateTimePrecision)
    }

  def offsetDateTimeAsIsoStringExpr(config: Expr[AvroConfig], evaluated: Option[AvroConfig]): Expr[Boolean] =
    evaluated match {
      case Some(cfg) => Expr(cfg.offsetDateTimeAsIsoString)
      case None      => Expr.quote(Expr.splice(config).offsetDateTimeAsIsoString)
    }

  // Types

  private[compiletime] object SfTypes {

    def AvroSchemaFor: Type.Ctor1[AvroSchemaFor] = Type.Ctor1.of[AvroSchemaFor]
    val SchemaForLogDerivation: Type[hearth.kindlings.avroderivation.AvroSchemaFor.LogDerivation] =
      Type.of[hearth.kindlings.avroderivation.AvroSchemaFor.LogDerivation]
    val Schema: Type.Lazy[Schema] = Type.Lazy(Type.of[Schema])
    val SchemaField: Type.Lazy[org.apache.avro.Schema.Field] = Type.Lazy(Type.of[org.apache.avro.Schema.Field])
    val AvroConfig: Type.Lazy[AvroConfig] = Type.Lazy(Type.of[AvroConfig])
    val DecimalConfig: Type.Lazy[DecimalConfig] = Type.Lazy(Type.of[DecimalConfig])
    val TimestampPrecision: Type.Lazy[TimestampPrecision] = Type.Lazy(Type.of[TimestampPrecision])
    val String: Type.Lazy[String] = Type.Lazy(Type.of[String])
    val FieldName: Type.Lazy[fieldName] = Type.Lazy(Type.of[fieldName])
    val TransientField: Type.Lazy[transientField] = Type.Lazy(Type.of[transientField])
    val AvroDoc: Type.Lazy[avroDoc] = Type.Lazy(Type.of[avroDoc])
    val AvroNamespace: Type.Lazy[avroNamespace] = Type.Lazy(Type.of[avroNamespace])
    val AvroDefault: Type.Lazy[avroDefault] = Type.Lazy(Type.of[avroDefault])
    val AvroFixed: Type.Lazy[avroFixed] = Type.Lazy(Type.of[avroFixed])
    val AvroError: Type.Lazy[avroError] = Type.Lazy(Type.of[avroError])
    val AvroProp: Type.Lazy[avroProp] = Type.Lazy(Type.of[avroProp])
    val AvroAlias: Type.Lazy[avroAlias] = Type.Lazy(Type.of[avroAlias])
    val AvroSortPriority: Type.Lazy[avroSortPriority] = Type.Lazy(Type.of[avroSortPriority])
    val AvroUnionPosition: Type.Lazy[avroUnionPosition] = Type.Lazy(Type.of[avroUnionPosition])
    val AvroNoDefault: Type.Lazy[avroNoDefault] = Type.Lazy(Type.of[avroNoDefault])
    val AvroEnumDefault: Type.Lazy[avroEnumDefault] = Type.Lazy(Type.of[avroEnumDefault])
    val AvroErasedName: Type.Lazy[avroErasedName] = Type.Lazy(Type.of[avroErasedName])
    val AvroFqnParamNames: Type.Lazy[avroFqnParamNames] = Type.Lazy(Type.of[avroFqnParamNames])
    val AvroName: Type.Lazy[avroName] = Type.Lazy(Type.of[avroName])
    val AvroScalePrecision: Type.Lazy[avroScalePrecision] = Type.Lazy(Type.of[avroScalePrecision])
  }
}
