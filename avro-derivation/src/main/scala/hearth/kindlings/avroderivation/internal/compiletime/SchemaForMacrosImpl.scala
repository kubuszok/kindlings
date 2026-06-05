package hearth.kindlings.avroderivation.internal.compiletime

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

import hearth.kindlings.avroderivation.{AvroConfig, AvroSchemaFor, DecimalConfig}
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
  fieldName,
  transientField
}
import hearth.kindlings.avroderivation.internal.runtime.AvroDerivationUtils
import org.apache.avro.Schema

trait SchemaForMacrosImpl
    extends AvroDerivationTimeout
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
    with rules.AvroSchemaForHandleAsEnumRuleImpl {
  this: MacroCommons & StdExtensions & AnnotationSupport & LoadStandardExtensionsOnce =>

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
          hearth.kindlings.avroderivation.internal.runtime.AvroDerivationFactories.schemaForInstance[A](
            Expr.splice {
              fromCtx(SchemaForCtx.from[A](Expr.quote(cfg), derivedType = selfType))
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
      derivedType: Option[??],
      namespaceOverride: Option[String] = None
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

    def getCachedSchema[B: Type]: MIO[Option[Expr[Schema]]] = {
      implicit val SchemaT: Type[Schema] = SfTypes.Schema
      cache.get0Ary[Schema](s"cached-schema-for-${Type[B].plainPrint}")
    }
    def setCachedSchema[B: Type](instance: Expr[Schema]): MIO[Unit] = {
      implicit val SchemaT: Type[Schema] = SfTypes.Schema
      Log.info(s"Caching schema for ${Type[B].prettyPrint}") >>
        cache.buildCachedWith(
          s"cached-schema-for-${Type[B].plainPrint}",
          ValDefBuilder.ofLazy[Schema](s"schema_${sanitizedTypeName[B]}")
        )(_ => instance)
    }

    /** Registers an eagerly-initialized empty record Schema (no fields yet) in the cache. Recursive field derivation
      * finds this reference via SfCheckSelfRecordRule, breaking the cycle. After field derivation completes, the main
      * schema lazy val calls setRecordFields on the self-record.
      */
    def setSelfRecordSchema[B: Type](emptyRecord: Expr[Schema]): MIO[Unit] = {
      implicit val SchemaT: Type[Schema] = SfTypes.Schema
      Log.info(s"Registering self-record schema for ${Type[B].prettyPrint}") >>
        cache.buildCachedWith(
          s"self-record-for-${Type[B].plainPrint}",
          ValDefBuilder.ofVal[Schema](s"selfRecord_${sanitizedTypeName[B]}")
        )(_ => emptyRecord)
    }

    def getSelfRecordSchema[B: Type]: MIO[Option[Expr[Schema]]] = {
      implicit val SchemaT: Type[Schema] = SfTypes.Schema
      cache.get0Ary[Schema](s"self-record-for-${Type[B].plainPrint}")
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
      _ <- ensureStandardExtensionsLoaded()
      result <- deriveSchemaRecursively[B](using ctx)
      cache <- localCache.get
    } yield cache.toValDefs.use(_ => result)
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
          AvroSchemaForUseBuiltInSupportRule,
          AvroSchemaForHandleAsValueTypeRule,
          AvroSchemaForHandleAsOptionRule,
          AvroSchemaForHandleAsEitherRule,
          AvroSchemaForHandleAsMapRule,
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
    val fqcn = Type[A].plainPrint.takeWhile(_ != '[')
    val lastDot = fqcn.lastIndexOf('.')
    if (lastDot < 0) "" else fqcn.substring(0, lastDot)
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
        val classNamespace: Option[String] = getTypeAnnotationStringArg[avroNamespace, A]
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

  // Avro name computation

  @scala.annotation.nowarn("msg=is never used")
  protected def computeAvroNameExpr[A: SchemaForCtx]: Expr[String] = {
    implicit val avroNameT: Type[avroName] = SfTypes.AvroName
    implicit val avroErasedNameT: Type[avroErasedName] = SfTypes.AvroErasedName
    getTypeAnnotationStringArg[avroName, A] match {
      case Some(explicitName)                           => Expr(explicitName)
      case None if hasTypeAnnotation[avroErasedName, A] => Expr(Type[A].shortName)
      case None                                         =>
        implicit val SchemaT: Type[Schema] = SfTypes.Schema
        implicit val StringT: Type[String] = SfTypes.String
        implicit val avroFqnParamNamesT: Type[avroFqnParamNames] = SfTypes.AvroFqnParamNames
        val useFqn = hasTypeAnnotation[avroFqnParamNames, A]
        val ignoredImplicits = AvroSchemaForUseImplicitWhenAvailableRule.ignoredImplicits
        val fullNameExpr: Expr[String] = Type[A].runtimePlainPrint { tpe =>
          import tpe.Underlying
          if (tpe.Underlying =:= Type[A]) None
          else if (sfctx.derivedType.exists(_.Underlying =:= tpe.Underlying)) None
          else {
            implicit val AvroSchemaForT: Type[AvroSchemaFor[tpe.Underlying]] = SfTypes.AvroSchemaFor[tpe.Underlying]
            Type[AvroSchemaFor[tpe.Underlying]].summonExprIgnoring(ignoredImplicits*).toEither match {
              case Right(schemaForExpr) =>
                val fallback = Expr(Type.plainPrint[tpe.Underlying])
                Some(Expr.quote {
                  val s = Expr.splice(schemaForExpr).schema
                  val t = s.getType
                  if (t == Schema.Type.RECORD || t == Schema.Type.ENUM || t == Schema.Type.FIXED)
                    s.getFullName
                  else Expr.splice(fallback)
                })
              case Left(_) => None
            }
          }
        }
        if (useFqn) Expr.quote(AvroDerivationUtils.parseAvroNameFqn(Expr.splice(fullNameExpr)))
        else Expr.quote(AvroDerivationUtils.parseAvroName(Expr.splice(fullNameExpr)))
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
    val AvroNoDefault: Type[avroNoDefault] = Type.of[avroNoDefault]
    val AvroEnumDefault: Type[avroEnumDefault] = Type.of[avroEnumDefault]
    val AvroErasedName: Type[avroErasedName] = Type.of[avroErasedName]
    val AvroFqnParamNames: Type[avroFqnParamNames] = Type.of[avroFqnParamNames]
    val AvroName: Type[avroName] = Type.of[avroName]
    val AvroScalePrecision: Type[avroScalePrecision] = Type.of[avroScalePrecision]
  }
}
