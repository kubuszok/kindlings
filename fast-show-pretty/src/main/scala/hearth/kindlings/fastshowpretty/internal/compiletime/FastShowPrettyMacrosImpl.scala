package hearth.kindlings.fastshowpretty.internal.compiletime

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

import hearth.kindlings.fastshowpretty.{FastShowPretty, RenderConfig}

trait FastShowPrettyMacrosImpl
    extends rules.FastShowPrettyUseCachedDefWhenAvailableRuleImpl
    with rules.FastShowPrettyUseImplicitWhenAvailableRuleImpl
    with rules.FastShowPrettyUseBuiltInSupportRuleImpl
    with rules.FastShowPrettyHandleAsValueTypeRuleImpl
    with rules.FastShowPrettyHandleAsOptionRuleImpl
    with rules.FastShowPrettyHandleAsMapRuleImpl
    with rules.FastShowPrettyHandleAsCollectionRuleImpl
    with rules.FastShowPrettyHandleAsNamedTupleRuleImpl
    with rules.FastShowPrettyHandleAsSingletonRuleImpl
    with rules.FastShowPrettyHandleAsCaseClassRuleImpl
    with rules.FastShowPrettyHandleAsEnumRuleImpl { this: MacroCommons & StdExtensions =>

  // Entrypoints to the macro

  def deriveInline[A: Type](valueExpr: Expr[A], configExpr: Expr[RenderConfig], levelExpr: Expr[Int]): Expr[String] = {
    implicit val StringBuilder: Type[StringBuilder] = Types.StringBuilder
    implicit val RenderConfig: Type[RenderConfig] = Types.RenderConfig
    implicit val String: Type[String] = Types.String
    implicit val Int: Type[Int] = Types.Int

    deriveFromCtxAndAdaptForEntrypoint[A, String]("FastShowPretty.render") { fromCtx =>
      ValDefs.createVal[StringBuilder](Expr.quote(new StringBuilder)).use { sbVal =>
        ValDefs.createVal[A](valueExpr).use { valueVal =>
          ValDefs.createVal[RenderConfig](configExpr).use { configVal =>
            ValDefs.createVal[Int](levelExpr).use { levelVal =>
              Expr.quote {
                val _ = Expr.splice(sbVal)
                val _ = Expr.splice(valueVal)
                val _ = Expr.splice(configVal)
                val _ = Expr.splice(levelVal)
                Expr
                  .splice(fromCtx(DerivationCtx.from(sbVal, valueVal, configVal, levelVal, derivedType = None)))
                  .toString
              }
            }
          }
        }
      }
    }
  }

  def deriveTypeClass[A: Type]: Expr[FastShowPretty[A]] = {
    implicit val FastShowPretty: Type[FastShowPretty[A]] = Types.FastShowPretty[A]
    implicit val RenderConfigType: Type[RenderConfig] = Types.RenderConfig
    val selfType: Option[??] = Some(Type[A].as_??)

    deriveFromCtxAndAdaptForEntrypoint[A, FastShowPretty[A]]("FastShowPretty.derived") { fromCtx =>
      Expr.quote {
        new FastShowPretty[A] {

          def render(sb: StringBuilder, config: RenderConfig, level: Int)(value: A): StringBuilder = {
            val _ = sb
            val _ = config
            val _ = level
            val _ = value
            Expr.splice {
              fromCtx(
                DerivationCtx.from(
                  Expr.quote(sb),
                  Expr.quote(value),
                  Expr.quote(config),
                  Expr.quote(level),
                  derivedType = selfType
                )
              )
            }
          }
        }
      }
    }
  }

  // Handles logging, error reporting and prepending "cached" defs and vals to the result.
  // We used a continuation passing style, to allow sharing the same code between:
  //  - the case that inlines the whole logic to return a String, and
  //  - the case that returns a FastShowPretty instance.

  def deriveFromCtxAndAdaptForEntrypoint[A: Type, Out: Type](macroName: String)(
      provideCtxAndAdapt: (DerivationCtx[A] => Expr[StringBuilder]) => Expr[Out]
  ): Expr[Out] = {
    if (Type[A] =:= Type.of[Nothing].asInstanceOf[Type[A]] || Type[A] =:= Type.of[Any].asInstanceOf[Type[A]])
      Environment.reportErrorAndAbort(
        s"$macroName: type parameter was inferred as ${Type[A].prettyPrint}, which is likely unintended.\n" +
          s"Provide an explicit type parameter, e.g.: $macroName[MyType](...)\n" +
          "or add a type ascription to the result variable."
      )
    Log
      .namedScope(
        s"Deriving the value ${Type[A].prettyPrint} for ${Type[Out].prettyPrint} at: ${Environment.currentPosition.prettyPrint}"
      ) {
        MIO.scoped { runSafe =>
          val fromCtx: (DerivationCtx[A] => Expr[StringBuilder]) = (ctx: DerivationCtx[A]) =>
            runSafe {
              for {
                // Enables usage of IsCollection, IsMap, etc.
                _ <- Environment.loadStandardExtensions().toMIO(allowFailures = false)
                result <- deriveResultRecursively[A](using ctx)
                cache <- ctx.cache.get
              } yield cache.toValDefs.use(_ => result)
            }

          provideCtxAndAdapt(fromCtx)
        }
      }
      .flatTap { result =>
        Log.info(s"Derived final result for: ${result.prettyPrint}")
      }
      .runToExprOrFail(
        macroName,
        infoRendering = if (shouldWeLogDerivation) RenderFrom(Log.Level.Info) else DontRender,
        errorRendering = if (shouldWeLogDerivation) RenderFrom(Log.Level.Info) else DontRender
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
          "Enable debug logging with: import hearth.kindlings.fastshowpretty.debug.logDerivationForFastShowPretty or scalac option -Xmacro-settings:fastShowPretty.logDerivation=true"
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

  /** Enables logging if we either:
    *   - import [[hearth.kindlings.fastshowpretty.debug.logDerivationForFastShowPretty]] in the scope
    *   - have set scalac option `-Xmacro-settings:fastShowPretty.logDerivation=true`
    */
  def shouldWeLogDerivation: Boolean = {
    implicit val LogDerivation: Type[FastShowPretty.LogDerivation] = Types.LogDerivation
    def logDerivationImported = Expr.summonImplicit[FastShowPretty.LogDerivation].isDefined

    def logDerivationSetGlobally = (for {
      data <- Environment.typedSettings.toOption
      fastShowPretty <- data.get("fastShowPretty")
      shouldLog <- fastShowPretty.get("logDerivation").flatMap(_.asBoolean)
    } yield shouldLog).getOrElse(false)

    logDerivationImported || logDerivationSetGlobally
  }

  // Context utilities - instead of passing around multiple types, expressions, helpers,
  // maybe some config options in the future - we can just pass around a single context object.
  // If we would have to pass more things, we can just modify it instead of changing every single method signature.

  final case class DerivationCtx[A](
      tpe: Type[A],
      sb: Expr[StringBuilder],
      value: Expr[A],
      config: Expr[RenderConfig],
      level: Expr[Int],
      cache: MLocal[ValDefsCache],
      derivedType: Option[??]
  ) {

    def nest[B: Type](newValue: Expr[B]): DerivationCtx[B] = copy[B](
      tpe = Type[B],
      value = newValue
    )

    def nestInCache(
        newSb: Expr[StringBuilder],
        newValue: Expr[A],
        newConfig: Expr[RenderConfig],
        newLevel: Expr[Int]
    ): DerivationCtx[A] = copy(
      sb = newSb,
      value = newValue,
      config = newConfig,
      level = newLevel
    )

    def incrementLevel: DerivationCtx[A] = copy(
      level = Expr.quote(Expr.splice(level) + 1)
    )

    // Let us reuse type class instance by "caching" it in a lazy val.
    def getInstance[B: Type]: MIO[Option[Expr[FastShowPretty[B]]]] = {
      implicit val FastShowPrettyB: Type[FastShowPretty[B]] = Types.FastShowPretty[B]
      cache.get0Ary[FastShowPretty[B]]("cached-fast-show-pretty-instance")
    }
    def setInstance[B: Type](instance: Expr[FastShowPretty[B]]): MIO[Unit] = {
      implicit val FastShowPrettyB: Type[FastShowPretty[B]] = Types.FastShowPretty[B]
      Log.info(s"Caching FastShowPretty instance for ${Type[B].prettyPrint}") >>
        cache.buildCachedWith(
          "cached-fast-show-pretty-instance",
          ValDefBuilder.ofLazy[FastShowPretty[B]](s"instance_${Type[B].shortName}")
        )(_ => instance)
    }

    // Let us reuse code derived for some type, by putting all: case class handling or enum handling into a local def.
    def getHelper[B: Type]
        : MIO[Option[(Expr[StringBuilder], Expr[RenderConfig], Expr[Int], Expr[B]) => Expr[StringBuilder]]] = {
      implicit val StringBuilderT: Type[StringBuilder] = Types.StringBuilder
      implicit val RenderConfigT: Type[RenderConfig] = Types.RenderConfig
      implicit val IntT: Type[Int] = Types.Int
      cache.get4Ary[StringBuilder, RenderConfig, Int, B, StringBuilder]("cached-render-method")
    }
    def setHelper[B: Type](
        helper: (Expr[StringBuilder], Expr[RenderConfig], Expr[Int], Expr[B]) => MIO[Expr[StringBuilder]]
    ): MIO[Unit] = {
      implicit val StringBuilderT: Type[StringBuilder] = Types.StringBuilder
      implicit val RenderConfigT: Type[RenderConfig] = Types.RenderConfig
      implicit val IntT: Type[Int] = Types.Int
      val defBuilder =
        ValDefBuilder.ofDef4[StringBuilder, RenderConfig, Int, B, StringBuilder](s"render_${Type[B].shortName}")
      for {
        _ <- Log.info(s"Forward-declaring render helper for ${Type[B].prettyPrint}")
        _ <- cache.forwardDeclare("cached-render-method", defBuilder)
        _ <- MIO.scoped { runSafe =>
          runSafe(cache.buildCachedWith("cached-render-method", defBuilder) { case (_, (sb, config, level, value)) =>
            runSafe(helper(sb, config, level, value))
          })
        }
        _ <- Log.info(s"Defined render helper for ${Type[B].prettyPrint}")
      } yield ()
    }

    override def toString: String =
      s"render[${tpe.prettyPrint}](sb = ${sb.prettyPrint}, config = ${config.prettyPrint}, level = ${level.prettyPrint})(value = ${value.prettyPrint})"
  }
  object DerivationCtx {

    def from[A: Type](
        sb: Expr[StringBuilder],
        value: Expr[A],
        config: Expr[RenderConfig],
        level: Expr[Int],
        derivedType: Option[??]
    ): DerivationCtx[A] = DerivationCtx(
      tpe = Type[A],
      sb = sb,
      value = value,
      config = config,
      level = level,
      cache = ValDefsCache.mlocal,
      derivedType = derivedType
    )
  }

  def ctx[A](implicit A: DerivationCtx[A]): DerivationCtx[A] = A

  implicit def currentValueType[A: DerivationCtx]: Type[A] = ctx.tpe

  abstract class DerivationRule(val name: String) extends Rule {

    def apply[A: DerivationCtx]: MIO[Rule.Applicability[Expr[StringBuilder]]]
  }

  // Reusable components

  protected object Types {

    def FastShowPretty: Type.Ctor1[FastShowPretty] = Type.Ctor1.of[FastShowPretty]
    val LogDerivation: Type[hearth.kindlings.fastshowpretty.FastShowPretty.LogDerivation] =
      Type.of[hearth.kindlings.fastshowpretty.FastShowPretty.LogDerivation]
    val StringBuilder: Type[StringBuilder] = Type.of[StringBuilder]
    val RenderConfig: Type[RenderConfig] = Type.of[RenderConfig]

    val Boolean: Type[Boolean] = Type.of[Boolean]
    val Byte: Type[Byte] = Type.of[Byte]
    val Short: Type[Short] = Type.of[Short]
    val Int: Type[Int] = Type.of[Int]
    val Long: Type[Long] = Type.of[Long]
    val Float: Type[Float] = Type.of[Float]
    val Double: Type[Double] = Type.of[Double]
    val Char: Type[Char] = Type.of[Char]
    val String: Type[String] = Type.of[String]
    val Product: Type[Product] = Type.of[Product]
  }

  // The actual derivation logic in the form of DerivationCtx[A] ?=> MIO[Expr[StringBuilder]].

  def deriveResultRecursively[A: DerivationCtx]: MIO[Expr[StringBuilder]] =
    ctx.getHelper[A].flatMap {
      case Some(helperCall) =>
        Log.info(s"Found cached render helper for ${Type[A].prettyPrint}") >>
          MIO.pure(helperCall(ctx.sb, ctx.config, ctx.level, ctx.value))
      case None =>
        ctx.getInstance[A].flatMap {
          case Some(instance) =>
            Log.info(s"Found cached instance for ${Type[A].prettyPrint}") >>
              MIO.pure(Expr.quote {
                Expr
                  .splice(instance)
                  .render(Expr.splice(ctx.sb), Expr.splice(ctx.config), Expr.splice(ctx.level))(Expr.splice(ctx.value))
              })
          case None =>
            ctx.setHelper[A] { (sb, config, level, value) =>
              deriveResultRecursivelyViaRules[A](using ctx.nestInCache(sb, value, config, level))
            } >> ctx.getHelper[A].flatMap {
              case Some(helperCall) => MIO.pure(helperCall(ctx.sb, ctx.config, ctx.level, ctx.value))
              case None             =>
                MIO.fail(new Exception(s"Failed to build render helper for ${Type[A].prettyPrint}"))
            }
        }
    }

  private def deriveResultRecursivelyViaRules[A: DerivationCtx]: MIO[Expr[StringBuilder]] =
    Log
      .namedScope(s"Deriving for type ${Type[A].prettyPrint}") {
        Rules(
          FastShowPrettyUseImplicitWhenAvailableRule,
          FastShowPrettyUseBuiltInSupportRule,
          FastShowPrettyHandleAsValueTypeRule,
          FastShowPrettyHandleAsOptionRule,
          FastShowPrettyHandleAsMapRule,
          FastShowPrettyHandleAsCollectionRule,
          FastShowPrettyHandleAsNamedTupleRule,
          FastShowPrettyHandleAsSingletonRule,
          FastShowPrettyHandleAsCaseClassRule,
          FastShowPrettyHandleAsEnumRule
        )(_[A]).flatMap {
          case Right(result) =>
            Log.info(s"Derived result for ${Type[A].prettyPrint}: ${result.prettyPrint}") >>
              MIO.pure(result)
          case Left(reasons) =>
            val reasonsStrings = reasons.toListMap
              // .removed(FastShowPrettyUseCachedDefWhenAvailableRule)
              .view.map { case (rule, reasons) =>
                if (reasons.isEmpty) s"The rule ${rule.name} was not applicable"
                else
                  s" - The rule ${rule.name} was not applicable, for the following reasons: ${reasons.mkString(", ")}"
              }.toList
            val err = DerivationError.UnsupportedType(Type[A].prettyPrint, reasonsStrings)
            Log.error(err.message) >> MIO.fail(err)
        }
      }
}
