package hearth.kindlings.fastshowpretty.internal.compiletime

import hearth.MacroCommons
import hearth.fp.data.NonEmptyList
import hearth.fp.effect.*
import hearth.fp.syntax.*
import hearth.std.*

import hearth.kindlings.fastshowpretty.{FastShowPretty, RenderConfig}
import hearth.kindlings.fastshowpretty.internal.runtime.FastShowPrettyUtils

@scala.annotation.nowarn
private[compiletime] trait FastShowPrettyMacrosImpl { this: MacroCommons & StdExtensions =>

  // Entrypoints to the macro

  def deriveInline[A: Type](value: Expr[A], config: Expr[RenderConfig], level: Expr[Int]): Expr[String] = {
    implicit val StringBuilder: Type[StringBuilder] = Types.StringBuilder
    implicit val String: Type[String] = Types.String

    deriveFromCtxAndAdaptForEntrypoint[A, String]("FastShowPretty.render") { fromCtx =>
      ValDefs.createVal[StringBuilder](Expr.quote(new StringBuilder)).use { sb =>
        Expr.quote {
          Expr.splice(fromCtx(DerivationCtx.from(sb, value, config, level))).toString
        }
      }
    }
  }

  def deriveTypeClass[A: Type]: Expr[FastShowPretty[A]] = {
    implicit val FastShowPretty: Type[FastShowPretty[A]] = Types.FastShowPretty[A]
    implicit val RenderConfigType: Type[RenderConfig] = Types.RenderConfig

    deriveFromCtxAndAdaptForEntrypoint[A, FastShowPretty[A]]("FastShowPretty.derived") { fromCtx =>
      Expr.quote {
        new FastShowPretty[A] {
          def render(sb: StringBuilder)(value: A): StringBuilder =
            render(sb, RenderConfig.Default, 0)(value)

          def render(sb: StringBuilder, config: RenderConfig, level: Int)(value: A): StringBuilder = Expr.splice {
            fromCtx(
              DerivationCtx.from(
                Expr.quote(sb),
                Expr.quote(value),
                Expr.quote(config),
                Expr.quote(level)
              )
            )
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
  ): Expr[Out] = MIO
    .scoped { runSafe =>
      val fromCtx: (DerivationCtx[A] => Expr[StringBuilder]) = (ctx: DerivationCtx[A]) =>
        runSafe {
          Log.namedScope(
            s"Deriving the value ${Type[Out].prettyPrint} for ${Type[Out].prettyPrint} at: ${Environment.currentPosition.prettyPrint}"
          ) {
            for {
              result <- deriveResultRecursively[A](using ctx)
              cache <- ctx.cache.get
            } yield cache.toValDefs.use(_ => result)
          }
        }

      provideCtxAndAdapt(fromCtx)
    }
    .runToExprOrFail(
      macroName,
      infoRendering = hearth.fp.effect.RenderFrom(hearth.fp.effect.Log.Level.Info),
      warnRendering = hearth.fp.effect.DontRender,
      errorRendering = hearth.fp.effect.RenderFrom(hearth.fp.effect.Log.Level.Info)
    ) { (errorLogs, errors) =>
      s"""Macro derivation failed with the following errors:
         |${errors.map(e => s"  - ${e.getMessage()}").mkString("\n")}
         |and the following logs:
         |$errorLogs""".stripMargin
    }

  // Context utilities - instead of passing around multiple types, expressions, helpers,
  // maybe some config options in the future - we can just pass around a single context object.
  // If we would have to pass more things, we can just modify it instead of changing every single method signature.

  final case class DerivationCtx[A](
      tpe: Type[A],
      sb: Expr[StringBuilder],
      value: Expr[A],
      cache: MLocal[ValDefsCache],
      config: Expr[RenderConfig],
      level: Expr[Int]
  ) {

    def nest[B: Type](newValue: Expr[B]): DerivationCtx[B] = DerivationCtx(
      tpe = Type[B],
      sb = sb,
      value = newValue,
      cache = cache,
      config = config,
      level = level
    )

    def incrementLevel: DerivationCtx[A] = copy(
      level = Expr.quote(Expr.splice(level) + 1)
    )
  }
  object DerivationCtx {

    def from[A: Type](
        sb: Expr[StringBuilder],
        value: Expr[A],
        config: Expr[RenderConfig],
        level: Expr[Int]
    ): DerivationCtx[A] = DerivationCtx(
      tpe = Type[A],
      sb = sb,
      value = value,
      cache = ValDefsCache.mlocal,
      config = config,
      level = level
    )
  }

  def ctx[A](implicit A: DerivationCtx[A]): DerivationCtx[A] = A

  implicit def currentValueType[A: DerivationCtx]: Type[A] = ctx.tpe

  abstract class DerivationRule(val name: String) extends Rule {

    def apply[A: DerivationCtx]: MIO[Rule.Applicability[Expr[StringBuilder]]]
  }

  // Reusable components

  private object Types {

    def FastShowPretty: Type.Ctor1[FastShowPretty] = Type.Ctor1.of[FastShowPretty]
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
  }

  // The actual derivation logic in the form of DerivationCtx[A] ?=> MIO[Expr[StringBuilder]].

  // TODO: cache results for nested derivations
  def deriveResultRecursively[A: DerivationCtx]: MIO[Expr[StringBuilder]] =
    Log.namedScope(s"Deriving for type ${Type[A].prettyPrint}") {
      Rules(
        UseCachedDefWhenAvailableRule,
        UseImplicitWhenAvailableRule,
        UseBuiltInSupportRule,
        HandleAsCaseClassRule,
        HandleAsEnumRule
      )(_[A]).flatMap {
        case Right(result) =>
          Log.info(s"Derived result for ${Type[A].prettyPrint}: ${result.prettyPrint}") >>
            MIO.pure(result)
        case Left(reasons) =>
          val reasonsStrings = reasons.toListMap
            .removed(UseCachedDefWhenAvailableRule)
            .view
            .map { case (rule, reasons) =>
              if (reasons.isEmpty) s"The rule ${rule.name} was not applicable"
              else s" - The rule ${rule.name} was not applicable, for the following reasons: ${reasons.mkString(", ")}"
            }
            .toList
          Log.info(s"Failed to derive result for ${Type[A].prettyPrint}:\n${reasonsStrings.mkString("\n")}") >>
            MIO.fail(DerivationError.UnsupportedType(Type[A].prettyPrint, reasonsStrings))
      }
    }

  // Particular derivation rules - the first one that applies (succeeding OR failing) is used.

  object UseCachedDefWhenAvailableRule extends DerivationRule("use cached def when available") {
    implicit val StringBuilder: Type[StringBuilder] = Types.StringBuilder

    def apply[A: DerivationCtx]: MIO[Rule.Applicability[Expr[StringBuilder]]] =
      Log.info(s"Attempting to use cached definition for ${Type[A].prettyPrint}") >> {
        implicit val FastShowPretty: Type[FastShowPretty[A]] = Types.FastShowPretty[A]

        ctx.cache.get0Ary[FastShowPretty[A]]("instance").flatMap {
          case Some(instance) =>
            Log.info(s"Found cached instance for ${Type[A].prettyPrint}, using it") >>
              MIO.pure(Rule.matched(Expr.quote {
                Expr.splice(instance).render(Expr.splice(ctx.sb))(Expr.splice(ctx.value))
              }))

          case None =>
            ctx.cache.get1Ary[A, StringBuilder]("helper").flatMap {
              case Some(helperCall) =>
                Log.info(s"Found cached helper call for ${Type[A].prettyPrint}, using it") >> MIO.pure(
                  Rule.matched(helperCall(ctx.value))
                )
              case None => MIO.pure(Rule.yielded(s"The type ${Type[A].prettyPrint} does not have a cached definition"))
            }
        }
      }
  }

  object UseImplicitWhenAvailableRule extends DerivationRule("use implicit when available") {

    lazy val ignoredImplicits = Type.of[FastShowPretty.type].methods.collect {
      case method if method.value.name == "derived" => method.value.asUntyped
    }

    def apply[A: DerivationCtx]: MIO[Rule.Applicability[Expr[StringBuilder]]] =
      Log.info(s"Attempting to use implicit support for ${Type[A].prettyPrint}") >> {
        implicit val FastShowPretty: Type[FastShowPretty[A]] = Types.FastShowPretty[A]

        Type[FastShowPretty[A]].summonExprIgnoring(ignoredImplicits*).toEither match {
          case Right(instanceExpr) =>
            Log.info(s"Found implicit ${instanceExpr.prettyPrint}, caching it and using a cached value")
            ctx.cache.buildCachedWith("instance", ValDefBuilder.ofVal[FastShowPretty[A]]("instance"))(_ =>
              instanceExpr
            ) >> UseCachedDefWhenAvailableRule[A]
          case Left(reason) =>
            MIO.pure(
              Rule.yielded(
                s"The type ${Type[A].prettyPrint} is does not have an implicit FastShowPretty instance: $reason"
              )
            )
        }
      }
  }

  object UseBuiltInSupportRule extends DerivationRule("use built-in support") {

    implicit val Boolean: Type[Boolean] = Types.Boolean
    implicit val Byte: Type[Byte] = Types.Byte
    implicit val Short: Type[Short] = Types.Short
    implicit val Int: Type[Int] = Types.Int
    implicit val Long: Type[Long] = Types.Long
    implicit val Float: Type[Float] = Types.Float
    implicit val Double: Type[Double] = Types.Double
    implicit val Char: Type[Char] = Types.Char
    implicit val String: Type[String] = Types.String

    def apply[A: DerivationCtx]: MIO[Rule.Applicability[Expr[StringBuilder]]] =
      Log.info(s"Attempting to use built-in support for ${Type[A].prettyPrint}") >> MIO {
        if (Type[A] <:< Type[Boolean]) Rule.matched(Expr.quote {
          FastShowPrettyUtils.renderBoolean(Expr.splice(ctx.sb))(Expr.splice(ctx.value.upcast[Boolean]))
        })
        else if (Type[A] <:< Type[Byte]) Rule.matched(Expr.quote {
          FastShowPrettyUtils.renderByte(Expr.splice(ctx.sb))(Expr.splice(ctx.value.upcast[Byte]))
        })
        else if (Type[A] <:< Type[Short]) Rule.matched(Expr.quote {
          FastShowPrettyUtils.renderShort(Expr.splice(ctx.sb))(Expr.splice(ctx.value.upcast[Short]))
        })
        else if (Type[A] <:< Type[Int]) Rule.matched(Expr.quote {
          FastShowPrettyUtils.renderInt(Expr.splice(ctx.sb))(Expr.splice(ctx.value.upcast[Int]))
        })
        else if (Type[A] <:< Type[Long]) Rule.matched(Expr.quote {
          FastShowPrettyUtils.renderLong(Expr.splice(ctx.sb))(Expr.splice(ctx.value.upcast[Long]))
        })
        else if (Type[A] <:< Type[Float]) Rule.matched(Expr.quote {
          FastShowPrettyUtils.renderFloat(Expr.splice(ctx.sb))(Expr.splice(ctx.value.upcast[Float]))
        })
        else if (Type[A] <:< Type[Double]) Rule.matched(Expr.quote {
          FastShowPrettyUtils.renderDouble(Expr.splice(ctx.sb))(Expr.splice(ctx.value.upcast[Double]))
        })
        else if (Type[A] <:< Type[Char]) Rule.matched(Expr.quote {
          FastShowPrettyUtils.renderChar(Expr.splice(ctx.sb))(Expr.splice(ctx.value.upcast[Char]))
        })
        else if (Type[A] <:< Type[String]) Rule.matched(Expr.quote {
          FastShowPrettyUtils.renderString(Expr.splice(ctx.sb))(Expr.splice(ctx.value.upcast[String]))
        })
        else Rule.yielded(s"The type ${Type[A].prettyPrint} is not considered to be a built-in type")
      }
  }

  object HandleAsCaseClassRule extends DerivationRule("handle as case class") {

    def apply[A: DerivationCtx]: MIO[Rule.Applicability[Expr[StringBuilder]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as a case class") >> {
        CaseClass.parse[A] match {
          case Some(caseClass) =>
            val name = Expr(Type[A].shortName)

            val result = NonEmptyList.fromList(caseClass.caseFieldValuesAt(ctx.value).toList) match {
              case Some(fieldValues) =>
                fieldValues
                  .parTraverse { case (fieldName, fieldValue) =>
                    import fieldValue.{Underlying as Field, value as fieldExpr}
                    Log.namedScope(s"Deriving the value ${ctx.value.prettyPrint}.$fieldName: ${Field.prettyPrint}") {
                      // Use incrementLevel so nested case classes are indented properly
                      deriveResultRecursively[Field](using ctx.incrementLevel.nest(fieldExpr)).map { fieldResult =>
                        (fieldName, fieldResult)
                      }
                    }
                  }
                  .map { toAppend =>
                    val renderLeftParenthesisAndHeadField = toAppend.head match {
                      case (fieldName, fieldResult) =>
                        // TODO: fix in cross-quotes on Scala 2!!!
                        // private[this] val x$$4 - is generated by val _ = ... !!!
                        Expr.quote {
                          Expr
                            .splice(ctx.sb)
                            .append(Expr.splice(name))
                            .append("(\n")
                          FastShowPrettyUtils
                            .appendIndent(
                              Expr.splice(ctx.sb),
                              Expr.splice(ctx.config).indentString,
                              Expr.splice(ctx.level) + 1
                            )
                            .append(Expr.splice(Expr(fieldName)))
                            .append(" = ")
                          Expr.splice(fieldResult)
                        }
                    }
                    val renderAllFields = toAppend.tail.foldLeft(renderLeftParenthesisAndHeadField) {
                      case (renderPreviousFields, (fieldName, fieldResult)) =>
                        Expr.quote {
                          Expr
                            .splice(renderPreviousFields)
                            .append(",\n")
                          FastShowPrettyUtils
                            .appendIndent(
                              Expr.splice(ctx.sb),
                              Expr.splice(ctx.config).indentString,
                              Expr.splice(ctx.level) + 1
                            )
                            .append(Expr.splice(Expr(fieldName)))
                            .append(" = ")
                          Expr.splice(fieldResult)
                        }
                    }

                    Expr.quote {
                      Expr.splice(renderAllFields).append("\n")
                      FastShowPrettyUtils
                        .appendIndent(
                          Expr.splice(ctx.sb),
                          Expr.splice(ctx.config).indentString,
                          Expr.splice(ctx.level)
                        )
                        .append(")")
                    }
                  }
              case None =>
                MIO.pure {
                  Expr.quote {
                    Expr.splice(ctx.sb).append(Expr.splice(name)).append("()")
                  }
                }
            }

            result.map(Rule.matched(_))
          case None =>
            MIO.pure(Rule.yielded(s"The type ${Type[A].prettyPrint} is not considered to be a case class"))
        }
      }
  }

  object HandleAsEnumRule extends DerivationRule("handle as enum") {

    def apply[A: DerivationCtx]: MIO[Rule.Applicability[Expr[StringBuilder]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as an enum") >> {
        Enum.parse[A] match {
          case Some(enumm) =>
            val name = Expr(Type[A].shortName)
            implicit val StringBuilder: Type[StringBuilder] = Types.StringBuilder

            enumm
              .parMatchOn[MIO, StringBuilder](ctx.value) { matched =>
                import matched.{value as enumCaseValue, Underlying as EnumCase}
                Log.namedScope(s"Deriving the value ${enumCaseValue.prettyPrint}: ${EnumCase.prettyPrint}") {
                  // Use incrementLevel so nested case classes in enum cases are indented properly
                  deriveResultRecursively[EnumCase](using ctx.incrementLevel.nest(enumCaseValue)).map { enumCaseResult =>
                    Expr.quote {
                      Expr.splice(ctx.sb).append("(")
                      Expr.splice(enumCaseResult).append("): ").append(Expr.splice(name))
                    }
                  }
                }
              }
              .map {
                case Some(result) =>
                  Rule.matched(result)
                case None =>
                  Rule.yielded(s"The type ${Type[A].prettyPrint} does not have any children!")
              }
          case None =>
            MIO.pure(Rule.yielded(s"The type ${Type[A].prettyPrint} is not considered to be an enum"))
        }
      }
  }
}

sealed private[compiletime] trait DerivationError extends util.control.NoStackTrace with Product with Serializable {
  def message: String
  override def getMessage(): String = message
}
private[compiletime] object DerivationError {
  final case class UnsupportedType(tpeName: String, reasons: List[String]) extends DerivationError {
    override def message: String = s"The type $tpeName was "
  }
}
