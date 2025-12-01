package hearth.kindlings.fastshowpretty.internal.compiletime

import hearth.MacroCommons
import hearth.fp.data.NonEmptyList
import hearth.fp.effect.*
import hearth.fp.instances.*
import hearth.fp.syntax.*

import hearth.kindlings.fastshowpretty.FastShowPretty
import hearth.kindlings.fastshowpretty.internal.runtime.FastShowPrettyUtils

@scala.annotation.nowarn
private[compiletime] trait FastShowPrettyMacrosImpl { this: MacroCommons =>

  // Entrypoints to the macro

  def deriveInline[A: Type](value: Expr[A]): Expr[String] = {
    implicit val StringBuilder: Type[StringBuilder] = Types.StringBuilder
    implicit val String: Type[String] = Types.String

    deriveFromCtxAndAdaptForEntrypoint[A, String]("FastShowPretty.render") { fromCtx =>
      ValDefs.createVal[StringBuilder](Expr.quote(new StringBuilder)).use { sb =>
        Expr.quote {
          Expr.splice(fromCtx(DerivationCtx.from(sb, value))).toString
        }
      }
    }
  }

  def deriveTypeClass[A: Type]: Expr[FastShowPretty[A]] = {
    implicit val FastShowPretty: Type[FastShowPretty[A]] = Types.FastShowPretty[A]
    implicit val StringBuilder: Type[StringBuilder] = Types.StringBuilder

    deriveFromCtxAndAdaptForEntrypoint[A, FastShowPretty[A]]("FastShowPretty.derived") { fromCtx =>
      Expr.quote {
        new FastShowPretty[A] {
          def render(sb: StringBuilder)(value: A): StringBuilder = Expr.splice {
            // TODO: Figure out why missing Type[StringBuilder] causes a parser error
            fromCtx(DerivationCtx.from(Expr.quote(sb), Expr.quote(value)))
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
      cache: MLocal[ValDefsCache]
  ) {

    def nest[B: Type](newValue: Expr[B]): DerivationCtx[B] = DerivationCtx(
      tpe = Type[B],
      sb = sb,
      value = newValue,
      cache = cache
    )
  }
  object DerivationCtx {

    def from[A: Type](sb: Expr[StringBuilder], value: Expr[A]): DerivationCtx[A] = DerivationCtx(
      tpe = Type[A],
      sb = sb,
      value = value,
      cache = ValDefsCache.mlocal
    )
  }

  def ctx[A](implicit A: DerivationCtx[A]): DerivationCtx[A] = A

  implicit def currentValueType[A: DerivationCtx]: Type[A] = ctx.tpe

  // Attempt utilities - allow representing the derivation logic as a series of derivation rules,
  // which can return the result (success or failure), or yield so that another rule can be tried.

  sealed trait Attempt[+A] extends Product with Serializable
  object Attempt {
    final case class Derived[A](value: Expr[A]) extends Attempt[A]
    case object Skipped extends Attempt[Nothing]

    def derived[A](value: Expr[A]): MIO[Attempt[A]] =
      Log.info(s"Derived ${value.prettyPrint}") >> MIO.pure(Derived(value))
    def skippedBecause(reason: String): MIO[Attempt[Nothing]] =
      Log.info(s"Skipped because $reason") >> MIO.pure(Skipped)
  }

  implicit final class MioAttemptOps[A](private val mioAttempt: MIO[Attempt[A]]) {

    def orElseAttempt(other: => MIO[Attempt[A]]): MIO[Attempt[A]] =
      mioAttempt.flatMap {
        case Attempt.Derived(value) => Attempt.derived(value)
        case Attempt.Skipped        => other
      }

    def orFailAttempt(error: => Throwable): MIO[Expr[A]] =
      mioAttempt.flatMap {
        case Attempt.Derived(value) => MIO.pure(value.asInstanceOf[Expr[A]])
        case Attempt.Skipped        => MIO.fail(error)
      }
  }

  implicit final class CacheAttemptOps[A](private val cache: MLocal[ValDefsCache]) {

    def ofDef1Attempt[A: Type, Out: Type](name: FreshName, key: String)(
        body: Expr[A] => MIO[Attempt[Out]]
    )(a: Expr[A]): MIO[Attempt[Out]] =
      ValDefBuilder
        .ofDef1[A, Out](name)
        .traverse { case (_, input) =>
          body(input)
        }
        .flatMap { builder =>
          builder.traverse {
            case Attempt.Derived(value) => Some(value.asInstanceOf[Expr[Out]])
            case Attempt.Skipped        => None
          } match {
            case Some(builder) =>
              cache.buildCached(key, builder).flatMap { _ =>
                cache.get1Ary[A, Out](key).map {
                  case Some(thunk) => Attempt.Derived(thunk(a))
                  case None        => Attempt.Skipped
                }
              }
            case None => MIO.pure(Attempt.Skipped)
          }
        }
  }

  // Reusable components

  private object Types {

    def FastShowPretty: Type.Ctor1[FastShowPretty] = Type.Ctor1.of[FastShowPretty]
    val StringBuilder: Type[StringBuilder] = Type.of[StringBuilder]

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
    useCachedDefWhenAvailableRule[A]
      .orElseAttempt(useImplicitWhenAvailableRule[A])
      .orElseAttempt(useBuiltInSupportRule[A])
      .orElseAttempt {
        implicit val StringBuilder: Type[StringBuilder] = Types.StringBuilder
        ctx.cache.ofDef1Attempt[A, StringBuilder]("helper", "helper") { a =>
          handleAsCaseClassRule[A](using ctx.nest(a)).orElseAttempt(handleAsEnumRule[A](using ctx.nest(a)))
        }(ctx.value)
      }
      .orFailAttempt(DerivationError.UnsupportedType(Type[A].prettyPrint))

  // Particular derivation rules - the first one that applies (succeeding OR failing) is used.

  def useCachedDefWhenAvailableRule[A: DerivationCtx]: MIO[Attempt[StringBuilder]] =
    Log.info(s"Attempting to use cached definition for ${Type[A].prettyPrint}") >> {
      implicit val FastShowPretty: Type[FastShowPretty[A]] = Types.FastShowPretty[A]

      ctx.cache.get0Ary[FastShowPretty[A]]("instance").flatMap {
        case Some(instance) =>
          // TODO: figure out why we need this workaround for Scala 3 and fix it in cross-quotes
          def workaround[A0: Type, FSP <: FastShowPretty[A0]: Type](
              instance: Expr[FSP],
              value: Expr[A0]
          ): Expr[StringBuilder] = Expr.quote {
            Expr.splice(instance).render(Expr.splice(ctx.sb))(Expr.splice(value))
          }
          Attempt.derived(workaround[A, FastShowPretty[A]](instance, ctx.value))
        case None =>
          implicit val StringBuilder: Type[StringBuilder] = Types.StringBuilder

          ctx.cache.get1Ary[A, StringBuilder]("helper").flatMap {
            case Some(helperCall) =>
              Attempt.derived(helperCall(ctx.value))
            case None =>
              Attempt.skippedBecause(s"The type ${Type[A].prettyPrint} does not have a cached definition")
          }
      }
    }

  private lazy val ignoredImplicits = Type.of[FastShowPretty.type].methods.collect {
    case method if method.value.name == "derived" => method.value.asUntyped
  }
  def useImplicitWhenAvailableRule[A: DerivationCtx]: MIO[Attempt[StringBuilder]] =
    Log.info(s"Attempting to use implicit support for ${Type[A].prettyPrint}") >> {
      implicit val FastShowPretty: Type[FastShowPretty[A]] = Types.FastShowPretty[A]

      Type[FastShowPretty[A]].summonExprIgnoring(ignoredImplicits*).toEither match {
        case Right(instanceExpr) =>
          ctx.cache.buildCachedWith("instance", ValDefBuilder.ofVal[FastShowPretty[A]]("instance"))(_ =>
            instanceExpr
          ) >> useCachedDefWhenAvailableRule[A]
        case Left(reason) =>
          Attempt.skippedBecause(
            s"The type ${Type[A].prettyPrint} is does not have an implicit FastShowPretty instance: $reason"
          )
      }
    }
  def useBuiltInSupportRule[A: DerivationCtx]: MIO[Attempt[StringBuilder]] =
    Log.info(s"Attempting to use built-in support for ${Type[A].prettyPrint}") >> {
      implicit val Boolean: Type[Boolean] = Types.Boolean
      implicit val Byte: Type[Byte] = Types.Byte
      implicit val Short: Type[Short] = Types.Short
      implicit val Int: Type[Int] = Types.Int
      implicit val Long: Type[Long] = Types.Long
      implicit val Float: Type[Float] = Types.Float
      implicit val Double: Type[Double] = Types.Double
      implicit val Char: Type[Char] = Types.Char
      implicit val String: Type[String] = Types.String

      if (Type[A] <:< Type[Boolean]) Attempt.derived(Expr.quote {
        FastShowPrettyUtils.renderBoolean(Expr.splice(ctx.sb))(Expr.splice(ctx.value.upcast[Boolean]))
      })
      else if (Type[A] <:< Type[Byte]) Attempt.derived(Expr.quote {
        FastShowPrettyUtils.renderByte(Expr.splice(ctx.sb))(Expr.splice(ctx.value.upcast[Byte]))
      })
      else if (Type[A] <:< Type[Short]) Attempt.derived(Expr.quote {
        FastShowPrettyUtils.renderShort(Expr.splice(ctx.sb))(Expr.splice(ctx.value.upcast[Short]))
      })
      else if (Type[A] <:< Type[Int]) Attempt.derived(Expr.quote {
        FastShowPrettyUtils.renderInt(Expr.splice(ctx.sb))(Expr.splice(ctx.value.upcast[Int]))
      })
      else if (Type[A] <:< Type[Long]) Attempt.derived(Expr.quote {
        FastShowPrettyUtils.renderLong(Expr.splice(ctx.sb))(Expr.splice(ctx.value.upcast[Long]))
      })
      else if (Type[A] <:< Type[Float]) Attempt.derived(Expr.quote {
        FastShowPrettyUtils.renderFloat(Expr.splice(ctx.sb))(Expr.splice(ctx.value.upcast[Float]))
      })
      else if (Type[A] <:< Type[Double]) Attempt.derived(Expr.quote {
        FastShowPrettyUtils.renderDouble(Expr.splice(ctx.sb))(Expr.splice(ctx.value.upcast[Double]))
      })
      else if (Type[A] <:< Type[Char]) Attempt.derived(Expr.quote {
        FastShowPrettyUtils.renderChar(Expr.splice(ctx.sb))(Expr.splice(ctx.value.upcast[Char]))
      })
      else if (Type[A] <:< Type[String]) Attempt.derived(Expr.quote {
        FastShowPrettyUtils.renderString(Expr.splice(ctx.sb))(Expr.splice(ctx.value.upcast[String]))
      })
      else Attempt.skippedBecause(s"The type ${Type[A].prettyPrint} is not considered to be a built-in type")
    }

  def handleAsCaseClassRule[A: DerivationCtx]: MIO[Attempt[StringBuilder]] =
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
                    deriveResultRecursively[Field](using ctx.nest(fieldExpr)).map { fieldResult =>
                      (fieldName, fieldResult)
                    }
                  }
                }
                .map { toAppend =>
                  val renderLeftParenthesisAndHeadField = toAppend.head match {
                    case (fieldName, fieldResult) =>
                      // TODO: fix error in Expr.splice for Scala 2 - chaining seem to not be fixed o_0
                      Expr.quote {
                        val _ = Expr.splice(ctx.sb)
                          .append(Expr.splice(name))
                          .append("(\n")
                          .append(Expr.splice(Expr(fieldName)))
                          .append(" = ")
                        Expr.splice(fieldResult)
                      }
                  }
                  val renderAllFields = toAppend.tail.foldLeft(renderLeftParenthesisAndHeadField) {
                    case (renderPreviousFields, (fieldName, fieldResult)) =>
                      // TODO
                      Expr.quote {
                        val _ =
                          Expr
                            .splice(renderPreviousFields)
                            .append(",\n")
                            .append(Expr.splice(Expr(fieldName)))
                            .append(" = ")
                        Expr.splice(fieldResult)
                      }
                  }
                  Expr.quote {
                    Expr
                      .splice(renderAllFields)
                      .append("""\n)""") // TODO: figure out this error (macro parser error when we use single quotes)
                  }
                }
            case None =>
              MIO.pure {
                Expr.quote {
                  Expr.splice(ctx.sb).append(Expr.splice(name)).append("()")
                }
              }
          }

          result.flatMap(Attempt.derived(_))
        case None =>
          Attempt.skippedBecause(s"The type ${Type[A].prettyPrint} is not considered to be a case class")
      }
    }

  def handleAsEnumRule[A: DerivationCtx]: MIO[Attempt[StringBuilder]] =
    Log.info(s"Attempting to handle ${Type[A].prettyPrint} as an enum") >> {
      Enum.parse[A] match {
        case Some(enumm) =>
          val name = Expr(Type[A].shortName)
          implicit val StringBuilder: Type[StringBuilder] = Types.StringBuilder

          enumm
            .parMatchOn[MIO, StringBuilder](ctx.value) { matched =>
              import matched.{value as enumCaseValue, Underlying as EnumCase}
              Log.namedScope(s"Deriving the value ${enumCaseValue.prettyPrint}: ${EnumCase.prettyPrint}") {
                deriveResultRecursively[EnumCase](using ctx.nest(enumCaseValue)).map { enumCaseResult =>
                  Expr.quote {
                    val _ = Expr.splice(ctx.sb).append("(")
                    Expr.splice(enumCaseResult).append("): ").append(Expr.splice(name))
                  }
                }
              }
            }
            .flatMap {
              case Some(result) =>
                Attempt.derived(result)
              case None =>
                Attempt.skippedBecause(s"The type ${Type[A].prettyPrint} does not have any children!")
            }
        case None =>
          Attempt.skippedBecause(s"The type ${Type[A].prettyPrint} is not considered to be an enum")
      }
    }
}

sealed private[compiletime] trait DerivationError extends util.control.NoStackTrace with Product with Serializable {
  def message: String
  override def getMessage(): String = message
}
private[compiletime] object DerivationError {
  final case class UnsupportedType(tpeName: String) extends DerivationError {
    override def message: String = s"The type $tpeName is not neither-built-in nor case-class nor enum"
  }
}
