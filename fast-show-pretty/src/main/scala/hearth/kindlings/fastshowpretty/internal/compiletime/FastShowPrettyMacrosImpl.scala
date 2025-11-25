package hearth.kindlings.fastshowpretty.internal.compiletime

import hearth.MacroCommons
import hearth.fp.effect.*

import hearth.kindlings.fastshowpretty.FastShowPretty
import hearth.kindlings.fastshowpretty.internal.runtime.FastShowPrettyUtils

private[compiletime] trait FastShowPrettyMacrosImpl { this: MacroCommons =>

  // Entrypoints to the macro

  def deriveInline[A: Type](value: Expr[A]): Expr[String] = {
    implicit val stringBuilderType: Type[StringBuilder] = StringBuilderType
    implicit val stringType: Type[String] = StringType
    deriveFromCtxAndAdaptForEntrypoint[A, String] { fromCtx =>
      ValDefs.createVal[StringBuilder](Expr.quote(new StringBuilder)).use { sb =>
        Expr.quote {
          Expr.splice(fromCtx(DerivationCtx.from(sb, value))).toString
        }
      }
    }
  }

  def deriveTypeClass[A: Type]: Expr[FastShowPretty[A]] = {
    implicit val fastShowPrettyType: Type[FastShowPretty[A]] = FastShowPrettyType[A]
    implicit val stringBuilderType: Type[StringBuilder] = StringBuilderType
    deriveFromCtxAndAdaptForEntrypoint[A, FastShowPretty[A]] { fromCtx =>
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

  def deriveFromCtxAndAdaptForEntrypoint[A: Type, Out: Type](
      provideCtxToGetResult: (DerivationCtx[A] => Expr[StringBuilder]) => Expr[Out]
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

      provideCtxToGetResult(fromCtx)
    }
    .runToExprOrFail("TODO", DontRender) { (errorLogs, logs) =>
      "" // TODO
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

  def ctx[A](implicit A: DerivationCtx[A]): DerivationCtx[A] = ctx

  implicit def currentValueType[A: DerivationCtx]: Type[A] = ctx.tpe

  // Attempt utilities - allow representing the derivation logic as a series of derivation rules,
  // which can return the result (success or failure), or yield so that another rule can be tried.

  sealed trait Attempt[+A] extends Product with Serializable
  object Attempt {
    final case class Derived[A](value: Expr[A]) extends Attempt[A]
    final case object Skipped extends Attempt[Nothing]

    def derived[A](value: Expr[A]): MIO[Attempt[A]] =
      Log.info(s"Derived ${value.prettyPrint}") >> MIO.pure(Derived(value))
    def skippedBecause(reason: String): MIO[Attempt[Nothing]] =
      Log.info(s"Skipped because $reason") >> MIO.pure(Skipped)
  }

  implicit class MioAttemptOps[A](private val mioAttempt: MIO[Attempt[A]]) {

    def orElse(other: => MIO[Attempt[A]]): MIO[Attempt[A]] =
      mioAttempt.flatMap {
        case Attempt.Derived(value) => Attempt.derived(value)
        case Attempt.Skipped        => other
      }

    def orFail(error: => DerivationError): MIO[Expr[A]] =
      mioAttempt.flatMap {
        case Attempt.Derived(value) => MIO.pure(value)
        case Attempt.Skipped        => MIO.fail(error)
      }
  }

  // The actual derivation logic in the form of DerivationCtx[A] ?=> MIO[Expr[StringBuilder]].

  // TODO: cache results for nested derivations
  def deriveResultRecursively[A: DerivationCtx]: MIO[Expr[StringBuilder]] =
    useCachedDefWhenAvailableRule[A]
      .orElse(useImplicitWhenAvailableRule[A])
      .orElse(useBuiltInSupportRule[A])
      .orElse(handleAsCaseClassRule[A])
      .orElse(handleAsEnumRule[A])
      .orFail(DerivationError.UnsupportedType()) // TODO better error

  // Particular derivation rules - the first one that applies (succeeding OR failing) is used.

  private def StringBuilderType = Type.of[StringBuilder]
  def useCachedDefWhenAvailableRule[A: DerivationCtx]: MIO[Attempt[StringBuilder]] =
    Log.info(s"Attempting to use cached definition for ${Type[A].prettyPrint}") >> {
      implicit val fastShowPrettyType: Type[FastShowPretty[A]] = FastShowPrettyType[A]
      ctx.cache.get0Ary[FastShowPretty[A]]("instance").flatMap {
        case Some(instance) =>
          Attempt.derived(Expr.quote {
            Expr.splice(instance).render(Expr.splice(ctx.sb))(Expr.splice(ctx.value))
          })
        case None =>
          implicit val stringBuilderType: Type[StringBuilder] = StringBuilderType
          ctx.cache.get1Ary[A, StringBuilder]("helper").flatMap {
            case Some(helperCall) =>
              Attempt.derived(helperCall(ctx.value))
            case None =>
              Attempt.skippedBecause(s"The type ${Type[A].prettyPrint} does not have a cached definition")
          }
      }
    }

  private lazy val ignoredImplicits = Seq.empty[UntypedMethod]
  private def FastShowPrettyType[A: Type]: Type[FastShowPretty[A]] = Type.of[FastShowPretty[A]]
  def useImplicitWhenAvailableRule[A: DerivationCtx]: MIO[Attempt[StringBuilder]] =
    Log.info(s"Attempting to use implicit support for ${Type[A].prettyPrint}") >> {
      implicit val fastShowPrettyType: Type[FastShowPretty[A]] = FastShowPrettyType[A]
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

  private val BooleanType = Type.of[Boolean]
  private val ByteType = Type.of[Byte]
  private val ShortType = Type.of[Short]
  private val IntType = Type.of[Int]
  private val LongType = Type.of[Long]
  private val FloatType = Type.of[Float]
  private val DoubleType = Type.of[Double]
  private val CharType = Type.of[Char]
  private val StringType = Type.of[String]
  def useBuiltInSupportRule[A: DerivationCtx]: MIO[Attempt[StringBuilder]] =
    Log.info(s"Attempting to use built-in support for ${Type[A].prettyPrint}") >> {
      implicit val booleanType: Type[Boolean] = BooleanType
      implicit val byteType: Type[Byte] = ByteType
      implicit val shortType: Type[Short] = ShortType
      implicit val intType: Type[Int] = IntType
      implicit val longType: Type[Long] = LongType
      implicit val floatType: Type[Float] = FloatType
      implicit val doubleType: Type[Double] = DoubleType
      implicit val charType: Type[Char] = CharType
      implicit val stringType: Type[String] = StringType

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

  def handleAsCaseClassRule[A: DerivationCtx]: MIO[Attempt[StringBuilder]] = ??? // TODO

  def handleAsEnumRule[A: DerivationCtx]: MIO[Attempt[StringBuilder]] = ??? // TODO
}

sealed private[compiletime] trait DerivationError extends util.control.NoStackTrace with Product with Serializable
private[compiletime] object DerivationError {
  final case class UnsupportedType() extends DerivationError
}
