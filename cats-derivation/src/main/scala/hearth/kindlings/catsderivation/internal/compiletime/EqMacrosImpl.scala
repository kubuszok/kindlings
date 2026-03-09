package hearth.kindlings.catsderivation.internal.compiletime

import hearth.MacroCommons
import hearth.fp.data.NonEmptyList
import hearth.fp.effect.*
import hearth.fp.syntax.*
import hearth.std.*

import hearth.kindlings.catsderivation.LogDerivation

trait EqMacrosImpl { this: MacroCommons & StdExtensions =>

  @scala.annotation.nowarn("msg=is never used")
  def deriveEq[A: Type]: Expr[cats.kernel.Eq[A]] = {
    val macroName = "Eq.derived"
    implicit val EqA: Type[cats.kernel.Eq[A]] = EqTypes.Eq[A]
    implicit val BooleanType: Type[Boolean] = EqTypes.Boolean
    val selfType: Option[??] = Some(Type[A].as_??)

    deriveEqFromCtxAndAdaptForEntrypoint[A, cats.kernel.Eq[A]](macroName) { fromCtx =>
      Expr.quote {
        new cats.kernel.Eq[A] {
          def eqv(x: A, y: A): Boolean = {
            val _ = x
            val _ = y
            Expr.splice {
              fromCtx(EqCtx.from(Expr.quote(x), Expr.quote(y), derivedType = selfType))
            }
          }
        }
      }
    }
  }

  private def deriveEqFromCtxAndAdaptForEntrypoint[A: Type, Out: Type](macroName: String)(
      provideCtxAndAdapt: (EqCtx[A] => Expr[Boolean]) => Expr[Out]
  ): Expr[Out] = {
    if (Type[A] =:= Type.of[Nothing].asInstanceOf[Type[A]] || Type[A] =:= Type.of[Any].asInstanceOf[Type[A]])
      Environment.reportErrorAndAbort(
        s"$macroName: type parameter was inferred as ${Type[A].prettyPrint}, which is likely unintended."
      )

    Log
      .namedScope(s"Deriving Eq[${Type[A].prettyPrint}] at: ${Environment.currentPosition.prettyPrint}") {
        MIO.scoped { runSafe =>
          val fromCtx: EqCtx[A] => Expr[Boolean] = (ctx: EqCtx[A]) =>
            runSafe {
              for {
                _ <- Environment.loadStandardExtensions().toMIO(allowFailures = false)
                result <- deriveEqRecursively[A](using ctx)
                cache <- ctx.cache.get
              } yield cache.toValDefs.use(_ => result)
            }
          provideCtxAndAdapt(fromCtx)
        }
      }
      .flatTap(result => Log.info(s"Derived final result: ${result.prettyPrint}"))
      .runToExprOrFail(
        macroName,
        infoRendering = if (shouldWeLogEqDerivation) RenderFrom(Log.Level.Info) else DontRender,
        errorRendering = if (shouldWeLogEqDerivation) RenderFrom(Log.Level.Info) else DontRender
      ) { (errorLogs, errors) =>
        val errorsRendered = errors.map(e => "  - " + e.getMessage).mkString("\n")
        val hint =
          "Enable debug logging with: import hearth.kindlings.catsderivation.debug.logDerivationForCatsDerivation"
        if (errorLogs.nonEmpty)
          s"Macro derivation failed:\n$errorsRendered\nlogs:\n$errorLogs\n$hint"
        else
          s"Macro derivation failed:\n$errorsRendered\n$hint"
      }
  }

  // Context for Eq derivation - carries two values to compare

  final case class EqCtx[A](
      tpe: Type[A],
      x: Expr[A],
      y: Expr[A],
      cache: MLocal[ValDefsCache],
      derivedType: Option[??]
  ) {
    def nest[B: Type](newX: Expr[B], newY: Expr[B]): EqCtx[B] = EqCtx(Type[B], newX, newY, cache, derivedType)
  }
  object EqCtx {
    def from[A: Type](x: Expr[A], y: Expr[A], derivedType: Option[??]): EqCtx[A] =
      EqCtx(Type[A], x, y, ValDefsCache.mlocal, derivedType)
  }

  def eqctx[A](implicit A: EqCtx[A]): EqCtx[A] = A
  implicit def eqCtxType[A: EqCtx]: Type[A] = eqctx.tpe

  abstract class EqDerivationRule(val name: String) extends Rule {
    def apply[A: EqCtx]: MIO[Rule.Applicability[Expr[Boolean]]]
  }

  // Recursive derivation

  def deriveEqRecursively[A: EqCtx]: MIO[Expr[Boolean]] =
    Log.namedScope(s"Deriving Eq for ${Type[A].prettyPrint}") {
      Rules(
        EqUseCachedRule,
        EqUseImplicitRule,
        EqValueTypeRule,
        EqOptionRule,
        EqSingletonRule,
        EqCaseClassRule,
        EqEnumRule
      )(_[A]).flatMap {
        case Right(result) =>
          Log.info(s"Derived Eq for ${Type[A].prettyPrint}") >> MIO.pure(result)
        case Left(reasons) =>
          val reasonsStrings = reasons.toListMap
            .removed(EqUseCachedRule)
            .view
            .map { case (rule, reasons) =>
              if (reasons.isEmpty) s"The rule ${rule.name} was not applicable"
              else s" - ${rule.name}: ${reasons.mkString(", ")}"
            }
            .toList
          val err = EqDerivationError.UnsupportedType(Type[A].prettyPrint, reasonsStrings)
          Log.error(err.message) >> MIO.fail(err)
      }
    }

  // Rules

  object EqUseCachedRule extends EqDerivationRule("use cached Eq") {
    def apply[A: EqCtx]: MIO[Rule.Applicability[Expr[Boolean]]] = {
      implicit val EqA: Type[cats.kernel.Eq[A]] = EqTypes.Eq[A]
      eqctx.cache.get0Ary[cats.kernel.Eq[A]]("cached-eq-instance").flatMap {
        case Some(instance) =>
          MIO.pure(Rule.matched(Expr.quote(Expr.splice(instance).eqv(Expr.splice(eqctx.x), Expr.splice(eqctx.y)))))
        case None =>
          implicit val BooleanType: Type[Boolean] = EqTypes.Boolean
          eqctx.cache.get2Ary[A, A, Boolean]("cached-eq-method").flatMap {
            case Some(helper) =>
              MIO.pure(Rule.matched(helper(eqctx.x, eqctx.y)))
            case None =>
              MIO.pure(Rule.yielded(s"No cached Eq for ${Type[A].prettyPrint}"))
          }
      }
    }
  }

  object EqUseImplicitRule extends EqDerivationRule("use implicit Eq") {
    def apply[A: EqCtx]: MIO[Rule.Applicability[Expr[Boolean]]] = {
      implicit val EqA: Type[cats.kernel.Eq[A]] = EqTypes.Eq[A]
      if (eqctx.derivedType.exists(_.Underlying =:= Type[A]))
        MIO.pure(Rule.yielded(s"${Type[A].prettyPrint} is the self-type"))
      else
        EqTypes.Eq[A].summonExprIgnoring().toEither match {
          case Right(instanceExpr) =>
            eqctx.cache.buildCachedWith(
              "cached-eq-instance",
              ValDefBuilder.ofLazy[cats.kernel.Eq[A]](s"eq_${Type[A].shortName}")
            )(_ => instanceExpr) >>
              EqUseCachedRule[A]
          case Left(reason) =>
            MIO.pure(Rule.yielded(s"No implicit Eq[${Type[A].prettyPrint}]: $reason"))
        }
    }
  }

  @scala.annotation.nowarn("msg=is never used")
  object EqValueTypeRule extends EqDerivationRule("Eq as value type") {
    def apply[A: EqCtx]: MIO[Rule.Applicability[Expr[Boolean]]] =
      Type[A] match {
        case IsValueType(isValueType) =>
          import isValueType.Underlying as Inner
          val unwrappedX = isValueType.value.unwrap(eqctx.x)
          val unwrappedY = isValueType.value.unwrap(eqctx.y)
          deriveEqRecursively[Inner](using eqctx.nest(unwrappedX, unwrappedY)).map(Rule.matched(_))
        case _ =>
          MIO.pure(Rule.yielded(s"${Type[A].prettyPrint} is not a value type"))
      }
  }

  @scala.annotation.nowarn("msg=is never used")
  object EqOptionRule extends EqDerivationRule("Eq as Option") {
    def apply[A: EqCtx]: MIO[Rule.Applicability[Expr[Boolean]]] =
      Type[A] match {
        case IsOption(isOption) =>
          import isOption.Underlying as Inner
          implicit val BooleanType: Type[Boolean] = EqTypes.Boolean
          // For Option, we derive Eq for the inner type
          LambdaBuilder
            .of2[Inner, Inner]("ix", "iy")
            .traverse { case (ix, iy) =>
              deriveEqRecursively[Inner](using eqctx.nest(ix, iy))
            }
            .map { builder =>
              val lambda = builder.build[Boolean]
              // Compare: both None = true, both Some = delegate, otherwise false
              Rule.matched(
                isOption.value.fold[Boolean](eqctx.x)(
                  onEmpty = isOption.value.fold[Boolean](eqctx.y)(
                    onEmpty = Expr(true),
                    onSome = _ => Expr(false)
                  ),
                  onSome = xInner =>
                    isOption.value.fold[Boolean](eqctx.y)(
                      onEmpty = Expr(false),
                      onSome = yInner => Expr.quote(Expr.splice(lambda).apply(Expr.splice(xInner), Expr.splice(yInner)))
                    )
                )
              )
            }
        case _ =>
          MIO.pure(Rule.yielded(s"${Type[A].prettyPrint} is not an Option"))
      }
  }

  object EqSingletonRule extends EqDerivationRule("Eq as singleton") {
    def apply[A: EqCtx]: MIO[Rule.Applicability[Expr[Boolean]]] =
      SingletonValue.parse[A].toEither match {
        case Right(_) =>
          // Singletons are always equal to themselves (identity)
          MIO.pure(Rule.matched(Expr(true)))
        case Left(reason) =>
          MIO.pure(Rule.yielded(reason))
      }
  }

  object EqCaseClassRule extends EqDerivationRule("Eq as case class") {
    def apply[A: EqCtx]: MIO[Rule.Applicability[Expr[Boolean]]] =
      CaseClass.parse[A].toEither match {
        case Right(caseClass) =>
          implicit val BooleanType: Type[Boolean] = EqTypes.Boolean
          val defBuilder = ValDefBuilder.ofDef2[A, A, Boolean](s"eqv_${Type[A].shortName}")
          for {
            _ <- eqctx.cache.forwardDeclare("cached-eq-method", defBuilder)
            _ <- MIO.scoped { runSafe =>
              runSafe(eqctx.cache.buildCachedWith("cached-eq-method", defBuilder) { case (_, (x, y)) =>
                runSafe(deriveCaseClassEq[A](caseClass, x, y))
              })
            }
            result <- EqUseCachedRule[A]
          } yield result
        case Left(reason) =>
          MIO.pure(Rule.yielded(reason))
      }

    private def deriveCaseClassEq[A: EqCtx](
        caseClass: CaseClass[A],
        x: Expr[A],
        y: Expr[A]
    ): MIO[Expr[Boolean]] = {
      val fields = caseClass.caseFieldValuesAt(x).toList
      val fieldsY = caseClass.caseFieldValuesAt(y).toList

      NonEmptyList.fromList(fields.zip(fieldsY)) match {
        case Some(fieldPairs) =>
          fieldPairs
            .traverse { case ((fieldName, fieldValueX), (_, fieldValueY)) =>
              import fieldValueX.Underlying as Field
              val fx = fieldValueX.value.asInstanceOf[Expr[Field]]
              val fy = fieldValueY.value.asInstanceOf[Expr[Field]]
              Log.namedScope(s"Deriving Eq for field $fieldName: ${Field.prettyPrint}") {
                deriveEqRecursively[Field](using eqctx.nest(fx, fy))
              }
            }
            .map { results =>
              results.toList.reduceLeft { (acc, next) =>
                Expr.quote(Expr.splice(acc) && Expr.splice(next))
              }
            }
        case None =>
          // No fields — always equal
          MIO.pure(Expr(true))
      }
    }
  }

  object EqEnumRule extends EqDerivationRule("Eq as enum") {
    def apply[A: EqCtx]: MIO[Rule.Applicability[Expr[Boolean]]] =
      Enum.parse[A].toEither match {
        case Right(enumm) =>
          implicit val BooleanType: Type[Boolean] = EqTypes.Boolean
          val defBuilder = ValDefBuilder.ofDef2[A, A, Boolean](s"eqv_${Type[A].shortName}")
          for {
            _ <- eqctx.cache.forwardDeclare("cached-eq-method", defBuilder)
            _ <- MIO.scoped { runSafe =>
              runSafe(eqctx.cache.buildCachedWith("cached-eq-method", defBuilder) { case (_, (x, y)) =>
                runSafe(deriveEnumEq[A](enumm, x, y))
              })
            }
            result <- EqUseCachedRule[A]
          } yield result
        case Left(reason) =>
          MIO.pure(Rule.yielded(reason))
      }

    @scala.annotation.nowarn("msg=is never used|is unchecked")
    private def deriveEnumEq[A: EqCtx](
        enumm: Enum[A],
        x: Expr[A],
        y: Expr[A]
    ): MIO[Expr[Boolean]] = {
      implicit val BooleanType: Type[Boolean] = EqTypes.Boolean
      // Match on x, then cast y to same type and compare
      enumm
        .matchOn[MIO, Boolean](x) { matchedX =>
          import matchedX.{value as caseX, Underlying as EnumCase}
          Log.namedScope(s"Deriving Eq for enum case ${EnumCase.prettyPrint}") {
            // Check if y is the same case, then compare
            val caseY = Expr.quote(Expr.splice(y).asInstanceOf[EnumCase])
            val isInstance = Expr.quote {
              Expr.splice(y).isInstanceOf[EnumCase]
            }
            deriveEqRecursively[EnumCase](using eqctx.nest(caseX, caseY)).map { eqResult =>
              Expr.quote {
                Expr.splice(isInstance) && Expr.splice(eqResult)
              }
            }
          }
        }
        .flatMap {
          case Some(result) => MIO.pure(result)
          case None         =>
            val err = EqDerivationError.NoChildrenInSealedTrait(Type[A].prettyPrint)
            Log.error(err.message) >> MIO.fail(err)
        }
    }
  }

  // Types

  protected object EqTypes {
    def Eq: Type.Ctor1[cats.kernel.Eq] = Type.Ctor1.of[cats.kernel.Eq]
    val Boolean: Type[Boolean] = Type.of[Boolean]
    val Int: Type[Int] = Type.of[Int]
    val String: Type[String] = Type.of[String]
    val LogDerivation: Type[hearth.kindlings.catsderivation.LogDerivation] =
      Type.of[hearth.kindlings.catsderivation.LogDerivation]
  }

  def shouldWeLogEqDerivation: Boolean = {
    implicit val LogDerivationType: Type[LogDerivation] = EqTypes.LogDerivation
    def logDerivationImported = Expr.summonImplicit[LogDerivation].isDefined
    def logDerivationSetGlobally = (for {
      data <- Environment.typedSettings.toOption
      catsDerivation <- data.get("catsDerivation")
      shouldLog <- catsDerivation.get("logDerivation").flatMap(_.asBoolean)
    } yield shouldLog).getOrElse(false)
    logDerivationImported || logDerivationSetGlobally
  }
}

sealed private[compiletime] trait EqDerivationError extends util.control.NoStackTrace with Product with Serializable {
  def message: String
  override def getMessage(): String = message
}
private[compiletime] object EqDerivationError {
  final case class UnsupportedType(tpeName: String, reasons: List[String]) extends EqDerivationError {
    override def message: String =
      s"The type $tpeName was not handled by any Eq derivation rule:\n${reasons.mkString("\n")}"
  }
  final case class NoChildrenInSealedTrait(tpeName: String) extends EqDerivationError {
    override def message: String = s"The type $tpeName does not have any children!"
  }
}
