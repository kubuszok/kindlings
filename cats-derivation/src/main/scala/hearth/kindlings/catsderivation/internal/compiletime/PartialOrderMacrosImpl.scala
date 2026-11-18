package hearth.kindlings.catsderivation.internal.compiletime

import hearth.MacroCommons
import hearth.fp.data.NonEmptyList
import hearth.fp.effect.*
import hearth.fp.syntax.*
import hearth.std.*

import hearth.kindlings.catsderivation.LogDerivation

/** PartialOrder derivation, like kittens: fields are compared with their `PartialOrder` (so a field with only a partial
  * order, e.g. a `Set`, is supported) lexicographically - the first result other than `0.0` (including `NaN`) wins -
  * and values of different subtypes of a sealed trait are incomparable (`NaN`).
  */
trait PartialOrderMacrosImpl extends StrictDerivationSupport with CatsDerivationTimeout {
  this: MacroCommons & StdExtensions =>

  @scala.annotation.nowarn("msg=is never used")
  def derivePartialOrder[A: Type]: Expr[cats.kernel.PartialOrder[A]] = {
    val macroName = "PartialOrder.derived"
    implicit val PartialOrderA: Type[cats.kernel.PartialOrder[A]] = PartialOrderTypes.PartialOrder[A]
    implicit val DoubleType: Type[Double] = PartialOrderTypes.Double
    val selfType: Option[??] = Some(Type[A].as_??)

    if (Type[A] =:= Type.of[Nothing].asInstanceOf[Type[A]] || Type[A] =:= Type.of[Any].asInstanceOf[Type[A]])
      Environment.reportErrorAndAbort(
        s"$macroName: type parameter was inferred as ${Type[A].prettyPrint}, which is likely unintended."
      )

    Log
      .namedScope(s"Deriving PartialOrder[${Type[A].prettyPrint}] at: ${Environment.currentPosition.prettyPrint}") {
        MIO.scoped { runSafe =>
          val fromCtx: PartialOrderCtx[A] => Expr[Double] = (ctx: PartialOrderCtx[A]) =>
            runSafe {
              for {
                _ <- Environment.loadStandardExtensions().toMIO(allowFailures = false)
                result <- derivePartialOrderRecursively[A](using ctx)
                cache <- ctx.cache.get
              } yield cache.toValDefs.use(_ => result)
            }
          Expr.quote {
            hearth.kindlings.catsderivation.internal.runtime.CatsDerivationFactories.partialOrderInstance[A] {
              (x: A, y: A) =>
                val _ = x
                val _ = y
                Expr.splice(fromCtx(PartialOrderCtx.from(Expr.quote(x), Expr.quote(y), derivedType = selfType)))
            }
          }
        }
      }
      .flatTap(result => Log.info(s"Derived final result: ${result.prettyPrint}"))
      .runToExprOrFail(
        macroName,
        infoRendering = if (shouldWeLogPartialOrderDerivation) RenderFrom(Log.Level.Info) else DontRender,
        errorRendering = if (shouldWeLogPartialOrderDerivation) RenderFrom(Log.Level.Info) else DontRender,
        timeout = derivationTimeout
      ) { (errorLogs, errors) =>
        val errorsRendered =
          hearth.kindlings.derivation.compiletime.DerivationErrorRendering.renderErrors(errors.toVector)
        val hint =
          "Enable debug logging with: import hearth.kindlings.catsderivation.debug.logDerivationForCatsDerivation"
        if (errorLogs.nonEmpty) s"Macro derivation failed:\n$errorsRendered\nlogs:\n$errorLogs\n$hint"
        else s"Macro derivation failed:\n$errorsRendered\n$hint"
      }
  }

  final case class PartialOrderCtx[A](
      tpe: Type[A],
      x: Expr[A],
      y: Expr[A],
      cache: MLocal[ValDefsCache],
      derivedType: Option[??]
  ) {
    def nest[B: Type](newX: Expr[B], newY: Expr[B]): PartialOrderCtx[B] =
      PartialOrderCtx(Type[B], newX, newY, cache, derivedType)
  }
  object PartialOrderCtx {
    def from[A: Type](x: Expr[A], y: Expr[A], derivedType: Option[??]): PartialOrderCtx[A] =
      PartialOrderCtx(Type[A], x, y, ValDefsCache.mlocal, derivedType)
  }

  def poctx[A](implicit A: PartialOrderCtx[A]): PartialOrderCtx[A] = A
  implicit def partialOrderCtxType[A: PartialOrderCtx]: Type[A] = poctx.tpe

  abstract class PartialOrderDerivationRule(val name: String) extends Rule {
    def apply[A: PartialOrderCtx]: MIO[Rule.Applicability[Expr[Double]]]
  }

  def derivePartialOrderRecursively[A: PartialOrderCtx]: MIO[Expr[Double]] =
    Log.namedScope(s"Deriving PartialOrder for ${Type[A].prettyPrint}") {
      Rules(
        PartialOrderUseCachedRule,
        PartialOrderUseImplicitRule,
        PartialOrderDerivationPolicyRule,
        PartialOrderValueTypeRule,
        PartialOrderSingletonRule,
        PartialOrderCaseClassRule,
        PartialOrderEnumRule
      )(_[A]).flatMap {
        case Right(result) => MIO.pure(result)
        case Left(reasons) =>
          val reasonsStrings = reasons.toListMap
            .removed(PartialOrderUseCachedRule)
            .view
            .map { case (rule, reasons) =>
              if (reasons.isEmpty) s"${rule.name} not applicable"
              else s" - ${rule.name}: ${reasons.mkString(", ")}"
            }
            .toList
          MIO.fail(PartialOrderDerivationError.UnsupportedType(Type[A].prettyPrint, reasonsStrings))
      }
    }

  object PartialOrderUseCachedRule extends PartialOrderDerivationRule("use cached PartialOrder") {
    def apply[A: PartialOrderCtx]: MIO[Rule.Applicability[Expr[Double]]] = {
      implicit val PartialOrderA: Type[cats.kernel.PartialOrder[A]] = PartialOrderTypes.PartialOrder[A]
      implicit val DoubleType: Type[Double] = PartialOrderTypes.Double
      poctx.cache.get0Ary[cats.kernel.PartialOrder[A]]("cached-partial-order-instance").flatMap {
        case Some(instance) =>
          MIO.pure(
            Rule.matched(
              Expr.quote(Expr.splice(instance).partialCompare(Expr.splice(poctx.x), Expr.splice(poctx.y)))
            )
          )
        case None =>
          poctx.cache.get2Ary[A, A, Double]("cached-partial-order-method").flatMap {
            case Some(helper) => MIO.pure(Rule.matched(helper(poctx.x, poctx.y)))
            case None         => MIO.pure(Rule.yielded(s"No cached PartialOrder for ${Type[A].prettyPrint}"))
          }
      }
    }
  }

  object PartialOrderUseImplicitRule extends PartialOrderDerivationRule("use implicit PartialOrder") {
    def apply[A: PartialOrderCtx]: MIO[Rule.Applicability[Expr[Double]]] = {
      implicit val PartialOrderA: Type[cats.kernel.PartialOrder[A]] = PartialOrderTypes.PartialOrder[A]
      if (poctx.derivedType.exists(_.Underlying =:= Type[A]))
        MIO.pure(Rule.yielded(s"${Type[A].prettyPrint} is the self-type"))
      else
        // Any `PartialOrder` (an `Order` is one too), like kittens' `PartialOrder |: Derived`.
        PartialOrderTypes.PartialOrder[A].summonExprIgnoring().toEither match {
          case Right(instanceExpr) =>
            poctx.cache.buildCachedWith(
              "cached-partial-order-instance",
              ValDefBuilder.ofLazy[cats.kernel.PartialOrder[A]](s"partialOrder_${Type[A].shortName}")
            )(_ => instanceExpr) >>
              PartialOrderUseCachedRule[A]
          case Left(reason) =>
            if (isStrictDerivationMode)
              MIO.fail(
                new StrictDerivationError(
                  s"Strict derivation mode: no implicit PartialOrder[${Type[A].prettyPrint}] found. Provide an explicit instance or remove StrictDerivation from scope."
                )
              )
            else
              MIO.pure(Rule.yielded(s"No implicit PartialOrder[${Type[A].prettyPrint}]: $reason"))
        }
    }
  }

  /** Root rule for the derivation policy (issue kubuszok/kindlings#85). */
  object PartialOrderDerivationPolicyRule extends PartialOrderDerivationRule("derivation policy") {
    def apply[A: PartialOrderCtx]: MIO[Rule.Applicability[Nothing]] =
      checkDerivationPolicyOncePerExpansionFor[A].map(_ => Rule.yielded())
  }

  @scala.annotation.nowarn("msg=is never used")
  object PartialOrderValueTypeRule extends PartialOrderDerivationRule("PartialOrder as value type") {
    def apply[A: PartialOrderCtx]: MIO[Rule.Applicability[Expr[Double]]] =
      Type[A] match {
        case IsValueType(isValueType) =>
          import isValueType.Underlying as Inner
          derivePartialOrderRecursively[Inner](using
            poctx.nest(isValueType.value.unwrap(poctx.x), isValueType.value.unwrap(poctx.y))
          ).map(Rule.matched(_))
        case _ =>
          MIO.pure(Rule.yielded(s"${Type[A].prettyPrint} is not a value type"))
      }
  }

  object PartialOrderSingletonRule extends PartialOrderDerivationRule("PartialOrder as singleton") {
    def apply[A: PartialOrderCtx]: MIO[Rule.Applicability[Expr[Double]]] =
      SingletonValue.parse[A].toEither match {
        case Right(_)     => MIO.pure(Rule.matched(Expr(0.0)))
        case Left(reason) => MIO.pure(Rule.yielded(reason.toString))
      }
  }

  object PartialOrderCaseClassRule extends PartialOrderDerivationRule("PartialOrder as case class") {
    def apply[A: PartialOrderCtx]: MIO[Rule.Applicability[Expr[Double]]] =
      CaseClass.parse[A].toEither match {
        case Right(caseClass) =>
          implicit val DoubleType: Type[Double] = PartialOrderTypes.Double
          val defBuilder = ValDefBuilder.ofDef2[A, A, Double](s"partialCompare_${Type[A].shortName}")
          for {
            _ <- poctx.cache.forwardDeclare("cached-partial-order-method", defBuilder)
            _ <- MIO.scoped { runSafe =>
              runSafe(poctx.cache.buildCachedWith("cached-partial-order-method", defBuilder) { case (_, (x, y)) =>
                runSafe(deriveCaseClassPartialOrder[A](caseClass, x, y))
              })
            }
            result <- PartialOrderUseCachedRule[A]
          } yield result
        case Left(reason) =>
          MIO.pure(Rule.yielded(reason.toString))
      }

    private def deriveCaseClassPartialOrder[A: PartialOrderCtx](
        caseClass: CaseClass[A],
        x: Expr[A],
        y: Expr[A]
    ): MIO[Expr[Double]] = {
      implicit val DoubleType: Type[Double] = PartialOrderTypes.Double
      val fieldsX = caseClass.caseFieldValuesAt(x).toList
      val fieldsY = caseClass.caseFieldValuesAt(y).toList
      NonEmptyList.fromList(fieldsX.zip(fieldsY)) match {
        case Some(fieldPairs) =>
          fieldPairs
            .traverse { case ((fieldName, fieldValueX), (_, fieldValueY)) =>
              import fieldValueX.Underlying as Field
              val fx = fieldValueX.value.asInstanceOf[Expr[Field]]
              val fy = fieldValueY.value.asInstanceOf[Expr[Field]]
              derivePartialOrderRecursively[Field](using poctx.nest(fx, fy)).map(r => (fieldName, r))
            }
            .map { results =>
              // The first result other than 0.0 decides - NaN included (NaN != 0.0), like kittens.
              results.toList.reverse.foldLeft(Expr(0.0): Expr[Double]) { case (rest, (_, cmp)) =>
                Expr.quote {
                  val c = Expr.splice(cmp)
                  if (c != 0.0) c else Expr.splice(rest)
                }
              }
            }
        case None =>
          MIO.pure(Expr(0.0))
      }
    }
  }

  object PartialOrderEnumRule extends PartialOrderDerivationRule("PartialOrder as enum") {
    def apply[A: PartialOrderCtx]: MIO[Rule.Applicability[Expr[Double]]] =
      Enum.parse[A].toEither match {
        case Right(enumm) =>
          implicit val DoubleType: Type[Double] = PartialOrderTypes.Double
          val defBuilder = ValDefBuilder.ofDef2[A, A, Double](s"partialCompare_${Type[A].shortName}")
          for {
            _ <- poctx.cache.forwardDeclare("cached-partial-order-method", defBuilder)
            _ <- MIO.scoped { runSafe =>
              runSafe(poctx.cache.buildCachedWith("cached-partial-order-method", defBuilder) { case (_, (x, y)) =>
                runSafe(deriveEnumPartialOrder[A](enumm, x, y))
              })
            }
            result <- PartialOrderUseCachedRule[A]
          } yield result
        case Left(reason) =>
          MIO.pure(Rule.yielded(reason.toString))
      }

    @scala.annotation.nowarn("msg=is unchecked")
    private def deriveEnumPartialOrder[A: PartialOrderCtx](
        enumm: Enum[A],
        x: Expr[A],
        y: Expr[A]
    ): MIO[Expr[Double]] = {
      implicit val DoubleType: Type[Double] = PartialOrderTypes.Double
      enumm
        .matchOn[MIO, Double](x) { matchedX =>
          import matchedX.{value as caseX, Underlying as EnumCase}
          SingletonValue.unapply(Type[EnumCase]) match {
            case Some(sv) =>
              // Reference equality: no unchecked type test on erased singleton types (Scala 3 enums).
              val singletonExpr = sv.singletonExpr
              MIO.pure(Expr.quote {
                if (Expr.splice(y).asInstanceOf[AnyRef] eq Expr.splice(singletonExpr).asInstanceOf[AnyRef]) 0.0
                else hearth.kindlings.catsderivation.internal.runtime.CatsDerivationFactories.incomparable
              })
            case None =>
              val caseY = Expr.quote(Expr.splice(y).asInstanceOf[EnumCase])
              val isInstance = Expr.quote(Expr.splice(y).isInstanceOf[EnumCase])
              derivePartialOrderRecursively[EnumCase](using poctx.nest(caseX, caseY)).map { innerCmp =>
                // Values of different subtypes are incomparable, like kittens.
                Expr.quote(
                  if (Expr.splice(isInstance)) Expr.splice(innerCmp)
                  else hearth.kindlings.catsderivation.internal.runtime.CatsDerivationFactories.incomparable
                )
              }
          }
        }
        .flatMap {
          case Some(result) => MIO.pure(result)
          case None         => MIO.fail(PartialOrderDerivationError.NoChildrenInSealedTrait(Type[A].prettyPrint))
        }
    }
  }

  protected object PartialOrderTypes {
    def PartialOrder: Type.Ctor1[cats.kernel.PartialOrder] = Type.Ctor1.of[cats.kernel.PartialOrder]
    val Double: Type.Lazy[Double] = Type.Lazy(Type.of[Double])
    val LogDerivation: Type[hearth.kindlings.catsderivation.LogDerivation] =
      Type.of[hearth.kindlings.catsderivation.LogDerivation]
  }

  def shouldWeLogPartialOrderDerivation: Boolean = {
    implicit val LogDerivationType: Type[LogDerivation] = PartialOrderTypes.LogDerivation
    Expr.summonImplicit[LogDerivation].isDefined || (for {
      data <- Environment.typedSettings.toOption
      cd <- data.get("catsDerivation")
      shouldLog <- cd.get("logDerivation").flatMap(_.asBoolean)
    } yield shouldLog).getOrElse(false)
  }
}

sealed private[compiletime] trait PartialOrderDerivationError
    extends util.control.NoStackTrace
    with Product
    with Serializable {
  def message: String
  override def getMessage(): String = message
}
private[compiletime] object PartialOrderDerivationError {
  final case class UnsupportedType(tpeName: String, reasons: List[String]) extends PartialOrderDerivationError {
    override def message: String = s"$tpeName not handled by any PartialOrder rule:\n${reasons.mkString("\n")}"
  }
  final case class NoChildrenInSealedTrait(tpeName: String) extends PartialOrderDerivationError {
    override def message: String = s"$tpeName has no children!"
  }
}
