package hearth.kindlings.catsderivation.internal.compiletime

import hearth.MacroCommons
import hearth.fp.data.NonEmptyList
import hearth.fp.effect.*
import hearth.fp.syntax.*
import hearth.std.*

import hearth.kindlings.catsderivation.LogDerivation

/** Order derivation: lexicographic comparison of case class fields, ordinal comparison for enums. */
trait OrderMacrosImpl { this: MacroCommons & StdExtensions =>

  @scala.annotation.nowarn("msg=is never used")
  def deriveOrder[A: Type]: Expr[cats.kernel.Order[A]] = {
    val macroName = "Order.derived"
    implicit val OrderA: Type[cats.kernel.Order[A]] = OrderTypes.Order[A]
    implicit val IntType: Type[Int] = OrderTypes.Int
    val selfType: Option[??] = Some(Type[A].as_??)

    deriveOrderEntrypoint[A, cats.kernel.Order[A]](macroName) { fromCtx =>
      Expr.quote {
        new cats.kernel.Order[A] {
          def compare(x: A, y: A): Int = {
            val _ = x
            val _ = y
            Expr.splice {
              fromCtx(OrderCtx.from(Expr.quote(x), Expr.quote(y), derivedType = selfType))
            }
          }
        }
      }
    }
  }

  protected def deriveOrderEntrypoint[A: Type, Out: Type](macroName: String)(
      provideCtxAndAdapt: (OrderCtx[A] => Expr[Int]) => Expr[Out]
  ): Expr[Out] = {
    if (Type[A] =:= Type.of[Nothing].asInstanceOf[Type[A]] || Type[A] =:= Type.of[Any].asInstanceOf[Type[A]])
      Environment.reportErrorAndAbort(
        s"$macroName: type parameter was inferred as ${Type[A].prettyPrint}, which is likely unintended."
      )

    Log
      .namedScope(s"Deriving Order[${Type[A].prettyPrint}] at: ${Environment.currentPosition.prettyPrint}") {
        MIO.scoped { runSafe =>
          val fromCtx: OrderCtx[A] => Expr[Int] = (ctx: OrderCtx[A]) =>
            runSafe {
              for {
                _ <- Environment.loadStandardExtensions().toMIO(allowFailures = false)
                result <- deriveOrderRecursively[A](using ctx)
                cache <- ctx.cache.get
              } yield cache.toValDefs.use(_ => result)
            }
          provideCtxAndAdapt(fromCtx)
        }
      }
      .flatTap(result => Log.info(s"Derived final result: ${result.prettyPrint}"))
      .runToExprOrFail(
        macroName,
        infoRendering = if (shouldWeLogOrderDerivation) RenderFrom(Log.Level.Info) else DontRender,
        errorRendering = if (shouldWeLogOrderDerivation) RenderFrom(Log.Level.Info) else DontRender
      ) { (errorLogs, errors) =>
        val errorsRendered = errors.map(e => "  - " + e.getMessage).mkString("\n")
        val hint =
          "Enable debug logging with: import hearth.kindlings.catsderivation.debug.logDerivationForCatsDerivation"
        if (errorLogs.nonEmpty) s"Macro derivation failed:\n$errorsRendered\nlogs:\n$errorLogs\n$hint"
        else s"Macro derivation failed:\n$errorsRendered\n$hint"
      }
  }

  final case class OrderCtx[A](
      tpe: Type[A],
      x: Expr[A],
      y: Expr[A],
      cache: MLocal[ValDefsCache],
      derivedType: Option[??]
  ) {
    def nest[B: Type](newX: Expr[B], newY: Expr[B]): OrderCtx[B] = OrderCtx(Type[B], newX, newY, cache, derivedType)
  }
  object OrderCtx {
    def from[A: Type](x: Expr[A], y: Expr[A], derivedType: Option[??]): OrderCtx[A] =
      OrderCtx(Type[A], x, y, ValDefsCache.mlocal, derivedType)
  }

  def octx[A](implicit A: OrderCtx[A]): OrderCtx[A] = A
  implicit def orderCtxType[A: OrderCtx]: Type[A] = octx.tpe

  abstract class OrderDerivationRule(val name: String) extends Rule {
    def apply[A: OrderCtx]: MIO[Rule.Applicability[Expr[Int]]]
  }

  def deriveOrderRecursively[A: OrderCtx]: MIO[Expr[Int]] =
    Log.namedScope(s"Deriving Order for ${Type[A].prettyPrint}") {
      Rules(
        OrderUseCachedRule,
        OrderUseImplicitRule,
        OrderValueTypeRule,
        OrderSingletonRule,
        OrderCaseClassRule,
        OrderEnumRule
      )(_[A]).flatMap {
        case Right(result) => MIO.pure(result)
        case Left(reasons) =>
          val reasonsStrings = reasons.toListMap
            .removed(OrderUseCachedRule)
            .view
            .map { case (rule, reasons) =>
              if (reasons.isEmpty) s"${rule.name} not applicable"
              else s" - ${rule.name}: ${reasons.mkString(", ")}"
            }
            .toList
          MIO.fail(OrderDerivationError.UnsupportedType(Type[A].prettyPrint, reasonsStrings))
      }
    }

  object OrderUseCachedRule extends OrderDerivationRule("use cached Order") {
    def apply[A: OrderCtx]: MIO[Rule.Applicability[Expr[Int]]] = {
      implicit val OrderA: Type[cats.kernel.Order[A]] = OrderTypes.Order[A]
      octx.cache.get0Ary[cats.kernel.Order[A]]("cached-order-instance").flatMap {
        case Some(instance) =>
          MIO.pure(
            Rule.matched(Expr.quote(Expr.splice(instance).compare(Expr.splice(octx.x), Expr.splice(octx.y))))
          )
        case None =>
          implicit val IntType: Type[Int] = OrderTypes.Int
          octx.cache.get2Ary[A, A, Int]("cached-order-method").flatMap {
            case Some(helper) => MIO.pure(Rule.matched(helper(octx.x, octx.y)))
            case None         => MIO.pure(Rule.yielded(s"No cached Order for ${Type[A].prettyPrint}"))
          }
      }
    }
  }

  object OrderUseImplicitRule extends OrderDerivationRule("use implicit Order") {
    def apply[A: OrderCtx]: MIO[Rule.Applicability[Expr[Int]]] = {
      implicit val OrderA: Type[cats.kernel.Order[A]] = OrderTypes.Order[A]
      if (octx.derivedType.exists(_.Underlying =:= Type[A]))
        MIO.pure(Rule.yielded(s"${Type[A].prettyPrint} is the self-type"))
      else
        OrderTypes.Order[A].summonExprIgnoring().toEither match {
          case Right(instanceExpr) =>
            octx.cache.buildCachedWith(
              "cached-order-instance",
              ValDefBuilder.ofLazy[cats.kernel.Order[A]](s"order_${Type[A].shortName}")
            )(_ => instanceExpr) >>
              OrderUseCachedRule[A]
          case Left(reason) =>
            MIO.pure(Rule.yielded(s"No implicit Order[${Type[A].prettyPrint}]: $reason"))
        }
    }
  }

  @scala.annotation.nowarn("msg=is never used")
  object OrderValueTypeRule extends OrderDerivationRule("Order as value type") {
    def apply[A: OrderCtx]: MIO[Rule.Applicability[Expr[Int]]] =
      Type[A] match {
        case IsValueType(isValueType) =>
          import isValueType.Underlying as Inner
          deriveOrderRecursively[Inner](using
            octx.nest(isValueType.value.unwrap(octx.x), isValueType.value.unwrap(octx.y))
          ).map(Rule.matched(_))
        case _ => MIO.pure(Rule.yielded(s"${Type[A].prettyPrint} is not a value type"))
      }
  }

  object OrderSingletonRule extends OrderDerivationRule("Order as singleton") {
    def apply[A: OrderCtx]: MIO[Rule.Applicability[Expr[Int]]] =
      SingletonValue.parse[A].toEither match {
        case Right(_) =>
          // Singletons always compare as equal
          MIO.pure(Rule.matched(Expr(0)))
        case Left(reason) =>
          MIO.pure(Rule.yielded(reason))
      }
  }

  object OrderCaseClassRule extends OrderDerivationRule("Order as case class") {
    def apply[A: OrderCtx]: MIO[Rule.Applicability[Expr[Int]]] =
      CaseClass.parse[A].toEither match {
        case Right(caseClass) =>
          implicit val IntType: Type[Int] = OrderTypes.Int
          val defBuilder = ValDefBuilder.ofDef2[A, A, Int](s"compare_${Type[A].shortName}")
          for {
            _ <- octx.cache.forwardDeclare("cached-order-method", defBuilder)
            _ <- MIO.scoped { runSafe =>
              runSafe(octx.cache.buildCachedWith("cached-order-method", defBuilder) { case (_, (x, y)) =>
                runSafe(deriveCaseClassOrder[A](caseClass, x, y))
              })
            }
            result <- OrderUseCachedRule[A]
          } yield result
        case Left(reason) => MIO.pure(Rule.yielded(reason))
      }

    // Lexicographic: compare field by field, return first non-zero
    private def deriveCaseClassOrder[A: OrderCtx](
        caseClass: CaseClass[A],
        x: Expr[A],
        y: Expr[A]
    ): MIO[Expr[Int]] = {
      val fieldsX = caseClass.caseFieldValuesAt(x).toList
      val fieldsY = caseClass.caseFieldValuesAt(y).toList

      NonEmptyList.fromList(fieldsX.zip(fieldsY)) match {
        case Some(fieldPairs) =>
          fieldPairs
            .traverse { case ((fieldName, fieldValueX), (_, fieldValueY)) =>
              import fieldValueX.Underlying as Field
              val fx = fieldValueX.value.asInstanceOf[Expr[Field]]
              val fy = fieldValueY.value.asInstanceOf[Expr[Field]]
              deriveOrderRecursively[Field](using octx.nest(fx, fy)).map(r => (fieldName, r))
            }
            .map { results =>
              // Lexicographic: val c1 = compare(f1x, f1y); if (c1 != 0) c1 else { val c2 = ...; ... }
              results.toList.reverse.foldLeft(Expr(0): Expr[Int]) { case (rest, (_, cmp)) =>
                Expr.quote {
                  val c = Expr.splice(cmp)
                  if (c != 0) c else Expr.splice(rest)
                }
              }
            }
        case None =>
          MIO.pure(Expr(0))
      }
    }
  }

  @scala.annotation.nowarn("msg=is never used")
  object OrderEnumRule extends OrderDerivationRule("Order as enum") {
    def apply[A: OrderCtx]: MIO[Rule.Applicability[Expr[Int]]] =
      Enum.parse[A].toEither match {
        case Right(enumm) =>
          implicit val IntType: Type[Int] = OrderTypes.Int
          val defBuilder = ValDefBuilder.ofDef2[A, A, Int](s"compare_${Type[A].shortName}")
          for {
            _ <- octx.cache.forwardDeclare("cached-order-method", defBuilder)
            _ <- MIO.scoped { runSafe =>
              runSafe(octx.cache.buildCachedWith("cached-order-method", defBuilder) { case (_, (x, y)) =>
                runSafe(deriveEnumOrder[A](enumm, x, y))
              })
            }
            result <- OrderUseCachedRule[A]
          } yield result
        case Left(reason) => MIO.pure(Rule.yielded(reason))
      }

    @scala.annotation.nowarn("msg=is unchecked")
    private def deriveEnumOrder[A: OrderCtx](
        enumm: Enum[A],
        x: Expr[A],
        y: Expr[A]
    ): MIO[Expr[Int]] = {
      implicit val IntType: Type[Int] = OrderTypes.Int
      enumm
        .matchOn[MIO, Int](x) { matchedX =>
          import matchedX.{value as caseX, Underlying as EnumCase}
          val caseY = Expr.quote(Expr.splice(y).asInstanceOf[EnumCase])
          val isInstance = Expr.quote(Expr.splice(y).isInstanceOf[EnumCase])
          deriveOrderRecursively[EnumCase](using octx.nest(caseX, caseY)).map { innerCmp =>
            Expr.quote {
              if (Expr.splice(isInstance)) Expr.splice(innerCmp)
              else {
                // Different cases: compare by hashCode as a stable ordering proxy
                java.lang.Integer.compare(Expr.splice(x).hashCode(), Expr.splice(y).hashCode())
              }
            }
          }
        }
        .flatMap {
          case Some(result) => MIO.pure(result)
          case None         => MIO.fail(OrderDerivationError.NoChildrenInSealedTrait(Type[A].prettyPrint))
        }
    }
  }

  protected object OrderTypes {
    def Order: Type.Ctor1[cats.kernel.Order] = Type.Ctor1.of[cats.kernel.Order]
    val Int: Type[Int] = Type.of[Int]
    val LogDerivation: Type[hearth.kindlings.catsderivation.LogDerivation] =
      Type.of[hearth.kindlings.catsderivation.LogDerivation]
  }

  def shouldWeLogOrderDerivation: Boolean = {
    implicit val LogDerivationType: Type[LogDerivation] = OrderTypes.LogDerivation
    Expr.summonImplicit[LogDerivation].isDefined || (for {
      data <- Environment.typedSettings.toOption
      cd <- data.get("catsDerivation")
      shouldLog <- cd.get("logDerivation").flatMap(_.asBoolean)
    } yield shouldLog).getOrElse(false)
  }
}

sealed private[compiletime] trait OrderDerivationError
    extends util.control.NoStackTrace
    with Product
    with Serializable {
  def message: String
  override def getMessage(): String = message
}
private[compiletime] object OrderDerivationError {
  final case class UnsupportedType(tpeName: String, reasons: List[String]) extends OrderDerivationError {
    override def message: String = s"$tpeName not handled by any Order rule:\n${reasons.mkString("\n")}"
  }
  final case class NoChildrenInSealedTrait(tpeName: String) extends OrderDerivationError {
    override def message: String = s"$tpeName has no children!"
  }
}
