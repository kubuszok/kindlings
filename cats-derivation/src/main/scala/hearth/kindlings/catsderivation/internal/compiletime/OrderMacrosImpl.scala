package hearth.kindlings.catsderivation.internal.compiletime

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

import hearth.kindlings.catsderivation.LogDerivation

/** Order derivation: lexicographic comparison of case class fields, ordinal comparison for enums. */
trait OrderMacrosImpl
    extends rules.OrderUseCachedRuleImpl
    with rules.OrderUseImplicitRuleImpl
    with rules.OrderBuiltInRuleImpl
    with rules.OrderValueTypeRuleImpl
    with rules.OrderSingletonRuleImpl
    with rules.OrderCaseClassRuleImpl
    with rules.OrderEnumRuleImpl
    with CatsDerivationTimeout { this: MacroCommons & StdExtensions =>

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
        errorRendering = if (shouldWeLogOrderDerivation) RenderFrom(Log.Level.Info) else DontRender,
        timeout = derivationTimeout
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
        OrderBuiltInRule,
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

  protected object OrderTypes {
    def Order: Type.Ctor1[cats.kernel.Order] = Type.Ctor1.of[cats.kernel.Order]
    val Boolean: Type[Boolean] = Type.of[Boolean]
    val Byte: Type[Byte] = Type.of[Byte]
    val Short: Type[Short] = Type.of[Short]
    val Int: Type[Int] = Type.of[Int]
    val Long: Type[Long] = Type.of[Long]
    val Float: Type[Float] = Type.of[Float]
    val Double: Type[Double] = Type.of[Double]
    val Char: Type[Char] = Type.of[Char]
    val String: Type[String] = Type.of[String]
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
