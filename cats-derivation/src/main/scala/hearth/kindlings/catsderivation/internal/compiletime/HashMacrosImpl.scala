package hearth.kindlings.catsderivation.internal.compiletime

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.fp.syntax.*
import hearth.std.*

/** Hash derivation: combines Eq (field-by-field equality) with Hash (MurmurHash3-based hashing). */
trait HashMacrosImpl
    extends EqMacrosImpl
    with rules.HashUseCachedRuleImpl
    with rules.HashUseImplicitRuleImpl
    with rules.HashBuiltInRuleImpl
    with rules.HashSingletonRuleImpl
    with rules.HashCaseClassRuleImpl
    with rules.HashEnumRuleImpl { this: MacroCommons & StdExtensions =>

  @scala.annotation.nowarn("msg=is never used")
  def deriveHash[A: Type]: Expr[cats.kernel.Hash[A]] = {
    val macroName = "Hash.derived"
    implicit val HashA: Type[cats.kernel.Hash[A]] = HashTypes.Hash[A]

    deriveHashEntrypoint[A, cats.kernel.Hash[A]](macroName)
  }

  @scala.annotation.nowarn("msg=is never used")
  private def deriveHashEntrypoint[A: Type, Out: Type](macroName: String): Expr[Out] = {
    implicit val BooleanType: Type[Boolean] = EqTypes.Boolean
    implicit val IntType: Type[Int] = HashTypes.Int
    val selfType: Option[??] = Some(Type[A].as_??)

    if (Type[A] =:= Type.of[Nothing].asInstanceOf[Type[A]] || Type[A] =:= Type.of[Any].asInstanceOf[Type[A]])
      Environment.reportErrorAndAbort(
        s"$macroName: type parameter was inferred as ${Type[A].prettyPrint}, which is likely unintended."
      )

    Log
      .namedScope(s"Deriving $macroName[${Type[A].prettyPrint}] at: ${Environment.currentPosition.prettyPrint}") {
        MIO.scoped { runSafe =>
          // Build hash lambda: A => Int
          val hashLambdaExpr: Expr[A => Int] = runSafe {
            LambdaBuilder
              .of1[A]("hashValue")
              .traverse { valueExpr =>
                val hashCtx = HashCtx.from(valueExpr, selfType)
                for {
                  _ <- Environment.loadStandardExtensions().toMIO(allowFailures = false)
                  result <- deriveHashRecursively[A](using hashCtx)
                  cache <- hashCtx.cache.get
                } yield cache.toValDefs.use(_ => result)
              }
              .map(_.build[Int])
          }

          // Build eqv lambda: (A, A) => Boolean
          val eqLambdaExpr: Expr[(A, A) => Boolean] = runSafe {
            val eqCache = ValDefsCache.mlocal
            LambdaBuilder
              .of2[A, A]("eqX", "eqY")
              .traverse { case (xExpr, yExpr) =>
                val ctx = EqCtx[A](Type[A], xExpr, yExpr, eqCache, selfType)
                for {
                  _ <- Environment.loadStandardExtensions().toMIO(allowFailures = false)
                  result <- deriveEqRecursively[A](using ctx)
                  cache <- ctx.cache.get
                } yield cache.toValDefs.use(_ => result)
              }
              .map(_.build[Boolean])
          }

          // Single Expr.quote with both lambdas spliced — avoids sibling splice isolation
          Expr
            .quote {
              new cats.kernel.Hash[A] {
                def hash(value: A): Int = Expr.splice(hashLambdaExpr).apply(value)
                def eqv(x: A, y: A): Boolean = Expr.splice(eqLambdaExpr).apply(x, y)
              }
            }
            .asInstanceOf[Expr[Out]]
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
        if (errorLogs.nonEmpty) s"Macro derivation failed:\n$errorsRendered\nlogs:\n$errorLogs\n$hint"
        else s"Macro derivation failed:\n$errorsRendered\n$hint"
      }
  }

  // Hash context — carries a single value to hash

  final case class HashCtx[A](
      tpe: Type[A],
      value: Expr[A],
      cache: MLocal[ValDefsCache],
      derivedType: Option[??]
  ) {
    def nest[B: Type](newValue: Expr[B]): HashCtx[B] = HashCtx(Type[B], newValue, cache, derivedType)
  }
  object HashCtx {
    def from[A: Type](value: Expr[A], derivedType: Option[??]): HashCtx[A] =
      HashCtx(Type[A], value, ValDefsCache.mlocal, derivedType)
  }

  def hctx[A](implicit A: HashCtx[A]): HashCtx[A] = A
  implicit def hashCtxType[A: HashCtx]: Type[A] = hctx.tpe

  abstract class HashDerivationRule(val name: String) extends Rule {
    def apply[A: HashCtx]: MIO[Rule.Applicability[Expr[Int]]]
  }

  // Recursive derivation

  def deriveHashRecursively[A: HashCtx]: MIO[Expr[Int]] =
    Log.namedScope(s"Deriving Hash for ${Type[A].prettyPrint}") {
      Rules(
        HashUseCachedRule,
        HashUseImplicitRule,
        HashBuiltInRule,
        HashSingletonRule,
        HashCaseClassRule,
        HashEnumRule
      )(_[A]).flatMap {
        case Right(result) =>
          Log.info(s"Derived Hash for ${Type[A].prettyPrint}") >> MIO.pure(result)
        case Left(reasons) =>
          val reasonsStrings = reasons.toListMap
            .removed(HashUseCachedRule)
            .view
            .map { case (rule, reasons) =>
              if (reasons.isEmpty) s"${rule.name} not applicable"
              else s" - ${rule.name}: ${reasons.mkString(", ")}"
            }
            .toList
          MIO.fail(HashDerivationError.UnsupportedType(Type[A].prettyPrint, reasonsStrings))
      }
    }

  protected object HashTypes {
    def Hash: Type.Ctor1[cats.kernel.Hash] = Type.Ctor1.of[cats.kernel.Hash]
    val Int: Type[Int] = Type.of[Int]
  }
}

sealed private[compiletime] trait HashDerivationError extends util.control.NoStackTrace with Product with Serializable {
  def message: String
  override def getMessage(): String = message
}
private[compiletime] object HashDerivationError {
  final case class UnsupportedType(tpeName: String, reasons: List[String]) extends HashDerivationError {
    override def message: String = s"$tpeName not handled by any Hash rule:\n${reasons.mkString("\n")}"
  }
}
