package hearth.kindlings.catstaglessderivation.internal.compiletime

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

trait ApplyKMacrosImpl extends FunctorKMacrosImpl with SemigroupalKMacrosImpl { this: MacroCommons & StdExtensions =>

  import hearth.kindlings.catstaglessderivation.internal.runtime.CatsTaglessFactories

  // --- Context ---

  final case class ApplyKCtx[Alg[_[_]]](
      algCtorK1: Type.CtorK1[Alg],
      applyKAlgType: Type[cats.tagless.ApplyK[Alg]],
      functorKCtx: FunctorKCtx[Alg],
      semigroupalKCtx: SemigroupalKCtx[Alg],
      cache: MLocal[ValDefsCache],
      derivedType: Option[??]
  )

  def akctx[Alg[_[_]]](implicit ctx: ApplyKCtx[Alg]): ApplyKCtx[Alg] = ctx

  // --- Entry point ---

  @scala.annotation.nowarn("msg=is never used|unused explicit parameter")
  def deriveApplyK[Alg[_[_]]](
      AlgCtorK10: Type.CtorK1[Alg],
      ApplyKAlgType: Type[cats.tagless.ApplyK[Alg]],
      FunctorKAlgType: Type[cats.tagless.FunctorK[Alg]],
      SemigroupalKAlgType: Type[cats.tagless.SemigroupalK[Alg]]
  ): Expr[cats.tagless.ApplyK[Alg]] = {
    val macroName = "ApplyK.derived"

    implicit val AlgCtorK1: Type.CtorK1[Alg] = AlgCtorK10
    hearth.fp.ignore(AlgCtorK1)

    val selfType: Option[??] = Some(ApplyKAlgType.as_??)

    Log
      .namedScope(s"Deriving ApplyK at: ${Environment.currentPosition.prettyPrint}") {
        MIO.scoped { runSafe =>
          val fromCtx: ApplyKCtx[Alg] => Expr[cats.tagless.ApplyK[Alg]] =
            (ctx: ApplyKCtx[Alg]) =>
              runSafe {
                for {
                  _ <- Environment.loadStandardExtensions().toMIO(allowFailures = false)
                  result <- deriveApplyKImpl[Alg](runSafe)(ctx)
                  cache <- ctx.cache.get
                } yield cache.toValDefs.use(_ => result)
              }

          val cache = ValDefsCache.mlocal
          val fkCtx = FunctorKCtx[Alg](AlgCtorK1, FunctorKAlgType, cache, selfType)
          val skCtx = SemigroupalKCtx[Alg](AlgCtorK1, SemigroupalKAlgType, cache, selfType)
          val ctx = ApplyKCtx[Alg](AlgCtorK1, ApplyKAlgType, fkCtx, skCtx, cache, selfType)
          fromCtx(ctx)
        }
      }
      .flatTap(result => Log.info(s"Derived final result: ${result.prettyPrint}"))
      .runToExprOrFail(
        macroName,
        infoRendering = if (shouldWeLogDerivation) RenderFrom(Log.Level.Info) else DontRender,
        errorRendering = if (shouldWeLogDerivation) RenderFrom(Log.Level.Info) else DontRender,
        timeout = derivationTimeout
      ) { (errorLogs, errors) =>
        val errorsRendered = errors.map(e => "  - " + e.getMessage).mkString("\n")
        val hint =
          "Enable debug logging with: import hearth.kindlings.catstaglessderivation.debug.logDerivationForCatsTaglessDerivation"
        if (errorLogs.nonEmpty) s"Macro derivation failed:\n$errorsRendered\nlogs:\n$errorLogs\n$hint"
        else s"Macro derivation failed:\n$errorsRendered\n$hint"
      }
  }

  @scala.annotation.nowarn("msg=is never used|unused implicit parameter|unused local definition")
  private def deriveApplyKImpl[Alg[_[_]]](
      runSafe: hearth.fp.DirectStyle.RunSafe[hearth.fp.effect.MIO]
  )(implicit ctx: ApplyKCtx[Alg]): MIO[Expr[cats.tagless.ApplyK[Alg]]] = {
    implicit val AlgCtorK1: Type.CtorK1[Alg] = ctx.algCtorK1
    implicit val AKAlgType: Type[cats.tagless.ApplyK[Alg]] = ctx.applyKAlgType

    val WCtor1Ctor = mkWCtor1
    val AlgWCtor1Type = ctx.algCtorK1.apply[CatsTaglessFactories.WCtor1](using WCtor1Ctor)

    // Check if it's a case class or a trait
    val isCaseClass = CaseClass.parse(using AlgWCtor1Type).toEither.isRight
    val isTrait = AnonymousInstance.parse(using AlgWCtor1Type).toEither.isRight

    if (!isCaseClass && !isTrait) {
      MIO.fail(
        new RuntimeException(
          s"Cannot derive ApplyK for ${ctx.applyKAlgType.prettyPrint}: " +
            "type is neither a case class nor a trait"
        )
      )
    } else if (isCaseClass) {
      buildApplyKCaseClassExpr[Alg](runSafe).map(identity)
    } else {
      buildApplyKTraitExpr[Alg](runSafe).map(identity)
    }
  }

  @scala.annotation.nowarn("msg=is never used|unused implicit parameter")
  private def buildApplyKCaseClassExpr[Alg[_[_]]](
      runSafe: hearth.fp.DirectStyle.RunSafe[hearth.fp.effect.MIO]
  )(implicit ctx: ApplyKCtx[Alg]): MIO[Expr[cats.tagless.ApplyK[Alg]]] = {
    implicit val AlgCtorK1: Type.CtorK1[Alg] = ctx.algCtorK1
    implicit val AKAlgType: Type[cats.tagless.ApplyK[Alg]] = ctx.applyKAlgType

    MIO.pure {
      Expr.quote {
        CatsTaglessFactories.applyKInstance[Alg](
          {
            (
                af: Alg[CatsTaglessFactories.WCtor1],
                fk: cats.arrow.FunctionK[CatsTaglessFactories.WCtor1, CatsTaglessFactories.WCtor2]
            ) =>
              val _ = af
              val _ = fk
              Expr.splice {
                runSafe {
                  deriveFunctorKCaseClassBody[Alg](
                    Expr.quote(af),
                    Expr.quote(fk).asInstanceOf[Expr[Any]]
                  )(ctx.functorKCtx)
                }
              }
          },
          {
            (
                af: Alg[CatsTaglessFactories.WCtor1],
                ag: Alg[CatsTaglessFactories.WCtor2]
            ) =>
              val _ = af
              val _ = ag
              Expr.splice {
                runSafe {
                  deriveSemigroupalKCaseClassBody[Alg](
                    Expr.quote(af),
                    Expr.quote(ag)
                  )(ctx.semigroupalKCtx)
                }
              }
          }
        )
      }
    }
  }

  @scala.annotation.nowarn("msg=is never used|unused implicit parameter")
  private def buildApplyKTraitExpr[Alg[_[_]]](
      runSafe: hearth.fp.DirectStyle.RunSafe[hearth.fp.effect.MIO]
  )(implicit ctx: ApplyKCtx[Alg]): MIO[Expr[cats.tagless.ApplyK[Alg]]] = {
    implicit val AlgCtorK1: Type.CtorK1[Alg] = ctx.algCtorK1
    implicit val AKAlgType: Type[cats.tagless.ApplyK[Alg]] = ctx.applyKAlgType

    MIO.pure {
      Expr.quote {
        CatsTaglessFactories.applyKInstance[Alg](
          {
            (
                af: Alg[CatsTaglessFactories.WCtor1],
                fk: cats.arrow.FunctionK[CatsTaglessFactories.WCtor1, CatsTaglessFactories.WCtor2]
            ) =>
              val _ = af
              val _ = fk
              Expr.splice {
                runSafe {
                  deriveFunctorKTraitBody[Alg](
                    Expr.quote(af),
                    Expr.quote(fk).asInstanceOf[Expr[Any]]
                  )(ctx.functorKCtx)
                }
              }
          },
          {
            (
                af: Alg[CatsTaglessFactories.WCtor1],
                ag: Alg[CatsTaglessFactories.WCtor2]
            ) =>
              val _ = af
              val _ = ag
              Expr.splice {
                runSafe {
                  deriveSemigroupalKTraitBody[Alg](
                    Expr.quote(af),
                    Expr.quote(ag)
                  )(ctx.semigroupalKCtx)
                }
              }
          }
        )
      }
    }
  }
}

sealed private[compiletime] trait ApplyKDerivationError
    extends util.control.NoStackTrace
    with Product
    with Serializable {
  def message: String
  override def getMessage(): String = message
}
private[compiletime] object ApplyKDerivationError {
  final case class UnsupportedType(tpeName: String, reasons: List[String]) extends ApplyKDerivationError {
    override def message: String =
      s"The type $tpeName was not handled by any ApplyK derivation rule:\n${reasons.mkString("\n")}"
  }
}
