package hearth.kindlings.catstaglessderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

trait ContravariantKTraitRuleImpl {
  this: ContravariantKMacrosImpl & MacroCommons & StdExtensions =>

  import hearth.kindlings.catstaglessderivation.internal.runtime.CatsTaglessFactories

  object ContravariantKTraitRule extends ContravariantKDerivationRule("ContravariantK as trait") {
    def apply[Alg[_[_]]: ContravariantKCtx]: MIO[Rule.Applicability[Expr[cats.tagless.ContravariantK[Alg]]]] = {
      val WCtor1Ctor = mkWCtor1
      val AlgWCtor1Type = ckctx.algCtorK1.apply[CatsTaglessFactories.WCtor1](using WCtor1Ctor)

      AnonymousInstance.parse(using AlgWCtor1Type).toEither match {
        case Right(_) =>
          implicit val CKAlgType: Type[cats.tagless.ContravariantK[Alg]] = ckctx.contravariantKAlgType
          for {
            _ <- MIO.scoped { runSafe =>
              runSafe {
                ckctx.cache.buildCachedWith(
                  "cached-contravariantk-instance",
                  ValDefBuilder.ofLazy[cats.tagless.ContravariantK[Alg]](
                    s"contravariantK_${ckctx.contravariantKAlgType.shortName}"
                  )
                )(_ => buildContravariantKTraitExpr[Alg](runSafe))
              }
            }
            result <- ContravariantKUseCachedRule[Alg]
          } yield result
        case Left(reason) =>
          MIO.pure(Rule.yielded(reason.toString))
      }
    }
  }

  @scala.annotation.nowarn("msg=is never used|unused implicit parameter")
  private def buildContravariantKTraitExpr[Alg[_[_]]](
      runSafe: hearth.fp.DirectStyle.RunSafe[hearth.fp.effect.MIO]
  )(implicit ctx: ContravariantKCtx[Alg]): Expr[cats.tagless.ContravariantK[Alg]] = {
    implicit val AlgCtorK1: Type.CtorK1[Alg] = ctx.algCtorK1
    implicit val CKAlgType: Type[cats.tagless.ContravariantK[Alg]] = ctx.contravariantKAlgType
    Expr.quote {
      CatsTaglessFactories.contravariantKInstance[Alg] {
        (
            af: Alg[CatsTaglessFactories.WCtor1],
            fk: cats.arrow.FunctionK[CatsTaglessFactories.WCtor2, CatsTaglessFactories.WCtor1]
        ) =>
          val _ = af
          val _ = fk
          Expr.splice {
            runSafe {
              deriveContravariantKTraitBody[Alg](
                Expr.quote(af),
                Expr.quote(fk).asInstanceOf[Expr[Any]]
              )
            }
          }
      }
    }
  }

  @scala.annotation.nowarn("msg=is never used|unused implicit parameter")
  private[compiletime] def deriveContravariantKTraitBody[Alg[_[_]]](
      afExpr0: Expr[Alg[CatsTaglessFactories.WCtor1]],
      fkExpr0: Expr[Any]
  )(implicit ctx: ContravariantKCtx[Alg]): MIO[Expr[Alg[CatsTaglessFactories.WCtor2]]] = {
    val AlgCtorK1 = ctx.algCtorK1
    buildTraitAlgebra[Alg](
      "ContravariantK",
      AlgCtorK1,
      AlgCtorK1.apply[CatsTaglessFactories.WCtor2](using mkWCtor2).asInstanceOf[Type[Any]],
      List(AlgCtorK1.apply[CatsTaglessFactories.WCtor1](using mkWCtor1).asInstanceOf[Type[Any]]),
      TraitOp.ContramapK(afExpr0.asInstanceOf[Expr[Any]], fkExpr0),
      contravariantKNestedDeriver(ctx.cache, ctx.derivedType)
    ).map(_.asInstanceOf[Expr[Alg[CatsTaglessFactories.WCtor2]]])
  }
}
