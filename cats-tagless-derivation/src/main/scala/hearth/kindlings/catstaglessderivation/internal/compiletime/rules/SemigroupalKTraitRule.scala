package hearth.kindlings.catstaglessderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

trait SemigroupalKTraitRuleImpl {
  this: SemigroupalKMacrosImpl & MacroCommons & StdExtensions =>

  import hearth.kindlings.catstaglessderivation.internal.runtime.CatsTaglessFactories

  object SemigroupalKTraitRule extends SemigroupalKDerivationRule("SemigroupalK as trait") {
    def apply[Alg[_[_]]: SemigroupalKCtx]: MIO[Rule.Applicability[Expr[cats.tagless.SemigroupalK[Alg]]]] = {
      val WCtor1Ctor = mkWCtor1
      val AlgWCtor1Type = skctx.algCtorK1.apply[CatsTaglessFactories.WCtor1](using WCtor1Ctor)

      AnonymousInstance.parse(using AlgWCtor1Type).toEither match {
        case Right(_) =>
          implicit val SKAlgType: Type[cats.tagless.SemigroupalK[Alg]] = skctx.semigroupalKAlgType
          for {
            _ <- MIO.scoped { runSafe =>
              runSafe {
                skctx.cache.buildCachedWith(
                  "cached-semigroupalk-instance",
                  ValDefBuilder.ofLazy[cats.tagless.SemigroupalK[Alg]](
                    s"semigroupalK_${skctx.semigroupalKAlgType.shortName}"
                  )
                )(_ => buildSemigroupalKTraitExpr[Alg](runSafe))
              }
            }
            result <- SemigroupalKUseCachedRule[Alg]
          } yield result
        case Left(reason) =>
          MIO.pure(Rule.yielded(reason.toString))
      }
    }
  }

  @scala.annotation.nowarn("msg=is never used|unused implicit parameter")
  private def buildSemigroupalKTraitExpr[Alg[_[_]]](
      runSafe: hearth.fp.DirectStyle.RunSafe[hearth.fp.effect.MIO]
  )(implicit ctx: SemigroupalKCtx[Alg]): Expr[cats.tagless.SemigroupalK[Alg]] = {
    implicit val AlgCtorK1: Type.CtorK1[Alg] = ctx.algCtorK1
    implicit val SKAlgType: Type[cats.tagless.SemigroupalK[Alg]] = ctx.semigroupalKAlgType
    Expr.quote {
      CatsTaglessFactories.semigroupalKInstance[Alg] {
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
              )
            }
          }
      }
    }
  }

  @scala.annotation.nowarn("msg=is never used|unused implicit parameter")
  private[compiletime] def deriveSemigroupalKTraitBody[Alg[_[_]]](
      afExpr0: Expr[Alg[CatsTaglessFactories.WCtor1]],
      agExpr0: Expr[Alg[CatsTaglessFactories.WCtor2]]
  )(implicit ctx: SemigroupalKCtx[Alg]): MIO[Expr[Alg[CatsTaglessFactories.WCtor3]]] = {
    val AlgCtorK1 = ctx.algCtorK1
    buildTraitAlgebra[Alg](
      "SemigroupalK",
      AlgCtorK1,
      AlgCtorK1.apply[CatsTaglessFactories.WCtor3](using mkWCtor3).asInstanceOf[Type[Any]],
      List(
        AlgCtorK1.apply[CatsTaglessFactories.WCtor1](using mkWCtor1).asInstanceOf[Type[Any]],
        AlgCtorK1.apply[CatsTaglessFactories.WCtor2](using mkWCtor2).asInstanceOf[Type[Any]]
      ),
      TraitOp.ProductK(afExpr0.asInstanceOf[Expr[Any]], agExpr0.asInstanceOf[Expr[Any]]),
      semigroupalKNestedDeriver(ctx.cache, ctx.derivedType)
    ).map(_.asInstanceOf[Expr[Alg[CatsTaglessFactories.WCtor3]]])
  }
}
