package hearth.kindlings.catstaglessderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

trait SemigroupalKCaseClassRuleImpl {
  this: SemigroupalKMacrosImpl & MacroCommons & StdExtensions =>

  import hearth.kindlings.catstaglessderivation.internal.runtime.CatsTaglessFactories

  object SemigroupalKCaseClassRule extends SemigroupalKDerivationRule("SemigroupalK as case class") {
    def apply[Alg[_[_]]: SemigroupalKCtx]: MIO[Rule.Applicability[Expr[cats.tagless.SemigroupalK[Alg]]]] = {
      val WCtor1Ctor = mkWCtor1
      val AlgWCtor1Type = skctx.algCtorK1.apply[CatsTaglessFactories.WCtor1](using WCtor1Ctor)

      CaseClass.parse(using AlgWCtor1Type).toEither match {
        case Right(_) =>
          implicit val SKAlgType: Type[cats.tagless.SemigroupalK[Alg]] = skctx.semigroupalKAlgType
          for {
            _ <- MIO.scoped { runSafe =>
              val instanceExpr = buildSemigroupalKFactoryExpr[Alg](runSafe)
              runSafe {
                skctx.cache.buildCachedWith(
                  "cached-semigroupalk-instance",
                  ValDefBuilder.ofLazy[cats.tagless.SemigroupalK[Alg]](
                    s"semigroupalK_${skctx.semigroupalKAlgType.shortName}"
                  )
                )(_ => instanceExpr)
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
  private def buildSemigroupalKFactoryExpr[Alg[_[_]]](
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
              deriveSemigroupalKCaseClassBody[Alg](
                Expr.quote(af),
                Expr.quote(ag)
              )
            }
          }
      }
    }
  }
}
