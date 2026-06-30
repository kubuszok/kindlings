package hearth.kindlings.catstaglessderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

trait FunctorKCaseClassRuleImpl {
  this: FunctorKMacrosImpl & MacroCommons & StdExtensions =>

  import hearth.kindlings.catstaglessderivation.internal.runtime.CatsTaglessFactories

  object FunctorKCaseClassRule extends FunctorKDerivationRule("FunctorK as case class") {
    def apply[Alg[_[_]]: FunctorKCtx]: MIO[Rule.Applicability[Expr[cats.tagless.FunctorK[Alg]]]] = {
      val WCtor1Ctor = mkWCtor1
      val AlgWCtor1Type = fkctx.algCtorK1.apply[CatsTaglessFactories.WCtor1](using WCtor1Ctor)

      CaseClass.parse(using AlgWCtor1Type).toEither match {
        case Right(_) =>
          implicit val FKAlgType: Type[cats.tagless.FunctorK[Alg]] = fkctx.functorKAlgType
          for {
            _ <- MIO.scoped { runSafe =>
              val instanceExpr = buildFunctorKFactoryExpr[Alg](runSafe)
              runSafe {
                fkctx.cache.buildCachedWith(
                  "cached-functork-instance",
                  ValDefBuilder.ofLazy[cats.tagless.FunctorK[Alg]](
                    s"functorK_${fkctx.functorKAlgType.shortName}"
                  )
                )(_ => instanceExpr)
              }
            }
            result <- FunctorKUseCachedRule[Alg]
          } yield result
        case Left(reason) =>
          MIO.pure(Rule.yielded(reason.toString))
      }
    }
  }

  @scala.annotation.nowarn("msg=is never used|unused implicit parameter")
  private def buildFunctorKFactoryExpr[Alg[_[_]]](
      runSafe: hearth.fp.DirectStyle.RunSafe[hearth.fp.effect.MIO]
  )(implicit ctx: FunctorKCtx[Alg]): Expr[cats.tagless.FunctorK[Alg]] = {
    implicit val AlgCtorK1: Type.CtorK1[Alg] = ctx.algCtorK1
    implicit val FKAlgType: Type[cats.tagless.FunctorK[Alg]] = ctx.functorKAlgType
    Expr.quote {
      CatsTaglessFactories.functorKInstance[Alg] {
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
              )
            }
          }
      }
    }
  }
}
