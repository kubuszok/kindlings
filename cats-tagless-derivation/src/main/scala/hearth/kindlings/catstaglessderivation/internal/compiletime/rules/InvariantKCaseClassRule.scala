package hearth.kindlings.catstaglessderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

trait InvariantKCaseClassRuleImpl {
  this: InvariantKMacrosImpl & MacroCommons & StdExtensions =>

  import hearth.kindlings.catstaglessderivation.internal.runtime.CatsTaglessFactories

  object InvariantKCaseClassRule extends InvariantKDerivationRule("InvariantK as case class") {
    def apply[Alg[_[_]]: InvariantKCtx]: MIO[Rule.Applicability[Expr[cats.tagless.InvariantK[Alg]]]] = {
      val WCtor1Ctor = mkWCtor1
      val AlgWCtor1Type: Type[Alg[CatsTaglessFactories.WCtor1]] =
        ikctx.algCtorK1.apply[CatsTaglessFactories.WCtor1](using WCtor1Ctor)

      CaseClass.parse(using AlgWCtor1Type).toEither match {
        case Right(_) =>
          implicit val IKAlgType: Type[cats.tagless.InvariantK[Alg]] = ikctx.invariantKAlgType
          for {
            _ <- MIO.scoped { runSafe =>
              val instanceExpr: Expr[cats.tagless.InvariantK[Alg]] =
                buildInvariantKFactoryExpr[Alg](runSafe)
              runSafe {
                ikctx.cache.buildCachedWith(
                  "cached-invariantk-instance",
                  ValDefBuilder.ofLazy[cats.tagless.InvariantK[Alg]](
                    s"invariantK_${ikctx.invariantKAlgType.shortName}"
                  )
                )(_ => instanceExpr)
              }
            }
            result <- InvariantKUseCachedRule[Alg]
          } yield result
        case Left(reason) =>
          MIO.pure(Rule.yielded(reason.toString))
      }
    }
  }

  @scala.annotation.nowarn("msg=is never used|unused implicit parameter")
  private def buildInvariantKFactoryExpr[Alg[_[_]]](
      runSafe: hearth.fp.DirectStyle.RunSafe[hearth.fp.effect.MIO]
  )(implicit ctx: InvariantKCtx[Alg]): Expr[cats.tagless.InvariantK[Alg]] = {
    implicit val AlgCtorK1: Type.CtorK1[Alg] = ctx.algCtorK1
    implicit val IKAlgType: Type[cats.tagless.InvariantK[Alg]] = ctx.invariantKAlgType
    Expr.quote {
      CatsTaglessFactories.invariantKInstance[Alg] {
        (
            af: Alg[CatsTaglessFactories.WCtor1],
            fk: cats.arrow.FunctionK[CatsTaglessFactories.WCtor1, CatsTaglessFactories.WCtor2],
            gk: cats.arrow.FunctionK[CatsTaglessFactories.WCtor2, CatsTaglessFactories.WCtor1]
        ) =>
          val _ = af
          val _ = fk
          val _ = gk
          Expr.splice {
            runSafe {
              deriveInvariantKCaseClassBody[Alg](
                Expr.quote(af),
                Expr.quote(fk).asInstanceOf[Expr[Any]],
                Expr.quote(gk).asInstanceOf[Expr[Any]]
              )
            }
          }
      }
    }
  }
}
