package hearth.kindlings.catstaglessderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

trait ContravariantKCaseClassRuleImpl {
  this: ContravariantKMacrosImpl & MacroCommons & StdExtensions =>

  import hearth.kindlings.catstaglessderivation.internal.runtime.CatsTaglessFactories

  object ContravariantKCaseClassRule extends ContravariantKDerivationRule("ContravariantK as case class") {
    def apply[Alg[_[_]]: ContravariantKCtx]: MIO[Rule.Applicability[Expr[cats.tagless.ContravariantK[Alg]]]] = {
      val WCtor1Ctor = mkWCtor1
      val AlgWCtor1Type = ckctx.algCtorK1.apply[CatsTaglessFactories.WCtor1](using WCtor1Ctor)

      CaseClass.parse(using AlgWCtor1Type).toEither match {
        case Right(_) =>
          implicit val CKAlgType: Type[cats.tagless.ContravariantK[Alg]] = ckctx.contravariantKAlgType
          for {
            _ <- MIO.scoped { runSafe =>
              val instanceExpr = buildContravariantKFactoryExpr[Alg](runSafe)
              runSafe {
                ckctx.cache.buildCachedWith(
                  "cached-contravariantk-instance",
                  ValDefBuilder.ofLazy[cats.tagless.ContravariantK[Alg]](
                    s"contravariantK_${ckctx.contravariantKAlgType.shortName}"
                  )
                )(_ => instanceExpr)
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
  private def buildContravariantKFactoryExpr[Alg[_[_]]](
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
              deriveContravariantKCaseClassBody[Alg](
                Expr.quote(af),
                Expr.quote(fk).asInstanceOf[Expr[Any]]
              )
            }
          }
      }
    }
  }
}
