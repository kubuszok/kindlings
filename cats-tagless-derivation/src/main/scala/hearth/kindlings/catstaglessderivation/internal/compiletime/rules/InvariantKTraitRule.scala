package hearth.kindlings.catstaglessderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

trait InvariantKTraitRuleImpl {
  this: InvariantKMacrosImpl & MacroCommons & StdExtensions =>

  import hearth.kindlings.catstaglessderivation.internal.runtime.CatsTaglessFactories

  object InvariantKTraitRule extends InvariantKDerivationRule("InvariantK as trait") {
    def apply[Alg[_[_]]: InvariantKCtx]: MIO[Rule.Applicability[Expr[cats.tagless.InvariantK[Alg]]]] = {
      val WCtor1Ctor = mkWCtor1
      val AlgWCtor1Type = ikctx.algCtorK1.apply[CatsTaglessFactories.WCtor1](using WCtor1Ctor)

      AnonymousInstance.parse(using AlgWCtor1Type).toEither match {
        case Right(_) =>
          implicit val IKAlgType: Type[cats.tagless.InvariantK[Alg]] = ikctx.invariantKAlgType
          for {
            _ <- MIO.scoped { runSafe =>
              val instanceExpr = buildInvariantKTraitExpr[Alg](runSafe)
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
  private def buildInvariantKTraitExpr[Alg[_[_]]](
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
              deriveInvariantKTraitBody[Alg](
                Expr.quote(af),
                Expr.quote(fk).asInstanceOf[Expr[Any]],
                Expr.quote(gk).asInstanceOf[Expr[Any]]
              )
            }
          }
      }
    }
  }

  @scala.annotation.nowarn("msg=is never used|unused implicit parameter")
  private[compiletime] def deriveInvariantKTraitBody[Alg[_[_]]](
      afExpr0: Expr[Alg[CatsTaglessFactories.WCtor1]],
      fkExpr0: Expr[Any],
      gkExpr0: Expr[Any]
  )(implicit ctx: InvariantKCtx[Alg]): MIO[Expr[Alg[CatsTaglessFactories.WCtor2]]] = {
    val AlgCtorK1 = ctx.algCtorK1

    val WCtor1Ctor = mkWCtor1
    val WCtor2Ctor = mkWCtor2

    val AlgWCtor1Type = AlgCtorK1.apply[CatsTaglessFactories.WCtor1](using WCtor1Ctor)
    val AlgWCtor2Type = AlgCtorK1.apply[CatsTaglessFactories.WCtor2](using WCtor2Ctor)

    buildInvariantKTraitInstance[Alg](
      afExpr0.asInstanceOf[Expr[Any]],
      fkExpr0,
      gkExpr0,
      AlgWCtor1Type.asInstanceOf[Type[Any]],
      AlgWCtor2Type.asInstanceOf[Type[Any]]
    )
  }

  @scala.annotation.nowarn("msg=is never used|unused implicit parameter|unused explicit parameter")
  private def buildInvariantKTraitInstance[Alg[_[_]]](
      afExpr: Expr[Any],
      fkExpr: Expr[Any],
      gkExpr: Expr[Any],
      AlgWCtor1Type: Type[Any],
      AlgWCtor2Type: Type[Any]
  )(implicit ctx: InvariantKCtx[Alg]): MIO[Expr[Alg[CatsTaglessFactories.WCtor2]]] = {
    implicit val AnyType: Type[Any] = InvariantKTypes.Any
    val AlgCtorK1 = ctx.algCtorK1

    val OptionCtor = Type.Ctor1.of[Option]
    val ListCtor = Type.Ctor1.of[List]
    val AlgOptionType = AlgCtorK1.apply[Option](using OptionCtor)
    val AlgListType = AlgCtorK1.apply[List](using ListCtor)

    val anonInstance = AnonymousInstance.parse(using AlgWCtor2Type.asInstanceOf[Type[Any]]).toEither match {
      case Right(ai) => ai
      case Left(e)   => throw new RuntimeException(s"Cannot parse Alg[WCtor2] as anonymous instance: $e")
    }

    val anonInstanceSource = AnonymousInstance.parse(using AlgWCtor1Type.asInstanceOf[Type[Any]]).toEither match {
      case Right(ai) => ai
      case Left(e)   => throw new RuntimeException(s"Cannot parse Alg[WCtor1] as anonymous instance: $e")
    }

    val aiOption = AnonymousInstance.parse(using AlgOptionType.asInstanceOf[Type[Any]]).toEither match {
      case Right(ai) => ai; case Left(e) => throw new RuntimeException(s"Cannot parse Alg[Option]: $e")
    }
    val aiList = AnonymousInstance.parse(using AlgListType.asInstanceOf[Type[Any]]).toEither match {
      case Right(ai) => ai; case Left(e) => throw new RuntimeException(s"Cannot parse Alg[List]: $e")
    }

    val optionMethods = aiOption.mustOverride.map(cm => cm.method.name -> cm).toMap
    val listMethods = aiList.mustOverride.map(cm => cm.method.name -> cm).toMap

    val overrides: Map[UntypedMethod, OverrideBody] = anonInstance.mustOverride.map { cm =>
      val methodName = cm.method.name
      val sourceMethodOpt = anonInstanceSource.mustOverride.find(_.method.name == methodName)

      val isCovariantReturn: Boolean = (optionMethods.get(methodName), listMethods.get(methodName)) match {
        case (Some(om), Some(lm)) =>
          (om.method.knownReturning, lm.method.knownReturning) match {
            case (Some(or), Some(lr)) =>
              (
                OptionCtor.unapply(or.Underlying.asInstanceOf[Type[Any]]),
                ListCtor.unapply(lr.Underlying.asInstanceOf[Type[Any]])
              ) match {
                case (Some(io), Some(il)) =>
                  import io.Underlying as IO; import il.Underlying as IL
                  IO =:= IL
                case _ => false
              }
            case _ => false
          }
        case _ => false
      }

      // Classify parameters: which params contain F[X] (contravariant position)
      val contravariantParamIndices: Set[Int] = (optionMethods.get(methodName), listMethods.get(methodName)) match {
        case (Some(om), Some(lm)) =>
          val optParams = om.method.totalParameters.flatten.toList
          val listParams = lm.method.totalParameters.flatten.toList
          optParams
            .zip(listParams)
            .zipWithIndex
            .collect {
              case (((_, optP), (_, listP)), idx) if !(optP.tpe.Underlying =:= listP.tpe.Underlying) =>
                idx
            }
            .toSet
        case _ => Set.empty
      }

      val body: OverrideBody = new OverrideBody {
        def apply(octx: OverrideContext): Expr_?? = {
          val returnT = octx.returnType

          // Build params for the source call, transforming contravariant params with gk (G ~> F)
          val transformedParams: List[Expr_??] = octx.parameters.zipWithIndex.map { case (paramExpr, idx) =>
            if (contravariantParamIndices.contains(idx)) {
              import paramExpr.Underlying as P
              val transformed = mkApplyFk[P](gkExpr, paramExpr.value.upcast[Any])(paramExpr.Underlying)
              transformed.as_??(paramExpr.Underlying)
            } else {
              paramExpr
            }
          }

          // Call the same method on the source (af) instance with transformed params
          val sourceCall: Expr[Any] = sourceMethodOpt match {
            case Some(sourceCM) =>
              val sm = sourceCM.method
              try
                sm.fold(
                  onInstance = oi => afExpr.as_??(oi.Instance.asInstanceOf[Type[Any]]),
                  onTypes = _ => Map.empty,
                  onValues = av => {
                    val paramNames = av.totalParameters.flatten.toList.map(_._1)
                    paramNames.zip(transformedParams).toMap
                  }
                ) match {
                  case Right(exprE) => import exprE.Underlying; exprE.value.upcast[Any]
                  case Left(e)      => throw new RuntimeException(s"fold returned Left for $methodName: $e")
                }
              catch {
                case e: RuntimeException => throw e
                case e: Throwable        =>
                  throw new RuntimeException(
                    s"fold crashed for method $methodName (${sm.getClass.getSimpleName}, " +
                      s"expectations=${sm.expectations}, isNullary=${sm.isNullary}): ${e.getMessage}",
                    e
                  )
              }
            case None =>
              throw new RuntimeException(s"Source method $methodName not found")
          }

          if (isCovariantReturn) {
            import returnT.Underlying as RT
            mkApplyFk[RT](fkExpr, sourceCall)(returnT.Underlying).as_??(returnT.Underlying)
          } else {
            import returnT.Underlying as RT
            Expr.quote(Expr.splice(sourceCall).asInstanceOf[RT]).as_??(returnT.Underlying)
          }
        }
      }

      cm.method.asUntyped -> body
    }.toMap

    anonInstance.construct(None, Map.empty, overrides) match {
      case Right(expr) =>
        MIO.pure(expr.asInstanceOf[Expr[Alg[CatsTaglessFactories.WCtor2]]])
      case Left(errors) =>
        MIO.fail(
          new RuntimeException(
            s"Cannot construct InvariantK trait instance: ${errors.toVector.mkString(", ")}"
          )
        )
    }
  }
}
