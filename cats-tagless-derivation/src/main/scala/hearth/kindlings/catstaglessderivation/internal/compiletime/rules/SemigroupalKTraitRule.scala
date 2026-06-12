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
              val instanceExpr = buildSemigroupalKTraitExpr[Alg](runSafe)
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

    val WCtor1Ctor = mkWCtor1
    val WCtor2Ctor = mkWCtor2
    val WCtor3Ctor = mkWCtor3

    val AlgWCtor1Type = AlgCtorK1.apply[CatsTaglessFactories.WCtor1](using WCtor1Ctor)
    val AlgWCtor2Type = AlgCtorK1.apply[CatsTaglessFactories.WCtor2](using WCtor2Ctor)
    val AlgWCtor3Type = AlgCtorK1.apply[CatsTaglessFactories.WCtor3](using WCtor3Ctor)

    buildSemigroupalKTraitInstance[Alg](
      afExpr0.asInstanceOf[Expr[Any]],
      agExpr0.asInstanceOf[Expr[Any]],
      AlgWCtor1Type.asInstanceOf[Type[Any]],
      AlgWCtor2Type.asInstanceOf[Type[Any]],
      AlgWCtor3Type.asInstanceOf[Type[Any]]
    )
  }

  @scala.annotation.nowarn("msg=is never used|unused implicit parameter|unused explicit parameter")
  private def buildSemigroupalKTraitInstance[Alg[_[_]]](
      afExpr: Expr[Any],
      agExpr: Expr[Any],
      AlgWCtor1Type: Type[Any],
      AlgWCtor2Type: Type[Any],
      AlgWCtor3Type: Type[Any]
  )(implicit ctx: SemigroupalKCtx[Alg]): MIO[Expr[Alg[CatsTaglessFactories.WCtor3]]] = {
    implicit val AnyType: Type[Any] = InvariantKTypes.Any
    val AlgCtorK1 = ctx.algCtorK1

    val OptionCtor = Type.Ctor1.of[Option]
    val ListCtor = Type.Ctor1.of[List]
    val AlgOptionType = AlgCtorK1.apply[Option](using OptionCtor)
    val AlgListType = AlgCtorK1.apply[List](using ListCtor)

    parsedOrFail(
      AnonymousInstance.parse(using AlgWCtor3Type.asInstanceOf[Type[Any]]).toEither,
      "Alg[WCtor3] as anonymous instance"
    )
      .parTuple(
        parsedOrFail(
          AnonymousInstance.parse(using AlgWCtor1Type.asInstanceOf[Type[Any]]).toEither,
          "Alg[WCtor1] as anonymous instance"
        )
      )
      .parTuple(
        parsedOrFail(
          AnonymousInstance.parse(using AlgWCtor2Type.asInstanceOf[Type[Any]]).toEither,
          "Alg[WCtor2] as anonymous instance"
        )
      )
      .parTuple(
        parsedOrFail(AnonymousInstance.parse(using AlgOptionType.asInstanceOf[Type[Any]]).toEither, "Alg[Option]")
      )
      .parTuple(parsedOrFail(AnonymousInstance.parse(using AlgListType.asInstanceOf[Type[Any]]).toEither, "Alg[List]"))
      .flatMap { case ((((anonInstance, anonInstanceSourceF), anonInstanceSourceG), aiOption), aiList) =>
        val optionMethods = aiOption.mustOverride.map(cm => cm.method.name -> cm).toMap
        val listMethods = aiList.mustOverride.map(cm => cm.method.name -> cm).toMap

        // Pre-validate that every method we must override exists on both source instances, so that the
        // OverrideBody callbacks below can never hit a missing source method.
        val missingSourceMethods: List[CatsTaglessDerivationError] = anonInstance.mustOverride
          .map(_.method.name)
          .flatMap { n =>
            val missingF =
              if (anonInstanceSourceF.mustOverride.exists(_.method.name == n)) Nil
              else List(CatsTaglessDerivationError.SourceMethodNotFound(n, " (F)"))
            val missingG =
              if (anonInstanceSourceG.mustOverride.exists(_.method.name == n)) Nil
              else List(CatsTaglessDerivationError.SourceMethodNotFound(n, " (G)"))
            missingF ++ missingG
          }

        missingSourceMethods match {
          case head :: tail =>
            Log.error((head :: tail).map(_.message).mkString("\n")) >> MIO.fail(head, tail*)
          case Nil =>

            val overrides: Map[UntypedMethod, OverrideBody] = anonInstance.mustOverride.map { cm =>
              val methodName = cm.method.name
              val sourceMethodOptF = anonInstanceSourceF.mustOverride.find(_.method.name == methodName)
              val sourceMethodOptG = anonInstanceSourceG.mustOverride.find(_.method.name == methodName)

              val isDirectF: Boolean = (optionMethods.get(methodName), listMethods.get(methodName)) match {
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

              val body: OverrideBody = new OverrideBody {
                def apply(octx: OverrideContext): Expr_?? = {
                  val returnT = octx.returnType

                  // OverrideBody is a synchronous callback invoked inside `anonInstance.construct`; it cannot
                  // return MIO. Typed errors thrown here are caught by the MIO block wrapping `construct`
                  // below and routed into the MIO error channel.
                  def foldSourceMethod(sm: Method, srcExpr: Expr[Any]): Expr[Any] =
                    try
                      sm.fold(
                        onInstance = oi => srcExpr.as_??(oi.Instance.asInstanceOf[Type[Any]]),
                        onTypes = _ => Map.empty,
                        onValues = av => {
                          val paramNames = av.totalParameters.flatten.toList.map(_._1)
                          paramNames.zip(octx.parameters).toMap
                        }
                      ) match {
                        case Right(exprE) => import exprE.Underlying; exprE.value.upcast[Any]
                        case Left(e)      =>
                          throw CatsTaglessDerivationError.MethodForwardingFailed(methodName, s"fold returned Left: $e")
                      }
                    catch {
                      case e: CatsTaglessDerivationError  => throw e
                      case scala.util.control.NonFatal(e) =>
                        throw CatsTaglessDerivationError.MethodForwardingFailed(
                          methodName,
                          s"fold crashed: ${e.getMessage}"
                        )
                    }

                  // The getOrElse branches are unreachable: validated via missingSourceMethods before
                  // building overrides.
                  if (isDirectF) {
                    val smF =
                      sourceMethodOptF.getOrElse(
                        throw CatsTaglessDerivationError.SourceMethodNotFound(methodName, " (F)")
                      )
                    val smG =
                      sourceMethodOptG.getOrElse(
                        throw CatsTaglessDerivationError.SourceMethodNotFound(methodName, " (G)")
                      )
                    val sourceCallF = foldSourceMethod(smF.method, afExpr)
                    val sourceCallG = foldSourceMethod(smG.method, agExpr)
                    import returnT.Underlying as RT
                    mkTuple2K[RT](sourceCallF, sourceCallG)(returnT.Underlying).as_??(returnT.Underlying)
                  } else {
                    // Invariant — take from af
                    val smF =
                      sourceMethodOptF.getOrElse(
                        throw CatsTaglessDerivationError.SourceMethodNotFound(methodName, " (F)")
                      )
                    val sourceCallF = foldSourceMethod(smF.method, afExpr)
                    import returnT.Underlying as RT
                    sourceCallF.asInstanceOf[Expr[RT]].as_??(returnT.Underlying)
                  }
                }
              }

              cm.method.asUntyped -> body
            }.toMap

            // The MIO wrapper captures typed errors thrown from the OverrideBody callbacks invoked
            // synchronously inside `construct` and routes them into the error channel.
            MIO(anonInstance.construct(None, Map.empty, overrides)).flatMap {
              case Right(expr) =>
                MIO.pure(expr.asInstanceOf[Expr[Alg[CatsTaglessFactories.WCtor3]]])
              case Left(errors) =>
                failCannotConstruct("SemigroupalK", s"trait instance: ${errors.toVector.mkString(", ")}")
            }
        }
      }
  }
}
