package hearth.kindlings.catstaglessderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

trait FunctorKTraitRuleImpl {
  this: FunctorKMacrosImpl & MacroCommons & StdExtensions =>

  import hearth.kindlings.catstaglessderivation.internal.runtime.CatsTaglessFactories

  object FunctorKTraitRule extends FunctorKDerivationRule("FunctorK as trait") {
    def apply[Alg[_[_]]: FunctorKCtx]: MIO[Rule.Applicability[Expr[cats.tagless.FunctorK[Alg]]]] = {
      val WCtor1Ctor = mkWCtor1
      val AlgWCtor1Type = fkctx.algCtorK1.apply[CatsTaglessFactories.WCtor1](using WCtor1Ctor)

      AnonymousInstance.parse(using AlgWCtor1Type).toEither match {
        case Right(_) =>
          implicit val FKAlgType: Type[cats.tagless.FunctorK[Alg]] = fkctx.functorKAlgType
          for {
            _ <- MIO.scoped { runSafe =>
              val instanceExpr = buildFunctorKTraitExpr[Alg](runSafe)
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
  private def buildFunctorKTraitExpr[Alg[_[_]]](
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
              deriveFunctorKTraitBody[Alg](
                Expr.quote(af),
                Expr.quote(fk).asInstanceOf[Expr[Any]]
              )
            }
          }
      }
    }
  }

  @scala.annotation.nowarn("msg=is never used|unused implicit parameter")
  private[compiletime] def deriveFunctorKTraitBody[Alg[_[_]]](
      afExpr0: Expr[Alg[CatsTaglessFactories.WCtor1]],
      fkExpr0: Expr[Any]
  )(implicit ctx: FunctorKCtx[Alg]): MIO[Expr[Alg[CatsTaglessFactories.WCtor2]]] = {
    val AlgCtorK1 = ctx.algCtorK1

    val WCtor1Ctor = mkWCtor1
    val WCtor2Ctor = mkWCtor2

    val AlgWCtor1Type = AlgCtorK1.apply[CatsTaglessFactories.WCtor1](using WCtor1Ctor)
    val AlgWCtor2Type = AlgCtorK1.apply[CatsTaglessFactories.WCtor2](using WCtor2Ctor)

    buildTraitInstance[Alg](
      afExpr0.asInstanceOf[Expr[Any]],
      fkExpr0,
      AlgWCtor1Type.asInstanceOf[Type[Any]],
      AlgWCtor2Type.asInstanceOf[Type[Any]]
    )
  }

  @scala.annotation.nowarn("msg=is never used|unused implicit parameter")
  private def buildTraitInstance[Alg[_[_]]](
      afExpr: Expr[Any],
      fkExpr: Expr[Any],
      AlgWCtor1Type: Type[Any],
      AlgWCtor2Type: Type[Any]
  )(implicit ctx: FunctorKCtx[Alg]): MIO[Expr[Alg[CatsTaglessFactories.WCtor2]]] = {
    implicit val AnyType: Type[Any] = InvariantKTypes.Any
    val AlgCtorK1 = ctx.algCtorK1

    val OptionCtor = Type.Ctor1.of[Option]
    val ListCtor = Type.Ctor1.of[List]
    val AlgOptionType = AlgCtorK1.apply[Option](using OptionCtor)
    val AlgListType = AlgCtorK1.apply[List](using ListCtor)

    parsedOrFail(
      AnonymousInstance.parse(using AlgWCtor2Type.asInstanceOf[Type[Any]]).toEither,
      "Alg[WCtor2] as anonymous instance"
    )
      .parTuple(
        parsedOrFail(
          AnonymousInstance.parse(using AlgWCtor1Type.asInstanceOf[Type[Any]]).toEither,
          "Alg[WCtor1] as anonymous instance"
        )
      )
      .parTuple(
        parsedOrFail(AnonymousInstance.parse(using AlgOptionType.asInstanceOf[Type[Any]]).toEither, "Alg[Option]")
      )
      .parTuple(parsedOrFail(AnonymousInstance.parse(using AlgListType.asInstanceOf[Type[Any]]).toEither, "Alg[List]"))
      .flatMap { case (((anonInstance, anonInstanceSource), aiOption), aiList) =>
        val optionMethods = aiOption.mustOverride.map(cm => cm.method.name -> cm).toMap
        val listMethods = aiList.mustOverride.map(cm => cm.method.name -> cm).toMap

        // Pre-validate method shapes and source-method presence so the OverrideBody callbacks below
        // cannot run into unsupported methods.
        val validationErrors: List[CatsTaglessDerivationError] = anonInstance.mustOverride.flatMap { cm =>
          val methodName = cm.method.name

          val missingSource =
            if (anonInstanceSource.mustOverride.exists(_.method.name == methodName)) Nil
            else List(CatsTaglessDerivationError.SourceMethodNotFound(methodName))

          // Check if any parameter contains F[X] — FunctorK can't handle contravariant positions
          val hasContravariantParams: Boolean = (optionMethods.get(methodName), listMethods.get(methodName)) match {
            case (Some(om), Some(lm)) =>
              val optParams = om.method.totalParameters.flatten.toList
              val listParams = lm.method.totalParameters.flatten.toList
              optParams.zip(listParams).exists { case ((_, optP), (_, listP)) =>
                val tOpt = optP.tpe.Underlying
                val tList = listP.tpe.Underlying
                !(tOpt =:= tList)
              }
            case _ => false
          }
          val unsupportedMethod =
            if (hasContravariantParams)
              List(
                CatsTaglessDerivationError.UnsupportedMethod(
                  "FunctorK",
                  methodName,
                  "has F[_] in parameter position. " +
                    "FunctorK can only handle F[_] in return position (covariant). Use InvariantK instead."
                )
              )
            else Nil

          missingSource ++ unsupportedMethod
        }

        validationErrors match {
          case head :: tail =>
            Log.error((head :: tail).map(_.message).mkString("\n")) >> MIO.fail(head, tail*)
          case Nil =>

            val overrides: Map[UntypedMethod, OverrideBody] = anonInstance.mustOverride.map { cm =>
              val methodName = cm.method.name
              val sourceMethodOpt = anonInstanceSource.mustOverride.find(_.method.name == methodName)

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

                  // Call the same method on the source (af) instance.
                  // OverrideBody is a synchronous callback invoked inside `anonInstance.construct`; it cannot
                  // return MIO. Typed errors thrown here are caught by the MIO block wrapping `construct`
                  // below and routed into the MIO error channel.
                  val sourceCall: Expr[Any] = sourceMethodOpt match {
                    case Some(sourceCM) =>
                      val sm = sourceCM.method
                      try
                        sm.fold(
                          onInstance = oi => afExpr.as_??(oi.Instance.asInstanceOf[Type[Any]]),
                          onTypes = _ => Map.empty,
                          onValues = av => {
                            val paramNames = av.totalParameters.flatten.toList.map(_._1)
                            paramNames.zip(octx.parameters).toMap
                          }
                        ) match {
                          case Right(exprE) => import exprE.Underlying; exprE.value.upcast[Any]
                          case Left(e)      =>
                            throw CatsTaglessDerivationError.MethodForwardingFailed(
                              methodName,
                              s"fold returned Left: $e"
                            )
                        }
                      catch {
                        case e: CatsTaglessDerivationError  => throw e
                        case scala.util.control.NonFatal(e) =>
                          throw CatsTaglessDerivationError.MethodForwardingFailed(
                            methodName,
                            s"fold crashed (${sm.getClass.getSimpleName}, " +
                              s"expectations=${sm.expectations}, isNullary=${sm.isNullary}): ${e.getMessage}"
                          )
                      }
                    case None =>
                      // Unreachable: validated via validationErrors before building overrides.
                      throw CatsTaglessDerivationError.SourceMethodNotFound(methodName)
                  }

                  if (isDirectF) {
                    import returnT.Underlying as RT
                    mkApplyFk[RT](fkExpr, sourceCall)(returnT.Underlying).as_??(returnT.Underlying)
                  } else {
                    // Invariant method — return type is the same in source and target.
                    // Use Expr.quote with asInstanceOf to produce properly typed tree on Scala 2.
                    import returnT.Underlying as RT
                    Expr.quote(Expr.splice(sourceCall).asInstanceOf[RT]).as_??(returnT.Underlying)
                  }
                }
              }

              cm.method.asUntyped -> body
            }.toMap

            // The MIO wrapper captures typed errors thrown from the OverrideBody callbacks invoked
            // synchronously inside `construct` and routes them into the error channel.
            MIO(anonInstance.construct(None, Map.empty, overrides)).flatMap {
              case Right(expr) =>
                MIO.pure(expr.asInstanceOf[Expr[Alg[CatsTaglessFactories.WCtor2]]])
              case Left(errors) =>
                failCannotConstruct("FunctorK", s"trait instance: ${errors.toVector.mkString(", ")}")
            }
        }
      }
  }
}
