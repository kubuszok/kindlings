package hearth.kindlings.catstaglessderivation.internal.compiletime

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

trait InstrumentMacrosImpl extends FunctorKMacrosImpl { this: MacroCommons & StdExtensions =>

  import hearth.kindlings.catstaglessderivation.internal.runtime.CatsTaglessFactories

  // --- Context ---

  final case class InstrumentCtx[Alg[_[_]]](
      algCtorK1: Type.CtorK1[Alg],
      instrumentAlgType: Type[cats.tagless.aop.Instrument[Alg]],
      functorKCtx: FunctorKCtx[Alg],
      algebraName: String,
      cache: MLocal[ValDefsCache],
      derivedType: Option[??]
  )

  def instctx[Alg[_[_]]](implicit ctx: InstrumentCtx[Alg]): InstrumentCtx[Alg] = ctx

  // --- Entry point ---

  @scala.annotation.nowarn("msg=is never used|unused explicit parameter")
  def deriveInstrument[Alg[_[_]]](
      AlgCtorK10: Type.CtorK1[Alg],
      InstrumentAlgType: Type[cats.tagless.aop.Instrument[Alg]],
      FunctorKAlgType: Type[cats.tagless.FunctorK[Alg]],
      algebraName: String
  ): Expr[cats.tagless.aop.Instrument[Alg]] = {
    val macroName = "Instrument.derived"

    implicit val AlgCtorK1: Type.CtorK1[Alg] = AlgCtorK10
    hearth.fp.ignore(AlgCtorK1)

    val selfType: Option[??] = Some(InstrumentAlgType.as_??)

    Log
      .namedScope(s"Deriving Instrument at: ${Environment.currentPosition.prettyPrint}") {
        MIO.scoped { runSafe =>
          val fromCtx: InstrumentCtx[Alg] => Expr[cats.tagless.aop.Instrument[Alg]] =
            (ctx: InstrumentCtx[Alg]) =>
              runSafe {
                for {
                  _ <- Environment.loadStandardExtensions().toMIO(allowFailures = false)
                  result <- deriveInstrumentImpl[Alg](runSafe)(ctx)
                  cache <- ctx.cache.get
                } yield cache.toValDefs.use(_ => result)
              }

          val cache = ValDefsCache.mlocal
          val fkCtx = FunctorKCtx[Alg](AlgCtorK1, FunctorKAlgType, cache, selfType)
          val ctx = InstrumentCtx[Alg](AlgCtorK1, InstrumentAlgType, fkCtx, algebraName, cache, selfType)
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
  private def deriveInstrumentImpl[Alg[_[_]]](
      runSafe: hearth.fp.DirectStyle.RunSafe[hearth.fp.effect.MIO]
  )(implicit ctx: InstrumentCtx[Alg]): MIO[Expr[cats.tagless.aop.Instrument[Alg]]] = {
    implicit val AlgCtorK1: Type.CtorK1[Alg] = ctx.algCtorK1
    implicit val InstAlgType: Type[cats.tagless.aop.Instrument[Alg]] = ctx.instrumentAlgType

    val WCtor1Ctor = mkWCtor1
    val AlgWCtor1Type = ctx.algCtorK1.apply[CatsTaglessFactories.WCtor1](using WCtor1Ctor)

    // Check if it's a case class or a trait
    val isCaseClass = CaseClass.parse(using AlgWCtor1Type).toEither.isRight
    val isTrait = AnonymousInstance.parse(using AlgWCtor1Type).toEither.isRight

    if (!isCaseClass && !isTrait) {
      val err = CatsTaglessDerivationError.NotCaseClassOrTrait("Instrument", ctx.instrumentAlgType.prettyPrint)
      Log.error(err.message) >> MIO.fail(err)
    } else if (isCaseClass) {
      buildInstrumentCaseClassExpr[Alg](runSafe)
    } else {
      buildInstrumentTraitExpr[Alg](runSafe)
    }
  }

  // --- Case class: wrap each F[X] field in Instrumentation(fieldName, af.field) ---

  @scala.annotation.nowarn("msg=is never used|unused implicit parameter")
  private def buildInstrumentCaseClassExpr[Alg[_[_]]](
      runSafe: hearth.fp.DirectStyle.RunSafe[hearth.fp.effect.MIO]
  )(implicit ctx: InstrumentCtx[Alg]): MIO[Expr[cats.tagless.aop.Instrument[Alg]]] = {
    implicit val AlgCtorK1: Type.CtorK1[Alg] = ctx.algCtorK1
    implicit val InstAlgType: Type[cats.tagless.aop.Instrument[Alg]] = ctx.instrumentAlgType
    val algName = ctx.algebraName

    MIO.pure {
      Expr.quote {
        CatsTaglessFactories.instrumentInstance[Alg](
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
          { (af: Alg[CatsTaglessFactories.WCtor1]) =>
            val _ = af
            Expr.splice {
              runSafe {
                deriveInstrumentCaseClassBody[Alg](
                  Expr.quote(af),
                  algName
                )
              }
            }
          }
        )
      }
    }
  }

  @scala.annotation.nowarn("msg=is never used|unused implicit parameter")
  private[compiletime] def deriveInstrumentCaseClassBody[Alg[_[_]]](
      afExpr: Expr[Alg[CatsTaglessFactories.WCtor1]],
      algebraName: String
  )(implicit ctx: InstrumentCtx[Alg]): MIO[Expr[Alg[CatsTaglessFactories.WCtor2]]] = {
    val AlgCtorK1 = ctx.algCtorK1
    implicit val AnyType: Type[Any] = InvariantKTypes.Any

    val WCtor1Ctor = mkWCtor1
    val WCtor2Ctor = mkWCtor2

    val AlgWCtor1Type = AlgCtorK1.apply[CatsTaglessFactories.WCtor1](using WCtor1Ctor)
    val AlgWCtor2Type = AlgCtorK1.apply[CatsTaglessFactories.WCtor2](using WCtor2Ctor)

    val OptionCtor = Type.Ctor1.of[Option]
    val ListCtor = Type.Ctor1.of[List]
    val AlgOptionType = AlgCtorK1.apply[Option](using OptionCtor)
    val AlgListType = AlgCtorK1.apply[List](using ListCtor)

    parsedOrFail(CaseClass.parse(using AlgOptionType).toEither, "Alg[Option]")
      .parTuple(parsedOrFail(CaseClass.parse(using AlgListType).toEither, "Alg[List]"))
      .flatMap { case (ccOption, ccList) =>
        val fieldsOption = ccOption.primaryConstructor.parameters.flatten.toList
        val fieldsList = ccList.primaryConstructor.parameters.flatten.toList

        // Classify fields: direct F[X] fields get wrapped in Instrumentation
        val directFields = scala.collection.mutable.Set.empty[String]

        fieldsOption.zip(fieldsList).foreach { case ((name, pOption), (_, pList)) =>
          val tOption = pOption.tpe.Underlying
          val tList = pList.tpe.Underlying
          val optionUnapply = OptionCtor.unapply(tOption.asInstanceOf[Type[Any]])
          val listUnapply = ListCtor.unapply(tList.asInstanceOf[Type[Any]])

          (optionUnapply, listUnapply) match {
            case (Some(innerOption), Some(innerList)) =>
              import innerOption.Underlying as InnerO
              import innerList.Underlying as InnerL
              if (InnerO =:= InnerL) directFields += name
            case _ => // invariant or nested — pass through
          }
        }

        parsedOrFail(CaseClass.parse(using AlgWCtor1Type).toEither, "Alg[WCtor1]")
          .parTuple(parsedOrFail(CaseClass.parse(using AlgWCtor2Type).toEither, "Alg[WCtor2]"))
          .flatMap { case (sourceCC, targetCC) =>
            val sourceFields = sourceCC.caseFieldValuesAt(afExpr).toList
            val targetParamTypes: Map[String, Type[Any]] = targetCC.primaryConstructor.parameters.flatten.toList.map {
              case (n, p) => import p.tpe.Underlying as PF; (n, Type[PF].asInstanceOf[Type[Any]])
            }.toMap

            val directFieldSet = directFields.toSet

            val mappedFields: List[(String, Expr_??)] = sourceFields.map { case (fieldName, fieldValue) =>
              import fieldValue.Underlying as Field
              val fieldExpr = fieldValue.value.asInstanceOf[Expr[Field]]

              if (directFieldSet.contains(fieldName)) {
                val tgt: Type[Field] = targetParamTypes(fieldName).asInstanceOf[Type[Field]]
                val mapped = mkInstrumentation[Field](fieldExpr.upcast[Any], algebraName, fieldName)(tgt)
                (fieldName, mapped.as_??(tgt))
              } else {
                (fieldName, fieldExpr.as_??)
              }
            }

            foldInstanceFree(targetCC.primaryConstructor, "Constructor")(
              onTypes = _ => Map.empty,
              onValues = _ => mappedFields.toMap
            ) match {
              case Right(constructExpr) =>
                import constructExpr.Underlying; MIO.pure(constructExpr.value.upcast(using implicitly, AlgWCtor2Type))
              case Left(error) => failCannotConstruct("Instrument", error)
            }
          }
      }
  }

  // --- Trait: wrap each F[X] method return in Instrumentation(methodName, af.method(args)) ---

  @scala.annotation.nowarn("msg=is never used|unused implicit parameter")
  private def buildInstrumentTraitExpr[Alg[_[_]]](
      runSafe: hearth.fp.DirectStyle.RunSafe[hearth.fp.effect.MIO]
  )(implicit ctx: InstrumentCtx[Alg]): MIO[Expr[cats.tagless.aop.Instrument[Alg]]] = {
    implicit val AlgCtorK1: Type.CtorK1[Alg] = ctx.algCtorK1
    implicit val InstAlgType: Type[cats.tagless.aop.Instrument[Alg]] = ctx.instrumentAlgType
    val algName = ctx.algebraName

    MIO.pure {
      Expr.quote {
        CatsTaglessFactories.instrumentInstance[Alg](
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
          { (af: Alg[CatsTaglessFactories.WCtor1]) =>
            val _ = af
            Expr.splice {
              runSafe {
                deriveInstrumentTraitBody[Alg](
                  Expr.quote(af),
                  algName
                )
              }
            }
          }
        )
      }
    }
  }

  @scala.annotation.nowarn("msg=is never used|unused implicit parameter")
  private[compiletime] def deriveInstrumentTraitBody[Alg[_[_]]](
      afExpr0: Expr[Alg[CatsTaglessFactories.WCtor1]],
      algebraName: String
  )(implicit ctx: InstrumentCtx[Alg]): MIO[Expr[Alg[CatsTaglessFactories.WCtor2]]] = {
    val AlgCtorK1 = ctx.algCtorK1
    implicit val AnyType: Type[Any] = InvariantKTypes.Any

    val WCtor1Ctor = mkWCtor1
    val WCtor2Ctor = mkWCtor2

    val AlgWCtor1Type = AlgCtorK1.apply[CatsTaglessFactories.WCtor1](using WCtor1Ctor)
    val AlgWCtor2Type = AlgCtorK1.apply[CatsTaglessFactories.WCtor2](using WCtor2Ctor)

    val afExpr = afExpr0.asInstanceOf[Expr[Any]]

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

        // Pre-validate that every method we must override exists on the source instance, so that the
        // OverrideBody callbacks below can never hit a missing source method.
        val missingSourceMethods: List[CatsTaglessDerivationError] = anonInstance.mustOverride
          .map(_.method.name)
          .filterNot(n => anonInstanceSource.mustOverride.exists(_.method.name == n))
          .map(n => CatsTaglessDerivationError.SourceMethodNotFound(n))

        missingSourceMethods match {
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

                  // Call the same method on the source (af) instance
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
                      // Unreachable: validated via missingSourceMethods before building overrides.
                      throw CatsTaglessDerivationError.SourceMethodNotFound(methodName)
                  }

                  if (isDirectF) {
                    import returnT.Underlying as RT
                    mkInstrumentation[RT](sourceCall, algebraName, methodName)(returnT.Underlying).as_??(
                      returnT.Underlying
                    )
                  } else {
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
                failCannotConstruct("Instrument", s"trait instance: ${errors.toVector.mkString(", ")}")
            }
        }
      }
  }

  // --- Helpers ---

  @scala.annotation.nowarn("msg=is never used|unused implicit parameter")
  private[compiletime] def mkInstrumentation[Result](
      valueExpr: Expr[Any],
      algebraName: String,
      methodName: String
  )(implicit ResultType: Type[Result]): Expr[Result] = {
    val algNameExpr = Expr(algebraName)
    val methodNameExpr = Expr(methodName)
    Expr.quote {
      hearth.kindlings.catstaglessderivation.internal.runtime.CatsTaglessRuntime
        .mkInstrumentation(Expr.splice(valueExpr), Expr.splice(algNameExpr), Expr.splice(methodNameExpr))
        .asInstanceOf[Result]
    }
  }
}
