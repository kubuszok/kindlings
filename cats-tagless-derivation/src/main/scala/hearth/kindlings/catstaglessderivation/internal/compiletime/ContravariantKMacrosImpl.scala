package hearth.kindlings.catstaglessderivation.internal.compiletime

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

trait ContravariantKMacrosImpl
    extends rules.ContravariantKUseCachedRuleImpl
    with rules.ContravariantKUseImplicitRuleImpl
    with rules.ContravariantKCaseClassRuleImpl
    with rules.ContravariantKTraitRuleImpl
    with InvariantKMacrosImpl { this: MacroCommons & StdExtensions =>

  import hearth.kindlings.catstaglessderivation.internal.runtime.CatsTaglessFactories

  // --- Context ---

  final case class ContravariantKCtx[Alg[_[_]]](
      algCtorK1: Type.CtorK1[Alg],
      contravariantKAlgType: Type[cats.tagless.ContravariantK[Alg]],
      cache: MLocal[ValDefsCache],
      derivedType: Option[??]
  )

  def ckctx[Alg[_[_]]](implicit ctx: ContravariantKCtx[Alg]): ContravariantKCtx[Alg] = ctx

  abstract class ContravariantKDerivationRule(val name: String) extends Rule {
    def apply[Alg[_[_]]: ContravariantKCtx]: MIO[Rule.Applicability[Expr[cats.tagless.ContravariantK[Alg]]]]
  }

  // --- Recursive derivation ---

  def deriveContravariantKRecursively[Alg[_[_]]: ContravariantKCtx]: MIO[Expr[cats.tagless.ContravariantK[Alg]]] =
    Log.namedScope(s"Deriving ContravariantK for ${ckctx.contravariantKAlgType.prettyPrint}") {
      Rules(
        ContravariantKUseCachedRule,
        ContravariantKUseImplicitRule,
        ContravariantKCaseClassRule,
        ContravariantKTraitRule
      )(_[Alg]).flatMap {
        case Right(result) =>
          Log.info(s"Derived ContravariantK for ${ckctx.contravariantKAlgType.prettyPrint}") >> MIO.pure(result)
        case Left(reasons) =>
          val reasonsStrings = reasons.toListMap
            .removed(ContravariantKUseCachedRule)
            .view
            .map { case (rule, reasons) =>
              if (reasons.isEmpty) s"The rule ${rule.name} was not applicable"
              else s" - ${rule.name}: ${reasons.mkString(", ")}"
            }
            .toList
          val err = CatsTaglessDerivationError.UnsupportedType(
            "ContravariantK",
            ckctx.contravariantKAlgType.prettyPrint,
            reasonsStrings
          )
          Log.error(err.message) >> MIO.fail(err)
      }
    }

  // --- Entry point ---

  @scala.annotation.nowarn("msg=is never used|unused explicit parameter")
  def deriveContravariantK[Alg[_[_]]](
      AlgCtorK10: Type.CtorK1[Alg],
      ContravariantKAlgType: Type[cats.tagless.ContravariantK[Alg]]
  ): Expr[cats.tagless.ContravariantK[Alg]] = {
    val macroName = "ContravariantK.derived"

    implicit val AlgCtorK1: Type.CtorK1[Alg] = AlgCtorK10
    hearth.fp.ignore(AlgCtorK1)

    val selfType: Option[??] = Some(ContravariantKAlgType.as_??)

    Log
      .namedScope(s"Deriving ContravariantK at: ${Environment.currentPosition.prettyPrint}") {
        MIO.scoped { runSafe =>
          val fromCtx: ContravariantKCtx[Alg] => Expr[cats.tagless.ContravariantK[Alg]] =
            (ctx: ContravariantKCtx[Alg]) =>
              runSafe {
                for {
                  _ <- Environment.loadStandardExtensions().toMIO(allowFailures = false)
                  result <- deriveContravariantKRecursively[Alg](using ctx)
                  cache <- ctx.cache.get
                } yield cache.toValDefs.use(_ => result)
              }

          val ctx = ContravariantKCtx[Alg](AlgCtorK1, ContravariantKAlgType, ValDefsCache.mlocal, selfType)
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

  // --- Body derivation ---

  @scala.annotation.nowarn("msg=is never used|unused implicit parameter")
  private[compiletime] def deriveContravariantKCaseClassBody[Alg[_[_]]](
      afExpr: Expr[Alg[CatsTaglessFactories.WCtor1]],
      fkExpr: Expr[Any]
  )(implicit ctx: ContravariantKCtx[Alg]): MIO[Expr[Alg[CatsTaglessFactories.WCtor2]]] = {
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

        val nestedFieldExprs = scala.collection.mutable.Map.empty[String, Expr[Any]]

        val contravariantKAnchor = Type
          .of[cats.tagless.ContravariantK[CatsTaglessFactories.DummyHKT]]
          .asInstanceOf[Type[Any]]
        val invariantKAnchor = Type
          .of[cats.tagless.InvariantK[CatsTaglessFactories.DummyHKT]]
          .asInstanceOf[Type[Any]]

        val nestedFieldDerivations: MIO[Unit] =
          fieldsOption.zip(fieldsList).foldLeft(MIO.pure(())) { case (acc, ((name, pOption), (_, pList))) =>
            val tOption = pOption.tpe.Underlying
            val tList = pList.tpe.Underlying
            val optionUnapply = OptionCtor.unapply(tOption.asInstanceOf[Type[Any]])
            val listUnapply = ListCtor.unapply(tList.asInstanceOf[Type[Any]])

            (optionUnapply, listUnapply) match {
              case (Some(_), Some(_)) =>
                acc >> failUnsupportedField(
                  "ContravariantK",
                  name,
                  "is covariant in the type constructor parameter " +
                    "(has type F[X]). ContravariantK can only handle contravariant or invariant fields. " +
                    "Consider using InvariantK or FunctorK instead."
                )
              case (None, None) =>
                if (tOption =:= tList) acc
                else {
                  acc >> (constructTypeClassTypeForField(tOption.asInstanceOf[Type[Any]], contravariantKAnchor) match {
                    case Some(info) =>
                      val nestedCtorK1 = Type.CtorK1.fromUntyped[CatsTaglessFactories.DummyHKT](info.ctorK1Untyped)
                      val nestedCKType = info.typeClassType
                        .asInstanceOf[Type[cats.tagless.ContravariantK[CatsTaglessFactories.DummyHKT]]]
                      val nestedCtx = ContravariantKCtx[CatsTaglessFactories.DummyHKT](
                        nestedCtorK1,
                        nestedCKType,
                        ctx.cache,
                        ctx.derivedType
                      )
                      deriveContravariantKRecursively[CatsTaglessFactories.DummyHKT](using nestedCtx).map {
                        nestedExpr =>
                          nestedFieldExprs += (name -> nestedExpr.asInstanceOf[Expr[Any]])
                          ()
                      }
                    case None =>
                      val invariantKAvailable = constructTypeClassTypeForField(
                        tOption.asInstanceOf[Type[Any]],
                        invariantKAnchor
                      ).exists(info => info.typeClassType.summonExprIgnoring().toEither.isRight)
                      val hint =
                        if (invariantKAvailable)
                          " An InvariantK instance exists for it — consider using InvariantK.derived instead."
                        else ""
                      failUnsupportedField(
                        "ContravariantK",
                        name,
                        "depends on the type constructor parameter " +
                          s"but no ContravariantK instance could be derived for its outer type constructor.$hint"
                      )
                  })
                }
              case _ =>
                acc >> failUnsupportedField("ContravariantK", name, "has inconsistent probe decomposition.")
            }
          }

        for {
          _ <- nestedFieldDerivations
          sourceAndTarget <- parsedOrFail(CaseClass.parse(using AlgWCtor1Type).toEither, "Alg[WCtor1]")
            .parTuple(parsedOrFail(CaseClass.parse(using AlgWCtor2Type).toEither, "Alg[WCtor2]"))
          result <- {
            val (sourceCC, targetCC) = sourceAndTarget
            val sourceFields = sourceCC.caseFieldValuesAt(afExpr).toList
            val targetParamTypes: Map[String, Type[Any]] = targetCC.primaryConstructor.parameters.flatten.toList.map {
              case (n, p) => import p.tpe.Underlying as PF; (n, Type[PF].asInstanceOf[Type[Any]])
            }.toMap

            val nestedFieldMap = nestedFieldExprs.toMap

            val mappedFields: List[(String, Expr_??)] = sourceFields.map { case (fieldName, fieldValue) =>
              import fieldValue.Underlying as Field
              val fieldExpr = fieldValue.value.asInstanceOf[Expr[Field]]

              if (nestedFieldMap.contains(fieldName)) {
                val tgt: Type[Field] = targetParamTypes(fieldName).asInstanceOf[Type[Field]]
                val mapped = mkNestedContramapK[Field](nestedFieldMap(fieldName), fieldExpr.upcast[Any], fkExpr)(tgt)
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
              case Left(error) => failCannotConstruct("ContravariantK", error)
            }
          }
        } yield result
      }
  }

  @scala.annotation.nowarn("msg=is never used|unused implicit parameter")
  private def mkNestedContramapK[Result](
      contravariantKExpr: Expr[Any],
      fieldExpr: Expr[Any],
      fkExpr: Expr[Any]
  )(implicit ResultType: Type[Result]): Expr[Result] =
    Expr.quote {
      hearth.kindlings.catstaglessderivation.internal.runtime.CatsTaglessRuntime
        .contramapK(Expr.splice(contravariantKExpr), Expr.splice(fieldExpr), Expr.splice(fkExpr))
        .asInstanceOf[Result]
    }
}
