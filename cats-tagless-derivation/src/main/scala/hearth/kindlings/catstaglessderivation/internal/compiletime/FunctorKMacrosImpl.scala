package hearth.kindlings.catstaglessderivation.internal.compiletime

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

trait FunctorKMacrosImpl
    extends rules.FunctorKUseCachedRuleImpl
    with rules.FunctorKUseImplicitRuleImpl
    with rules.FunctorKCaseClassRuleImpl
    with rules.FunctorKTraitRuleImpl
    with InvariantKMacrosImpl { this: MacroCommons & StdExtensions =>

  import hearth.kindlings.catstaglessderivation.internal.runtime.CatsTaglessFactories

  // --- Context ---

  final case class FunctorKCtx[Alg[_[_]]](
      algCtorK1: Type.CtorK1[Alg],
      functorKAlgType: Type[cats.tagless.FunctorK[Alg]],
      cache: MLocal[ValDefsCache],
      derivedType: Option[??]
  )

  def fkctx[Alg[_[_]]](implicit ctx: FunctorKCtx[Alg]): FunctorKCtx[Alg] = ctx

  abstract class FunctorKDerivationRule(val name: String) extends Rule {
    def apply[Alg[_[_]]: FunctorKCtx]: MIO[Rule.Applicability[Expr[cats.tagless.FunctorK[Alg]]]]
  }

  // --- Recursive derivation ---

  def deriveFunctorKRecursively[Alg[_[_]]: FunctorKCtx]: MIO[Expr[cats.tagless.FunctorK[Alg]]] =
    Log.namedScope(s"Deriving FunctorK for ${fkctx.functorKAlgType.prettyPrint}") {
      Rules(
        FunctorKUseCachedRule,
        FunctorKUseImplicitRule,
        FunctorKCaseClassRule,
        FunctorKTraitRule
      )(_[Alg]).flatMap {
        case Right(result) =>
          Log.info(s"Derived FunctorK for ${fkctx.functorKAlgType.prettyPrint}") >> MIO.pure(result)
        case Left(reasons) =>
          val reasonsStrings = reasons.toListMap
            .removed(FunctorKUseCachedRule)
            .view
            .map { case (rule, reasons) =>
              if (reasons.isEmpty) s"The rule ${rule.name} was not applicable"
              else s" - ${rule.name}: ${reasons.mkString(", ")}"
            }
            .toList
          val err = FunctorKDerivationError.UnsupportedType(
            fkctx.functorKAlgType.prettyPrint,
            reasonsStrings
          )
          Log.error(err.message) >> MIO.fail(err)
      }
    }

  // --- Entry point ---

  @scala.annotation.nowarn("msg=is never used|unused explicit parameter")
  def deriveFunctorK[Alg[_[_]]](
      AlgCtorK10: Type.CtorK1[Alg],
      FunctorKAlgType: Type[cats.tagless.FunctorK[Alg]]
  ): Expr[cats.tagless.FunctorK[Alg]] = {
    val macroName = "FunctorK.derived"

    implicit val AlgCtorK1: Type.CtorK1[Alg] = AlgCtorK10
    hearth.fp.ignore(AlgCtorK1)

    val selfType: Option[??] = Some(FunctorKAlgType.as_??)

    Log
      .namedScope(s"Deriving FunctorK at: ${Environment.currentPosition.prettyPrint}") {
        MIO.scoped { runSafe =>
          val fromCtx: FunctorKCtx[Alg] => Expr[cats.tagless.FunctorK[Alg]] =
            (ctx: FunctorKCtx[Alg]) =>
              runSafe {
                for {
                  _ <- Environment.loadStandardExtensions().toMIO(allowFailures = false)
                  result <- deriveFunctorKRecursively[Alg](using ctx)
                  cache <- ctx.cache.get
                } yield cache.toValDefs.use(_ => result)
              }

          val ctx = FunctorKCtx[Alg](AlgCtorK1, FunctorKAlgType, ValDefsCache.mlocal, selfType)
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
  private[compiletime] def deriveFunctorKCaseClassBody[Alg[_[_]]](
      afExpr: Expr[Alg[CatsTaglessFactories.WCtor1]],
      fkExpr: Expr[Any]
  )(implicit ctx: FunctorKCtx[Alg]): MIO[Expr[Alg[CatsTaglessFactories.WCtor2]]] = {
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

    val ccOption = CaseClass.parse(using AlgOptionType).toEither match {
      case Right(cc) => cc
      case Left(e)   => throw new RuntimeException(s"Cannot parse Alg[Option]: $e")
    }
    val ccList = CaseClass.parse(using AlgListType).toEither match {
      case Right(cc) => cc
      case Left(e)   => throw new RuntimeException(s"Cannot parse Alg[List]: $e")
    }

    val fieldsOption = ccOption.primaryConstructor.parameters.flatten.toList
    val fieldsList = ccList.primaryConstructor.parameters.flatten.toList

    val directFields = scala.collection.mutable.Set.empty[String]
    val nestedFieldExprs = scala.collection.mutable.Map.empty[String, Expr[Any]]

    val functorKAnchor = Type
      .of[cats.tagless.FunctorK[CatsTaglessFactories.DummyHKT]]
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
          case (Some(innerOption), Some(innerList)) =>
            import innerOption.Underlying as InnerO
            import innerList.Underlying as InnerL
            if (InnerO =:= InnerL) { directFields += name; acc }
            else
              acc >> MIO.fail(
                new RuntimeException(
                  s"Cannot derive FunctorK: field '$name' has type constructor parameter " +
                    "nested within its own type argument (e.g. F[F[X]]). This is not supported."
                )
              )
          case (None, None) =>
            if (tOption =:= tList) acc
            else {
              acc >> (constructTypeClassTypeForField(tOption.asInstanceOf[Type[Any]], functorKAnchor) match {
                case Some(info) =>
                  val nestedCtorK1 = Type.CtorK1.fromUntyped[CatsTaglessFactories.DummyHKT](info.ctorK1Untyped)
                  val nestedFKType =
                    info.typeClassType.asInstanceOf[Type[cats.tagless.FunctorK[CatsTaglessFactories.DummyHKT]]]
                  val nestedCtx =
                    FunctorKCtx[CatsTaglessFactories.DummyHKT](nestedCtorK1, nestedFKType, ctx.cache, ctx.derivedType)
                  deriveFunctorKRecursively[CatsTaglessFactories.DummyHKT](using nestedCtx).map { nestedExpr =>
                    nestedFieldExprs += (name -> nestedExpr.asInstanceOf[Expr[Any]])
                    ()
                  }
                case None =>
                  val invariantKAvailable =
                    constructTypeClassTypeForField(tOption.asInstanceOf[Type[Any]], invariantKAnchor)
                      .exists(info => info.typeClassType.summonExprIgnoring().toEither.isRight)
                  val hint =
                    if (invariantKAvailable)
                      " An InvariantK instance exists for it — consider using InvariantK.derived instead."
                    else ""
                  MIO.fail(
                    new RuntimeException(
                      s"Cannot derive FunctorK: field '$name' depends on the type constructor parameter " +
                        s"but no FunctorK instance could be derived for its outer type constructor.$hint"
                    )
                  )
              })
            }
          case _ =>
            acc >> MIO.fail(
              new RuntimeException(
                s"Cannot derive FunctorK: field '$name' has inconsistent probe decomposition."
              )
            )
        }
      }

    for {
      _ <- nestedFieldDerivations
      result <- {
        val sourceCC = CaseClass.parse(using AlgWCtor1Type).toEither match {
          case Right(cc) => cc; case Left(e) => throw new RuntimeException(s"Cannot parse: $e")
        }
        val targetCC = CaseClass.parse(using AlgWCtor2Type).toEither match {
          case Right(cc) => cc; case Left(e) => throw new RuntimeException(s"Cannot parse: $e")
        }
        val sourceFields = sourceCC.caseFieldValuesAt(afExpr).toList
        val targetParamTypes: Map[String, Type[Any]] = targetCC.primaryConstructor.parameters.flatten.toList.map {
          case (n, p) => import p.tpe.Underlying as PF; (n, Type[PF].asInstanceOf[Type[Any]])
        }.toMap

        val directFieldSet = directFields.toSet
        val nestedFieldMap = nestedFieldExprs.toMap

        val mappedFields: List[(String, Expr_??)] = sourceFields.map { case (fieldName, fieldValue) =>
          import fieldValue.Underlying as Field
          val fieldExpr = fieldValue.value.asInstanceOf[Expr[Field]]

          if (directFieldSet.contains(fieldName)) {
            val tgt: Type[Field] = targetParamTypes(fieldName).asInstanceOf[Type[Field]]
            val mapped = mkApplyFk[Field](fkExpr, fieldExpr.upcast[Any])(tgt)
            (fieldName, mapped.as_??(tgt))
          } else if (nestedFieldMap.contains(fieldName)) {
            val tgt: Type[Field] = targetParamTypes(fieldName).asInstanceOf[Type[Field]]
            val mapped = mkNestedMapK[Field](nestedFieldMap(fieldName), fieldExpr.upcast[Any], fkExpr)(tgt)
            (fieldName, mapped.as_??(tgt))
          } else {
            (fieldName, fieldExpr.as_??)
          }
        }

        targetCC.primaryConstructor.fold(
          onInstance = _ => throw new RuntimeException("Constructor should not need instance"),
          onTypes = _ => Map.empty,
          onValues = _ => mappedFields.toMap
        ) match {
          case Right(constructExpr) =>
            import constructExpr.Underlying; MIO.pure(constructExpr.value.upcast(using implicitly, AlgWCtor2Type))
          case Left(error) => MIO.fail(new RuntimeException(s"Cannot construct FunctorK result: $error"))
        }
      }
    } yield result
  }

  @scala.annotation.nowarn("msg=is never used|unused implicit parameter")
  private def mkNestedMapK[Result](
      functorKExpr: Expr[Any],
      fieldExpr: Expr[Any],
      fkExpr: Expr[Any]
  )(implicit ResultType: Type[Result]): Expr[Result] =
    Expr.quote {
      hearth.kindlings.catstaglessderivation.internal.runtime.CatsTaglessRuntime
        .mapK(Expr.splice(functorKExpr), Expr.splice(fieldExpr), Expr.splice(fkExpr))
        .asInstanceOf[Result]
    }
}

sealed private[compiletime] trait FunctorKDerivationError
    extends util.control.NoStackTrace
    with Product
    with Serializable {
  def message: String
  override def getMessage(): String = message
}
private[compiletime] object FunctorKDerivationError {
  final case class UnsupportedType(tpeName: String, reasons: List[String]) extends FunctorKDerivationError {
    override def message: String =
      s"The type $tpeName was not handled by any FunctorK derivation rule:\n${reasons.mkString("\n")}"
  }
}
