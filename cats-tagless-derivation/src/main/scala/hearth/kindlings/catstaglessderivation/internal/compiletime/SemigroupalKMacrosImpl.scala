package hearth.kindlings.catstaglessderivation.internal.compiletime

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

trait SemigroupalKMacrosImpl
    extends rules.SemigroupalKUseCachedRuleImpl
    with rules.SemigroupalKUseImplicitRuleImpl
    with rules.SemigroupalKCaseClassRuleImpl
    with rules.SemigroupalKTraitRuleImpl
    with InvariantKMacrosImpl { this: MacroCommons & StdExtensions =>

  import hearth.kindlings.catstaglessderivation.internal.runtime.CatsTaglessFactories

  protected def mkWCtor3: Type.Ctor1[CatsTaglessFactories.WCtor3]

  // --- Context ---

  final case class SemigroupalKCtx[Alg[_[_]]](
      algCtorK1: Type.CtorK1[Alg],
      semigroupalKAlgType: Type[cats.tagless.SemigroupalK[Alg]],
      cache: MLocal[ValDefsCache],
      derivedType: Option[??]
  )

  def skctx[Alg[_[_]]](implicit ctx: SemigroupalKCtx[Alg]): SemigroupalKCtx[Alg] = ctx

  abstract class SemigroupalKDerivationRule(val name: String) extends Rule {
    def apply[Alg[_[_]]: SemigroupalKCtx]: MIO[Rule.Applicability[Expr[cats.tagless.SemigroupalK[Alg]]]]
  }

  // --- Recursive derivation ---

  def deriveSemigroupalKRecursively[Alg[_[_]]: SemigroupalKCtx]: MIO[Expr[cats.tagless.SemigroupalK[Alg]]] =
    Log.namedScope(s"Deriving SemigroupalK for ${skctx.semigroupalKAlgType.prettyPrint}") {
      Rules(
        SemigroupalKUseCachedRule,
        SemigroupalKUseImplicitRule,
        SemigroupalKCaseClassRule,
        SemigroupalKTraitRule
      )(_[Alg]).flatMap {
        case Right(result) =>
          Log.info(s"Derived SemigroupalK for ${skctx.semigroupalKAlgType.prettyPrint}") >> MIO.pure(result)
        case Left(reasons) =>
          val reasonsStrings = reasons.toListMap
            .removed(SemigroupalKUseCachedRule)
            .view
            .map { case (rule, reasons) =>
              if (reasons.isEmpty) s"The rule ${rule.name} was not applicable"
              else s" - ${rule.name}: ${reasons.mkString(", ")}"
            }
            .toList
          val err = SemigroupalKDerivationError.UnsupportedType(
            skctx.semigroupalKAlgType.prettyPrint,
            reasonsStrings
          )
          Log.error(err.message) >> MIO.fail(err)
      }
    }

  // --- Entry point ---

  @scala.annotation.nowarn("msg=is never used|unused explicit parameter")
  def deriveSemigroupalK[Alg[_[_]]](
      AlgCtorK10: Type.CtorK1[Alg],
      SemigroupalKAlgType: Type[cats.tagless.SemigroupalK[Alg]]
  ): Expr[cats.tagless.SemigroupalK[Alg]] = {
    val macroName = "SemigroupalK.derived"

    implicit val AlgCtorK1: Type.CtorK1[Alg] = AlgCtorK10
    hearth.fp.ignore(AlgCtorK1)

    val selfType: Option[??] = Some(SemigroupalKAlgType.as_??)

    Log
      .namedScope(s"Deriving SemigroupalK at: ${Environment.currentPosition.prettyPrint}") {
        MIO.scoped { runSafe =>
          val fromCtx: SemigroupalKCtx[Alg] => Expr[cats.tagless.SemigroupalK[Alg]] =
            (ctx: SemigroupalKCtx[Alg]) =>
              runSafe {
                for {
                  _ <- Environment.loadStandardExtensions().toMIO(allowFailures = false)
                  result <- deriveSemigroupalKRecursively[Alg](using ctx)
                  cache <- ctx.cache.get
                } yield cache.toValDefs.use(_ => result)
              }

          val ctx = SemigroupalKCtx[Alg](AlgCtorK1, SemigroupalKAlgType, ValDefsCache.mlocal, selfType)
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
  private[compiletime] def deriveSemigroupalKCaseClassBody[Alg[_[_]]](
      afExpr: Expr[Alg[CatsTaglessFactories.WCtor1]],
      agExpr: Expr[Alg[CatsTaglessFactories.WCtor2]]
  )(implicit ctx: SemigroupalKCtx[Alg]): MIO[Expr[Alg[CatsTaglessFactories.WCtor3]]] = {
    val AlgCtorK1 = ctx.algCtorK1
    implicit val AnyType: Type[Any] = InvariantKTypes.Any

    val WCtor1Ctor = mkWCtor1
    val WCtor2Ctor = mkWCtor2
    val WCtor3Ctor = mkWCtor3

    val AlgWCtor1Type = AlgCtorK1.apply[CatsTaglessFactories.WCtor1](using WCtor1Ctor)
    val AlgWCtor2Type = AlgCtorK1.apply[CatsTaglessFactories.WCtor2](using WCtor2Ctor)
    val AlgWCtor3Type = AlgCtorK1.apply[CatsTaglessFactories.WCtor3](using WCtor3Ctor)

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

    // --- Field classification ---

    val directFields = scala.collection.mutable.Set.empty[String]
    val nestedFieldExprs = scala.collection.mutable.Map.empty[String, Expr[Any]]

    val semigroupalKAnchor = Type
      .of[cats.tagless.SemigroupalK[CatsTaglessFactories.DummyHKT]]
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
            if (InnerO =:= InnerL) {
              directFields += name
              acc
            } else {
              acc >> MIO.fail(
                new RuntimeException(
                  s"Cannot derive SemigroupalK: field '$name' has type constructor parameter " +
                    "nested within its own type argument (e.g. F[F[X]]). This is not supported."
                )
              )
            }
          case (None, None) =>
            if (tOption =:= tList) {
              acc // invariant
            } else {
              acc >> (constructTypeClassTypeForField(tOption.asInstanceOf[Type[Any]], semigroupalKAnchor) match {
                case Some(info) =>
                  val nestedCtorK1 =
                    Type.CtorK1.fromUntyped[CatsTaglessFactories.DummyHKT](info.ctorK1Untyped)
                  val nestedSKType = info.typeClassType.asInstanceOf[Type[cats.tagless.SemigroupalK[
                    CatsTaglessFactories.DummyHKT
                  ]]]
                  val nestedCtx = SemigroupalKCtx[CatsTaglessFactories.DummyHKT](
                    nestedCtorK1,
                    nestedSKType,
                    ctx.cache,
                    ctx.derivedType
                  )
                  deriveSemigroupalKRecursively[CatsTaglessFactories.DummyHKT](using nestedCtx).map { nestedExpr =>
                    nestedFieldExprs += (name -> nestedExpr.asInstanceOf[Expr[Any]])
                    ()
                  }
                case None =>
                  MIO.fail(
                    new RuntimeException(
                      s"Cannot derive SemigroupalK: field '$name' depends on the type constructor parameter " +
                        "but is not a direct application of it (e.g. F[X]), and its outer type constructor " +
                        "could not be extracted for recursive derivation."
                    )
                  )
              })
            }
          case _ =>
            acc >> MIO.fail(
              new RuntimeException(
                s"Cannot derive SemigroupalK: field '$name' has inconsistent probe decomposition."
              )
            )
        }
      }

    for {
      _ <- nestedFieldDerivations
      result <- {
        val sourceCC_F = CaseClass.parse(using AlgWCtor1Type).toEither match {
          case Right(cc) => cc; case Left(e) => throw new RuntimeException(s"Cannot parse Alg[WCtor1]: $e")
        }
        val sourceCC_G = CaseClass.parse(using AlgWCtor2Type).toEither match {
          case Right(cc) => cc; case Left(e) => throw new RuntimeException(s"Cannot parse Alg[WCtor2]: $e")
        }
        val targetCC = CaseClass.parse(using AlgWCtor3Type).toEither match {
          case Right(cc) => cc; case Left(e) => throw new RuntimeException(s"Cannot parse Alg[WCtor3]: $e")
        }

        val sourceFieldsF = sourceCC_F.caseFieldValuesAt(afExpr).toList
        val sourceFieldsG = sourceCC_G.caseFieldValuesAt(agExpr).toList
        val sourceFieldsGMap = sourceFieldsG.toMap

        val targetParamTypes: Map[String, Type[Any]] = targetCC.primaryConstructor.parameters.flatten.toList.map {
          case (n, p) => import p.tpe.Underlying as PF; (n, Type[PF].asInstanceOf[Type[Any]])
        }.toMap

        val directFieldSet = directFields.toSet
        val nestedFieldMap = nestedFieldExprs.toMap

        val mappedFields: List[(String, Expr_??)] = sourceFieldsF.map { case (fieldName, fieldValueF) =>
          import fieldValueF.Underlying as FieldF
          val fieldExprF = fieldValueF.value.asInstanceOf[Expr[FieldF]]

          if (directFieldSet.contains(fieldName)) {
            val tgt: Type[FieldF] = targetParamTypes(fieldName).asInstanceOf[Type[FieldF]]
            val fieldValueG = sourceFieldsGMap(fieldName)
            import fieldValueG.Underlying as FieldG
            val fieldExprG = fieldValueG.value.asInstanceOf[Expr[FieldG]]
            val mapped = mkTuple2K[FieldF](fieldExprF.upcast[Any], fieldExprG.upcast[Any])(tgt)
            (fieldName, mapped.as_??(tgt))
          } else if (nestedFieldMap.contains(fieldName)) {
            val tgt: Type[FieldF] = targetParamTypes(fieldName).asInstanceOf[Type[FieldF]]
            val fieldValueG = sourceFieldsGMap(fieldName)
            import fieldValueG.Underlying as FieldG
            val fieldExprG = fieldValueG.value.asInstanceOf[Expr[FieldG]]
            val mapped =
              mkNestedProductK[FieldF](nestedFieldMap(fieldName), fieldExprF.upcast[Any], fieldExprG.upcast[Any])(tgt)
            (fieldName, mapped.as_??(tgt))
          } else {
            (fieldName, fieldExprF.as_??)
          }
        }

        targetCC.primaryConstructor.fold(
          onInstance = _ => throw new RuntimeException("Constructor should not need instance"),
          onTypes = _ => Map.empty,
          onValues = _ => mappedFields.toMap
        ) match {
          case Right(constructExpr) =>
            import constructExpr.Underlying; MIO.pure(constructExpr.value.upcast(using implicitly, AlgWCtor3Type))
          case Left(error) =>
            MIO.fail(new RuntimeException(s"Cannot construct SemigroupalK result: $error"))
        }
      }
    } yield result
  }

  // --- Helpers ---

  @scala.annotation.nowarn("msg=is never used|unused implicit parameter")
  private[compiletime] def mkTuple2K[Result](
      firstExpr: Expr[Any],
      secondExpr: Expr[Any]
  )(implicit ResultType: Type[Result]): Expr[Result] =
    Expr.quote {
      hearth.kindlings.catstaglessderivation.internal.runtime.CatsTaglessRuntime
        .mkTuple2K(Expr.splice(firstExpr), Expr.splice(secondExpr))
        .asInstanceOf[Result]
    }

  @scala.annotation.nowarn("msg=is never used|unused implicit parameter")
  private def mkNestedProductK[Result](
      semigroupalKExpr: Expr[Any],
      afFieldExpr: Expr[Any],
      agFieldExpr: Expr[Any]
  )(implicit ResultType: Type[Result]): Expr[Result] =
    Expr.quote {
      hearth.kindlings.catstaglessderivation.internal.runtime.CatsTaglessRuntime
        .productK(Expr.splice(semigroupalKExpr), Expr.splice(afFieldExpr), Expr.splice(agFieldExpr))
        .asInstanceOf[Result]
    }
}

sealed private[compiletime] trait SemigroupalKDerivationError
    extends util.control.NoStackTrace
    with Product
    with Serializable {
  def message: String
  override def getMessage(): String = message
}
private[compiletime] object SemigroupalKDerivationError {
  final case class UnsupportedType(tpeName: String, reasons: List[String]) extends SemigroupalKDerivationError {
    override def message: String =
      s"The type $tpeName was not handled by any SemigroupalK derivation rule:\n${reasons.mkString("\n")}"
  }
}
