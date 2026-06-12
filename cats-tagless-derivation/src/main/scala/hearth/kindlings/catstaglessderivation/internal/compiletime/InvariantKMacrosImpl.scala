package hearth.kindlings.catstaglessderivation.internal.compiletime

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

import hearth.kindlings.catstaglessderivation.LogDerivation

trait InvariantKMacrosImpl
    extends rules.InvariantKUseCachedRuleImpl
    with rules.InvariantKUseImplicitRuleImpl
    with rules.InvariantKCaseClassRuleImpl
    with rules.InvariantKTraitRuleImpl
    with CatsTaglessDerivationTimeout
    with hearth.kindlings.derivation.compiletime.MethodFolds { this: MacroCommons & StdExtensions =>

  import hearth.kindlings.catstaglessderivation.internal.runtime.CatsTaglessFactories

  protected def mkWCtor1: Type.Ctor1[CatsTaglessFactories.WCtor1]
  protected def mkWCtor2: Type.Ctor1[CatsTaglessFactories.WCtor2]

  /** Extract the HKT constructor from a nested field type (e.g. InnerAlg from InnerAlg[Option]) and construct an
    * applied type class type (e.g. Type[InvariantK[InnerAlg]]). The anchor type provides the type class constructor
    * (e.g. Type[InvariantK[DummyHKT]]). Returns None if the field type is not an applied HKT.
    */
  protected def constructTypeClassTypeForField(
      fieldType: Type[Any],
      typeClassAnchorType: Type[Any]
  ): Option[NestedFieldInfo]

  final case class NestedFieldInfo(
      typeClassType: Type[Any],
      ctorK1Untyped: UntypedType
  )

  /** Routes a `CaseClass.parse`/`AnonymousInstance.parse` result into the MIO error channel instead of throwing. */
  private[compiletime] def parsedOrFail[E, A](parsed: Either[E, A], algebra: String): MIO[A] =
    parsed match {
      case Right(a) => MIO.pure(a)
      case Left(e)  =>
        val err = CatsTaglessDerivationError.CannotParseAlgebra(algebra, e.toString)
        Log.error(err.message) >> MIO.fail(err)
    }

  /** Routes field-classification failures into the MIO error channel. */
  private[compiletime] def failUnsupportedField[A](typeClass: String, fieldName: String, reason: String): MIO[A] = {
    val err = CatsTaglessDerivationError.UnsupportedField(typeClass, fieldName, reason)
    Log.error(err.message) >> MIO.fail(err)
  }

  /** Routes instance-construction failures into the MIO error channel. */
  private[compiletime] def failCannotConstruct[A](typeClass: String, reason: String): MIO[A] = {
    val err = CatsTaglessDerivationError.CannotConstructResult(typeClass, reason)
    Log.error(err.message) >> MIO.fail(err)
  }

  // --- Context ---

  final case class InvariantKCtx[Alg[_[_]]](
      algCtorK1: Type.CtorK1[Alg],
      invariantKAlgType: Type[cats.tagless.InvariantK[Alg]],
      cache: MLocal[ValDefsCache],
      derivedType: Option[??]
  )

  def ikctx[Alg[_[_]]](implicit ctx: InvariantKCtx[Alg]): InvariantKCtx[Alg] = ctx

  abstract class InvariantKDerivationRule(val name: String) extends Rule {
    def apply[Alg[_[_]]: InvariantKCtx]: MIO[Rule.Applicability[Expr[cats.tagless.InvariantK[Alg]]]]
  }

  // --- Recursive derivation ---

  def deriveInvariantKRecursively[Alg[_[_]]: InvariantKCtx]: MIO[Expr[cats.tagless.InvariantK[Alg]]] =
    Log.namedScope(s"Deriving InvariantK for ${ikctx.invariantKAlgType.prettyPrint}") {
      Rules(
        InvariantKUseCachedRule,
        InvariantKUseImplicitRule,
        InvariantKCaseClassRule,
        InvariantKTraitRule
      )(_[Alg]).flatMap {
        case Right(result) =>
          Log.info(s"Derived InvariantK for ${ikctx.invariantKAlgType.prettyPrint}") >> MIO.pure(result)
        case Left(reasons) =>
          val reasonsStrings = reasons.toListMap
            .removed(InvariantKUseCachedRule)
            .view
            .map { case (rule, reasons) =>
              if (reasons.isEmpty) s"The rule ${rule.name} was not applicable"
              else s" - ${rule.name}: ${reasons.mkString(", ")}"
            }
            .toList
          val err = CatsTaglessDerivationError.UnsupportedType(
            "InvariantK",
            ikctx.invariantKAlgType.prettyPrint,
            reasonsStrings
          )
          Log.error(err.message) >> MIO.fail(err)
      }
    }

  // --- Entry point ---

  @scala.annotation.nowarn("msg=is never used|unused explicit parameter")
  def deriveInvariantK[Alg[_[_]]](
      AlgCtorK10: Type.CtorK1[Alg],
      InvariantKAlgType: Type[cats.tagless.InvariantK[Alg]]
  ): Expr[cats.tagless.InvariantK[Alg]] = {
    val macroName = "InvariantK.derived"

    implicit val AlgCtorK1: Type.CtorK1[Alg] = AlgCtorK10
    hearth.fp.ignore(AlgCtorK1) // used by cross-quotes plugin

    val selfType: Option[??] = Some(InvariantKAlgType.as_??)

    Log
      .namedScope(s"Deriving InvariantK at: ${Environment.currentPosition.prettyPrint}") {
        MIO.scoped { runSafe =>
          val fromCtx: InvariantKCtx[Alg] => Expr[cats.tagless.InvariantK[Alg]] =
            (ctx: InvariantKCtx[Alg]) =>
              runSafe {
                for {
                  _ <- Environment.loadStandardExtensions().toMIO(allowFailures = false)
                  result <- deriveInvariantKRecursively[Alg](using ctx)
                  cache <- ctx.cache.get
                } yield cache.toValDefs.use(_ => result)
              }

          val ctx = InvariantKCtx[Alg](
            AlgCtorK1,
            InvariantKAlgType,
            ValDefsCache.mlocal,
            selfType
          )

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
  private[compiletime] def deriveInvariantKCaseClassBody[Alg[_[_]]](
      afExpr: Expr[Alg[CatsTaglessFactories.WCtor1]],
      fkExpr: Expr[Any],
      gkExpr: Expr[Any]
  )(implicit ctx: InvariantKCtx[Alg]): MIO[Expr[Alg[CatsTaglessFactories.WCtor2]]] = {
    val AlgCtorK1 = ctx.algCtorK1
    implicit val AnyType: Type[Any] = InvariantKTypes.Any

    val WCtor1Ctor = mkWCtor1
    val WCtor2Ctor = mkWCtor2

    val AlgWCtor1Type: Type[Alg[CatsTaglessFactories.WCtor1]] =
      AlgCtorK1.apply[CatsTaglessFactories.WCtor1](using WCtor1Ctor)
    val AlgWCtor2Type: Type[Alg[CatsTaglessFactories.WCtor2]] =
      AlgCtorK1.apply[CatsTaglessFactories.WCtor2](using WCtor2Ctor)

    val OptionCtor = Type.Ctor1.of[Option]
    val ListCtor = Type.Ctor1.of[List]
    val AlgOptionType = AlgCtorK1.apply[Option](using OptionCtor)
    val AlgListType = AlgCtorK1.apply[List](using ListCtor)

    parsedOrFail(CaseClass.parse(using AlgOptionType).toEither, "Alg[Option]")
      .parTuple(parsedOrFail(CaseClass.parse(using AlgListType).toEither, "Alg[List]"))
      .flatMap { case (ccOption, ccList) =>
        val fieldsOption = ccOption.primaryConstructor.parameters.flatten.toList
        val fieldsList = ccList.primaryConstructor.parameters.flatten.toList

        // --- Field classification ---

        val directFields = scala.collection.mutable.Set.empty[String]
        val nestedFieldExprs = scala.collection.mutable.Map.empty[String, Expr[Any]]

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
                  acc >> failUnsupportedField(
                    "InvariantK",
                    name,
                    "has type constructor parameter " +
                      "nested within its own type argument (e.g. F[F[X]]). This is not supported."
                  )
                }
              case (None, None) =>
                if (tOption =:= tList) {
                  acc // invariant
                } else {
                  val invariantKAnchor = Type
                    .of[cats.tagless.InvariantK[CatsTaglessFactories.DummyHKT]]
                    .asInstanceOf[Type[Any]]
                  acc >> (constructTypeClassTypeForField(tOption.asInstanceOf[Type[Any]], invariantKAnchor) match {
                    case Some(info) =>
                      val nestedCtorK1 =
                        Type.CtorK1.fromUntyped[CatsTaglessFactories.DummyHKT](info.ctorK1Untyped)
                      val nestedIKType = info.typeClassType.asInstanceOf[Type[cats.tagless.InvariantK[
                        CatsTaglessFactories.DummyHKT
                      ]]]
                      val nestedCtx = InvariantKCtx[CatsTaglessFactories.DummyHKT](
                        nestedCtorK1,
                        nestedIKType,
                        ctx.cache,
                        ctx.derivedType
                      )
                      deriveInvariantKRecursively[CatsTaglessFactories.DummyHKT](using nestedCtx).map { nestedExpr =>
                        nestedFieldExprs += (name -> nestedExpr.asInstanceOf[Expr[Any]])
                        ()
                      }
                    case None =>
                      failUnsupportedField(
                        "InvariantK",
                        name,
                        "depends on the type constructor parameter " +
                          "but is not a direct application of it (e.g. F[X]), and its outer type constructor " +
                          "could not be extracted for recursive derivation. " +
                          "Only direct F[X] fields, nested Alg[F] fields, and invariant fields are supported."
                      )
                  })
                }
              case _ =>
                acc >> failUnsupportedField("InvariantK", name, "has inconsistent probe decomposition.")
            }
          }

        for {
          _ <- nestedFieldDerivations
          sourceAndTarget <- parsedOrFail(CaseClass.parse(using AlgWCtor1Type).toEither, "Alg[WCtor1]")
            .parTuple(parsedOrFail(CaseClass.parse(using AlgWCtor2Type).toEither, "Alg[WCtor2]"))
          result <- {
            val (sourceCC, targetCC) = sourceAndTarget

            val sourceFields = sourceCC.caseFieldValuesAt(afExpr).toList
            val targetFieldParams = targetCC.primaryConstructor.parameters.flatten.toList
            val targetParamTypes: Map[String, Type[Any]] = targetFieldParams.map { case (name, param) =>
              import param.tpe.Underlying as PField
              (name, Type[PField].asInstanceOf[Type[Any]])
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
                val nestedIK = nestedFieldMap(fieldName)
                val mapped = mkNestedImapK[Field](nestedIK, fieldExpr.upcast[Any], fkExpr, gkExpr)(tgt)
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
              case Left(error) =>
                failCannotConstruct("InvariantK", error)
            }
          }
        } yield result
      }
  }

  // --- Helpers ---

  @scala.annotation.nowarn("msg=is never used|unused implicit parameter")
  private[compiletime] def mkApplyFk[Result](
      fkExpr: Expr[Any],
      fieldExpr: Expr[Any]
  )(implicit ResultType: Type[Result]): Expr[Result] =
    Expr.quote {
      hearth.kindlings.catstaglessderivation.internal.runtime.CatsTaglessRuntime
        .applyFk(Expr.splice(fkExpr), Expr.splice(fieldExpr))
        .asInstanceOf[Result]
    }

  @scala.annotation.nowarn("msg=is never used|unused implicit parameter")
  private def mkNestedImapK[Result](
      invariantKExpr: Expr[Any],
      fieldExpr: Expr[Any],
      fkExpr: Expr[Any],
      gkExpr: Expr[Any]
  )(implicit ResultType: Type[Result]): Expr[Result] =
    Expr.quote {
      hearth.kindlings.catstaglessderivation.internal.runtime.CatsTaglessRuntime
        .imapK(Expr.splice(invariantKExpr), Expr.splice(fieldExpr), Expr.splice(fkExpr), Expr.splice(gkExpr))
        .asInstanceOf[Result]
    }

  // --- Types ---

  protected object InvariantKTypes {
    val Any: Type[Any] = Type.of[Any]
    val LogDerivation: Type[hearth.kindlings.catstaglessderivation.LogDerivation] =
      Type.of[hearth.kindlings.catstaglessderivation.LogDerivation]
  }

  def shouldWeLogDerivation: Boolean = {
    implicit val LogDerivationType: Type[LogDerivation] = InvariantKTypes.LogDerivation
    Expr.summonImplicit[LogDerivation].isDefined || (for {
      data <- Environment.typedSettings.toOption
      cd <- data.get("catsTaglessDerivation")
      shouldLog <- cd.get("logDerivation").flatMap(_.asBoolean)
    } yield shouldLog).getOrElse(false)
  }
}
