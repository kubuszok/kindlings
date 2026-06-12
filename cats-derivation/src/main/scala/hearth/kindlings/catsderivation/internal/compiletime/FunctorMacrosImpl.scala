package hearth.kindlings.catsderivation.internal.compiletime

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.fp.instances.*
import hearth.fp.syntax.*
import hearth.std.*

import hearth.kindlings.catsderivation.LogDerivation

/** Functor derivation: maps over type parameter fields in case classes.
  *
  * Uses free type variables A and B directly in the generated code, relying on Hearth 0.2.0-264+ cross-quotes support
  * for method-level type parameters inside Expr.quote/Expr.splice.
  */
trait FunctorMacrosImpl
    extends rules.FunctorCaseClassRuleImpl
    with CatsDerivationTimeout
    with CatsDerivationErrorSupport {
  this: MacroCommons & StdExtensions =>

  /** Summon `cats.Functor[G]` for the type constructor `G` of the applied type `G[X]`, using Hearth's `Type.decompose1`
    * to discover `G` and `Type.CtorK1#apply` to build `Type[cats.Functor[G]]` to summon for. Returns None when the
    * field is not an applied type or no `Functor` instance is in scope.
    *
    * Replaces the former platform-specific `summonFunctorForFieldType` (Issue #284: HKT ctor primitives).
    */
  final protected def summonFunctorForFieldType(fieldType: Type[Any]): Option[Expr[Any]] =
    Type.decompose1(using fieldType).flatMap { case (gCtor, _) =>
      implicit val FunctorOfG: Type[cats.Functor[AnyK]] =
        CatsFunctorCtor.apply(using gCtor).asInstanceOf[Type[cats.Functor[AnyK]]]
      Expr.summonImplicit[cats.Functor[AnyK]].toOption.map(_.asInstanceOf[Expr[Any]])
    }

  /** Decompose an applied type `G[X]` into its (live) type constructor `G` and that constructor's untyped form. The
    * returned `Type.Ctor1` can be re-applied (`ctor[B]`) or compared (`sameTypeConstructorAs`). Returns None for
    * non-applied types.
    *
    * Replaces the former platform-specific `mkCtor1FromType` (Issue #284).
    */
  final protected def mkCtor1FromType(appliedType: Type[Any]): Option[(Type.Ctor1[AnyK], UntypedType)] =
    Type.decompose1(using appliedType).map { case (gCtor, _) =>
      (gCtor.asInstanceOf[Type.Ctor1[AnyK]], gCtor.asUntyped)
    }

  /** Given an applied type `G[X]`, extract the single type argument `X` as a `Type[Any]`. Returns None if the type is
    * not a single-argument applied type.
    *
    * Replaces the former platform-specific `extractSingleTypeArg` (Issue #284).
    */
  final protected def extractSingleTypeArg(appliedType: Type[Any]): Option[Type[Any]] =
    Type.decompose1(using appliedType).map { case (_, arg) => arg.Underlying.asInstanceOf[Type[Any]] }

  /** Check whether the given applied type `G[X]` has an inner type argument `X` whose type constructor matches the
    * given parent constructor. For example, `Option[Search[Int]]` with parent `Search` would return true because
    * `Search` (from `Search[Int]`) matches the parent.
    *
    * Replaces the former platform-specific `isNestedSelfRecursive` (Issue #284).
    */
  final protected def isNestedSelfRecursive(fieldType: Type[Any], parentCtor: UntypedType): Boolean =
    Type.decompose1(using fieldType).exists { case (_, innerArg) =>
      Type.decompose1(using innerArg.Underlying.asInstanceOf[Type[Any]]).exists { case (innerCtor, _) =>
        innerCtor.sameTypeConstructorAs(parentCtor)
      }
    }

  final case class FunctorCaseClassResult[F[_]](
      FCtor: Type.Ctor1[F],
      directFieldSet: Set[String],
      nestedFieldFunctors: Map[String, Expr[Any]],
      selfRecursiveFields: Set[String] = Set.empty,
      selfRecursiveOuterFunctors: Map[String, Expr[Any]] = Map.empty
  )

  abstract class FunctorDerivationRule(val name: String) extends Rule {
    def apply[F[_]](implicit FCtor: Type.Ctor1[F]): MIO[Rule.Applicability[FunctorCaseClassResult[F]]]
  }

  def deriveFunctorRecursively[F[_]](implicit FCtor: Type.Ctor1[F]): MIO[FunctorCaseClassResult[F]] = {
    implicit val AnyType: Type[Any] = FunctorTypes.Any
    val fName = FCtor.apply[Any].shortName
    Log.namedScope(s"Deriving Functor for $fName") {
      Rules(FunctorCaseClassRule)(_[F]).flatMap {
        case Right(result) =>
          Log.info(s"Derived Functor for $fName") >> MIO.pure(result)
        case Left(reasons) =>
          val reasonsStrings = reasons.toListMap.view.map { case (rule, rs) =>
            if (rs.isEmpty) s"The rule ${rule.name} was not applicable"
            else s" - ${rule.name}: ${rs.mkString(", ")}"
          }.toList
          val err = FunctorDerivationError.UnsupportedType(fName, reasonsStrings)
          Log.error(err.message) >> MIO.fail(err)
      }
    }
  }

  @scala.annotation.nowarn("msg=is never used|unused explicit parameter|unused local definition")
  protected def deriveFunctorForCaseClass[F[_]](
      result: FunctorCaseClassResult[F],
      runSafe: hearth.fp.DirectStyle.RunSafe[hearth.fp.effect.MIO]
  )(implicit FCtor: Type.Ctor1[F]): Expr[cats.Functor[F]] = {
    import hearth.kindlings.catsderivation.internal.runtime.CatsDerivationFactories

    val hasSelfRecursive = result.selfRecursiveFields.nonEmpty

    if (hasSelfRecursive) {
      Expr.quote {
        var self$macro: Any = null
        self$macro = CatsDerivationFactories.functorInstance[F] {
          (fa: F[CatsDerivationFactories.W1], f: CatsDerivationFactories.W1 => CatsDerivationFactories.W2) =>
            val _ = fa; val _ = f
            val r$0: F[Any] = Expr.splice {
              val body: MIO[Expr[F[Any]]] =
                deriveFunctorMapBody[F, CatsDerivationFactories.W1, CatsDerivationFactories.W2](
                  result.FCtor,
                  result.directFieldSet,
                  result.nestedFieldFunctors,
                  result.selfRecursiveFields,
                  result.selfRecursiveOuterFunctors,
                  Expr.quote(fa),
                  Expr.quote(f),
                  Some(Expr.quote(self$macro))
                )(
                  Type.of[CatsDerivationFactories.W1],
                  Type.of[CatsDerivationFactories.W2]
                ).asInstanceOf[MIO[Expr[F[Any]]]]
              runSafe(body)
            }
            r$0.asInstanceOf[F[CatsDerivationFactories.W2]]
        }
        self$macro.asInstanceOf[cats.Functor[F]]
      }
    } else {
      Expr.quote {
        CatsDerivationFactories.functorInstance[F] {
          (fa: F[CatsDerivationFactories.W1], f: CatsDerivationFactories.W1 => CatsDerivationFactories.W2) =>
            val _ = fa
            val _ = f
            Expr.splice {
              val body: MIO[Expr[F[CatsDerivationFactories.W2]]] =
                deriveFunctorMapBody[F, CatsDerivationFactories.W1, CatsDerivationFactories.W2](
                  result.FCtor,
                  result.directFieldSet,
                  result.nestedFieldFunctors,
                  result.selfRecursiveFields,
                  result.selfRecursiveOuterFunctors,
                  Expr.quote(fa),
                  Expr.quote(f),
                  None
                )(Type.of[CatsDerivationFactories.W1], Type.of[CatsDerivationFactories.W2])
              runSafe(body)
            }
        }
      }
    }
  }

  @scala.annotation.nowarn("msg=is never used|unused explicit parameter|unused local definition")
  def deriveFunctor[F[_]](FCtor0: Type.Ctor1[F], FunctorFType: Type[cats.Functor[F]]): Expr[cats.Functor[F]] = {
    val macroName = "Functor.derived"

    implicit val FCtor: Type.Ctor1[F] = FCtor0
    implicit val FunctorFT: Type[cats.Functor[F]] = FunctorFType

    Log
      .namedScope(s"Deriving Functor at: ${Environment.currentPosition.prettyPrint}") {
        MIO.scoped { runSafe =>
          val _ = runSafe(Environment.loadStandardExtensions().toMIO(allowFailures = false))
          implicit val AnyType: Type[Any] = FunctorTypes.Any
          val isCaseClass = CaseClass.parse(using FCtor.apply[Any]).toEither.isRight
          if (isCaseClass) {
            val result = runSafe(deriveFunctorRecursively[F])
            deriveFunctorForCaseClass(result, runSafe)(FCtor)
          } else {
            runSafe(deriveFunctorForEnum[F](FCtor, runSafe))
          }
        }
      }
      .flatTap(result => Log.info(s"Derived final result: ${result.prettyPrint}"))
      .runToExprOrFail(
        macroName,
        infoRendering = if (shouldWeLogFunctorDerivation) RenderFrom(Log.Level.Info) else DontRender,
        errorRendering = if (shouldWeLogFunctorDerivation) RenderFrom(Log.Level.Info) else DontRender,
        timeout = derivationTimeout
      ) { (errorLogs, errors) =>
        val errorsRendered = errors.map(e => "  - " + e.getMessage).mkString("\n")
        val hint =
          "Enable debug logging with: import hearth.kindlings.catsderivation.debug.logDerivationForCatsDerivation"
        if (errorLogs.nonEmpty) s"Macro derivation failed:\n$errorsRendered\nlogs:\n$errorLogs\n$hint"
        else s"Macro derivation failed:\n$errorsRendered\n$hint"
      }
  }

  @scala.annotation.nowarn("msg=is never used|unused implicit parameter")
  private def deriveFunctorMapBody[F[_], A, B](
      FCtor: Type.Ctor1[F],
      directFields: Set[String],
      nestedFieldFunctors: Map[String, Expr[Any]],
      selfRecursiveFields: Set[String],
      selfRecursiveOuterFunctors: Map[String, Expr[Any]],
      faExpr: Expr[F[A]],
      fExpr: Expr[A => B],
      selfOpt: Option[Expr[Any]]
  )(implicit AType: Type[A], BType: Type[B]): MIO[Expr[F[B]]] = {
    implicit val FAType: Type[F[A]] = FCtor.apply[A]
    implicit val FBType: Type[F[B]] = FCtor.apply[B]
    implicit val AnyType: Type[Any] = FunctorTypes.Any

    val selfOrFail: MIO[Expr[Any]] = selfOpt match {
      case Some(self) => MIO.pure(self)
      case None       => failDerivation(CatsDerivationError.MissingSelfReference("Functor"))
    }

    parseCaseClassMIO[F[A]]("F[A]").parTuple(parseCaseClassMIO[F[B]]("F[B]")).flatMap { case (caseClass, caseClassB) =>
      val bFieldMap = caseClassB.primaryConstructor.totalParameters.flatten.toMap

      val fields = caseClass.caseFieldValuesAt(faExpr).toList

      val mappedFieldsMIO: MIO[List[(String, Expr_??)]] = fields.traverse { case (fieldName, fieldValue) =>
        import fieldValue.Underlying as Field
        val fieldExpr = fieldValue.value.asInstanceOf[Expr[Field]]

        if (directFields.contains(fieldName)) {
          val mapped: Expr[B] = Expr.quote(Expr.splice(fExpr)(Expr.splice(fieldExpr.asInstanceOf[Expr[A]])))
          MIO.pure((fieldName, mapped.as_??))
        } else if (selfRecursiveFields.contains(fieldName) && selfRecursiveOuterFunctors.contains(fieldName)) {
          // Self-recursive nested field: e.g. Option[Search[A]] where Search is the parent.
          // Use outerFunctor.map(field)(inner => self.map(inner)(f))
          val outerFunctorExpr = selfRecursiveOuterFunctors(fieldName)
          selfOrFail.map { selfExpr =>
            val nestedResult: Expr[Any] = Expr.quote {
              hearth.kindlings.catsderivation.internal.runtime.HktNestedRuntime
                .mapNestedSelfRecursive(
                  Expr.splice(outerFunctorExpr),
                  Expr.splice(selfExpr),
                  Expr.splice(fieldExpr.upcast[Any]),
                  Expr.splice(fExpr.asInstanceOf[Expr[Any => Any]])
                )
            }
            val bParam = bFieldMap(fieldName)
            (fieldName, castAnyToTyped(nestedResult, bParam.tpe))
          }
        } else if (selfRecursiveFields.contains(fieldName)) {
          // Direct self-recursive field (no outer wrapper): e.g. F[A] itself
          selfOrFail.map { selfExpr =>
            val nestedResult: Expr[Any] = Expr.quote {
              hearth.kindlings.catsderivation.internal.runtime.HktNestedRuntime
                .mapNested(
                  Expr.splice(selfExpr),
                  Expr.splice(fieldExpr.upcast[Any]),
                  Expr.splice(fExpr.asInstanceOf[Expr[Any => Any]])
                )
            }
            val bParam = bFieldMap(fieldName)
            (fieldName, castAnyToTyped(nestedResult, bParam.tpe))
          }
        } else if (nestedFieldFunctors.contains(fieldName)) {
          val functorExpr = nestedFieldFunctors(fieldName)
          val nestedResult: Expr[Any] = Expr.quote {
            hearth.kindlings.catsderivation.internal.runtime.HktNestedRuntime
              .mapNested(
                Expr.splice(functorExpr),
                Expr.splice(fieldExpr.upcast[Any]),
                Expr.splice(fExpr.asInstanceOf[Expr[Any => Any]])
              )
          }
          val bParam = bFieldMap(fieldName)
          MIO.pure((fieldName, castAnyToTyped(nestedResult, bParam.tpe)))
        } else {
          MIO.pure((fieldName, fieldExpr.as_??))
        }
      }

      mappedFieldsMIO.flatMap { mappedFields =>
        constructInstanceFree(caseClassB.primaryConstructor, "Constructor", "mapped result")(mappedFields.toMap)
          .map(constructExpr => constructExpr.value.asInstanceOf[Expr[F[B]]])
      }
    }
  }

  private def castAnyToTyped(value: Expr[Any], targetType: ??): Expr_?? = {
    import targetType.Underlying as T
    Expr.quote(Expr.splice(value).asInstanceOf[T]).as_??
  }

  @scala.annotation.nowarn("msg=is never used|unused explicit parameter|unused local definition")
  private def deriveFunctorForEnum[F[_]](
      FCtor: Type.Ctor1[F],
      runSafe: hearth.fp.DirectStyle.RunSafe[hearth.fp.effect.MIO]
  ): MIO[Expr[cats.Functor[F]]] = {
    implicit val AnyType: Type[Any] = FunctorTypes.Any
    implicit val IntType: Type[Int] = FunctorTypes.Int
    implicit val StringType: Type[String] = FunctorTypes.String
    implicit val FAnyType: Type[F[Any]] = FCtor.apply[Any]
    implicit val FCtor1: Type.Ctor1[F] = FCtor

    val parentCtor = FCtor.asUntyped

    parseEnumMIO[F[Any]]("F[Any]").flatMap { enumm =>
      parseEnumMIO[Any]("F[Int]")(using FCtor.apply[Int].asInstanceOf[Type[Any]])
        .parTuple(parseEnumMIO[Any]("F[String]")(using FCtor.apply[String].asInstanceOf[Type[Any]]))
        .flatMap { case (enumInt, enumString) =>
          case class ChildFieldInfo(
              directFields: Set[String],
              nestedFieldFunctors: Map[String, Expr[Any]],
              selfRecursiveFields: Set[String]
          )

          val childInfo: Map[String, ChildFieldInfo] = runSafe {
            val childrenInt = enumInt.directChildren.toList
            val childrenString = enumString.directChildren.toList

            childrenInt
              .zip(childrenString)
              .traverse { case ((childName, childI), (_, childS)) =>
                import childI.Underlying as ChildI
                import childS.Underlying as ChildS

                val ccI = CaseClass.parse[ChildI].toEither
                val ccS = CaseClass.parse[ChildS].toEither

                (ccI, ccS) match {
                  case (Right(cI), Right(cS)) =>
                    val fieldsI = cI.primaryConstructor.totalParameters.flatten.toList
                    val fieldsS = cS.primaryConstructor.totalParameters.flatten.toList
                    val directFields = scala.collection.mutable.Set.empty[String]
                    val nestedFunctors = scala.collection.mutable.Map.empty[String, Expr[Any]]
                    val selfRecursive = scala.collection.mutable.Set.empty[String]

                    fieldsI
                      .zip(fieldsS)
                      .traverse { case ((name, pI), (_, pS)) =>
                        val tI = pI.tpe.Underlying
                        val tS = pS.tpe.Underlying
                        if (tI =:= IntType && tS =:= StringType) {
                          directFields += name
                          MIO.void
                        } else if (tI =:= tS) {
                          // invariant
                          MIO.void
                        } else {
                          val param = fieldsI.find(_._1 == name).get._2
                          import param.tpe.Underlying as FieldType
                          summonFunctorForFieldType(Type[FieldType].asInstanceOf[Type[Any]]) match {
                            case Some(functorExpr) =>
                              nestedFunctors += (name -> functorExpr)
                              MIO.void
                            case None =>
                              mkCtor1FromType(Type[FieldType].asInstanceOf[Type[Any]]) match {
                                case Some((_, nestedUntyped)) if nestedUntyped == parentCtor =>
                                  selfRecursive += name
                                  MIO.void
                                case _ =>
                                  failDerivation[Unit](
                                    CatsDerivationError.MissingInstanceForField("Functor", name, childName)
                                  )
                              }
                          }
                        }
                      }
                      .map { _ =>
                        childName -> ChildFieldInfo(directFields.toSet, nestedFunctors.toMap, selfRecursive.toSet)
                      }
                  case _ =>
                    MIO.pure(childName -> ChildFieldInfo(Set.empty, Map.empty, Set.empty))
                }
              }
              .map(_.toMap)
          }

          val hasSelfRecursive = childInfo.values.exists(_.selfRecursiveFields.nonEmpty)

          import hearth.kindlings.catsderivation.internal.runtime.CatsDerivationFactories

          def mkEnumMapBody(
              faExpr: Expr[F[Any]],
              fExpr: Expr[Any => Any],
              selfOpt: Option[Expr[Any]]
          ): Expr[F[Any]] =
            runSafe {
              enumm
                .matchOn[MIO, F[Any]](faExpr) { matched =>
                  import matched.{value as caseValue, Underlying as ChildType}
                  val childName = Type[ChildType].shortName
                  childInfo.get(childName) match {
                    case None =>
                      failDerivation(CatsDerivationError.MissingDerivationInfo("Functor", childName))
                    case Some(info) =>
                      parseCaseClassMIO[ChildType](s"child $childName").flatMap { childCC =>
                        val fields = childCC.caseFieldValuesAt(caseValue).toList
                        if (fields.isEmpty) {
                          MIO.pure(caseValue.upcast[F[Any]])
                        } else {
                          val mappedMIO: MIO[List[(String, Expr_??)]] = fields.traverse {
                            case (fieldName, fieldValue) =>
                              import fieldValue.Underlying as Field
                              val fieldExpr = fieldValue.value.asInstanceOf[Expr[Field]]
                              if (info.directFields.contains(fieldName)) {
                                val m: Expr[Any] =
                                  Expr.quote(Expr.splice(fExpr)(Expr.splice(fieldExpr.upcast[Any])))
                                val typedM: Expr[Field] = Expr.quote(Expr.splice(m).asInstanceOf[Field])
                                MIO.pure((fieldName, typedM.as_??))
                              } else if (
                                info.selfRecursiveFields.contains(fieldName) ||
                                info.nestedFieldFunctors.contains(fieldName)
                              ) {
                                val functorExprMIO: MIO[Expr[Any]] =
                                  if (info.selfRecursiveFields.contains(fieldName)) {
                                    selfOpt match {
                                      case Some(self) => MIO.pure(self)
                                      case None       =>
                                        failDerivation(CatsDerivationError.MissingSelfReference("Functor"))
                                    }
                                  } else MIO.pure(info.nestedFieldFunctors(fieldName))
                                functorExprMIO.map { functorExpr =>
                                  val nested: Expr[Any] = Expr.quote {
                                    hearth.kindlings.catsderivation.internal.runtime.HktNestedRuntime
                                      .mapNested(
                                        Expr.splice(functorExpr),
                                        Expr.splice(fieldExpr.upcast[Any]),
                                        Expr.splice(fExpr)
                                      )
                                  }
                                  val typedNested: Expr[Field] =
                                    Expr.quote(Expr.splice(nested).asInstanceOf[Field])
                                  (fieldName, typedNested.as_??)
                                }
                              } else {
                                MIO.pure((fieldName, fieldExpr.as_??))
                              }
                          }
                          mappedMIO.flatMap { mapped =>
                            constructInstanceFree(childCC.primaryConstructor, "Constructor", childName)(mapped.toMap)
                              .map(expr => expr.value.asInstanceOf[Expr[F[Any]]])
                          }
                        }
                      }
                  }
                }
                .flatMap {
                  case Some(r) => MIO.pure(r)
                  case None    => failDerivation(CatsDerivationError.EnumHasNoChildren(Type[F[Any]].prettyPrint))
                }
            }

          if (hasSelfRecursive) {
            MIO.pure {
              Expr.quote {
                var self$macro: Any = null
                self$macro = CatsDerivationFactories.functorInstance[F] {
                  (fa: F[CatsDerivationFactories.W1], f: CatsDerivationFactories.W1 => CatsDerivationFactories.W2) =>
                    val _ = fa; val _ = f
                    val r$0: F[Any] = Expr.splice {
                      mkEnumMapBody(
                        Expr.quote(fa.asInstanceOf[F[Any]]),
                        Expr.quote(f.asInstanceOf[Any => Any]),
                        Some(Expr.quote(self$macro))
                      )
                    }
                    r$0.asInstanceOf[F[CatsDerivationFactories.W2]]
                }
                self$macro.asInstanceOf[cats.Functor[F]]
              }
            }
          } else {
            MIO.pure {
              Expr.quote {
                CatsDerivationFactories.functorInstance[F] {
                  (fa: F[CatsDerivationFactories.W1], f: CatsDerivationFactories.W1 => CatsDerivationFactories.W2) =>
                    val _ = fa; val _ = f
                    val r$0: F[Any] = Expr.splice {
                      mkEnumMapBody(
                        Expr.quote(fa.asInstanceOf[F[Any]]),
                        Expr.quote(f.asInstanceOf[Any => Any]),
                        None
                      )
                    }
                    r$0.asInstanceOf[F[CatsDerivationFactories.W2]]
                }
              }
            }
          }
        }
    }
  }

  protected type AnyK[X] = Any

  /** Higher-kinded constructor for `cats.Functor`, used to build `Type[cats.Functor[G]]` for a discovered `G` via
    * `CatsFunctorCtor.apply(using gCtor)` (Issue #284).
    */
  protected lazy val CatsFunctorCtor: Type.CtorK1[cats.Functor] = Type.CtorK1.of[cats.Functor]

  protected object FunctorTypes {
    val Any: Type[Any] = Type.of[Any]
    val Int: Type[Int] = Type.of[Int]
    val String: Type[String] = Type.of[String]
    val LogDerivation: Type[hearth.kindlings.catsderivation.LogDerivation] =
      Type.of[hearth.kindlings.catsderivation.LogDerivation]
  }

  def shouldWeLogFunctorDerivation: Boolean = {
    implicit val LogDerivationType: Type[LogDerivation] = FunctorTypes.LogDerivation
    Expr.summonImplicit[LogDerivation].isDefined || (for {
      data <- Environment.typedSettings.toOption
      cd <- data.get("catsDerivation")
      shouldLog <- cd.get("logDerivation").flatMap(_.asBoolean)
    } yield shouldLog).getOrElse(false)
  }
}

sealed private[compiletime] trait FunctorDerivationError
    extends util.control.NoStackTrace
    with Product
    with Serializable {
  def message: String
  override def getMessage(): String = message
}
private[compiletime] object FunctorDerivationError {
  final case class UnsupportedType(tpeName: String, reasons: List[String]) extends FunctorDerivationError {
    override def message: String =
      s"The type constructor $tpeName was not handled by any Functor derivation rule:\n${reasons.mkString("\n")}"
  }
}
