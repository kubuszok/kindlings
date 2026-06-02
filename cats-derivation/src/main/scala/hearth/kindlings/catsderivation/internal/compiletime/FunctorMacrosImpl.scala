package hearth.kindlings.catsderivation.internal.compiletime

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

import hearth.kindlings.catsderivation.LogDerivation

/** Functor derivation: maps over type parameter fields in case classes.
  *
  * Uses free type variables A and B directly in the generated code, relying on Hearth 0.2.0-264+ cross-quotes support
  * for method-level type parameters inside Expr.quote/Expr.splice.
  */
trait FunctorMacrosImpl extends rules.FunctorCaseClassRuleImpl with CatsDerivationTimeout {
  this: MacroCommons & StdExtensions =>

  protected def summonFunctorForFieldType(fieldType: Type[Any]): Option[Expr[Any]]

  protected def mkCtor1FromType(appliedType: Type[Any]): Option[(Type.Ctor1[AnyK], UntypedType)]

  /** Given an applied type `G[X]`, extract the single type argument `X` as a `Type[Any]`. Returns None if the type is
    * not a single-argument applied type.
    */
  protected def extractSingleTypeArg(appliedType: Type[Any]): Option[Type[Any]]

  /** Check whether the given applied type `G[X]` has an inner type argument `X` whose type constructor matches the
    * given parent constructor. For example, `Option[Search[Int]]` with parent `Search` would return true because
    * `Search` (from `Search[Int]`) matches the parent.
    */
  protected def isNestedSelfRecursive(fieldType: Type[Any], parentCtor: UntypedType): Boolean

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

    val caseClass = CaseClass.parse[F[A]].toEither match {
      case Right(cc) => cc
      case Left(e)   => throw new RuntimeException(s"Cannot parse F[A]: $e")
    }
    val caseClassB = CaseClass.parse[F[B]].toEither match {
      case Right(cc) => cc
      case Left(e)   => throw new RuntimeException(s"Cannot parse F[B]: $e")
    }
    val bFieldMap = caseClassB.primaryConstructor.parameters.flatten.toMap

    val fields = caseClass.caseFieldValuesAt(faExpr).toList

    val mappedFields: List[(String, Expr_??)] = fields.map { case (fieldName, fieldValue) =>
      import fieldValue.Underlying as Field
      val fieldExpr = fieldValue.value.asInstanceOf[Expr[Field]]

      if (directFields.contains(fieldName)) {
        val mapped: Expr[B] = Expr.quote(Expr.splice(fExpr)(Expr.splice(fieldExpr.asInstanceOf[Expr[A]])))
        (fieldName, mapped.as_??)
      } else if (selfRecursiveFields.contains(fieldName) && selfRecursiveOuterFunctors.contains(fieldName)) {
        // Self-recursive nested field: e.g. Option[Search[A]] where Search is the parent.
        // Use outerFunctor.map(field)(inner => self.map(inner)(f))
        val outerFunctorExpr = selfRecursiveOuterFunctors(fieldName)
        val selfExpr = selfOpt.getOrElse(
          throw new RuntimeException("Self-recursive field but no self Functor reference")
        )
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
      } else if (selfRecursiveFields.contains(fieldName)) {
        // Direct self-recursive field (no outer wrapper): e.g. F[A] itself
        val selfExpr = selfOpt.getOrElse(
          throw new RuntimeException("Self-recursive field but no self Functor reference")
        )
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
        (fieldName, castAnyToTyped(nestedResult, bParam.tpe))
      } else {
        (fieldName, fieldExpr.as_??)
      }
    }
    caseClassB.primaryConstructor(mappedFields.toMap) match {
      case Right(constructExpr) => MIO.pure(constructExpr)
      case Left(error)          =>
        MIO.fail(new RuntimeException(s"Cannot construct mapped result: $error"))
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

    Enum.parse[F[Any]].toEither match {
      case Left(reason) =>
        MIO.fail(new RuntimeException(s"Cannot parse as enum: $reason"))
      case Right(enumm) =>
        val enumInt = Enum.parse(using FCtor.apply[Int].asInstanceOf[Type[Any]]).toEither match {
          case Right(e) => e
          case Left(_)  => return MIO.fail(new RuntimeException("Cannot parse F[Int] as enum"))
        }
        val enumString = Enum.parse(using FCtor.apply[String].asInstanceOf[Type[Any]]).toEither match {
          case Right(e) => e
          case Left(_)  => return MIO.fail(new RuntimeException("Cannot parse F[String] as enum"))
        }

        case class ChildFieldInfo(
            directFields: Set[String],
            nestedFieldFunctors: Map[String, Expr[Any]],
            selfRecursiveFields: Set[String]
        )

        val childInfo: Map[String, ChildFieldInfo] = runSafe {
          val result = scala.collection.mutable.Map.empty[String, ChildFieldInfo]
          val childrenInt = enumInt.directChildren.toList
          val childrenString = enumString.directChildren.toList

          childrenInt.zip(childrenString).foreach { case ((childName, childI), (_, childS)) =>
            import childI.Underlying as ChildI
            import childS.Underlying as ChildS

            val ccI = CaseClass.parse[ChildI].toEither
            val ccS = CaseClass.parse[ChildS].toEither

            (ccI, ccS) match {
              case (Right(cI), Right(cS)) =>
                val fieldsI = cI.primaryConstructor.parameters.flatten.toList
                val fieldsS = cS.primaryConstructor.parameters.flatten.toList
                val directFields = scala.collection.mutable.Set.empty[String]
                val nestedFunctors = scala.collection.mutable.Map.empty[String, Expr[Any]]
                val selfRecursive = scala.collection.mutable.Set.empty[String]

                fieldsI.zip(fieldsS).foreach { case ((name, pI), (_, pS)) =>
                  val tI = pI.tpe.Underlying
                  val tS = pS.tpe.Underlying
                  if (tI =:= IntType && tS =:= StringType) {
                    directFields += name
                  } else if (tI =:= tS) {
                    // invariant
                  } else {
                    val param = fieldsI.find(_._1 == name).get._2
                    import param.tpe.Underlying as FieldType
                    summonFunctorForFieldType(Type[FieldType].asInstanceOf[Type[Any]]) match {
                      case Some(functorExpr) => nestedFunctors += (name -> functorExpr)
                      case None              =>
                        mkCtor1FromType(Type[FieldType].asInstanceOf[Type[Any]]) match {
                          case Some((_, nestedUntyped)) if nestedUntyped == parentCtor =>
                            selfRecursive += name
                          case _ =>
                            throw new RuntimeException(
                              s"Cannot find or derive Functor for nested field $name in $childName"
                            )
                        }
                    }
                  }
                }
                result += (childName -> ChildFieldInfo(directFields.toSet, nestedFunctors.toMap, selfRecursive.toSet))
              case _ =>
                result += (childName -> ChildFieldInfo(Set.empty, Map.empty, Set.empty))
            }
          }
          MIO.pure(result.toMap)
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
                    MIO.fail(new RuntimeException(s"No derivation info for child $childName"))
                  case Some(info) =>
                    val childCC = CaseClass.parse[ChildType].toEither match {
                      case Right(cc) => cc
                      case Left(e)   => throw new RuntimeException(s"Cannot parse child $childName: $e")
                    }
                    val fields = childCC.caseFieldValuesAt(caseValue).toList
                    if (fields.isEmpty) {
                      MIO.pure(caseValue.upcast[F[Any]])
                    } else {
                      val mapped: List[(String, Expr_??)] = fields.map { case (fieldName, fieldValue) =>
                        import fieldValue.Underlying as Field
                        val fieldExpr = fieldValue.value.asInstanceOf[Expr[Field]]
                        if (info.directFields.contains(fieldName)) {
                          val m: Expr[Any] =
                            Expr.quote(Expr.splice(fExpr)(Expr.splice(fieldExpr.upcast[Any])))
                          val typedM: Expr[Field] = Expr.quote(Expr.splice(m).asInstanceOf[Field])
                          (fieldName, typedM.as_??)
                        } else if (
                          info.selfRecursiveFields.contains(fieldName) ||
                          info.nestedFieldFunctors.contains(fieldName)
                        ) {
                          val functorExpr = if (info.selfRecursiveFields.contains(fieldName)) {
                            selfOpt.getOrElse(
                              throw new RuntimeException("Self-recursive field but no self Functor")
                            )
                          } else info.nestedFieldFunctors(fieldName)
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
                        } else {
                          (fieldName, fieldExpr.as_??)
                        }
                      }
                      childCC.primaryConstructor(mapped.toMap) match {
                        case Right(expr) => MIO.pure(expr.upcast[F[Any]])
                        case Left(err)   => MIO.fail(new RuntimeException(s"Cannot construct $childName: $err"))
                      }
                    }
                }
              }
              .flatMap {
                case Some(r) => MIO.pure(r)
                case None    => MIO.fail(new RuntimeException("Enum has no children"))
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

  protected type AnyK[X] = Any

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
