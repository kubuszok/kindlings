package hearth.kindlings.catsderivation.internal.compiletime

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

import hearth.kindlings.catsderivation.LogDerivation

/** Foldable derivation: foldLeft and foldRight over direct type parameter fields.
  *
  * Uses free type variables A and B directly in the generated code, relying on Hearth 0.2.0-264+ cross-quotes support
  * for method-level type parameters inside Expr.quote/Expr.splice.
  */
trait FoldableMacrosImpl extends CatsDerivationTimeout { this: MacroCommons & StdExtensions =>

  protected def summonFoldableForFieldType(fieldType: Type[Any]): Option[Expr[Any]]

  @scala.annotation.nowarn("msg=is never used|unused explicit parameter")
  def deriveFoldable[F[_]](
      FCtor0: Type.Ctor1[F],
      FoldableFType: Type[cats.Foldable[F]]
  ): Expr[cats.Foldable[F]] = {
    val macroName = "Foldable.derived"

    implicit val FCtor: Type.Ctor1[F] = FCtor0
    implicit val FoldableFT: Type[cats.Foldable[F]] = FoldableFType

    Log
      .namedScope(s"Deriving Foldable at: ${Environment.currentPosition.prettyPrint}") {
        implicit val AnyType: Type[Any] = FoldableTypes.Any
        val isCaseClass = CaseClass.parse(using FCtor.apply[Any]).toEither.isRight
        if (!isCaseClass) {
          MIO.scoped { runSafe =>
            val _ = runSafe(Environment.loadStandardExtensions().toMIO(allowFailures = false))
            runSafe(deriveFoldableForEnum[F](FCtor, runSafe))
          }
        } else
          MIO.scoped { runSafe =>
            implicit val IntType: Type[Int] = FoldableTypes.Int
            implicit val StringType: Type[String] = FoldableTypes.String

            val ccInt = CaseClass.parse(using FCtor.apply[Int]).toEither match {
              case Right(cc) => cc
              case Left(e)   => throw new RuntimeException(s"Cannot parse F[Int]: $e")
            }
            val ccString = CaseClass.parse(using FCtor.apply[String]).toEither match {
              case Right(cc) => cc
              case Left(e)   => throw new RuntimeException(s"Cannot parse F[String]: $e")
            }

            val fieldsInt = ccInt.primaryConstructor.totalParameters.flatten.toList
            val fieldsString = ccString.primaryConstructor.totalParameters.flatten.toList

            val directFields = scala.collection.mutable.Set.empty[String]
            val nestedFieldNames = scala.collection.mutable.ListBuffer.empty[String]

            fieldsInt.zip(fieldsString).foreach { case ((name, pInt), (_, pString)) =>
              val tInt = pInt.tpe.Underlying
              val tString = pString.tpe.Underlying
              if (tInt =:= IntType && tString =:= StringType) {
                directFields += name
              } else if (tInt =:= tString) {
                // Invariant — skip in fold
              } else {
                nestedFieldNames += name
              }
            }

            val nestedFieldFoldables = scala.collection.mutable.Map.empty[String, Expr[Any]]
            if (nestedFieldNames.nonEmpty) {
              val unsupported = scala.collection.mutable.ListBuffer.empty[String]
              nestedFieldNames.foreach { name =>
                val param = fieldsInt.find(_._1 == name).get._2
                import param.tpe.Underlying as FieldType
                summonFoldableForFieldType(Type[FieldType].asInstanceOf[Type[Any]]) match {
                  case Some(foldableExpr) => nestedFieldFoldables += (name -> foldableExpr)
                  case None               => unsupported += name
                }
              }
              if (unsupported.nonEmpty) {
                throw new RuntimeException(
                  s"Cannot derive Foldable: fields ${unsupported.mkString(", ")} contain nested type constructors " +
                    "without Foldable instances."
                )
              }
            }

            val directFieldSet: Set[String] = directFields.toSet
            val nestedFieldMap: Map[String, Expr[Any]] = nestedFieldFoldables.toMap

            // Pre-load extensions before entering the quote
            runSafe {
              Environment.loadStandardExtensions().toMIO(allowFailures = false).map(_ => ())
            }

            implicit val FAnyType: Type[F[Any]] = FCtor.apply[Any]
            implicit val EvalAnyType: Type[cats.Eval[Any]] = FoldableTypes.EvalCtor.apply[Any]

            val ccAny = CaseClass.parse[F[Any]].toEither match {
              case Right(cc) => cc
              case Left(e)   => throw new RuntimeException(s"Cannot parse F[Any]: $e")
            }

            val doFoldLeft: (Expr[F[Any]], Expr[Any], Expr[(Any, Any) => Any]) => Expr[Any] =
              (faExpr, bExpr, fExpr) =>
                runSafe {
                  val fields = ccAny.caseFieldValuesAt(faExpr).toList
                  var result: Expr[Any] = bExpr
                  fields.foreach { case (fieldName, fieldValue) =>
                    import fieldValue.Underlying as Field
                    val fieldExpr = fieldValue.value.asInstanceOf[Expr[Field]]
                    if (directFieldSet.contains(fieldName)) {
                      result = Expr.quote(Expr.splice(fExpr)(Expr.splice(result), Expr.splice(fieldExpr.upcast[Any])))
                    } else if (nestedFieldMap.contains(fieldName)) {
                      val foldableExpr = nestedFieldMap(fieldName)
                      result = Expr.quote {
                        hearth.kindlings.catsderivation.internal.runtime.HktNestedRuntime
                          .foldLeftNested(
                            Expr.splice(foldableExpr),
                            Expr.splice(fieldExpr.upcast[Any]),
                            Expr.splice(result),
                            Expr.splice(fExpr)
                          )
                      }
                    }
                  }
                  MIO.pure(result)
                }

            val doFoldRight
                : (Expr[F[Any]], Expr[cats.Eval[Any]], Expr[(Any, cats.Eval[Any]) => cats.Eval[Any]]) => Expr[
                  cats.Eval[Any]
                ] =
              (faExpr, lbExpr, fExpr) =>
                runSafe {
                  val fields = ccAny.caseFieldValuesAt(faExpr).toList
                  val foldableFields: List[(String, Expr[Any], Option[Expr[Any]])] = fields.collect {
                    case (fieldName, fieldValue) if directFieldSet.contains(fieldName) =>
                      import fieldValue.Underlying as Field
                      (fieldName, fieldValue.value.asInstanceOf[Expr[Field]].upcast[Any], None)
                    case (fieldName, fieldValue) if nestedFieldMap.contains(fieldName) =>
                      import fieldValue.Underlying as Field
                      (
                        fieldName,
                        fieldValue.value.asInstanceOf[Expr[Field]].upcast[Any],
                        Some(nestedFieldMap(fieldName))
                      )
                  }
                  val result = foldableFields.foldRight(lbExpr) { case ((_, fieldExpr, foldableOpt), acc) =>
                    foldableOpt match {
                      case None =>
                        Expr.quote(Expr.splice(fExpr)(Expr.splice(fieldExpr), Expr.splice(acc)))
                      case Some(foldableExpr) =>
                        Expr.quote {
                          hearth.kindlings.catsderivation.internal.runtime.HktNestedRuntime
                            .foldRightNested(
                              Expr.splice(foldableExpr),
                              Expr.splice(fieldExpr),
                              Expr.splice(acc),
                              Expr.splice(fExpr)
                            )
                        }
                    }
                  }
                  MIO.pure(result)
                }

            import hearth.kindlings.catsderivation.internal.runtime.CatsDerivationFactories
            Expr.quote {
              CatsDerivationFactories.foldableInstance[F](
                foldLeftFn = { (fa: F[CatsDerivationFactories.W1], bAny: Any, fAny: (Any, Any) => Any) =>
                  val anyFa: F[Any] = fa.asInstanceOf[F[Any]]
                  val _ = anyFa
                  val _ = bAny
                  val _ = fAny
                  Expr
                    .splice(doFoldLeft(Expr.quote(anyFa), Expr.quote(bAny), Expr.quote(fAny)))
                    .asInstanceOf[Any]
                },
                foldRightFn = {
                  (
                      fa: F[CatsDerivationFactories.W1],
                      lbAny: cats.Eval[Any],
                      fAny: (Any, cats.Eval[Any]) => cats.Eval[Any]
                  ) =>
                    val anyFa: F[Any] = fa.asInstanceOf[F[Any]]
                    val _ = anyFa
                    val _ = lbAny
                    val _ = fAny
                    Expr
                      .splice(doFoldRight(Expr.quote(anyFa), Expr.quote(lbAny), Expr.quote(fAny)))
                      .asInstanceOf[cats.Eval[Any]]
                }
              )
            }
          }
      }
      .flatTap(result => Log.info(s"Derived final result: ${result.prettyPrint}"))
      .runToExprOrFail(
        macroName,
        infoRendering = if (shouldWeLogFoldableDerivation) RenderFrom(Log.Level.Info) else DontRender,
        errorRendering = if (shouldWeLogFoldableDerivation) RenderFrom(Log.Level.Info) else DontRender,
        timeout = derivationTimeout
      ) { (errorLogs, errors) =>
        val errorsRendered = errors.map(e => "  - " + e.getMessage).mkString("\n")
        val hint =
          "Enable debug logging with: import hearth.kindlings.catsderivation.debug.logDerivationForCatsDerivation"
        if (errorLogs.nonEmpty) s"Macro derivation failed:\n$errorsRendered\nlogs:\n$errorLogs\n$hint"
        else s"Macro derivation failed:\n$errorsRendered\n$hint"
      }
  }

  protected object FoldableTypes {
    val Any: Type[Any] = Type.of[Any]
    val Int: Type[Int] = Type.of[Int]
    val String: Type[String] = Type.of[String]
    def EvalCtor: Type.Ctor1[cats.Eval] = Type.Ctor1.of[cats.Eval]
    val LogDerivation: Type[hearth.kindlings.catsderivation.LogDerivation] =
      Type.of[hearth.kindlings.catsderivation.LogDerivation]
  }

  protected def mkCtor1FromTypeFoldable(appliedType: Type[Any]): Option[UntypedType]

  @scala.annotation.nowarn("msg=is never used|unused explicit parameter|unused local definition")
  private def deriveFoldableForEnum[F[_]](
      FCtor: Type.Ctor1[F],
      runSafe: hearth.fp.DirectStyle.RunSafe[hearth.fp.effect.MIO]
  ): MIO[Expr[cats.Foldable[F]]] = {
    implicit val AnyType: Type[Any] = FoldableTypes.Any
    implicit val IntType: Type[Int] = FoldableTypes.Int
    implicit val StringType: Type[String] = FoldableTypes.String
    implicit val FAnyType: Type[F[Any]] = FCtor.apply[Any]
    implicit val EvalAnyType: Type[cats.Eval[Any]] = FoldableTypes.EvalCtor.apply[Any]
    implicit val FCtor1: Type.Ctor1[F] = FCtor

    val parentCtor = FCtor.asUntyped

    Enum.parse[F[Any]].toEither match {
      case Left(reason) =>
        MIO.fail(new RuntimeException(s"Cannot parse as enum for Foldable: $reason"))
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
            nestedFoldables: Map[String, Expr[Any]],
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
                val fieldsI = cI.primaryConstructor.totalParameters.flatten.toList
                val fieldsS = cS.primaryConstructor.totalParameters.flatten.toList
                val direct = scala.collection.mutable.Set.empty[String]
                val nested = scala.collection.mutable.Map.empty[String, Expr[Any]]
                val selfRec = scala.collection.mutable.Set.empty[String]
                fieldsI.zip(fieldsS).foreach { case ((name, pI), (_, pS)) =>
                  val tI = pI.tpe.Underlying
                  val tS = pS.tpe.Underlying
                  if (tI =:= IntType && tS =:= StringType) direct += name
                  else if (tI =:= tS) ()
                  else {
                    val param = fieldsI.find(_._1 == name).get._2
                    import param.tpe.Underlying as FT
                    summonFoldableForFieldType(Type[FT].asInstanceOf[Type[Any]]) match {
                      case Some(expr) => nested += (name -> expr)
                      case None       =>
                        mkCtor1FromTypeFoldable(Type[FT].asInstanceOf[Type[Any]]) match {
                          case Some(ut) if ut == parentCtor => selfRec += name
                          case _ => throw new RuntimeException(s"No Foldable for nested field $name in $childName")
                        }
                    }
                  }
                }
                result += (childName -> ChildFieldInfo(direct.toSet, nested.toMap, selfRec.toSet))
              case _ =>
                result += (childName -> ChildFieldInfo(Set.empty, Map.empty, Set.empty))
            }
          }
          MIO.pure(result.toMap)
        }

        val hasSelfRecursive = childInfo.values.exists(_.selfRecursiveFields.nonEmpty)

        import hearth.kindlings.catsderivation.internal.runtime.CatsDerivationFactories

        def mkFoldLeftBody(
            faExpr: Expr[F[Any]],
            bExpr: Expr[Any],
            fExpr: Expr[(Any, Any) => Any],
            selfOpt: Option[Expr[Any]]
        ): Expr[Any] = runSafe {
          enumm
            .matchOn[MIO, Any](faExpr) { matched =>
              import matched.{value as caseValue, Underlying as CT}
              val cn = Type[CT].shortName
              childInfo.get(cn) match {
                case None       => MIO.fail(new RuntimeException(s"No info for $cn"))
                case Some(info) =>
                  val cc = CaseClass.parse[CT].toEither match {
                    case Right(c) => c; case Left(e) => throw new RuntimeException(e.toString)
                  }
                  val fields = cc.caseFieldValuesAt(caseValue).toList
                  var result: Expr[Any] = bExpr
                  fields.foreach { case (fn, fv) =>
                    import fv.Underlying as Field
                    val fe = fv.value.asInstanceOf[Expr[Field]]
                    if (info.directFields.contains(fn))
                      result = Expr.quote(Expr.splice(fExpr)(Expr.splice(result), Expr.splice(fe.upcast[Any])))
                    else if (info.selfRecursiveFields.contains(fn) || info.nestedFoldables.contains(fn)) {
                      val foldableE =
                        if (info.selfRecursiveFields.contains(fn))
                          selfOpt.getOrElse(throw new RuntimeException("No self"))
                        else info.nestedFoldables(fn)
                      result = Expr.quote {
                        hearth.kindlings.catsderivation.internal.runtime.HktNestedRuntime
                          .foldLeftNested(
                            Expr.splice(foldableE),
                            Expr.splice(fe.upcast[Any]),
                            Expr.splice(result),
                            Expr.splice(fExpr)
                          )
                      }
                    }
                  }
                  MIO.pure(result)
              }
            }
            .flatMap { case Some(r) => MIO.pure(r); case None => MIO.fail(new RuntimeException("No children")) }
        }

        def mkFoldRightBody(
            faExpr: Expr[F[Any]],
            lbExpr: Expr[cats.Eval[Any]],
            fExpr: Expr[(Any, cats.Eval[Any]) => cats.Eval[Any]],
            selfOpt: Option[Expr[Any]]
        ): Expr[cats.Eval[Any]] = runSafe {
          enumm
            .matchOn[MIO, cats.Eval[Any]](faExpr) { matched =>
              import matched.{value as caseValue, Underlying as CT}
              val cn = Type[CT].shortName
              childInfo.get(cn) match {
                case None       => MIO.fail(new RuntimeException(s"No info for $cn"))
                case Some(info) =>
                  val cc = CaseClass.parse[CT].toEither match {
                    case Right(c) => c; case Left(e) => throw new RuntimeException(e.toString)
                  }
                  val fields = cc.caseFieldValuesAt(caseValue).toList
                  val foldable: List[(String, Expr[Any], Option[Expr[Any]])] = fields.collect {
                    case (fn, fv) if info.directFields.contains(fn) =>
                      import fv.Underlying as Field
                      (fn, fv.value.asInstanceOf[Expr[Field]].upcast[Any], None)
                    case (fn, fv) if info.selfRecursiveFields.contains(fn) || info.nestedFoldables.contains(fn) =>
                      import fv.Underlying as Field
                      val fe =
                        if (info.selfRecursiveFields.contains(fn))
                          selfOpt.getOrElse(throw new RuntimeException("No self"))
                        else info.nestedFoldables(fn)
                      (fn, fv.value.asInstanceOf[Expr[Field]].upcast[Any], Some(fe))
                  }
                  val result = foldable.foldRight(lbExpr) { case ((_, fieldExpr, foldableOpt), acc) =>
                    foldableOpt match {
                      case None =>
                        Expr.quote(Expr.splice(fExpr)(Expr.splice(fieldExpr), Expr.splice(acc)))
                      case Some(foldableE) =>
                        Expr.quote {
                          hearth.kindlings.catsderivation.internal.runtime.HktNestedRuntime
                            .foldRightNested(
                              Expr.splice(foldableE),
                              Expr.splice(fieldExpr),
                              Expr.splice(acc),
                              Expr.splice(fExpr)
                            )
                        }
                    }
                  }
                  MIO.pure(result)
              }
            }
            .flatMap { case Some(r) => MIO.pure(r); case None => MIO.fail(new RuntimeException("No children")) }
        }

        if (hasSelfRecursive) {
          MIO.pure {
            Expr.quote {
              var self$macro: Any = null
              self$macro = CatsDerivationFactories.foldableInstance[F](
                foldLeftFn = { (fa: F[CatsDerivationFactories.W1], bAny: Any, fAny: (Any, Any) => Any) =>
                  val anyFa: F[Any] = fa.asInstanceOf[F[Any]]
                  val _ = anyFa; val _ = bAny; val _ = fAny
                  Expr.splice(
                    mkFoldLeftBody(Expr.quote(anyFa), Expr.quote(bAny), Expr.quote(fAny), Some(Expr.quote(self$macro)))
                  )
                },
                foldRightFn = {
                  (
                      fa: F[CatsDerivationFactories.W1],
                      lbAny: cats.Eval[Any],
                      fAny: (Any, cats.Eval[Any]) => cats.Eval[Any]
                  ) =>
                    val anyFa: F[Any] = fa.asInstanceOf[F[Any]]
                    val _ = anyFa; val _ = lbAny; val _ = fAny
                    Expr.splice(
                      mkFoldRightBody(
                        Expr.quote(anyFa),
                        Expr.quote(lbAny),
                        Expr.quote(fAny),
                        Some(Expr.quote(self$macro))
                      )
                    )
                }
              )
              self$macro.asInstanceOf[cats.Foldable[F]]
            }
          }
        } else {
          MIO.pure {
            Expr.quote {
              CatsDerivationFactories.foldableInstance[F](
                foldLeftFn = { (fa: F[CatsDerivationFactories.W1], bAny: Any, fAny: (Any, Any) => Any) =>
                  val anyFa: F[Any] = fa.asInstanceOf[F[Any]]
                  val _ = anyFa; val _ = bAny; val _ = fAny
                  Expr.splice(mkFoldLeftBody(Expr.quote(anyFa), Expr.quote(bAny), Expr.quote(fAny), None))
                },
                foldRightFn = {
                  (
                      fa: F[CatsDerivationFactories.W1],
                      lbAny: cats.Eval[Any],
                      fAny: (Any, cats.Eval[Any]) => cats.Eval[Any]
                  ) =>
                    val anyFa: F[Any] = fa.asInstanceOf[F[Any]]
                    val _ = anyFa; val _ = lbAny; val _ = fAny
                    Expr.splice(mkFoldRightBody(Expr.quote(anyFa), Expr.quote(lbAny), Expr.quote(fAny), None))
                }
              )
            }
          }
        }
    }
  }

  def shouldWeLogFoldableDerivation: Boolean = {
    implicit val LogDerivationType: Type[LogDerivation] = FoldableTypes.LogDerivation
    Expr.summonImplicit[LogDerivation].isDefined || (for {
      data <- Environment.typedSettings.toOption
      cd <- data.get("catsDerivation")
      shouldLog <- cd.get("logDerivation").flatMap(_.asBoolean)
    } yield shouldLog).getOrElse(false)
  }
}
