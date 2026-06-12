package hearth.kindlings.catsderivation.internal.compiletime

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.fp.instances.*
import hearth.fp.syntax.*
import hearth.std.*

import hearth.kindlings.catsderivation.LogDerivation

/** Traverse derivation: traverse + map + foldLeft + foldRight.
  *
  * Supports direct (A) and nested (G[A] with Traverse[G]) type parameter fields. Invariant fields are passed through.
  *
  * The traverse body generates the G[List[Any]] fold chain at macro time, allowing distinct treatment for direct fields
  * (apply f) and nested fields (delegate to Traverse[G].traverse). A LambdaBuilder-built reconstruction lambda rebuilds
  * the case class from the resulting List[Any] + original invariant field values.
  */
trait TraverseMacrosImpl extends CatsDerivationTimeout with CatsDerivationErrorSupport {
  this: MacroCommons & StdExtensions =>

  protected def summonTraverseForFieldType(fieldType: Type[Any]): Option[Expr[Any]]

  @scala.annotation.nowarn("msg=is never used|unused explicit parameter")
  def deriveTraverse[F[_]](
      FCtor0: Type.Ctor1[F],
      TraverseFType: Type[cats.Traverse[F]]
  ): Expr[cats.Traverse[F]] = {
    val macroName = "Traverse.derived"

    implicit val FCtor: Type.Ctor1[F] = FCtor0
    implicit val TraverseFT: Type[cats.Traverse[F]] = TraverseFType
    implicit val AnyType: Type[Any] = TraverseTypes.Any
    implicit val FAnyType: Type[F[Any]] = FCtor.apply[Any]
    implicit val EvalAnyType: Type[cats.Eval[Any]] = TraverseTypes.EvalAny
    implicit val ListAnyType: Type[List[Any]] = TraverseTypes.ListAny

    Log
      .namedScope(s"Deriving Traverse at: ${Environment.currentPosition.prettyPrint}") {
        CaseClass.parse[F[Any]].toEither match {
          case Right(caseClass) =>
            MIO.scoped { runSafe =>
              implicit val IntType: Type[Int] = TraverseTypes.Int
              implicit val StringType: Type[String] = TraverseTypes.String

              val ccInt = runSafe(parseCaseClassMIO[F[Int]]("F[Int]")(using FCtor.apply[Int]))
              val ccString = runSafe(parseCaseClassMIO[F[String]]("F[String]")(using FCtor.apply[String]))

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
                  // Invariant — skip in traverse
                } else {
                  nestedFieldNames += name
                }
              }

              val nestedFieldTraverses = scala.collection.mutable.Map.empty[String, Expr[Any]]
              if (nestedFieldNames.nonEmpty) {
                val unsupported = scala.collection.mutable.ListBuffer.empty[String]
                nestedFieldNames.foreach { name =>
                  val param = fieldsInt.find(_._1 == name).get._2
                  import param.tpe.Underlying as FieldType
                  summonTraverseForFieldType(Type[FieldType].asInstanceOf[Type[Any]]) match {
                    case Some(traverseExpr) => nestedFieldTraverses += (name -> traverseExpr)
                    case None               => unsupported += name
                  }
                }
                if (unsupported.nonEmpty) {
                  runSafe {
                    failDerivation[Unit](
                      CatsDerivationError.UnsupportedFieldShape(
                        "Traverse",
                        unsupported.mkString(", "),
                        "the fields contain nested type constructors without Traverse instances"
                      )
                    )
                  }
                }
              }

              val directFieldSet: Set[String] = directFields.toSet
              val nestedFieldMap: Map[String, Expr[Any]] = nestedFieldTraverses.toMap

              // Pre-load extensions eagerly to avoid Scala 3 sibling splice isolation issues
              val _ = runSafe {
                Environment.loadStandardExtensions().toMIO(allowFailures = false)
              }

              val traversableFieldSet: Set[String] = directFieldSet ++ nestedFieldMap.keySet

              // Build reconstruction lambda: (List[Any], Any) => Any
              // Uses Any instead of F[Any] to avoid F references inside lambdas on Scala 2
              val reconstructFn: Expr[(List[Any], Any) => Any] = runSafe {
                deriveReconstructFromList[F](caseClass, traversableFieldSet)
              }

              // Traverse body: generates the G[List[Any]] fold chain at macro time
              val doTraverse: (Expr[F[Any]], Expr[Any], Expr[Any]) => Expr[Any] =
                (faExprM, fExprM, gExprM) =>
                  runSafe {
                    deriveTraverseBody[F](
                      caseClass,
                      directFieldSet,
                      nestedFieldMap,
                      faExprM,
                      fExprM,
                      gExprM
                    )
                  }

              // FoldLeft body (same as Foldable, with nested support)
              val doFoldLeft: (Expr[F[Any]], Expr[Any], Expr[(Any, Any) => Any]) => Expr[Any] =
                (faExpr, bExpr, fExpr) =>
                  runSafe {
                    deriveTraverseFoldLeftBody[F](caseClass, directFieldSet, nestedFieldMap, faExpr, bExpr, fExpr)
                  }

              // FoldRight body (same as Foldable, with nested support)
              val doFoldRight
                  : (Expr[F[Any]], Expr[cats.Eval[Any]], Expr[(Any, cats.Eval[Any]) => cats.Eval[Any]]) => Expr[
                    cats.Eval[Any]
                  ] =
                (faExpr, lbExpr, fExpr) =>
                  runSafe {
                    deriveTraverseFoldRightBody[F](
                      caseClass,
                      directFieldSet,
                      nestedFieldMap,
                      faExpr,
                      lbExpr,
                      fExpr
                    )
                  }

              import hearth.kindlings.catsderivation.internal.runtime.CatsDerivationFactories
              Expr.quote {
                CatsDerivationFactories.traverseInstance[F](
                  traverseFn = { (fa: F[CatsDerivationFactories.W1], fAny: Any, gAny: Any) =>
                    val anyFa: F[Any] = fa.asInstanceOf[F[Any]]
                    val f = fAny.asInstanceOf[Any => CatsDerivationFactories.AnyF[Any]]
                    val G = gAny.asInstanceOf[cats.Applicative[CatsDerivationFactories.AnyF]]
                    val _ = anyFa
                    val _ = f
                    val _ = G
                    val recon: (List[Any], Any) => Any = Expr.splice(reconstructFn)
                    val _ = recon
                    val gList: CatsDerivationFactories.AnyF[List[Any]] =
                      Expr
                        .splice(doTraverse(Expr.quote(anyFa), Expr.quote(f: Any), Expr.quote(G: Any)))
                        .asInstanceOf[CatsDerivationFactories.AnyF[List[Any]]]
                    G.map[List[Any], Any](gList)(newVals => recon(newVals, anyFa))
                  },
                  foldLeftFn = { (fa: F[CatsDerivationFactories.W1], anyB: Any, anyF: (Any, Any) => Any) =>
                    val anyFa: F[Any] = fa.asInstanceOf[F[Any]]
                    val _ = anyFa
                    val _ = anyB
                    val _ = anyF
                    Expr
                      .splice(doFoldLeft(Expr.quote(anyFa), Expr.quote(anyB), Expr.quote(anyF)))
                      .asInstanceOf[Any]
                  },
                  foldRightFn = {
                    (
                        fa: F[CatsDerivationFactories.W1],
                        anyLb: cats.Eval[Any],
                        anyF: (Any, cats.Eval[Any]) => cats.Eval[Any]
                    ) =>
                      val anyFa: F[Any] = fa.asInstanceOf[F[Any]]
                      val _ = anyFa
                      val _ = anyLb
                      val _ = anyF
                      Expr
                        .splice(doFoldRight(Expr.quote(anyFa), Expr.quote(anyLb), Expr.quote(anyF)))
                        .asInstanceOf[cats.Eval[Any]]
                  }
                )
              }
            }
          case Left(_) =>
            Enum.parse[F[Any]].toEither match {
              case Left(reason2) =>
                failDerivation(
                  CatsDerivationError.CannotParseEnum(
                    Type[F[Any]].prettyPrint,
                    s"not a case class and not a sealed trait ($reason2). $macroName cannot derive for this type."
                  )
                )
              case Right(enumm) =>
                MIO.scoped { runSafe =>
                  val _ = runSafe(Environment.loadStandardExtensions().toMIO(allowFailures = false))
                  runSafe(deriveTraverseForEnum[F](FCtor, enumm, runSafe))
                }
            }
        }
      }
      .flatTap(result => Log.info(s"Derived final result: ${result.prettyPrint}"))
      .runToExprOrFail(
        macroName,
        infoRendering = if (shouldWeLogTraverseDerivation) RenderFrom(Log.Level.Info) else DontRender,
        errorRendering = if (shouldWeLogTraverseDerivation) RenderFrom(Log.Level.Info) else DontRender,
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
  private def deriveReconstructFromList[F[_]](
      caseClass: CaseClass[F[Any]],
      traversableFields: Set[String]
  )(implicit
      FCtor: Type.Ctor1[F],
      FAnyType: Type[F[Any]],
      AnyType: Type[Any],
      ListAnyType: Type[List[Any]]
  ): MIO[Expr[(List[Any], Any) => Any]] =
    // LambdaBuilder callbacks are synchronous and cannot return MIO, so failures inside the
    // callback throw the typed CatsDerivationError; the enclosing MIO(...) converts the
    // NonFatal throw into a proper MIO error-channel failure.
    MIO {
      LambdaBuilder.of2[List[Any], Any]("newVals", "original").buildWith { case (newValsExpr, originalExpr) =>
        val faExpr: Expr[F[Any]] = Expr.quote(Expr.splice(originalExpr).asInstanceOf[F[Any]])
        val fields = caseClass.caseFieldValuesAt(faExpr).toList
        var currentList: Expr[List[Any]] = newValsExpr
        val fieldExprs: List[(String, Expr_??)] = fields.map { case (fieldName, fieldValue) =>
          import fieldValue.Underlying as Field
          if (traversableFields.contains(fieldName)) {
            val headExpr: Expr[Any] = Expr.quote(Expr.splice(currentList).head)
            val tailExpr: Expr[List[Any]] = Expr.quote(Expr.splice(currentList).tail)
            currentList = tailExpr
            val typedHead: Expr[Field] = Expr.quote(Expr.splice(headExpr).asInstanceOf[Field])
            (fieldName, typedHead.as_??)
          } else {
            (fieldName, fieldValue.value.asInstanceOf[Expr[Field]].as_??)
          }
        }
        foldInstanceFree(caseClass.primaryConstructor, "Constructor")(
          onTypes = _ => Map.empty,
          onValues = _ => fieldExprs.toMap
        ) match {
          case Right(constructExpr) => constructExpr.value.asInstanceOf[Expr[Any]]
          case Left(error)          =>
            throw CatsDerivationError.CannotConstructResult("traversed result", error)
        }
      }
    }

  @scala.annotation.nowarn("msg=is never used|unused implicit parameter")
  private def deriveTraverseBody[F[_]](
      caseClass: CaseClass[F[Any]],
      directFields: Set[String],
      nestedFieldTraverses: Map[String, Expr[Any]],
      faExpr: Expr[F[Any]],
      fExpr: Expr[Any],
      gExpr: Expr[Any]
  )(implicit FCtor: Type.Ctor1[F], FAnyType: Type[F[Any]], AnyType: Type[Any]): MIO[Expr[Any]] = {
    import hearth.kindlings.catsderivation.internal.runtime.CatsDerivationFactories

    val fields = caseClass.caseFieldValuesAt(faExpr).toList

    val gFieldExprs: List[Expr[CatsDerivationFactories.AnyF[Any]]] = fields.collect {
      case (fieldName, fieldValue) if directFields.contains(fieldName) =>
        import fieldValue.Underlying as Field
        val fieldE = fieldValue.value.asInstanceOf[Expr[Field]].upcast[Any]
        Expr.quote {
          Expr.splice(fExpr).asInstanceOf[Any => CatsDerivationFactories.AnyF[Any]](Expr.splice(fieldE))
        }
      case (fieldName, fieldValue) if nestedFieldTraverses.contains(fieldName) =>
        import fieldValue.Underlying as Field
        val fieldE = fieldValue.value.asInstanceOf[Expr[Field]].upcast[Any]
        val traverseE = nestedFieldTraverses(fieldName)
        Expr.quote {
          hearth.kindlings.catsderivation.internal.runtime.HktNestedRuntime
            .traverseNested(
              Expr.splice(traverseE),
              Expr.splice(fieldE),
              Expr.splice(fExpr),
              Expr.splice(gExpr)
            )
            .asInstanceOf[CatsDerivationFactories.AnyF[Any]]
        }
    }

    val gListExpr: Expr[CatsDerivationFactories.AnyF[List[Any]]] = gFieldExprs.foldRight(
      Expr.quote {
        Expr
          .splice(gExpr)
          .asInstanceOf[cats.Applicative[CatsDerivationFactories.AnyF]]
          .pure(Nil: List[Any])
      }
    ) { (gvExpr, accExpr) =>
      Expr.quote {
        Expr
          .splice(gExpr)
          .asInstanceOf[cats.Applicative[CatsDerivationFactories.AnyF]]
          .map2[Any, List[Any], List[Any]](Expr.splice(gvExpr), Expr.splice(accExpr)) { (a, acc) =>
            a :: acc
          }
      }
    }

    MIO.pure(gListExpr.asInstanceOf[Expr[Any]])
  }

  @scala.annotation.nowarn("msg=is never used|unused implicit parameter")
  private def deriveTraverseFoldLeftBody[F[_]](
      caseClass: CaseClass[F[Any]],
      directFields: Set[String],
      nestedFieldFoldables: Map[String, Expr[Any]],
      faExpr: Expr[F[Any]],
      bExpr: Expr[Any],
      fExpr: Expr[(Any, Any) => Any]
  )(implicit FCtor: Type.Ctor1[F], FAnyType: Type[F[Any]], AnyType: Type[Any]): MIO[Expr[Any]] = {
    val fields = caseClass.caseFieldValuesAt(faExpr).toList

    var result: Expr[Any] = bExpr
    fields.foreach { case (fieldName, fieldValue) =>
      import fieldValue.Underlying as Field
      val fieldExpr = fieldValue.value.asInstanceOf[Expr[Field]]
      if (directFields.contains(fieldName)) {
        result = Expr.quote(Expr.splice(fExpr)(Expr.splice(result), Expr.splice(fieldExpr.upcast[Any])))
      } else if (nestedFieldFoldables.contains(fieldName)) {
        val foldableExpr = nestedFieldFoldables(fieldName)
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

  @scala.annotation.nowarn("msg=is never used|unused implicit parameter")
  private def deriveTraverseFoldRightBody[F[_]](
      caseClass: CaseClass[F[Any]],
      directFields: Set[String],
      nestedFieldFoldables: Map[String, Expr[Any]],
      faExpr: Expr[F[Any]],
      lbExpr: Expr[cats.Eval[Any]],
      fExpr: Expr[(Any, cats.Eval[Any]) => cats.Eval[Any]]
  )(implicit
      FCtor: Type.Ctor1[F],
      FAnyType: Type[F[Any]],
      AnyType: Type[Any],
      EvalAnyType: Type[cats.Eval[Any]]
  ): MIO[Expr[cats.Eval[Any]]] = {
    val fields = caseClass.caseFieldValuesAt(faExpr).toList

    val foldableFields: List[(String, Expr[Any], Option[Expr[Any]])] = fields.collect {
      case (fieldName, fieldValue) if directFields.contains(fieldName) =>
        import fieldValue.Underlying as Field
        (fieldName, fieldValue.value.asInstanceOf[Expr[Field]].upcast[Any], None)
      case (fieldName, fieldValue) if nestedFieldFoldables.contains(fieldName) =>
        import fieldValue.Underlying as Field
        (fieldName, fieldValue.value.asInstanceOf[Expr[Field]].upcast[Any], Some(nestedFieldFoldables(fieldName)))
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

  protected def mkCtor1FromTypeTraverse(appliedType: Type[Any]): Option[UntypedType]

  @scala.annotation.nowarn(
    "msg=is never used|unused explicit parameter|unused local definition|unused implicit parameter"
  )
  private def deriveTraverseForEnum[F[_]](
      FCtor: Type.Ctor1[F],
      enumm: Enum[F[Any]],
      runSafe: hearth.fp.DirectStyle.RunSafe[hearth.fp.effect.MIO]
  ): MIO[Expr[cats.Traverse[F]]] = {
    implicit val AnyType: Type[Any] = TraverseTypes.Any
    implicit val IntType: Type[Int] = TraverseTypes.Int
    implicit val StringType: Type[String] = TraverseTypes.String
    implicit val FAnyType: Type[F[Any]] = FCtor.apply[Any]
    implicit val EvalAnyType: Type[cats.Eval[Any]] = TraverseTypes.EvalAny
    implicit val ListAnyType: Type[List[Any]] = TraverseTypes.ListAny
    implicit val FCtor1: Type.Ctor1[F] = FCtor

    val parentCtor = FCtor.asUntyped

    parseEnumMIO[Any]("F[Int]")(using FCtor.apply[Int].asInstanceOf[Type[Any]])
      .parTuple(parseEnumMIO[Any]("F[String]")(using FCtor.apply[String].asInstanceOf[Type[Any]]))
      .flatMap { case (enumInt, enumString) =>
        case class ChildFieldInfo(
            directFields: Set[String],
            nestedInstances: Map[String, Expr[Any]],
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
                  val direct = scala.collection.mutable.Set.empty[String]
                  val nested = scala.collection.mutable.Map.empty[String, Expr[Any]]
                  val selfRec = scala.collection.mutable.Set.empty[String]
                  fieldsI
                    .zip(fieldsS)
                    .traverse { case ((name, pI), (_, pS)) =>
                      val tI = pI.tpe.Underlying
                      val tS = pS.tpe.Underlying
                      if (tI =:= IntType && tS =:= StringType) {
                        direct += name
                        MIO.void
                      } else if (tI =:= tS) MIO.void
                      else {
                        val param = fieldsI.find(_._1 == name).get._2
                        import param.tpe.Underlying as FT
                        summonTraverseForFieldType(Type[FT].asInstanceOf[Type[Any]]) match {
                          case Some(expr) =>
                            nested += (name -> expr)
                            MIO.void
                          case None =>
                            mkCtor1FromTypeTraverse(Type[FT].asInstanceOf[Type[Any]]) match {
                              case Some(ut) if ut == parentCtor =>
                                selfRec += name
                                MIO.void
                              case _ =>
                                failDerivation[Unit](
                                  CatsDerivationError.MissingInstanceForField("Traverse", name, childName)
                                )
                            }
                        }
                      }
                    }
                    .map(_ => childName -> ChildFieldInfo(direct.toSet, nested.toMap, selfRec.toSet))
                case _ =>
                  MIO.pure(childName -> ChildFieldInfo(Set.empty, Map.empty, Set.empty))
              }
            }
            .map(_.toMap)
        }

        val hasSelfRecursive = childInfo.values.exists(_.selfRecursiveFields.nonEmpty)

        import hearth.kindlings.catsderivation.internal.runtime.CatsDerivationFactories

        def mkTraverseBody(
            faExpr: Expr[F[Any]],
            fExpr: Expr[Any],
            gExpr: Expr[Any],
            selfOpt: Option[Expr[Any]]
        ): Expr[Any] = runSafe {
          val selfOrFail: MIO[Expr[Any]] = selfOpt match {
            case Some(self) => MIO.pure(self)
            case None       => failDerivation(CatsDerivationError.MissingSelfReference("Traverse"))
          }
          enumm
            .matchOn[MIO, Any](faExpr) { matched =>
              import matched.{value as caseValue, Underlying as CT}
              val cn = Type[CT].shortName
              childInfo.get(cn) match {
                case None       => failDerivation(CatsDerivationError.MissingDerivationInfo("Traverse", cn))
                case Some(info) =>
                  parseCaseClassMIO[CT](s"child $cn").flatMap { cc =>
                    val fields = cc.caseFieldValuesAt(caseValue).toList
                    if (fields.isEmpty) {
                      val gE =
                        Expr.quote(Expr.splice(gExpr).asInstanceOf[cats.Applicative[CatsDerivationFactories.AnyF]])
                      MIO.pure(Expr.quote(Expr.splice(gE).pure(Expr.splice(caseValue.upcast[Any]))).upcast[Any])
                    } else {
                      val gFieldExprsMIO: MIO[List[Option[Expr[CatsDerivationFactories.AnyF[Any]]]]] =
                        fields.traverse { case (fn, fv) =>
                          import fv.Underlying as Field
                          if (info.directFields.contains(fn)) {
                            val fe = fv.value.asInstanceOf[Expr[Field]].upcast[Any]
                            MIO.pure(
                              Option(
                                Expr.quote(
                                  Expr
                                    .splice(fExpr)
                                    .asInstanceOf[Any => CatsDerivationFactories.AnyF[Any]](Expr.splice(fe))
                                )
                              )
                            )
                          } else if (info.selfRecursiveFields.contains(fn) || info.nestedInstances.contains(fn)) {
                            val fe = fv.value.asInstanceOf[Expr[Field]].upcast[Any]
                            val travEMIO: MIO[Expr[Any]] =
                              if (info.selfRecursiveFields.contains(fn)) selfOrFail
                              else MIO.pure(info.nestedInstances(fn))
                            travEMIO.map { travE =>
                              Option(
                                Expr.quote(
                                  hearth.kindlings.catsderivation.internal.runtime.HktNestedRuntime
                                    .traverseNested(
                                      Expr.splice(travE),
                                      Expr.splice(fe),
                                      Expr.splice(fExpr),
                                      Expr.splice(gExpr)
                                    )
                                    .asInstanceOf[CatsDerivationFactories.AnyF[Any]]
                                )
                              )
                            }
                          } else MIO.pure(Option.empty[Expr[CatsDerivationFactories.AnyF[Any]]])
                        }
                      gFieldExprsMIO.flatMap { gFieldOpts =>
                        val gFieldExprs = gFieldOpts.flatten
                        val traversableFields =
                          info.directFields ++ info.nestedInstances.keySet ++ info.selfRecursiveFields

                        // LambdaBuilder callbacks are synchronous and cannot return MIO, so failures
                        // inside the callback throw the typed CatsDerivationError; the enclosing
                        // MIO(...) converts the NonFatal throw into an MIO error-channel failure.
                        val reconLambda: Expr[List[Any] => Any] = runSafe {
                          MIO {
                            LambdaBuilder.of1[List[Any]]("newVals").buildWith { newValsExpr =>
                              var currentList: Expr[List[Any]] = newValsExpr
                              val fieldExprs: List[(String, Expr_??)] = fields.map { case (fn2, fv2) =>
                                import fv2.Underlying as Field2
                                if (traversableFields.contains(fn2)) {
                                  val headExpr: Expr[Any] = Expr.quote(Expr.splice(currentList).head)
                                  val tailExpr: Expr[List[Any]] = Expr.quote(Expr.splice(currentList).tail)
                                  currentList = tailExpr
                                  val typed: Expr[Field2] = Expr.quote(Expr.splice(headExpr).asInstanceOf[Field2])
                                  (fn2, typed.as_??)
                                } else {
                                  (fn2, fv2.value.asInstanceOf[Expr[Field2]].as_??)
                                }
                              }
                              foldInstanceFree(cc.primaryConstructor, "Constructor")(
                                onTypes = _ => Map.empty,
                                onValues = _ => fieldExprs.toMap
                              ) match {
                                case Right(expr) => expr.value.asInstanceOf[Expr[Any]]
                                case Left(err)   => throw CatsDerivationError.CannotConstructResult(cn, err)
                              }
                            }
                          }
                        }

                        val gListExpr = gFieldExprs.foldRight(
                          Expr.quote(
                            Expr
                              .splice(gExpr)
                              .asInstanceOf[cats.Applicative[CatsDerivationFactories.AnyF]]
                              .pure(Nil: List[Any])
                          )
                        ) { (gvExpr, accExpr) =>
                          Expr.quote(
                            Expr
                              .splice(gExpr)
                              .asInstanceOf[cats.Applicative[CatsDerivationFactories.AnyF]]
                              .map2[Any, List[Any], List[Any]](Expr.splice(gvExpr), Expr.splice(accExpr))(_ :: _)
                          )
                        }
                        val result = Expr.quote {
                          val recon: List[Any] => Any = Expr.splice(reconLambda)
                          val _ = recon
                          Expr
                            .splice(gExpr)
                            .asInstanceOf[cats.Applicative[CatsDerivationFactories.AnyF]]
                            .map[List[Any], Any](Expr.splice(gListExpr))(newVals => recon(newVals))
                        }
                        MIO.pure(result.upcast[Any])
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

        def mkFoldLeftBody(
            faExpr: Expr[F[Any]],
            bExpr: Expr[Any],
            fExpr: Expr[(Any, Any) => Any],
            selfOpt: Option[Expr[Any]]
        ): Expr[Any] = runSafe {
          val selfOrFail: MIO[Expr[Any]] = selfOpt match {
            case Some(self) => MIO.pure(self)
            case None       => failDerivation(CatsDerivationError.MissingSelfReference("Traverse"))
          }
          enumm
            .matchOn[MIO, Any](faExpr) { matched =>
              import matched.{value as caseValue, Underlying as CT}
              val cn = Type[CT].shortName
              childInfo.get(cn) match {
                case None       => failDerivation(CatsDerivationError.MissingDerivationInfo("Traverse", cn))
                case Some(info) =>
                  parseCaseClassMIO[CT](s"child $cn").flatMap { cc =>
                    val fields = cc.caseFieldValuesAt(caseValue).toList
                    var result: Expr[Any] = bExpr
                    fields
                      .traverse { case (fn, fv) =>
                        import fv.Underlying as Field
                        val fe = fv.value.asInstanceOf[Expr[Field]]
                        if (info.directFields.contains(fn)) {
                          result = Expr.quote(Expr.splice(fExpr)(Expr.splice(result), Expr.splice(fe.upcast[Any])))
                          MIO.void
                        } else if (info.selfRecursiveFields.contains(fn) || info.nestedInstances.contains(fn)) {
                          val foldEMIO: MIO[Expr[Any]] =
                            if (info.selfRecursiveFields.contains(fn)) selfOrFail
                            else MIO.pure(info.nestedInstances(fn))
                          foldEMIO.map { foldE =>
                            result = Expr.quote(
                              hearth.kindlings.catsderivation.internal.runtime.HktNestedRuntime
                                .foldLeftNested(
                                  Expr.splice(foldE),
                                  Expr.splice(fe.upcast[Any]),
                                  Expr.splice(result),
                                  Expr.splice(fExpr)
                                )
                            )
                          }
                        } else MIO.void
                      }
                      .map(_ => result)
                  }
              }
            }
            .flatMap {
              case Some(r) => MIO.pure(r)
              case None    => failDerivation(CatsDerivationError.EnumHasNoChildren(Type[F[Any]].prettyPrint))
            }
        }

        def mkFoldRightBody(
            faExpr: Expr[F[Any]],
            lbExpr: Expr[cats.Eval[Any]],
            fExpr: Expr[(Any, cats.Eval[Any]) => cats.Eval[Any]],
            selfOpt: Option[Expr[Any]]
        ): Expr[cats.Eval[Any]] = runSafe {
          val selfOrFail: MIO[Expr[Any]] = selfOpt match {
            case Some(self) => MIO.pure(self)
            case None       => failDerivation(CatsDerivationError.MissingSelfReference("Traverse"))
          }
          enumm
            .matchOn[MIO, cats.Eval[Any]](faExpr) { matched =>
              import matched.{value as caseValue, Underlying as CT}
              val cn = Type[CT].shortName
              childInfo.get(cn) match {
                case None       => failDerivation(CatsDerivationError.MissingDerivationInfo("Traverse", cn))
                case Some(info) =>
                  parseCaseClassMIO[CT](s"child $cn").flatMap { cc =>
                    val fields = cc.caseFieldValuesAt(caseValue).toList
                    val foldableMIO: MIO[List[Option[(Expr[Any], Option[Expr[Any]])]]] =
                      fields.traverse { case (fn, fv) =>
                        import fv.Underlying as Field
                        if (info.directFields.contains(fn))
                          MIO.pure(
                            Option((fv.value.asInstanceOf[Expr[Field]].upcast[Any], Option.empty[Expr[Any]]))
                          )
                        else if (info.selfRecursiveFields.contains(fn) || info.nestedInstances.contains(fn)) {
                          val feMIO: MIO[Expr[Any]] =
                            if (info.selfRecursiveFields.contains(fn)) selfOrFail
                            else MIO.pure(info.nestedInstances(fn))
                          feMIO.map { fe =>
                            Option((fv.value.asInstanceOf[Expr[Field]].upcast[Any], Option(fe)))
                          }
                        } else MIO.pure(Option.empty[(Expr[Any], Option[Expr[Any]])])
                      }
                    foldableMIO.map { foldableOpts =>
                      foldableOpts.flatten.foldRight(lbExpr) { case ((fieldExpr, foldableOpt), acc) =>
                        foldableOpt match {
                          case None =>
                            Expr.quote(Expr.splice(fExpr)(Expr.splice(fieldExpr), Expr.splice(acc)))
                          case Some(foldableE) =>
                            Expr.quote(
                              hearth.kindlings.catsderivation.internal.runtime.HktNestedRuntime
                                .foldRightNested(
                                  Expr.splice(foldableE),
                                  Expr.splice(fieldExpr),
                                  Expr.splice(acc),
                                  Expr.splice(fExpr)
                                )
                            )
                        }
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
              self$macro = CatsDerivationFactories.traverseInstance[F](
                traverseFn = { (fa: F[CatsDerivationFactories.W1], fAny: Any, gAny: Any) =>
                  val anyFa: F[Any] = fa.asInstanceOf[F[Any]]
                  val _ = anyFa; val _ = fAny; val _ = gAny
                  Expr.splice(
                    mkTraverseBody(Expr.quote(anyFa), Expr.quote(fAny), Expr.quote(gAny), Some(Expr.quote(self$macro)))
                  )
                },
                foldLeftFn = { (fa: F[CatsDerivationFactories.W1], anyB: Any, anyF: (Any, Any) => Any) =>
                  val anyFa: F[Any] = fa.asInstanceOf[F[Any]]
                  val _ = anyFa; val _ = anyB; val _ = anyF
                  Expr.splice(
                    mkFoldLeftBody(Expr.quote(anyFa), Expr.quote(anyB), Expr.quote(anyF), Some(Expr.quote(self$macro)))
                  )
                },
                foldRightFn = {
                  (
                      fa: F[CatsDerivationFactories.W1],
                      anyLb: cats.Eval[Any],
                      anyF: (Any, cats.Eval[Any]) => cats.Eval[Any]
                  ) =>
                    val anyFa: F[Any] = fa.asInstanceOf[F[Any]]
                    val _ = anyFa; val _ = anyLb; val _ = anyF
                    Expr.splice(
                      mkFoldRightBody(
                        Expr.quote(anyFa),
                        Expr.quote(anyLb),
                        Expr.quote(anyF),
                        Some(Expr.quote(self$macro))
                      )
                    )
                }
              )
              self$macro.asInstanceOf[cats.Traverse[F]]
            }
          }
        } else {
          MIO.pure {
            Expr.quote {
              CatsDerivationFactories.traverseInstance[F](
                traverseFn = { (fa: F[CatsDerivationFactories.W1], fAny: Any, gAny: Any) =>
                  val anyFa: F[Any] = fa.asInstanceOf[F[Any]]
                  val _ = anyFa; val _ = fAny; val _ = gAny
                  Expr.splice(mkTraverseBody(Expr.quote(anyFa), Expr.quote(fAny), Expr.quote(gAny), None))
                },
                foldLeftFn = { (fa: F[CatsDerivationFactories.W1], anyB: Any, anyF: (Any, Any) => Any) =>
                  val anyFa: F[Any] = fa.asInstanceOf[F[Any]]
                  val _ = anyFa; val _ = anyB; val _ = anyF
                  Expr.splice(mkFoldLeftBody(Expr.quote(anyFa), Expr.quote(anyB), Expr.quote(anyF), None))
                },
                foldRightFn = {
                  (
                      fa: F[CatsDerivationFactories.W1],
                      anyLb: cats.Eval[Any],
                      anyF: (Any, cats.Eval[Any]) => cats.Eval[Any]
                  ) =>
                    val anyFa: F[Any] = fa.asInstanceOf[F[Any]]
                    val _ = anyFa; val _ = anyLb; val _ = anyF
                    Expr.splice(mkFoldRightBody(Expr.quote(anyFa), Expr.quote(anyLb), Expr.quote(anyF), None))
                }
              )
            }
          }
        }
      }
  }

  protected object TraverseTypes {
    val Any: Type[Any] = Type.of[Any]
    val Int: Type[Int] = Type.of[Int]
    val String: Type[String] = Type.of[String]
    def EvalAny: Type[cats.Eval[Any]] = Type.of[cats.Eval[Any]]
    def ListAny: Type[List[Any]] = Type.of[List[Any]]
    val LogDerivation: Type[hearth.kindlings.catsderivation.LogDerivation] =
      Type.of[hearth.kindlings.catsderivation.LogDerivation]
  }

  def shouldWeLogTraverseDerivation: Boolean = {
    implicit val LogDerivationType: Type[LogDerivation] = TraverseTypes.LogDerivation
    Expr.summonImplicit[LogDerivation].isDefined || (for {
      data <- Environment.typedSettings.toOption
      cd <- data.get("catsDerivation")
      shouldLog <- cd.get("logDerivation").flatMap(_.asBoolean)
    } yield shouldLog).getOrElse(false)
  }
}
