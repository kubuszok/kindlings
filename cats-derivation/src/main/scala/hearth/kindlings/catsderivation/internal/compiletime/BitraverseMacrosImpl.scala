package hearth.kindlings.catsderivation.internal.compiletime

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

import hearth.kindlings.catsderivation.LogDerivation

trait BitraverseMacrosImpl extends CatsDerivationTimeout { this: MacroCommons & StdExtensions =>

  @scala.annotation.nowarn("msg=is never used|unused explicit parameter")
  def deriveBitraverse[F[_, _]](
      FCtor0: Type.Ctor2[F],
      BitraverseFType: Type[cats.Bitraverse[F]]
  ): Expr[cats.Bitraverse[F]] = {
    val macroName = "Bitraverse.derived"

    implicit val FCtor: Type.Ctor2[F] = FCtor0
    implicit val BitraverseFT: Type[cats.Bitraverse[F]] = BitraverseFType
    implicit val AnyType: Type[Any] = BitraverseTypes.Any
    implicit val FAnyAnyType: Type[F[Any, Any]] = FCtor.apply[Any, Any]
    implicit val ListAnyType: Type[List[Any]] = BitraverseTypes.ListAny

    Log
      .namedScope(s"Deriving Bitraverse at: ${Environment.currentPosition.prettyPrint}") {
        CaseClass.parse[F[Any, Any]].toEither match {
          case Right(caseClass) =>
            MIO.scoped { runSafe =>
              implicit val IntType: Type[Int] = BitraverseTypes.Int
              implicit val StringType: Type[String] = BitraverseTypes.String

              val ccIntInt = CaseClass.parse(using FCtor.apply[Int, Int]).toEither match {
                case Right(cc) => cc
                case Left(e)   => throw new RuntimeException(s"Cannot parse F[Int, Int]: $e")
              }
              val ccStringInt = CaseClass.parse(using FCtor.apply[String, Int]).toEither match {
                case Right(cc) => cc
                case Left(e)   => throw new RuntimeException(s"Cannot parse F[String, Int]: $e")
              }
              val ccIntString = CaseClass.parse(using FCtor.apply[Int, String]).toEither match {
                case Right(cc) => cc
                case Left(e)   => throw new RuntimeException(s"Cannot parse F[Int, String]: $e")
              }

              val fieldsP1 = ccIntInt.primaryConstructor.totalParameters.flatten.toList
              val fieldsP2 = ccStringInt.primaryConstructor.totalParameters.flatten.toList
              val fieldsP3 = ccIntString.primaryConstructor.totalParameters.flatten.toList

              val leftFieldSet = scala.collection.mutable.Set.empty[String]
              val rightFieldSet = scala.collection.mutable.Set.empty[String]
              val covariantFieldOrder = scala.collection.mutable.ListBuffer.empty[(String, Boolean)]

              fieldsP1.zip(fieldsP2).zip(fieldsP3).foreach { case (((name, p1), (_, p2)), (_, p3)) =>
                val t1 = p1.tpe.Underlying
                val t2 = p2.tpe.Underlying
                val t3 = p3.tpe.Underlying
                val firstChanges = !(t1 =:= t2)
                val secondChanges = !(t1 =:= t3)
                if (firstChanges && !secondChanges) {
                  leftFieldSet += name
                  covariantFieldOrder += ((name, true))
                } else if (!firstChanges && secondChanges) {
                  rightFieldSet += name
                  covariantFieldOrder += ((name, false))
                } else if (firstChanges && secondChanges) {
                  throw new RuntimeException(
                    s"Cannot derive Bitraverse: field $name depends on both type parameters."
                  )
                }
              }

              val allCovariantFields: Set[String] = leftFieldSet.toSet ++ rightFieldSet.toSet
              val isLeftFlagsList: List[Boolean] = covariantFieldOrder.map(_._2).toList

              runSafe {
                Environment.loadStandardExtensions().toMIO(allowFailures = false).map(_ => ())
              }

              val extractDirect: Expr[Any => List[Any]] = runSafe {
                deriveBitraverseExtract(caseClass, allCovariantFields)
              }

              val reconstructFn: Expr[(List[Any], Any) => Any] = runSafe {
                deriveBitraverseReconstruct(caseClass, allCovariantFields)
              }

              implicit val BooleanType: Type[Boolean] = BitraverseTypes.Boolean
              implicit val ListBoolType: Type[List[Boolean]] = BitraverseTypes.ListBoolean
              implicit val EvalAnyType: Type[cats.Eval[Any]] = BitraverseTypes.EvalAny
              val flagsExpr: Expr[List[Boolean]] =
                isLeftFlagsList.foldRight(Expr.quote(Nil: List[Boolean])) { (flag, acc) =>
                  if (flag) Expr.quote(true :: Expr.splice(acc))
                  else Expr.quote(false :: Expr.splice(acc))
                }

              val doBimap: (Expr[F[Any, Any]], Expr[Any => Any], Expr[Any => Any]) => Expr[F[Any, Any]] =
                (fabExpr, fExpr, gExpr) =>
                  runSafe {
                    val fields = caseClass.caseFieldValuesAt(fabExpr).toList
                    val mappedFields: List[(String, Expr_??)] = fields.map { case (fieldName, fieldValue) =>
                      import fieldValue.Underlying as Field
                      if (leftFieldSet.contains(fieldName)) {
                        val fieldExpr = fieldValue.value.asInstanceOf[Expr[Field]].upcast[Any]
                        (fieldName, Expr.quote(Expr.splice(fExpr)(Expr.splice(fieldExpr))).as_??)
                      } else if (rightFieldSet.contains(fieldName)) {
                        val fieldExpr = fieldValue.value.asInstanceOf[Expr[Field]].upcast[Any]
                        (fieldName, Expr.quote(Expr.splice(gExpr)(Expr.splice(fieldExpr))).as_??)
                      } else {
                        (fieldName, fieldValue.value.asInstanceOf[Expr[Field]].as_??)
                      }
                    }
                    caseClass.primaryConstructor.fold(
                      onInstance = _ => throw new RuntimeException("Constructor should not need instance"),
                      onTypes = _ => Map.empty,
                      onValues = _ => mappedFields.toMap
                    ) match {
                      case Right(expr) => MIO.pure(expr.value.asInstanceOf[Expr[F[Any, Any]]])
                      case Left(e)     => MIO.fail(new RuntimeException(s"Cannot construct bimap result: $e"))
                    }
                  }

              val doBifoldLeft
                  : (Expr[F[Any, Any]], Expr[Any], Expr[(Any, Any) => Any], Expr[(Any, Any) => Any]) => Expr[Any] =
                (fabExpr, cExpr, fExpr, gExpr) =>
                  runSafe {
                    val fields = caseClass.caseFieldValuesAt(fabExpr).toList
                    var acc = cExpr
                    fields.foreach { case (fieldName, fieldValue) =>
                      import fieldValue.Underlying as Field
                      val fieldExpr = fieldValue.value.asInstanceOf[Expr[Field]].upcast[Any]
                      if (leftFieldSet.contains(fieldName))
                        acc = Expr.quote(Expr.splice(fExpr)(Expr.splice(acc), Expr.splice(fieldExpr)))
                      else if (rightFieldSet.contains(fieldName))
                        acc = Expr.quote(Expr.splice(gExpr)(Expr.splice(acc), Expr.splice(fieldExpr)))
                    }
                    MIO.pure(acc)
                  }

              val doBifoldRight: (
                  Expr[F[Any, Any]],
                  Expr[cats.Eval[Any]],
                  Expr[(Any, cats.Eval[Any]) => cats.Eval[Any]],
                  Expr[(Any, cats.Eval[Any]) => cats.Eval[Any]]
              ) => Expr[cats.Eval[Any]] =
                (fabExpr, lcExpr, fExpr, gExpr) =>
                  runSafe {
                    val fields = caseClass.caseFieldValuesAt(fabExpr).toList
                    val directFieldExprs: List[(String, Expr[Any])] = fields.collect {
                      case (fieldName, fieldValue)
                          if leftFieldSet.contains(fieldName) || rightFieldSet.contains(fieldName) =>
                        import fieldValue.Underlying as Field
                        (fieldName, fieldValue.value.asInstanceOf[Expr[Field]].upcast[Any])
                    }
                    val result = directFieldExprs.foldRight(lcExpr) { case ((fieldName, fieldExpr), acc) =>
                      if (leftFieldSet.contains(fieldName))
                        Expr.quote(Expr.splice(fExpr)(Expr.splice(fieldExpr), Expr.splice(acc)))
                      else
                        Expr.quote(Expr.splice(gExpr)(Expr.splice(fieldExpr), Expr.splice(acc)))
                    }
                    MIO.pure(result)
                  }

              import hearth.kindlings.catsderivation.internal.runtime.CatsDerivationFactories
              Expr.quote {
                CatsDerivationFactories.bitraverseInstance[F](
                  bitraverseFn = {
                    (
                        fab: F[CatsDerivationFactories.W1, CatsDerivationFactories.W2],
                        fAny: Any,
                        gAny: Any,
                        gApplicative: Any
                    ) =>
                      val anyFab: F[Any, Any] = fab.asInstanceOf[F[Any, Any]]
                      val f = fAny.asInstanceOf[Any => CatsDerivationFactories.AnyF[Any]]
                      val g = gAny.asInstanceOf[Any => CatsDerivationFactories.AnyF[Any]]
                      val G = gApplicative.asInstanceOf[cats.Applicative[CatsDerivationFactories.AnyF]]
                      val _ = anyFab
                      val _ = f
                      val _ = g
                      val _ = G
                      val extract: Any => List[Any] = Expr.splice(extractDirect)
                      val _ = extract
                      val recon: (List[Any], Any) => Any = Expr.splice(reconstructFn)
                      val _ = recon
                      val flags: List[Boolean] = Expr.splice(flagsExpr)
                      val covariantValues: List[Any] = extract(anyFab)
                      val gList: CatsDerivationFactories.AnyF[List[Any]] =
                        covariantValues.zip(flags).foldRight(G.pure(Nil: List[Any])) { case ((v, isLeft), gacc) =>
                          val gv: CatsDerivationFactories.AnyF[Any] = if (isLeft) f(v) else g(v)
                          G.map2[Any, List[Any], List[Any]](gv, gacc) { (a, acc) =>
                            a :: acc
                          }
                        }
                      G.map[List[Any], Any](gList)(newVals => recon(newVals, anyFab))
                  },
                  bimapFn = {
                    (
                        fab: F[CatsDerivationFactories.W1, CatsDerivationFactories.W2],
                        f: CatsDerivationFactories.W1 => CatsDerivationFactories.W3,
                        g: CatsDerivationFactories.W2 => CatsDerivationFactories.W4
                    ) =>
                      val anyFab: F[Any, Any] = fab.asInstanceOf[F[Any, Any]]
                      val fAny: Any => Any = f.asInstanceOf[Any => Any]
                      val gAny: Any => Any = g.asInstanceOf[Any => Any]
                      val _ = anyFab
                      val _ = fAny
                      val _ = gAny
                      Expr
                        .splice(doBimap(Expr.quote(anyFab), Expr.quote(fAny), Expr.quote(gAny)))
                        .asInstanceOf[F[CatsDerivationFactories.W3, CatsDerivationFactories.W4]]
                  },
                  bifoldLeftFn = {
                    (
                        fab: F[CatsDerivationFactories.W1, CatsDerivationFactories.W2],
                        cAny: Any,
                        fAny: (Any, Any) => Any,
                        gAny: (Any, Any) => Any
                    ) =>
                      val anyFab: F[Any, Any] = fab.asInstanceOf[F[Any, Any]]
                      val _ = anyFab
                      val _ = cAny
                      val _ = fAny
                      val _ = gAny
                      Expr
                        .splice(doBifoldLeft(Expr.quote(anyFab), Expr.quote(cAny), Expr.quote(fAny), Expr.quote(gAny)))
                        .asInstanceOf[Any]
                  },
                  bifoldRightFn = {
                    (
                        fab: F[CatsDerivationFactories.W1, CatsDerivationFactories.W2],
                        lcAny: cats.Eval[Any],
                        fAny: (Any, cats.Eval[Any]) => cats.Eval[Any],
                        gAny: (Any, cats.Eval[Any]) => cats.Eval[Any]
                    ) =>
                      val anyFab: F[Any, Any] = fab.asInstanceOf[F[Any, Any]]
                      val _ = anyFab
                      val _ = lcAny
                      val _ = fAny
                      val _ = gAny
                      Expr
                        .splice(
                          doBifoldRight(Expr.quote(anyFab), Expr.quote(lcAny), Expr.quote(fAny), Expr.quote(gAny))
                        )
                        .asInstanceOf[cats.Eval[Any]]
                  }
                )
              }
            }
          case Left(_) =>
            Enum.parse[F[Any, Any]].toEither match {
              case Left(reason2) =>
                MIO.fail(
                  new RuntimeException(
                    s"$macroName: Cannot derive: not a case class and not a sealed trait ($reason2)."
                  )
                )
              case Right(enumm) =>
                MIO.scoped { runSafe =>
                  val _ = runSafe(Environment.loadStandardExtensions().toMIO(allowFailures = false))
                  runSafe(deriveBitraverseForEnum[F](FCtor, enumm, runSafe))
                }
            }
        }
      }
      .flatTap(result => Log.info(s"Derived final result: ${result.prettyPrint}"))
      .runToExprOrFail(
        macroName,
        infoRendering = if (shouldWeLogBitraverseDerivation) RenderFrom(Log.Level.Info) else DontRender,
        errorRendering = if (shouldWeLogBitraverseDerivation) RenderFrom(Log.Level.Info) else DontRender,
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
  private def deriveBitraverseExtract[F[_, _]](
      caseClass: CaseClass[F[Any, Any]],
      covariantFields: Set[String]
  )(implicit
      FCtor: Type.Ctor2[F],
      FAnyAnyType: Type[F[Any, Any]],
      AnyType: Type[Any],
      ListAnyType: Type[List[Any]]
  ): MIO[Expr[Any => List[Any]]] = {
    val lambda = LambdaBuilder.of1[Any]("fab").buildWith { fabExpr0 =>
      val fabExpr: Expr[F[Any, Any]] = Expr.quote(Expr.splice(fabExpr0).asInstanceOf[F[Any, Any]])
      val fields = caseClass.caseFieldValuesAt(fabExpr).toList
      val directExprs: List[Expr[Any]] = fields.collect {
        case (fieldName, fieldValue) if covariantFields.contains(fieldName) =>
          import fieldValue.Underlying as Field
          fieldValue.value.asInstanceOf[Expr[Field]].upcast[Any]
      }
      directExprs.foldRight(Expr.quote(Nil: List[Any])) { (elem, acc) =>
        Expr.quote(Expr.splice(elem) :: Expr.splice(acc))
      }
    }
    MIO.pure(lambda)
  }

  @scala.annotation.nowarn("msg=is never used|unused implicit parameter")
  private def deriveBitraverseReconstruct[F[_, _]](
      caseClass: CaseClass[F[Any, Any]],
      covariantFields: Set[String]
  )(implicit
      FCtor: Type.Ctor2[F],
      FAnyAnyType: Type[F[Any, Any]],
      AnyType: Type[Any],
      ListAnyType: Type[List[Any]]
  ): MIO[Expr[(List[Any], Any) => Any]] = {
    val lambda =
      LambdaBuilder.of2[List[Any], Any]("newVals", "original").buildWith { case (newValsExpr, originalExpr) =>
        val fabExpr: Expr[F[Any, Any]] = Expr.quote(Expr.splice(originalExpr).asInstanceOf[F[Any, Any]])
        val fields = caseClass.caseFieldValuesAt(fabExpr).toList
        var currentList: Expr[List[Any]] = newValsExpr
        val fieldExprs: List[(String, Expr_??)] = fields.map { case (fieldName, fieldValue) =>
          import fieldValue.Underlying as Field
          if (covariantFields.contains(fieldName)) {
            val headExpr: Expr[Any] = Expr.quote(Expr.splice(currentList).head)
            val tailExpr: Expr[List[Any]] = Expr.quote(Expr.splice(currentList).tail)
            currentList = tailExpr
            (fieldName, headExpr.as_??)
          } else {
            (fieldName, fieldValue.value.asInstanceOf[Expr[Field]].as_??)
          }
        }
        caseClass.primaryConstructor.fold(
          onInstance = _ => throw new RuntimeException("Constructor should not need instance"),
          onTypes = _ => Map.empty,
          onValues = _ => fieldExprs.toMap
        ) match {
          case Right(constructExpr) => constructExpr.value.asInstanceOf[Expr[Any]]
          case Left(error)          =>
            throw new RuntimeException(s"Cannot construct bitraversed result: $error")
        }
      }
    MIO.pure(lambda)
  }

  @scala.annotation.nowarn(
    "msg=is never used|unused explicit parameter|unused local definition|unused implicit parameter"
  )
  private def deriveBitraverseForEnum[F[_, _]](
      FCtor: Type.Ctor2[F],
      enumm: Enum[F[Any, Any]],
      runSafe: hearth.fp.DirectStyle.RunSafe[hearth.fp.effect.MIO]
  ): MIO[Expr[cats.Bitraverse[F]]] = {
    implicit val AnyType: Type[Any] = BitraverseTypes.Any
    implicit val IntType: Type[Int] = BitraverseTypes.Int
    implicit val StringType: Type[String] = BitraverseTypes.String
    implicit val FAAType: Type[F[Any, Any]] = FCtor.apply[Any, Any]
    implicit val EvalAnyType: Type[cats.Eval[Any]] = BitraverseTypes.EvalAny
    implicit val ListAnyType: Type[List[Any]] = BitraverseTypes.ListAny
    implicit val FCtor1: Type.Ctor2[F] = FCtor

    val enumP1 = Enum.parse(using FCtor.apply[Int, Int].asInstanceOf[Type[Any]]).toEither match {
      case Right(e) => e; case Left(_) => return MIO.fail(new RuntimeException("Cannot parse F[Int,Int]"))
    }
    val enumP2 = Enum.parse(using FCtor.apply[String, Int].asInstanceOf[Type[Any]]).toEither match {
      case Right(e) => e; case Left(_) => return MIO.fail(new RuntimeException("Cannot parse F[String,Int]"))
    }
    val enumP3 = Enum.parse(using FCtor.apply[Int, String].asInstanceOf[Type[Any]]).toEither match {
      case Right(e) => e; case Left(_) => return MIO.fail(new RuntimeException("Cannot parse F[Int,String]"))
    }

    case class BiChildInfo(leftFields: Set[String], rightFields: Set[String])
    val childInfo: Map[String, BiChildInfo] = runSafe {
      val result = scala.collection.mutable.Map.empty[String, BiChildInfo]
      val c1 = enumP1.directChildren.toList
      val c2 = enumP2.directChildren.toList
      val c3 = enumP3.directChildren.toList
      c1.zip(c2).zip(c3).foreach { case (((cn, ch1), (_, ch2)), (_, ch3)) =>
        import ch1.Underlying as C1; import ch2.Underlying as C2; import ch3.Underlying as C3
        (CaseClass.parse[C1].toEither, CaseClass.parse[C2].toEither, CaseClass.parse[C3].toEither) match {
          case (Right(cc1), Right(cc2), Right(cc3)) =>
            val f1 = cc1.primaryConstructor.totalParameters.flatten.toList
            val f2 = cc2.primaryConstructor.totalParameters.flatten.toList
            val f3 = cc3.primaryConstructor.totalParameters.flatten.toList
            val left = scala.collection.mutable.Set.empty[String]
            val right = scala.collection.mutable.Set.empty[String]
            f1.zip(f2).zip(f3).foreach { case (((n, p1), (_, p2)), (_, p3)) =>
              val t1 = p1.tpe.Underlying; val t2 = p2.tpe.Underlying; val t3 = p3.tpe.Underlying
              if (!(t1 =:= t2) && (t1 =:= t3)) left += n
              else if ((t1 =:= t2) && !(t1 =:= t3)) right += n
            }
            result += (cn -> BiChildInfo(left.toSet, right.toSet))
          case _ => result += (cn -> BiChildInfo(Set.empty, Set.empty))
        }
      }
      MIO.pure(result.toMap)
    }

    import hearth.kindlings.catsderivation.internal.runtime.CatsDerivationFactories

    def mkBimapBody(fabExpr: Expr[F[Any, Any]], fExpr: Expr[Any => Any], gExpr: Expr[Any => Any]): Expr[F[Any, Any]] =
      runSafe {
        enumm
          .matchOn[MIO, F[Any, Any]](fabExpr) { matched =>
            import matched.{value as cv, Underlying as CT}
            val cn = Type[CT].shortName
            childInfo.get(cn) match {
              case None       => MIO.fail(new RuntimeException(s"No info for $cn"))
              case Some(info) =>
                CaseClass.parse[CT].toEither match {
                  case Left(_)   => MIO.pure(cv.upcast[F[Any, Any]])
                  case Right(cc) =>
                    val fields = cc.caseFieldValuesAt(cv).toList
                    val mapped: List[(String, Expr_??)] = fields.map { case (fn, fv) =>
                      import fv.Underlying as Field
                      val fe = fv.value.asInstanceOf[Expr[Field]]
                      if (info.leftFields.contains(fn))
                        (fn, Expr.quote(Expr.splice(fExpr)(Expr.splice(fe.upcast[Any])).asInstanceOf[Field]).as_??)
                      else if (info.rightFields.contains(fn))
                        (fn, Expr.quote(Expr.splice(gExpr)(Expr.splice(fe.upcast[Any])).asInstanceOf[Field]).as_??)
                      else (fn, fe.as_??)
                    }
                    cc.primaryConstructor.fold(
                      onInstance = _ => throw new RuntimeException("Constructor should not need instance"),
                      onTypes = _ => Map.empty,
                      onValues = _ => mapped.toMap
                    ) match {
                      case Right(e) => MIO.pure(e.value.asInstanceOf[Expr[F[Any, Any]]])
                      case Left(e)  => MIO.fail(new RuntimeException(s"Cannot construct $cn: $e"))
                    }
                }
            }
          }
          .flatMap { case Some(r) => MIO.pure(r); case None => MIO.fail(new RuntimeException("No children")) }
      }

    def mkBifoldLeftBody(
        fabExpr: Expr[F[Any, Any]],
        cExpr: Expr[Any],
        fExpr: Expr[(Any, Any) => Any],
        gExpr: Expr[(Any, Any) => Any]
    ): Expr[Any] = runSafe {
      enumm
        .matchOn[MIO, Any](fabExpr) { matched =>
          import matched.{value as cv, Underlying as CT}
          val cn = Type[CT].shortName
          childInfo.get(cn) match {
            case None       => MIO.fail(new RuntimeException(s"No info for $cn"))
            case Some(info) =>
              CaseClass.parse[CT].toEither match {
                case Left(_)   => MIO.pure(cExpr)
                case Right(cc) =>
                  val fields = cc.caseFieldValuesAt(cv).toList
                  var acc = cExpr
                  fields.foreach { case (fn, fv) =>
                    import fv.Underlying as Field
                    val fe = fv.value.asInstanceOf[Expr[Field]].upcast[Any]
                    if (info.leftFields.contains(fn))
                      acc = Expr.quote(Expr.splice(fExpr)(Expr.splice(acc), Expr.splice(fe)))
                    else if (info.rightFields.contains(fn))
                      acc = Expr.quote(Expr.splice(gExpr)(Expr.splice(acc), Expr.splice(fe)))
                  }
                  MIO.pure(acc)
              }
          }
        }
        .flatMap { case Some(r) => MIO.pure(r); case None => MIO.fail(new RuntimeException("No children")) }
    }

    def mkBifoldRightBody(
        fabExpr: Expr[F[Any, Any]],
        lcExpr: Expr[cats.Eval[Any]],
        fExpr: Expr[(Any, cats.Eval[Any]) => cats.Eval[Any]],
        gExpr: Expr[(Any, cats.Eval[Any]) => cats.Eval[Any]]
    ): Expr[cats.Eval[Any]] = runSafe {
      enumm
        .matchOn[MIO, cats.Eval[Any]](fabExpr) { matched =>
          import matched.{value as cv, Underlying as CT}
          val cn = Type[CT].shortName
          childInfo.get(cn) match {
            case None       => MIO.fail(new RuntimeException(s"No info for $cn"))
            case Some(info) =>
              CaseClass.parse[CT].toEither match {
                case Left(_)   => MIO.pure(lcExpr)
                case Right(cc) =>
                  val fields = cc.caseFieldValuesAt(cv).toList
                  val covariant: List[(Expr[Any], Boolean)] = fields.collect {
                    case (fn, fv) if info.leftFields.contains(fn) =>
                      import fv.Underlying as Field; (fv.value.asInstanceOf[Expr[Field]].upcast[Any], true)
                    case (fn, fv) if info.rightFields.contains(fn) =>
                      import fv.Underlying as Field; (fv.value.asInstanceOf[Expr[Field]].upcast[Any], false)
                  }
                  val result = covariant.foldRight(lcExpr) { case ((fe, isLeft), acc) =>
                    if (isLeft) Expr.quote(Expr.splice(fExpr)(Expr.splice(fe), Expr.splice(acc)))
                    else Expr.quote(Expr.splice(gExpr)(Expr.splice(fe), Expr.splice(acc)))
                  }
                  MIO.pure(result)
              }
          }
        }
        .flatMap { case Some(r) => MIO.pure(r); case None => MIO.fail(new RuntimeException("No children")) }
    }

    def mkBitraverseBody(
        fabExpr: Expr[F[Any, Any]],
        fExpr: Expr[Any],
        gExpr: Expr[Any],
        gAppExpr: Expr[Any]
    ): Expr[Any] = runSafe {
      enumm
        .matchOn[MIO, Any](fabExpr) { matched =>
          import matched.{value as cv, Underlying as CT}
          val cn = Type[CT].shortName
          childInfo.get(cn) match {
            case None       => MIO.fail(new RuntimeException(s"No info for $cn"))
            case Some(info) =>
              CaseClass.parse[CT].toEither match {
                case Left(_) =>
                  MIO.pure(
                    Expr
                      .quote(
                        Expr
                          .splice(gAppExpr)
                          .asInstanceOf[cats.Applicative[CatsDerivationFactories.AnyF]]
                          .pure(Expr.splice(cv.upcast[Any]))
                      )
                      .upcast[Any]
                  )
                case Right(cc) =>
                  val fields = cc.caseFieldValuesAt(cv).toList
                  val covariantFields = info.leftFields ++ info.rightFields
                  val gFieldExprs: List[Expr[CatsDerivationFactories.AnyF[Any]]] = fields.collect {
                    case (fn, fv) if info.leftFields.contains(fn) =>
                      import fv.Underlying as Field
                      val fe = fv.value.asInstanceOf[Expr[Field]].upcast[Any]
                      Expr.quote(
                        Expr.splice(fExpr).asInstanceOf[Any => CatsDerivationFactories.AnyF[Any]](Expr.splice(fe))
                      )
                    case (fn, fv) if info.rightFields.contains(fn) =>
                      import fv.Underlying as Field
                      val fe = fv.value.asInstanceOf[Expr[Field]].upcast[Any]
                      Expr.quote(
                        Expr.splice(gExpr).asInstanceOf[Any => CatsDerivationFactories.AnyF[Any]](Expr.splice(fe))
                      )
                  }
                  if (gFieldExprs.isEmpty) {
                    MIO.pure(
                      Expr
                        .quote(
                          Expr
                            .splice(gAppExpr)
                            .asInstanceOf[cats.Applicative[CatsDerivationFactories.AnyF]]
                            .pure(Expr.splice(cv.upcast[Any]))
                        )
                        .upcast[Any]
                    )
                  } else {
                    val reconLambda: Expr[List[Any] => Any] = runSafe {
                      val lambda = LambdaBuilder.of1[List[Any]]("newVals").buildWith { nvExpr =>
                        var currentList: Expr[List[Any]] = nvExpr
                        val fieldExprs: List[(String, Expr_??)] = fields.map { case (fn2, fv2) =>
                          import fv2.Underlying as Field2
                          if (covariantFields.contains(fn2)) {
                            val h = Expr.quote(Expr.splice(currentList).head)
                            val t = Expr.quote(Expr.splice(currentList).tail)
                            currentList = t
                            (fn2, Expr.quote(Expr.splice(h).asInstanceOf[Field2]).as_??)
                          } else {
                            (fn2, fv2.value.asInstanceOf[Expr[Field2]].as_??)
                          }
                        }
                        cc.primaryConstructor.fold(
                          onInstance = _ => throw new RuntimeException("Constructor should not need instance"),
                          onTypes = _ => Map.empty,
                          onValues = _ => fieldExprs.toMap
                        ) match {
                          case Right(e) => e.value.asInstanceOf[Expr[Any]]
                          case Left(e)  => throw new RuntimeException(s"Cannot construct $cn: $e")
                        }
                      }
                      MIO.pure(lambda)
                    }
                    val gListExpr = gFieldExprs.foldRight(
                      Expr.quote(
                        Expr
                          .splice(gAppExpr)
                          .asInstanceOf[cats.Applicative[CatsDerivationFactories.AnyF]]
                          .pure(Nil: List[Any])
                      )
                    ) { (gvExpr, accExpr) =>
                      Expr.quote(
                        Expr
                          .splice(gAppExpr)
                          .asInstanceOf[cats.Applicative[CatsDerivationFactories.AnyF]]
                          .map2[Any, List[Any], List[Any]](Expr.splice(gvExpr), Expr.splice(accExpr))(_ :: _)
                      )
                    }
                    val result = Expr.quote {
                      val recon: List[Any] => Any = Expr.splice(reconLambda)
                      val _ = recon
                      Expr
                        .splice(gAppExpr)
                        .asInstanceOf[cats.Applicative[CatsDerivationFactories.AnyF]]
                        .map[List[Any], Any](Expr.splice(gListExpr))(nv => recon(nv))
                    }
                    MIO.pure(result.upcast[Any])
                  }
              }
          }
        }
        .flatMap { case Some(r) => MIO.pure(r); case None => MIO.fail(new RuntimeException("No children")) }
    }

    MIO.pure {
      Expr.quote {
        CatsDerivationFactories.bitraverseInstance[F](
          bitraverseFn = {
            (fab: F[CatsDerivationFactories.W1, CatsDerivationFactories.W2], fAny: Any, gAny: Any, gApp: Any) =>
              val anyFab: F[Any, Any] = fab.asInstanceOf[F[Any, Any]]
              val _ = anyFab; val _ = fAny; val _ = gAny; val _ = gApp
              Expr.splice(mkBitraverseBody(Expr.quote(anyFab), Expr.quote(fAny), Expr.quote(gAny), Expr.quote(gApp)))
          },
          bimapFn = {
            (
                fab: F[CatsDerivationFactories.W1, CatsDerivationFactories.W2],
                f: CatsDerivationFactories.W1 => CatsDerivationFactories.W3,
                g: CatsDerivationFactories.W2 => CatsDerivationFactories.W4
            ) =>
              val anyFab: F[Any, Any] = fab.asInstanceOf[F[Any, Any]]
              val _ = anyFab; val _ = f; val _ = g
              Expr
                .splice(
                  mkBimapBody(
                    Expr.quote(anyFab),
                    Expr.quote(f.asInstanceOf[Any => Any]),
                    Expr.quote(g.asInstanceOf[Any => Any])
                  )
                )
                .asInstanceOf[F[CatsDerivationFactories.W3, CatsDerivationFactories.W4]]
          },
          bifoldLeftFn = {
            (
                fab: F[CatsDerivationFactories.W1, CatsDerivationFactories.W2],
                cAny: Any,
                fAny: (Any, Any) => Any,
                gAny: (Any, Any) => Any
            ) =>
              val anyFab: F[Any, Any] = fab.asInstanceOf[F[Any, Any]]
              val _ = anyFab; val _ = cAny; val _ = fAny; val _ = gAny
              Expr.splice(mkBifoldLeftBody(Expr.quote(anyFab), Expr.quote(cAny), Expr.quote(fAny), Expr.quote(gAny)))
          },
          bifoldRightFn = {
            (
                fab: F[CatsDerivationFactories.W1, CatsDerivationFactories.W2],
                lcAny: cats.Eval[Any],
                fAny: (Any, cats.Eval[Any]) => cats.Eval[Any],
                gAny: (Any, cats.Eval[Any]) => cats.Eval[Any]
            ) =>
              val anyFab: F[Any, Any] = fab.asInstanceOf[F[Any, Any]]
              val _ = anyFab; val _ = lcAny; val _ = fAny; val _ = gAny
              Expr.splice(mkBifoldRightBody(Expr.quote(anyFab), Expr.quote(lcAny), Expr.quote(fAny), Expr.quote(gAny)))
          }
        )
      }
    }
  }

  protected object BitraverseTypes {
    val Any: Type[Any] = Type.of[Any]
    val Int: Type[Int] = Type.of[Int]
    val String: Type[String] = Type.of[String]
    val Boolean: Type[Boolean] = Type.of[Boolean]
    val ListAny: Type[List[Any]] = Type.of[List[Any]]
    val ListBoolean: Type[List[Boolean]] = Type.of[List[Boolean]]
    val EvalAny: Type[cats.Eval[Any]] = Type.of[cats.Eval[Any]]
    val LogDerivation: Type[hearth.kindlings.catsderivation.LogDerivation] =
      Type.of[hearth.kindlings.catsderivation.LogDerivation]
  }

  def shouldWeLogBitraverseDerivation: Boolean = {
    implicit val LogDerivationType: Type[LogDerivation] = BitraverseTypes.LogDerivation
    Expr.summonImplicit[LogDerivation].isDefined || (for {
      data <- Environment.typedSettings.toOption
      cd <- data.get("catsDerivation")
      shouldLog <- cd.get("logDerivation").flatMap(_.asBoolean)
    } yield shouldLog).getOrElse(false)
  }
}
