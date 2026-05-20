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

              val fieldsP1 = ccIntInt.primaryConstructor.parameters.flatten.toList
              val fieldsP2 = ccStringInt.primaryConstructor.parameters.flatten.toList
              val fieldsP3 = ccIntString.primaryConstructor.parameters.flatten.toList

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
                    caseClass.primaryConstructor(mappedFields.toMap) match {
                      case Right(expr) => MIO.pure(expr)
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
          case Left(reason) =>
            MIO.fail(
              new RuntimeException(
                s"$macroName: Cannot derive for type: $reason. Can only be derived for case classes."
              )
            )
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
        caseClass.primaryConstructor(fieldExprs.toMap) match {
          case Right(constructExpr) => constructExpr.upcast[Any]
          case Left(error)          =>
            throw new RuntimeException(s"Cannot construct bitraversed result: $error")
        }
      }
    MIO.pure(lambda)
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
