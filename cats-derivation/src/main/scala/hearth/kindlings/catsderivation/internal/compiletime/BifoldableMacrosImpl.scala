package hearth.kindlings.catsderivation.internal.compiletime

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

import hearth.kindlings.catsderivation.LogDerivation

trait BifoldableMacrosImpl extends CatsDerivationTimeout { this: MacroCommons & StdExtensions =>

  @scala.annotation.nowarn("msg=is never used|unused explicit parameter")
  def deriveBifoldable[F[_, _]](
      FCtor0: Type.Ctor2[F],
      BifoldableFType: Type[cats.Bifoldable[F]]
  ): Expr[cats.Bifoldable[F]] = {
    val macroName = "Bifoldable.derived"

    implicit val FCtor: Type.Ctor2[F] = FCtor0
    implicit val BifoldableFT: Type[cats.Bifoldable[F]] = BifoldableFType

    Log
      .namedScope(s"Deriving Bifoldable at: ${Environment.currentPosition.prettyPrint}") {
        implicit val AnyType0: Type[Any] = BifoldableTypes.Any
        val isCaseClass = CaseClass.parse(using FCtor.apply[Any, Any]).toEither.isRight
        if (!isCaseClass) MIO.scoped { runSafe =>
          val _ = runSafe(Environment.loadStandardExtensions().toMIO(allowFailures = false))
          runSafe(deriveBifoldableForEnum[F](FCtor, runSafe))
        }
        else
          MIO.scoped { runSafe =>
            implicit val IntType: Type[Int] = BifoldableTypes.Int
            implicit val StringType: Type[String] = BifoldableTypes.String

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

            val leftFields = scala.collection.mutable.Set.empty[String]
            val rightFields = scala.collection.mutable.Set.empty[String]

            fieldsP1.zip(fieldsP2).zip(fieldsP3).foreach { case (((name, p1), (_, p2)), (_, p3)) =>
              val t1 = p1.tpe.Underlying
              val t2 = p2.tpe.Underlying
              val t3 = p3.tpe.Underlying
              val firstChanges = !(t1 =:= t2)
              val secondChanges = !(t1 =:= t3)
              if (firstChanges && !secondChanges) leftFields += name
              else if (!firstChanges && secondChanges) rightFields += name
            }

            val leftFieldSet: Set[String] = leftFields.toSet
            val rightFieldSet: Set[String] = rightFields.toSet

            runSafe {
              Environment.loadStandardExtensions().toMIO(allowFailures = false).map(_ => ())
            }

            implicit val FAnyAnyType: Type[F[Any, Any]] = FCtor.apply[Any, Any]
            implicit val EvalAnyType: Type[cats.Eval[Any]] = BifoldableTypes.EvalCtor.apply[Any]

            val ccAnyAny = CaseClass.parse[F[Any, Any]].toEither match {
              case Right(cc) => cc
              case Left(e)   => throw new RuntimeException(s"Cannot parse F[Any, Any]: $e")
            }

            val doBifoldLeft: (Expr[F[Any, Any]], Expr[Any], Expr[(Any, Any) => Any], Expr[(Any, Any) => Any]) => Expr[
              Any
            ] =
              (fabExpr, cExpr, fExpr, gExpr) =>
                runSafe {
                  val fields = ccAnyAny.caseFieldValuesAt(fabExpr).toList
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
                Expr[
                  (Any, cats.Eval[Any]) => cats.Eval[Any]
                ]
            ) => Expr[cats.Eval[Any]] =
              (fabExpr, lcExpr, fExpr, gExpr) =>
                runSafe {
                  val fields = ccAnyAny.caseFieldValuesAt(fabExpr).toList
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
              CatsDerivationFactories.bifoldableInstance[F](
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
      }
      .flatTap(result => Log.info(s"Derived final result: ${result.prettyPrint}"))
      .runToExprOrFail(
        macroName,
        infoRendering = if (shouldWeLogBifoldableDerivation) RenderFrom(Log.Level.Info) else DontRender,
        errorRendering = if (shouldWeLogBifoldableDerivation) RenderFrom(Log.Level.Info) else DontRender,
        timeout = derivationTimeout
      ) { (errorLogs, errors) =>
        val errorsRendered = errors.map(e => "  - " + e.getMessage).mkString("\n")
        val hint =
          "Enable debug logging with: import hearth.kindlings.catsderivation.debug.logDerivationForCatsDerivation"
        if (errorLogs.nonEmpty) s"Macro derivation failed:\n$errorsRendered\nlogs:\n$errorLogs\n$hint"
        else s"Macro derivation failed:\n$errorsRendered\n$hint"
      }
  }

  @scala.annotation.nowarn(
    "msg=is never used|unused explicit parameter|unused local definition|unused implicit parameter"
  )
  private def deriveBifoldableForEnum[F[_, _]](
      FCtor: Type.Ctor2[F],
      runSafe: hearth.fp.DirectStyle.RunSafe[hearth.fp.effect.MIO]
  ): MIO[Expr[cats.Bifoldable[F]]] = {
    implicit val AnyType: Type[Any] = BifoldableTypes.Any
    implicit val IntType: Type[Int] = BifoldableTypes.Int
    implicit val StringType: Type[String] = BifoldableTypes.String
    implicit val FAAType: Type[F[Any, Any]] = FCtor.apply[Any, Any]
    implicit val EvalAnyType: Type[cats.Eval[Any]] = BifoldableTypes.EvalCtor.apply[Any]
    implicit val FCtor1: Type.Ctor2[F] = FCtor

    Enum.parse[F[Any, Any]].toEither match {
      case Left(reason) =>
        MIO.fail(new RuntimeException(s"Cannot parse as enum for Bifoldable: $reason"))
      case Right(enumm) =>
        val enumP1 = Enum.parse(using FCtor.apply[Int, Int].asInstanceOf[Type[Any]]).toEither match {
          case Right(e) => e; case Left(_) => return MIO.fail(new RuntimeException("Cannot parse F[Int,Int] as enum"))
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
                val f1 = cc1.primaryConstructor.parameters.flatten.toList
                val f2 = cc2.primaryConstructor.parameters.flatten.toList
                val f3 = cc3.primaryConstructor.parameters.flatten.toList
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

        def mkBifoldLeftBody(
            fabExpr: Expr[F[Any, Any]],
            cExpr: Expr[Any],
            fExpr: Expr[(Any, Any) => Any],
            gExpr: Expr[(Any, Any) => Any]
        ): Expr[Any] = runSafe {
          enumm
            .matchOn[MIO, Any](fabExpr) { matched =>
              import matched.{value as caseValue, Underlying as CT}
              val cn = Type[CT].shortName
              childInfo.get(cn) match {
                case None       => MIO.fail(new RuntimeException(s"No info for $cn"))
                case Some(info) =>
                  CaseClass.parse[CT].toEither match {
                    case Left(_)   => MIO.pure(cExpr)
                    case Right(cc) =>
                      val fields = cc.caseFieldValuesAt(caseValue).toList
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
              import matched.{value as caseValue, Underlying as CT}
              val cn = Type[CT].shortName
              childInfo.get(cn) match {
                case None       => MIO.fail(new RuntimeException(s"No info for $cn"))
                case Some(info) =>
                  CaseClass.parse[CT].toEither match {
                    case Left(_)   => MIO.pure(lcExpr)
                    case Right(cc) =>
                      val fields = cc.caseFieldValuesAt(caseValue).toList
                      val covariant: List[(Expr[Any], Boolean)] = fields.collect {
                        case (fn, fv) if info.leftFields.contains(fn) =>
                          import fv.Underlying as Field
                          (fv.value.asInstanceOf[Expr[Field]].upcast[Any], true)
                        case (fn, fv) if info.rightFields.contains(fn) =>
                          import fv.Underlying as Field
                          (fv.value.asInstanceOf[Expr[Field]].upcast[Any], false)
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

        MIO.pure {
          Expr.quote {
            CatsDerivationFactories.bifoldableInstance[F](
              bifoldLeftFn = {
                (
                    fab: F[CatsDerivationFactories.W1, CatsDerivationFactories.W2],
                    cAny: Any,
                    fAny: (Any, Any) => Any,
                    gAny: (Any, Any) => Any
                ) =>
                  val anyFab: F[Any, Any] = fab.asInstanceOf[F[Any, Any]]
                  val _ = anyFab; val _ = cAny; val _ = fAny; val _ = gAny
                  Expr.splice(
                    mkBifoldLeftBody(Expr.quote(anyFab), Expr.quote(cAny), Expr.quote(fAny), Expr.quote(gAny))
                  )
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
                  Expr.splice(
                    mkBifoldRightBody(Expr.quote(anyFab), Expr.quote(lcAny), Expr.quote(fAny), Expr.quote(gAny))
                  )
              }
            )
          }
        }
    }
  }

  protected object BifoldableTypes {
    val Any: Type[Any] = Type.of[Any]
    val Int: Type[Int] = Type.of[Int]
    val String: Type[String] = Type.of[String]
    def EvalCtor: Type.Ctor1[cats.Eval] = Type.Ctor1.of[cats.Eval]
    val LogDerivation: Type[hearth.kindlings.catsderivation.LogDerivation] =
      Type.of[hearth.kindlings.catsderivation.LogDerivation]
  }

  def shouldWeLogBifoldableDerivation: Boolean = {
    implicit val LogDerivationType: Type[LogDerivation] = BifoldableTypes.LogDerivation
    Expr.summonImplicit[LogDerivation].isDefined || (for {
      data <- Environment.typedSettings.toOption
      cd <- data.get("catsDerivation")
      shouldLog <- cd.get("logDerivation").flatMap(_.asBoolean)
    } yield shouldLog).getOrElse(false)
  }
}
