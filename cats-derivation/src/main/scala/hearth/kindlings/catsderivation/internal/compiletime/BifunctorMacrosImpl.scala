package hearth.kindlings.catsderivation.internal.compiletime

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

import hearth.kindlings.catsderivation.LogDerivation

trait BifunctorMacrosImpl extends rules.BifunctorCaseClassRuleImpl with CatsDerivationTimeout {
  this: MacroCommons & StdExtensions =>

  final case class BifunctorCaseClassResult[F[_, _]](
      FCtor: Type.Ctor2[F],
      leftFields: Set[String],
      rightFields: Set[String]
  )

  abstract class BifunctorDerivationRule(val name: String) extends Rule {
    def apply[F[_, _]](implicit FCtor: Type.Ctor2[F]): MIO[Rule.Applicability[BifunctorCaseClassResult[F]]]
  }

  def deriveBifunctorRecursively[F[_, _]](implicit FCtor: Type.Ctor2[F]): MIO[BifunctorCaseClassResult[F]] = {
    implicit val AnyType: Type[Any] = BifunctorTypes.Any
    val fName = FCtor.apply[Any, Any].shortName
    Log.namedScope(s"Deriving Bifunctor for $fName") {
      Rules(BifunctorCaseClassRule)(_[F]).flatMap {
        case Right(result) =>
          Log.info(s"Derived Bifunctor for $fName") >> MIO.pure(result)
        case Left(reasons) =>
          val reasonsStrings = reasons.toListMap.view.map { case (rule, rs) =>
            if (rs.isEmpty) s"The rule ${rule.name} was not applicable"
            else s" - ${rule.name}: ${rs.mkString(", ")}"
          }.toList
          val err = BifunctorDerivationError.UnsupportedType(fName, reasonsStrings)
          Log.error(err.message) >> MIO.fail(err)
      }
    }
  }

  @scala.annotation.nowarn("msg=is never used|unused explicit parameter|unused local definition")
  protected def deriveBifunctorForCaseClass[F[_, _]](
      result: BifunctorCaseClassResult[F],
      runSafe: hearth.fp.DirectStyle.RunSafe[hearth.fp.effect.MIO]
  )(implicit FCtor: Type.Ctor2[F]): Expr[cats.Bifunctor[F]] = {
    import hearth.kindlings.catsderivation.internal.runtime.CatsDerivationFactories
    Expr.quote {
      CatsDerivationFactories.bifunctorInstance[F] {
        (
            fab: F[CatsDerivationFactories.W1, CatsDerivationFactories.W2],
            f: CatsDerivationFactories.W1 => CatsDerivationFactories.W3,
            g: CatsDerivationFactories.W2 => CatsDerivationFactories.W4
        ) =>
          val _ = fab
          val _ = f
          val _ = g
          Expr.splice {
            val body: MIO[Expr[F[CatsDerivationFactories.W3, CatsDerivationFactories.W4]]] =
              deriveBifunctorBimapBody[
                F,
                CatsDerivationFactories.W1,
                CatsDerivationFactories.W2,
                CatsDerivationFactories.W3,
                CatsDerivationFactories.W4
              ](
                result.FCtor,
                result.leftFields,
                result.rightFields,
                Expr.quote(fab),
                Expr.quote(f),
                Expr.quote(g)
              )(
                Type.of[CatsDerivationFactories.W1],
                Type.of[CatsDerivationFactories.W2],
                Type.of[CatsDerivationFactories.W3],
                Type.of[CatsDerivationFactories.W4]
              )
            runSafe(body)
          }
      }
    }
  }

  @scala.annotation.nowarn("msg=is never used|unused explicit parameter|unused implicit parameter")
  private def deriveBifunctorBimapBody[F[_, _], A, B, C, D](
      FCtor: Type.Ctor2[F],
      leftFields: Set[String],
      rightFields: Set[String],
      fabExpr: Expr[F[A, B]],
      fExpr: Expr[A => C],
      gExpr: Expr[B => D]
  )(implicit AType: Type[A], BType: Type[B], CType: Type[C], DType: Type[D]): MIO[Expr[F[C, D]]] = {
    implicit val FABType: Type[F[A, B]] = FCtor.apply[A, B]
    implicit val FCDType: Type[F[C, D]] = FCtor.apply[C, D]

    val caseClass = CaseClass.parse[F[A, B]].toEither match {
      case Right(cc) => cc
      case Left(e)   => throw new RuntimeException(s"Cannot parse F[A, B]: $e")
    }
    val fields = caseClass.caseFieldValuesAt(fabExpr).toList

    val mappedFields: List[(String, Expr_??)] = fields.map { case (fieldName, fieldValue) =>
      import fieldValue.Underlying as Field
      val fieldExpr = fieldValue.value.asInstanceOf[Expr[Field]]

      if (leftFields.contains(fieldName)) {
        val mapped: Expr[C] = Expr.quote(Expr.splice(fExpr)(Expr.splice(fieldExpr.asInstanceOf[Expr[A]])))
        (fieldName, mapped.as_??)
      } else if (rightFields.contains(fieldName)) {
        val mapped: Expr[D] = Expr.quote(Expr.splice(gExpr)(Expr.splice(fieldExpr.asInstanceOf[Expr[B]])))
        (fieldName, mapped.as_??)
      } else {
        (fieldName, fieldExpr.as_??)
      }
    }

    val caseClassCD = CaseClass.parse[F[C, D]].toEither match {
      case Right(cc) => cc
      case Left(e)   => throw new RuntimeException(s"Cannot parse F[C, D]: $e")
    }
    caseClassCD.primaryConstructor.fold(
      onInstance = _ => throw new RuntimeException("Constructor should not need instance"),
      onTypes = _ => Map.empty,
      onValues = _ => mappedFields.toMap
    ) match {
      case Right(constructExpr) =>
        MIO.pure(constructExpr.value.asInstanceOf[Expr[F[C, D]]])
      case Left(error) =>
        MIO.fail(new RuntimeException(s"Cannot construct bimap result: $error"))
    }
  }

  @scala.annotation.nowarn("msg=is never used|unused explicit parameter|unused local definition")
  def deriveBifunctor[F[_, _]](
      FCtor0: Type.Ctor2[F],
      BifunctorFType: Type[cats.Bifunctor[F]]
  ): Expr[cats.Bifunctor[F]] = {
    val macroName = "Bifunctor.derived"

    implicit val FCtor: Type.Ctor2[F] = FCtor0
    implicit val BifunctorFT: Type[cats.Bifunctor[F]] = BifunctorFType

    Log
      .namedScope(s"Deriving Bifunctor at: ${Environment.currentPosition.prettyPrint}") {
        implicit val AnyType: Type[Any] = BifunctorTypes.Any
        val isCaseClass = CaseClass.parse(using FCtor.apply[Any, Any]).toEither.isRight
        if (isCaseClass) MIO.scoped { runSafe =>
          runSafe {
            for {
              _ <- Environment.loadStandardExtensions().toMIO(allowFailures = false)
              result <- deriveBifunctorRecursively[F]
            } yield deriveBifunctorForCaseClass(result, runSafe)(FCtor)
          }
        }
        else
          MIO.scoped { runSafe =>
            val _ = runSafe(Environment.loadStandardExtensions().toMIO(allowFailures = false))
            runSafe(deriveBifunctorForEnum[F](FCtor, runSafe))
          }
      }
      .flatTap(result => Log.info(s"Derived final result: ${result.prettyPrint}"))
      .runToExprOrFail(
        macroName,
        infoRendering = if (shouldWeLogBifunctorDerivation) RenderFrom(Log.Level.Info) else DontRender,
        errorRendering = if (shouldWeLogBifunctorDerivation) RenderFrom(Log.Level.Info) else DontRender,
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
  private def deriveBifunctorForEnum[F[_, _]](
      FCtor: Type.Ctor2[F],
      runSafe: hearth.fp.DirectStyle.RunSafe[hearth.fp.effect.MIO]
  ): MIO[Expr[cats.Bifunctor[F]]] = {
    implicit val AnyType: Type[Any] = BifunctorTypes.Any
    implicit val IntType: Type[Int] = BifunctorTypes.Int
    implicit val StringType: Type[String] = BifunctorTypes.String
    implicit val FAAType: Type[F[Any, Any]] = FCtor.apply[Any, Any]
    implicit val FCtor1: Type.Ctor2[F] = FCtor

    Enum.parse[F[Any, Any]].toEither match {
      case Left(reason) =>
        MIO.fail(new RuntimeException(s"Cannot parse as enum for Bifunctor: $reason"))
      case Right(enumm) =>
        val enumProbe1 = Enum.parse(using FCtor.apply[Int, Int].asInstanceOf[Type[Any]]).toEither match {
          case Right(e) => e; case Left(_) => return MIO.fail(new RuntimeException("Cannot parse F[Int,Int] as enum"))
        }
        val enumProbe2 = Enum.parse(using FCtor.apply[String, Int].asInstanceOf[Type[Any]]).toEither match {
          case Right(e) => e
          case Left(_)  => return MIO.fail(new RuntimeException("Cannot parse F[String,Int] as enum"))
        }
        val enumProbe3 = Enum.parse(using FCtor.apply[Int, String].asInstanceOf[Type[Any]]).toEither match {
          case Right(e) => e
          case Left(_)  => return MIO.fail(new RuntimeException("Cannot parse F[Int,String] as enum"))
        }

        case class BiChildInfo(leftFields: Set[String], rightFields: Set[String])

        val childInfo: Map[String, BiChildInfo] = runSafe {
          val result = scala.collection.mutable.Map.empty[String, BiChildInfo]
          val c1 = enumProbe1.directChildren.toList
          val c2 = enumProbe2.directChildren.toList
          val c3 = enumProbe3.directChildren.toList
          c1.zip(c2).zip(c3).foreach { case (((cn, ch1), (_, ch2)), (_, ch3)) =>
            import ch1.Underlying as C1
            import ch2.Underlying as C2
            import ch3.Underlying as C3
            val cc1 = CaseClass.parse[C1].toEither
            val cc2 = CaseClass.parse[C2].toEither
            val cc3 = CaseClass.parse[C3].toEither
            (cc1, cc2, cc3) match {
              case (Right(cI), Right(cII), Right(cIII)) =>
                val f1 = cI.primaryConstructor.totalParameters.flatten.toList
                val f2 = cII.primaryConstructor.totalParameters.flatten.toList
                val f3 = cIII.primaryConstructor.totalParameters.flatten.toList
                val left = scala.collection.mutable.Set.empty[String]
                val right = scala.collection.mutable.Set.empty[String]
                f1.zip(f2).zip(f3).foreach { case (((name, p1), (_, p2)), (_, p3)) =>
                  val t1 = p1.tpe.Underlying
                  val t2 = p2.tpe.Underlying
                  val t3 = p3.tpe.Underlying
                  val firstChanges = !(t1 =:= t2)
                  val secondChanges = !(t1 =:= t3)
                  if (firstChanges && !secondChanges) left += name
                  else if (!firstChanges && secondChanges) right += name
                }
                result += (cn -> BiChildInfo(left.toSet, right.toSet))
              case _ =>
                result += (cn -> BiChildInfo(Set.empty, Set.empty))
            }
          }
          MIO.pure(result.toMap)
        }

        import hearth.kindlings.catsderivation.internal.runtime.CatsDerivationFactories

        def mkBimapBody(
            fabExpr: Expr[F[Any, Any]],
            fExpr: Expr[Any => Any],
            gExpr: Expr[Any => Any]
        ): Expr[F[Any, Any]] = runSafe {
          enumm
            .matchOn[MIO, F[Any, Any]](fabExpr) { matched =>
              import matched.{value as caseValue, Underlying as CT}
              val cn = Type[CT].shortName
              childInfo.get(cn) match {
                case None       => MIO.fail(new RuntimeException(s"No info for $cn"))
                case Some(info) =>
                  CaseClass.parse[CT].toEither match {
                    case Left(_) =>
                      MIO.pure(caseValue.upcast[F[Any, Any]])
                    case Right(cc) =>
                      val fields = cc.caseFieldValuesAt(caseValue).toList
                      if (fields.isEmpty) {
                        MIO.pure(caseValue.upcast[F[Any, Any]])
                      } else {
                        val mapped: List[(String, Expr_??)] = fields.map { case (fn, fv) =>
                          import fv.Underlying as Field
                          val fe = fv.value.asInstanceOf[Expr[Field]]
                          if (info.leftFields.contains(fn)) {
                            val m: Expr[Any] = Expr.quote(Expr.splice(fExpr)(Expr.splice(fe.upcast[Any])))
                            (fn, Expr.quote(Expr.splice(m).asInstanceOf[Field]).as_??)
                          } else if (info.rightFields.contains(fn)) {
                            val m: Expr[Any] = Expr.quote(Expr.splice(gExpr)(Expr.splice(fe.upcast[Any])))
                            (fn, Expr.quote(Expr.splice(m).asInstanceOf[Field]).as_??)
                          } else {
                            (fn, fe.as_??)
                          }
                        }
                        cc.primaryConstructor.fold(
                          onInstance = _ => throw new RuntimeException("Constructor should not need instance"),
                          onTypes = _ => Map.empty,
                          onValues = _ => mapped.toMap
                        ) match {
                          case Right(expr) => MIO.pure(expr.value.asInstanceOf[Expr[F[Any, Any]]])
                          case Left(err)   => MIO.fail(new RuntimeException(s"Cannot construct $cn: $err"))
                        }
                      }
                  }
              }
            }
            .flatMap {
              case Some(r) => MIO.pure(r)
              case None    => MIO.fail(new RuntimeException("No children"))
            }
        }

        MIO.pure {
          Expr.quote {
            CatsDerivationFactories.bifunctorInstance[F] {
              (
                  fab: F[CatsDerivationFactories.W1, CatsDerivationFactories.W2],
                  f: CatsDerivationFactories.W1 => CatsDerivationFactories.W3,
                  g: CatsDerivationFactories.W2 => CatsDerivationFactories.W4
              ) =>
                val _ = fab; val _ = f; val _ = g
                val r$0: F[Any, Any] = Expr.splice {
                  mkBimapBody(
                    Expr.quote(fab.asInstanceOf[F[Any, Any]]),
                    Expr.quote(f.asInstanceOf[Any => Any]),
                    Expr.quote(g.asInstanceOf[Any => Any])
                  )
                }
                r$0.asInstanceOf[F[CatsDerivationFactories.W3, CatsDerivationFactories.W4]]
            }
          }
        }
    }
  }

  protected object BifunctorTypes {
    val Any: Type[Any] = Type.of[Any]
    val Int: Type[Int] = Type.of[Int]
    val String: Type[String] = Type.of[String]
    val LogDerivation: Type[hearth.kindlings.catsderivation.LogDerivation] =
      Type.of[hearth.kindlings.catsderivation.LogDerivation]
  }

  def shouldWeLogBifunctorDerivation: Boolean = {
    implicit val LogDerivationType: Type[LogDerivation] = BifunctorTypes.LogDerivation
    Expr.summonImplicit[LogDerivation].isDefined || (for {
      data <- Environment.typedSettings.toOption
      cd <- data.get("catsDerivation")
      shouldLog <- cd.get("logDerivation").flatMap(_.asBoolean)
    } yield shouldLog).getOrElse(false)
  }
}

sealed private[compiletime] trait BifunctorDerivationError
    extends util.control.NoStackTrace
    with Product
    with Serializable {
  def message: String
  override def getMessage(): String = message
}
private[compiletime] object BifunctorDerivationError {
  final case class UnsupportedType(tpeName: String, reasons: List[String]) extends BifunctorDerivationError {
    override def message: String =
      s"The type constructor $tpeName was not handled by any Bifunctor derivation rule:\n${reasons.mkString("\n")}"
  }
}
