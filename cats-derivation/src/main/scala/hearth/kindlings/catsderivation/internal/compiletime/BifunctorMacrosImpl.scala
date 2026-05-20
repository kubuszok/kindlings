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
    caseClassCD.primaryConstructor(mappedFields.toMap) match {
      case Right(constructExpr) => MIO.pure(constructExpr)
      case Left(error)          =>
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
        MIO.scoped { runSafe =>
          runSafe {
            for {
              _ <- Environment.loadStandardExtensions().toMIO(allowFailures = false)
              result <- deriveBifunctorRecursively[F]
            } yield deriveBifunctorForCaseClass(result, runSafe)(FCtor)
          }
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
