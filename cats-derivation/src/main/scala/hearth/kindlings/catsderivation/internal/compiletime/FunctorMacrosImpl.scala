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
trait FunctorMacrosImpl extends rules.FunctorCaseClassRuleImpl { this: MacroCommons & StdExtensions =>

  final case class FunctorCaseClassResult[F[_]](FCtor: Type.Ctor1[F], directFieldSet: Set[String])

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

  protected def deriveFunctorForCaseClass[F[_]](
      result: FunctorCaseClassResult[F],
      runSafe: hearth.fp.DirectStyle.RunSafe[hearth.fp.effect.MIO]
  )(implicit FCtor: Type.Ctor1[F]): Expr[cats.Functor[F]] =
    Expr.quote {
      new cats.Functor[F] {
        def map[A, B](fa: F[A])(f: A => B): F[B] =
          Expr.splice {
            val body: MIO[Expr[F[B]]] = deriveFunctorMapBody[F, A, B](
              result.FCtor,
              result.directFieldSet,
              Expr.quote(fa),
              Expr.quote(f)
            )(Type.of[A], Type.of[B])
            runSafe(body)
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
          runSafe {
            for {
              _ <- Environment.loadStandardExtensions().toMIO(allowFailures = false)
              result <- deriveFunctorRecursively[F]
            } yield deriveFunctorForCaseClass(result, runSafe)(FCtor)
          }
        }
      }
      .flatTap(result => Log.info(s"Derived final result: ${result.prettyPrint}"))
      .runToExprOrFail(
        macroName,
        infoRendering = if (shouldWeLogFunctorDerivation) RenderFrom(Log.Level.Info) else DontRender,
        errorRendering = if (shouldWeLogFunctorDerivation) RenderFrom(Log.Level.Info) else DontRender
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
      faExpr: Expr[F[A]],
      fExpr: Expr[A => B]
  )(implicit AType: Type[A], BType: Type[B]): MIO[Expr[F[B]]] = {
    implicit val FAType: Type[F[A]] = FCtor.apply[A]
    implicit val FBType: Type[F[B]] = FCtor.apply[B]

    val caseClass = CaseClass.parse[F[A]].toEither match {
      case Right(cc) => cc
      case Left(e)   => throw new RuntimeException(s"Cannot parse F[A]: $e")
    }
    val fields = caseClass.caseFieldValuesAt(faExpr).toList

    val mappedFields: List[(String, Expr_??)] = fields.map { case (fieldName, fieldValue) =>
      import fieldValue.Underlying as Field
      val fieldExpr = fieldValue.value.asInstanceOf[Expr[Field]]

      if (directFields.contains(fieldName)) {
        val mapped: Expr[B] = Expr.quote(Expr.splice(fExpr)(Expr.splice(fieldExpr.asInstanceOf[Expr[A]])))
        (fieldName, mapped.as_??)
      } else {
        (fieldName, fieldExpr.as_??)
      }
    }

    val caseClassB = CaseClass.parse[F[B]].toEither match {
      case Right(cc) => cc
      case Left(e)   => throw new RuntimeException(s"Cannot parse F[B]: $e")
    }
    caseClassB.primaryConstructor(mappedFields.toMap) match {
      case Right(constructExpr) => MIO.pure(constructExpr)
      case Left(error)          =>
        MIO.fail(new RuntimeException(s"Cannot construct mapped result: $error"))
    }
  }

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
