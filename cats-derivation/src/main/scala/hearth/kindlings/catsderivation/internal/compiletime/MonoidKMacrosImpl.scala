package hearth.kindlings.catsderivation.internal.compiletime

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

import hearth.kindlings.catsderivation.LogDerivation

/** MonoidK derivation: combines EmptyK + SemigroupK by summoning Monoid for each field type in F[Any]. */
trait MonoidKMacrosImpl extends CatsDerivationTimeout { this: MacroCommons & StdExtensions =>

  @scala.annotation.nowarn("msg=is never used|unused explicit parameter")
  def deriveMonoidK[F[_]](
      FCtor0: Type.Ctor1[F],
      MonoidKFType: Type[cats.MonoidK[F]]
  ): Expr[cats.MonoidK[F]] = {
    val macroName = "MonoidK.derived"

    implicit val FCtor: Type.Ctor1[F] = FCtor0
    implicit val MonoidKFT: Type[cats.MonoidK[F]] = MonoidKFType
    implicit val AnyType: Type[Any] = MonoidKTypes.Any
    implicit val FAnyType: Type[F[Any]] = FCtor.apply[Any]

    Log
      .namedScope(s"Deriving MonoidK at: ${Environment.currentPosition.prettyPrint}") {
        CaseClass.parse[F[Any]].toEither match {
          case Right(caseClass) =>
            MIO.scoped { runSafe =>
              // Load standard extensions exactly once for this derivation, before deriving
              // either the empty or the combineK body (issue kubuszok/kindlings#65).
              val _ = runSafe {
                Environment.loadStandardExtensions().toMIO(allowFailures = false)
              }

              val doEmpty: Expr[F[Any]] = runSafe {
                deriveMonoidKEmptyBody[F](caseClass)
              }

              val doCombineK: (Expr[F[Any]], Expr[F[Any]]) => Expr[F[Any]] = (xExpr, yExpr) =>
                runSafe {
                  deriveMonoidKCombineBody[F](caseClass, xExpr, yExpr)
                }

              Expr.quote {
                new cats.MonoidK[F] {
                  def empty[A]: F[A] = Expr.splice(doEmpty).asInstanceOf[F[A]]
                  def combineK[A](x: F[A], y: F[A]): F[A] = {
                    val anyX: F[Any] = x.asInstanceOf[F[Any]]
                    val anyY: F[Any] = y.asInstanceOf[F[Any]]
                    val _ = anyX
                    val _ = anyY
                    Expr.splice(doCombineK(Expr.quote(anyX), Expr.quote(anyY))).asInstanceOf[F[A]]
                  }
                }
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
        infoRendering = if (shouldWeLogMonoidKDerivation) RenderFrom(Log.Level.Info) else DontRender,
        errorRendering = if (shouldWeLogMonoidKDerivation) RenderFrom(Log.Level.Info) else DontRender,
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
  private def deriveMonoidKEmptyBody[F[_]](
      caseClass: CaseClass[F[Any]]
  )(implicit FCtor: Type.Ctor1[F], FAnyType: Type[F[Any]], AnyType: Type[Any]): MIO[Expr[F[Any]]] = {
    val fields = caseClass.primaryConstructor.parameters.flatten.toList

    val fieldExprs: List[(String, Expr_??)] = fields.map { case (fieldName, param) =>
      import param.tpe.Underlying as Field

      val monoidExpr = MonoidKTypes.Monoid[Field].summonExprIgnoring().toEither match {
        case Right(m)     => m
        case Left(reason) =>
          throw new RuntimeException(
            s"No Monoid instance found for field '$fieldName': ${Field.prettyPrint}: $reason"
          )
      }
      val value: Expr[Field] = Expr.quote(Expr.splice(monoidExpr).empty)
      (fieldName, value.as_??)
    }

    caseClass.primaryConstructor(fieldExprs.toMap) match {
      case Right(constructExpr) => MIO.pure(constructExpr)
      case Left(error)          =>
        MIO.fail(new RuntimeException(s"Cannot construct empty result: $error"))
    }
  }

  @scala.annotation.nowarn("msg=is never used|unused implicit parameter")
  private def deriveMonoidKCombineBody[F[_]](
      caseClass: CaseClass[F[Any]],
      xExpr: Expr[F[Any]],
      yExpr: Expr[F[Any]]
  )(implicit FCtor: Type.Ctor1[F], FAnyType: Type[F[Any]], AnyType: Type[Any]): MIO[Expr[F[Any]]] = {
    val fieldsX = caseClass.caseFieldValuesAt(xExpr).toList
    val fieldsY = caseClass.caseFieldValuesAt(yExpr).toList

    val combinedFields: List[(String, Expr_??)] =
      fieldsX.zip(fieldsY).map { case ((fieldName, fieldValueX), (_, fieldValueY)) =>
        import fieldValueX.Underlying as Field
        val fx = fieldValueX.value.asInstanceOf[Expr[Field]]
        val fy = fieldValueY.value.asInstanceOf[Expr[Field]]

        val sgExpr = MonoidKTypes.Semigroup[Field].summonExprIgnoring().toEither match {
          case Right(sg)    => sg
          case Left(reason) =>
            throw new RuntimeException(
              s"No Semigroup instance found for field '$fieldName': ${Field.prettyPrint}: $reason"
            )
        }
        val combined: Expr[Field] = Expr.quote(Expr.splice(sgExpr).combine(Expr.splice(fx), Expr.splice(fy)))
        (fieldName, combined.as_??)
      }

    caseClass.primaryConstructor(combinedFields.toMap) match {
      case Right(constructExpr) => MIO.pure(constructExpr)
      case Left(error)          =>
        MIO.fail(new RuntimeException(s"Cannot construct combined result: $error"))
    }
  }

  protected object MonoidKTypes {
    val Any: Type[Any] = Type.of[Any]
    def Monoid: Type.Ctor1[cats.kernel.Monoid] = Type.Ctor1.of[cats.kernel.Monoid]
    def Semigroup: Type.Ctor1[cats.kernel.Semigroup] = Type.Ctor1.of[cats.kernel.Semigroup]
    val LogDerivation: Type[hearth.kindlings.catsderivation.LogDerivation] =
      Type.of[hearth.kindlings.catsderivation.LogDerivation]
  }

  def shouldWeLogMonoidKDerivation: Boolean = {
    implicit val LogDerivationType: Type[LogDerivation] = MonoidKTypes.LogDerivation
    Expr.summonImplicit[LogDerivation].isDefined || (for {
      data <- Environment.typedSettings.toOption
      cd <- data.get("catsDerivation")
      shouldLog <- cd.get("logDerivation").flatMap(_.asBoolean)
    } yield shouldLog).getOrElse(false)
  }
}
