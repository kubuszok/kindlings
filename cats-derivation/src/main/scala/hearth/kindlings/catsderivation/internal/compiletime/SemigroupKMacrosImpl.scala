package hearth.kindlings.catsderivation.internal.compiletime

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.fp.instances.*
import hearth.fp.syntax.*
import hearth.std.*

import hearth.kindlings.catsderivation.LogDerivation

/** SemigroupK derivation: combines F[A] values field-wise by summoning Semigroup for each field type in F[Any]. */
trait SemigroupKMacrosImpl extends CatsDerivationTimeout with CatsDerivationErrorSupport {
  this: MacroCommons & StdExtensions =>

  @scala.annotation.nowarn("msg=is never used|unused explicit parameter")
  def deriveSemigroupK[F[_]](
      FCtor0: Type.Ctor1[F],
      SemigroupKFType: Type[cats.SemigroupK[F]]
  ): Expr[cats.SemigroupK[F]] = {
    val macroName = "SemigroupK.derived"

    implicit val FCtor: Type.Ctor1[F] = FCtor0
    implicit val SemigroupKFT: Type[cats.SemigroupK[F]] = SemigroupKFType
    implicit val AnyType: Type[Any] = SemigroupKTypes.Any
    implicit val FAnyType: Type[F[Any]] = FCtor.apply[Any]

    Log
      .namedScope(s"Deriving SemigroupK at: ${Environment.currentPosition.prettyPrint}") {
        CaseClass.parse[F[Any]].toEither match {
          case Right(caseClass) =>
            MIO.scoped { runSafe =>
              val doCombineK: (Expr[F[Any]], Expr[F[Any]]) => Expr[F[Any]] = (xExpr, yExpr) =>
                runSafe {
                  for {
                    _ <- Environment.loadStandardExtensions().toMIO(allowFailures = false)
                    result <- deriveSemigroupKBody[F](caseClass, xExpr, yExpr)
                  } yield result
                }

              import hearth.kindlings.catsderivation.internal.runtime.CatsDerivationFactories
              Expr.quote {
                CatsDerivationFactories.semigroupKInstance[F] {
                  (x: F[CatsDerivationFactories.W1], y: F[CatsDerivationFactories.W1]) =>
                    val anyX: F[Any] = x.asInstanceOf[F[Any]]
                    val anyY: F[Any] = y.asInstanceOf[F[Any]]
                    val _ = anyX
                    val _ = anyY
                    Expr
                      .splice(doCombineK(Expr.quote(anyX), Expr.quote(anyY)))
                      .asInstanceOf[F[CatsDerivationFactories.W1]]
                }
              }
            }
          case Left(reason) =>
            failDerivation(
              CatsDerivationError.CannotParseCaseClass(
                Type[F[Any]].prettyPrint,
                s"$reason. $macroName can only be derived for case classes."
              )
            )
        }
      }
      .flatTap(result => Log.info(s"Derived final result: ${result.prettyPrint}"))
      .runToExprOrFail(
        macroName,
        infoRendering = if (shouldWeLogSemigroupKDerivation) RenderFrom(Log.Level.Info) else DontRender,
        errorRendering = if (shouldWeLogSemigroupKDerivation) RenderFrom(Log.Level.Info) else DontRender,
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
  private def deriveSemigroupKBody[F[_]](
      caseClass: CaseClass[F[Any]],
      xExpr: Expr[F[Any]],
      yExpr: Expr[F[Any]]
  )(implicit FCtor: Type.Ctor1[F], FAnyType: Type[F[Any]], AnyType: Type[Any]): MIO[Expr[F[Any]]] = {
    val fieldsX = caseClass.caseFieldValuesAt(xExpr).toList
    val fieldsY = caseClass.caseFieldValuesAt(yExpr).toList

    val combinedFieldsMIO: MIO[List[(String, Expr_??)]] =
      fieldsX.zip(fieldsY).traverse { case ((fieldName, fieldValueX), (_, fieldValueY)) =>
        import fieldValueX.Underlying as Field
        val fx = fieldValueX.value.asInstanceOf[Expr[Field]]
        val fy = fieldValueY.value.asInstanceOf[Expr[Field]]

        SemigroupKTypes.Semigroup[Field].summonExprIgnoring().toEither match {
          case Right(sgExpr) =>
            val combined: Expr[Field] = Expr.quote(Expr.splice(sgExpr).combine(Expr.splice(fx), Expr.splice(fy)))
            MIO.pure((fieldName, combined.as_??))
          case Left(reason) =>
            failDerivation(
              CatsDerivationError.MissingInstanceForField(
                "Semigroup",
                fieldName,
                Type[F[Any]].prettyPrint,
                Some(s"${Field.prettyPrint}: $reason")
              )
            )
        }
      }

    combinedFieldsMIO.flatMap { combinedFields =>
      constructInstanceFree(caseClass.primaryConstructor, "Constructor", "combined result")(combinedFields.toMap)
        .map(constructExpr => constructExpr.value.asInstanceOf[Expr[F[Any]]])
    }
  }

  protected object SemigroupKTypes {
    val Any: Type[Any] = Type.of[Any]
    def Semigroup: Type.Ctor1[cats.kernel.Semigroup] = Type.Ctor1.of[cats.kernel.Semigroup]
    val LogDerivation: Type[hearth.kindlings.catsderivation.LogDerivation] =
      Type.of[hearth.kindlings.catsderivation.LogDerivation]
  }

  def shouldWeLogSemigroupKDerivation: Boolean = {
    implicit val LogDerivationType: Type[LogDerivation] = SemigroupKTypes.LogDerivation
    Expr.summonImplicit[LogDerivation].isDefined || (for {
      data <- Environment.typedSettings.toOption
      cd <- data.get("catsDerivation")
      shouldLog <- cd.get("logDerivation").flatMap(_.asBoolean)
    } yield shouldLog).getOrElse(false)
  }
}
