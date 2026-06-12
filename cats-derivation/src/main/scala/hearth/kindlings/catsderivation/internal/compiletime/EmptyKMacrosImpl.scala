package hearth.kindlings.catsderivation.internal.compiletime

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.fp.instances.*
import hearth.fp.syntax.*
import hearth.std.*

import hearth.kindlings.catsderivation.LogDerivation

/** EmptyK derivation: constructs empty F[A] by summoning Empty for each field type in F[Any]. */
trait EmptyKMacrosImpl extends CatsDerivationTimeout with CatsDerivationErrorSupport {
  this: MacroCommons & StdExtensions =>

  @scala.annotation.nowarn("msg=is never used|unused explicit parameter")
  def deriveEmptyK[F[_]](FCtor0: Type.Ctor1[F], EmptyKFType: Type[alleycats.EmptyK[F]]): Expr[alleycats.EmptyK[F]] = {
    val macroName = "EmptyK.derived"

    implicit val FCtor: Type.Ctor1[F] = FCtor0
    implicit val EmptyKFT: Type[alleycats.EmptyK[F]] = EmptyKFType
    implicit val AnyType: Type[Any] = EmptyKTypes.Any
    implicit val FAnyType: Type[F[Any]] = FCtor.apply[Any]

    Log
      .namedScope(s"Deriving EmptyK at: ${Environment.currentPosition.prettyPrint}") {
        CaseClass.parse[F[Any]].toEither match {
          case Right(caseClass) =>
            MIO.scoped { runSafe =>
              val doEmpty: Expr[F[Any]] = runSafe {
                for {
                  _ <- Environment.loadStandardExtensions().toMIO(allowFailures = false)
                  result <- deriveEmptyKBody[F](caseClass)
                } yield result
              }

              import hearth.kindlings.catsderivation.internal.runtime.CatsDerivationFactories
              Expr.quote {
                CatsDerivationFactories.emptyKInstance[F] { () =>
                  Expr.splice(doEmpty).asInstanceOf[F[CatsDerivationFactories.W1]]
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
        infoRendering = if (shouldWeLogEmptyKDerivation) RenderFrom(Log.Level.Info) else DontRender,
        errorRendering = if (shouldWeLogEmptyKDerivation) RenderFrom(Log.Level.Info) else DontRender,
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
  private def deriveEmptyKBody[F[_]](
      caseClass: CaseClass[F[Any]]
  )(implicit FCtor: Type.Ctor1[F], FAnyType: Type[F[Any]], AnyType: Type[Any]): MIO[Expr[F[Any]]] = {
    val fields = caseClass.primaryConstructor.totalParameters.flatten.toList

    val fieldExprsMIO: MIO[List[(String, Expr_??)]] = fields.traverse { case (fieldName, param) =>
      import param.tpe.Underlying as Field

      EmptyKTypes.Empty[Field].summonExprIgnoring().toEither match {
        case Right(emptyExpr) =>
          val value: Expr[Field] = Expr.quote(Expr.splice(emptyExpr).empty)
          MIO.pure((fieldName, value.as_??))
        case Left(reason) =>
          failDerivation(
            CatsDerivationError.MissingInstanceForField(
              "Empty",
              fieldName,
              Type[F[Any]].prettyPrint,
              Some(s"${Field.prettyPrint}: $reason")
            )
          )
      }
    }

    fieldExprsMIO.flatMap { fieldExprs =>
      constructInstanceFree(caseClass.primaryConstructor, "Constructor", "empty result")(fieldExprs.toMap)
        .map(constructExpr => constructExpr.value.asInstanceOf[Expr[F[Any]]])
    }
  }

  protected object EmptyKTypes {
    val Any: Type[Any] = Type.of[Any]
    def Empty: Type.Ctor1[alleycats.Empty] = Type.Ctor1.of[alleycats.Empty]
    val LogDerivation: Type[hearth.kindlings.catsderivation.LogDerivation] =
      Type.of[hearth.kindlings.catsderivation.LogDerivation]
  }

  def shouldWeLogEmptyKDerivation: Boolean = {
    implicit val LogDerivationType: Type[LogDerivation] = EmptyKTypes.LogDerivation
    Expr.summonImplicit[LogDerivation].isDefined || (for {
      data <- Environment.typedSettings.toOption
      cd <- data.get("catsDerivation")
      shouldLog <- cd.get("logDerivation").flatMap(_.asBoolean)
    } yield shouldLog).getOrElse(false)
  }
}
