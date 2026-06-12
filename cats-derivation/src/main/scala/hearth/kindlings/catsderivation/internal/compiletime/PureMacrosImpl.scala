package hearth.kindlings.catsderivation.internal.compiletime

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.fp.instances.*
import hearth.fp.syntax.*
import hearth.std.*

import hearth.kindlings.catsderivation.LogDerivation

/** Pure derivation: constructs F[A] from a single value A.
  *
  * Uses free type variables A directly in the generated code, relying on Hearth 0.2.0-264+ cross-quotes support for
  * method-level type parameters inside Expr.quote/Expr.splice. All type-parameter-dependent fields are filled with the
  * provided value. Invariant fields cause a derivation error.
  */
trait PureMacrosImpl extends CatsDerivationTimeout with CatsDerivationErrorSupport {
  this: MacroCommons & StdExtensions =>

  @scala.annotation.nowarn("msg=is never used|unused explicit parameter")
  def derivePure[F[_]](FCtor0: Type.Ctor1[F], PureFType: Type[alleycats.Pure[F]]): Expr[alleycats.Pure[F]] = {
    val macroName = "Pure.derived"

    implicit val FCtor: Type.Ctor1[F] = FCtor0
    implicit val PureFT: Type[alleycats.Pure[F]] = PureFType

    Log
      .namedScope(s"Deriving Pure at: ${Environment.currentPosition.prettyPrint}") {
        MIO.scoped { runSafe =>
          implicit val IntType: Type[Int] = PureTypes.Int
          implicit val StringType: Type[String] = PureTypes.String

          val ccInt = runSafe(parseCaseClassMIO[F[Int]]("F[Int]")(using FCtor.apply[Int]))
          val ccString = runSafe(parseCaseClassMIO[F[String]]("F[String]")(using FCtor.apply[String]))

          val fieldsInt = ccInt.primaryConstructor.totalParameters.flatten.toList
          val fieldsString = ccString.primaryConstructor.totalParameters.flatten.toList

          val directFields = scala.collection.mutable.Set.empty[String]

          runSafe {
            fieldsInt
              .zip(fieldsString)
              .traverse { case ((name, pInt), (_, pString)) =>
                val tInt = pInt.tpe.Underlying
                val tString = pString.tpe.Underlying
                if (tInt =:= tString) {
                  // Invariant field — no value to fill
                  MIO.void
                } else if (tInt =:= IntType && tString =:= StringType) {
                  directFields += name
                  MIO.void
                } else {
                  failDerivation[Unit](
                    CatsDerivationError.UnsupportedFieldShape(
                      "Pure",
                      name,
                      "the field contains a nested type constructor - " +
                        "only direct type parameter fields (A) and invariant fields are supported, " +
                        "but invariant fields also require default values which are not yet supported"
                    )
                  )
                }
              }
              .void
          }

          // Check if there are any non-direct fields (invariant) — these need defaults we can't provide
          val allFieldNames = fieldsInt.map(_._1).toSet
          val invariantFields = allFieldNames -- directFields.toSet
          if (invariantFields.nonEmpty) {
            runSafe {
              failDerivation[Unit](
                CatsDerivationError.UnsupportedFieldShape(
                  "Pure",
                  invariantFields.mkString(", "),
                  "the fields are not of the type parameter type - " +
                    "Pure can only be derived when all fields use the type parameter directly"
                )
              )
            }
          }

          val directFieldSet: Set[String] = directFields.toSet

          // Pre-load extensions before entering the quote
          runSafe {
            Environment.loadStandardExtensions().toMIO(allowFailures = false).map(_ => ())
          }

          implicit val AnyType: Type[Any] = PureTypes.Any
          implicit val FAnyType: Type[F[Any]] = FCtor.apply[Any]

          val doPure: Expr[Any] => Expr[F[Any]] = (aExpr: Expr[Any]) =>
            runSafe {
              derivePureBody[F, Any](FCtor, directFieldSet, aExpr)(Type.of[Any])
            }

          import hearth.kindlings.catsderivation.internal.runtime.CatsDerivationFactories
          Expr.quote {
            CatsDerivationFactories.pureInstance[F] { (aAny: Any) =>
              val _ = aAny
              Expr.splice(doPure(Expr.quote(aAny))).asInstanceOf[F[CatsDerivationFactories.W1]]
            }
          }
        }
      }
      .flatTap(result => Log.info(s"Derived final result: ${result.prettyPrint}"))
      .runToExprOrFail(
        macroName,
        infoRendering = if (shouldWeLogPureDerivation) RenderFrom(Log.Level.Info) else DontRender,
        errorRendering = if (shouldWeLogPureDerivation) RenderFrom(Log.Level.Info) else DontRender,
        timeout = derivationTimeout
      ) { (errorLogs, errors) =>
        val errorsRendered = errors.map(e => "  - " + e.getMessage).mkString("\n")
        val hint =
          "Enable debug logging with: import hearth.kindlings.catsderivation.debug.logDerivationForCatsDerivation"
        if (errorLogs.nonEmpty) s"Macro derivation failed:\n$errorsRendered\nlogs:\n$errorLogs\n$hint"
        else s"Macro derivation failed:\n$errorsRendered\n$hint"
      }
  }

  @scala.annotation.nowarn("msg=is never used|unused explicit parameter|unused implicit parameter")
  private def derivePureBody[F[_], A](
      FCtor: Type.Ctor1[F],
      directFields: Set[String],
      aExpr: Expr[A]
  )(implicit AType: Type[A]): MIO[Expr[F[A]]] = {
    implicit val FAType: Type[F[A]] = FCtor.apply[A]

    parseCaseClassMIO[F[A]]("F[A]").flatMap { caseClass =>
      val fields = caseClass.primaryConstructor.totalParameters.flatten.toList

      val fieldMap: Map[String, Expr_??] = fields.map { case (fieldName, _) =>
        // All fields are direct (we verified this above), so all get `a`
        (fieldName, aExpr.as_??)
      }.toMap

      constructInstanceFree(caseClass.primaryConstructor, "Constructor", "pure result")(fieldMap)
        .map(constructExpr => constructExpr.value.asInstanceOf[Expr[F[A]]])
    }
  }

  protected object PureTypes {
    val Any: Type[Any] = Type.of[Any]
    val Int: Type[Int] = Type.of[Int]
    val String: Type[String] = Type.of[String]
    val LogDerivation: Type[hearth.kindlings.catsderivation.LogDerivation] =
      Type.of[hearth.kindlings.catsderivation.LogDerivation]
  }

  def shouldWeLogPureDerivation: Boolean = {
    implicit val LogDerivationType: Type[LogDerivation] = PureTypes.LogDerivation
    Expr.summonImplicit[LogDerivation].isDefined || (for {
      data <- Environment.typedSettings.toOption
      cd <- data.get("catsDerivation")
      shouldLog <- cd.get("logDerivation").flatMap(_.asBoolean)
    } yield shouldLog).getOrElse(false)
  }
}
