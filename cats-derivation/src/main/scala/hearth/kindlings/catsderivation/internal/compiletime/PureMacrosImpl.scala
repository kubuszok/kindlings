package hearth.kindlings.catsderivation.internal.compiletime

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

import hearth.kindlings.catsderivation.LogDerivation

/** Pure derivation: constructs F[A] from a single value A.
  *
  * Uses free type variables A directly in the generated code, relying on Hearth 0.2.0-264+ cross-quotes support for
  * method-level type parameters inside Expr.quote/Expr.splice. All type-parameter-dependent fields are filled with the
  * provided value. Invariant fields cause a derivation error.
  */
trait PureMacrosImpl extends CatsDerivationTimeout { this: MacroCommons & StdExtensions =>

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

          val ccInt = CaseClass.parse(using FCtor.apply[Int]).toEither match {
            case Right(cc) => cc
            case Left(e)   => throw new RuntimeException(s"Cannot parse F[Int]: $e")
          }
          val ccString = CaseClass.parse(using FCtor.apply[String]).toEither match {
            case Right(cc) => cc
            case Left(e)   => throw new RuntimeException(s"Cannot parse F[String]: $e")
          }

          val fieldsInt = ccInt.primaryConstructor.parameters.flatten.toList
          val fieldsString = ccString.primaryConstructor.parameters.flatten.toList

          val directFields = scala.collection.mutable.Set.empty[String]

          fieldsInt.zip(fieldsString).foreach { case ((name, pInt), (_, pString)) =>
            val tInt = pInt.tpe.Underlying
            val tString = pString.tpe.Underlying
            if (tInt =:= tString) {
              // Invariant field — no value to fill
            } else if (tInt =:= IntType && tString =:= StringType) {
              directFields += name
            } else {
              throw new RuntimeException(
                s"Cannot derive Pure: field '$name' contains a nested type constructor. " +
                  "Only direct type parameter fields (A) and invariant fields are supported, " +
                  "but invariant fields also require default values which are not yet supported."
              )
            }
          }

          // Check if there are any non-direct fields (invariant) — these need defaults we can't provide
          val allFieldNames = fieldsInt.map(_._1).toSet
          val invariantFields = allFieldNames -- directFields.toSet
          if (invariantFields.nonEmpty) {
            throw new RuntimeException(
              s"Cannot derive Pure: fields ${invariantFields.mkString(", ")} are not of the type parameter type. " +
                "Pure can only be derived when all fields use the type parameter directly."
            )
          }

          val directFieldSet: Set[String] = directFields.toSet

          // Pre-load extensions before entering the quote
          runSafe {
            Environment.loadStandardExtensions().toMIO(allowFailures = false).map(_ => ())
          }

          Expr.quote {
            new alleycats.Pure[F] {
              def pure[A](a: A): F[A] =
                Expr.splice {
                  runSafe {
                    derivePureBody[F, A](FCtor, directFieldSet, Expr.quote(a))(
                      Type.of[A]
                    )
                  }
                }
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

    val caseClass = CaseClass.parse[F[A]].toEither match {
      case Right(cc) => cc
      case Left(e)   => throw new RuntimeException(s"Cannot parse F[A]: $e")
    }
    val fields = caseClass.primaryConstructor.parameters.flatten.toList

    val fieldMap: Map[String, Expr_??] = fields.map { case (fieldName, _) =>
      // All fields are direct (we verified this above), so all get `a`
      (fieldName, aExpr.as_??)
    }.toMap

    caseClass.primaryConstructor(fieldMap) match {
      case Right(constructExpr) => MIO.pure(constructExpr)
      case Left(error)          =>
        MIO.fail(new RuntimeException(s"Cannot construct pure result: $error"))
    }
  }

  protected object PureTypes {
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
