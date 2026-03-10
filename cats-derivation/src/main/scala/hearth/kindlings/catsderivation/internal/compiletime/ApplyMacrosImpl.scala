package hearth.kindlings.catsderivation.internal.compiletime

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

import hearth.kindlings.catsderivation.LogDerivation

/** Apply derivation: Functor map + ap (apply function fields to value fields).
  *
  * Uses free type variables A and B directly in the generated code for map, relying on Hearth 0.2.0-264+ cross-quotes
  * support for method-level type parameters inside Expr.quote/Expr.splice. For ap, uses an erased approach since both
  * ff: F[A => B] and fa: F[A] must be treated as F[Any] for field extraction.
  */
trait ApplyMacrosImpl { this: MacroCommons & StdExtensions =>

  @scala.annotation.nowarn("msg=is never used|unused explicit parameter")
  def deriveApply[F[_]](FCtor0: Type.Ctor1[F], ApplyFType: Type[cats.Apply[F]]): Expr[cats.Apply[F]] = {
    val macroName = "Apply.derived"

    implicit val FCtor: Type.Ctor1[F] = FCtor0
    implicit val ApplyFT: Type[cats.Apply[F]] = ApplyFType

    Log
      .namedScope(s"Deriving Apply at: ${Environment.currentPosition.prettyPrint}") {
        MIO.scoped { runSafe =>
          implicit val IntType: Type[Int] = ApplyTypes.Int
          implicit val StringType: Type[String] = ApplyTypes.String

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
          val nestedFields = scala.collection.mutable.ListBuffer.empty[String]

          fieldsInt.zip(fieldsString).foreach { case ((name, pInt), (_, pString)) =>
            val tInt = pInt.tpe.Underlying
            val tString = pString.tpe.Underlying
            if (tInt =:= IntType && tString =:= StringType) {
              directFields += name
            } else if (tInt =:= tString) {
              // Invariant
            } else {
              nestedFields += name
            }
          }

          if (nestedFields.nonEmpty) {
            throw new RuntimeException(
              s"Cannot derive Apply: fields ${nestedFields.mkString(", ")} contain nested type constructors. " +
                "Only direct type parameter fields (A) and invariant fields are supported."
            )
          }

          val directFieldSet: Set[String] = directFields.toSet

          // Pre-load extensions before entering the quote
          runSafe {
            Environment.loadStandardExtensions().toMIO(allowFailures = false).map(_ => ())
          }

          // ap still needs the erased approach: ff: F[A => B] and fa: F[A] both need F[Any] for field extraction
          implicit val AnyType: Type[Any] = ApplyTypes.Any
          implicit val FAnyType: Type[F[Any]] = FCtor.apply[Any]

          val caseClass = CaseClass.parse[F[Any]].toEither match {
            case Right(cc) => cc
            case Left(e)   => throw new RuntimeException(s"Cannot parse F[Any]: $e")
          }

          val doAp: (Expr[F[Any]], Expr[F[Any]]) => Expr[F[Any]] = (ffExpr, faExpr) =>
            runSafe {
              deriveApplyApBody[F](caseClass, directFieldSet, ffExpr, faExpr)
            }

          Expr.quote {
            new cats.Apply[F] {
              def map[A, B](fa: F[A])(f: A => B): F[B] =
                Expr.splice {
                  runSafe {
                    deriveApplyMapBody[F, A, B](FCtor, directFieldSet, Expr.quote(fa), Expr.quote(f))(
                      Type.of[A],
                      Type.of[B]
                    )
                  }
                }
              def ap[A, B](ff: F[A => B])(fa: F[A]): F[B] = {
                val anyFf: F[Any] = ff.asInstanceOf[F[Any]]
                val anyFa: F[Any] = fa.asInstanceOf[F[Any]]
                val _ = anyFf
                val _ = anyFa
                Expr.splice(doAp(Expr.quote(anyFf), Expr.quote(anyFa))).asInstanceOf[F[B]]
              }
            }
          }
        }
      }
      .flatTap(result => Log.info(s"Derived final result: ${result.prettyPrint}"))
      .runToExprOrFail(
        macroName,
        infoRendering = if (shouldWeLogApplyDerivation) RenderFrom(Log.Level.Info) else DontRender,
        errorRendering = if (shouldWeLogApplyDerivation) RenderFrom(Log.Level.Info) else DontRender
      ) { (errorLogs, errors) =>
        val errorsRendered = errors.map(e => "  - " + e.getMessage).mkString("\n")
        val hint =
          "Enable debug logging with: import hearth.kindlings.catsderivation.debug.logDerivationForCatsDerivation"
        if (errorLogs.nonEmpty) s"Macro derivation failed:\n$errorsRendered\nlogs:\n$errorLogs\n$hint"
        else s"Macro derivation failed:\n$errorsRendered\n$hint"
      }
  }

  @scala.annotation.nowarn("msg=is never used|unused implicit parameter")
  private def deriveApplyMapBody[F[_], A, B](
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

  @scala.annotation.nowarn("msg=is never used|unused implicit parameter")
  private def deriveApplyApBody[F[_]](
      caseClass: CaseClass[F[Any]],
      directFields: Set[String],
      ffExpr: Expr[F[Any]],
      faExpr: Expr[F[Any]]
  )(implicit FCtor: Type.Ctor1[F], FAnyType: Type[F[Any]], AnyType: Type[Any]): MIO[Expr[F[Any]]] = {
    val fieldsFF = caseClass.caseFieldValuesAt(ffExpr).toList
    val fieldsFA = caseClass.caseFieldValuesAt(faExpr).toList

    val apFields: List[(String, Expr_??)] =
      fieldsFF.zip(fieldsFA).map { case ((fieldName, ffFieldValue), (_, faFieldValue)) =>
        import ffFieldValue.Underlying as Field
        val ffField = ffFieldValue.value.asInstanceOf[Expr[Field]]
        val faField = faFieldValue.value.asInstanceOf[Expr[Field]]

        if (directFields.contains(fieldName)) {
          // Direct field: ff's value is a function (A => B at runtime), apply to fa's value
          val applied: Expr[Any] = Expr.quote(
            Expr.splice(ffField).asInstanceOf[Any => Any].apply(Expr.splice(faField).asInstanceOf[Any])
          )
          (fieldName, applied.as_??)
        } else {
          // Invariant field: combine using Semigroup
          val sgExpr = ApplyTypes.Semigroup[Field].summonExprIgnoring().toEither match {
            case Right(sg)    => sg
            case Left(reason) =>
              throw new RuntimeException(
                s"No Semigroup instance found for invariant field '$fieldName': ${Field.prettyPrint}: $reason"
              )
          }
          val combined: Expr[Field] = Expr.quote(
            Expr.splice(sgExpr).combine(Expr.splice(ffField), Expr.splice(faField))
          )
          (fieldName, combined.as_??)
        }
      }

    caseClass.primaryConstructor(apFields.toMap) match {
      case Right(constructExpr) => MIO.pure(constructExpr)
      case Left(error)          =>
        MIO.fail(new RuntimeException(s"Cannot construct ap result: $error"))
    }
  }

  protected object ApplyTypes {
    val Any: Type[Any] = Type.of[Any]
    val Int: Type[Int] = Type.of[Int]
    val String: Type[String] = Type.of[String]
    def Semigroup: Type.Ctor1[cats.kernel.Semigroup] = Type.Ctor1.of[cats.kernel.Semigroup]
    val LogDerivation: Type[hearth.kindlings.catsderivation.LogDerivation] =
      Type.of[hearth.kindlings.catsderivation.LogDerivation]
  }

  def shouldWeLogApplyDerivation: Boolean = {
    implicit val LogDerivationType: Type[LogDerivation] = ApplyTypes.LogDerivation
    Expr.summonImplicit[LogDerivation].isDefined || (for {
      data <- Environment.typedSettings.toOption
      cd <- data.get("catsDerivation")
      shouldLog <- cd.get("logDerivation").flatMap(_.asBoolean)
    } yield shouldLog).getOrElse(false)
  }
}
