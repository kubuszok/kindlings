package hearth.kindlings.catsderivation.internal.compiletime

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

import hearth.kindlings.catsderivation.LogDerivation

/** NonEmptyAlternative derivation: Applicative (pure + ap + map) + SemigroupK (combineK).
  *
  * Uses an erased approach. For pure, direct fields are filled with the given value; invariant fields use Monoid.empty.
  * For ap, direct fields apply the function; invariant fields combine via Semigroup. For combineK, all fields are
  * combined via Semigroup.
  */
trait NonEmptyAlternativeMacrosImpl { this: MacroCommons & StdExtensions =>

  @scala.annotation.nowarn("msg=is never used|unused explicit parameter")
  def deriveNonEmptyAlternative[F[_]](
      FCtor0: Type.Ctor1[F],
      NEAFType: Type[cats.NonEmptyAlternative[F]]
  ): Expr[cats.NonEmptyAlternative[F]] = {
    val macroName = "NonEmptyAlternative.derived"

    implicit val FCtor: Type.Ctor1[F] = FCtor0
    implicit val NEAFT: Type[cats.NonEmptyAlternative[F]] = NEAFType
    implicit val AnyType: Type[Any] = NEATypes.Any
    implicit val FAnyType: Type[F[Any]] = FCtor.apply[Any]

    Log
      .namedScope(s"Deriving NonEmptyAlternative at: ${Environment.currentPosition.prettyPrint}") {
        CaseClass.parse[F[Any]].toEither match {
          case Right(caseClass) =>
            MIO.scoped { runSafe =>
              implicit val IntType: Type[Int] = NEATypes.Int
              implicit val StringType: Type[String] = NEATypes.String

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
                  s"Cannot derive NonEmptyAlternative: fields ${nestedFields.mkString(", ")} contain nested type constructors. " +
                    "Only direct type parameter fields (A) and invariant fields are supported."
                )
              }

              val directFieldSet: Set[String] = directFields.toSet

              // Pre-load extensions eagerly
              val _ = runSafe {
                Environment.loadStandardExtensions().toMIO(allowFailures = false)
              }

              val doPure: Expr[Any] => Expr[F[Any]] = aExpr =>
                runSafe {
                  deriveNEAPureBody[F](caseClass, directFieldSet, aExpr)
                }

              val doMap: (Expr[F[Any]], Expr[Any => Any]) => Expr[F[Any]] = (faExpr, fExpr) =>
                runSafe {
                  deriveNEAMapBody[F](caseClass, directFieldSet, faExpr, fExpr)
                }

              val doAp: (Expr[F[Any]], Expr[F[Any]]) => Expr[F[Any]] = (ffExpr, faExpr) =>
                runSafe {
                  deriveNEAApBody[F](caseClass, directFieldSet, ffExpr, faExpr)
                }

              val doCombineK: (Expr[F[Any]], Expr[F[Any]]) => Expr[F[Any]] = (xExpr, yExpr) =>
                runSafe {
                  deriveNEACombineKBody[F](caseClass, xExpr, yExpr)
                }

              Expr.quote {
                new cats.NonEmptyAlternative[F] {
                  def pure[A](a: A): F[A] = {
                    val anyA: Any = a.asInstanceOf[Any]
                    val _ = anyA
                    Expr.splice(doPure(Expr.quote(anyA))).asInstanceOf[F[A]]
                  }
                  override def map[A, B](fa: F[A])(f: A => B): F[B] = {
                    val anyFa: F[Any] = fa.asInstanceOf[F[Any]]
                    val anyF: Any => Any = f.asInstanceOf[Any => Any]
                    val _ = anyFa
                    val _ = anyF
                    Expr.splice(doMap(Expr.quote(anyFa), Expr.quote(anyF))).asInstanceOf[F[B]]
                  }
                  def ap[A, B](ff: F[A => B])(fa: F[A]): F[B] = {
                    val anyFf: F[Any] = ff.asInstanceOf[F[Any]]
                    val anyFa: F[Any] = fa.asInstanceOf[F[Any]]
                    val _ = anyFf
                    val _ = anyFa
                    Expr.splice(doAp(Expr.quote(anyFf), Expr.quote(anyFa))).asInstanceOf[F[B]]
                  }
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
        infoRendering = if (shouldWeLogNEADerivation) RenderFrom(Log.Level.Info) else DontRender,
        errorRendering = if (shouldWeLogNEADerivation) RenderFrom(Log.Level.Info) else DontRender
      ) { (errorLogs, errors) =>
        val errorsRendered = errors.map(e => "  - " + e.getMessage).mkString("\n")
        val hint =
          "Enable debug logging with: import hearth.kindlings.catsderivation.debug.logDerivationForCatsDerivation"
        if (errorLogs.nonEmpty) s"Macro derivation failed:\n$errorsRendered\nlogs:\n$errorLogs\n$hint"
        else s"Macro derivation failed:\n$errorsRendered\n$hint"
      }
  }

  @scala.annotation.nowarn("msg=is never used|unused explicit parameter|unused implicit parameter")
  private def deriveNEAPureBody[F[_]](
      caseClass: CaseClass[F[Any]],
      directFields: Set[String],
      aExpr: Expr[Any]
  )(implicit FCtor: Type.Ctor1[F], FAnyType: Type[F[Any]], AnyType: Type[Any]): MIO[Expr[F[Any]]] = {
    val fields = caseClass.primaryConstructor.parameters.flatten.toList

    val fieldExprs: List[(String, Expr_??)] = fields.map { case (fieldName, param) =>
      import param.tpe.Underlying as Field

      if (directFields.contains(fieldName)) {
        (fieldName, aExpr.as_??)
      } else {
        val monoidExpr = NEATypes.Monoid[Field].summonExprIgnoring().toEither match {
          case Right(m)     => m
          case Left(reason) =>
            throw new RuntimeException(
              s"No Monoid instance found for invariant field '$fieldName': ${Field.prettyPrint}: $reason"
            )
        }
        val emptyValue: Expr[Field] = Expr.quote(Expr.splice(monoidExpr).empty)
        (fieldName, emptyValue.as_??)
      }
    }

    caseClass.primaryConstructor(fieldExprs.toMap) match {
      case Right(constructExpr) => MIO.pure(constructExpr)
      case Left(error)          =>
        MIO.fail(new RuntimeException(s"Cannot construct pure result: $error"))
    }
  }

  @scala.annotation.nowarn("msg=is never used|unused implicit parameter")
  private def deriveNEAMapBody[F[_]](
      caseClass: CaseClass[F[Any]],
      directFields: Set[String],
      faExpr: Expr[F[Any]],
      fExpr: Expr[Any => Any]
  )(implicit FCtor: Type.Ctor1[F], FAnyType: Type[F[Any]], AnyType: Type[Any]): MIO[Expr[F[Any]]] = {
    val fields = caseClass.caseFieldValuesAt(faExpr).toList

    val mappedFields: List[(String, Expr_??)] = fields.map { case (fieldName, fieldValue) =>
      import fieldValue.Underlying as Field
      val fieldExpr = fieldValue.value.asInstanceOf[Expr[Field]]

      if (directFields.contains(fieldName)) {
        val mapped: Expr[Any] = Expr.quote(Expr.splice(fExpr)(Expr.splice(fieldExpr.upcast[Any])))
        (fieldName, mapped.as_??)
      } else {
        (fieldName, fieldExpr.as_??)
      }
    }

    caseClass.primaryConstructor(mappedFields.toMap) match {
      case Right(constructExpr) => MIO.pure(constructExpr)
      case Left(error)          =>
        MIO.fail(new RuntimeException(s"Cannot construct mapped result: $error"))
    }
  }

  @scala.annotation.nowarn("msg=is never used|unused implicit parameter")
  private def deriveNEAApBody[F[_]](
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
          val applied: Expr[Any] = Expr.quote(
            Expr.splice(ffField).asInstanceOf[Any => Any].apply(Expr.splice(faField).asInstanceOf[Any])
          )
          (fieldName, applied.as_??)
        } else {
          val sgExpr = NEATypes.Semigroup[Field].summonExprIgnoring().toEither match {
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

  @scala.annotation.nowarn("msg=is never used|unused implicit parameter")
  private def deriveNEACombineKBody[F[_]](
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

        val sgExpr = NEATypes.Semigroup[Field].summonExprIgnoring().toEither match {
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

  protected object NEATypes {
    val Any: Type[Any] = Type.of[Any]
    val Int: Type[Int] = Type.of[Int]
    val String: Type[String] = Type.of[String]
    def Semigroup: Type.Ctor1[cats.kernel.Semigroup] = Type.Ctor1.of[cats.kernel.Semigroup]
    def Monoid: Type.Ctor1[cats.kernel.Monoid] = Type.Ctor1.of[cats.kernel.Monoid]
    val LogDerivation: Type[hearth.kindlings.catsderivation.LogDerivation] =
      Type.of[hearth.kindlings.catsderivation.LogDerivation]
  }

  def shouldWeLogNEADerivation: Boolean = {
    implicit val LogDerivationType: Type[LogDerivation] = NEATypes.LogDerivation
    Expr.summonImplicit[LogDerivation].isDefined || (for {
      data <- Environment.typedSettings.toOption
      cd <- data.get("catsDerivation")
      shouldLog <- cd.get("logDerivation").flatMap(_.asBoolean)
    } yield shouldLog).getOrElse(false)
  }
}
