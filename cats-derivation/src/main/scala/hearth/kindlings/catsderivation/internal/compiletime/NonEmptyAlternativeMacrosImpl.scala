package hearth.kindlings.catsderivation.internal.compiletime

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

import hearth.kindlings.catsderivation.LogDerivation

/** NonEmptyAlternative derivation: Applicative (pure + ap + map) + SemigroupK (combineK).
  *
  * Uses free type variables for pure and map methods, relying on Hearth 0.2.0-264+ cross-quotes support for
  * method-level type parameters inside Expr.quote/Expr.splice.
  *
  * Keeps erased approach (F[Any]) for:
  *   - ap[A, B]: needs F[Any] erasure because ff: F[A => B] and fa: F[A] have different type parameters
  *   - combineK[A]: summons Semigroup for field types at macro time, needs concrete types
  */
trait NonEmptyAlternativeMacrosImpl extends CatsDerivationTimeout { this: MacroCommons & StdExtensions =>

  @scala.annotation.nowarn("msg=is never used|unused explicit parameter")
  def deriveNonEmptyAlternative[F[_]](
      FCtor0: Type.Ctor1[F],
      NEAFType: Type[cats.NonEmptyAlternative[F]]
  ): Expr[cats.NonEmptyAlternative[F]] = {
    val macroName = "NonEmptyAlternative.derived"

    implicit val FCtor: Type.Ctor1[F] = FCtor0
    implicit val NEAFT: Type[cats.NonEmptyAlternative[F]] = NEAFType

    Log
      .namedScope(s"Deriving NonEmptyAlternative at: ${Environment.currentPosition.prettyPrint}") {
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

          val fieldsInt = ccInt.primaryConstructor.totalParameters.flatten.toList
          val fieldsString = ccString.primaryConstructor.totalParameters.flatten.toList

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

          // Pre-load extensions before entering the quote
          runSafe {
            Environment.loadStandardExtensions().toMIO(allowFailures = false).map(_ => ())
          }

          // Erased types needed by ap and combineK
          implicit val AnyType: Type[Any] = NEATypes.Any
          implicit val FAnyType: Type[F[Any]] = FCtor.apply[Any]

          val caseClassAny = CaseClass.parse[F[Any]].toEither match {
            case Right(cc) => cc
            case Left(e)   => throw new RuntimeException(s"Cannot parse F[Any]: $e")
          }

          val doAp: (Expr[F[Any]], Expr[F[Any]]) => Expr[F[Any]] = (ffExpr, faExpr) =>
            runSafe {
              deriveNEAApBody[F](caseClassAny, directFieldSet, ffExpr, faExpr)
            }

          val doCombineK: (Expr[F[Any]], Expr[F[Any]]) => Expr[F[Any]] = (xExpr, yExpr) =>
            runSafe {
              deriveNEACombineKBody[F](caseClassAny, xExpr, yExpr)
            }

          val doPure: Expr[Any] => Expr[F[Any]] =
            (aExpr: Expr[Any]) => runSafe(deriveNEAPureBody[F, Any](FCtor, directFieldSet, aExpr)(AnyType))

          import hearth.kindlings.catsderivation.internal.runtime.CatsDerivationFactories
          Expr.quote {
            CatsDerivationFactories.nonEmptyAlternativeInstance[F](
              pureFn = { (aAny: Any) =>
                val _ = aAny
                Expr.splice(doPure(Expr.quote(aAny))).asInstanceOf[F[CatsDerivationFactories.W1]]
              },
              mapFn = {
                (fa: F[CatsDerivationFactories.W1], f: CatsDerivationFactories.W1 => CatsDerivationFactories.W2) =>
                  val _ = fa
                  val _ = f
                  Expr.splice {
                    runSafe {
                      deriveNEAMapBody[F, CatsDerivationFactories.W1, CatsDerivationFactories.W2](
                        FCtor,
                        directFieldSet,
                        Expr.quote(fa),
                        Expr.quote(f)
                      )(
                        Type.of[CatsDerivationFactories.W1],
                        Type.of[CatsDerivationFactories.W2]
                      )
                    }
                  }
              },
              apFn = {
                (ff: F[CatsDerivationFactories.W1 => CatsDerivationFactories.W2], fa: F[CatsDerivationFactories.W1]) =>
                  val anyFf: F[Any] = ff.asInstanceOf[F[Any]]
                  val anyFa: F[Any] = fa.asInstanceOf[F[Any]]
                  val _ = anyFf
                  val _ = anyFa
                  Expr.splice(doAp(Expr.quote(anyFf), Expr.quote(anyFa))).asInstanceOf[F[CatsDerivationFactories.W2]]
              },
              combineKFn = { (x: F[CatsDerivationFactories.W1], y: F[CatsDerivationFactories.W1]) =>
                val anyX: F[Any] = x.asInstanceOf[F[Any]]
                val anyY: F[Any] = y.asInstanceOf[F[Any]]
                val _ = anyX
                val _ = anyY
                Expr
                  .splice(doCombineK(Expr.quote(anyX), Expr.quote(anyY)))
                  .asInstanceOf[F[CatsDerivationFactories.W1]]
              }
            )
          }
        }
      }
      .flatTap(result => Log.info(s"Derived final result: ${result.prettyPrint}"))
      .runToExprOrFail(
        macroName,
        infoRendering = if (shouldWeLogNEADerivation) RenderFrom(Log.Level.Info) else DontRender,
        errorRendering = if (shouldWeLogNEADerivation) RenderFrom(Log.Level.Info) else DontRender,
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
  private def deriveNEAPureBody[F[_], A](
      FCtor: Type.Ctor1[F],
      directFields: Set[String],
      aExpr: Expr[A]
  )(implicit AType: Type[A]): MIO[Expr[F[A]]] = {
    implicit val FAType: Type[F[A]] = FCtor.apply[A]

    val caseClass = CaseClass.parse[F[A]].toEither match {
      case Right(cc) => cc
      case Left(e)   => throw new RuntimeException(s"Cannot parse F[A]: $e")
    }
    val fields = caseClass.primaryConstructor.totalParameters.flatten.toList

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

    caseClass.primaryConstructor.fold(
      onInstance = _ => throw new RuntimeException("Constructor should not need instance"),
      onTypes = _ => Map.empty,
      onValues = _ => fieldExprs.toMap
    ) match {
      case Right(constructExpr) =>
        MIO.pure(constructExpr.value.asInstanceOf[Expr[F[A]]])
      case Left(error) =>
        MIO.fail(new RuntimeException(s"Cannot construct pure result: $error"))
    }
  }

  @scala.annotation.nowarn("msg=is never used|unused implicit parameter")
  private def deriveNEAMapBody[F[_], A, B](
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
    caseClassB.primaryConstructor.fold(
      onInstance = _ => throw new RuntimeException("Constructor should not need instance"),
      onTypes = _ => Map.empty,
      onValues = _ => mappedFields.toMap
    ) match {
      case Right(constructExpr) =>
        MIO.pure(constructExpr.value.asInstanceOf[Expr[F[B]]])
      case Left(error) =>
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

    caseClass.primaryConstructor.fold(
      onInstance = _ => throw new RuntimeException("Constructor should not need instance"),
      onTypes = _ => Map.empty,
      onValues = _ => apFields.toMap
    ) match {
      case Right(constructExpr) =>
        MIO.pure(constructExpr.value.asInstanceOf[Expr[F[Any]]])
      case Left(error) =>
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

    caseClass.primaryConstructor.fold(
      onInstance = _ => throw new RuntimeException("Constructor should not need instance"),
      onTypes = _ => Map.empty,
      onValues = _ => combinedFields.toMap
    ) match {
      case Right(constructExpr) =>
        MIO.pure(constructExpr.value.asInstanceOf[Expr[F[Any]]])
      case Left(error) =>
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
