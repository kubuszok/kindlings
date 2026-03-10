package hearth.kindlings.catsderivation.internal.compiletime

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

import hearth.kindlings.catsderivation.LogDerivation

/** Invariant derivation: imaps over type parameter fields in case classes.
  *
  * Uses free type variables A and B directly in the generated code, relying on Hearth 0.2.0-264+ cross-quotes support
  * for method-level type parameters inside Expr.quote/Expr.splice. Supports direct A fields (covariant), Function1[A,
  * R] fields (contravariant), Function1[R, A] fields (covariant), and Function1[A, A] fields (both).
  */
trait InvariantMacrosImpl { this: MacroCommons & StdExtensions =>

  @scala.annotation.nowarn("msg=is never used|unused explicit parameter")
  def deriveInvariant[F[_]](
      FCtor0: Type.Ctor1[F],
      InvariantFType: Type[cats.Invariant[F]]
  ): Expr[cats.Invariant[F]] = {
    val macroName = "Invariant.derived"

    implicit val FCtor: Type.Ctor1[F] = FCtor0
    implicit val InvariantFT: Type[cats.Invariant[F]] = InvariantFType

    Log
      .namedScope(s"Deriving Invariant at: ${Environment.currentPosition.prettyPrint}") {
        MIO.scoped { runSafe =>
          implicit val IntType: Type[Int] = InvariantTypes.Int
          implicit val StringType: Type[String] = InvariantTypes.String

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

          // Field classification using string tags
          val fieldKinds = scala.collection.mutable.Map.empty[String, String]

          val Function1Ctor2 = {
            val impl = Type.Ctor2.of[Function1]
            Type.Ctor2.fromUntyped[Function1](impl.asUntyped)
          }

          fieldsInt.zip(fieldsString).foreach { case ((name, pInt), (_, pString)) =>
            val tInt = pInt.tpe.Underlying
            val tString = pString.tpe.Underlying
            if (tInt =:= tString) {
              fieldKinds(name) = "invariant"
            } else if (tInt =:= IntType && tString =:= StringType) {
              fieldKinds(name) = "direct"
            } else {
              (Function1Ctor2.unapply(tInt), Function1Ctor2.unapply(tString)) match {
                case (Some((arg1Int, arg2Int)), Some((arg1String, arg2String))) =>
                  import arg1Int.Underlying as A1Int
                  import arg2Int.Underlying as A2Int
                  import arg1String.Underlying as A1String
                  import arg2String.Underlying as A2String

                  val firstArgChanges = !(A1Int =:= A1String)
                  val secondArgChanges = !(A2Int =:= A2String)

                  if (firstArgChanges && !secondArgChanges) {
                    fieldKinds(name) = "fn1Contra"
                  } else if (!firstArgChanges && secondArgChanges) {
                    fieldKinds(name) = "fn1Covar"
                  } else if (firstArgChanges && secondArgChanges) {
                    fieldKinds(name) = "fn1Both"
                  } else {
                    throw new RuntimeException(
                      s"Cannot derive Invariant: field '$name' has inconsistent Function1 decomposition."
                    )
                  }
                case _ =>
                  throw new RuntimeException(
                    s"Cannot derive Invariant: field '$name' contains a nested type constructor that is not Function1. " +
                      "Only direct A fields, Function1[A, R] fields, and invariant fields are currently supported."
                  )
              }
            }
          }

          val fieldKindsMap: Map[String, String] = fieldKinds.toMap

          // Pre-load extensions before entering the quote
          runSafe {
            Environment.loadStandardExtensions().toMIO(allowFailures = false).map(_ => ())
          }

          Expr.quote {
            new cats.Invariant[F] {
              def imap[A, B](fa: F[A])(f: A => B)(g: B => A): F[B] =
                Expr.splice {
                  runSafe {
                    deriveImapBody[F, A, B](FCtor, fieldKindsMap, Expr.quote(fa), Expr.quote(f), Expr.quote(g))(
                      Type.of[A],
                      Type.of[B]
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
        infoRendering = if (shouldWeLogInvariantDerivation) RenderFrom(Log.Level.Info) else DontRender,
        errorRendering = if (shouldWeLogInvariantDerivation) RenderFrom(Log.Level.Info) else DontRender
      ) { (errorLogs, errors) =>
        val errorsRendered = errors.map(e => "  - " + e.getMessage).mkString("\n")
        val hint =
          "Enable debug logging with: import hearth.kindlings.catsderivation.debug.logDerivationForCatsDerivation"
        if (errorLogs.nonEmpty) s"Macro derivation failed:\n$errorsRendered\nlogs:\n$errorLogs\n$hint"
        else s"Macro derivation failed:\n$errorsRendered\n$hint"
      }
  }

  @scala.annotation.nowarn("msg=is never used|unused implicit parameter")
  private def deriveImapBody[F[_], A, B](
      FCtor: Type.Ctor1[F],
      fieldKinds: Map[String, String],
      faExpr: Expr[F[A]],
      fExpr: Expr[A => B],
      gExpr: Expr[B => A]
  )(implicit AType: Type[A], BType: Type[B]): MIO[Expr[F[B]]] = {
    implicit val FAType: Type[F[A]] = FCtor.apply[A]
    implicit val FBType: Type[F[B]] = FCtor.apply[B]

    val caseClass = CaseClass.parse[F[A]].toEither match {
      case Right(cc) => cc
      case Left(e)   => throw new RuntimeException(s"Cannot parse F[A]: $e")
    }
    val caseClassB = CaseClass.parse[F[B]].toEither match {
      case Right(cc) => cc
      case Left(e)   => throw new RuntimeException(s"Cannot parse F[B]: $e")
    }

    val fields = caseClass.caseFieldValuesAt(faExpr).toList
    val fieldsB = caseClassB.primaryConstructor.parameters.flatten.toList
    val fieldBTypes: Map[String, Type[Any]] =
      fieldsB.map { case (name, param) => (name, param.tpe.Underlying.asInstanceOf[Type[Any]]) }.toMap

    val mappedFields: List[(String, Expr_??)] = fields.map { case (fieldName, fieldValue) =>
      import fieldValue.Underlying as Field
      val fieldExpr = fieldValue.value.asInstanceOf[Expr[Field]]

      fieldKinds.getOrElse(fieldName, "invariant") match {
        case "direct" =>
          val mapped: Expr[B] = Expr.quote(Expr.splice(fExpr)(Expr.splice(fieldExpr.asInstanceOf[Expr[A]])))
          (fieldName, mapped.as_??)
        case "fn1Contra" =>
          val bFieldType = fieldBTypes(fieldName).asInstanceOf[Type[Field]]
          val composed: Expr[Field] =
            mkComposeExpr[Field](fieldExpr.asInstanceOf[Expr[Any]], gExpr.asInstanceOf[Expr[Any]])(bFieldType)
          (fieldName, composed.as_??(bFieldType))
        case "fn1Covar" =>
          val bFieldType = fieldBTypes(fieldName).asInstanceOf[Type[Field]]
          val composed: Expr[Field] =
            mkAndThenExpr[Field](fieldExpr.asInstanceOf[Expr[Any]], fExpr.asInstanceOf[Expr[Any]])(bFieldType)
          (fieldName, composed.as_??(bFieldType))
        case "fn1Both" =>
          val bFieldType = fieldBTypes(fieldName).asInstanceOf[Type[Field]]
          val composed: Expr[Field] = mkComposeAndThenExpr[Field](
            fieldExpr.asInstanceOf[Expr[Any]],
            fExpr.asInstanceOf[Expr[Any]],
            gExpr.asInstanceOf[Expr[Any]]
          )(bFieldType)
          (fieldName, composed.as_??(bFieldType))
        case _ =>
          (fieldName, fieldExpr.as_??)
      }
    }

    caseClassB.primaryConstructor(mappedFields.toMap) match {
      case Right(constructExpr) => MIO.pure(constructExpr)
      case Left(error)          =>
        MIO.fail(new RuntimeException(s"Cannot construct imapped result: $error"))
    }
  }

  @scala.annotation.nowarn("msg=is never used|unused implicit parameter")
  private def mkComposeExpr[Result](
      fieldExpr: Expr[Any],
      gExpr: Expr[Any]
  )(implicit ResultType: Type[Result]): Expr[Result] =
    Expr.quote {
      Expr
        .splice(fieldExpr)
        .asInstanceOf[Function1[Any, Any]]
        .compose(Expr.splice(gExpr).asInstanceOf[Any => Any])
        .asInstanceOf[Result]
    }

  @scala.annotation.nowarn("msg=is never used|unused implicit parameter")
  private def mkAndThenExpr[Result](
      fieldExpr: Expr[Any],
      fExpr: Expr[Any]
  )(implicit ResultType: Type[Result]): Expr[Result] =
    Expr.quote {
      Expr
        .splice(fieldExpr)
        .asInstanceOf[Function1[Any, Any]]
        .andThen(Expr.splice(fExpr).asInstanceOf[Any => Any])
        .asInstanceOf[Result]
    }

  @scala.annotation.nowarn("msg=is never used|unused implicit parameter")
  private def mkComposeAndThenExpr[Result](
      fieldExpr: Expr[Any],
      fExpr: Expr[Any],
      gExpr: Expr[Any]
  )(implicit ResultType: Type[Result]): Expr[Result] =
    Expr.quote {
      Expr
        .splice(fieldExpr)
        .asInstanceOf[Function1[Any, Any]]
        .compose(Expr.splice(gExpr).asInstanceOf[Any => Any])
        .andThen(Expr.splice(fExpr).asInstanceOf[Any => Any])
        .asInstanceOf[Result]
    }

  protected object InvariantTypes {
    val Int: Type[Int] = Type.of[Int]
    val String: Type[String] = Type.of[String]
    val LogDerivation: Type[hearth.kindlings.catsderivation.LogDerivation] =
      Type.of[hearth.kindlings.catsderivation.LogDerivation]
  }

  def shouldWeLogInvariantDerivation: Boolean = {
    implicit val LogDerivationType: Type[LogDerivation] = InvariantTypes.LogDerivation
    Expr.summonImplicit[LogDerivation].isDefined || (for {
      data <- Environment.typedSettings.toOption
      cd <- data.get("catsDerivation")
      shouldLog <- cd.get("logDerivation").flatMap(_.asBoolean)
    } yield shouldLog).getOrElse(false)
  }
}
