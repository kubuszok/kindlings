package hearth.kindlings.catsderivation.internal.compiletime

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

import hearth.kindlings.catsderivation.LogDerivation

/** Reducible derivation: reduceLeftTo + reduceRightTo + foldLeft + foldRight over direct type parameter fields.
  *
  * Uses free type variables A and B directly in the generated code, relying on Hearth 0.2.0-264+ cross-quotes support
  * for method-level type parameters inside Expr.quote/Expr.splice. All four methods (reduceLeftTo, reduceRightTo,
  * foldLeft, foldRight) use free types since they return B / Eval[B] (not F[B]) and don't need F[Any] erasure.
  *
  * Requires at least one direct field (non-empty guarantee).
  */
trait ReducibleMacrosImpl extends CatsDerivationTimeout { this: MacroCommons & StdExtensions =>

  @scala.annotation.nowarn("msg=is never used|unused explicit parameter")
  def deriveReducible[F[_]](
      FCtor0: Type.Ctor1[F],
      ReducibleFType: Type[cats.Reducible[F]]
  ): Expr[cats.Reducible[F]] = {
    val macroName = "Reducible.derived"

    implicit val FCtor: Type.Ctor1[F] = FCtor0
    implicit val ReducibleFT: Type[cats.Reducible[F]] = ReducibleFType

    Log
      .namedScope(s"Deriving Reducible at: ${Environment.currentPosition.prettyPrint}") {
        MIO.scoped { runSafe =>
          implicit val IntType: Type[Int] = ReducibleTypes.Int
          implicit val StringType: Type[String] = ReducibleTypes.String

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
              // Invariant — skip in reduce
            } else {
              nestedFields += name
            }
          }

          if (nestedFields.nonEmpty) {
            throw new RuntimeException(
              s"Cannot derive Reducible: fields ${nestedFields.mkString(", ")} contain nested type constructors. " +
                "Only direct type parameter fields (A) and invariant fields are supported."
            )
          }

          if (directFields.isEmpty) {
            throw new RuntimeException(
              "Cannot derive Reducible: no direct type parameter fields found. " +
                "Reducible requires at least one field of the type parameter."
            )
          }

          val directFieldSet: Set[String] = directFields.toSet

          // Pre-load extensions before entering the quote
          runSafe {
            Environment.loadStandardExtensions().toMIO(allowFailures = false).map(_ => ())
          }

          Expr.quote {
            new cats.Reducible[F] {
              def reduceLeftTo[A, B](fa: F[A])(f: A => B)(g: (B, A) => B): B =
                Expr.splice {
                  runSafe {
                    deriveReduceLeftToBody[F, A, B](
                      FCtor,
                      directFieldSet,
                      Expr.quote(fa),
                      Expr.quote(f),
                      Expr.quote(g)
                    )(
                      Type.of[A],
                      Type.of[B]
                    )
                  }
                }

              def reduceRightTo[A, B](fa: F[A])(f: A => B)(
                  g: (A, cats.Eval[B]) => cats.Eval[B]
              ): cats.Eval[B] =
                Expr.splice {
                  runSafe {
                    deriveReduceRightToBody[F, A, B](
                      FCtor,
                      directFieldSet,
                      Expr.quote(fa),
                      Expr.quote(f),
                      Expr.quote(g)
                    )(
                      Type.of[A],
                      Type.of[B]
                    )
                  }
                }

              def foldLeft[A, B](fa: F[A], b: B)(f: (B, A) => B): B =
                Expr.splice {
                  runSafe {
                    deriveReducibleFoldLeftBody[F, A, B](
                      FCtor,
                      directFieldSet,
                      Expr.quote(fa),
                      Expr.quote(b),
                      Expr.quote(f)
                    )(
                      Type.of[A],
                      Type.of[B]
                    )
                  }
                }

              def foldRight[A, B](fa: F[A], lb: cats.Eval[B])(
                  f: (A, cats.Eval[B]) => cats.Eval[B]
              ): cats.Eval[B] =
                Expr.splice {
                  runSafe {
                    deriveReducibleFoldRightBody[F, A, B](
                      FCtor,
                      directFieldSet,
                      Expr.quote(fa),
                      Expr.quote(lb),
                      Expr.quote(f)
                    )(
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
        infoRendering = if (shouldWeLogReducibleDerivation) RenderFrom(Log.Level.Info) else DontRender,
        errorRendering = if (shouldWeLogReducibleDerivation) RenderFrom(Log.Level.Info) else DontRender,
        timeout = derivationTimeout
      ) { (errorLogs, errors) =>
        val errorsRendered = errors.map(e => "  - " + e.getMessage).mkString("\n")
        val hint =
          "Enable debug logging with: import hearth.kindlings.catsderivation.debug.logDerivationForCatsDerivation"
        if (errorLogs.nonEmpty) s"Macro derivation failed:\n$errorsRendered\nlogs:\n$errorLogs\n$hint"
        else s"Macro derivation failed:\n$errorsRendered\n$hint"
      }
  }

  /** reduceLeftTo: map first direct field with f, fold rest with g. Produces: g(g(f(field1), field2), field3) */
  @scala.annotation.nowarn("msg=is never used|unused implicit parameter")
  private def deriveReduceLeftToBody[F[_], A, B](
      FCtor: Type.Ctor1[F],
      directFields: Set[String],
      faExpr: Expr[F[A]],
      fExpr: Expr[A => B],
      gExpr: Expr[(B, A) => B]
  )(implicit AType: Type[A], BType: Type[B]): MIO[Expr[B]] = {
    implicit val FAType: Type[F[A]] = FCtor.apply[A]

    val caseClass = CaseClass.parse[F[A]].toEither match {
      case Right(cc) => cc
      case Left(e)   => throw new RuntimeException(s"Cannot parse F[A]: $e")
    }
    val fields = caseClass.caseFieldValuesAt(faExpr).toList

    val directFieldExprs: List[Expr[A]] = fields.collect {
      case (fieldName, fieldValue) if directFields.contains(fieldName) =>
        fieldValue.value.asInstanceOf[Expr[A]]
    }

    // First field mapped with f, rest folded with g
    val head = directFieldExprs.head
    val tail = directFieldExprs.tail
    val seed: Expr[B] = Expr.quote(Expr.splice(fExpr)(Expr.splice(head)))
    val result = tail.foldLeft(seed) { (acc, fieldExpr) =>
      Expr.quote(Expr.splice(gExpr)(Expr.splice(acc), Expr.splice(fieldExpr)))
    }

    MIO.pure(result)
  }

  /** reduceRightTo: map last direct field with f, fold rest right-to-left with g. Produces: g(field1,
    * Eval.now(g(field2, Eval.now(f(field3)))))
    */
  @scala.annotation.nowarn("msg=is never used|unused implicit parameter")
  private def deriveReduceRightToBody[F[_], A, B](
      FCtor: Type.Ctor1[F],
      directFields: Set[String],
      faExpr: Expr[F[A]],
      fExpr: Expr[A => B],
      gExpr: Expr[(A, cats.Eval[B]) => cats.Eval[B]]
  )(implicit AType: Type[A], BType: Type[B]): MIO[Expr[cats.Eval[B]]] = {
    implicit val FAType: Type[F[A]] = FCtor.apply[A]
    implicit val EvalBType: Type[cats.Eval[B]] = ReducibleTypes.EvalCtor.apply[B]

    val caseClass = CaseClass.parse[F[A]].toEither match {
      case Right(cc) => cc
      case Left(e)   => throw new RuntimeException(s"Cannot parse F[A]: $e")
    }
    val fields = caseClass.caseFieldValuesAt(faExpr).toList

    val directFieldExprs: List[Expr[A]] = fields.collect {
      case (fieldName, fieldValue) if directFields.contains(fieldName) =>
        fieldValue.value.asInstanceOf[Expr[A]]
    }

    // Last field mapped with f, rest folded right-to-left with g
    val last = directFieldExprs.last
    val init = directFieldExprs.init
    val seed: Expr[cats.Eval[B]] = Expr.quote(cats.Eval.now(Expr.splice(fExpr)(Expr.splice(last)): B))
    val result = init.foldRight(seed) { (fieldExpr, acc) =>
      Expr.quote(Expr.splice(gExpr)(Expr.splice(fieldExpr), Expr.splice(acc)))
    }

    MIO.pure(result)
  }

  @scala.annotation.nowarn("msg=is never used|unused implicit parameter")
  private def deriveReducibleFoldLeftBody[F[_], A, B](
      FCtor: Type.Ctor1[F],
      directFields: Set[String],
      faExpr: Expr[F[A]],
      bExpr: Expr[B],
      fExpr: Expr[(B, A) => B]
  )(implicit AType: Type[A], BType: Type[B]): MIO[Expr[B]] = {
    implicit val FAType: Type[F[A]] = FCtor.apply[A]

    val caseClass = CaseClass.parse[F[A]].toEither match {
      case Right(cc) => cc
      case Left(e)   => throw new RuntimeException(s"Cannot parse F[A]: $e")
    }
    val fields = caseClass.caseFieldValuesAt(faExpr).toList

    val directFieldExprs: List[Expr[A]] = fields.collect {
      case (fieldName, fieldValue) if directFields.contains(fieldName) =>
        fieldValue.value.asInstanceOf[Expr[A]]
    }

    val result = directFieldExprs.foldLeft(bExpr) { (acc, fieldExpr) =>
      Expr.quote(Expr.splice(fExpr)(Expr.splice(acc), Expr.splice(fieldExpr)))
    }

    MIO.pure(result)
  }

  @scala.annotation.nowarn("msg=is never used|unused implicit parameter")
  private def deriveReducibleFoldRightBody[F[_], A, B](
      FCtor: Type.Ctor1[F],
      directFields: Set[String],
      faExpr: Expr[F[A]],
      lbExpr: Expr[cats.Eval[B]],
      fExpr: Expr[(A, cats.Eval[B]) => cats.Eval[B]]
  )(implicit AType: Type[A], BType: Type[B]): MIO[Expr[cats.Eval[B]]] = {
    implicit val FAType: Type[F[A]] = FCtor.apply[A]
    implicit val EvalBType: Type[cats.Eval[B]] = ReducibleTypes.EvalCtor.apply[B]

    val caseClass = CaseClass.parse[F[A]].toEither match {
      case Right(cc) => cc
      case Left(e)   => throw new RuntimeException(s"Cannot parse F[A]: $e")
    }
    val fields = caseClass.caseFieldValuesAt(faExpr).toList

    val directFieldExprs: List[Expr[A]] = fields.collect {
      case (fieldName, fieldValue) if directFields.contains(fieldName) =>
        fieldValue.value.asInstanceOf[Expr[A]]
    }

    val result = directFieldExprs.foldRight(lbExpr) { (fieldExpr, acc) =>
      Expr.quote(Expr.splice(fExpr)(Expr.splice(fieldExpr), Expr.splice(acc)))
    }

    MIO.pure(result)
  }

  protected object ReducibleTypes {
    val Int: Type[Int] = Type.of[Int]
    val String: Type[String] = Type.of[String]
    def EvalCtor: Type.Ctor1[cats.Eval] = Type.Ctor1.of[cats.Eval]
    val LogDerivation: Type[hearth.kindlings.catsderivation.LogDerivation] =
      Type.of[hearth.kindlings.catsderivation.LogDerivation]
  }

  def shouldWeLogReducibleDerivation: Boolean = {
    implicit val LogDerivationType: Type[LogDerivation] = ReducibleTypes.LogDerivation
    Expr.summonImplicit[LogDerivation].isDefined || (for {
      data <- Environment.typedSettings.toOption
      cd <- data.get("catsDerivation")
      shouldLog <- cd.get("logDerivation").flatMap(_.asBoolean)
    } yield shouldLog).getOrElse(false)
  }
}
