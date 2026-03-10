package hearth.kindlings.catsderivation.internal.compiletime

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

import hearth.kindlings.catsderivation.LogDerivation

/** Reducible derivation: reduceLeftTo + reduceRightTo + foldLeft + foldRight over direct type parameter fields.
  *
  * Uses an erased approach: builds reduce/fold bodies for F[Any], reducing over direct fields only (invariant fields
  * are skipped). Requires at least one direct field (non-empty guarantee).
  *
  * reduceLeftTo: maps the first direct field with f, then folds remaining direct fields with g. reduceRightTo: maps the
  * last direct field with f, then folds preceding direct fields right-to-left with g.
  */
trait ReducibleMacrosImpl { this: MacroCommons & StdExtensions =>

  @scala.annotation.nowarn("msg=is never used|unused explicit parameter")
  def deriveReducible[F[_]](
      FCtor0: Type.Ctor1[F],
      ReducibleFType: Type[cats.Reducible[F]]
  ): Expr[cats.Reducible[F]] = {
    val macroName = "Reducible.derived"

    implicit val FCtor: Type.Ctor1[F] = FCtor0
    implicit val ReducibleFT: Type[cats.Reducible[F]] = ReducibleFType
    implicit val AnyType: Type[Any] = ReducibleTypes.Any
    implicit val FAnyType: Type[F[Any]] = FCtor.apply[Any]
    implicit val EvalAnyType: Type[cats.Eval[Any]] = ReducibleTypes.EvalAny

    Log
      .namedScope(s"Deriving Reducible at: ${Environment.currentPosition.prettyPrint}") {
        CaseClass.parse[F[Any]].toEither match {
          case Right(caseClass) =>
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

              // Pre-load extensions eagerly to avoid Scala 3 sibling splice isolation issues
              val _ = runSafe {
                Environment.loadStandardExtensions().toMIO(allowFailures = false)
              }

              val doReduceLeftTo: (Expr[F[Any]], Expr[Any => Any], Expr[(Any, Any) => Any]) => Expr[Any] =
                (faExpr, fExpr, gExpr) =>
                  runSafe {
                    deriveReduceLeftToBody[F](caseClass, directFieldSet, faExpr, fExpr, gExpr)
                  }

              val doReduceRightTo
                  : (Expr[F[Any]], Expr[Any => Any], Expr[(Any, cats.Eval[Any]) => cats.Eval[Any]]) => Expr[
                    cats.Eval[Any]
                  ] =
                (faExpr, fExpr, gExpr) =>
                  runSafe {
                    deriveReduceRightToBody[F](caseClass, directFieldSet, faExpr, fExpr, gExpr)
                  }

              val doFoldLeft: (Expr[F[Any]], Expr[Any], Expr[(Any, Any) => Any]) => Expr[Any] =
                (faExpr, bExpr, fExpr) =>
                  runSafe {
                    deriveReducibleFoldLeftBody[F](caseClass, directFieldSet, faExpr, bExpr, fExpr)
                  }

              val doFoldRight
                  : (Expr[F[Any]], Expr[cats.Eval[Any]], Expr[(Any, cats.Eval[Any]) => cats.Eval[Any]]) => Expr[
                    cats.Eval[Any]
                  ] =
                (faExpr, lbExpr, fExpr) =>
                  runSafe {
                    deriveReducibleFoldRightBody[F](caseClass, directFieldSet, faExpr, lbExpr, fExpr)
                  }

              Expr.quote {
                new cats.Reducible[F] {
                  def reduceLeftTo[A, B](fa: F[A])(f: A => B)(g: (B, A) => B): B = {
                    val anyFa: F[Any] = fa.asInstanceOf[F[Any]]
                    val anyF: Any => Any = f.asInstanceOf[Any => Any]
                    val anyG: (Any, Any) => Any = g.asInstanceOf[(Any, Any) => Any]
                    val _ = anyFa
                    val _ = anyF
                    val _ = anyG
                    Expr
                      .splice(doReduceLeftTo(Expr.quote(anyFa), Expr.quote(anyF), Expr.quote(anyG)))
                      .asInstanceOf[B]
                  }

                  def reduceRightTo[A, B](fa: F[A])(f: A => B)(
                      g: (A, cats.Eval[B]) => cats.Eval[B]
                  ): cats.Eval[B] = {
                    val anyFa: F[Any] = fa.asInstanceOf[F[Any]]
                    val anyF: Any => Any = f.asInstanceOf[Any => Any]
                    val anyG: (Any, cats.Eval[Any]) => cats.Eval[Any] =
                      g.asInstanceOf[(Any, cats.Eval[Any]) => cats.Eval[Any]]
                    val _ = anyFa
                    val _ = anyF
                    val _ = anyG
                    Expr
                      .splice(doReduceRightTo(Expr.quote(anyFa), Expr.quote(anyF), Expr.quote(anyG)))
                      .asInstanceOf[cats.Eval[B]]
                  }

                  def foldLeft[A, B](fa: F[A], b: B)(f: (B, A) => B): B = {
                    val anyFa: F[Any] = fa.asInstanceOf[F[Any]]
                    val anyB: Any = b.asInstanceOf[Any]
                    val anyF: (Any, Any) => Any = f.asInstanceOf[(Any, Any) => Any]
                    val _ = anyFa
                    val _ = anyB
                    val _ = anyF
                    Expr
                      .splice(doFoldLeft(Expr.quote(anyFa), Expr.quote(anyB), Expr.quote(anyF)))
                      .asInstanceOf[B]
                  }

                  def foldRight[A, B](fa: F[A], lb: cats.Eval[B])(
                      f: (A, cats.Eval[B]) => cats.Eval[B]
                  ): cats.Eval[B] = {
                    val anyFa: F[Any] = fa.asInstanceOf[F[Any]]
                    val anyLb: cats.Eval[Any] = lb.asInstanceOf[cats.Eval[Any]]
                    val anyF: (Any, cats.Eval[Any]) => cats.Eval[Any] =
                      f.asInstanceOf[(Any, cats.Eval[Any]) => cats.Eval[Any]]
                    val _ = anyFa
                    val _ = anyLb
                    val _ = anyF
                    Expr
                      .splice(doFoldRight(Expr.quote(anyFa), Expr.quote(anyLb), Expr.quote(anyF)))
                      .asInstanceOf[cats.Eval[B]]
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
        infoRendering = if (shouldWeLogReducibleDerivation) RenderFrom(Log.Level.Info) else DontRender,
        errorRendering = if (shouldWeLogReducibleDerivation) RenderFrom(Log.Level.Info) else DontRender
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
  private def deriveReduceLeftToBody[F[_]](
      caseClass: CaseClass[F[Any]],
      directFields: Set[String],
      faExpr: Expr[F[Any]],
      fExpr: Expr[Any => Any],
      gExpr: Expr[(Any, Any) => Any]
  )(implicit FCtor: Type.Ctor1[F], FAnyType: Type[F[Any]], AnyType: Type[Any]): MIO[Expr[Any]] = {
    val fields = caseClass.caseFieldValuesAt(faExpr).toList

    val directFieldExprs: List[Expr[Any]] = fields.collect {
      case (fieldName, fieldValue) if directFields.contains(fieldName) =>
        import fieldValue.Underlying as Field
        fieldValue.value.asInstanceOf[Expr[Field]].upcast[Any]
    }

    // First field mapped with f, rest folded with g
    val head = directFieldExprs.head
    val tail = directFieldExprs.tail
    val seed: Expr[Any] = Expr.quote(Expr.splice(fExpr)(Expr.splice(head)))
    val result = tail.foldLeft(seed) { (acc, fieldExpr) =>
      Expr.quote(Expr.splice(gExpr)(Expr.splice(acc), Expr.splice(fieldExpr)))
    }

    MIO.pure(result)
  }

  /** reduceRightTo: map last direct field with f, fold rest right-to-left with g. Produces: g(field1,
    * Eval.now(g(field2, Eval.now(f(field3)))))
    */
  @scala.annotation.nowarn("msg=is never used|unused implicit parameter")
  private def deriveReduceRightToBody[F[_]](
      caseClass: CaseClass[F[Any]],
      directFields: Set[String],
      faExpr: Expr[F[Any]],
      fExpr: Expr[Any => Any],
      gExpr: Expr[(Any, cats.Eval[Any]) => cats.Eval[Any]]
  )(implicit
      FCtor: Type.Ctor1[F],
      FAnyType: Type[F[Any]],
      AnyType: Type[Any],
      EvalAnyType: Type[cats.Eval[Any]]
  ): MIO[Expr[cats.Eval[Any]]] = {
    val fields = caseClass.caseFieldValuesAt(faExpr).toList

    val directFieldExprs: List[Expr[Any]] = fields.collect {
      case (fieldName, fieldValue) if directFields.contains(fieldName) =>
        import fieldValue.Underlying as Field
        fieldValue.value.asInstanceOf[Expr[Field]].upcast[Any]
    }

    // Last field mapped with f, rest folded right-to-left with g
    val last = directFieldExprs.last
    val init = directFieldExprs.init
    val seed: Expr[cats.Eval[Any]] = Expr.quote(cats.Eval.now(Expr.splice(fExpr)(Expr.splice(last)): Any))
    val result = init.foldRight(seed) { (fieldExpr, acc) =>
      Expr.quote(Expr.splice(gExpr)(Expr.splice(fieldExpr), Expr.splice(acc)))
    }

    MIO.pure(result)
  }

  @scala.annotation.nowarn("msg=is never used|unused implicit parameter")
  private def deriveReducibleFoldLeftBody[F[_]](
      caseClass: CaseClass[F[Any]],
      directFields: Set[String],
      faExpr: Expr[F[Any]],
      bExpr: Expr[Any],
      fExpr: Expr[(Any, Any) => Any]
  )(implicit FCtor: Type.Ctor1[F], FAnyType: Type[F[Any]], AnyType: Type[Any]): MIO[Expr[Any]] = {
    val fields = caseClass.caseFieldValuesAt(faExpr).toList

    val directFieldExprs: List[Expr[Any]] = fields.collect {
      case (fieldName, fieldValue) if directFields.contains(fieldName) =>
        import fieldValue.Underlying as Field
        fieldValue.value.asInstanceOf[Expr[Field]].upcast[Any]
    }

    val result = directFieldExprs.foldLeft(bExpr) { (acc, fieldExpr) =>
      Expr.quote(Expr.splice(fExpr)(Expr.splice(acc), Expr.splice(fieldExpr)))
    }

    MIO.pure(result)
  }

  @scala.annotation.nowarn("msg=is never used|unused implicit parameter")
  private def deriveReducibleFoldRightBody[F[_]](
      caseClass: CaseClass[F[Any]],
      directFields: Set[String],
      faExpr: Expr[F[Any]],
      lbExpr: Expr[cats.Eval[Any]],
      fExpr: Expr[(Any, cats.Eval[Any]) => cats.Eval[Any]]
  )(implicit
      FCtor: Type.Ctor1[F],
      FAnyType: Type[F[Any]],
      AnyType: Type[Any],
      EvalAnyType: Type[cats.Eval[Any]]
  ): MIO[Expr[cats.Eval[Any]]] = {
    val fields = caseClass.caseFieldValuesAt(faExpr).toList

    val directFieldExprs: List[Expr[Any]] = fields.collect {
      case (fieldName, fieldValue) if directFields.contains(fieldName) =>
        import fieldValue.Underlying as Field
        fieldValue.value.asInstanceOf[Expr[Field]].upcast[Any]
    }

    val result = directFieldExprs.foldRight(lbExpr) { (fieldExpr, acc) =>
      Expr.quote(Expr.splice(fExpr)(Expr.splice(fieldExpr), Expr.splice(acc)))
    }

    MIO.pure(result)
  }

  protected object ReducibleTypes {
    val Any: Type[Any] = Type.of[Any]
    val Int: Type[Int] = Type.of[Int]
    val String: Type[String] = Type.of[String]
    def EvalAny: Type[cats.Eval[Any]] = Type.of[cats.Eval[Any]]
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
