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
trait ReducibleMacrosImpl extends CatsDerivationTimeout with CatsDerivationErrorSupport {
  this: MacroCommons & StdExtensions =>

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

          val ccInt = runSafe(parseCaseClassMIO[F[Int]]("F[Int]")(using FCtor.apply[Int]))
          val ccString = runSafe(parseCaseClassMIO[F[String]]("F[String]")(using FCtor.apply[String]))

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
              // Invariant — skip in reduce
            } else {
              nestedFields += name
            }
          }

          if (nestedFields.nonEmpty) {
            runSafe {
              failDerivation[Unit](
                CatsDerivationError.UnsupportedFieldShape(
                  "Reducible",
                  nestedFields.mkString(", "),
                  "the fields contain nested type constructors - " +
                    "only direct type parameter fields (A) and invariant fields are supported"
                )
              )
            }
          }

          if (directFields.isEmpty) {
            runSafe {
              failDerivation[Unit](
                CatsDerivationError.DerivationFailed(
                  "Reducible",
                  "no direct type parameter fields found - " +
                    "Reducible requires at least one field of the type parameter"
                )
              )
            }
          }

          val directFieldSet: Set[String] = directFields.toSet

          // Pre-load extensions before entering the quote
          runSafe {
            Environment.loadStandardExtensions().toMIO(allowFailures = false).map(_ => ())
          }

          implicit val AnyType: Type[Any] = ReducibleTypes.Any
          implicit val FAnyType: Type[F[Any]] = FCtor.apply[Any]
          implicit val EvalAnyType: Type[cats.Eval[Any]] = ReducibleTypes.EvalCtor.apply[Any]

          val ccAny = runSafe(parseCaseClassMIO[F[Any]]("F[Any]"))

          val doReduceLeftTo: (Expr[F[Any]], Expr[Any => Any], Expr[(Any, Any) => Any]) => Expr[Any] =
            (faExpr, fExpr, gExpr) =>
              runSafe {
                val fields = ccAny.caseFieldValuesAt(faExpr).toList
                val directFieldExprs: List[Expr[Any]] = fields.collect {
                  case (fieldName, fieldValue) if directFieldSet.contains(fieldName) =>
                    import fieldValue.Underlying as Field
                    fieldValue.value.asInstanceOf[Expr[Field]].upcast[Any]
                }
                val head = directFieldExprs.head
                val tail = directFieldExprs.tail
                val seed: Expr[Any] = Expr.quote(Expr.splice(fExpr)(Expr.splice(head)))
                val result = tail.foldLeft(seed) { (acc, fieldExpr) =>
                  Expr.quote(Expr.splice(gExpr)(Expr.splice(acc), Expr.splice(fieldExpr)))
                }
                MIO.pure(result)
              }

          val doReduceRightTo: (Expr[F[Any]], Expr[Any => Any], Expr[(Any, cats.Eval[Any]) => cats.Eval[Any]]) => Expr[
            cats.Eval[Any]
          ] =
            (faExpr, fExpr, gExpr) =>
              runSafe {
                val fields = ccAny.caseFieldValuesAt(faExpr).toList
                val directFieldExprs: List[Expr[Any]] = fields.collect {
                  case (fieldName, fieldValue) if directFieldSet.contains(fieldName) =>
                    import fieldValue.Underlying as Field
                    fieldValue.value.asInstanceOf[Expr[Field]].upcast[Any]
                }
                val last = directFieldExprs.last
                val init = directFieldExprs.init
                val seed: Expr[cats.Eval[Any]] =
                  Expr.quote(cats.Eval.now(Expr.splice(fExpr)(Expr.splice(last)): Any))
                val result = init.foldRight(seed) { (fieldExpr, acc) =>
                  Expr.quote(Expr.splice(gExpr)(Expr.splice(fieldExpr), Expr.splice(acc)))
                }
                MIO.pure(result)
              }

          val doFoldLeft: (Expr[F[Any]], Expr[Any], Expr[(Any, Any) => Any]) => Expr[Any] =
            (faExpr, bExpr, fExpr) =>
              runSafe {
                val fields = ccAny.caseFieldValuesAt(faExpr).toList
                val directFieldExprs: List[Expr[Any]] = fields.collect {
                  case (fieldName, fieldValue) if directFieldSet.contains(fieldName) =>
                    import fieldValue.Underlying as Field
                    fieldValue.value.asInstanceOf[Expr[Field]].upcast[Any]
                }
                val result = directFieldExprs.foldLeft(bExpr) { (acc, fieldExpr) =>
                  Expr.quote(Expr.splice(fExpr)(Expr.splice(acc), Expr.splice(fieldExpr)))
                }
                MIO.pure(result)
              }

          val doFoldRight: (Expr[F[Any]], Expr[cats.Eval[Any]], Expr[(Any, cats.Eval[Any]) => cats.Eval[Any]]) => Expr[
            cats.Eval[Any]
          ] =
            (faExpr, lbExpr, fExpr) =>
              runSafe {
                val fields = ccAny.caseFieldValuesAt(faExpr).toList
                val directFieldExprs: List[Expr[Any]] = fields.collect {
                  case (fieldName, fieldValue) if directFieldSet.contains(fieldName) =>
                    import fieldValue.Underlying as Field
                    fieldValue.value.asInstanceOf[Expr[Field]].upcast[Any]
                }
                val result = directFieldExprs.foldRight(lbExpr) { (fieldExpr, acc) =>
                  Expr.quote(Expr.splice(fExpr)(Expr.splice(fieldExpr), Expr.splice(acc)))
                }
                MIO.pure(result)
              }

          import hearth.kindlings.catsderivation.internal.runtime.CatsDerivationFactories
          Expr.quote {
            CatsDerivationFactories.reducibleInstance[F](
              reduceLeftToFn = { (fa: F[CatsDerivationFactories.W1], fAny: Any, gAny: Any) =>
                val anyFa: F[Any] = fa.asInstanceOf[F[Any]]
                val anyF: Any => Any = fAny.asInstanceOf[Any => Any]
                val anyG: (Any, Any) => Any = gAny.asInstanceOf[(Any, Any) => Any]
                val _ = anyFa
                val _ = anyF
                val _ = anyG
                Expr
                  .splice(doReduceLeftTo(Expr.quote(anyFa), Expr.quote(anyF), Expr.quote(anyG)))
                  .asInstanceOf[Any]
              },
              reduceRightToFn = { (fa: F[CatsDerivationFactories.W1], fAny: Any, gAny: Any) =>
                val anyFa: F[Any] = fa.asInstanceOf[F[Any]]
                val anyF: Any => Any = fAny.asInstanceOf[Any => Any]
                val anyG: (Any, cats.Eval[Any]) => cats.Eval[Any] =
                  gAny.asInstanceOf[(Any, cats.Eval[Any]) => cats.Eval[Any]]
                val _ = anyFa
                val _ = anyF
                val _ = anyG
                Expr
                  .splice(doReduceRightTo(Expr.quote(anyFa), Expr.quote(anyF), Expr.quote(anyG)))
                  .asInstanceOf[Any]
              },
              foldLeftFn = { (fa: F[CatsDerivationFactories.W1], bAny: Any, fAny: (Any, Any) => Any) =>
                val anyFa: F[Any] = fa.asInstanceOf[F[Any]]
                val _ = anyFa
                val _ = bAny
                val _ = fAny
                Expr
                  .splice(doFoldLeft(Expr.quote(anyFa), Expr.quote(bAny), Expr.quote(fAny)))
                  .asInstanceOf[Any]
              },
              foldRightFn = {
                (
                    fa: F[CatsDerivationFactories.W1],
                    lbAny: cats.Eval[Any],
                    fAny: (Any, cats.Eval[Any]) => cats.Eval[Any]
                ) =>
                  val anyFa: F[Any] = fa.asInstanceOf[F[Any]]
                  val _ = anyFa
                  val _ = lbAny
                  val _ = fAny
                  Expr
                    .splice(doFoldRight(Expr.quote(anyFa), Expr.quote(lbAny), Expr.quote(fAny)))
                    .asInstanceOf[cats.Eval[Any]]
              }
            )
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

  protected object ReducibleTypes {
    val Any: Type[Any] = Type.of[Any]
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
