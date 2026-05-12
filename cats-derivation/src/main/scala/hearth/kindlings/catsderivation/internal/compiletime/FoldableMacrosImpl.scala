package hearth.kindlings.catsderivation.internal.compiletime

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

import hearth.kindlings.catsderivation.LogDerivation

/** Foldable derivation: foldLeft and foldRight over direct type parameter fields.
  *
  * Uses free type variables A and B directly in the generated code, relying on Hearth 0.2.0-264+ cross-quotes support
  * for method-level type parameters inside Expr.quote/Expr.splice.
  */
trait FoldableMacrosImpl extends CatsDerivationTimeout { this: MacroCommons & StdExtensions =>

  @scala.annotation.nowarn("msg=is never used|unused explicit parameter")
  def deriveFoldable[F[_]](
      FCtor0: Type.Ctor1[F],
      FoldableFType: Type[cats.Foldable[F]]
  ): Expr[cats.Foldable[F]] = {
    val macroName = "Foldable.derived"

    implicit val FCtor: Type.Ctor1[F] = FCtor0
    implicit val FoldableFT: Type[cats.Foldable[F]] = FoldableFType

    Log
      .namedScope(s"Deriving Foldable at: ${Environment.currentPosition.prettyPrint}") {
        MIO.scoped { runSafe =>
          implicit val IntType: Type[Int] = FoldableTypes.Int
          implicit val StringType: Type[String] = FoldableTypes.String

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
              // Invariant — skip in fold
            } else {
              nestedFields += name
            }
          }

          if (nestedFields.nonEmpty) {
            throw new RuntimeException(
              s"Cannot derive Foldable: fields ${nestedFields.mkString(", ")} contain nested type constructors. " +
                "Only direct type parameter fields (A) and invariant fields are supported."
            )
          }

          val directFieldSet: Set[String] = directFields.toSet

          // Pre-load extensions before entering the quote
          runSafe {
            Environment.loadStandardExtensions().toMIO(allowFailures = false).map(_ => ())
          }

          implicit val AnyType: Type[Any] = FoldableTypes.Any
          implicit val FAnyType: Type[F[Any]] = FCtor.apply[Any]
          implicit val EvalAnyType: Type[cats.Eval[Any]] = FoldableTypes.EvalCtor.apply[Any]

          val ccAny = CaseClass.parse[F[Any]].toEither match {
            case Right(cc) => cc
            case Left(e)   => throw new RuntimeException(s"Cannot parse F[Any]: $e")
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
            CatsDerivationFactories.foldableInstance[F](
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
        infoRendering = if (shouldWeLogFoldableDerivation) RenderFrom(Log.Level.Info) else DontRender,
        errorRendering = if (shouldWeLogFoldableDerivation) RenderFrom(Log.Level.Info) else DontRender,
        timeout = derivationTimeout
      ) { (errorLogs, errors) =>
        val errorsRendered = errors.map(e => "  - " + e.getMessage).mkString("\n")
        val hint =
          "Enable debug logging with: import hearth.kindlings.catsderivation.debug.logDerivationForCatsDerivation"
        if (errorLogs.nonEmpty) s"Macro derivation failed:\n$errorsRendered\nlogs:\n$errorLogs\n$hint"
        else s"Macro derivation failed:\n$errorsRendered\n$hint"
      }
  }

  protected object FoldableTypes {
    val Any: Type[Any] = Type.of[Any]
    val Int: Type[Int] = Type.of[Int]
    val String: Type[String] = Type.of[String]
    def EvalCtor: Type.Ctor1[cats.Eval] = Type.Ctor1.of[cats.Eval]
    val LogDerivation: Type[hearth.kindlings.catsderivation.LogDerivation] =
      Type.of[hearth.kindlings.catsderivation.LogDerivation]
  }

  def shouldWeLogFoldableDerivation: Boolean = {
    implicit val LogDerivationType: Type[LogDerivation] = FoldableTypes.LogDerivation
    Expr.summonImplicit[LogDerivation].isDefined || (for {
      data <- Environment.typedSettings.toOption
      cd <- data.get("catsDerivation")
      shouldLog <- cd.get("logDerivation").flatMap(_.asBoolean)
    } yield shouldLog).getOrElse(false)
  }
}
