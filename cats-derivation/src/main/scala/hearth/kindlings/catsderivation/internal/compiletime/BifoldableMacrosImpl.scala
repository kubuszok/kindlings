package hearth.kindlings.catsderivation.internal.compiletime

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

import hearth.kindlings.catsderivation.LogDerivation

trait BifoldableMacrosImpl extends CatsDerivationTimeout { this: MacroCommons & StdExtensions =>

  @scala.annotation.nowarn("msg=is never used|unused explicit parameter")
  def deriveBifoldable[F[_, _]](
      FCtor0: Type.Ctor2[F],
      BifoldableFType: Type[cats.Bifoldable[F]]
  ): Expr[cats.Bifoldable[F]] = {
    val macroName = "Bifoldable.derived"

    implicit val FCtor: Type.Ctor2[F] = FCtor0
    implicit val BifoldableFT: Type[cats.Bifoldable[F]] = BifoldableFType

    Log
      .namedScope(s"Deriving Bifoldable at: ${Environment.currentPosition.prettyPrint}") {
        MIO.scoped { runSafe =>
          implicit val IntType: Type[Int] = BifoldableTypes.Int
          implicit val StringType: Type[String] = BifoldableTypes.String

          val ccIntInt = CaseClass.parse(using FCtor.apply[Int, Int]).toEither match {
            case Right(cc) => cc
            case Left(e)   => throw new RuntimeException(s"Cannot parse F[Int, Int]: $e")
          }
          val ccStringInt = CaseClass.parse(using FCtor.apply[String, Int]).toEither match {
            case Right(cc) => cc
            case Left(e)   => throw new RuntimeException(s"Cannot parse F[String, Int]: $e")
          }
          val ccIntString = CaseClass.parse(using FCtor.apply[Int, String]).toEither match {
            case Right(cc) => cc
            case Left(e)   => throw new RuntimeException(s"Cannot parse F[Int, String]: $e")
          }

          val fieldsP1 = ccIntInt.primaryConstructor.parameters.flatten.toList
          val fieldsP2 = ccStringInt.primaryConstructor.parameters.flatten.toList
          val fieldsP3 = ccIntString.primaryConstructor.parameters.flatten.toList

          val leftFields = scala.collection.mutable.Set.empty[String]
          val rightFields = scala.collection.mutable.Set.empty[String]

          fieldsP1.zip(fieldsP2).zip(fieldsP3).foreach { case (((name, p1), (_, p2)), (_, p3)) =>
            val t1 = p1.tpe.Underlying
            val t2 = p2.tpe.Underlying
            val t3 = p3.tpe.Underlying
            val firstChanges = !(t1 =:= t2)
            val secondChanges = !(t1 =:= t3)
            if (firstChanges && !secondChanges) leftFields += name
            else if (!firstChanges && secondChanges) rightFields += name
          }

          val leftFieldSet: Set[String] = leftFields.toSet
          val rightFieldSet: Set[String] = rightFields.toSet

          runSafe {
            Environment.loadStandardExtensions().toMIO(allowFailures = false).map(_ => ())
          }

          implicit val AnyType: Type[Any] = BifoldableTypes.Any
          implicit val FAnyAnyType: Type[F[Any, Any]] = FCtor.apply[Any, Any]
          implicit val EvalAnyType: Type[cats.Eval[Any]] = BifoldableTypes.EvalCtor.apply[Any]

          val ccAnyAny = CaseClass.parse[F[Any, Any]].toEither match {
            case Right(cc) => cc
            case Left(e)   => throw new RuntimeException(s"Cannot parse F[Any, Any]: $e")
          }

          val doBifoldLeft: (Expr[F[Any, Any]], Expr[Any], Expr[(Any, Any) => Any], Expr[(Any, Any) => Any]) => Expr[
            Any
          ] =
            (fabExpr, cExpr, fExpr, gExpr) =>
              runSafe {
                val fields = ccAnyAny.caseFieldValuesAt(fabExpr).toList
                var acc = cExpr
                fields.foreach { case (fieldName, fieldValue) =>
                  import fieldValue.Underlying as Field
                  val fieldExpr = fieldValue.value.asInstanceOf[Expr[Field]].upcast[Any]
                  if (leftFieldSet.contains(fieldName))
                    acc = Expr.quote(Expr.splice(fExpr)(Expr.splice(acc), Expr.splice(fieldExpr)))
                  else if (rightFieldSet.contains(fieldName))
                    acc = Expr.quote(Expr.splice(gExpr)(Expr.splice(acc), Expr.splice(fieldExpr)))
                }
                MIO.pure(acc)
              }

          val doBifoldRight: (
              Expr[F[Any, Any]],
              Expr[cats.Eval[Any]],
              Expr[(Any, cats.Eval[Any]) => cats.Eval[Any]],
              Expr[
                (Any, cats.Eval[Any]) => cats.Eval[Any]
              ]
          ) => Expr[cats.Eval[Any]] =
            (fabExpr, lcExpr, fExpr, gExpr) =>
              runSafe {
                val fields = ccAnyAny.caseFieldValuesAt(fabExpr).toList
                val directFieldExprs: List[(String, Expr[Any])] = fields.collect {
                  case (fieldName, fieldValue)
                      if leftFieldSet.contains(fieldName) || rightFieldSet.contains(fieldName) =>
                    import fieldValue.Underlying as Field
                    (fieldName, fieldValue.value.asInstanceOf[Expr[Field]].upcast[Any])
                }
                val result = directFieldExprs.foldRight(lcExpr) { case ((fieldName, fieldExpr), acc) =>
                  if (leftFieldSet.contains(fieldName))
                    Expr.quote(Expr.splice(fExpr)(Expr.splice(fieldExpr), Expr.splice(acc)))
                  else
                    Expr.quote(Expr.splice(gExpr)(Expr.splice(fieldExpr), Expr.splice(acc)))
                }
                MIO.pure(result)
              }

          import hearth.kindlings.catsderivation.internal.runtime.CatsDerivationFactories
          Expr.quote {
            CatsDerivationFactories.bifoldableInstance[F](
              bifoldLeftFn = {
                (
                    fab: F[CatsDerivationFactories.W1, CatsDerivationFactories.W2],
                    cAny: Any,
                    fAny: (Any, Any) => Any,
                    gAny: (Any, Any) => Any
                ) =>
                  val anyFab: F[Any, Any] = fab.asInstanceOf[F[Any, Any]]
                  val _ = anyFab
                  val _ = cAny
                  val _ = fAny
                  val _ = gAny
                  Expr
                    .splice(doBifoldLeft(Expr.quote(anyFab), Expr.quote(cAny), Expr.quote(fAny), Expr.quote(gAny)))
                    .asInstanceOf[Any]
              },
              bifoldRightFn = {
                (
                    fab: F[CatsDerivationFactories.W1, CatsDerivationFactories.W2],
                    lcAny: cats.Eval[Any],
                    fAny: (Any, cats.Eval[Any]) => cats.Eval[Any],
                    gAny: (Any, cats.Eval[Any]) => cats.Eval[Any]
                ) =>
                  val anyFab: F[Any, Any] = fab.asInstanceOf[F[Any, Any]]
                  val _ = anyFab
                  val _ = lcAny
                  val _ = fAny
                  val _ = gAny
                  Expr
                    .splice(
                      doBifoldRight(Expr.quote(anyFab), Expr.quote(lcAny), Expr.quote(fAny), Expr.quote(gAny))
                    )
                    .asInstanceOf[cats.Eval[Any]]
              }
            )
          }
        }
      }
      .flatTap(result => Log.info(s"Derived final result: ${result.prettyPrint}"))
      .runToExprOrFail(
        macroName,
        infoRendering = if (shouldWeLogBifoldableDerivation) RenderFrom(Log.Level.Info) else DontRender,
        errorRendering = if (shouldWeLogBifoldableDerivation) RenderFrom(Log.Level.Info) else DontRender,
        timeout = derivationTimeout
      ) { (errorLogs, errors) =>
        val errorsRendered = errors.map(e => "  - " + e.getMessage).mkString("\n")
        val hint =
          "Enable debug logging with: import hearth.kindlings.catsderivation.debug.logDerivationForCatsDerivation"
        if (errorLogs.nonEmpty) s"Macro derivation failed:\n$errorsRendered\nlogs:\n$errorLogs\n$hint"
        else s"Macro derivation failed:\n$errorsRendered\n$hint"
      }
  }

  protected object BifoldableTypes {
    val Any: Type[Any] = Type.of[Any]
    val Int: Type[Int] = Type.of[Int]
    val String: Type[String] = Type.of[String]
    def EvalCtor: Type.Ctor1[cats.Eval] = Type.Ctor1.of[cats.Eval]
    val LogDerivation: Type[hearth.kindlings.catsderivation.LogDerivation] =
      Type.of[hearth.kindlings.catsderivation.LogDerivation]
  }

  def shouldWeLogBifoldableDerivation: Boolean = {
    implicit val LogDerivationType: Type[LogDerivation] = BifoldableTypes.LogDerivation
    Expr.summonImplicit[LogDerivation].isDefined || (for {
      data <- Environment.typedSettings.toOption
      cd <- data.get("catsDerivation")
      shouldLog <- cd.get("logDerivation").flatMap(_.asBoolean)
    } yield shouldLog).getOrElse(false)
  }
}
