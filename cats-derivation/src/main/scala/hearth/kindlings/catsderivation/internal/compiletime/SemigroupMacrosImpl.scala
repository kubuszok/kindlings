package hearth.kindlings.catsderivation.internal.compiletime

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

import hearth.kindlings.catsderivation.LogDerivation

/** Semigroup derivation: combines case class fields pairwise using their Semigroup instances. */
trait SemigroupMacrosImpl extends rules.SemigroupUseImplicitRuleImpl with rules.SemigroupCaseClassRuleImpl {
  this: MacroCommons & StdExtensions =>

  @scala.annotation.nowarn("msg=is never used")
  def deriveSemigroup[A: Type]: Expr[cats.kernel.Semigroup[A]] = {
    val macroName = "Semigroup.derived"
    implicit val SemigroupA: Type[cats.kernel.Semigroup[A]] = SemigroupTypes.Semigroup[A]

    deriveSemigroupEntrypoint[A, cats.kernel.Semigroup[A]](macroName) { doCombine =>
      Expr.quote {
        new cats.kernel.Semigroup[A] {
          def combine(x: A, y: A): A = {
            val _ = x
            val _ = y
            Expr.splice(doCombine(Expr.quote(x), Expr.quote(y)))
          }
        }
      }
    }
  }

  // Context

  final case class SemigroupCtx[A](
      tpe: Type[A],
      x: Expr[A],
      y: Expr[A],
      cache: MLocal[ValDefsCache],
      derivedType: Option[??]
  ) {
    def nest[B: Type](newX: Expr[B], newY: Expr[B]): SemigroupCtx[B] =
      SemigroupCtx(Type[B], newX, newY, cache, derivedType)
  }
  object SemigroupCtx {
    def from[A: Type](x: Expr[A], y: Expr[A], derivedType: Option[??]): SemigroupCtx[A] =
      SemigroupCtx(Type[A], x, y, ValDefsCache.mlocal, derivedType)
  }

  def sgctx[A](implicit A: SemigroupCtx[A]): SemigroupCtx[A] = A
  implicit def semigroupCtxType[A: SemigroupCtx]: Type[A] = sgctx.tpe

  abstract class SemigroupDerivationRule(val name: String) extends Rule {
    def apply[A: SemigroupCtx]: MIO[Rule.Applicability[Expr[A]]]
  }

  // Recursive derivation

  def deriveSemigroupRecursively[A: SemigroupCtx]: MIO[Expr[A]] =
    Log.namedScope(s"Deriving Semigroup for ${Type[A].prettyPrint}") {
      Rules(
        SemigroupUseImplicitRule,
        SemigroupCaseClassRule
      )(_[A]).flatMap {
        case Right(result) =>
          Log.info(s"Derived Semigroup for ${Type[A].prettyPrint}") >> MIO.pure(result)
        case Left(reasons) =>
          val reasonsStrings = reasons.toListMap
            .removed(SemigroupUseImplicitRule)
            .view
            .map { case (rule, reasons) =>
              if (reasons.isEmpty) s"The rule ${rule.name} was not applicable"
              else s" - ${rule.name}: ${reasons.mkString(", ")}"
            }
            .toList
          val err = SemigroupDerivationError.UnsupportedType(Type[A].prettyPrint, reasonsStrings)
          Log.error(err.message) >> MIO.fail(err)
      }
    }

  protected def deriveSemigroupEntrypoint[A: Type, Out: Type](macroName: String)(
      adapt: ((Expr[A], Expr[A]) => Expr[A]) => Expr[Out]
  ): Expr[Out] = {
    if (Type[A] =:= Type.of[Nothing].asInstanceOf[Type[A]] || Type[A] =:= Type.of[Any].asInstanceOf[Type[A]])
      Environment.reportErrorAndAbort(
        s"$macroName: type parameter was inferred as ${Type[A].prettyPrint}, which is likely unintended."
      )

    Log
      .namedScope(s"Deriving $macroName[${Type[A].prettyPrint}] at: ${Environment.currentPosition.prettyPrint}") {
        MIO.scoped { runSafe =>
          val doCombine: (Expr[A], Expr[A]) => Expr[A] = (xExpr, yExpr) =>
            runSafe {
              for {
                _ <- Environment.loadStandardExtensions().toMIO(allowFailures = false)
                result <- deriveSemigroupRecursively[A](using SemigroupCtx.from(xExpr, yExpr, Some(Type[A].as_??)))
              } yield result
            }
          adapt(doCombine)
        }
      }
      .flatTap(result => Log.info(s"Derived final result: ${result.prettyPrint}"))
      .runToExprOrFail(
        macroName,
        infoRendering = if (shouldWeLogSemigroupDerivation) RenderFrom(Log.Level.Info) else DontRender,
        errorRendering = if (shouldWeLogSemigroupDerivation) RenderFrom(Log.Level.Info) else DontRender
      ) { (errorLogs, errors) =>
        val errorsRendered = errors.map(e => "  - " + e.getMessage).mkString("\n")
        val hint =
          "Enable debug logging with: import hearth.kindlings.catsderivation.debug.logDerivationForCatsDerivation"
        if (errorLogs.nonEmpty) s"Macro derivation failed:\n$errorsRendered\nlogs:\n$errorLogs\n$hint"
        else s"Macro derivation failed:\n$errorsRendered\n$hint"
      }
  }

  protected def deriveSemigroupCombine[A: Type](
      caseClass: CaseClass[A],
      x: Expr[A],
      y: Expr[A]
  ): MIO[Expr[A]] = {
    val fieldsX = caseClass.caseFieldValuesAt(x).toList
    val fieldsY = caseClass.caseFieldValuesAt(y).toList

    val combinedFields: List[(String, Expr_??)] =
      fieldsX.zip(fieldsY).map { case ((fieldName, fieldValueX), (_, fieldValueY)) =>
        import fieldValueX.Underlying as Field
        val fx = fieldValueX.value.asInstanceOf[Expr[Field]]
        val fy = fieldValueY.value.asInstanceOf[Expr[Field]]

        val sgExpr = SemigroupTypes.Semigroup[Field].summonExprIgnoring().toEither match {
          case Right(sg)    => sg
          case Left(reason) =>
            throw new RuntimeException(
              s"No Semigroup instance found for field $fieldName: ${Field.prettyPrint}: $reason"
            )
        }
        val combined: Expr[Field] = Expr.quote(Expr.splice(sgExpr).combine(Expr.splice(fx), Expr.splice(fy)))
        (fieldName, combined.as_??)
      }

    val fieldMap: Map[String, Expr_??] = combinedFields.toMap
    caseClass.primaryConstructor(fieldMap) match {
      case Right(constructExpr) => MIO.pure(constructExpr)
      case Left(error)          =>
        MIO.fail(new RuntimeException(s"Cannot construct ${Type[A].prettyPrint}: $error"))
    }
  }

  protected object SemigroupTypes {
    def Semigroup: Type.Ctor1[cats.kernel.Semigroup] = Type.Ctor1.of[cats.kernel.Semigroup]
    val LogDerivation: Type[hearth.kindlings.catsderivation.LogDerivation] =
      Type.of[hearth.kindlings.catsderivation.LogDerivation]
  }

  def shouldWeLogSemigroupDerivation: Boolean = {
    implicit val LogDerivationType: Type[LogDerivation] = SemigroupTypes.LogDerivation
    Expr.summonImplicit[LogDerivation].isDefined || (for {
      data <- Environment.typedSettings.toOption
      cd <- data.get("catsDerivation")
      shouldLog <- cd.get("logDerivation").flatMap(_.asBoolean)
    } yield shouldLog).getOrElse(false)
  }
}

sealed private[compiletime] trait SemigroupDerivationError
    extends util.control.NoStackTrace
    with Product
    with Serializable {
  def message: String
  override def getMessage(): String = message
}
private[compiletime] object SemigroupDerivationError {
  final case class UnsupportedType(tpeName: String, reasons: List[String]) extends SemigroupDerivationError {
    override def message: String =
      s"The type $tpeName was not handled by any Semigroup derivation rule:\n${reasons.mkString("\n")}"
  }
}
