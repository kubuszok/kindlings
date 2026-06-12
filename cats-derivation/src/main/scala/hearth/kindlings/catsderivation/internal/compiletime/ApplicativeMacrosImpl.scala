package hearth.kindlings.catsderivation.internal.compiletime

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.fp.instances.*
import hearth.fp.syntax.*
import hearth.std.*

import hearth.kindlings.catsderivation.LogDerivation

/** Applicative derivation: Apply (map + ap) + pure.
  *
  * Uses free type variables A and B directly in the generated code for pure and map, relying on Hearth 0.2.0-264+
  * cross-quotes support for method-level type parameters inside Expr.quote/Expr.splice. For ap, uses an erased approach
  * since both ff: F[A => B] and fa: F[A] must be treated as F[Any] for field extraction.
  */
trait ApplicativeMacrosImpl
    extends rules.ApplicativeCaseClassRuleImpl
    with CatsDerivationTimeout
    with CatsDerivationErrorSupport {
  this: MacroCommons & StdExtensions =>

  final case class ApplicativeCaseClassResult[F[_]](
      FCtor: Type.Ctor1[F],
      directFieldSet: Set[String],
      caseClass: CaseClass[F[Any]]
  )

  abstract class ApplicativeDerivationRule(val name: String) extends Rule {
    def apply[F[_]](implicit FCtor: Type.Ctor1[F]): MIO[Rule.Applicability[ApplicativeCaseClassResult[F]]]
  }

  def deriveApplicativeRecursively[F[_]](implicit FCtor: Type.Ctor1[F]): MIO[ApplicativeCaseClassResult[F]] = {
    implicit val AnyType: Type[Any] = ApplicativeTypes.Any
    val fName = FCtor.apply[Any].shortName
    Log.namedScope(s"Deriving Applicative for $fName") {
      Rules(ApplicativeCaseClassRule)(_[F]).flatMap {
        case Right(result) =>
          Log.info(s"Derived Applicative for $fName") >> MIO.pure(result)
        case Left(reasons) =>
          val reasonsStrings = reasons.toListMap.view.map { case (rule, rs) =>
            if (rs.isEmpty) s"The rule ${rule.name} was not applicable"
            else s" - ${rule.name}: ${rs.mkString(", ")}"
          }.toList
          val err = ApplicativeDerivationError.UnsupportedType(fName, reasonsStrings)
          Log.error(err.message) >> MIO.fail(err)
      }
    }
  }

  protected def deriveApplicativeForCaseClass[F[_]](
      result: ApplicativeCaseClassResult[F],
      runSafe: hearth.fp.DirectStyle.RunSafe[hearth.fp.effect.MIO]
  )(implicit FCtor: Type.Ctor1[F]): Expr[cats.Applicative[F]] = {
    implicit val AnyType: Type[Any] = ApplicativeTypes.Any
    implicit val FAnyType: Type[F[Any]] = FCtor.apply[Any]
    val directFieldSet = result.directFieldSet
    val caseClass = result.caseClass
    val doAp: (Expr[F[Any]], Expr[F[Any]]) => Expr[F[Any]] = (ffExpr, faExpr) =>
      runSafe(deriveApplicativeApBody[F](caseClass, directFieldSet, ffExpr, faExpr))
    val doPure: Expr[Any] => Expr[F[Any]] = (aExpr: Expr[Any]) =>
      runSafe(deriveApplicativePureBody[F, Any](FCtor, directFieldSet, aExpr)(AnyType))
    import hearth.kindlings.catsderivation.internal.runtime.CatsDerivationFactories
    Expr.quote {
      CatsDerivationFactories.applicativeInstance[F](
        pureFn = { (aAny: Any) =>
          val _ = aAny
          Expr.splice(doPure(Expr.quote(aAny))).asInstanceOf[F[CatsDerivationFactories.W1]]
        },
        mapFn = { (fa: F[CatsDerivationFactories.W1], f: CatsDerivationFactories.W1 => CatsDerivationFactories.W2) =>
          val _ = fa
          val _ = f
          Expr.splice {
            runSafe {
              deriveApplicativeMapBody[F, CatsDerivationFactories.W1, CatsDerivationFactories.W2](
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
        apFn = { (ff: F[CatsDerivationFactories.W1 => CatsDerivationFactories.W2], fa: F[CatsDerivationFactories.W1]) =>
          val anyFf: F[Any] = ff.asInstanceOf[F[Any]]
          val anyFa: F[Any] = fa.asInstanceOf[F[Any]]
          val _ = anyFf
          val _ = anyFa
          Expr.splice(doAp(Expr.quote(anyFf), Expr.quote(anyFa))).asInstanceOf[F[CatsDerivationFactories.W2]]
        }
      )
    }
  }

  @scala.annotation.nowarn("msg=is never used|unused explicit parameter|unused local definition")
  def deriveApplicative[F[_]](
      FCtor0: Type.Ctor1[F],
      ApplicativeFType: Type[cats.Applicative[F]]
  ): Expr[cats.Applicative[F]] = {
    val macroName = "Applicative.derived"

    implicit val FCtor: Type.Ctor1[F] = FCtor0
    implicit val ApplicativeFT: Type[cats.Applicative[F]] = ApplicativeFType

    Log
      .namedScope(s"Deriving Applicative at: ${Environment.currentPosition.prettyPrint}") {
        MIO.scoped { runSafe =>
          runSafe {
            for {
              _ <- Environment.loadStandardExtensions().toMIO(allowFailures = false)
              result <- deriveApplicativeRecursively[F]
            } yield deriveApplicativeForCaseClass(result, runSafe)(FCtor)
          }
        }
      }
      .flatTap(result => Log.info(s"Derived final result: ${result.prettyPrint}"))
      .runToExprOrFail(
        macroName,
        infoRendering = if (shouldWeLogApplicativeDerivation) RenderFrom(Log.Level.Info) else DontRender,
        errorRendering = if (shouldWeLogApplicativeDerivation) RenderFrom(Log.Level.Info) else DontRender,
        timeout = derivationTimeout
      ) { (errorLogs, errors) =>
        val errorsRendered = errors.map(e => "  - " + e.getMessage).mkString("\n")
        val hint =
          "Enable debug logging with: import hearth.kindlings.catsderivation.debug.logDerivationForCatsDerivation"
        if (errorLogs.nonEmpty) s"Macro derivation failed:\n$errorsRendered\nlogs:\n$errorLogs\n$hint"
        else s"Macro derivation failed:\n$errorsRendered\n$hint"
      }
  }

  @scala.annotation.nowarn("msg=is never used|unused implicit parameter")
  private def deriveApplicativePureBody[F[_], A](
      FCtor: Type.Ctor1[F],
      directFields: Set[String],
      aExpr: Expr[A]
  )(implicit AType: Type[A]): MIO[Expr[F[A]]] = {
    implicit val FAType: Type[F[A]] = FCtor.apply[A]

    parseCaseClassMIO[F[A]]("F[A]").flatMap { caseClass =>
      val fields = caseClass.primaryConstructor.totalParameters.flatten.toList

      val fieldExprsMIO: MIO[List[(String, Expr_??)]] = fields.traverse { case (fieldName, param) =>
        import param.tpe.Underlying as Field

        if (directFields.contains(fieldName)) {
          MIO.pure((fieldName, aExpr.as_??))
        } else {
          // Invariant field: use Monoid.empty
          ApplicativeTypes.Monoid[Field].summonExprIgnoring().toEither match {
            case Right(monoidExpr) =>
              val emptyValue: Expr[Field] = Expr.quote(Expr.splice(monoidExpr).empty)
              MIO.pure((fieldName, emptyValue.as_??))
            case Left(reason) =>
              failDerivation(
                CatsDerivationError.MissingInstanceForField(
                  "Monoid",
                  fieldName,
                  Type[F[A]].prettyPrint,
                  Some(s"${Field.prettyPrint}: $reason")
                )
              )
          }
        }
      }

      fieldExprsMIO.flatMap { fieldExprs =>
        constructInstanceFree(caseClass.primaryConstructor, "Constructor", "pure result")(fieldExprs.toMap)
          .map(constructExpr => constructExpr.value.asInstanceOf[Expr[F[A]]])
      }
    }
  }

  @scala.annotation.nowarn("msg=is never used|unused implicit parameter")
  private def deriveApplicativeMapBody[F[_], A, B](
      FCtor: Type.Ctor1[F],
      directFields: Set[String],
      faExpr: Expr[F[A]],
      fExpr: Expr[A => B]
  )(implicit AType: Type[A], BType: Type[B]): MIO[Expr[F[B]]] = {
    implicit val FAType: Type[F[A]] = FCtor.apply[A]
    implicit val FBType: Type[F[B]] = FCtor.apply[B]

    parseCaseClassMIO[F[A]]("F[A]").parTuple(parseCaseClassMIO[F[B]]("F[B]")).flatMap { case (caseClass, caseClassB) =>
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

      constructInstanceFree(caseClassB.primaryConstructor, "Constructor", "mapped result")(mappedFields.toMap)
        .map(constructExpr => constructExpr.value.asInstanceOf[Expr[F[B]]])
    }
  }

  @scala.annotation.nowarn("msg=is never used|unused implicit parameter")
  private def deriveApplicativeApBody[F[_]](
      caseClass: CaseClass[F[Any]],
      directFields: Set[String],
      ffExpr: Expr[F[Any]],
      faExpr: Expr[F[Any]]
  )(implicit FCtor: Type.Ctor1[F], FAnyType: Type[F[Any]], AnyType: Type[Any]): MIO[Expr[F[Any]]] = {
    val fieldsFF = caseClass.caseFieldValuesAt(ffExpr).toList
    val fieldsFA = caseClass.caseFieldValuesAt(faExpr).toList

    val apFieldsMIO: MIO[List[(String, Expr_??)]] =
      fieldsFF.zip(fieldsFA).traverse { case ((fieldName, ffFieldValue), (_, faFieldValue)) =>
        import ffFieldValue.Underlying as Field
        val ffField = ffFieldValue.value.asInstanceOf[Expr[Field]]
        val faField = faFieldValue.value.asInstanceOf[Expr[Field]]

        if (directFields.contains(fieldName)) {
          val applied: Expr[Any] = Expr.quote(
            Expr.splice(ffField).asInstanceOf[Any => Any].apply(Expr.splice(faField).asInstanceOf[Any])
          )
          MIO.pure((fieldName, applied.as_??))
        } else {
          ApplicativeTypes.Semigroup[Field].summonExprIgnoring().toEither match {
            case Right(sgExpr) =>
              val combined: Expr[Field] = Expr.quote(
                Expr.splice(sgExpr).combine(Expr.splice(ffField), Expr.splice(faField))
              )
              MIO.pure((fieldName, combined.as_??))
            case Left(reason) =>
              failDerivation(
                CatsDerivationError.MissingInstanceForField(
                  "Semigroup",
                  fieldName,
                  Type[F[Any]].prettyPrint,
                  Some(s"${Field.prettyPrint}: $reason")
                )
              )
          }
        }
      }

    apFieldsMIO.flatMap { apFields =>
      constructInstanceFree(caseClass.primaryConstructor, "Constructor", "ap result")(apFields.toMap)
        .map(constructExpr => constructExpr.value.asInstanceOf[Expr[F[Any]]])
    }
  }

  protected object ApplicativeTypes {
    val Any: Type[Any] = Type.of[Any]
    val Int: Type[Int] = Type.of[Int]
    val String: Type[String] = Type.of[String]
    def Semigroup: Type.Ctor1[cats.kernel.Semigroup] = Type.Ctor1.of[cats.kernel.Semigroup]
    def Monoid: Type.Ctor1[cats.kernel.Monoid] = Type.Ctor1.of[cats.kernel.Monoid]
    val LogDerivation: Type[hearth.kindlings.catsderivation.LogDerivation] =
      Type.of[hearth.kindlings.catsderivation.LogDerivation]
  }

  def shouldWeLogApplicativeDerivation: Boolean = {
    implicit val LogDerivationType: Type[LogDerivation] = ApplicativeTypes.LogDerivation
    Expr.summonImplicit[LogDerivation].isDefined || (for {
      data <- Environment.typedSettings.toOption
      cd <- data.get("catsDerivation")
      shouldLog <- cd.get("logDerivation").flatMap(_.asBoolean)
    } yield shouldLog).getOrElse(false)
  }
}

sealed private[compiletime] trait ApplicativeDerivationError
    extends util.control.NoStackTrace
    with Product
    with Serializable {
  def message: String
  override def getMessage(): String = message
}
private[compiletime] object ApplicativeDerivationError {
  final case class UnsupportedType(tpeName: String, reasons: List[String]) extends ApplicativeDerivationError {
    override def message: String =
      s"The type constructor $tpeName was not handled by any Applicative derivation rule:\n${reasons.mkString("\n")}"
  }
}
