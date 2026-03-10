package hearth.kindlings.catsderivation.internal.compiletime

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

import hearth.kindlings.catsderivation.LogDerivation

/** ConsK derivation: constructs F[A] by prepending a head element to an existing F[A].
  *
  * Uses the "carry and absorb" algorithm:
  *   - Walk fields left to right with a "carried" element (starting with hd)
  *   - Direct A fields: shift (field = carried, old value becomes new carried)
  *   - Nested G[A] fields with ConsK[G]: absorb (field = ConsK[G].cons(carried, old), carried cleared)
  *   - Invariant or non-absorbable fields: copy from tl
  *   - Error if carried element not absorbed by end
  *
  * Uses erased approach: builds body for F[Any] with Any, wraps with asInstanceOf.
  */
trait ConsKMacrosImpl { this: MacroCommons & StdExtensions =>

  /** Bridge method: summon ConsK for the type constructor of a nested field type.
    *
    * Given a field type like `List[Int]` (from the Int probe), extract the type constructor (`List`) and summon
    * `ConsK[List]`. Returns the summoned instance as `Expr[Any]`, or None if not available.
    *
    * @param fieldType
    *   The field type from one of the probes (cast to Type[Any]). The actual type is e.g. List[Int].
    */
  protected def summonConsKForFieldType(fieldType: Type[Any]): Option[Expr[Any]]

  @scala.annotation.nowarn("msg=is never used|unused explicit parameter")
  def deriveConsK[F[_]](FCtor0: Type.Ctor1[F], ConsKFType: Type[alleycats.ConsK[F]]): Expr[alleycats.ConsK[F]] = {
    val macroName = "ConsK.derived"

    implicit val FCtor: Type.Ctor1[F] = FCtor0
    implicit val ConsKFT: Type[alleycats.ConsK[F]] = ConsKFType
    implicit val AnyType: Type[Any] = ConsKTypes.Any
    implicit val FAnyType: Type[F[Any]] = FCtor.apply[Any]

    Log
      .namedScope(s"Deriving ConsK at: ${Environment.currentPosition.prettyPrint}") {
        CaseClass.parse[F[Any]].toEither match {
          case Right(caseClass) =>
            MIO.scoped { runSafe =>
              implicit val IntType: Type[Int] = ConsKTypes.Int
              implicit val StringType: Type[String] = ConsKTypes.String

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
              val nestedFieldConsKs = scala.collection.mutable.Map.empty[String, Expr[Any]]

              fieldsInt.zip(fieldsString).foreach { case ((name, pInt), (_, pString)) =>
                val tInt = pInt.tpe.Underlying
                val tString = pString.tpe.Underlying
                if (tInt =:= IntType && tString =:= StringType) {
                  directFields += name
                } else if (tInt =:= tString) {
                  // Invariant field — copy from tl
                } else {
                  // Nested field — try to summon ConsK for its container
                  summonConsKForFieldType(tInt.asInstanceOf[Type[Any]]) match {
                    case Some(consKExpr) => nestedFieldConsKs += (name -> consKExpr)
                    case None            => // No ConsK available — treat as non-absorbable (copy from tl)
                  }
                }
              }

              if (directFields.isEmpty && nestedFieldConsKs.isEmpty) {
                throw new RuntimeException(
                  "Cannot derive ConsK: no type-parameter-dependent fields found. " +
                    "Need at least one field of type A or G[A] where ConsK[G] exists."
                )
              }

              val directFieldSet: Set[String] = directFields.toSet
              val nestedFieldMap: Map[String, Expr[Any]] = nestedFieldConsKs.toMap

              val doCons: (Expr[Any], Expr[F[Any]]) => Expr[F[Any]] = (hdExpr, tlExpr) =>
                runSafe {
                  for {
                    _ <- Environment.loadStandardExtensions().toMIO(allowFailures = false)
                    result <- deriveConsKBody[F](caseClass, directFieldSet, nestedFieldMap, hdExpr, tlExpr)
                  } yield result
                }

              Expr.quote {
                new alleycats.ConsK[F] {
                  def cons[A](hd: A, tl: F[A]): F[A] = {
                    val anyHd: Any = hd.asInstanceOf[Any]
                    val anyTl: F[Any] = tl.asInstanceOf[F[Any]]
                    val _ = anyHd
                    val _ = anyTl
                    Expr.splice(doCons(Expr.quote(anyHd), Expr.quote(anyTl))).asInstanceOf[F[A]]
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
        infoRendering = if (shouldWeLogConsKDerivation) RenderFrom(Log.Level.Info) else DontRender,
        errorRendering = if (shouldWeLogConsKDerivation) RenderFrom(Log.Level.Info) else DontRender
      ) { (errorLogs, errors) =>
        val errorsRendered = errors.map(e => "  - " + e.getMessage).mkString("\n")
        val hint =
          "Enable debug logging with: import hearth.kindlings.catsderivation.debug.logDerivationForCatsDerivation"
        if (errorLogs.nonEmpty) s"Macro derivation failed:\n$errorsRendered\nlogs:\n$errorLogs\n$hint"
        else s"Macro derivation failed:\n$errorsRendered\n$hint"
      }
  }

  @scala.annotation.nowarn("msg=is never used|unused implicit parameter")
  private def deriveConsKBody[F[_]](
      caseClass: CaseClass[F[Any]],
      directFields: Set[String],
      nestedFieldConsKs: Map[String, Expr[Any]],
      hdExpr: Expr[Any],
      tlExpr: Expr[F[Any]]
  )(implicit FCtor: Type.Ctor1[F], FAnyType: Type[F[Any]], AnyType: Type[Any]): MIO[Expr[F[Any]]] = {
    val fields = caseClass.caseFieldValuesAt(tlExpr).toList

    var carried: Option[Expr[Any]] = Some(hdExpr)
    val resultFields = scala.collection.mutable.ListBuffer.empty[(String, Expr_??)]

    fields.foreach { case (fieldName, fieldValue) =>
      import fieldValue.Underlying as Field
      val fieldExpr = fieldValue.value.asInstanceOf[Expr[Field]]

      carried match {
        case Some(carr) =>
          if (directFields.contains(fieldName)) {
            // Shift: field gets carried value (both are Any), old field value becomes new carried
            resultFields += ((fieldName, carr.as_??))
            carried = Some(fieldExpr.upcast[Any])
          } else if (nestedFieldConsKs.contains(fieldName)) {
            // Absorb: use ConsK to cons carried into this field, cast result to field type
            val consKExpr = nestedFieldConsKs(fieldName)
            val absorbed: Expr[Field] = Expr.quote {
              hearth.kindlings.catsderivation.internal.runtime.ConsKRuntime
                .cons(
                  Expr.splice(consKExpr),
                  Expr.splice(carr),
                  Expr.splice(fieldExpr.upcast[Any])
                )
                .asInstanceOf[Field]
            }
            resultFields += ((fieldName, absorbed.as_??))
            carried = None
          } else {
            // Invariant or non-absorbable nested: keep original typed expression
            resultFields += ((fieldName, fieldExpr.as_??))
          }
        case None =>
          // After absorption: copy all remaining fields preserving original type
          resultFields += ((fieldName, fieldExpr.as_??))
      }
    }

    if (carried.isDefined) {
      MIO.fail(
        new RuntimeException(
          "Cannot derive ConsK: no container field found to absorb the consed element. " +
            "Need at least one field of type G[A] where ConsK[G] exists (e.g., List[A], Vector[A])."
        )
      )
    } else {
      caseClass.primaryConstructor(resultFields.toMap) match {
        case Right(constructExpr) => MIO.pure(constructExpr)
        case Left(error)          =>
          MIO.fail(new RuntimeException(s"Cannot construct ConsK result: $error"))
      }
    }
  }

  protected object ConsKTypes {
    val Any: Type[Any] = Type.of[Any]
    val Int: Type[Int] = Type.of[Int]
    val String: Type[String] = Type.of[String]
    val LogDerivation: Type[hearth.kindlings.catsderivation.LogDerivation] =
      Type.of[hearth.kindlings.catsderivation.LogDerivation]
  }

  def shouldWeLogConsKDerivation: Boolean = {
    implicit val LogDerivationType: Type[LogDerivation] = ConsKTypes.LogDerivation
    Expr.summonImplicit[LogDerivation].isDefined || (for {
      data <- Environment.typedSettings.toOption
      cd <- data.get("catsDerivation")
      shouldLog <- cd.get("logDerivation").flatMap(_.asBoolean)
    } yield shouldLog).getOrElse(false)
  }
}
