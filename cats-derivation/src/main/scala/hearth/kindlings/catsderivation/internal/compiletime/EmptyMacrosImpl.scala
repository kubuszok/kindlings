package hearth.kindlings.catsderivation.internal.compiletime

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

import hearth.kindlings.catsderivation.LogDerivation

/** Empty derivation: product (all fields: Empty) and coproduct (exactly one variant: Empty). */
trait EmptyMacrosImpl { this: MacroCommons & StdExtensions =>

  @scala.annotation.nowarn("msg=is never used")
  def deriveEmpty[A: Type]: Expr[alleycats.Empty[A]] = {
    val macroName = "Empty.derived"
    implicit val EmptyA: Type[alleycats.Empty[A]] = EmptyTypes.Empty[A]
    val selfType: Option[??] = Some(Type[A].as_??)

    deriveEmptyEntrypoint[A, alleycats.Empty[A]](macroName, selfType) { emptyExpr =>
      Expr.quote {
        new alleycats.Empty[A] {
          def empty: A = Expr.splice(emptyExpr)
        }
      }
    }
  }

  private def deriveEmptyEntrypoint[A: Type, Out: Type](macroName: String, selfType: Option[??])(
      adapt: Expr[A] => Expr[Out]
  ): Expr[Out] = {
    if (Type[A] =:= Type.of[Nothing].asInstanceOf[Type[A]] || Type[A] =:= Type.of[Any].asInstanceOf[Type[A]])
      Environment.reportErrorAndAbort(
        s"$macroName: type parameter was inferred as ${Type[A].prettyPrint}, which is likely unintended."
      )

    Log
      .namedScope(s"Deriving Empty[${Type[A].prettyPrint}] at: ${Environment.currentPosition.prettyPrint}") {
        MIO.scoped { runSafe =>
          runSafe {
            for {
              _ <- Environment.loadStandardExtensions().toMIO(allowFailures = false)
              result <- deriveEmptyForType[A](selfType)
            } yield adapt(result)
          }
        }
      }
      .flatTap(result => Log.info(s"Derived final result: ${result.prettyPrint}"))
      .runToExprOrFail(
        macroName,
        infoRendering = if (shouldWeLogEmptyDerivation) RenderFrom(Log.Level.Info) else DontRender,
        errorRendering = if (shouldWeLogEmptyDerivation) RenderFrom(Log.Level.Info) else DontRender
      ) { (errorLogs, errors) =>
        val errorsRendered = errors.map(e => "  - " + e.getMessage).mkString("\n")
        val hint =
          "Enable debug logging with: import hearth.kindlings.catsderivation.debug.logDerivationForCatsDerivation"
        if (errorLogs.nonEmpty) s"Macro derivation failed:\n$errorsRendered\nlogs:\n$errorLogs\n$hint"
        else s"Macro derivation failed:\n$errorsRendered\n$hint"
      }
  }

  /** Derive the empty value for type A. Does NOT try to summon Empty[A] — that would cause infinite recursion for the
    * top-level type. Instead goes directly to structural derivation (case class or enum).
    */
  private def deriveEmptyForType[A: Type](selfType: Option[??]): MIO[Expr[A]] =
    Log.namedScope(s"Deriving Empty for ${Type[A].prettyPrint}") {
      CaseClass.parse[A].toEither match {
        case Right(caseClass) => deriveEmptyCaseClass[A](caseClass, selfType)
        case Left(_)          =>
          Enum.parse[A].toEither match {
            case Right(enumm) => deriveEmptyEnum[A](enumm, selfType)
            case Left(reason) =>
              MIO.fail(
                new RuntimeException(
                  s"Cannot derive Empty for ${Type[A].prettyPrint}: not a case class and not an enum. $reason"
                )
              )
          }
      }
    }

  @scala.annotation.nowarn("msg=is never used")
  private def deriveEmptyCaseClass[A: Type](
      caseClass: CaseClass[A],
      selfType: Option[??]
  ): MIO[Expr[A]] = {
    val constructor = caseClass.primaryConstructor
    val fields = constructor.parameters.flatten.toList

    val emptyFields: List[(String, Expr_??)] = fields.map { case (fieldName, param) =>
      import param.tpe.Underlying as Field
      val emptyExpr: Expr[Field] = summonEmptyFor[Field](fieldName, selfType)
      (fieldName, emptyExpr.as_??)
    }

    caseClass.primaryConstructor(emptyFields.toMap) match {
      case Right(constructExpr) => MIO.pure(constructExpr)
      case Left(error)          =>
        MIO.fail(new RuntimeException(s"Cannot construct empty ${Type[A].prettyPrint}: $error"))
    }
  }

  @scala.annotation.nowarn("msg=is never used|is unchecked")
  private def deriveEmptyEnum[A: Type](
      enumm: Enum[A],
      selfType: Option[??]
  ): MIO[Expr[A]] = {
    val children = enumm.exhaustiveChildren match {
      case Some(m) => m.toList.map(_._2)
      case None    =>
        return MIO.fail(
          new RuntimeException(s"Cannot derive Empty for ${Type[A].prettyPrint}: no children found")
        )
    }

    val emptyVariants: List[Expr[A]] = children.flatMap { (child: ??<:[A]) =>
      val childType = child.Underlying
      summonEmptyExprOpt(childType, selfType).map { instanceExpr =>
        implicit val ct: Type[child.Underlying] = childType
        Expr.quote(Expr.splice(instanceExpr).empty.asInstanceOf[A])
      }
    }

    emptyVariants match {
      case List(singleEmpty) => MIO.pure(singleEmpty)
      case Nil               =>
        MIO.fail(
          new RuntimeException(
            s"Cannot derive Empty for ${Type[A].prettyPrint}: no variant has an Empty instance"
          )
        )
      case _ =>
        MIO.fail(
          new RuntimeException(
            s"Cannot derive Empty for ${Type[A].prettyPrint}: multiple variants have Empty instances (need exactly one)"
          )
        )
    }
  }

  /** Summon Empty[Field] for a field type, skipping the self-type to avoid circular references. */
  @scala.annotation.nowarn("msg=is never used")
  private def summonEmptyFor[Field: Type](fieldName: String, selfType: Option[??]): Expr[Field] = {
    val emptyFieldType = EmptyTypes.Empty[Field]
    val result =
      if (selfType.exists(_.Underlying =:= Type[Field]))
        emptyFieldType.summonExprIgnoring().toEither.left.map(_ => "self-type skipped")
      else
        emptyFieldType.summonExprIgnoring().toEither

    result match {
      case Right(m)     => Expr.quote(Expr.splice(m).empty)
      case Left(reason) =>
        throw new RuntimeException(
          s"No Empty instance found for field $fieldName: ${Type[Field].prettyPrint}: $reason"
        )
    }
  }

  /** Try to summon Empty[A] optionally, skipping self-type. */
  @scala.annotation.nowarn("msg=is never used")
  private def summonEmptyExprOpt[A](tpe: Type[A], selfType: Option[??]): Option[Expr[alleycats.Empty[A]]] = {
    implicit val t: Type[A] = tpe
    if (selfType.exists(_.Underlying =:= Type[A])) None
    else EmptyTypes.Empty[A].summonExprIgnoring().toOption
  }

  protected object EmptyTypes {
    def Empty: Type.Ctor1[alleycats.Empty] = Type.Ctor1.of[alleycats.Empty]
    val LogDerivation: Type[hearth.kindlings.catsderivation.LogDerivation] =
      Type.of[hearth.kindlings.catsderivation.LogDerivation]
  }

  def shouldWeLogEmptyDerivation: Boolean = {
    implicit val LogDerivationType: Type[LogDerivation] = EmptyTypes.LogDerivation
    def logDerivationImported = Expr.summonImplicit[LogDerivation].isDefined
    def logDerivationSetGlobally = (for {
      data <- Environment.typedSettings.toOption
      catsDerivation <- data.get("catsDerivation")
      shouldLog <- catsDerivation.get("logDerivation").flatMap(_.asBoolean)
    } yield shouldLog).getOrElse(false)
    logDerivationImported || logDerivationSetGlobally
  }
}
