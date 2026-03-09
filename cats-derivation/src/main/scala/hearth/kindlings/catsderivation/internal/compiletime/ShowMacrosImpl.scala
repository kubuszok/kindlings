package hearth.kindlings.catsderivation.internal.compiletime

import hearth.MacroCommons
import hearth.fp.data.NonEmptyList
import hearth.fp.effect.*
import hearth.fp.syntax.*
import hearth.std.*

import hearth.kindlings.catsderivation.LogDerivation

trait ShowMacrosImpl { this: MacroCommons & StdExtensions =>

  // Entrypoint

  @scala.annotation.nowarn("msg=is never used")
  def deriveShow[A: Type]: Expr[cats.Show[A]] = {
    val macroName = "Show.derived"
    implicit val ShowA: Type[cats.Show[A]] = ShowTypes.Show[A]
    implicit val StringType: Type[String] = ShowTypes.String
    val selfType: Option[??] = Some(Type[A].as_??)

    deriveShowFromCtxAndAdaptForEntrypoint[A, cats.Show[A]](macroName) { fromCtx =>
      Expr.quote {
        new cats.Show[A] {
          def show(value: A): String = {
            val _ = value
            Expr.splice {
              fromCtx(ShowCtx.from(Expr.quote(value), derivedType = selfType))
            }
          }
        }
      }
    }
  }

  // CPS-style entrypoint that handles logging, error reporting and cache

  private def deriveShowFromCtxAndAdaptForEntrypoint[A: Type, Out: Type](macroName: String)(
      provideCtxAndAdapt: (ShowCtx[A] => Expr[String]) => Expr[Out]
  ): Expr[Out] = {
    if (Type[A] =:= Type.of[Nothing].asInstanceOf[Type[A]] || Type[A] =:= Type.of[Any].asInstanceOf[Type[A]])
      Environment.reportErrorAndAbort(
        s"$macroName: type parameter was inferred as ${Type[A].prettyPrint}, which is likely unintended.\n" +
          s"Provide an explicit type parameter, e.g.: $macroName[MyType]\n" +
          "or add a type ascription to the result variable."
      )

    Log
      .namedScope(
        s"Deriving Show[${Type[A].prettyPrint}] at: ${Environment.currentPosition.prettyPrint}"
      ) {
        MIO.scoped { runSafe =>
          val fromCtx: ShowCtx[A] => Expr[String] = (ctx: ShowCtx[A]) =>
            runSafe {
              for {
                _ <- Environment.loadStandardExtensions().toMIO(allowFailures = false)
                result <- deriveShowRecursively[A](using ctx)
                cache <- ctx.cache.get
              } yield cache.toValDefs.use(_ => result)
            }
          provideCtxAndAdapt(fromCtx)
        }
      }
      .flatTap(result => Log.info(s"Derived final result: ${result.prettyPrint}"))
      .runToExprOrFail(
        macroName,
        infoRendering = if (shouldWeLogShowDerivation) RenderFrom(Log.Level.Info) else DontRender,
        errorRendering = if (shouldWeLogShowDerivation) RenderFrom(Log.Level.Info) else DontRender
      ) { (errorLogs, errors) =>
        val errorsRendered = errors.map(e => "  - " + e.getMessage).mkString("\n")
        val hint =
          "Enable debug logging with: import hearth.kindlings.catsderivation.debug.logDerivationForCatsDerivation"
        if (errorLogs.nonEmpty)
          s"Macro derivation failed with the following errors:\n$errorsRendered\nand the following logs:\n$errorLogs\n$hint"
        else
          s"Macro derivation failed with the following errors:\n$errorsRendered\n$hint"
      }
  }

  // Context

  final case class ShowCtx[A](
      tpe: Type[A],
      value: Expr[A],
      cache: MLocal[ValDefsCache],
      derivedType: Option[??]
  ) {
    def nest[B: Type](newValue: Expr[B]): ShowCtx[B] = ShowCtx(Type[B], newValue, cache, derivedType)
  }
  object ShowCtx {
    def from[A: Type](value: Expr[A], derivedType: Option[??]): ShowCtx[A] =
      ShowCtx(Type[A], value, ValDefsCache.mlocal, derivedType)
  }

  def sctx[A](implicit A: ShowCtx[A]): ShowCtx[A] = A
  implicit def showCtxType[A: ShowCtx]: Type[A] = sctx.tpe

  abstract class ShowDerivationRule(val name: String) extends Rule {
    def apply[A: ShowCtx]: MIO[Rule.Applicability[Expr[String]]]
  }

  // Recursive derivation

  def deriveShowRecursively[A: ShowCtx]: MIO[Expr[String]] =
    Log.namedScope(s"Deriving Show for ${Type[A].prettyPrint}") {
      Rules(
        ShowUseCachedRule,
        ShowUseImplicitRule,
        ShowBuiltInRule,
        ShowValueTypeRule,
        ShowOptionRule,
        ShowMapRule,
        ShowCollectionRule,
        ShowSingletonRule,
        ShowCaseClassRule,
        ShowEnumRule
      )(_[A]).flatMap {
        case Right(result) =>
          Log.info(s"Derived Show for ${Type[A].prettyPrint}: ${result.prettyPrint}") >> MIO.pure(result)
        case Left(reasons) =>
          val reasonsStrings = reasons.toListMap
            .removed(ShowUseCachedRule)
            .view
            .map { case (rule, reasons) =>
              if (reasons.isEmpty) s"The rule ${rule.name} was not applicable"
              else s" - The rule ${rule.name} was not applicable: ${reasons.mkString(", ")}"
            }
            .toList
          val err = ShowDerivationError.UnsupportedType(Type[A].prettyPrint, reasonsStrings)
          Log.error(err.message) >> MIO.fail(err)
      }
    }

  // Rules

  object ShowUseCachedRule extends ShowDerivationRule("use cached Show") {

    def apply[A: ShowCtx]: MIO[Rule.Applicability[Expr[String]]] = {
      implicit val ShowA: Type[cats.Show[A]] = ShowTypes.Show[A]
      sctx.cache.get0Ary[cats.Show[A]]("cached-show-instance").flatMap {
        case Some(instance) =>
          Log.info(s"Using cached Show instance for ${Type[A].prettyPrint}") >>
            MIO.pure(Rule.matched(Expr.quote(Expr.splice(instance).show(Expr.splice(sctx.value)))))
        case None =>
          implicit val StringType: Type[String] = ShowTypes.String
          sctx.cache.get1Ary[A, String]("cached-show-method").flatMap {
            case Some(helper) =>
              Log.info(s"Using cached Show helper for ${Type[A].prettyPrint}") >>
                MIO.pure(Rule.matched(helper(sctx.value)))
            case None =>
              MIO.pure(Rule.yielded(s"No cached definition for ${Type[A].prettyPrint}"))
          }
      }
    }
  }

  object ShowUseImplicitRule extends ShowDerivationRule("use implicit Show") {

    def apply[A: ShowCtx]: MIO[Rule.Applicability[Expr[String]]] = {
      implicit val ShowA: Type[cats.Show[A]] = ShowTypes.Show[A]
      Log.info(s"Searching implicit Show[${Type[A].prettyPrint}]") >> {
        if (sctx.derivedType.exists(_.Underlying =:= Type[A]))
          MIO.pure(Rule.yielded(s"${Type[A].prettyPrint} is the self-type, skipping"))
        else
          ShowTypes.Show[A].summonExprIgnoring().toEither match {
            case Right(instanceExpr) =>
              Log.info(s"Found implicit Show[${Type[A].prettyPrint}]: ${instanceExpr.prettyPrint}") >>
                sctx.cache.buildCachedWith(
                  "cached-show-instance",
                  ValDefBuilder.ofLazy[cats.Show[A]](s"show_${Type[A].shortName}")
                )(_ => instanceExpr) >>
                ShowUseCachedRule[A]
            case Left(reason) =>
              MIO.pure(Rule.yielded(s"No implicit Show[${Type[A].prettyPrint}]: $reason"))
          }
      }
    }
  }

  object ShowBuiltInRule extends ShowDerivationRule("built-in Show for primitives") {

    def apply[A: ShowCtx]: MIO[Rule.Applicability[Expr[String]]] = {
      implicit val StringType: Type[String] = ShowTypes.String
      Log.info(s"Checking built-in Show for ${Type[A].prettyPrint}") >> MIO {
        if (Type[A] <:< ShowTypes.String)
          Rule.matched(sctx.value.upcast[String])
        else if (
          Type[A] <:< ShowTypes.Boolean || Type[A] <:< ShowTypes.Byte || Type[A] <:< ShowTypes.Short ||
          Type[A] <:< ShowTypes.Int || Type[A] <:< ShowTypes.Long || Type[A] <:< ShowTypes.Float ||
          Type[A] <:< ShowTypes.Double || Type[A] <:< ShowTypes.Char
        )
          Rule.matched(Expr.quote(Expr.splice(sctx.value).toString))
        else
          Rule.yielded(s"${Type[A].prettyPrint} is not a built-in type")
      }
    }
  }

  @scala.annotation.nowarn("msg=is never used")
  object ShowValueTypeRule extends ShowDerivationRule("Show as value type") {

    def apply[A: ShowCtx]: MIO[Rule.Applicability[Expr[String]]] =
      Log.info(s"Checking value type for Show[${Type[A].prettyPrint}]") >> {
        Type[A] match {
          case IsValueType(isValueType) =>
            import isValueType.Underlying as Inner
            val unwrapped = isValueType.value.unwrap(sctx.value)
            deriveShowRecursively[Inner](using sctx.nest(unwrapped)).map(Rule.matched(_))
          case _ =>
            MIO.pure(Rule.yielded(s"${Type[A].prettyPrint} is not a value type"))
        }
      }
  }

  @scala.annotation.nowarn("msg=is never used")
  object ShowOptionRule extends ShowDerivationRule("Show as Option") {

    def apply[A: ShowCtx]: MIO[Rule.Applicability[Expr[String]]] =
      Log.info(s"Checking Option for Show[${Type[A].prettyPrint}]") >> {
        Type[A] match {
          case IsOption(isOption) =>
            import isOption.Underlying as Inner
            implicit val StringType: Type[String] = ShowTypes.String
            LambdaBuilder
              .of1[Inner]("inner")
              .traverse { innerExpr =>
                deriveShowRecursively[Inner](using sctx.nest(innerExpr))
              }
              .map { builder =>
                val lambda = builder.build[String]
                Rule.matched(
                  isOption.value.fold[String](sctx.value)(
                    onEmpty = Expr("None"),
                    onSome = innerExpr => Expr.quote("Some(" + Expr.splice(lambda).apply(Expr.splice(innerExpr)) + ")")
                  )
                )
              }
          case _ =>
            MIO.pure(Rule.yielded(s"${Type[A].prettyPrint} is not an Option"))
        }
      }
  }

  @scala.annotation.nowarn("msg=is never used")
  object ShowMapRule extends ShowDerivationRule("Show as map") {

    def apply[A: ShowCtx]: MIO[Rule.Applicability[Expr[String]]] =
      Log.info(s"Checking map for Show[${Type[A].prettyPrint}]") >> {
        Type[A] match {
          case IsMap(isMapExist) =>
            import isMapExist.Underlying as Pair
            deriveShowMap[A, Pair](isMapExist.value)
          case _ =>
            MIO.pure(Rule.yielded(s"${Type[A].prettyPrint} is not a map"))
        }
      }

    @scala.annotation.nowarn("msg=is never used")
    private def deriveShowMap[A: ShowCtx, Pair: Type](
        isMap: IsMapOf[A, Pair]
    ): MIO[Rule.Applicability[Expr[String]]] = {
      import isMap.{Key, Value}
      implicit val StringType: Type[String] = ShowTypes.String
      val name = Type[A].shortName
      val iterableExpr = isMap.asIterable(sctx.value)

      LambdaBuilder
        .of1[Pair]("pair")
        .traverse { pairExpr =>
          val keyExpr = isMap.key(pairExpr)
          val valueExpr = isMap.value(pairExpr)
          for {
            keyStr <- deriveShowRecursively[Key](using sctx.nest(keyExpr))
            valueStr <- deriveShowRecursively[Value](using sctx.nest(valueExpr))
          } yield Expr.quote(Expr.splice(keyStr) + " -> " + Expr.splice(valueStr))
        }
        .map { builder =>
          val lambda = builder.build[String]
          Rule.matched(Expr.quote {
            val items = Expr.splice(iterableExpr).map(pair => Expr.splice(lambda).apply(pair))
            Expr.splice(Expr(name)) + "(" + items.mkString(", ") + ")"
          })
        }
    }
  }

  @scala.annotation.nowarn("msg=is never used")
  object ShowCollectionRule extends ShowDerivationRule("Show as collection") {

    def apply[A: ShowCtx]: MIO[Rule.Applicability[Expr[String]]] =
      Log.info(s"Checking collection for Show[${Type[A].prettyPrint}]") >> {
        Type[A] match {
          case IsCollection(isCollectionExist) =>
            import isCollectionExist.Underlying as Item
            deriveShowCollection[A, Item](isCollectionExist.value)
          case _ =>
            MIO.pure(Rule.yielded(s"${Type[A].prettyPrint} is not a collection"))
        }
      }

    private def deriveShowCollection[A: ShowCtx, Item: Type](
        isCollection: IsCollectionOf[A, Item]
    ): MIO[Rule.Applicability[Expr[String]]] = {
      implicit val StringType: Type[String] = ShowTypes.String
      val name = Type[A].shortName
      val iterableExpr = isCollection.asIterable(sctx.value)

      LambdaBuilder
        .of1[Item]("item")
        .traverse { itemExpr =>
          deriveShowRecursively[Item](using sctx.nest(itemExpr))
        }
        .map { builder =>
          val lambda = builder.build[String]
          Rule.matched(Expr.quote {
            val items = Expr.splice(iterableExpr).map(item => Expr.splice(lambda).apply(item))
            Expr.splice(Expr(name)) + "(" + items.mkString(", ") + ")"
          })
        }
    }
  }

  object ShowCaseClassRule extends ShowDerivationRule("Show as case class") {

    def apply[A: ShowCtx]: MIO[Rule.Applicability[Expr[String]]] =
      Log.info(s"Checking case class for Show[${Type[A].prettyPrint}]") >> {
        CaseClass.parse[A].toEither match {
          case Right(caseClass) =>
            implicit val StringType: Type[String] = ShowTypes.String
            val defBuilder = ValDefBuilder.ofDef1[A, String](s"show_${Type[A].shortName}")
            for {
              _ <- sctx.cache.forwardDeclare("cached-show-method", defBuilder)
              _ <- MIO.scoped { runSafe =>
                runSafe(sctx.cache.buildCachedWith("cached-show-method", defBuilder) { case (_, value) =>
                  runSafe(deriveCaseClassShow[A](caseClass, value))
                })
              }
              result <- ShowUseCachedRule[A]
            } yield result
          case Left(reason) =>
            MIO.pure(Rule.yielded(reason))
        }
      }

    private def deriveCaseClassShow[A: ShowCtx](
        caseClass: CaseClass[A],
        value: Expr[A]
    ): MIO[Expr[String]] = {
      val name = Type[A].shortName

      NonEmptyList.fromList(caseClass.caseFieldValuesAt(value).toList) match {
        case Some(fieldValues) =>
          fieldValues
            .traverse { case (fieldName, fieldValue) =>
              import fieldValue.{Underlying as Field, value as fieldExpr}
              Log.namedScope(s"Deriving Show for $fieldName: ${Field.prettyPrint}") {
                deriveShowRecursively[Field](using sctx.nest(fieldExpr)).map(r => (fieldName, r))
              }
            }
            .map { fields =>
              val fieldStrings: List[Expr[String]] = fields.toList.map { case (fieldName, fieldResult) =>
                Expr.quote(Expr.splice(Expr(fieldName)) + " = " + Expr.splice(fieldResult))
              }
              fieldStrings match {
                case head :: tail =>
                  val combined = tail.foldLeft(head) { (acc, next) =>
                    Expr.quote(Expr.splice(acc) + ", " + Expr.splice(next))
                  }
                  Expr.quote(Expr.splice(Expr(name)) + "(" + Expr.splice(combined) + ")")
                case Nil =>
                  Expr(s"$name()")
              }
            }
        case None =>
          MIO.pure(Expr(s"$name()"))
      }
    }
  }

  object ShowSingletonRule extends ShowDerivationRule("Show as singleton") {

    def apply[A: ShowCtx]: MIO[Rule.Applicability[Expr[String]]] =
      SingletonValue.parse[A].toEither match {
        case Right(_) =>
          val name = Type[A].shortName
          MIO.pure(Rule.matched(Expr(s"$name()")))
        case Left(reason) =>
          MIO.pure(Rule.yielded(reason))
      }
  }

  object ShowEnumRule extends ShowDerivationRule("Show as enum") {

    def apply[A: ShowCtx]: MIO[Rule.Applicability[Expr[String]]] =
      Log.info(s"Checking enum for Show[${Type[A].prettyPrint}]") >> {
        Enum.parse[A].toEither match {
          case Right(enumm) =>
            implicit val StringType: Type[String] = ShowTypes.String
            val defBuilder = ValDefBuilder.ofDef1[A, String](s"show_${Type[A].shortName}")
            for {
              _ <- sctx.cache.forwardDeclare("cached-show-method", defBuilder)
              _ <- MIO.scoped { runSafe =>
                runSafe(sctx.cache.buildCachedWith("cached-show-method", defBuilder) { case (_, value) =>
                  runSafe(deriveEnumShow[A](enumm, value))
                })
              }
              result <- ShowUseCachedRule[A]
            } yield result
          case Left(reason) =>
            MIO.pure(Rule.yielded(reason))
        }
      }

    private def deriveEnumShow[A: ShowCtx](
        enumm: Enum[A],
        value: Expr[A]
    ): MIO[Expr[String]] = {
      implicit val StringType: Type[String] = ShowTypes.String
      enumm
        .matchOn[MIO, String](value) { matched =>
          import matched.{value as caseValue, Underlying as EnumCase}
          Log.namedScope(s"Deriving Show for enum case ${EnumCase.prettyPrint}") {
            deriveShowRecursively[EnumCase](using sctx.nest(caseValue))
          }
        }
        .flatMap {
          case Some(result) => MIO.pure(result)
          case None         =>
            val err = ShowDerivationError.NoChildrenInSealedTrait(Type[A].prettyPrint)
            Log.error(err.message) >> MIO.fail(err)
        }
    }
  }

  // Types helper

  protected object ShowTypes {
    def Show: Type.Ctor1[cats.Show] = Type.Ctor1.of[cats.Show]
    val LogDerivation: Type[LogDerivation] = Type.of[LogDerivation]
    val Boolean: Type[Boolean] = Type.of[Boolean]
    val Byte: Type[Byte] = Type.of[Byte]
    val Short: Type[Short] = Type.of[Short]
    val Int: Type[Int] = Type.of[Int]
    val Long: Type[Long] = Type.of[Long]
    val Float: Type[Float] = Type.of[Float]
    val Double: Type[Double] = Type.of[Double]
    val Char: Type[Char] = Type.of[Char]
    val String: Type[String] = Type.of[String]
  }

  def shouldWeLogShowDerivation: Boolean = {
    implicit val LogDerivationType: Type[LogDerivation] = ShowTypes.LogDerivation
    def logDerivationImported = Expr.summonImplicit[LogDerivation].isDefined

    def logDerivationSetGlobally = (for {
      data <- Environment.typedSettings.toOption
      catsDerivation <- data.get("catsDerivation")
      shouldLog <- catsDerivation.get("logDerivation").flatMap(_.asBoolean)
    } yield shouldLog).getOrElse(false)

    logDerivationImported || logDerivationSetGlobally
  }
}

sealed private[compiletime] trait ShowDerivationError extends util.control.NoStackTrace with Product with Serializable {
  def message: String
  override def getMessage(): String = message
}
private[compiletime] object ShowDerivationError {
  final case class UnsupportedType(tpeName: String, reasons: List[String]) extends ShowDerivationError {
    override def message: String =
      s"The type $tpeName was not handled by any Show derivation rule:\n${reasons.mkString("\n")}"
  }
  final case class NoChildrenInSealedTrait(tpeName: String) extends ShowDerivationError {
    override def message: String =
      s"The type $tpeName does not have any children!"
  }
}
