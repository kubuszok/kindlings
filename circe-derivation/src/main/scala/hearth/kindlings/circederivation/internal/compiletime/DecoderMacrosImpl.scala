package hearth.kindlings.circederivation.internal.compiletime

import hearth.MacroCommons
import hearth.fp.data.NonEmptyList
import hearth.fp.effect.*
import hearth.fp.syntax.*
import hearth.std.*

import hearth.kindlings.circederivation.{Configuration, KindlingsDecoder}
import hearth.kindlings.circederivation.internal.runtime.CirceDerivationUtils
import io.circe.{Decoder, DecodingFailure, HCursor}

trait DecoderMacrosImpl { this: MacroCommons & StdExtensions =>

  // Entrypoints

  @scala.annotation.nowarn("msg=is never used")
  def deriveInlineDecode[A: Type](
      cursorExpr: Expr[HCursor],
      configExpr: Expr[Configuration]
  ): Expr[Either[DecodingFailure, A]] = {
    implicit val EitherT: Type[Either[DecodingFailure, A]] = DTypes.DecoderResult[A]
    implicit val HCursorT: Type[HCursor] = DTypes.HCursor
    implicit val ConfigT: Type[Configuration] = DTypes.Configuration
    implicit val DecodingFailureT: Type[DecodingFailure] = DTypes.DecodingFailure

    deriveDecoderFromCtxAndAdaptForEntrypoint[A, Either[DecodingFailure, A]]("KindlingsDecoder.decode") { fromCtx =>
      ValDefs.createVal[HCursor](cursorExpr).use { cursorVal =>
        ValDefs.createVal[Configuration](configExpr).use { configVal =>
          Expr.quote {
            val _ = Expr.splice(cursorVal)
            val _ = Expr.splice(configVal)
            Expr.splice(fromCtx(DecoderCtx.from(cursorVal, configVal)))
          }
        }
      }
    }
  }

  @scala.annotation.nowarn("msg=is never used")
  def deriveDecoderTypeClass[A: Type](configExpr: Expr[Configuration]): Expr[KindlingsDecoder[A]] = {
    implicit val DecoderA: Type[Decoder[A]] = DTypes.Decoder[A]
    implicit val KindlingsDecoderA: Type[KindlingsDecoder[A]] = DTypes.KindlingsDecoder[A]
    implicit val EitherT: Type[Either[DecodingFailure, A]] = DTypes.DecoderResult[A]
    implicit val HCursorT: Type[HCursor] = DTypes.HCursor
    implicit val ConfigT: Type[Configuration] = DTypes.Configuration
    implicit val DecodingFailureT: Type[DecodingFailure] = DTypes.DecodingFailure

    deriveDecoderFromCtxAndAdaptForEntrypoint[A, KindlingsDecoder[A]]("KindlingsDecoder.derived") { fromCtx =>
      ValDefs.createVal[Configuration](configExpr).use { configVal =>
        Expr.quote {
          val cfg = Expr.splice(configVal)
          new KindlingsDecoder[A] {
            def apply(c: HCursor): Decoder.Result[A] = {
              val _ = c
              Expr.splice {
                fromCtx(DecoderCtx.from(Expr.quote(c), Expr.quote(cfg)))
              }
            }
          }
        }
      }
    }
  }

  // Handles logging, error reporting and prepending "cached" defs and vals to the result.

  def deriveDecoderFromCtxAndAdaptForEntrypoint[A: Type, Out: Type](macroName: String)(
      provideCtxAndAdapt: (DecoderCtx[A] => Expr[Either[DecodingFailure, A]]) => Expr[Out]
  ): Expr[Out] = Log
    .namedScope(
      s"Deriving decoder for ${Type[A].prettyPrint} at: ${Environment.currentPosition.prettyPrint}"
    ) {
      MIO.scoped { runSafe =>
        val fromCtx: (DecoderCtx[A] => Expr[Either[DecodingFailure, A]]) =
          (ctx: DecoderCtx[A]) =>
            runSafe {
              for {
                _ <- Environment.loadStandardExtensions().toMIO(allowFailures = false)
                result <- deriveDecoderRecursively[A](using ctx)
                cache <- ctx.cache.get
              } yield cache.toValDefs.use(_ => result)
            }

        provideCtxAndAdapt(fromCtx)
      }
    }
    .flatTap { result =>
      Log.info(s"Derived final decoder result: ${result.prettyPrint}")
    }
    .runToExprOrFail(
      macroName,
      infoRendering = if (shouldWeLogDecoderDerivation) RenderFrom(Log.Level.Info) else DontRender,
      errorRendering = RenderFrom(Log.Level.Info)
    ) { (errorLogs, errors) =>
      val errorsRendered = errors
        .map { e =>
          e.getMessage.split("\n").toList match {
            case head :: tail => (("  - " + head) :: tail.map("    " + _)).mkString("\n")
            case _            => "  - " + e.getMessage
          }
        }
        .mkString("\n")
      if (errorLogs.nonEmpty)
        s"""Macro derivation failed with the following errors:
           |$errorsRendered
           |and the following logs:
           |$errorLogs""".stripMargin
      else
        s"""Macro derivation failed with the following errors:
           |$errorsRendered""".stripMargin
    }

  def shouldWeLogDecoderDerivation: Boolean = {
    implicit val LogDerivation: Type[KindlingsDecoder.LogDerivation] = DTypes.DecoderLogDerivation
    def logDerivationImported = Expr.summonImplicit[KindlingsDecoder.LogDerivation].isDefined

    def logDerivationSetGlobally = (for {
      data <- Environment.typedSettings.toOption
      circeDerivation <- data.get("circeDerivation")
      shouldLog <- circeDerivation.get("logDerivation").flatMap(_.asBoolean)
    } yield shouldLog).getOrElse(false)

    logDerivationImported || logDerivationSetGlobally
  }

  // Context

  final case class DecoderCtx[A](
      tpe: Type[A],
      cursor: Expr[HCursor],
      config: Expr[Configuration],
      cache: MLocal[ValDefsCache]
  ) {

    def nest[B: Type](newCursor: Expr[HCursor]): DecoderCtx[B] = copy[B](
      tpe = Type[B],
      cursor = newCursor
    )

    def nestInCache(
        newCursor: Expr[HCursor],
        newConfig: Expr[Configuration]
    ): DecoderCtx[A] = copy(
      cursor = newCursor,
      config = newConfig
    )

    def getInstance[B: Type]: MIO[Option[Expr[Decoder[B]]]] = {
      implicit val DecoderB: Type[Decoder[B]] = DTypes.Decoder[B]
      cache.get0Ary[Decoder[B]]("cached-decoder-instance")
    }
    def setInstance[B: Type](instance: Expr[Decoder[B]]): MIO[Unit] = {
      implicit val DecoderB: Type[Decoder[B]] = DTypes.Decoder[B]
      cache.buildCachedWith(
        "cached-decoder-instance",
        ValDefBuilder.ofLazy[Decoder[B]](s"decoder_${Type[B].shortName}")
      )(_ => instance)
    }

    def getHelper[B: Type]: MIO[Option[(Expr[HCursor], Expr[Configuration]) => Expr[Either[DecodingFailure, B]]]] = {
      implicit val ResultB: Type[Either[DecodingFailure, B]] = DTypes.DecoderResult[B]
      implicit val HCursorT: Type[HCursor] = DTypes.HCursor
      implicit val ConfigT: Type[Configuration] = DTypes.Configuration
      cache.get2Ary[HCursor, Configuration, Either[DecodingFailure, B]]("cached-decode-method")
    }
    def setHelper[B: Type](
        helper: (Expr[HCursor], Expr[Configuration]) => MIO[Expr[Either[DecodingFailure, B]]]
    ): MIO[Unit] = {
      implicit val ResultB: Type[Either[DecodingFailure, B]] = DTypes.DecoderResult[B]
      implicit val HCursorT: Type[HCursor] = DTypes.HCursor
      implicit val ConfigT: Type[Configuration] = DTypes.Configuration
      val defBuilder =
        ValDefBuilder.ofDef2[HCursor, Configuration, Either[DecodingFailure, B]](s"decode_${Type[B].shortName}")
      for {
        _ <- cache.forwardDeclare("cached-decode-method", defBuilder)
        _ <- MIO.scoped { runSafe =>
          runSafe(cache.buildCachedWith("cached-decode-method", defBuilder) { case (_, (cursor, config)) =>
            runSafe(helper(cursor, config))
          })
        }
      } yield ()
    }

    override def toString: String =
      s"decode[${tpe.prettyPrint}](cursor = ${cursor.prettyPrint}, config = ${config.prettyPrint})"
  }
  object DecoderCtx {

    def from[A: Type](
        cursor: Expr[HCursor],
        config: Expr[Configuration]
    ): DecoderCtx[A] = DecoderCtx(
      tpe = Type[A],
      cursor = cursor,
      config = config,
      cache = ValDefsCache.mlocal
    )
  }

  def dctx[A](implicit A: DecoderCtx[A]): DecoderCtx[A] = A

  implicit def currentDecoderValueType[A: DecoderCtx]: Type[A] = dctx.tpe

  abstract class DecoderDerivationRule(val name: String) extends Rule {
    def apply[A: DecoderCtx]: MIO[Rule.Applicability[Expr[Either[DecodingFailure, A]]]]
  }

  // The actual derivation logic

  def deriveDecoderRecursively[A: DecoderCtx]: MIO[Expr[Either[DecodingFailure, A]]] =
    Log
      .namedScope(s"Deriving decoder for type ${Type[A].prettyPrint}") {
        Rules(
          DecUseCachedDefWhenAvailableRule,
          DecUseImplicitWhenAvailableRule,
          DecHandleAsValueTypeRule,
          DecHandleAsCaseClassRule,
          DecHandleAsEnumRule
        )(_[A]).flatMap {
          case Right(result) =>
            Log.info(s"Derived decoder for ${Type[A].prettyPrint}: ${result.prettyPrint}") >>
              MIO.pure(result)
          case Left(reasons) =>
            val reasonsStrings = reasons.toListMap
              .removed(DecUseCachedDefWhenAvailableRule)
              .view
              .map { case (rule, reasons) =>
                if (reasons.isEmpty) s"The rule ${rule.name} was not applicable"
                else
                  s" - The rule ${rule.name} was not applicable, for the following reasons: ${reasons.mkString(", ")}"
              }
              .toList
            Log.info(s"Failed to derive decoder for ${Type[A].prettyPrint}:\n${reasonsStrings.mkString("\n")}") >>
              MIO.fail(DecoderDerivationError.UnsupportedType(Type[A].prettyPrint, reasonsStrings))
        }
      }

  // Rules

  object DecUseCachedDefWhenAvailableRule extends DecoderDerivationRule("use cached def when available") {

    def apply[A: DecoderCtx]: MIO[Rule.Applicability[Expr[Either[DecodingFailure, A]]]] =
      Log.info(s"Attempting to use cached decoder for ${Type[A].prettyPrint}") >>
        dctx.getInstance[A].flatMap {
          case Some(instance) => callCachedInstance[A](instance)
          case None           =>
            dctx.getHelper[A].flatMap {
              case Some(helperCall) => callCachedHelper[A](helperCall)
              case None             => yieldUnsupported[A]
            }
        }

    private def callCachedInstance[A: DecoderCtx](
        instance: Expr[Decoder[A]]
    ): MIO[Rule.Applicability[Expr[Either[DecodingFailure, A]]]] =
      Log.info(s"Found cached decoder instance for ${Type[A].prettyPrint}") >> MIO.pure(
        Rule.matched(Expr.quote {
          Expr.splice(instance).apply(Expr.splice(dctx.cursor))
        })
      )

    private def callCachedHelper[A: DecoderCtx](
        helperCall: (Expr[HCursor], Expr[Configuration]) => Expr[Either[DecodingFailure, A]]
    ): MIO[Rule.Applicability[Expr[Either[DecodingFailure, A]]]] =
      Log.info(s"Found cached decoder helper for ${Type[A].prettyPrint}") >> MIO.pure(
        Rule.matched(helperCall(dctx.cursor, dctx.config))
      )

    private def yieldUnsupported[A: DecoderCtx]: MIO[Rule.Applicability[Expr[Either[DecodingFailure, A]]]] =
      MIO.pure(Rule.yielded(s"The type ${Type[A].prettyPrint} does not have a cached decoder"))
  }

  object DecUseImplicitWhenAvailableRule extends DecoderDerivationRule("use implicit when available") {

    lazy val ignoredImplicits: Seq[UntypedMethod] = {
      val ours = Type.of[KindlingsDecoder.type].methods.collect {
        case method if method.value.name == "derived" => method.value.asUntyped
      }
      val circeDecoder = Type.of[Decoder.type].methods.collect {
        case method if method.value.name == "derived" => method.value.asUntyped
      }
      ours ++ circeDecoder
    }

    def apply[A: DecoderCtx]: MIO[Rule.Applicability[Expr[Either[DecodingFailure, A]]]] =
      Log.info(s"Attempting to use implicit Decoder for ${Type[A].prettyPrint}") >> {
        DTypes.Decoder[A].summonExprIgnoring(ignoredImplicits*).toEither match {
          case Right(instanceExpr) => cacheAndUse[A](instanceExpr)
          case Left(reason)        => yieldUnsupported[A](reason)
        }
      }

    private def cacheAndUse[A: DecoderCtx](
        instanceExpr: Expr[Decoder[A]]
    ): MIO[Rule.Applicability[Expr[Either[DecodingFailure, A]]]] =
      Log.info(s"Found implicit decoder ${instanceExpr.prettyPrint}, using directly") >>
        MIO.pure(Rule.matched(Expr.quote {
          Expr.splice(instanceExpr).apply(Expr.splice(dctx.cursor))
        }))

    private def yieldUnsupported[A: DecoderCtx](
        reason: String
    ): MIO[Rule.Applicability[Expr[Either[DecodingFailure, A]]]] =
      MIO.pure(
        Rule.yielded(
          s"The type ${Type[A].prettyPrint} does not have an implicit Decoder instance: $reason"
        )
      )
  }

  object DecHandleAsValueTypeRule extends DecoderDerivationRule("handle as value type when possible") {

    def apply[A: DecoderCtx]: MIO[Rule.Applicability[Expr[Either[DecodingFailure, A]]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as a value type") >> {
        Type[A] match {
          case IsValueType(isValueType) =>
            import isValueType.Underlying as Inner

            // Summon a Decoder[Inner]
            DTypes.Decoder[Inner].summonExprIgnoring(DecUseImplicitWhenAvailableRule.ignoredImplicits*).toEither match {
              case Right(innerDecoder) =>
                // Build wrap lambda outside quotes to avoid staging issues with wrap.Result type
                LambdaBuilder
                  .of1[Inner]("inner")
                  .traverse { innerExpr =>
                    MIO.pure(isValueType.value.wrap.apply(innerExpr).asInstanceOf[Expr[A]])
                  }
                  .map { builder =>
                    val wrapLambda = builder.build[A]
                    Rule.matched(Expr.quote {
                      Expr.splice(innerDecoder).apply(Expr.splice(dctx.cursor)).map(Expr.splice(wrapLambda))
                    })
                  }
              case Left(reason) =>
                MIO.pure(Rule.yielded(s"Value type inner ${Type[Inner].prettyPrint} has no Decoder: $reason"))
            }

          case _ =>
            MIO.pure(Rule.yielded(s"The type ${Type[A].prettyPrint} is not a value type"))
        }
      }
  }

  object DecHandleAsCaseClassRule extends DecoderDerivationRule("handle as case class when possible") {

    def apply[A: DecoderCtx]: MIO[Rule.Applicability[Expr[Either[DecodingFailure, A]]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as a case class") >> {
        CaseClass.parse[A] match {
          case Some(caseClass) =>
            for {
              _ <- dctx.setHelper[A] { (cursor, config) =>
                decodeCaseClassFields[A](caseClass)(using dctx.nestInCache(cursor, config))
              }
              result <- dctx.getHelper[A].flatMap {
                case Some(helperCall) =>
                  MIO.pure(Rule.matched(helperCall(dctx.cursor, dctx.config)))
                case None =>
                  MIO.pure(Rule.yielded(s"Failed to build helper for ${Type[A].prettyPrint}"))
              }
            } yield result

          case None =>
            MIO.pure(Rule.yielded(s"The type ${Type[A].prettyPrint} is not a case class"))
        }
      }

    @scala.annotation.nowarn("msg=is never used|unused explicit parameter")
    private def decodeCaseClassFields[A: DecoderCtx](
        caseClass: CaseClass[A]
    ): MIO[Expr[Either[DecodingFailure, A]]] = {
      implicit val StringT: Type[String] = DTypes.String
      implicit val HCursorT: Type[HCursor] = DTypes.HCursor
      implicit val DecodingFailureT: Type[DecodingFailure] = DTypes.DecodingFailure

      val constructor = caseClass.primaryConstructor
      val fieldsList = constructor.parameters.flatten.toList

      NonEmptyList.fromList(fieldsList) match {
        case None =>
          // Zero-parameter case class: construct directly
          caseClass
            .construct[MIO](new CaseClass.ConstructField[MIO] {
              def apply(field: Parameter): MIO[Expr[field.tpe.Underlying]] =
                MIO.fail(
                  new RuntimeException(s"Unexpected parameter in zero-argument case class ${Type[A].prettyPrint}")
                )
            })
            .flatMap {
              case Some(expr) =>
                MIO.pure(Expr.quote(Right(Expr.splice(expr)): Either[DecodingFailure, A]))
              case None =>
                MIO.fail(new RuntimeException(s"Cannot construct ${Type[A].prettyPrint}"))
            }

        case Some(fields) =>
          implicit val AnyT: Type[Any] = DTypes.Any
          implicit val EitherDFAnyT: Type[Either[DecodingFailure, Any]] = DTypes.EitherDFAny
          implicit val ArrayAnyT: Type[Array[Any]] = DTypes.ArrayAny
          implicit val ListEitherT: Type[List[Either[DecodingFailure, Any]]] = DTypes.ListEitherDFAny

          // Step 1: For each field, derive a decode expression AND capture the decoder
          // expression for later use in type-safe construction. We use CirceDerivationUtils.unsafeCast
          // with the decoder expression as a type witness to avoid path-dependent type aliases
          // (like param.tpe.Underlying) inside Expr.quote blocks, which cause Scala 2 macro
          // hygiene issues ("not found: value param/field").
          fields
            .parTraverse { case (fieldName, param) =>
              import param.tpe.Underlying as Field
              Log.namedScope(s"Finding decoder for field $fieldName: ${Type[Field].prettyPrint}") {
                // Summon Decoder[Field] for both decoding and type inference in construction
                DTypes
                  .Decoder[Field]
                  .summonExprIgnoring(DecUseImplicitWhenAvailableRule.ignoredImplicits*)
                  .toEither match {
                  case Right(decoderExpr) =>
                    // Build decode expression
                    val decodeExpr: Expr[Either[DecodingFailure, Any]] = Expr.quote {
                      Expr
                        .splice(dctx.cursor)
                        .downField(Expr.splice(dctx.config).transformMemberNames(Expr.splice(Expr(fieldName))))
                        .as(Expr.splice(decoderExpr))
                        .asInstanceOf[Either[DecodingFailure, Any]]
                    }
                    // Build a compile-time function that creates typed cast expressions.
                    // Uses unsafeCast(value, decoder) where the decoder provides type inference
                    // for A, avoiding path-dependent type aliases in Expr.quote.
                    val makeAccessor: Expr[Array[Any]] => (String, Expr_??) = { arrExpr =>
                      val typedExpr = Expr.quote {
                        CirceDerivationUtils.unsafeCast(
                          Expr.splice(arrExpr)(Expr.splice(Expr(param.index))),
                          Expr.splice(decoderExpr)
                        )
                      }
                      (fieldName, typedExpr.as_??)
                    }
                    MIO.pure((decodeExpr, makeAccessor))

                  case Left(reason) =>
                    MIO.fail(
                      DecoderDerivationError.UnsupportedType(
                        Type[Field].prettyPrint,
                        List(s"No implicit Decoder found for field $fieldName: $reason")
                      )
                    )
                }
              }
            }
            .flatMap { fieldData =>
              val decodeExprs = fieldData.toList.map(_._1)
              val makeAccessors = fieldData.toList.map(_._2)

              // Step 2: Build List literal from the decode expressions.
              val listExpr: Expr[List[Either[DecodingFailure, Any]]] =
                decodeExprs.foldRight(Expr.quote(List.empty[Either[DecodingFailure, Any]])) { (elem, acc) =>
                  Expr.quote(Expr.splice(elem) :: Expr.splice(acc))
                }

              // Step 3: Build the constructor lambda using LambdaBuilder + primaryConstructor
              LambdaBuilder
                .of1[Array[Any]]("decodedValues")
                .traverse { decodedValuesExpr =>
                  // Apply each accessor closure with the array expression to get typed Expr_?? values
                  val fieldMap: Map[String, Expr_??] =
                    makeAccessors.map(_(decodedValuesExpr)).toMap
                  // Call the primary constructor directly with the typed field map
                  caseClass.primaryConstructor(fieldMap) match {
                    case Right(constructExpr) => MIO.pure(constructExpr)
                    case Left(error)          =>
                      MIO.fail(new RuntimeException(s"Cannot construct ${Type[A].prettyPrint}: $error"))
                  }
                }
                .map { builder =>
                  val constructLambda = builder.build[A]
                  Expr.quote {
                    CirceDerivationUtils
                      .sequenceDecodeResults(Expr.splice(listExpr))
                      .map(Expr.splice(constructLambda))
                  }
                }
            }
      }
    }

  }

  object DecHandleAsEnumRule extends DecoderDerivationRule("handle as enum when possible") {

    def apply[A: DecoderCtx]: MIO[Rule.Applicability[Expr[Either[DecodingFailure, A]]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as an enum") >> {
        Enum.parse[A] match {
          case Some(enumm) =>
            for {
              _ <- dctx.setHelper[A] { (cursor, config) =>
                decodeEnumCases[A](enumm)(using dctx.nestInCache(cursor, config))
              }
              result <- dctx.getHelper[A].flatMap {
                case Some(helperCall) =>
                  MIO.pure(Rule.matched(helperCall(dctx.cursor, dctx.config)))
                case None =>
                  MIO.pure(Rule.yielded(s"Failed to build helper for ${Type[A].prettyPrint}"))
              }
            } yield result
          case None =>
            MIO.pure(Rule.yielded(s"The type ${Type[A].prettyPrint} is not an enum"))
        }
      }

    @scala.annotation.nowarn("msg=is never used|unused explicit parameter")
    private def decodeEnumCases[A: DecoderCtx](
        enumm: Enum[A]
    ): MIO[Expr[Either[DecodingFailure, A]]] = {
      implicit val HCursorT: Type[HCursor] = DTypes.HCursor
      implicit val DecodingFailureT: Type[DecodingFailure] = DTypes.DecodingFailure
      implicit val StringT: Type[String] = DTypes.String
      implicit val ListStringT: Type[List[String]] = DTypes.ListString
      implicit val TupleT: Type[(String, HCursor)] = DTypes.StringHCursorTuple
      implicit val EitherDFA: Type[Either[DecodingFailure, A]] = DTypes.DecoderResult[A]

      val childrenList = enumm.directChildren.toList

      NonEmptyList.fromList(childrenList) match {
        case None =>
          MIO.pure(Expr.quote {
            Left(
              DecodingFailure(
                s"Enum ${Expr.splice(Expr(Type[A].prettyPrint))} has no subtypes",
                Expr.splice(dctx.cursor).history
              )
            ): Either[DecodingFailure, A]
          })

        case Some(children) =>
          val knownNames: List[String] = children.toList.map(_._1)

          // For each child, derive a decoder and produce a dispatch function
          // that takes (typeNameExpr, innerCursorExpr) and returns Either[DecodingFailure, A]
          children
            .parTraverse { case (childName, child) =>
              import child.Underlying as ChildType
              Log.namedScope(s"Deriving decoder for enum case $childName: ${Type[ChildType].prettyPrint}") {
                deriveChildDecoder[A, ChildType](childName)
              }
            }
            .flatMap { childDispatchers =>
              // Build a dispatch lambda: (String, HCursor) => Either[DecodingFailure, A]
              LambdaBuilder
                .of1[(String, HCursor)]("readResult")
                .traverse { readResultExpr =>
                  // Extract typeName and innerCursor from the tuple
                  val typeNameExpr: Expr[String] = Expr.quote(Expr.splice(readResultExpr)._1)
                  val innerCursorExpr: Expr[HCursor] = Expr.quote(Expr.splice(readResultExpr)._2)

                  // Build the if-else dispatch chain (foldRight to get correct order)
                  val errorExpr: Expr[Either[DecodingFailure, A]] = Expr.quote {
                    Left(
                      CirceDerivationUtils.failedToMatchSubtype(
                        Expr.splice(typeNameExpr),
                        Expr.splice(innerCursorExpr),
                        Expr.splice(Expr(knownNames))
                      )
                    ): Either[DecodingFailure, A]
                  }

                  MIO.pure(childDispatchers.toList.foldRight(errorExpr) { case (dispatcher, elseExpr) =>
                    dispatcher(typeNameExpr, innerCursorExpr, elseExpr)
                  })
                }
                .map { builder =>
                  val dispatchFn = builder.build[Either[DecodingFailure, A]]
                  Expr.quote {
                    val config = Expr.splice(dctx.config)
                    val cursor = Expr.splice(dctx.cursor)
                    val readResult: Either[DecodingFailure, (String, HCursor)] =
                      config.discriminator match {
                        case Some(field) => CirceDerivationUtils.decodeDiscriminator(cursor, field)
                        case None        => CirceDerivationUtils.decodeWrapped(cursor)
                      }
                    readResult.flatMap(Expr.splice(dispatchFn))
                  }
                }
            }
      }
    }

    /** Derives a decoder for a single enum child type and returns a dispatch function. The dispatch function takes
      * (typeNameExpr, innerCursorExpr, elseExpr) and produces an if-else expression that checks if the type name
      * matches and decodes accordingly.
      */
    @scala.annotation.nowarn("msg=is never used|unused explicit parameter")
    private def deriveChildDecoder[A: DecoderCtx, ChildType: Type](
        childName: String
    ): MIO[(Expr[String], Expr[HCursor], Expr[Either[DecodingFailure, A]]) => Expr[Either[DecodingFailure, A]]] = {
      implicit val HCursorT: Type[HCursor] = DTypes.HCursor
      implicit val DecodingFailureT: Type[DecodingFailure] = DTypes.DecodingFailure
      implicit val StringT: Type[String] = DTypes.String

      // Try to summon implicit Decoder[ChildType] first
      DTypes
        .Decoder[ChildType]
        .summonExprIgnoring(DecUseImplicitWhenAvailableRule.ignoredImplicits*)
        .toEither match {
        case Right(decoderExpr) =>
          Log.info(s"Found implicit Decoder[$childName], using it") >>
            MIO.pure {
              (
                  typeNameExpr: Expr[String],
                  innerCursorExpr: Expr[HCursor],
                  elseExpr: Expr[Either[DecodingFailure, A]]
              ) =>
                Expr.quote {
                  if (
                    Expr.splice(dctx.config).transformConstructorNames(Expr.splice(Expr(childName))) == Expr
                      .splice(typeNameExpr)
                  )
                    Expr
                      .splice(decoderExpr)
                      .apply(Expr.splice(innerCursorExpr))
                      .asInstanceOf[Either[DecodingFailure, A]]
                  else
                    Expr.splice(elseExpr)
                }
            }

        case Left(_) =>
          // No implicit - derive via full rules chain (this sets up a helper in cache)
          deriveDecoderRecursively[ChildType](using dctx.nest[ChildType](dctx.cursor)).flatMap { _ =>
            dctx.getHelper[ChildType].map {
              case Some(helper) =>
                (
                    typeNameExpr: Expr[String],
                    innerCursorExpr: Expr[HCursor],
                    elseExpr: Expr[Either[DecodingFailure, A]]
                ) => {
                  val helperCallExpr = helper(innerCursorExpr, dctx.config)
                  Expr.quote {
                    if (
                      Expr.splice(dctx.config).transformConstructorNames(Expr.splice(Expr(childName))) == Expr
                        .splice(typeNameExpr)
                    )
                      Expr.splice(helperCallExpr).asInstanceOf[Either[DecodingFailure, A]]
                    else
                      Expr.splice(elseExpr)
                  }
                }

              case None =>
                // No helper - the child was handled by implicit or value type rule
                // This shouldn't normally happen since we checked implicit above
                (
                    typeNameExpr: Expr[String],
                    innerCursorExpr: Expr[HCursor],
                    elseExpr: Expr[Either[DecodingFailure, A]]
                ) => elseExpr
            }
          }
      }
    }
  }

  // Types

  private[compiletime] object DTypes {

    def Decoder: Type.Ctor1[Decoder] = Type.Ctor1.of[Decoder]
    def KindlingsDecoder: Type.Ctor1[KindlingsDecoder] = Type.Ctor1.of[KindlingsDecoder]
    val DecoderLogDerivation: Type[hearth.kindlings.circederivation.KindlingsDecoder.LogDerivation] =
      Type.of[hearth.kindlings.circederivation.KindlingsDecoder.LogDerivation]
    val HCursor: Type[HCursor] = Type.of[HCursor]
    val DecodingFailure: Type[DecodingFailure] = Type.of[DecodingFailure]
    val Configuration: Type[Configuration] = Type.of[Configuration]
    val String: Type[String] = Type.of[String]
    val Any: Type[Any] = Type.of[Any]
    val ArrayAny: Type[Array[Any]] = Type.of[Array[Any]]
    val EitherDFAny: Type[Either[DecodingFailure, Any]] = Type.of[Either[DecodingFailure, Any]]
    val ListEitherDFAny: Type[List[Either[DecodingFailure, Any]]] =
      Type.of[List[Either[DecodingFailure, Any]]]
    val ListString: Type[List[String]] = Type.of[List[String]]
    val StringHCursorTuple: Type[(String, HCursor)] = Type.of[(String, HCursor)]

    def DecoderResult[A: Type]: Type[Either[DecodingFailure, A]] =
      Type.of[Either[DecodingFailure, A]]
  }
}

sealed private[compiletime] trait DecoderDerivationError
    extends util.control.NoStackTrace
    with Product
    with Serializable {
  def message: String
  override def getMessage(): String = message
}
private[compiletime] object DecoderDerivationError {
  final case class UnsupportedType(tpeName: String, reasons: List[String]) extends DecoderDerivationError {
    override def message: String =
      s"The type $tpeName was not handled by any decoder derivation rule:\n${reasons.mkString("\n")}"
  }
}
