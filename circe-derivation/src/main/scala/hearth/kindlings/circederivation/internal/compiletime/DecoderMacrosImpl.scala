package hearth.kindlings.circederivation.internal.compiletime

import hearth.MacroCommons
import hearth.fp.data.NonEmptyList
import hearth.fp.effect.*
import hearth.fp.syntax.*
import hearth.std.*

import hearth.kindlings.circederivation.{Configuration, KindlingsDecoder}
import hearth.kindlings.circederivation.annotations.{fieldName, transientField}
import hearth.kindlings.circederivation.internal.runtime.CirceDerivationUtils
import io.circe.{Decoder, DecodingFailure, HCursor, Json}

trait DecoderMacrosImpl { this: MacroCommons & StdExtensions & AnnotationSupport =>

  // Entrypoints

  @scala.annotation.nowarn("msg=is never used")
  def deriveInlineDecode[A: Type](
      jsonExpr: Expr[Json],
      configExpr: Expr[Configuration]
  ): Expr[Either[DecodingFailure, A]] = {
    implicit val EitherT: Type[Either[DecodingFailure, A]] = DTypes.DecoderResult[A]
    implicit val JsonT: Type[Json] = DTypes.Json
    implicit val HCursorT: Type[HCursor] = DTypes.HCursor
    implicit val ConfigT: Type[Configuration] = DTypes.Configuration
    implicit val DecodingFailureT: Type[DecodingFailure] = DTypes.DecodingFailure

    deriveDecoderFromCtxAndAdaptForEntrypoint[A, Either[DecodingFailure, A]]("KindlingsDecoder.decode") { fromCtx =>
      ValDefs.createVal[Json](jsonExpr).use { jsonVal =>
        ValDefs.createVal[Configuration](configExpr).use { configVal =>
          Expr.quote {
            val _ = Expr.splice(jsonVal)
            val _ = Expr.splice(configVal)
            Expr.splice {
              val cursorExpr: Expr[HCursor] = Expr.quote(Expr.splice(jsonVal).hcursor)
              fromCtx(DecoderCtx.from(cursorExpr, configVal, derivedType = None))
            }
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
    val selfType: Option[??] = Some(Type[A].as_??)

    deriveDecoderFromCtxAndAdaptForEntrypoint[A, KindlingsDecoder[A]]("KindlingsDecoder.derived") { fromCtx =>
      ValDefs.createVal[Configuration](configExpr).use { configVal =>
        Expr.quote {
          val cfg = Expr.splice(configVal)
          new KindlingsDecoder[A] {
            def apply(c: HCursor): Decoder.Result[A] = {
              val _ = c
              Expr.splice {
                fromCtx(DecoderCtx.from(Expr.quote(c), Expr.quote(cfg), derivedType = selfType))
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
      errorRendering = if (shouldWeLogDecoderDerivation) RenderFrom(Log.Level.Info) else DontRender
    ) { (errorLogs, errors) =>
      val errorsRendered = errors
        .map { e =>
          e.getMessage.split("\n").toList match {
            case head :: tail => (("  - " + head) :: tail.map("    " + _)).mkString("\n")
            case _            => "  - " + e.getMessage
          }
        }
        .mkString("\n")
      val hint =
        "Enable debug logging with: import hearth.kindlings.circederivation.debug.logDerivationForKindlingsDecoder or scalac option -Xmacro-settings:circeDerivation.logDerivation=true"
      if (errorLogs.nonEmpty)
        s"""Macro derivation failed with the following errors:
           |$errorsRendered
           |and the following logs:
           |$errorLogs
           |$hint""".stripMargin
      else
        s"""Macro derivation failed with the following errors:
           |$errorsRendered
           |$hint""".stripMargin
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
      cache: MLocal[ValDefsCache],
      derivedType: Option[??]
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
        config: Expr[Configuration],
        derivedType: Option[??]
    ): DecoderCtx[A] = DecoderCtx(
      tpe = Type[A],
      cursor = cursor,
      config = config,
      cache = ValDefsCache.mlocal,
      derivedType = derivedType
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
          DecHandleAsOptionRule,
          DecHandleAsMapRule,
          DecHandleAsCollectionRule,
          DecHandleAsNamedTupleRule,
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
            val err = DecoderDerivationError.UnsupportedType(Type[A].prettyPrint, reasonsStrings)
            Log.error(err.message) >> MIO.fail(err)
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
        // Skip implicit search for the self type being derived to prevent self-referential loops
        // (e.g., `implicit val dec: Decoder[X] = KindlingsDecoder.derived[X]` would otherwise
        // find `dec` itself during macro expansion, generating code that calls itself infinitely).
        if (dctx.derivedType.exists(_.Underlying =:= Type[A]))
          MIO.pure(
            Rule.yielded(s"The type ${Type[A].prettyPrint} is the type being derived, skipping implicit search")
          )
        else
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

  object DecHandleAsOptionRule extends DecoderDerivationRule("handle as Option when possible") {

    def apply[A: DecoderCtx]: MIO[Rule.Applicability[Expr[Either[DecodingFailure, A]]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as Option") >> {
        Type[A] match {
          case IsOption(isOption) =>
            import isOption.Underlying as Inner
            implicit val HCursorT: Type[HCursor] = DTypes.HCursor
            implicit val EitherDFInner: Type[Either[DecodingFailure, Inner]] = DTypes.DecoderResult[Inner]

            LambdaBuilder
              .of1[HCursor]("innerCursor")
              .traverse { innerCursorExpr =>
                deriveDecoderRecursively[Inner](using dctx.nest[Inner](innerCursorExpr))
              }
              .map { builder =>
                val decodeFn = builder.build[Either[DecodingFailure, Inner]]
                Rule.matched(Expr.quote {
                  CirceDerivationUtils
                    .decodeOptionFromFn(
                      Expr.splice(dctx.cursor),
                      Expr.splice(decodeFn)
                    )
                    .asInstanceOf[Either[DecodingFailure, A]]
                })
              }

          case _ =>
            MIO.pure(Rule.yielded(s"The type ${Type[A].prettyPrint} is not an Option"))
        }
      }
  }

  @scala.annotation.nowarn("msg=Infinite loop")
  object DecHandleAsMapRule extends DecoderDerivationRule("handle as map when possible") {

    def apply[A: DecoderCtx]: MIO[Rule.Applicability[Expr[Either[DecodingFailure, A]]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as a map") >> {
        Type[A] match {
          case IsMap(isMap) =>
            import isMap.Underlying as Pair
            decodeMapEntries[A, Pair](isMap.value)
          case _ =>
            MIO.pure(Rule.yielded(s"The type ${Type[A].prettyPrint} is not a map"))
        }
      }

    private def decodeMapEntries[A: DecoderCtx, Pair: Type](
        isMap: IsMapOf[A, Pair]
    ): MIO[Rule.Applicability[Expr[Either[DecodingFailure, A]]]] = {
      import isMap.{Key, Value, CtorResult}
      implicit val StringT: Type[String] = DTypes.String
      implicit val HCursorT: Type[HCursor] = DTypes.HCursor
      implicit val EitherDFValue: Type[Either[DecodingFailure, Value]] = DTypes.DecoderResult[Value]

      if (!(Key <:< Type[String]))
        MIO.pure(Rule.yielded(s"Map key type ${Key.prettyPrint} is not String"))
      else {
        LambdaBuilder
          .of1[HCursor]("valueCursor")
          .traverse { valueCursorExpr =>
            deriveDecoderRecursively[Value](using dctx.nest[Value](valueCursorExpr))
          }
          .map { builder =>
            val decodeFn = builder.build[Either[DecodingFailure, Value]]
            val factoryExpr = isMap.factory
            Rule.matched(Expr.quote {
              CirceDerivationUtils
                .decodeMapWith(
                  Expr.splice(dctx.cursor),
                  CirceDerivationUtils.decoderFromFn(Expr.splice(decodeFn)),
                  Expr
                    .splice(factoryExpr)
                    .asInstanceOf[scala.collection.Factory[(String, Value), A]]
                )
                .asInstanceOf[Either[DecodingFailure, A]]
            })
          }
      }
    }
  }

  object DecHandleAsCollectionRule extends DecoderDerivationRule("handle as collection when possible") {

    def apply[A: DecoderCtx]: MIO[Rule.Applicability[Expr[Either[DecodingFailure, A]]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as a collection") >> {
        Type[A] match {
          case IsCollection(isCollection) =>
            import isCollection.Underlying as Item
            import isCollection.value.CtorResult
            implicit val HCursorT: Type[HCursor] = DTypes.HCursor
            implicit val EitherDFItem: Type[Either[DecodingFailure, Item]] = DTypes.DecoderResult[Item]

            LambdaBuilder
              .of1[HCursor]("itemCursor")
              .traverse { itemCursorExpr =>
                deriveDecoderRecursively[Item](using dctx.nest[Item](itemCursorExpr))
              }
              .map { builder =>
                val decodeFn = builder.build[Either[DecodingFailure, Item]]
                val factoryExpr = isCollection.value.factory
                Rule.matched(Expr.quote {
                  CirceDerivationUtils
                    .decodeCollectionWith(
                      Expr.splice(dctx.cursor),
                      CirceDerivationUtils.decoderFromFn(Expr.splice(decodeFn)),
                      Expr.splice(factoryExpr).asInstanceOf[scala.collection.Factory[Item, A]]
                    )
                    .asInstanceOf[Either[DecodingFailure, A]]
                })
              }

          case _ =>
            MIO.pure(Rule.yielded(s"The type ${Type[A].prettyPrint} is not a collection"))
        }
      }
  }

  object DecHandleAsNamedTupleRule extends DecoderDerivationRule("handle as named tuple when possible") {

    def apply[A: DecoderCtx]: MIO[Rule.Applicability[Expr[Either[DecodingFailure, A]]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as a named tuple") >> {
        if (!Type[A].isNamedTuple)
          MIO.pure(Rule.yielded(s"The type ${Type[A].prettyPrint} is not a named tuple"))
        else
          Type[A].primaryConstructor match {
            case Some(constructor) =>
              for {
                _ <- dctx.setHelper[A] { (cursor, config) =>
                  decodeNamedTupleFields[A](constructor)(using dctx.nestInCache(cursor, config))
                }
                result <- dctx.getHelper[A].flatMap {
                  case Some(helperCall) =>
                    MIO.pure(Rule.matched(helperCall(dctx.cursor, dctx.config)))
                  case None =>
                    MIO.pure(Rule.yielded(s"Failed to build helper for ${Type[A].prettyPrint}"))
                }
              } yield result
            case None =>
              MIO.pure(Rule.yielded(s"Named tuple ${Type[A].prettyPrint} has no primary constructor"))
          }
      }

    @scala.annotation.nowarn("msg=is never used|unused explicit parameter")
    private def decodeNamedTupleFields[A: DecoderCtx](
        constructor: Method.NoInstance[A]
    ): MIO[Expr[Either[DecodingFailure, A]]] = {
      implicit val StringT: Type[String] = DTypes.String
      implicit val HCursorT: Type[HCursor] = DTypes.HCursor
      implicit val DecodingFailureT: Type[DecodingFailure] = DTypes.DecodingFailure
      implicit val AnyT: Type[Any] = DTypes.Any
      implicit val EitherDFAnyT: Type[Either[DecodingFailure, Any]] = DTypes.EitherDFAny
      implicit val ArrayAnyT: Type[Array[Any]] = DTypes.ArrayAny
      implicit val ListEitherT: Type[List[Either[DecodingFailure, Any]]] = DTypes.ListEitherDFAny

      val fieldsList = constructor.parameters.flatten.toList

      NonEmptyList.fromList(fieldsList) match {
        case None =>
          // Empty named tuple: return Right(empty tuple)
          constructor(Map.empty) match {
            case Right(constructExpr) =>
              MIO.pure(Expr.quote {
                Right(Expr.splice(constructExpr)): Either[DecodingFailure, A]
              })
            case Left(error) =>
              val err =
                DecoderDerivationError.CannotConstructType(Type[A].prettyPrint, isSingleton = false, Some(error))
              Log.error(err.message) >> MIO.fail(err)
          }

        case Some(fields) =>
          // Step 1: For each field, derive a decoder and build decode + accessor expressions
          fields
            .parTraverse { case (fName, param) =>
              import param.tpe.Underlying as Field
              Log.namedScope(s"Deriving decoder for named tuple field $fName: ${Type[Field].prettyPrint}") {
                deriveFieldDecoder[Field].map { decoderExpr =>
                  val decodeExpr: Expr[Either[DecodingFailure, Any]] = Expr.quote {
                    Expr
                      .splice(dctx.cursor)
                      .downField(
                        Expr.splice(dctx.config).transformMemberNames(Expr.splice(Expr(fName)))
                      )
                      .as(Expr.splice(decoderExpr))
                      .asInstanceOf[Either[DecodingFailure, Any]]
                  }

                  val makeAccessor: Expr[Array[Any]] => (String, Expr_??) = { arrExpr =>
                    val typedExpr = Expr.quote {
                      CirceDerivationUtils.unsafeCast(
                        Expr.splice(arrExpr)(Expr.splice(Expr(param.index))),
                        Expr.splice(decoderExpr)
                      )
                    }
                    (fName, typedExpr.as_??)
                  }
                  (decodeExpr, makeAccessor)
                }
              }
            }
            .flatMap { fieldData =>
              val decodeExprs = fieldData.toList.map(_._1)
              val makeAccessors = fieldData.toList.map(_._2)

              // Step 2: Build List literal from the decode expressions
              val listExpr: Expr[List[Either[DecodingFailure, Any]]] =
                decodeExprs.foldRight(Expr.quote(List.empty[Either[DecodingFailure, Any]])) { (elem, acc) =>
                  Expr.quote(Expr.splice(elem) :: Expr.splice(acc))
                }

              // Step 3: Build the constructor lambda using LambdaBuilder + primaryConstructor
              LambdaBuilder
                .of1[Array[Any]]("decodedValues")
                .traverse { decodedValuesExpr =>
                  val fieldMap: Map[String, Expr_??] =
                    makeAccessors.map(_(decodedValuesExpr)).toMap
                  constructor(fieldMap) match {
                    case Right(constructExpr) => MIO.pure(constructExpr)
                    case Left(error)          =>
                      val err = DecoderDerivationError.CannotConstructType(
                        Type[A].prettyPrint,
                        isSingleton = false,
                        Some(error)
                      )
                      Log.error(err.message) >> MIO.fail(err)
                  }
                }
                .map { builder =>
                  val constructLambda = builder.build[A]
                  Expr.quote {
                    CirceDerivationUtils.sequenceDecodeResults(Expr.splice(listExpr)).map { arr =>
                      Expr.splice(constructLambda).apply(arr)
                    }
                  }
                }
            }
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

    @scala.annotation.nowarn("msg=is never used|unused explicit parameter|Non local returns")
    private def decodeCaseClassFields[A: DecoderCtx](
        caseClass: CaseClass[A]
    ): MIO[Expr[Either[DecodingFailure, A]]] = {
      implicit val StringT: Type[String] = DTypes.String
      implicit val HCursorT: Type[HCursor] = DTypes.HCursor
      implicit val DecodingFailureT: Type[DecodingFailure] = DTypes.DecodingFailure
      implicit val UnitT: Type[Unit] = DTypes.Unit
      implicit val EitherDFUnitT: Type[Either[DecodingFailure, Unit]] = DTypes.EitherDFUnit
      implicit val SetStringT: Type[Set[String]] = DTypes.SetString
      implicit val ConfigT: Type[Configuration] = DTypes.Configuration
      implicit val BooleanT: Type[Boolean] = DTypes.Boolean
      implicit val fieldNameT: Type[fieldName] = DTypes.FieldName
      implicit val transientFieldT: Type[transientField] = DTypes.TransientField

      // Singletons (case objects, parameterless enum cases) have no primary constructor.
      // Use construct directly, which returns the singleton reference via Expr.singletonOf.
      if (caseClass.isSingleton) {
        return caseClass
          .construct[MIO](new CaseClass.ConstructField[MIO] {
            def apply(field: Parameter): MIO[Expr[field.tpe.Underlying]] = {
              val err = DecoderDerivationError.CannotConstructType(
                Type[A].prettyPrint,
                isSingleton = true,
                Some(s"Unexpected parameter in singleton")
              )
              Log.error(err.message) >> MIO.fail(err)
            }
          })
          .flatMap {
            case Some(expr) =>
              MIO.pure(Expr.quote {
                val config = Expr.splice(dctx.config)
                if (config.strictDecoding) {
                  CirceDerivationUtils
                    .checkStrictDecoding(Expr.splice(dctx.cursor), Set.empty[String])
                    .map(_ => Expr.splice(expr))
                } else
                  Right(Expr.splice(expr)): Either[DecodingFailure, A]
              })
            case None =>
              val err = DecoderDerivationError.CannotConstructType(Type[A].prettyPrint, isSingleton = true)
              Log.error(err.message) >> MIO.fail(err)
          }
      }

      val constructor = caseClass.primaryConstructor
      val fieldsList = constructor.parameters.flatten.toList

      // Validate: @transientField on fields without defaults is a compile error
      fieldsList
        .collectFirst {
          case (name, param) if hasAnnotationType[transientField](param) && !param.hasDefault => name
        }
        .foreach { name =>
          val err = DecoderDerivationError.TransientFieldMissingDefault(name, Type[A].prettyPrint)
          return Log.error(err.message) >> MIO.fail(err)
        }

      // Build a List[String] expression of the field names (accounting for @fieldName overrides)
      // for strict decoding — only non-transient fields
      val fieldNamesListExpr: Expr[List[String]] =
        fieldsList
          .filterNot { case (_, param) => hasAnnotationType[transientField](param) }
          .map { case (name, param) =>
            getAnnotationStringArg[fieldName](param).getOrElse(name)
          }
          .foldRight(Expr.quote(List.empty[String])) { (name, acc) =>
            Expr.quote(Expr.splice(Expr(name)) :: Expr.splice(acc))
          }

      // For strict decoding with @fieldName, the names are already resolved at compile time
      // so we need a version that doesn't apply config.transformMemberNames for those
      val hasAnyFieldNameAnnotation = fieldsList.exists { case (_, param) =>
        getAnnotationStringArg[fieldName](param).isDefined
      }

      NonEmptyList.fromList(fieldsList) match {
        case None =>
          // Zero-parameter case class: construct directly, but check strictDecoding
          caseClass
            .construct[MIO](new CaseClass.ConstructField[MIO] {
              def apply(field: Parameter): MIO[Expr[field.tpe.Underlying]] = {
                val err = DecoderDerivationError.CannotConstructType(
                  Type[A].prettyPrint,
                  isSingleton = false,
                  Some(s"Unexpected parameter in zero-argument case class")
                )
                Log.error(err.message) >> MIO.fail(err)
              }
            })
            .flatMap {
              case Some(expr) =>
                MIO.pure(Expr.quote {
                  CirceDerivationUtils.checkIsObject(Expr.splice(dctx.cursor)).flatMap { _ =>
                    val config = Expr.splice(dctx.config)
                    if (config.strictDecoding) {
                      CirceDerivationUtils
                        .checkStrictDecoding(Expr.splice(dctx.cursor), Set.empty[String])
                        .map(_ => Expr.splice(expr))
                    } else
                      Right(Expr.splice(expr)): Either[DecodingFailure, A]
                  }
                })
              case None =>
                val err = DecoderDerivationError.CannotConstructType(Type[A].prettyPrint, isSingleton = false)
                Log.error(err.message) >> MIO.fail(err)
            }

        case Some(fields) =>
          implicit val AnyT: Type[Any] = DTypes.Any
          implicit val EitherDFAnyT: Type[Either[DecodingFailure, Any]] = DTypes.EitherDFAny
          implicit val ArrayAnyT: Type[Array[Any]] = DTypes.ArrayAny
          implicit val ListEitherT: Type[List[Either[DecodingFailure, Any]]] = DTypes.ListEitherDFAny

          // Step 1: For each field, derive a decoder (implicit or recursive) and build
          // decode + accessor expressions. Uses unsafeCast with the decoder as type witness
          // to avoid path-dependent type aliases in Expr.quote (Scala 2 compatibility).
          // Also resolves default values for useDefaults support.
          // For @transientField fields, use the default value directly without decoding.
          fields
            .parTraverse { case (fName, param) =>
              import param.tpe.Underlying as Field
              val isTransient = hasAnnotationType[transientField](param)
              val nameOverride = getAnnotationStringArg[fieldName](param)
              Log.namedScope(s"Deriving decoder for field $fName: ${Type[Field].prettyPrint}") {
                deriveFieldDecoder[Field].map { decoderExpr =>
                  // Try to get the default value expression for useDefaults / transientField support.
                  val defaultAsAnyOpt: Option[Expr[Any]] =
                    if (param.hasDefault)
                      param.defaultValue.flatMap { existentialOuter =>
                        val methodOf = existentialOuter.value
                        methodOf.value match {
                          case noInstance: Method.NoInstance[?] =>
                            import noInstance.Returned
                            noInstance(Map.empty).toOption.map(_.upcast[Any])
                          case _ => None
                        }
                      }
                    else None

                  val decodeExpr: Expr[Either[DecodingFailure, Any]] =
                    if (isTransient) {
                      // Transient field: always use default value (validated above that default exists)
                      defaultAsAnyOpt match {
                        case Some(defaultAnyExpr) =>
                          Expr.quote {
                            Right(Expr.splice(defaultAnyExpr)): Either[DecodingFailure, Any]
                          }
                        case None =>
                          // Should not happen due to validation above, but be safe
                          Expr.quote {
                            Right(null): Either[DecodingFailure, Any]
                          }
                      }
                    } else {
                      nameOverride match {
                        case Some(customName) =>
                          // @fieldName override: use the custom name directly, ignoring config transform
                          defaultAsAnyOpt match {
                            case Some(defaultAnyExpr) =>
                              Expr.quote {
                                val config = Expr.splice(dctx.config)
                                if (config.useDefaults) {
                                  val field = Expr.splice(dctx.cursor).downField(Expr.splice(Expr(customName)))
                                  if (field.failed)
                                    Right(Expr.splice(defaultAnyExpr)): Either[DecodingFailure, Any]
                                  else
                                    field.as(Expr.splice(decoderExpr)).asInstanceOf[Either[DecodingFailure, Any]]
                                } else
                                  Expr
                                    .splice(dctx.cursor)
                                    .downField(Expr.splice(Expr(customName)))
                                    .as(Expr.splice(decoderExpr))
                                    .asInstanceOf[Either[DecodingFailure, Any]]
                              }
                            case None =>
                              Expr.quote {
                                Expr
                                  .splice(dctx.cursor)
                                  .downField(Expr.splice(Expr(customName)))
                                  .as(Expr.splice(decoderExpr))
                                  .asInstanceOf[Either[DecodingFailure, Any]]
                              }
                          }
                        case None =>
                          // Standard field: use config.transformMemberNames
                          defaultAsAnyOpt match {
                            case Some(defaultAnyExpr) =>
                              Expr.quote {
                                val config = Expr.splice(dctx.config)
                                val fn = config.transformMemberNames(Expr.splice(Expr(fName)))
                                if (config.useDefaults) {
                                  val field = Expr.splice(dctx.cursor).downField(fn)
                                  if (field.failed)
                                    Right(Expr.splice(defaultAnyExpr)): Either[DecodingFailure, Any]
                                  else
                                    field.as(Expr.splice(decoderExpr)).asInstanceOf[Either[DecodingFailure, Any]]
                                } else
                                  Expr
                                    .splice(dctx.cursor)
                                    .downField(fn)
                                    .as(Expr.splice(decoderExpr))
                                    .asInstanceOf[Either[DecodingFailure, Any]]
                              }
                            case None =>
                              Expr.quote {
                                Expr
                                  .splice(dctx.cursor)
                                  .downField(
                                    Expr.splice(dctx.config).transformMemberNames(Expr.splice(Expr(fName)))
                                  )
                                  .as(Expr.splice(decoderExpr))
                                  .asInstanceOf[Either[DecodingFailure, Any]]
                              }
                          }
                      }
                    }

                  val makeAccessor: Expr[Array[Any]] => (String, Expr_??) = { arrExpr =>
                    val typedExpr = Expr.quote {
                      CirceDerivationUtils.unsafeCast(
                        Expr.splice(arrExpr)(Expr.splice(Expr(param.index))),
                        Expr.splice(decoderExpr)
                      )
                    }
                    (fName, typedExpr.as_??)
                  }
                  (decodeExpr, makeAccessor)
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
                  val fieldMap: Map[String, Expr_??] =
                    makeAccessors.map(_(decodedValuesExpr)).toMap
                  caseClass.primaryConstructor(fieldMap) match {
                    case Right(constructExpr) => MIO.pure(constructExpr)
                    case Left(error)          =>
                      val err = DecoderDerivationError.CannotConstructType(
                        Type[A].prettyPrint,
                        isSingleton = false,
                        Some(error)
                      )
                      Log.error(err.message) >> MIO.fail(err)
                  }
                }
                .map { builder =>
                  val constructLambda = builder.build[A]
                  // Step 4: Wrap with strictDecoding check if configured
                  // For strict decoding, field names that have @fieldName are already resolved
                  // and names without @fieldName still need config.transformMemberNames
                  if (hasAnyFieldNameAnnotation) {
                    // Mixed: some names are already resolved, some need transform
                    // Build the expected fields set accounting for both
                    val resolvedFieldNames: Expr[Set[String]] = {
                      val nameExprs = fieldsList
                        .filterNot { case (_, p) => hasAnnotationType[transientField](p) }
                        .map { case (name, p) =>
                          getAnnotationStringArg[fieldName](p) match {
                            case Some(custom) => (custom, true) // already resolved
                            case None         => (name, false) // needs transform
                          }
                        }
                      val resolvedList = nameExprs.foldRight(Expr.quote(List.empty[String])) {
                        case ((name, true), acc) =>
                          Expr.quote(Expr.splice(Expr(name)) :: Expr.splice(acc))
                        case ((name, false), acc) =>
                          Expr.quote {
                            Expr.splice(dctx.config).transformMemberNames(Expr.splice(Expr(name))) ::
                              Expr.splice(acc)
                          }
                      }
                      Expr.quote(Expr.splice(resolvedList).toSet)
                    }
                    Expr.quote {
                      val config = Expr.splice(dctx.config)
                      val decoded = CirceDerivationUtils
                        .sequenceDecodeResults(Expr.splice(listExpr))
                        .map(Expr.splice(constructLambda))
                      if (config.strictDecoding) {
                        CirceDerivationUtils
                          .checkStrictDecoding(Expr.splice(dctx.cursor), Expr.splice(resolvedFieldNames))
                          .flatMap(_ => decoded)
                      } else decoded
                    }
                  } else {
                    Expr.quote {
                      val config = Expr.splice(dctx.config)
                      val decoded = CirceDerivationUtils
                        .sequenceDecodeResults(Expr.splice(listExpr))
                        .map(Expr.splice(constructLambda))
                      if (config.strictDecoding) {
                        val expectedFields =
                          Expr.splice(fieldNamesListExpr).map(config.transformMemberNames).toSet
                        CirceDerivationUtils
                          .checkStrictDecoding(Expr.splice(dctx.cursor), expectedFields)
                          .flatMap(_ => decoded)
                      } else decoded
                    }
                  }
                }
            }
      }
    }

  }

  /** Derive a Decoder[Field] for a case class field. Tries implicit summoning first, falls back to recursive derivation
    * via the full rule chain.
    */
  @scala.annotation.nowarn("msg=is never used|unused explicit parameter")
  private def deriveFieldDecoder[Field: Type](implicit ctx: DecoderCtx[?]): MIO[Expr[Decoder[Field]]] = {
    implicit val HCursorT: Type[HCursor] = DTypes.HCursor
    implicit val EitherDFField: Type[Either[DecodingFailure, Field]] = DTypes.DecoderResult[Field]

    DTypes.Decoder[Field].summonExprIgnoring(DecUseImplicitWhenAvailableRule.ignoredImplicits*).toEither match {
      case Right(decoderExpr) =>
        Log.info(s"Found implicit Decoder[${Type[Field].prettyPrint}]") >> MIO.pure(decoderExpr)
      case Left(_) =>
        Log.info(s"Building Decoder[${Type[Field].prettyPrint}] via recursive derivation") >>
          LambdaBuilder
            .of1[HCursor]("fieldCursor")
            .traverse { fieldCursorExpr =>
              deriveDecoderRecursively[Field](using ctx.nest[Field](fieldCursorExpr))
            }
            .map { builder =>
              val decodeFn = builder.build[Either[DecodingFailure, Field]]
              Expr.quote(CirceDerivationUtils.decoderFromFn(Expr.splice(decodeFn)))
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

      // Check at compile time if all children are singletons (case objects with no fields)
      val allCaseObjects = Type[A].isEnumeration || Type[A].isJavaEnum ||
        childrenList.forall { case (_, child) =>
          Type.isVal(using child.Underlying) ||
          CaseClass.parse(using child.Underlying).exists(_.primaryConstructor.parameters.flatten.isEmpty)
        }

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
                    if (Expr.splice(Expr(allCaseObjects)) && config.enumAsStrings) {
                      // String enum decode path: read plain string, dispatch on name
                      cursor.as[String](io.circe.Decoder.decodeString) match {
                        case Right(typeName) =>
                          Expr.splice(dispatchFn)((typeName, cursor))
                        case Left(_) =>
                          Left(
                            DecodingFailure(
                              "Expected a JSON string for enum value",
                              cursor.history
                            )
                          ): Either[DecodingFailure, A]
                      }
                    } else {
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
          // Try singletonOf first — handles Enumeration values, Java enum values, case objects
          Expr.singletonOf[ChildType] match {
            case Some(singleton) =>
              Log.info(s"Using singleton for $childName") >>
                MIO.pure {
                  (
                      typeNameExpr: Expr[String],
                      _: Expr[HCursor],
                      elseExpr: Expr[Either[DecodingFailure, A]]
                  ) =>
                    Expr.quote {
                      if (
                        Expr.splice(dctx.config).transformConstructorNames(Expr.splice(Expr(childName))) == Expr
                          .splice(typeNameExpr)
                      )
                        Right(Expr.splice(singleton).asInstanceOf[A]): Either[DecodingFailure, A]
                      else
                        Expr.splice(elseExpr)
                    }
                }
            case None =>
              // No singleton - derive via full rules chain (this sets up a helper in cache)
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
  }

  // Types

  private[compiletime] object DTypes {

    def Decoder: Type.Ctor1[Decoder] = Type.Ctor1.of[Decoder]
    def KindlingsDecoder: Type.Ctor1[KindlingsDecoder] = Type.Ctor1.of[KindlingsDecoder]
    val DecoderLogDerivation: Type[hearth.kindlings.circederivation.KindlingsDecoder.LogDerivation] =
      Type.of[hearth.kindlings.circederivation.KindlingsDecoder.LogDerivation]
    val Json: Type[Json] = Type.of[Json]
    val HCursor: Type[HCursor] = Type.of[HCursor]
    val DecodingFailure: Type[DecodingFailure] = Type.of[DecodingFailure]
    val Configuration: Type[Configuration] = Type.of[Configuration]
    val String: Type[String] = Type.of[String]
    val Boolean: Type[Boolean] = Type.of[Boolean]
    val Any: Type[Any] = Type.of[Any]
    val Unit: Type[Unit] = Type.of[Unit]
    val ArrayAny: Type[Array[Any]] = Type.of[Array[Any]]
    val EitherDFAny: Type[Either[DecodingFailure, Any]] = Type.of[Either[DecodingFailure, Any]]
    val EitherDFUnit: Type[Either[DecodingFailure, Unit]] = Type.of[Either[DecodingFailure, Unit]]
    val ListEitherDFAny: Type[List[Either[DecodingFailure, Any]]] =
      Type.of[List[Either[DecodingFailure, Any]]]
    val ListString: Type[List[String]] = Type.of[List[String]]
    val SetString: Type[Set[String]] = Type.of[Set[String]]
    val StringHCursorTuple: Type[(String, HCursor)] = Type.of[(String, HCursor)]
    val FieldName: Type[fieldName] = Type.of[fieldName]
    val TransientField: Type[transientField] = Type.of[transientField]

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
  final case class TransientFieldMissingDefault(fieldName: String, tpeName: String) extends DecoderDerivationError {
    override def message: String =
      s"@transientField on field '$fieldName' of $tpeName requires a default value"
  }
  final case class CannotConstructType(tpeName: String, isSingleton: Boolean, constructorError: Option[String] = None)
      extends DecoderDerivationError {
    override def message: String = {
      val prefix =
        if (isSingleton) s"Cannot construct singleton $tpeName" else s"Cannot construct $tpeName"
      constructorError.fold(prefix)(err => s"$prefix: $err")
    }
  }
}
