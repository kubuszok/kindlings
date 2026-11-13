package hearth.kindlings.circederivation.internal.compiletime

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.fp.syntax.*
import hearth.fp.instances.*
import hearth.std.*

import hearth.kindlings.circederivation.{Configuration, KindlingsDecoder}
import hearth.kindlings.circederivation.annotations.{fieldName, transientField}
import hearth.kindlings.circederivation.internal.runtime.CirceDerivationUtils
import cats.data.ValidatedNel
import io.circe.{Decoder, DecodingFailure, HCursor, Json, KeyDecoder}

trait DecoderMacrosImpl
    extends CirceDerivationTimeout
    with CirceDerivationPolicy
    with SealedHierarchyFlattening
    with hearth.kindlings.derivation.compiletime.MethodFolds
    with hearth.kindlings.derivation.compiletime.EitherFieldsConstruct
    with rules.DecoderUseCachedDefWhenAvailableRuleImpl
    with rules.DecoderUseImplicitWhenAvailableRuleImpl
    with rules.DecoderHandleAsLiteralTypeRuleImpl
    with rules.DecoderHandleAsBuiltInTypeRuleImpl
    with rules.DecoderHandleAsValueTypeRuleImpl
    with rules.DecoderHandleAsOptionRuleImpl
    with rules.DecoderHandleAsMapRuleImpl
    with rules.DecoderHandleAsCollectionRuleImpl
    with rules.DecoderHandleAsNamedTupleRuleImpl
    with rules.DecoderHandleAsTupleRuleImpl
    with rules.DecoderHandleAsSingletonRuleImpl
    with rules.DecoderHandleAsCaseClassRuleImpl
    with rules.DecoderHandleAsEnumRuleImpl
    with rules.DecoderDerivationPolicyRuleImpl {
  this: MacroCommons & StdExtensions & AnnotationSupport & LoadStandardExtensionsOnce =>

  // $COVERAGE-OFF$
  override protected def derivationPolicyTypeClassName: String = "KindlingsDecoder"
  // $COVERAGE-ON$

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

    val evConfig: Option[Configuration] = semiEvalConfig(configExpr)
    evaluatedConfigFlags = evConfig

    deriveDecoderFromCtxAndAdaptForEntrypoint[A, Either[DecodingFailure, A]]("KindlingsDecoder.decode") { fromCtx =>
      ValDefs.createVal[Json](jsonExpr).use { jsonVal =>
        ValDefs.createVal[Configuration](configExpr).use { configVal =>
          Expr.quote {
            val _ = Expr.splice(jsonVal)
            val _ = Expr.splice(configVal)
            Expr.splice {
              val cursorExpr: Expr[HCursor] = Expr.quote(Expr.splice(jsonVal).hcursor)
              fromCtx(DecoderCtx.from(cursorExpr, configVal, Expr.quote(true), derivedType = None, evConfig))
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
    implicit val BooleanT: Type[Boolean] = DTypes.Boolean
    implicit val AnyT: Type[Any] = DTypes.Any
    implicit val ValidatedNelDFA: Type[ValidatedNel[DecodingFailure, A]] = DTypes.ValidatedNelDF[A]
    implicit val StringT: Type[String] = DTypes.String
    implicit val ListStringT: Type[List[String]] = DTypes.ListString
    implicit val ListDecoderAnyT: Type[List[Decoder[Any]]] = DTypes.ListDecoderAny
    val selfType: Option[??] = Some(Type[A].as_??)

    if (Type[A] =:= Type.of[Nothing].asInstanceOf[Type[A]] || Type[A] =:= Type.of[Any].asInstanceOf[Type[A]])
      Environment.reportErrorAndAbort(
        s"KindlingsDecoder.derived: type parameter was inferred as ${Type[A].prettyPrint}, which is likely unintended.\n" +
          "Provide an explicit type parameter, e.g.: KindlingsDecoder.derived[MyType]\n" +
          "or add a type ascription to the result variable."
      )

    val evConfig: Option[Configuration] = semiEvalConfig(configExpr)
    evaluatedConfigFlags = evConfig

    Log
      .namedScope(
        s"Deriving decoder for ${Type[A].prettyPrint} at: ${Environment.currentPosition.prettyPrint}"
      ) {
        MIO.scoped { runSafe =>
          // Step 1: Run derivation once (populates cache with the cached def)
          val ctx = DecoderCtx.from[A](
            cursor = Expr.quote(null.asInstanceOf[HCursor]), // placeholder — real cursor comes from cached def params
            config = Expr.quote(null.asInstanceOf[Configuration]), // placeholder
            failFast = Expr.quote(true), // placeholder
            derivedType = selfType,
            evaluatedConfig = evConfig
          )
          runSafe {
            for {
              _ <- ensureStandardExtensionsLoaded()
              _ <- deriveDecoderRecursively[A](using ctx)
            } yield ()
          }

          // Step 2: Get the cached helper reference and cache
          val helperOpt = runSafe(ctx.getHelper[A])
          val cache = runSafe(ctx.cache.get)

          // Step 2b: For types handled without a cached helper (value types, options, collections, maps),
          // build a direct Decoder[A] that we can delegate to in the KindlingsDecoder body.
          val directDecoderOpt: Option[Expr[Decoder[A]]] =
            if (helperOpt.isDefined) None
            else Some(runSafe(deriveDecoderViaLambda[A](configExpr, selfType, evConfig)))

          // Step 2c: A derived sum is marked as a KindlingsSumDecoder, exposing the constructor names it dispatches on
          // (so that it is flattened when used as the instance of an intermediate sealed trait).
          val sumMarker: Option[(Expr[List[String]], Expr[List[Decoder[Any]]])] =
            if (helperOpt.isDefined && isMarkedSum[A])
              Enum.parse[A].toOption.map { enumm =>
                val entries = decoderSumEntries[A](enumm)
                val names = entries.map(_._1).foldRight(Expr.quote(List.empty[String])) { (n, acc) =>
                  Expr.quote(Expr.splice(Expr(n)) :: Expr.splice(acc))
                }
                (names, intermediateDecodersExpr(entries))
              }
            else None

          // Step 2d: A derived case class exposes its expected fields and its strictness (generic-extras' ExtrasDecoder).
          val productFields: Option[Expr[Configuration] => Expr[List[String]]] =
            if (helperOpt.isEmpty || sumMarker.isDefined || anyValCaseClass[A].isDefined || Type[A].isTuple) None
            else
              CaseClass.parse[A].toOption.filter(_ => SingletonValue.unapply(Type[A]).isEmpty).map { caseClass =>
                implicit val fieldNameT: Type[fieldName] = DTypes.FieldName
                implicit val transientFieldT: Type[transientField] = DTypes.TransientField
                val names: List[(String, Option[String])] = caseClass.primaryConstructor.totalParameters.flatten.toList
                  .filterNot { case (_, p) => hasAnnotationType[transientField](p) }
                  .map { case (name, p) => (name, getAnnotationStringArg[fieldName](p)) }
                (cfg: Expr[Configuration]) =>
                  names.foldRight(Expr.quote(Expr.splice(cfg).discriminator.toList)) {
                    case ((_, Some(custom)), acc) => Expr.quote(Expr.splice(Expr(custom)) :: Expr.splice(acc))
                    case ((name, None), acc)      =>
                      Expr.quote(Expr.splice(cfg).transformMemberNames(Expr.splice(Expr(name))) :: Expr.splice(acc))
                  }
              }

          // Step 3: Wrap cached defs around the entire KindlingsDecoder block
          // Both apply and decodeAccumulating call the same cached def with different failFast values
          // Without a cached helper the step-1 cache is not referenced (the direct decoder has its own).
          def withStep1Cache(body: Expr[KindlingsDecoder[A]]): Expr[KindlingsDecoder[A]] =
            if (helperOpt.isDefined) cache.toValDefs.use(_ => body) else body
          withStep1Cache {
            ValDefs.createVal[Configuration](configExpr).use { configVal =>
              Expr.quote {
                val cfg = Expr.splice(configVal)
                Expr.splice {
                  helperOpt match {
                    case Some(helper) if sumMarker.isDefined =>
                      val namesExpr: Expr[List[String]] = sumMarker.get._1
                      val intermediatesExpr: Expr[List[Decoder[Any]]] = sumMarker.get._2
                      Expr.quote {
                        hearth.kindlings.circederivation.internal.runtime.CirceDerivationFactories
                          .sumDecoderWithAccInstance[A](
                            (c: HCursor) => {
                              val _ = c
                              Expr
                                .splice(helper(Expr.quote(c), Expr.quote(cfg), Expr.quote(true)))
                                .asInstanceOf[Either[DecodingFailure, A]]
                            },
                            (c: HCursor) => {
                              val _ = c
                              Expr
                                .splice(helper(Expr.quote(c), Expr.quote(cfg), Expr.quote(false)))
                                .asInstanceOf[ValidatedNel[DecodingFailure, A]]
                            },
                            (name: String) =>
                              CirceDerivationUtils.sumHandles(
                                name,
                                cfg.transformConstructorNames,
                                Expr.splice(namesExpr),
                                Expr.splice(intermediatesExpr)
                              )
                          )
                      }
                    case Some(helper) if productFields.isDefined =>
                      val fieldsExpr: Expr[List[String]] = productFields.get(Expr.quote(cfg))
                      Expr.quote {
                        hearth.kindlings.circederivation.internal.runtime.CirceDerivationFactories
                          .productDecoderWithAccInstance[A](
                            (c: HCursor) => {
                              val _ = c
                              Expr
                                .splice(helper(Expr.quote(c), Expr.quote(cfg), Expr.quote(true)))
                                .asInstanceOf[Either[DecodingFailure, A]]
                            },
                            (c: HCursor) => {
                              val _ = c
                              Expr
                                .splice(helper(Expr.quote(c), Expr.quote(cfg), Expr.quote(false)))
                                .asInstanceOf[ValidatedNel[DecodingFailure, A]]
                            },
                            cfg.strictDecoding,
                            Expr.splice(fieldsExpr)
                          )
                      }
                    case Some(helper) =>
                      Expr.quote {
                        hearth.kindlings.circederivation.internal.runtime.CirceDerivationFactories
                          .decoderWithAccInstance[A](
                            (c: HCursor) => {
                              val _ = c
                              Expr
                                .splice(helper(Expr.quote(c), Expr.quote(cfg), Expr.quote(true)))
                                .asInstanceOf[Either[DecodingFailure, A]]
                            },
                            (c: HCursor) => {
                              val _ = c
                              Expr
                                .splice(helper(Expr.quote(c), Expr.quote(cfg), Expr.quote(false)))
                                .asInstanceOf[ValidatedNel[DecodingFailure, A]]
                            }
                          )
                      }
                    case None =>
                      Expr.quote {
                        val _ = cfg // suppress unused when the derived type is a built-in (#206)
                        // Delegates every method (incl. decodeAccumulating and tryDecode) to the direct decoder, so
                        // e.g. a derived Decoder[Option[X]] keeps circe's absent-key semantics.
                        hearth.kindlings.circederivation.internal.runtime.CirceDerivationFactories
                          .delegatingDecoderInstance[A](Expr.splice(directDecoderOpt.get))
                      }
                  }
                }
              }
            }
          }
        }
      }
      .flatTap { result =>
        Log.info(s"Derived final decoder result: ${result.prettyPrint}")
      }
      .runToExprOrFail(
        "KindlingsDecoder.derived",
        infoRendering = if (shouldWeLogDecoderDerivation) RenderFrom(Log.Level.Info) else DontRender,
        errorRendering = if (shouldWeLogDecoderDerivation) RenderFrom(Log.Level.Info) else DontRender,
        timeout = derivationTimeout
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
  }

  /** `KindlingsDecoder.patch[A]`: like circe-generic's `deriveFor[A].patch` (`IncompleteDerivedDecoders` +
    * `PatchWithOptions`) and generic-extras' configured equivalent. Every (non-transient) field `V` is decoded as
    * `Option[V]` with circe's `Decoder.decodeOption`, so an absent or `null` key keeps the original value, a value of
    * the wrong type fails at decode time, and a non-object patch fails. Nested values are replaced, not deep-merged.
    */
  @scala.annotation.nowarn("msg=is never used")
  def derivePatchDecoderTypeClass[A: Type](configExpr: Expr[Configuration]): Expr[Decoder[A => A]] = {
    implicit val FnA: Type[A => A] = DTypes.Endo[A]
    implicit val DecoderFnA: Type[Decoder[A => A]] = DTypes.Decoder[A => A]
    implicit val HCursorT: Type[HCursor] = DTypes.HCursor
    implicit val ConfigT: Type[Configuration] = DTypes.Configuration
    implicit val StringT: Type[String] = DTypes.String
    implicit val AnyT: Type[Any] = DTypes.Any
    implicit val ArrayAnyT: Type[Array[Any]] = DTypes.ArrayAny
    implicit val ArrayStringT: Type[Array[String]] = DTypes.ArrayString
    implicit val ListDecoderAnyT: Type[List[Decoder[Any]]] = DTypes.ListDecoderAny
    implicit val fieldNameT: Type[fieldName] = DTypes.FieldName
    implicit val transientFieldT: Type[transientField] = DTypes.TransientField

    val caseClass = CaseClass.parse[A].toEither match {
      case Right(cc) if !Type[A].isTuple && SingletonValue.unapply(Type[A]).isEmpty => cc
      case _                                                                        =>
        Environment.reportErrorAndAbort(
          s"KindlingsDecoder.patch: ${Type[A].prettyPrint} is not a case class; patch decoders can only be derived for case classes."
        )
    }
    rejectMultipleParameterLists[A](caseClass, "KindlingsDecoder.patch").foreach(Environment.reportErrorAndAbort(_))
    val evConfig: Option[Configuration] = semiEvalConfig(configExpr)
    evaluatedConfigFlags = evConfig

    Log
      .namedScope(s"Deriving patch decoder for ${Type[A].prettyPrint} at: ${Environment.currentPosition.prettyPrint}") {
        MIO.scoped { runSafe =>
          ValDefs.createVal[Configuration](configExpr).use { configVal =>
            val ctx = DecoderCtx.from[A](
              cursor = Expr.quote(null.asInstanceOf[HCursor]), // unused: field decoders bring their own cursors
              config = configVal,
              failFast = Expr.quote(true),
              derivedType = None,
              evaluatedConfig = evConfig
            )
            val params = caseClass.primaryConstructor.totalParameters.flatten.toList
              .filterNot { case (_, p) => hasAnnotationType[transientField](p) }

            // (field name expression, decoder) for every patched field, in declaration order.
            val decoded: List[(String, Expr[String], Expr[Decoder[Any]])] = runSafe {
              for {
                _ <- ensureStandardExtensionsLoaded()
                fields <- params.traverse { case (fName, param) =>
                  import param.tpe.Underlying as Field
                  deriveFieldDecoder[Field](Type[Field], ctx).map { decoderExpr =>
                    val nameExpr: Expr[String] = getAnnotationStringArg[fieldName](param)
                      .orElse(evConfig.map(_.transformMemberNames(fName))) match {
                      case Some(resolved) => Expr(resolved)
                      case None => Expr.quote(Expr.splice(configVal).transformMemberNames(Expr.splice(Expr(fName))))
                    }
                    implicit val DecoderField: Type[Decoder[Field]] = DTypes.Decoder[Field]
                    val asAny: Expr[Decoder[Any]] = Expr.quote(Expr.splice(decoderExpr).asInstanceOf[Decoder[Any]])
                    (fName, nameExpr, asAny)
                  }
                }
              } yield fields
            }
            val cache = runSafe(ctx.cache.get)
            val patchedIndex: Map[String, Int] = decoded.map(_._1).zipWithIndex.toMap

            val namesExpr: Expr[List[String]] =
              decoded.foldRight(Expr.quote(List.empty[String])) { case ((_, n, _), acc) =>
                Expr.quote(Expr.splice(n) :: Expr.splice(acc))
              }
            val decodersExpr: Expr[List[Decoder[Any]]] =
              decoded.foldRight(Expr.quote(List.empty[Decoder[Any]])) { case ((_, _, d), acc) =>
                Expr.quote(Expr.splice(d) :: Expr.splice(acc))
              }

            def construct(values: Expr[Array[Any]], original: Expr[A]): Expr[A] = {
              val fieldMap: Map[String, Expr_??] = caseClass
                .caseFieldValuesAt(original)
                .toList
                .map { case (fName, fieldValue) =>
                  import fieldValue.{Underlying as Field, value as fieldExpr}
                  val patched: Expr[Field] = patchedIndex.get(fName) match {
                    case Some(idx) =>
                      Expr.quote {
                        CirceDerivationUtils.patchValue[Field](
                          Expr.splice(values)(Expr.splice(Expr(idx))),
                          Expr.splice(fieldExpr)
                        )
                      }
                    case None => fieldExpr // transient: keep the original value
                  }
                  (fName, patched.as_??)
                }
                .toMap
              foldInstanceFree(caseClass.primaryConstructor, "Constructor")(
                onTypes = _ => Map.empty,
                onValues = _ => fieldMap
              ) match {
                case Right(constructExpr) => constructExpr.value.asInstanceOf[Expr[A]]
                case Left(error)          =>
                  Environment.reportErrorAndAbort(
                    DecoderDerivationError
                      .CannotConstructType(Type[A].prettyPrint, isSingleton = false, Some(error.toString))
                      .message
                  )
              }
            }

            cache.toValDefs.use { _ =>
              Expr.quote {
                val _ = Expr.splice(configVal)
                CirceDerivationUtils.patchDecoder[A](
                  Expr.splice(namesExpr),
                  Expr.splice(decodersExpr),
                  (values: Array[Any], original: A) => {
                    val _ = values
                    val _ = original
                    Expr.splice(construct(Expr.quote(values), Expr.quote(original)))
                  }
                )
              }
            }
          }
        }
      }
      .runToExprOrFail(
        "KindlingsDecoder.patch",
        infoRendering = if (shouldWeLogDecoderDerivation) RenderFrom(Log.Level.Info) else DontRender,
        errorRendering = if (shouldWeLogDecoderDerivation) RenderFrom(Log.Level.Info) else DontRender,
        timeout = derivationTimeout
      )(renderDecoderErrors)
  }

  /** `KindlingsDecoder.incomplete[F]`: like circe-generic's `deriveFor[F].incomplete` and generic-extras'
    * `deriveConfiguredFor[F].incomplete`. `F` is a function type `(P1, ..., Pn) => C` of a case class `C`: every `Pi`
    * stands for the first not yet supplied field of `C` whose type is `Pi` (shapeless' `RemoveAll`, which matches by
    * type), the other fields are decoded from the JSON object (with the configuration's names and defaults), and the
    * decoded value is a function that builds `C` from the supplied values.
    */
  @scala.annotation.nowarn("msg=is never used")
  def deriveIncompleteDecoderTypeClass[F: Type](configExpr: Expr[Configuration]): Expr[Decoder[F]] = {
    implicit val DecoderF: Type[Decoder[F]] = DTypes.Decoder[F]
    implicit val HCursorT: Type[HCursor] = DTypes.HCursor
    implicit val ConfigT: Type[Configuration] = DTypes.Configuration
    implicit val StringT: Type[String] = DTypes.String
    implicit val AnyT: Type[Any] = DTypes.Any
    implicit val ArrayAnyT: Type[Array[Any]] = DTypes.ArrayAny
    implicit val ListStringT: Type[List[String]] = DTypes.ListString
    implicit val ListDecoderAnyT: Type[List[Decoder[Any]]] = DTypes.ListDecoderAny
    implicit val DecoderAnyT: Type[Decoder[Any]] = DTypes.DecoderAny
    implicit val fieldNameT: Type[fieldName] = DTypes.FieldName
    implicit val transientFieldT: Type[transientField] = DTypes.TransientField

    val macroName = "KindlingsDecoder.incomplete"
    // (no pattern-val destructuring: the Scala 2 cross-quotes cannot refer to its synthetic tuple val)
    val components: (List[??], ??) = Type.functionComponents[F] match {
      case Some(fc) if fc._1.nonEmpty && fc._1.size <= 22 => fc
      case _                                              =>
        Environment.reportErrorAndAbort(
          s"$macroName: ${Type[F].prettyPrint} must be a function type (P1, ..., Pn) => C (1 to 22 parameters) of a case class C."
        )
    }
    val suppliedTypes: List[??] = components._1
    val result: ?? = components._2
    import result.Underlying as C
    val caseClass = CaseClass.parse[C].toEither match {
      case Right(cc) if !Type[C].isTuple && SingletonValue.unapply(Type[C]).isEmpty => cc
      case _                                                                        =>
        Environment.reportErrorAndAbort(s"$macroName: ${Type[C].prettyPrint} is not a case class.")
    }
    rejectMultipleParameterLists[C](caseClass, macroName).foreach(Environment.reportErrorAndAbort(_))
    val params: List[(String, Parameter)] = caseClass.primaryConstructor.totalParameters.flatten.toList

    // A `Labelled[Name, V]` takes the field named `Name` (circe-generic's shapeless `FieldType`), selected first; each
    // other supplied type takes the first remaining field of that type (shapeless' RemoveAll).
    val labelledCtor = Type.Ctor2.of[hearth.kindlings.circederivation.Labelled]
    val labelled: Map[Int, (String, ??)] = suppliedTypes.zipWithIndex.flatMap { case (tpe, argIndex) =>
      import tpe.Underlying as P
      labelledCtor.unapply(Type[P]).map { case (nameTpe, valueTpe) =>
        import nameTpe.Underlying as N
        Type.StringCodec.fromType(Type[N]) match {
          case Some(name) => argIndex -> (name.value, valueTpe)
          case None       =>
            Environment.reportErrorAndAbort(
              s"$macroName: the field name of ${Type[P].prettyPrint} must be a String literal type, e.g. Labelled[\"name\", Int]."
            )
        }
      }
    }.toMap
    val suppliedByLabel: Map[String, Int] =
      labelled.toList.sortBy(_._1).foldLeft(Map.empty[String, Int]) { case (acc, (argIndex, (name, valueTpe))) =>
        import valueTpe.Underlying as V
        params.find(_._1 == name) match {
          case Some((_, param)) if acc.contains(name) =>
            Environment.reportErrorAndAbort(s"$macroName: the field $name of ${Type[C].prettyPrint} is supplied twice.")
          case Some((_, param)) if param.tpe.Underlying =:= Type[V] => acc + (name -> argIndex)
          case Some((_, param))                                     =>
            Environment.reportErrorAndAbort(
              s"$macroName: the field $name of ${Type[C].prettyPrint} has type ${param.tpe.Underlying.prettyPrint}, not ${Type[V].prettyPrint}."
            )
          case None =>
            Environment.reportErrorAndAbort(s"$macroName: ${Type[C].prettyPrint} has no field named $name.")
        }
      }
    val supplied: Map[String, Int] = suppliedTypes.zipWithIndex
      .filterNot { case (_, argIndex) => labelled.contains(argIndex) }
      .foldLeft(suppliedByLabel) { case (acc, (tpe, argIndex)) =>
        import tpe.Underlying as P
        params.collectFirst {
          case (fName, param) if !acc.contains(fName) && param.tpe.Underlying =:= Type[P] => fName
        } match {
          case Some(fName) => acc + (fName -> argIndex)
          case None        =>
            Environment.reportErrorAndAbort(
              s"$macroName: ${Type[C].prettyPrint} has no (remaining) field of type ${Type[P].prettyPrint}."
            )
        }
      }
    val decodedParams = params.filterNot { case (fName, p) =>
      supplied.contains(fName) || hasAnnotationType[transientField](p)
    }
    val evConfig: Option[Configuration] = semiEvalConfig(configExpr)
    evaluatedConfigFlags = evConfig

    Log
      .namedScope(
        s"Deriving incomplete decoder for ${Type[F].prettyPrint} at: ${Environment.currentPosition.prettyPrint}"
      ) {
        MIO.scoped { runSafe =>
          ValDefs.createVal[Configuration](configExpr).use { configVal =>
            val ctx = DecoderCtx.from[C](
              cursor = Expr.quote(null.asInstanceOf[HCursor]), // unused: field decoders bring their own cursors
              config = configVal,
              failFast = Expr.quote(true),
              derivedType = None,
              evaluatedConfig = evConfig
            )
            def defaultOf(param: Parameter): Option[Expr[Any]] =
              if (param.hasDefault)
                param.defaultValue.flatMap { method =>
                  foldInstanceFree(method, "Default value")(
                    onTypes = _ => Map.empty,
                    onValues = _ => Map.empty
                  ).toOption
                    .map { ee => import ee.Underlying; ee.value.upcast[Any] }
                }
              else None

            val decoded: List[(String, Expr[String], Expr[Decoder[Any]])] = runSafe {
              for {
                _ <- ensureStandardExtensionsLoaded()
                fields <- decodedParams.traverse { case (fName, param) =>
                  val fieldTpe: ?? = param.tpe // a stable path for the Scala 2 cross-quotes
                  import fieldTpe.Underlying as Field
                  deriveFieldDecoder[Field](Type[Field], ctx).map { decoderExpr =>
                    val nameExpr: Expr[String] = getAnnotationStringArg[fieldName](param)
                      .orElse(evConfig.map(_.transformMemberNames(fName))) match {
                      case Some(resolved) => Expr(resolved)
                      case None => Expr.quote(Expr.splice(configVal).transformMemberNames(Expr.splice(Expr(fName))))
                    }
                    implicit val DecoderField: Type[Decoder[Field]] = DTypes.Decoder[Field]
                    val asAny: Expr[Decoder[Any]] = defaultOf(param) match {
                      case Some(default) =>
                        Expr.quote {
                          CirceDerivationUtils.defaultingDecoder(
                            Expr.splice(decoderExpr),
                            Expr.splice(default),
                            Expr.splice(configVal).useDefaults
                          )
                        }
                      case None => Expr.quote(Expr.splice(decoderExpr).asInstanceOf[Decoder[Any]])
                    }
                    (fName, nameExpr, asAny)
                  }
                }
              } yield fields
            }
            val cache = runSafe(ctx.cache.get)
            val decodedIndex: Map[String, Int] = decoded.map(_._1).zipWithIndex.toMap

            val namesExpr: Expr[List[String]] =
              decoded.foldRight(Expr.quote(List.empty[String])) { case ((_, n, _), acc) =>
                Expr.quote(Expr.splice(n) :: Expr.splice(acc))
              }
            val decodersExpr: Expr[List[Decoder[Any]]] =
              decoded.foldRight(Expr.quote(List.empty[Decoder[Any]])) { case ((_, _, d), acc) =>
                Expr.quote(Expr.splice(d) :: Expr.splice(acc))
              }

            def construct(values: Expr[Array[Any]], args: Expr[Array[Any]]): Expr[C] = {
              val fieldMap: Map[String, Expr_??] = params.map { case (fName, param) =>
                val fieldTpe: ?? = param.tpe // a stable path for the Scala 2 cross-quotes
                import fieldTpe.Underlying as Field
                val value: Expr[Field] = (supplied.get(fName), decodedIndex.get(fName)) match {
                  case (Some(argIndex), _) if labelled.contains(argIndex) =>
                    Expr.quote {
                      CirceDerivationUtils.castTo[Field](
                        CirceDerivationUtils.labelledValue(Expr.splice(args)(Expr.splice(Expr(argIndex))))
                      )
                    }
                  case (Some(argIndex), _) =>
                    Expr.quote(CirceDerivationUtils.castTo[Field](Expr.splice(args)(Expr.splice(Expr(argIndex)))))
                  case (_, Some(idx)) =>
                    Expr.quote(CirceDerivationUtils.castTo[Field](Expr.splice(values)(Expr.splice(Expr(idx)))))
                  case _ => // @transientField: its default value
                    defaultOf(param) match {
                      case Some(default) => Expr.quote(CirceDerivationUtils.castTo[Field](Expr.splice(default)))
                      case None          =>
                        Environment.reportErrorAndAbort(
                          DecoderDerivationError.TransientFieldMissingDefault(fName, Type[C].prettyPrint).message
                        )
                    }
                }
                (fName, value.as_??)
              }.toMap
              foldInstanceFree(caseClass.primaryConstructor, "Constructor")(
                onTypes = _ => Map.empty,
                onValues = _ => fieldMap
              ) match {
                case Right(constructExpr) => constructExpr.value.asInstanceOf[Expr[C]]
                case Left(error)          =>
                  Environment.reportErrorAndAbort(
                    DecoderDerivationError
                      .CannotConstructType(Type[C].prettyPrint, isSingleton = false, Some(error.toString))
                      .message
                  )
              }
            }

            cache.toValDefs.use { _ =>
              Expr.quote {
                val _ = Expr.splice(configVal)
                CirceDerivationUtils.incompleteDecoder[F](
                  Expr.splice(namesExpr),
                  Expr.splice(decodersExpr),
                  Expr.splice(Expr(suppliedTypes.size)),
                  (values: Array[Any], args: Array[Any]) => {
                    val _ = values
                    val _ = args
                    Expr.splice(construct(Expr.quote(values), Expr.quote(args)))
                  }
                )
              }
            }
          }
        }
      }
      .runToExprOrFail(
        macroName,
        infoRendering = if (shouldWeLogDecoderDerivation) RenderFrom(Log.Level.Info) else DontRender,
        errorRendering = if (shouldWeLogDecoderDerivation) RenderFrom(Log.Level.Info) else DontRender,
        timeout = derivationTimeout
      )(renderDecoderErrors)
  }

  /** `KindlingsDecoder.deriveUnwrapped[A]`: like generic-extras' `deriveUnwrappedDecoder`, a single-field case class is
    * decoded from its field's JSON (the field decoder's `tryDecode` is kept, so a wrapped `Option` decodes an absent
    * key as `None`).
    */
  @scala.annotation.nowarn("msg=is never used")
  def deriveUnwrappedDecoderTypeClass[A: Type](configExpr: Expr[Configuration]): Expr[KindlingsDecoder[A]] = {
    implicit val KindlingsDecoderA: Type[KindlingsDecoder[A]] = DTypes.KindlingsDecoder[A]
    implicit val HCursorT: Type[HCursor] = DTypes.HCursor
    implicit val ConfigT: Type[Configuration] = DTypes.Configuration
    val singleField: (CaseClass[A], Parameter) = singleFieldCaseClass[A]("KindlingsDecoder.deriveUnwrapped")
    val caseClass: CaseClass[A] = singleField._1
    val param: Parameter = singleField._2
    import param.tpe.Underlying as Field
    implicit val DecoderField: Type[Decoder[Field]] = DTypes.Decoder[Field]
    val evConfig: Option[Configuration] = semiEvalConfig(configExpr)
    evaluatedConfigFlags = evConfig

    Log
      .namedScope(s"Deriving unwrapped decoder for ${Type[A].prettyPrint}") {
        MIO.scoped { runSafe =>
          ValDefs.createVal[Configuration](configExpr).use { configVal =>
            val ctx =
              DecoderCtx.from[A](Expr.quote(null.asInstanceOf[HCursor]), configVal, Expr.quote(true), None, evConfig)
            val fieldDecoder: Expr[Decoder[Field]] = runSafe {
              ensureStandardExtensionsLoaded() >> deriveFieldDecoder[Field](Type[Field], ctx)
            }
            val cache = runSafe(ctx.cache.get)
            cache.toValDefs.use(_ => unwrappedDecoderExpr[A, Field](caseClass, param.name, fieldDecoder, configVal))
          }
        }
      }
      .runToExprOrFail(
        "KindlingsDecoder.deriveUnwrapped",
        infoRendering = if (shouldWeLogDecoderDerivation) RenderFrom(Log.Level.Info) else DontRender,
        errorRendering = if (shouldWeLogDecoderDerivation) RenderFrom(Log.Level.Info) else DontRender,
        timeout = derivationTimeout
      )(renderDecoderErrors)
  }

  @scala.annotation.nowarn("msg=is never used")
  private def unwrappedDecoderExpr[A: Type, Field: Type](
      caseClass: CaseClass[A],
      fieldName: String,
      fieldDecoder: Expr[Decoder[Field]],
      configVal: Expr[Configuration]
  ): Expr[KindlingsDecoder[A]] = {
    implicit val KindlingsDecoderA: Type[KindlingsDecoder[A]] = DTypes.KindlingsDecoder[A]
    implicit val ConfigT: Type[Configuration] = DTypes.Configuration
    def construct(value: Expr[Field]): Expr[A] =
      foldInstanceFree(caseClass.primaryConstructor, "Constructor")(
        onTypes = _ => Map.empty,
        onValues = _ => Map(fieldName -> value.as_??)
      ) match {
        case Right(constructExpr) => constructExpr.value.asInstanceOf[Expr[A]]
        case Left(error)          => Environment.reportErrorAndAbort(error.toString)
      }
    Expr.quote {
      val _ = Expr.splice(configVal)
      hearth.kindlings.circederivation.internal.runtime.CirceDerivationFactories.delegatingDecoderInstance[A](
        CirceDerivationUtils.mapDecoder(
          Expr.splice(fieldDecoder),
          (value: Field) => {
            val _ = value
            Expr.splice(construct(Expr.quote(value)))
          }
        )
      )
    }
  }

  protected def renderDecoderErrors(errorLogs: String, errors: hearth.fp.data.NonEmptyVector[Throwable]): String = {
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

  // Handles logging, error reporting and prepending "cached" defs and vals to the result.

  def deriveDecoderFromCtxAndAdaptForEntrypoint[A: Type, Out: Type](macroName: String)(
      provideCtxAndAdapt: (DecoderCtx[A] => Expr[Either[DecodingFailure, A]]) => Expr[Out]
  ): Expr[Out] = {
    if (Type[A] =:= Type.of[Nothing].asInstanceOf[Type[A]] || Type[A] =:= Type.of[Any].asInstanceOf[Type[A]])
      Environment.reportErrorAndAbort(
        s"$macroName: type parameter was inferred as Nothing, which is likely unintended.\n" +
          s"Provide an explicit type parameter, e.g.: $macroName[MyType](...)\n" +
          "or add a type ascription to the result variable."
      )
    Log
      .namedScope(
        s"Deriving decoder for ${Type[A].prettyPrint} at: ${Environment.currentPosition.prettyPrint}"
      ) {
        MIO.scoped { runSafe =>
          val fromCtx: (DecoderCtx[A] => Expr[Either[DecodingFailure, A]]) =
            (ctx: DecoderCtx[A]) =>
              runSafe {
                for {
                  _ <- ensureStandardExtensionsLoaded()
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
        errorRendering = if (shouldWeLogDecoderDerivation) RenderFrom(Log.Level.Info) else DontRender,
        timeout = derivationTimeout
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
      failFast: Expr[Boolean],
      cache: MLocal[ValDefsCache],
      derivedType: Option[??],
      evaluatedConfig: Option[Configuration] = None
  ) {

    /** Nested derivation keeps `failFast`: every rule returns an `Either` when it is true and a `ValidatedNel` when it
      * is false (typed as `Either` in the rule signatures), so accumulating decoding accumulates in nested types too.
      */
    def nest[B: Type](newCursor: Expr[HCursor]): DecoderCtx[B] = copy[B](
      tpe = Type[B],
      cursor = newCursor
    )

    def nestWithFailFast[B: Type](newCursor: Expr[HCursor], newFailFast: Expr[Boolean]): DecoderCtx[B] = copy[B](
      tpe = Type[B],
      cursor = newCursor,
      failFast = newFailFast
    )

    def nestInCache(
        newCursor: Expr[HCursor],
        newConfig: Expr[Configuration],
        newFailFast: Expr[Boolean]
    ): DecoderCtx[A] = copy(
      cursor = newCursor,
      config = newConfig,
      failFast = newFailFast
    )

    def getInstance[B: Type]: MIO[Option[Expr[Decoder[B]]]] = {
      implicit val DecoderB: Type[Decoder[B]] = DTypes.Decoder[B]
      cache.get0Ary[Decoder[B]]("cached-decoder-instance")
    }
    def setInstance[B: Type](instance: Expr[Decoder[B]]): MIO[Unit] = {
      implicit val DecoderB: Type[Decoder[B]] = DTypes.Decoder[B]
      Log.info(s"Caching Decoder instance for ${Type[B].prettyPrint}") >>
        cache.buildCachedWith(
          "cached-decoder-instance",
          ValDefBuilder.ofLazy[Decoder[B]](s"decoder_${Type[B].shortName}")
        )(_ => instance)
    }

    // The cached decode helper returns `Any` because the actual return type depends on `failFast`:
    //   - failFast=true  → Either[DecodingFailure, B]
    //   - failFast=false → ValidatedNel[DecodingFailure, B]
    //
    // Ideally we'd express this as a match type:
    //   type Result[FailFast <: Boolean] = FailFast match {
    //     case true  => [A] =>> Either[DecodingFailure, A]
    //     case false => [A] =>> ValidatedNel[DecodingFailure, A]
    //   }
    //   def decode_Foo(cursor: HCursor, config: Configuration, failFast: Boolean): Result[failFast.type][Foo]
    //
    // but writing a reusable macro utility (ValDefBuilder, ValDefsCache) that works with
    // match types would be very difficult — even without Scala 2.13 support. So we use `Any`
    // as the return type and cast at each call site where the actual type is known.
    //
    // The cache key includes Type[B].prettyPrint to prevent collisions: ValDefsCache keys by
    // (String, arg types, return type), and since the return type is always `Any`, the string
    // must disambiguate different types.
    private def helperCacheKey[B: Type]: String = s"cached-decode-method:${Type[B].prettyPrint}"

    def getHelper[B: Type]: MIO[Option[(Expr[HCursor], Expr[Configuration], Expr[Boolean]) => Expr[Any]]] = {
      implicit val AnyT: Type[Any] = DTypes.Any
      implicit val HCursorT: Type[HCursor] = DTypes.HCursor
      implicit val ConfigT: Type[Configuration] = DTypes.Configuration
      implicit val BooleanT: Type[Boolean] = DTypes.Boolean
      cache.get3Ary[HCursor, Configuration, Boolean, Any](helperCacheKey[B])
    }
    def setHelper[B: Type](
        helper: (Expr[HCursor], Expr[Configuration], Expr[Boolean]) => MIO[Expr[Any]]
    ): MIO[Unit] = {
      implicit val AnyT: Type[Any] = DTypes.Any
      implicit val HCursorT: Type[HCursor] = DTypes.HCursor
      implicit val ConfigT: Type[Configuration] = DTypes.Configuration
      implicit val BooleanT: Type[Boolean] = DTypes.Boolean
      val defBuilder =
        ValDefBuilder.ofDef3[HCursor, Configuration, Boolean, Any](s"decode_${Type[B].shortName}")
      for {
        _ <- Log.info(s"Forward-declaring decode helper for ${Type[B].prettyPrint}")
        _ <- cache.forwardDeclare(helperCacheKey[B], defBuilder)
        _ <- MIO.scoped { runSafe =>
          runSafe(cache.buildCachedWith(helperCacheKey[B], defBuilder) { case (_, (cursor, config, failFast)) =>
            runSafe(helper(cursor, config, failFast))
          })
        }
        _ <- Log.info(s"Defined decode helper for ${Type[B].prettyPrint}")
      } yield ()
    }

    override def toString: String =
      s"decode[${tpe.prettyPrint}](cursor = ${cursor.prettyPrint}, config = ${config.prettyPrint})"
  }
  object DecoderCtx {

    def from[A: Type](
        cursor: Expr[HCursor],
        config: Expr[Configuration],
        failFast: Expr[Boolean],
        derivedType: Option[??],
        evaluatedConfig: Option[Configuration] = None
    ): DecoderCtx[A] = DecoderCtx(
      tpe = Type[A],
      cursor = cursor,
      config = config,
      failFast = failFast,
      cache = ValDefsCache.mlocal,
      derivedType = derivedType,
      evaluatedConfig = evaluatedConfig
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
          DecoderUseCachedDefWhenAvailableRule,
          DecoderHandleAsLiteralTypeRule,
          DecoderUseImplicitWhenAvailableRule,
          DecoderDerivationPolicyRule,
          DecoderHandleAsBuiltInTypeRule,
          DecoderHandleAsValueTypeRule,
          DecoderHandleAsOptionRule,
          // DecoderHandleAsCollectionRule now handles maps too (single IsCollection parse + IsMapOf dispatch),
          // so the standalone DecoderHandleAsMapRule is no longer in the chain (its object is kept for
          // decodeMapEntries / deriveKeyDecoder).
          DecoderHandleAsCollectionRule,
          DecoderHandleAsNamedTupleRule,
          DecoderHandleAsTupleRule,
          DecoderHandleAsSingletonRule,
          DecoderHandleAsCaseClassRule,
          DecoderHandleAsEnumRule
        )(_[A]).flatMap {
          case Right(result) =>
            Log.info(s"Derived decoder for ${Type[A].prettyPrint}: ${result.prettyPrint}") >>
              MIO.pure(result)
          case Left(reasons) =>
            val reasonsStrings = reasons.toListMap
              .removed(DecoderUseCachedDefWhenAvailableRule)
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

  /** Derive a Decoder[Field] for a case class field. Tries implicit summoning first, falls back to recursive derivation
    * via the full rule chain (with both `apply` and `decodeAccumulating`).
    */
  @scala.annotation.nowarn("msg=is never used|unused explicit parameter")
  protected def deriveFieldDecoder[Field: Type](implicit ctx: DecoderCtx[?]): MIO[Expr[Decoder[Field]]] =
    DTypes
      .Decoder[Field]
      .summonExprIgnoring(DecoderUseImplicitWhenAvailableRule.ignoredImplicits*)
      .toEither match {
      case Right(decoderExpr) =>
        Log.info(s"Found implicit Decoder[${Type[Field].prettyPrint}]") >> MIO.pure(decoderExpr)
      case Left(_) =>
        Log.info(s"Building Decoder[${Type[Field].prettyPrint}] via recursive derivation") >>
          buildDerivedDecoder[Field](ctx.config, ctx)
    }

  /** Builds a `Decoder[B]` from the rule chain: one lambda `(cursor, failFast) => result` serves both `apply` and
    * `decodeAccumulating`; types whose decoder accepts `null` also get circe's absent-key `tryDecode`.
    */
  @scala.annotation.nowarn("msg=is never used")
  private def buildDerivedDecoder[B: Type](
      config: Expr[Configuration],
      parent: DecoderCtx[?]
  ): MIO[Expr[Decoder[B]]] = {
    implicit val HCursorT: Type[HCursor] = DTypes.HCursor
    implicit val BooleanT: Type[Boolean] = DTypes.Boolean
    implicit val AnyT: Type[Any] = DTypes.Any
    implicit val EitherDFB: Type[Either[DecodingFailure, B]] = DTypes.DecoderResult[B]
    LambdaBuilder
      .of2[HCursor, Boolean]("cursor", "failFast")
      .traverse { case (cursorExpr, failFastExpr) =>
        deriveDecoderRecursively[B](using
          parent.copy[B](tpe = Type[B], cursor = cursorExpr, config = config, failFast = failFastExpr)
        ).map(_.upcast[Any])
      }
      .map { builder =>
        val decodeFn = builder.build[Any]
        if (acceptsNull[B]) Expr.quote(CirceDerivationUtils.nullableDecoderFromFF[B](Expr.splice(decodeFn)))
        else Expr.quote(CirceDerivationUtils.decoderFromFF[B](Expr.splice(decodeFn)))
      }
  }

  /** Builds the decoder for a top-level derived type without a cached helper (collections, options, value types...). */
  private def deriveDecoderViaLambda[A: Type](
      configExpr: Expr[Configuration],
      selfType: Option[??],
      evConfig: Option[Configuration]
  ): MIO[Expr[Decoder[A]]] = {
    implicit val HCursorT: Type[HCursor] = DTypes.HCursor
    val ctx =
      DecoderCtx.from[A](Expr.quote(null.asInstanceOf[HCursor]), configExpr, Expr.quote(true), selfType, evConfig)
    for {
      _ <- ensureStandardExtensionsLoaded()
      decoder <- buildDerivedDecoder[A](configExpr, ctx)
      cache <- ctx.cache.get
    } yield cache.toValDefs.use(_ => decoder)
  }

  /** Adapts a fail-fast `Either` to the result representation selected by `dctx.failFast`. */
  protected def liftEither[A: DecoderCtx](
      either: Expr[Either[DecodingFailure, A]]
  ): Expr[Either[DecodingFailure, A]] = {
    implicit val EitherDFA: Type[Either[DecodingFailure, A]] = DTypes.DecoderResult[A]
    Expr.quote {
      CirceDerivationUtils.unsafeResult[Either[DecodingFailure, A]](
        CirceDerivationUtils.fromEitherResult(Expr.splice(dctx.failFast), Expr.splice(either))
      )
    }
  }

  /** Runs a decoder instance in the mode selected by `dctx.failFast`. */
  protected def runDecoderInstance[A: DecoderCtx](decoder: Expr[Decoder[A]]): Expr[Either[DecodingFailure, A]] = {
    implicit val EitherDFA: Type[Either[DecodingFailure, A]] = DTypes.DecoderResult[A]
    Expr.quote {
      CirceDerivationUtils.unsafeResult[Either[DecodingFailure, A]](
        CirceDerivationUtils.runDecoder(Expr.splice(decoder), Expr.splice(dctx.cursor), Expr.splice(dctx.failFast))
      )
    }
  }

  /** Whether `B` is decoded from `null` (an Option, or a value type wrapping one), so an absent key decodes too. */
  protected def acceptsNull[B: Type]: Boolean = Type[B] match {
    case IsOption(_)               => true
    case _ if Type[B].isNamedTuple => false
    case IsValueType(isValueType)  =>
      import isValueType.Underlying as Inner
      Type[Inner] match {
        case IsOption(_) => true
        case _           => false
      }
    case _ => false
  }

  // User-provided instances of sum children (intermediate sealed traits, leaves)

  private lazy val userDecoderCache = scala.collection.mutable.Map.empty[String, Option[Expr[Decoder[Any]]]]

  /** A user-provided `Decoder` of a sum's child (summoned ignoring Kindlings' auto-derivation), memoized. */
  @scala.annotation.nowarn("msg=is never used")
  protected def userDecoderOf(child: ??): Option[Expr[Decoder[Any]]] = {
    import child.Underlying as C
    implicit val DecoderC: Type[Decoder[C]] = DTypes.Decoder[C]
    implicit val DecoderAnyT: Type[Decoder[Any]] = DTypes.DecoderAny
    userDecoderCache.getOrElseUpdate(
      Type[C].plainPrint,
      DTypes
        .Decoder[C]
        .summonExprIgnoring(DecoderUseImplicitWhenAvailableRule.ignoredImplicits*)
        .toOption
        .map(d => Expr.quote(Expr.splice(d).asInstanceOf[Decoder[Any]]))
    )
  }

  /** The entries a sum decoder dispatches on (see [[flattenedChildren]]). */
  protected def decoderSumEntries[A: Type](enumm: Enum[A]): List[(String, ??)] =
    flattenedChildren[A](enumm)(child => userDecoderOf(child).isDefined)

  /** The user-provided instances of the intermediate sums among `entries`: at runtime those derived by Kindlings
    * (`KindlingsSumDecoder`) handle the constructor names of their own subtypes.
    */
  @scala.annotation.nowarn("msg=is never used")
  protected def intermediateDecodersExpr(entries: List[(String, ??)]): Expr[List[Decoder[Any]]] = {
    implicit val DecoderAnyT: Type[Decoder[Any]] = DTypes.DecoderAny
    implicit val ListDecoderAnyT: Type[List[Decoder[Any]]] = DTypes.ListDecoderAny
    entries
      .collect { case (_, child) if isNestedSumType(child) => userDecoderOf(child) }
      .flatten
      .foldRight(Expr.quote(List.empty[Decoder[Any]])) { (d, acc) =>
        Expr.quote(Expr.splice(d) :: Expr.splice(acc))
      }
  }

  protected def hasIntermediateDecoders(entries: List[(String, ??)]): Boolean =
    entries.exists { case (_, child) => isNestedSumType(child) && userDecoderOf(child).isDefined }

  // Types

  private[compiletime] object DTypes {

    def Decoder: Type.Ctor1[Decoder] = Type.Ctor1.of[Decoder]
    def KeyDecoder: Type.Ctor1[KeyDecoder] = Type.Ctor1.of[KeyDecoder]
    def KindlingsDecoder: Type.Ctor1[KindlingsDecoder] = Type.Ctor1.of[KindlingsDecoder]
    val DecoderLogDerivation: Type[hearth.kindlings.circederivation.KindlingsDecoder.LogDerivation] =
      Type.of[hearth.kindlings.circederivation.KindlingsDecoder.LogDerivation]
    val Json: Type.Lazy[Json] = Type.Lazy(Type.of[Json])
    val HCursor: Type.Lazy[HCursor] = Type.Lazy(Type.of[HCursor])
    val DecodingFailure: Type.Lazy[DecodingFailure] = Type.Lazy(Type.of[DecodingFailure])
    val Configuration: Type.Lazy[Configuration] = Type.Lazy(Type.of[Configuration])
    val String: Type.Lazy[String] = Type.Lazy(Type.of[String])
    val Int: Type.Lazy[Int] = Type.Lazy(Type.of[Int])
    val Long: Type.Lazy[Long] = Type.Lazy(Type.of[Long])
    val Double: Type.Lazy[Double] = Type.Lazy(Type.of[Double])
    val Boolean: Type.Lazy[Boolean] = Type.Lazy(Type.of[Boolean])
    val Any: Type.Lazy[Any] = Type.Lazy(Type.of[Any])
    val Unit: Type.Lazy[Unit] = Type.Lazy(Type.of[Unit])
    val ArrayAny: Type.Lazy[Array[Any]] = Type.Lazy(Type.of[Array[Any]])
    val EitherDFAny: Type.Lazy[Either[DecodingFailure, Any]] = Type.Lazy(Type.of[Either[DecodingFailure, Any]])
    val EitherDFUnit: Type.Lazy[Either[DecodingFailure, Unit]] = Type.Lazy(Type.of[Either[DecodingFailure, Unit]])
    val ListEitherDFAny: Type[List[Either[DecodingFailure, Any]]] =
      Type.of[List[Either[DecodingFailure, Any]]]
    val ListString: Type.Lazy[List[String]] = Type.Lazy(Type.of[List[String]])
    val SetString: Type.Lazy[Set[String]] = Type.Lazy(Type.of[Set[String]])
    val StringHCursorTuple: Type.Lazy[(String, HCursor)] = Type.Lazy(Type.of[(String, HCursor)])
    val FieldName: Type.Lazy[fieldName] = Type.Lazy(Type.of[fieldName])
    val TransientField: Type.Lazy[transientField] = Type.Lazy(Type.of[transientField])

    def DecoderResult[A: Type]: Type[Either[DecodingFailure, A]] =
      Type.of[Either[DecodingFailure, A]]
    def Endo[A: Type]: Type[A => A] = Type.of[A => A]
    val ArrayString: Type.Lazy[Array[String]] = Type.Lazy(Type.of[Array[String]])
    val ListDecoderAny: Type.Lazy[List[Decoder[Any]]] = Type.Lazy(Type.of[List[Decoder[Any]]])
    val DecoderAny: Type.Lazy[Decoder[Any]] = Type.Lazy(Type.of[Decoder[Any]])

    // Accumulating types
    def ValidatedNelDF[A: Type]: Type[ValidatedNel[DecodingFailure, A]] =
      Type.of[ValidatedNel[DecodingFailure, A]]
    val ValidatedNelDFAny: Type[ValidatedNel[DecodingFailure, Any]] =
      Type.of[ValidatedNel[DecodingFailure, Any]]
    val ValidatedNelDFUnit: Type[ValidatedNel[DecodingFailure, Unit]] =
      Type.of[ValidatedNel[DecodingFailure, Unit]]
    val ValidatedNelDFArrayAny: Type[ValidatedNel[DecodingFailure, Array[Any]]] =
      Type.of[ValidatedNel[DecodingFailure, Array[Any]]]
    val ListValidatedNelDFAny: Type[List[ValidatedNel[DecodingFailure, Any]]] =
      Type.of[List[ValidatedNel[DecodingFailure, Any]]]
  }
}
