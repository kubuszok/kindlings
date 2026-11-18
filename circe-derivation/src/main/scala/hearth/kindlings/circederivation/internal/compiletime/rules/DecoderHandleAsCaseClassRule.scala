package hearth.kindlings.circederivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.data.NonEmptyList
import hearth.fp.effect.*
import hearth.fp.syntax.*
import hearth.std.*

import hearth.kindlings.circederivation.Configuration
import hearth.kindlings.circederivation.annotations.{fieldName, transientField}
import hearth.kindlings.circederivation.internal.runtime.CirceDerivationUtils
import cats.data.{Validated, ValidatedNel}
import io.circe.{DecodingFailure, HCursor}

trait DecoderHandleAsCaseClassRuleImpl {
  this: DecoderMacrosImpl & MacroCommons & StdExtensions & AnnotationSupport =>

  object DecoderHandleAsCaseClassRule extends DecoderDerivationRule("handle as case class when possible") {

    def apply[A: DecoderCtx]: MIO[Rule.Applicability[Expr[Either[DecodingFailure, A]]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as a case class") >> {
        CaseClass.parse[A].toEither match {
          case Right(caseClass) =>
            for {
              // A case class with fields gets a fail-fast and an accumulating def; one without fields one helper
              // (both are wrapped by `getHelper`).
              _ <-
                if (caseClass.primaryConstructor.totalParameters.flatten.isEmpty)
                  dctx.setHelper[A] { (cursor, config, failFast) =>
                    decodeCaseClassFields[A](caseClass)(using dctx.nestInCache(cursor, config, failFast)).flatMap {
                      case Left(body) => MIO.pure(body)
                      case Right(_)   => MIO.fail(new IllegalStateException("expected a case class without fields"))
                    }
                  }
                else
                  dctx.setModeHelpers[A](decodeCaseClassFields[A](caseClass).flatMap {
                    case Right(bodies) => MIO.pure(bodies)
                    case Left(_)       => MIO.fail(new IllegalStateException("expected a case class with fields"))
                  })
              result <- dctx.getHelper[A].flatMap {
                case Some(helperCall) =>
                  implicit val EitherDFA: Type[Either[DecodingFailure, A]] = DTypes.DecoderResult[A]
                  MIO.pure(Rule.matched(Expr.quote {
                    CirceDerivationUtils.unsafeResult[Either[DecodingFailure, A]](
                      Expr.splice(helperCall(dctx.cursor, dctx.config, dctx.failFast))
                    )
                  }))
                case None =>
                  MIO.pure(Rule.yielded(s"Failed to build helper for ${Type[A].prettyPrint}"))
              }
            } yield result

          case Left(reason) =>
            MIO.pure(Rule.yielded(reason))
        }
      }

    /** How an absent field is decoded when its default is not used. */
    sealed private trait FieldKind
    private case object PlainField extends FieldKind
    private case object OptionField extends FieldKind

    /** The body of a helper decoding the case class in the mode selected by the runtime `dctx.failFast` (both modes
      * inlined; used for an `AnyVal` case class, whose helper also has the unwrapped form).
      */
    private[rules] def decodeCaseClassAsHelperBody[A: DecoderCtx](caseClass: CaseClass[A]): MIO[Expr[Any]] = {
      implicit val AnyT: Type[Any] = DTypes.Any
      decodeCaseClassFields[A](caseClass).map {
        case Left(body)                    => body
        case Right((failFast, accumulate)) =>
          Expr.quote {
            (if (Expr.splice(dctx.failFast)) Expr.splice(failFast(dctx.cursor))
             else Expr.splice(accumulate(dctx.cursor))): Any
          }
      }
    }

    /** A case class without fields: the helper's body (honouring the runtime `dctx.failFast`); with fields: the bodies
      * of its fail-fast and accumulating defs, given their cursor.
      */
    @scala.annotation.nowarn("msg=is never used|unused explicit parameter|Non local returns")
    private[rules] def decodeCaseClassFields[A: DecoderCtx](
        caseClass: CaseClass[A]
    ): MIO[Either[
      Expr[Any],
      (Expr[HCursor] => Expr[Either[DecodingFailure, A]], Expr[HCursor] => Expr[ValidatedNel[DecodingFailure, A]])
    ]] = {
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

      // circe names a product by its simple name (`Mirror.MirroredLabel`) in strict-decoding failures.
      val typeNameExpr: Expr[String] = Expr(Type[A].shortName)
      rejectMultipleParameterLists[A](caseClass, "KindlingsDecoder").foreach { msg =>
        return Log.error(msg) >> MIO.fail(
          DecoderDerivationError.CannotConstructType(Type[A].prettyPrint, isSingleton = false, Some(msg))
        )
      }
      val constructor = caseClass.primaryConstructor
      val fieldsList = constructor.totalParameters.flatten.toList

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
                implicit val ValidatedNelDFA: Type[ValidatedNel[DecodingFailure, A]] = DTypes.ValidatedNelDF[A]
                implicit val ValidatedNelDFUnitT: Type[ValidatedNel[DecodingFailure, Unit]] =
                  DTypes.ValidatedNelDFUnit
                val strictDecodingStaticallyFalse: Boolean =
                  dctx.evaluatedConfig.exists(!_.strictDecoding)
                MIO.pure(
                  if (strictDecodingStaticallyFalse) {
                    // strictDecoding is statically false — skip the strict decoding check entirely
                    Expr.quote {
                      (if (Expr.splice(dctx.failFast)) {
                         CirceDerivationUtils.checkIsObject(Expr.splice(dctx.cursor)).map { _ =>
                           Expr.splice(expr)
                         }
                       } else {
                         CirceDerivationUtils
                           .checkIsObjectAccumulating(Expr.splice(dctx.cursor))
                           .map { _ =>
                             Expr.splice(expr)
                           }
                       }): Any
                    }
                  } else {
                    Expr.quote {
                      (if (Expr.splice(dctx.failFast)) {
                         CirceDerivationUtils.checkIsObject(Expr.splice(dctx.cursor)).flatMap { _ =>
                           val config = Expr.splice(dctx.config)
                           if (config.strictDecoding)
                             CirceDerivationUtils
                               .checkStrictDecoding(
                                 Expr.splice(dctx.cursor),
                                 Expr.splice(typeNameExpr),
                                 config.discriminator.toList
                               )
                               .map(_ => Expr.splice(expr))
                           else
                             Right(Expr.splice(expr)): Either[DecodingFailure, A]
                         }
                       } else {
                         CirceDerivationUtils
                           .checkIsObjectAccumulating(Expr.splice(dctx.cursor))
                           .andThen { _ =>
                             if (Expr.splice(dctx.config).strictDecoding)
                               CirceDerivationUtils
                                 .checkStrictDecodingAccumulating(
                                   Expr.splice(dctx.cursor),
                                   Expr.splice(typeNameExpr),
                                   Expr.splice(dctx.config).discriminator.toList
                                 )
                                 .map(_ => Expr.splice(expr))
                             else
                               Validated.valid(Expr.splice(expr)): ValidatedNel[DecodingFailure, A]
                           }
                       }): Any
                    }
                  }
                )
              case None =>
                val err = DecoderDerivationError.CannotConstructType(Type[A].prettyPrint, isSingleton = false)
                Log.error(err.message) >> MIO.fail(err)
            }
            .map { decodeFields =>
              // Reject non-object input, like circe's derived product decoders (bug #235): both fail-fast and
              // accumulating report only "expecting object" (Scala 3 core `ConfiguredDecoder.decodeProductBase`).
              Left(Expr.quote {
                if (Expr.splice(dctx.cursor).value.isObject) Expr.splice(decodeFields)
                else CirceDerivationUtils.notAnObject(Expr.splice(dctx.cursor), Expr.splice(dctx.failFast))
              })
            }

        case Some(fields) =>
          implicit val AnyT: Type[Any] = DTypes.Any
          implicit val EitherDFAnyT: Type[Either[DecodingFailure, Any]] = DTypes.EitherDFAny
          implicit val ValidatedNelDFAnyT: Type[ValidatedNel[DecodingFailure, Any]] = DTypes.ValidatedNelDFAny
          implicit val EitherDFA: Type[Either[DecodingFailure, A]] = DTypes.DecoderResult[A]
          implicit val ValidatedNelDFA: Type[ValidatedNel[DecodingFailure, A]] = DTypes.ValidatedNelDF[A]
          implicit val NelDFT: Type[cats.data.NonEmptyList[DecodingFailure]] = DTypes.NelDF
          implicit val ListStringT: Type[List[String]] = DTypes.ListString

          // Step 1: For each field, derive a decoder (cached per instance, see deriveFieldDecoder) and build its
          // fail-fast and accumulating decoding from a cursor, plus its constructor argument.
          fields
            .parTraverse { case (fName, param) =>
              import param.tpe.Underlying as Field
              val isTransient = hasAnnotationType[transientField](param)
              val nameOverride = getAnnotationStringArg[fieldName](param)
              Log.namedScope(s"Deriving decoder for field $fName: ${Type[Field].prettyPrint}") {
                // A @transientField is not decoded (its default is used), so it needs no decoder.
                (if (isTransient) MIO.pure(Option.empty[Expr[io.circe.Decoder[Field]]])
                 else deriveFieldDecoder[Field].map(Some(_))).map { decoderOpt =>
                  def decoderExpr: Expr[io.circe.Decoder[Field]] = decoderOpt.get
                  val defaultAsAnyOpt: Option[Expr[Any]] =
                    if (param.hasDefault)
                      param.defaultValue.flatMap { method =>
                        foldInstanceFree(method, "Default value")(
                          onTypes = _ => Map.empty,
                          onValues = _ => Map.empty
                        ).toOption
                          .map { ee => import ee.Underlying; ee.value.upcast[Any] }
                      }
                    else None

                  // Pre-compute field name when evaluatedConfig is available and no @fieldName override
                  val resolvedFieldName: Option[String] = nameOverride.orElse(
                    dctx.evaluatedConfig.map(_.transformMemberNames(fName))
                  )
                  val useDefaultsStaticallyFalse: Boolean =
                    dctx.evaluatedConfig.exists(!_.useDefaults)

                  // Detect Option[_] fields at compile time — absent fields should decode as None,
                  // not fail (bug #120: circe's .as() on a failed cursor fails immediately without
                  // calling the decoder, so Decoder[Option[T]] never gets a chance to return None).
                  // This holds whether or not the field has a default (with useDefaults=false circe ignores the
                  // default, and its Option decoder still turns a missing key into None).
                  // A value type wrapping an Option (`case class MaybeName(v: Option[String]) extends AnyVal`) is
                  // decoded as if the absent field was `null`, so the wrapped Option decoder yields `None`.
                  val fieldKind: FieldKind = Type[Field] match {
                    case IsOption(_)                   => OptionField
                    case _ if Type[Field].isNamedTuple => PlainField
                    // Decoded with `tryDecode` like an Option: a derived (unwrapping) decoder accepts an absent key
                    // as `null` (`nullableDecoderFromFF`), while a user-provided decoder keeps circe's semantics
                    // (by default an absent key is a missing-field failure).
                    case IsValueType(isValueType) =>
                      import isValueType.Underlying as Inner
                      Type[Inner] match {
                        case IsOption(_) => OptionField
                        case _           => PlainField
                      }
                    case _ => PlainField
                  }

                  // Field name expression: a constant when resolved at compile time, else computed from the config.
                  def fieldNameExpr: Expr[String] = resolvedFieldName match {
                    case Some(resolvedName) => Expr(resolvedName)
                    case None => Expr.quote(Expr.splice(dctx.config).transformMemberNames(Expr.splice(Expr(fName))))
                  }

                  // Decoding without using the default value.
                  def plainFF(cursor: Expr[HCursor], name: Expr[String]): Expr[Either[DecodingFailure, Any]] =
                    fieldKind match {
                      case OptionField =>
                        Expr.quote {
                          CirceDerivationUtils.decodeOptionFieldAsAny(
                            Expr.splice(cursor),
                            Expr.splice(name),
                            Expr.splice(decoderExpr)
                          )
                        }
                      case PlainField =>
                        Expr.quote {
                          Expr
                            .splice(cursor)
                            .downField(Expr.splice(name))
                            .as(Expr.splice(decoderExpr))
                            .asInstanceOf[Either[DecodingFailure, Any]]
                        }
                    }
                  def plainAcc(cursor: Expr[HCursor], name: Expr[String]): Expr[ValidatedNel[DecodingFailure, Any]] =
                    fieldKind match {
                      case OptionField =>
                        Expr.quote {
                          CirceDerivationUtils.decodeOptionFieldAccumulatingAsAny(
                            Expr.splice(cursor),
                            Expr.splice(name),
                            Expr.splice(decoderExpr)
                          )
                        }
                      case PlainField =>
                        Expr.quote {
                          Expr
                            .splice(decoderExpr)
                            .tryDecodeAccumulating(Expr.splice(cursor).downField(Expr.splice(name)))
                            .asInstanceOf[ValidatedNel[DecodingFailure, Any]]
                        }
                    }

                  // --- Fail-fast decode expression ---
                  val ffOf: Expr[HCursor] => Expr[Either[DecodingFailure, Any]] = cursor =>
                    if (isTransient) {
                      defaultAsAnyOpt match {
                        case Some(d) => Expr.quote(Right(Expr.splice(d)): Either[DecodingFailure, Any])
                        case None    => Expr.quote(Right(null): Either[DecodingFailure, Any])
                      }
                    } else
                      defaultAsAnyOpt match {
                        case Some(_) if useDefaultsStaticallyFalse =>
                          // useDefaults is statically false — skip the defaults branch entirely
                          plainFF(cursor, fieldNameExpr)
                        case Some(defaultAnyExpr) =>
                          // Missing field, or a null the decoder rejects, decodes to the default (circe parity)
                          Expr.quote {
                            if (Expr.splice(dctx.config).useDefaults)
                              CirceDerivationUtils.decodeFieldUsingDefault(
                                Expr.splice(cursor),
                                Expr.splice(fieldNameExpr),
                                Expr.splice(decoderExpr),
                                Expr.splice(defaultAnyExpr)
                              )
                            else Expr.splice(plainFF(cursor, fieldNameExpr))
                          }
                        case None => plainFF(cursor, fieldNameExpr)
                      }

                  // --- Accumulating decode expression ---
                  val accOf: Expr[HCursor] => Expr[ValidatedNel[DecodingFailure, Any]] = cursor =>
                    if (isTransient) {
                      defaultAsAnyOpt match {
                        case Some(d) =>
                          Expr.quote(Validated.valid(Expr.splice(d)): ValidatedNel[DecodingFailure, Any])
                        case None =>
                          Expr.quote(Validated.valid(null): ValidatedNel[DecodingFailure, Any])
                      }
                    } else
                      defaultAsAnyOpt match {
                        case Some(_) if useDefaultsStaticallyFalse =>
                          plainAcc(cursor, fieldNameExpr)
                        case Some(defaultAnyExpr) =>
                          Expr.quote {
                            if (Expr.splice(dctx.config).useDefaults)
                              CirceDerivationUtils.decodeFieldUsingDefaultAccumulating(
                                Expr.splice(cursor),
                                Expr.splice(fieldNameExpr),
                                Expr.splice(decoderExpr),
                                Expr.splice(defaultAnyExpr)
                              )
                            else Expr.splice(plainAcc(cursor, fieldNameExpr))
                          }
                        case None => plainAcc(cursor, fieldNameExpr)
                      }

                  // The constructor argument: the field's (successfully decoded) value cast to the field's type.
                  val makeFieldArg: Expr[Any] => (String, Expr_??) = { valExpr =>
                    (fName, castTo[Field](valExpr).as_??)
                  }
                  (ffOf, accOf, makeFieldArg)
                }
              }
            }
            .flatMap { fieldData =>
              val ffOfs = fieldData.toList.map(_._1)
              val accOfs = fieldData.toList.map(_._2)
              val makeFieldArgs = fieldData.toList.map(_._3)

              def construct(values: List[Expr[Any]]): Expr[A] = {
                val fieldMap: Map[String, Expr_??] =
                  makeFieldArgs.zip(values).map { case (mk, v) => mk(v) }.toMap
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

              // The fields strict decoding expects: precomputed when the config is known (or for @fieldName).
              val resolvedFieldNames: Option[Expr[List[String]]] =
                dctx.evaluatedConfig match {
                  case Some(evCfg) =>
                    val names = fieldsList
                      .filterNot { case (_, p) => hasAnnotationType[transientField](p) }
                      .map { case (name, p) =>
                        getAnnotationStringArg[fieldName](p).getOrElse(evCfg.transformMemberNames(name))
                      }
                    Some(names.foldRight(Expr.quote(List.empty[String])) { (name, acc) =>
                      Expr.quote(Expr.splice(Expr(name)) :: Expr.splice(acc))
                    })
                  case None =>
                    if (hasAnyFieldNameAnnotation) {
                      val resolvedList = fieldsList
                        .filterNot { case (_, p) => hasAnnotationType[transientField](p) }
                        .map { case (name, p) =>
                          getAnnotationStringArg[fieldName](p) match {
                            case Some(custom) => (custom, true)
                            case None         => (name, false)
                          }
                        }
                        .foldRight(Expr.quote(List.empty[String])) {
                          case ((name, true), acc) =>
                            Expr.quote(Expr.splice(Expr(name)) :: Expr.splice(acc))
                          case ((name, false), acc) =>
                            Expr.quote {
                              Expr.splice(dctx.config).transformMemberNames(Expr.splice(Expr(name))) ::
                                Expr.splice(acc)
                            }
                        }
                      Some(resolvedList)
                    } else None
                }
              def expectedFields: Expr[List[String]] = {
                val names = resolvedFieldNames.getOrElse(
                  Expr.quote(Expr.splice(fieldNamesListExpr).map(Expr.splice(dctx.config).transformMemberNames))
                )
                Expr.quote(Expr.splice(names) ++ Expr.splice(dctx.config).discriminator)
              }
              val strictDecodingStatic: Option[Boolean] = dctx.evaluatedConfig.map(_.strictDecoding)

              // Fail-fast (`apply`): the first failing field in field order, later fields are not decoded. A
              // non-object fails with circe's "expecting object" (bug #235); with strict decoding the unexpected
              // fields are reported before a field failure (as circe).
              def failFastBody(cursor: Expr[HCursor]): Expr[Either[DecodingFailure, A]] = {
                val decoded: Expr[Either[DecodingFailure, A]] =
                  constructFailFast[Either[DecodingFailure, Any], A, Either[DecodingFailure, A]](ffOfs.map(_(cursor)))(
                    isSuccess = r => Expr.quote(Expr.splice(r).isRight),
                    valueOf = r => Expr.quote(Expr.splice(r).asInstanceOf[Right[DecodingFailure, Any]].value),
                    construct = construct,
                    onSuccess = a => Expr.quote(Right(Expr.splice(a)): Either[DecodingFailure, A]),
                    onFailure = r => Expr.quote(Expr.splice(r).asInstanceOf[Either[DecodingFailure, A]])
                  )
                def strict(decodedVal: Expr[Either[DecodingFailure, A]]): Expr[Either[DecodingFailure, A]] =
                  Expr.quote {
                    CirceDerivationUtils
                      .checkStrictDecoding(Expr.splice(cursor), Expr.splice(typeNameExpr), Expr.splice(expectedFields))
                      .flatMap(_ => Expr.splice(decodedVal))
                  }
                val checked: Expr[Either[DecodingFailure, A]] = strictDecodingStatic match {
                  case Some(false) => decoded
                  case Some(true)  =>
                    ValDefs.createVal[Either[DecodingFailure, A]](decoded, FreshName.FromPrefix("decoded")).use(strict)
                  case None =>
                    ValDefs.createVal[Either[DecodingFailure, A]](decoded, FreshName.FromPrefix("decoded")).use {
                      decodedVal =>
                        Expr.quote {
                          if (Expr.splice(dctx.config).strictDecoding) Expr.splice(strict(decodedVal))
                          else Expr.splice(decodedVal)
                        }
                    }
                }
                Expr.quote {
                  if (Expr.splice(cursor).value.isObject) Expr.splice(checked)
                  else Left(CirceDerivationUtils.notAnObjectFailure(Expr.splice(cursor)))
                }
              }

              // Accumulating (`decodeAccumulating`): every field is decoded and all failures are reported in field
              // order, after the strict-decoding failure (as circe).
              def accumulatingBody(cursor: Expr[HCursor]): Expr[ValidatedNel[DecodingFailure, A]] = {
                val decoded: Expr[ValidatedNel[DecodingFailure, A]] =
                  constructAccumulating[
                    ValidatedNel[DecodingFailure, Any],
                    cats.data.NonEmptyList[DecodingFailure],
                    A,
                    ValidatedNel[DecodingFailure, A]
                  ](accOfs.map(_(cursor)))(
                    mergeErrors = (failures, r) =>
                      Expr.quote(CirceDerivationUtils.mergeAccumulatedFailures(Expr.splice(failures), Expr.splice(r))),
                    valueOf = r => Expr.quote(Expr.splice(r).asInstanceOf[Validated.Valid[Any]].a),
                    construct = construct,
                    onSuccess = a => Expr.quote(Validated.Valid(Expr.splice(a)): ValidatedNel[DecodingFailure, A]),
                    onFailure =
                      failures => Expr.quote(Validated.Invalid(Expr.splice(failures)): ValidatedNel[DecodingFailure, A])
                  )
                def strict(decodedVal: Expr[ValidatedNel[DecodingFailure, A]]): Expr[ValidatedNel[DecodingFailure, A]] =
                  Expr.quote {
                    CirceDerivationUtils.withStrictFailureAccumulating(
                      CirceDerivationUtils.checkStrictDecodingAccumulating(
                        Expr.splice(cursor),
                        Expr.splice(typeNameExpr),
                        Expr.splice(expectedFields)
                      ),
                      Expr.splice(decodedVal)
                    )
                  }
                val checked: Expr[ValidatedNel[DecodingFailure, A]] = strictDecodingStatic match {
                  case Some(false) => decoded
                  case Some(true)  =>
                    ValDefs
                      .createVal[ValidatedNel[DecodingFailure, A]](decoded, FreshName.FromPrefix("decoded"))
                      .use(strict)
                  case None =>
                    ValDefs
                      .createVal[ValidatedNel[DecodingFailure, A]](decoded, FreshName.FromPrefix("decoded"))
                      .use { decodedVal =>
                        Expr.quote {
                          if (Expr.splice(dctx.config).strictDecoding) Expr.splice(strict(decodedVal))
                          else Expr.splice(decodedVal)
                        }
                      }
                }
                Expr.quote {
                  if (Expr.splice(cursor).value.isObject) Expr.splice(checked)
                  else Validated.invalidNel(CirceDerivationUtils.notAnObjectFailure(Expr.splice(cursor)))
                }
              }

              MIO.pure(Right((failFastBody(_), accumulatingBody(_))))
            }
      }
    }

    /** `value.asInstanceOf[F]` (a method of its own so that `F` is a type parameter in the quote). */
    private def castTo[F: Type](value: Expr[Any]): Expr[F] = Expr.quote(Expr.splice(value).asInstanceOf[F])
  }
}
