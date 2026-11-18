package hearth.kindlings.pureconfigderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.data.NonEmptyList
import hearth.fp.effect.*
import hearth.fp.syntax.*
import hearth.std.*

import hearth.kindlings.pureconfigderivation.KindlingsProductHint
import hearth.kindlings.pureconfigderivation.annotations.{configKey, transientField}
import hearth.kindlings.pureconfigderivation.internal.runtime.PureConfigDerivationUtils
import pureconfig.{ConfigCursor, ConfigObjectCursor, ConfigReader}
import pureconfig.error.ConfigReaderFailures

trait ReaderHandleAsCaseClassRuleImpl {
  this: ReaderMacrosImpl & MacroCommons & StdExtensions & AnnotationSupport =>

  object ReaderHandleAsCaseClassRule extends ReaderDerivationRule("handle as case class when possible") {

    def apply[A: ReaderCtx]: MIO[Rule.Applicability[Expr[Either[ConfigReaderFailures, A]]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as a case class") >> {
        CaseClass.parse[A].toEither match {
          case Right(caseClass) =>
            decodeCaseClassFields[A](caseClass).map(Rule.matched)

          case Left(reason) =>
            MIO.pure(Rule.yielded(reason))
        }
      }

    @scala.annotation.nowarn("msg=is never used|unused explicit parameter")
    private def decodeCaseClassFields[A: ReaderCtx](
        caseClass: CaseClass[A]
    ): MIO[Expr[Either[ConfigReaderFailures, A]]] = {
      implicit val StringT: Type[String] = RTypes.String
      implicit val SetStringT: Type[Set[String]] = RTypes.SetString
      implicit val ConfigCursorT: Type[ConfigCursor] = RTypes.ConfigCursor
      implicit val ConfigObjectCursorT: Type[ConfigObjectCursor] = RTypes.ConfigObjectCursor
      implicit val FailuresT: Type[ConfigReaderFailures] = RTypes.ConfigReaderFailures
      implicit val BooleanT: Type[Boolean] = RTypes.Boolean
      implicit val FuncT: Type[String => String] = RTypes.StringToString
      implicit val configKeyT: Type[configKey] = RTypes.ConfigKey
      implicit val transientFieldT: Type[transientField] = RTypes.TransientField
      implicit val ProductHintT: Type[KindlingsProductHint[A]] = RTypes.kindlingsProductHintType[A]

      // Per-type override: if the user has provided an implicit `KindlingsProductHint[A]`
      // for this specific case class, use it for `transformMemberNames` / `useDefaults`
      // (and `allowUnknownKeys` once strict mode is wired). Otherwise fall back to the
      // global `PureConfig` config in scope at the entry point.
      val maybeHint: Option[Expr[KindlingsProductHint[A]]] = Expr.summonImplicit[KindlingsProductHint[A]].toOption

      // When no per-type hint is present and the global config was evaluable at compile time,
      // we can pre-compute transformMemberNames(fieldName) as constant strings — eliminating
      // the expensive runtime regex in CapitalizedWordsNamingConvention.toTokens on every decode.
      val preComputedTransform: Option[String => String] = maybeHint match {
        case Some(_) => None // per-type hint: can't evaluate at compile time
        case None    => rctx.evaluatedConfig.map(_.transformMemberNames)
      }

      val transformExpr: Expr[String => String] = maybeHint match {
        case Some(h) => Expr.quote(Expr.splice(h).transformMemberNames)
        case None    => Expr.quote(Expr.splice(rctx.config).transformMemberNames)
      }

      // Static knowledge of useDefaults: when evaluable + no hint, we know at compile time
      // whether default-value branches are needed.
      val staticUseDefaults: Option[Boolean] = maybeHint match {
        case Some(_) => None
        case None    => rctx.evaluatedConfig.map(_.useDefaults)
      }
      val useDefaultsExpr: Expr[Boolean] = maybeHint match {
        case Some(h) => Expr.quote(Expr.splice(h).useDefaults)
        case None    => Expr.quote(Expr.splice(rctx.config).useDefaults)
      }

      val staticAllowUnknownKeys: Option[Boolean] = maybeHint match {
        case Some(_) => None
        case None    => rctx.evaluatedConfig.map(_.allowUnknownKeys)
      }
      val allowUnknownKeysExpr: Expr[Boolean] = maybeHint match {
        case Some(h) => Expr.quote(Expr.splice(h).allowUnknownKeys)
        case None    => Expr.quote(Expr.splice(rctx.config).allowUnknownKeys)
      }

      val constructor = caseClass.primaryConstructor
      val fieldsList = constructor.totalParameters.flatten.toList

      fieldsList.collectFirst {
        case (name, param) if hasAnnotationType[transientField](param) && !param.hasDefault => name
      } match {
        case Some(name) =>
          val err = ReaderDerivationError.TransientFieldMissingDefault(name, Type[A].prettyPrint)
          return Log.error(err.message) >> MIO.fail(err)
        case None => // OK
      }

      val transientFields = fieldsList.filter { case (_, param) => hasAnnotationType[transientField](param) }
      val nonTransientFields = fieldsList.filter { case (_, param) => !hasAnnotationType[transientField](param) }

      val transientDefaults: Map[String, Expr_??] = transientFields.flatMap { case (fName, param) =>
        param.defaultValue.flatMap { method =>
          foldInstanceFree(method, "Default value")(
            onTypes = _ => Map.empty,
            onValues = _ => Map.empty
          ).toOption
            .map(ee => (fName, ee))
        }
      }.toMap

      NonEmptyList.fromList(nonTransientFields) match {
        case None =>
          // Zero non-transient fields: just construct from transient defaults (if any).
          caseClass
            .construct[MIO](new CaseClass.ConstructField[MIO] {
              def apply(field: Parameter): MIO[Expr[field.tpe.Underlying]] =
                transientDefaults.get(field.name) match {
                  case Some(defaultExpr) =>
                    MIO.pure(defaultExpr.value.asInstanceOf[Expr[field.tpe.Underlying]])
                  case None =>
                    val err = ReaderDerivationError.CannotConstructType(
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
                  Expr.splice(rctx.cursor).asObjectCursor.map(_ => Expr.splice(expr))
                })
              case None =>
                val err = ReaderDerivationError.CannotConstructType(Type[A].prettyPrint, isSingleton = false)
                Log.error(err.message) >> MIO.fail(err)
            }

        case Some(fields) =>
          implicit val AnyT: Type[Any] = RTypes.Any
          implicit val EitherFailuresAnyT: Type[Either[ConfigReaderFailures, Any]] = RTypes.EitherFailuresAny
          implicit val ArrayAnyT: Type[Array[Any]] = RTypes.ArrayAny
          implicit val ListEitherT: Type[List[Either[ConfigReaderFailures, Any]]] = RTypes.ListEitherFailuresAny
          implicit val OptionFailuresT: Type[Option[ConfigReaderFailures]] = RTypes.OptionFailures
          implicit val EitherFailuresA: Type[Either[ConfigReaderFailures, A]] = RTypes.ReaderResult[A]

          val indexedFields = fields.toList.zipWithIndex
          NonEmptyList
            .fromList(indexedFields)
            .get
            .parTraverse { case ((fName, param), reindex) =>
              import param.tpe.Underlying as Field
              val nameOverride = getAnnotationStringArg[configKey](param)
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
              Log.namedScope(s"Deriving reader for field $fName: ${Type[Field].prettyPrint}") {
                deriveFieldReader[Field].map { readerExpr =>
                  // Resolve the HOCON key for this field. `@configKey("custom")` always wins;
                  // otherwise apply the resolved transform (from per-type ProductHint or the
                  // global PureConfig). When the config was evaluable at compile time, we
                  // pre-compute the mapped name as a constant string — eliminating the
                  // expensive runtime regex in CapitalizedWordsNamingConvention.toTokens.
                  val keyExpr: Expr[String] = nameOverride match {
                    case Some(customName) => Expr(customName)
                    case None             =>
                      preComputedTransform match {
                        case Some(transform) => Expr(transform(fName))
                        case None            => Expr.quote(Expr.splice(transformExpr)(Expr.splice(Expr(fName))))
                      }
                  }
                  // Option fields must use atKeyOrUndefined so missing keys propagate as an
                  // `isUndefined` cursor that the option rule turns into `None`. For non-option
                  // fields, missing keys produce a clean KeyNotFound via atKey.
                  val isOptionField: Boolean = Type[Field] match {
                    case IsOption(_) => true
                    case _           => false
                  }
                  // We splice `readerExpr` directly and call `.from(...)` on it inside the
                  // quote rather than passing it through a runtime helper with an explicit
                  // type parameter. This keeps the path-dependent `Field` type out of the
                  // quote (Scala 2's macro backend chokes on it via `asInstanceOf[Reader[Any]]`)
                  // and only casts the *result* of the reader call to `Either[…, Any]`.
                  // When we statically know useDefaults is false, defaults are never used even
                  // if the field has one — skip the useDefaults branch entirely. When statically
                  // true, unconditionally use the default on missing key (no runtime boolean check).
                  val effectiveDefault: Option[Expr[Any]] = defaultAsAnyOpt.flatMap { d =>
                    staticUseDefaults match {
                      case Some(false) => None // statically disabled: drop the default branch
                      case _           => Some(d) // statically true or unknown: keep it
                    }
                  }
                  val useDefaultsIsStaticallyTrue = staticUseDefaults.contains(true)

                  // Reads the field from the object cursor, which the reader obtains once for all the fields (a
                  // per-field `cursor.asObjectCursor.flatMap(...)` allocated a cursor, a `Right` and a lambda for
                  // every field of every read).
                  val decodeFrom: Expr[ConfigObjectCursor] => Expr[Either[ConfigReaderFailures, Any]] =
                    effectiveDefault match {
                      case Some(defaultAnyExpr) if useDefaultsIsStaticallyTrue =>
                        // useDefaults is statically true: no runtime boolean check needed
                        if (isOptionField) { objExpr =>
                          Expr.quote {
                            val keyCur = Expr.splice(objExpr).atKeyOrUndefined(Expr.splice(keyExpr))
                            (if (keyCur.isUndefined)
                               Right(Expr.splice(defaultAnyExpr)): Either[ConfigReaderFailures, Any]
                             else
                               Expr.splice(readerExpr).from(keyCur)).asInstanceOf[Either[ConfigReaderFailures, Any]]
                          }
                        } else { objExpr =>
                          Expr.quote {
                            val obj = Expr.splice(objExpr)
                            val key = Expr.splice(keyExpr)
                            val keyCur = obj.atKeyOrUndefined(key)
                            (if (keyCur.isUndefined)
                               Right(Expr.splice(defaultAnyExpr)): Either[ConfigReaderFailures, Any]
                             else
                               obj.atKey(key).flatMap(c => Expr.splice(readerExpr).from(c)))
                              .asInstanceOf[Either[ConfigReaderFailures, Any]]
                          }
                        }
                      case Some(defaultAnyExpr) =>
                        // useDefaults is unknown at compile time: keep runtime check
                        if (isOptionField) { objExpr =>
                          Expr.quote {
                            val keyCur = Expr.splice(objExpr).atKeyOrUndefined(Expr.splice(keyExpr))
                            (if (Expr.splice(useDefaultsExpr) && keyCur.isUndefined)
                               Right(Expr.splice(defaultAnyExpr)): Either[ConfigReaderFailures, Any]
                             else
                               Expr.splice(readerExpr).from(keyCur)).asInstanceOf[Either[ConfigReaderFailures, Any]]
                          }
                        } else { objExpr =>
                          Expr.quote {
                            val obj = Expr.splice(objExpr)
                            val key = Expr.splice(keyExpr)
                            val keyCur = obj.atKeyOrUndefined(key)
                            (if (Expr.splice(useDefaultsExpr) && keyCur.isUndefined)
                               Right(Expr.splice(defaultAnyExpr)): Either[ConfigReaderFailures, Any]
                             else
                               obj.atKey(key).flatMap(c => Expr.splice(readerExpr).from(c)))
                              .asInstanceOf[Either[ConfigReaderFailures, Any]]
                          }
                        }
                      case None =>
                        if (isOptionField) { objExpr =>
                          Expr.quote {
                            Expr
                              .splice(readerExpr)
                              .from(Expr.splice(objExpr).atKeyOrUndefined(Expr.splice(keyExpr)))
                              .asInstanceOf[Either[ConfigReaderFailures, Any]]
                          }
                        } else { objExpr =>
                          Expr.quote {
                            PureConfigDerivationUtils
                              .readField(Expr.splice(objExpr), Expr.splice(keyExpr), Expr.splice(readerExpr))
                              .asInstanceOf[Either[ConfigReaderFailures, Any]]
                          }
                        }
                    }
                  // Reads the already-bound `Right` value directly (zero-allocation fail-fast construction); no
                  // intermediate `Array[Any]`.
                  val makeFieldArg: Expr[Any] => (String, Expr_??) = { valExpr =>
                    val typedExpr = Expr.quote {
                      PureConfigDerivationUtils.unsafeCast(Expr.splice(valExpr), Expr.splice(readerExpr))
                    }
                    (fName, typedExpr.as_??)
                  }
                  (decodeFrom, makeFieldArg, keyExpr)
                }
              }
            }
            .flatMap { fieldData =>
              val decodeFroms = fieldData.toList.map(_._1)
              val makeFieldArgs = fieldData.toList.map(_._2)
              val keyExprs = fieldData.toList.map(_._3)

              // The keys of every field: what upstream's `ProductHint.bottom` treats as used when it reports unknown
              // keys (`allowUnknownKeys = false`). Not needed when unknown keys are statically allowed.
              val expectedKeysExpr: Expr[Set[String]] =
                if (staticAllowUnknownKeys.contains(true)) Expr.quote(Set.empty[String])
                else
                  keyExprs.foldRight(Expr.quote(Set.empty[String])) { (k, acc) =>
                    Expr.quote(Expr.splice(acc) + Expr.splice(k))
                  }

              def constructFF(boundValues: List[Expr[Any]]): Expr[A] = {
                val nonTransientFieldMap: Map[String, Expr_??] =
                  makeFieldArgs.zip(boundValues).map { case (mk, v) => mk(v) }.toMap
                val fieldMap: Map[String, Expr_??] = nonTransientFieldMap ++ transientDefaults
                foldInstanceFree(caseClass.primaryConstructor, "Constructor")(
                  onTypes = _ => Map.empty,
                  onValues = _ => fieldMap
                ) match {
                  case Right(constructExpr) => constructExpr.value.asInstanceOf[Expr[A]]
                  case Left(error)          =>
                    Environment.reportErrorAndAbort(
                      ReaderDerivationError
                        .CannotConstructType(Type[A].prettyPrint, isSingleton = false, Some(error))
                        .message
                    )
                }
              }

              // Like upstream pureconfig (`ConfigReader.Result.zipWith` over the fields, then `ProductHint.bottom`),
              // every field is read and all failures are reported: each field's result is bound to a `val`, the
              // value is constructed from them fail-fast (no intermediate collections on the success path), and only
              // when something failed are the failures of every field and the unknown keys combined.
              def bindAll(
                  remaining: List[Expr[Either[ConfigReaderFailures, Any]]],
                  boundReversed: List[Expr[Either[ConfigReaderFailures, Any]]]
              ): Expr[Either[ConfigReaderFailures, A]] = remaining match {
                case result :: rest =>
                  ValDefs.createVal[Either[ConfigReaderFailures, Any]](result).use { bound =>
                    bindAll(rest, bound :: boundReversed)
                  }
                case Nil =>
                  val bound = boundReversed.reverse
                  val constructedExpr = buildEitherFailFast[ConfigReaderFailures, A](bound)(constructFF)
                  val resultsExpr: Expr[List[Either[ConfigReaderFailures, Any]]] =
                    bound.foldRight(Expr.quote(List.empty[Either[ConfigReaderFailures, Any]])) { (r, acc) =>
                      Expr.quote(Expr.splice(r) :: Expr.splice(acc))
                    }
                  if (staticAllowUnknownKeys.contains(true))
                    Expr.quote {
                      val constructed = Expr.splice(constructedExpr)
                      if (constructed.isRight) constructed
                      else Left(PureConfigDerivationUtils.combineFieldFailures(Expr.splice(resultsExpr), None))
                    }
                  else
                    Expr.quote {
                      val constructed = Expr.splice(constructedExpr)
                      val unknownKeys: Option[ConfigReaderFailures] =
                        if (Expr.splice(allowUnknownKeysExpr)) None
                        else
                          PureConfigDerivationUtils
                            .unknownKeysFailures(Expr.splice(rctx.cursor), Expr.splice(expectedKeysExpr))
                      if (constructed.isRight && unknownKeys.isEmpty) constructed
                      else Left(PureConfigDerivationUtils.combineFieldFailures(Expr.splice(resultsExpr), unknownKeys))
                    }
              }

              MIO.pure(Expr.quote {
                // A value that is not an object fails once (as upstream), not once per field; every field is read from
                // the same object cursor.
                Expr.splice(rctx.cursor).asObjectCursor match {
                  case Left(failures) => Left(failures): Either[ConfigReaderFailures, A]
                  // (a distinct name: the field reads bind their own `obj`, and Scala 2 quotes are not hygienic)
                  case Right(objectCursor) =>
                    Expr.splice(bindAll(decodeFroms.map(_(Expr.quote(objectCursor))), Nil))
                }
              })
            }
      }
    }

    /** Derive a `ConfigReader[Field]` for a case-class field, either by summoning an implicit (filtered against our
      * auto-derivation entry points) or by recursively driving the rule chain on the field type.
      */
    @scala.annotation.nowarn("msg=is never used|unused explicit parameter")
    private def deriveFieldReader[Field: Type](implicit ctx: ReaderCtx[?]): MIO[Expr[ConfigReader[Field]]] = {
      implicit val ConfigCursorT: Type[ConfigCursor] = RTypes.ConfigCursor
      implicit val ReaderResultField: Type[Either[ConfigReaderFailures, Field]] = RTypes.ReaderResult[Field]
      implicit val ReaderField: Type[ConfigReader[Field]] = RTypes.ConfigReader[Field]

      RTypes
        .ConfigReader[Field]
        .summonExprIgnoring(ReaderUseImplicitWhenAvailableRule.ignoredImplicits*)
        .toEither match {
        case Right(readerExpr) =>
          Log.info(s"Found implicit ConfigReader[${Type[Field].prettyPrint}]") >> MIO.pure(readerExpr)
        case Left(_) =>
          Log.info(s"Building ConfigReader[${Type[Field].prettyPrint}] via recursive derivation") >>
            LambdaBuilder
              .of1[ConfigCursor]("fieldCursor")
              .traverse { fieldCursorExpr =>
                deriveReaderRecursively[Field](using ctx.nest[Field](fieldCursorExpr))
              }
              .map { builder =>
                val readFn = builder.build[Either[ConfigReaderFailures, Field]]
                Expr.quote(PureConfigDerivationUtils.readerFromFn(Expr.splice(readFn)))
              }
      }
    }
  }
}
