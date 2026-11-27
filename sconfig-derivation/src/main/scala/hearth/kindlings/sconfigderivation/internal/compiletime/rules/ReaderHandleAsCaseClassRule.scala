package hearth.kindlings.sconfigderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.data.NonEmptyList
import hearth.fp.effect.*
import hearth.fp.syntax.*
import hearth.std.*

import hearth.kindlings.sconfigderivation.{ConfigDecodingError, ProductHint}
import hearth.kindlings.sconfigderivation.annotations.{configKey, transientField}
import hearth.kindlings.sconfigderivation.internal.runtime.SConfigDerivationUtils
import hearth.kindlings.sconfigderivation.ConfigReader
import org.ekrich.config.{ConfigObject, ConfigValue}

trait ReaderHandleAsCaseClassRuleImpl {
  this: ReaderMacrosImpl & MacroCommons & StdExtensions & AnnotationSupport =>

  object ReaderHandleAsCaseClassRule extends ReaderDerivationRule("handle as case class when possible") {

    def apply[A: ReaderCtx]: MIO[Rule.Applicability[Expr[Either[ConfigDecodingError, A]]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as a case class") >> {
        CaseClass.parse[A].toEither match {
          case Right(caseClass) =>
            decodeCaseClassFields[A](caseClass).map(Rule.matched)
          case Left(reason) =>
            MIO.pure(Rule.yielded(reason))
        }
      }

    @scala.annotation.nowarn("msg=is never used|unused explicit parameter")
    private[rules] def decodeCaseClassFields[A: ReaderCtx](
        caseClass: CaseClass[A]
    ): MIO[Expr[Either[ConfigDecodingError, A]]] = {
      implicit val StringT: Type[String] = RTypes.String
      implicit val ConfigValueT: Type[ConfigValue] = RTypes.ConfigValue
      implicit val ErrorT: Type[ConfigDecodingError] = RTypes.ConfigDecodingError
      implicit val BooleanT: Type[Boolean] = RTypes.Boolean
      implicit val SetStringT: Type[Set[String]] = RTypes.SetString
      implicit val FuncT: Type[String => String] = RTypes.StringToString
      implicit val configKeyT: Type[configKey] = RTypes.ConfigKey
      implicit val transientFieldT: Type[transientField] = RTypes.TransientField
      implicit val ProductHintT: Type[ProductHint[A]] = RTypes.productHintType[A]
      implicit val ConfigObjectT: Type[ConfigObject] = RTypes.ConfigObject
      implicit val EitherErrorA: Type[Either[ConfigDecodingError, A]] = RTypes.ReaderResult[A]

      // Per-type override: see KindlingsConfigReader for the same pattern in pureconfig.
      val maybeHint: Option[Expr[ProductHint[A]]] = Expr.summonImplicit[ProductHint[A]].toOption

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
                      Some("Unexpected parameter in zero-argument case class")
                    )
                    Log.error(err.message) >> MIO.fail(err)
                }
            })
            .flatMap {
              case Some(expr) =>
                MIO.pure(Expr.quote {
                  SConfigDerivationUtils.asObject(Expr.splice(rctx.value)).flatMap { obj =>
                    val result: Either[ConfigDecodingError, A] = Right(Expr.splice(expr))
                    if (Expr.splice(allowUnknownKeysExpr)) result
                    else SConfigDerivationUtils.withUnknownKeys[A](obj, result, Set.empty[String])
                  }
                })
              case None =>
                val err = ReaderDerivationError.CannotConstructType(Type[A].prettyPrint, isSingleton = false)
                Log.error(err.message) >> MIO.fail(err)
            }

        case Some(fields) =>
          implicit val AnyT: Type[Any] = RTypes.Any
          implicit val EitherErrorAnyT: Type[Either[ConfigDecodingError, Any]] = RTypes.EitherErrorAny
          implicit val ArrayAnyT: Type[Array[Any]] = RTypes.ArrayAny
          implicit val ListEitherT: Type[List[Either[ConfigDecodingError, Any]]] = RTypes.ListEitherErrorAny

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
                  // global SConfig). When the config was evaluable at compile time, we
                  // pre-compute the mapped name as a constant string — eliminating the
                  // expensive runtime regex in CapitalizedWordsNamingConvention.toTokens.
                  val constantKey: Option[String] = nameOverride.orElse(preComputedTransform.map(_(fName)))
                  val keyExpr: Expr[String] = constantKey match {
                    case Some(key) => Expr(key)
                    case None      => Expr.quote(Expr.splice(transformExpr)(Expr.splice(Expr(fName))))
                  }
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

                  // Every field is read from the object (checked once, see below) like upstream PureConfig: a
                  // missing key uses the default (when enabled), goes to readers of missing keys (`Option` fields,
                  // `ReadsMissingKeys`), or fails with `KeyNotFound`; the value's failures are reported under the key.
                  // (whether a reader reads missing keys is its own property, `ReadsMissingKeys`: the built-in
                  // `Option` reader and the ones derived here for `Option`s do, a user-provided plain reader of an
                  // `Option` type does not)
                  val readsMissingKeysExpr: Expr[Boolean] = Expr.quote(false)
                  val decodeExpr: Expr[ConfigObject] => Expr[Either[ConfigDecodingError, Any]] = objExpr =>
                    effectiveDefault match {
                      case Some(defaultAnyExpr) if useDefaultsIsStaticallyTrue =>
                        // useDefaults is statically true: no runtime boolean check needed. One key lookup: the value
                        // found is read directly.
                        Expr.quote {
                          val key = Expr.splice(keyExpr)
                          val fieldValue = Expr.splice(objExpr).get(key)
                          if (fieldValue == null)
                            Right(Expr.splice(defaultAnyExpr)): Either[ConfigDecodingError, Any]
                          else
                            SConfigDerivationUtils
                              .readFieldValue(fieldValue, key, Expr.splice(readerExpr))
                              .asInstanceOf[Either[ConfigDecodingError, Any]]
                        }
                      case Some(defaultAnyExpr) =>
                        // useDefaults is unknown at compile time: keep runtime check
                        Expr.quote {
                          val o = Expr.splice(objExpr)
                          val key = Expr.splice(keyExpr)
                          val fieldValue = o.get(key)
                          if (fieldValue != null)
                            SConfigDerivationUtils
                              .readFieldValue(fieldValue, key, Expr.splice(readerExpr))
                              .asInstanceOf[Either[ConfigDecodingError, Any]]
                          else if (Expr.splice(useDefaultsExpr))
                            Right(Expr.splice(defaultAnyExpr)): Either[ConfigDecodingError, Any]
                          else
                            SConfigDerivationUtils
                              .readField(o, key, Expr.splice(readerExpr), Expr.splice(readsMissingKeysExpr))
                              .asInstanceOf[Either[ConfigDecodingError, Any]]
                        }
                      case None =>
                        Expr.quote {
                          SConfigDerivationUtils
                            .readField(
                              Expr.splice(objExpr),
                              Expr.splice(keyExpr),
                              Expr.splice(readerExpr),
                              Expr.splice(readsMissingKeysExpr)
                            )
                            .asInstanceOf[Either[ConfigDecodingError, Any]]
                        }
                    }
                  // The constructor argument: the field's value (read from its `Right`) cast to the field's type.
                  val makeAccessor: Expr[Any] => (String, Expr_??) = { valExpr =>
                    (fName, castTo[Field](valExpr).as_??)
                  }
                  (decodeExpr, makeAccessor, keyExpr, constantKey)
                }
              }
            }
            .flatMap { fieldData =>
              val decodeExprs = fieldData.toList.map(_._1)
              val makeAccessors = fieldData.toList.map(_._2)
              val keyExprs = fieldData.toList.map(_._3)
              val constantKeys = fieldData.toList.map(_._4)

              // The keys of every field: what upstream's `ProductHint.bottom` treats as used when it reports unknown
              // keys (`allowUnknownKeys = false`). Not needed when unknown keys are statically allowed. When every key
              // is known at compile time the set is a lazy val of the instance, built once instead of on every read.
              def keySet(keys: List[Expr[String]]): Expr[Set[String]] =
                keys.foldRight(Expr.quote(Set.empty[String])) { (k, acc) =>
                  Expr.quote(Expr.splice(acc) + Expr.splice(k))
                }
              val expectedKeysMIO: MIO[Expr[Set[String]]] =
                if (staticAllowUnknownKeys.contains(true)) MIO.pure(Expr.quote(Set.empty[String]))
                else if (constantKeys.forall(_.isDefined)) {
                  val cacheKey = s"cached-expected-keys:${Type[A].plainPrint}"
                  rctx.cache.buildCachedWith(
                    cacheKey,
                    ValDefBuilder.ofLazy[Set[String]](s"expectedKeys_${Type[A].shortName}")
                  )(_ => keySet(keyExprs)) >>
                    rctx.cache.get0Ary[Set[String]](cacheKey).map(_.get)
                } else MIO.pure(keySet(keyExprs))

              def constructFrom(boundValues: List[Expr[Any]]): MIO[Expr[A]] = {
                val nonTransientFieldMap: Map[String, Expr_??] =
                  makeAccessors.zip(boundValues).map { case (mk, v) => mk(v) }.toMap
                val fieldMap: Map[String, Expr_??] = nonTransientFieldMap ++ transientDefaults
                foldInstanceFree(caseClass.primaryConstructor, "Constructor")(
                  onTypes = _ => Map.empty,
                  onValues = _ => fieldMap
                ) match {
                  case Right(constructExpr) => MIO.pure(constructExpr.value.asInstanceOf[Expr[A]])
                  case Left(error)          =>
                    val err = ReaderDerivationError.CannotConstructType(
                      Type[A].prettyPrint,
                      isSingleton = false,
                      Some(error)
                    )
                    Log.error(err.message) >> MIO.fail(err)
                }
              }

              // Like upstream PureConfig: a value that is not an object fails once (not once per field); then every
              // field is read and all failures are reported, followed by the unknown keys of a strict case class
              // (`ProductHint.bottom`). Each field is read by its own local def and its failure merged as it is read;
              // the value is constructed when there is none (see FlatFieldsConstruct).
              expectedKeysMIO.flatMap { expectedKeysExpr =>
                MIO.scoped { runSafe =>
                  def readAll(objExpr: Expr[ConfigObject]): Expr[Either[ConfigDecodingError, A]] =
                    constructAccumulating[Either[ConfigDecodingError, Any], ConfigDecodingError, A, Either[
                      ConfigDecodingError,
                      A
                    ]](decodeExprs.map(_(objExpr)))(
                      mergeErrors = (failure, r) =>
                        Expr.quote(SConfigDerivationUtils.mergeFailure(Expr.splice(failure), Expr.splice(r))),
                      valueOf = r => Expr.quote(Expr.splice(r).asInstanceOf[Right[ConfigDecodingError, Any]].value),
                      construct = values => runSafe(constructFrom(values)),
                      onSuccess = a => Expr.quote(Right(Expr.splice(a)): Either[ConfigDecodingError, A]),
                      onFailure = failure => Expr.quote(Left(Expr.splice(failure)): Either[ConfigDecodingError, A])
                    )
                  if (staticAllowUnknownKeys.contains(true))
                    Expr.quote {
                      SConfigDerivationUtils.asObject(Expr.splice(rctx.value)) match {
                        case Left(e)             => Left(e): Either[ConfigDecodingError, A]
                        case Right(objectFields) => Expr.splice(readAll(Expr.quote(objectFields)))
                      }
                    }
                  else
                    Expr.quote {
                      SConfigDerivationUtils.asObject(Expr.splice(rctx.value)) match {
                        case Left(e)             => Left(e): Either[ConfigDecodingError, A]
                        case Right(objectFields) =>
                          val coreResult: Either[ConfigDecodingError, A] =
                            Expr.splice(readAll(Expr.quote(objectFields)))
                          if (Expr.splice(allowUnknownKeysExpr)) coreResult
                          else
                            SConfigDerivationUtils
                              .withUnknownKeys[A](objectFields, coreResult, Expr.splice(expectedKeysExpr))
                      }
                    }
                }
              }
            }
      }
    }

    /** `value.asInstanceOf[F]` (a method of its own so that `F` is a type parameter in the quote). */
    /** Whether unknown keys are allowed when reading `T`: what its `ProductHint` says when there is one, the global
      * config otherwise. The first element is the answer when it is known at compile time.
      */
    @scala.annotation.nowarn("msg=is never used")
    private[rules] def allowUnknownKeysOf[T: Type](implicit ctx: ReaderCtx[?]): (Option[Boolean], Expr[Boolean]) = {
      implicit val BooleanT: Type[Boolean] = RTypes.Boolean
      implicit val ProductHintT: Type[ProductHint[T]] = RTypes.productHintType[T]
      Expr.summonImplicit[ProductHint[T]].toOption match {
        case Some(hint) => (None, Expr.quote(Expr.splice(hint).allowUnknownKeys))
        case None       =>
          (ctx.evaluatedConfig.map(_.allowUnknownKeys), Expr.quote(Expr.splice(ctx.config).allowUnknownKeys))
      }
    }

    private[rules] def castTo[F: Type](value: Expr[Any]): Expr[F] = Expr.quote(Expr.splice(value).asInstanceOf[F])

    @scala.annotation.nowarn("msg=is never used|unused explicit parameter")
    private[rules] def deriveFieldReader[Field: Type](implicit ctx: ReaderCtx[?]): MIO[Expr[ConfigReader[Field]]] = {
      implicit val ConfigValueT: Type[ConfigValue] = RTypes.ConfigValue
      implicit val ReaderResultField: Type[Either[ConfigDecodingError, Field]] = RTypes.ReaderResult[Field]
      implicit val ReaderField: Type[ConfigReader[Field]] = RTypes.ConfigReader[Field]

      RTypes
        .ConfigReader[Field]
        .summonExprIgnoring(ReaderUseImplicitWhenAvailableRule.ignoredImplicits*)
        .toEither match {
        case Right(readerExpr) =>
          Log.info(s"Found implicit ConfigReader[${Type[Field].prettyPrint}]") >> ctx.cachedInstance[Field](readerExpr)
        case Left(_) =>
          Log.info(s"Building ConfigReader[${Type[Field].prettyPrint}] via recursive derivation") >>
            LambdaBuilder
              .of1[ConfigValue]("fieldValue")
              .traverse { fieldValueExpr =>
                deriveReaderRecursively[Field](using ctx.nest[Field](fieldValueExpr))
              }
              .map { builder =>
                val readFn = builder.build[Either[ConfigDecodingError, Field]]
                // A derived reader of an `Option` reads a missing key as `None`, like the built-in `Option` reader.
                Type[Field] match {
                  case IsOption(_) =>
                    Expr.quote(SConfigDerivationUtils.readerFromFnReadingMissingKeys(Expr.splice(readFn)))
                  case _ => Expr.quote(SConfigDerivationUtils.readerFromFn(Expr.splice(readFn)))
                }
              }
      }
    }
  }
}
