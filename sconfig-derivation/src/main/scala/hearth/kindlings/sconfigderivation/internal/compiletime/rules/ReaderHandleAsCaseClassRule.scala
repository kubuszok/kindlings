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
import org.ekrich.config.ConfigValue

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
    private def decodeCaseClassFields[A: ReaderCtx](
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
          method
            .fold(
              onInstance = _ => throw new RuntimeException("Default value should not need instance"),
              onTypes = _ => Map.empty,
              onValues = _ => Map.empty
            )
            .toOption
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
                  SConfigDerivationUtils.asObject(Expr.splice(rctx.value)).map(_ => Expr.splice(expr))
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
                    method
                      .fold(
                        onInstance = _ => throw new RuntimeException("Default value should not need instance"),
                        onTypes = _ => Map.empty,
                        onValues = _ => Map.empty
                      )
                      .toOption
                      .map { ee => import ee.Underlying; ee.value.upcast[Any] }
                  }
                else None
              val isOptionField: Boolean = Type[Field] match {
                case IsOption(_) => true
                case _           => false
              }
              Log.namedScope(s"Deriving reader for field $fName: ${Type[Field].prettyPrint}") {
                deriveFieldReader[Field].map { readerExpr =>
                  // Resolve the HOCON key for this field. `@configKey("custom")` always wins;
                  // otherwise apply the resolved transform (from per-type ProductHint or the
                  // global SConfig). When the config was evaluable at compile time, we
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

                  val decodeExpr: Expr[Either[ConfigDecodingError, Any]] = effectiveDefault match {
                    case Some(defaultAnyExpr) if useDefaultsIsStaticallyTrue =>
                      // useDefaults is statically true: no runtime boolean check needed
                      if (isOptionField)
                        Expr.quote {
                          val v = Expr.splice(rctx.value)
                          SConfigDerivationUtils
                            .asObject(v)
                            .flatMap { obj =>
                              val key = Expr.splice(keyExpr)
                              val keyVal = SConfigDerivationUtils.readOptionalField(obj, key)
                              if (obj.get(key) == null)
                                Right(Expr.splice(defaultAnyExpr)): Either[ConfigDecodingError, Any]
                              else
                                Expr.splice(readerExpr).from(keyVal)
                            }
                            .asInstanceOf[Either[ConfigDecodingError, Any]]
                        }
                      else
                        Expr.quote {
                          val v = Expr.splice(rctx.value)
                          SConfigDerivationUtils
                            .asObject(v)
                            .flatMap { obj =>
                              val key = Expr.splice(keyExpr)
                              if (obj.get(key) == null)
                                Right(Expr.splice(defaultAnyExpr)): Either[ConfigDecodingError, Any]
                              else
                                SConfigDerivationUtils
                                  .readRequiredField(obj, key)
                                  .flatMap(c => Expr.splice(readerExpr).from(c))
                            }
                            .asInstanceOf[Either[ConfigDecodingError, Any]]
                        }
                    case Some(defaultAnyExpr) =>
                      // useDefaults is unknown at compile time: keep runtime check
                      if (isOptionField)
                        Expr.quote {
                          val v = Expr.splice(rctx.value)
                          SConfigDerivationUtils
                            .asObject(v)
                            .flatMap { obj =>
                              val key = Expr.splice(keyExpr)
                              val keyVal = SConfigDerivationUtils.readOptionalField(obj, key)
                              if (Expr.splice(useDefaultsExpr) && obj.get(key) == null)
                                Right(Expr.splice(defaultAnyExpr)): Either[ConfigDecodingError, Any]
                              else
                                Expr.splice(readerExpr).from(keyVal)
                            }
                            .asInstanceOf[Either[ConfigDecodingError, Any]]
                        }
                      else
                        Expr.quote {
                          val v = Expr.splice(rctx.value)
                          SConfigDerivationUtils
                            .asObject(v)
                            .flatMap { obj =>
                              val key = Expr.splice(keyExpr)
                              if (Expr.splice(useDefaultsExpr) && obj.get(key) == null)
                                Right(Expr.splice(defaultAnyExpr)): Either[ConfigDecodingError, Any]
                              else
                                SConfigDerivationUtils
                                  .readRequiredField(obj, key)
                                  .flatMap(c => Expr.splice(readerExpr).from(c))
                            }
                            .asInstanceOf[Either[ConfigDecodingError, Any]]
                        }
                    case None =>
                      if (isOptionField)
                        Expr.quote {
                          val v = Expr.splice(rctx.value)
                          SConfigDerivationUtils
                            .asObject(v)
                            .flatMap { obj =>
                              Expr
                                .splice(readerExpr)
                                .from(SConfigDerivationUtils.readOptionalField(obj, Expr.splice(keyExpr)))
                            }
                            .asInstanceOf[Either[ConfigDecodingError, Any]]
                        }
                      else
                        Expr.quote {
                          val v = Expr.splice(rctx.value)
                          SConfigDerivationUtils
                            .asObject(v)
                            .flatMap { obj =>
                              SConfigDerivationUtils
                                .readRequiredField(obj, Expr.splice(keyExpr))
                                .flatMap(c => Expr.splice(readerExpr).from(c))
                            }
                            .asInstanceOf[Either[ConfigDecodingError, Any]]
                        }
                  }
                  val makeAccessor: Expr[Array[Any]] => (String, Expr_??) = { arrExpr =>
                    val typedExpr = Expr.quote {
                      SConfigDerivationUtils.unsafeCast(
                        Expr.splice(arrExpr)(Expr.splice(Expr(reindex))),
                        Expr.splice(readerExpr)
                      )
                    }
                    (fName, typedExpr.as_??)
                  }
                  (decodeExpr, makeAccessor, keyExpr)
                }
              }
            }
            .flatMap { fieldData =>
              val decodeExprs = fieldData.toList.map(_._1)
              val makeAccessors = fieldData.toList.map(_._2)
              val keyExprs = fieldData.toList.map(_._3)

              val listExpr: Expr[List[Either[ConfigDecodingError, Any]]] =
                decodeExprs.foldRight(Expr.quote(List.empty[Either[ConfigDecodingError, Any]])) { (elem, acc) =>
                  Expr.quote(Expr.splice(elem) :: Expr.splice(acc))
                }

              // Build the runtime expected-keys set from the per-field key expressions.
              // Used by the strict-mode (`allowUnknownKeys = false`) check at the end.
              // When we statically know allowUnknownKeys is true, skip building this set
              // entirely — it's never checked.
              val expectedKeysExpr: Expr[Set[String]] =
                if (staticAllowUnknownKeys.contains(true)) Expr.quote(Set.empty[String])
                else
                  keyExprs.foldRight(Expr.quote(Set.empty[String])) { (k, acc) =>
                    Expr.quote(Expr.splice(acc) + Expr.splice(k))
                  }

              LambdaBuilder
                .of1[Array[Any]]("decodedValues")
                .traverse { decodedValuesExpr =>
                  val nonTransientFieldMap: Map[String, Expr_??] =
                    makeAccessors.map(_(decodedValuesExpr)).toMap
                  val fieldMap: Map[String, Expr_??] = nonTransientFieldMap ++ transientDefaults
                  caseClass.primaryConstructor.fold(
                    onInstance = _ => throw new RuntimeException("Constructor should not need instance"),
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
                .map { builder =>
                  val constructLambda = builder.build[A]
                  // When allowUnknownKeys is statically true, skip the checkUnknownKeys
                  // call entirely — it's a no-op that just returns coreResult.
                  if (staticAllowUnknownKeys.contains(true))
                    Expr.quote {
                      SConfigDerivationUtils
                        .sequenceResults(Expr.splice(listExpr))
                        .map(Expr.splice(constructLambda))
                    }
                  else
                    Expr.quote {
                      val coreResult: Either[ConfigDecodingError, A] =
                        SConfigDerivationUtils
                          .sequenceResults(Expr.splice(listExpr))
                          .map(Expr.splice(constructLambda))
                      SConfigDerivationUtils.checkUnknownKeys[A](
                        Expr.splice(rctx.value),
                        coreResult,
                        Expr.splice(expectedKeysExpr),
                        Expr.splice(allowUnknownKeysExpr)
                      )
                    }
                }
            }
      }
    }

    @scala.annotation.nowarn("msg=is never used|unused explicit parameter")
    private def deriveFieldReader[Field: Type](implicit ctx: ReaderCtx[?]): MIO[Expr[ConfigReader[Field]]] = {
      implicit val ConfigValueT: Type[ConfigValue] = RTypes.ConfigValue
      implicit val ReaderResultField: Type[Either[ConfigDecodingError, Field]] = RTypes.ReaderResult[Field]
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
              .of1[ConfigValue]("fieldValue")
              .traverse { fieldValueExpr =>
                deriveReaderRecursively[Field](using ctx.nest[Field](fieldValueExpr))
              }
              .map { builder =>
                val readFn = builder.build[Either[ConfigDecodingError, Field]]
                Expr.quote(SConfigDerivationUtils.readerFromFn(Expr.splice(readFn)))
              }
      }
    }
  }
}
