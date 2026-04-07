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
import pureconfig.{ConfigCursor, ConfigReader}
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

      val transformExpr: Expr[String => String] = maybeHint match {
        case Some(h) => Expr.quote(Expr.splice(h).transformMemberNames)
        case None    => Expr.quote(Expr.splice(rctx.config).transformMemberNames)
      }
      val useDefaultsExpr: Expr[Boolean] = maybeHint match {
        case Some(h) => Expr.quote(Expr.splice(h).useDefaults)
        case None    => Expr.quote(Expr.splice(rctx.config).useDefaults)
      }
      val allowUnknownKeysExpr: Expr[Boolean] = maybeHint match {
        case Some(h) => Expr.quote(Expr.splice(h).allowUnknownKeys)
        case None    => Expr.quote(Expr.splice(rctx.config).allowUnknownKeys)
      }

      val constructor = caseClass.primaryConstructor
      val fieldsList = constructor.parameters.flatten.toList

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
        param.defaultValue.flatMap { existentialOuter =>
          val methodOf = existentialOuter.value
          methodOf.value match {
            case noInstance: Method.NoInstance[?] =>
              import noInstance.Returned
              noInstance(Map.empty).toOption.map { defaultExpr =>
                (fName, defaultExpr.as_??)
              }
            case _ => None
          }
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

          val indexedFields = fields.toList.zipWithIndex
          NonEmptyList
            .fromList(indexedFields)
            .get
            .parTraverse { case ((fName, param), reindex) =>
              import param.tpe.Underlying as Field
              val nameOverride = getAnnotationStringArg[configKey](param)
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
              Log.namedScope(s"Deriving reader for field $fName: ${Type[Field].prettyPrint}") {
                deriveFieldReader[Field].map { readerExpr =>
                  // Resolve the HOCON key for this field. `@configKey("custom")` always wins;
                  // otherwise apply the resolved transform (from per-type ProductHint or the
                  // global PureConfig).
                  val keyExpr: Expr[String] = nameOverride match {
                    case Some(customName) => Expr(customName)
                    case None             =>
                      Expr.quote(Expr.splice(transformExpr)(Expr.splice(Expr(fName))))
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
                  val decodeExpr: Expr[Either[ConfigReaderFailures, Any]] = defaultAsAnyOpt match {
                    case Some(defaultAnyExpr) =>
                      if (isOptionField)
                        Expr.quote {
                          val cur = Expr.splice(rctx.cursor)
                          cur.asObjectCursor
                            .flatMap { obj =>
                              val keyCur = obj.atKeyOrUndefined(Expr.splice(keyExpr))
                              if (Expr.splice(useDefaultsExpr) && keyCur.isUndefined)
                                Right(Expr.splice(defaultAnyExpr)): Either[ConfigReaderFailures, Any]
                              else
                                Expr.splice(readerExpr).from(keyCur)
                            }
                            .asInstanceOf[Either[ConfigReaderFailures, Any]]
                        }
                      else
                        Expr.quote {
                          val cur = Expr.splice(rctx.cursor)
                          cur.asObjectCursor
                            .flatMap { obj =>
                              val key = Expr.splice(keyExpr)
                              val keyCur = obj.atKeyOrUndefined(key)
                              if (Expr.splice(useDefaultsExpr) && keyCur.isUndefined)
                                Right(Expr.splice(defaultAnyExpr)): Either[ConfigReaderFailures, Any]
                              else
                                obj.atKey(key).flatMap(c => Expr.splice(readerExpr).from(c))
                            }
                            .asInstanceOf[Either[ConfigReaderFailures, Any]]
                        }
                    case None =>
                      if (isOptionField)
                        Expr.quote {
                          val cur = Expr.splice(rctx.cursor)
                          cur.asObjectCursor
                            .flatMap { obj =>
                              Expr.splice(readerExpr).from(obj.atKeyOrUndefined(Expr.splice(keyExpr)))
                            }
                            .asInstanceOf[Either[ConfigReaderFailures, Any]]
                        }
                      else
                        Expr.quote {
                          val cur = Expr.splice(rctx.cursor)
                          cur.asObjectCursor
                            .flatMap { obj =>
                              obj.atKey(Expr.splice(keyExpr)).flatMap(c => Expr.splice(readerExpr).from(c))
                            }
                            .asInstanceOf[Either[ConfigReaderFailures, Any]]
                        }
                  }
                  val makeAccessor: Expr[Array[Any]] => (String, Expr_??) = { arrExpr =>
                    val typedExpr = Expr.quote {
                      PureConfigDerivationUtils.unsafeCast(
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

              val listExpr: Expr[List[Either[ConfigReaderFailures, Any]]] =
                decodeExprs.foldRight(Expr.quote(List.empty[Either[ConfigReaderFailures, Any]])) { (elem, acc) =>
                  Expr.quote(Expr.splice(elem) :: Expr.splice(acc))
                }

              // Build the runtime expected-keys set from the per-field key expressions.
              // Used by the strict-mode (`allowUnknownKeys = false`) check at the end.
              val expectedKeysExpr: Expr[Set[String]] =
                keyExprs.foldRight(Expr.quote(Set.empty[String])) { (k, acc) =>
                  Expr.quote(Expr.splice(acc) + Expr.splice(k))
                }

              LambdaBuilder
                .of1[Array[Any]]("decodedValues")
                .traverse { decodedValuesExpr =>
                  val nonTransientFieldMap: Map[String, Expr_??] =
                    makeAccessors.map(_(decodedValuesExpr)).toMap
                  val fieldMap: Map[String, Expr_??] = nonTransientFieldMap ++ transientDefaults
                  caseClass.primaryConstructor(fieldMap) match {
                    case Right(constructExpr) => MIO.pure(constructExpr)
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
                  Expr.quote {
                    val coreResult: Either[ConfigReaderFailures, A] =
                      PureConfigDerivationUtils
                        .sequenceResults(Expr.splice(listExpr))
                        .map(Expr.splice(constructLambda))
                    PureConfigDerivationUtils.checkUnknownKeys[A](
                      Expr.splice(rctx.cursor),
                      coreResult,
                      Expr.splice(expectedKeysExpr),
                      Expr.splice(allowUnknownKeysExpr)
                    )
                  }
                }
            }
      }
    }

    /** Derive a `ConfigReader[Field]` for a case-class field, either by summoning an
      * implicit (filtered against our auto-derivation entry points) or by recursively
      * driving the rule chain on the field type.
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
