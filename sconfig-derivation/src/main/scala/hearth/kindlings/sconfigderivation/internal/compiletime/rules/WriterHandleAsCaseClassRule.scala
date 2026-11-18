package hearth.kindlings.sconfigderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.data.NonEmptyList
import hearth.fp.effect.*
import hearth.fp.syntax.*
import hearth.std.*

import hearth.kindlings.sconfigderivation.ProductHint
import hearth.kindlings.sconfigderivation.annotations.{configKey, transientField}
import hearth.kindlings.sconfigderivation.internal.runtime.SConfigDerivationUtils
import org.ekrich.config.ConfigValue

trait WriterHandleAsCaseClassRuleImpl {
  this: WriterMacrosImpl & MacroCommons & StdExtensions & AnnotationSupport =>

  object WriterHandleAsCaseClassRule extends WriterDerivationRule("handle as case class when possible") {

    def apply[A: WriterCtx]: MIO[Rule.Applicability[Expr[ConfigValue]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as a case class") >> {
        CaseClass.parse[A].toEither match {
          case Right(caseClass) =>
            encodeCaseClassFields[A](caseClass).map(Rule.matched)
          case Left(reason) =>
            MIO.pure(Rule.yielded(reason))
        }
      }

    @scala.annotation.nowarn("msg=is never used")
    private def encodeCaseClassFields[A: WriterCtx](
        caseClass: CaseClass[A]
    ): MIO[Expr[ConfigValue]] = {
      implicit val ConfigValueT: Type[ConfigValue] = WTypes.ConfigValue
      implicit val StringT: Type[String] = WTypes.String
      implicit val FuncT: Type[String => String] = WTypes.StringToString
      implicit val configKeyT: Type[configKey] = WTypes.ConfigKey
      implicit val transientFieldT: Type[transientField] = WTypes.TransientField
      implicit val ProductHintT: Type[ProductHint[A]] = WTypes.productHintType[A]

      // Per-type override mirrors the reader: if `ProductHint[A]` is in scope,
      // use it; otherwise fall back to the global `SConfig` config.
      val maybeHint: Option[Expr[ProductHint[A]]] = Expr.summonImplicit[ProductHint[A]].toOption

      // When no per-type hint is present and the global config was evaluable at compile time,
      // we can pre-compute transformMemberNames(fieldName) as constant strings.
      val preComputedTransform: Option[String => String] = maybeHint match {
        case Some(_) => None
        case None    => wctx.evaluatedConfig.map(_.transformMemberNames)
      }

      val transformExpr: Expr[String => String] = maybeHint match {
        case Some(h) => Expr.quote(Expr.splice(h).transformMemberNames)
        case None    => Expr.quote(Expr.splice(wctx.config).transformMemberNames)
      }

      val allFields = caseClass.caseFieldValuesAt(wctx.value).toList

      val paramsByName: Map[String, Parameter] =
        if (allFields.isEmpty) Map.empty
        else caseClass.primaryConstructor.totalParameters.flatten.toMap

      paramsByName.collectFirst {
        case (name, param) if hasAnnotationType[transientField](param) && !param.hasDefault => name
      } match {
        case Some(name) =>
          val err = WriterDerivationError.TransientFieldMissingDefault(name, Type[A].prettyPrint)
          Log.error(err.message) >> MIO.fail(err)
        case None =>
          val nonTransientFields = allFields.filter { case (name, _) =>
            paramsByName.get(name).forall(p => !hasAnnotationType[transientField](p))
          }

          NonEmptyList.fromList(nonTransientFields) match {
            case Some(fields) =>
              fields
                .parTraverse { case (fName, fieldValue) =>
                  import fieldValue.{Underlying as Field, value as fieldExpr}
                  Log.namedScope(s"Writing field ${wctx.value.prettyPrint}.$fName: ${Type[Field].prettyPrint}") {
                    deriveWriterRecursively[Field](using wctx.nest(fieldExpr)).map { fieldValueExpr =>
                      val nameOverride =
                        paramsByName.get(fName).flatMap(p => getAnnotationStringArg[configKey](p))
                      // Like upstream PureConfig (`WritesMissingKeys`), an empty `Option` field is omitted, not
                      // written as null.
                      val emptyOption: Option[Expr[Boolean]] = Type[Field] match {
                        case IsOption(_) =>
                          implicit val AnyT: Type[Any] = WTypes.Any
                          Some(Expr.quote(Expr.splice(fieldExpr.upcast[Any]).asInstanceOf[Option[Any]].isEmpty))
                        case _ => None
                      }
                      (fName, fieldValueExpr, nameOverride, emptyOption)
                    }
                  }
                }
                .map { fieldData =>
                  // One statement per field putting it into the object's map (an empty `Option` is not put: like
                  // upstream's `WritesMissingKeys`, it is omitted). Building `(key, value) :: ...` with an `if` inside
                  // each element made the compiler keep every previous element on the stack and emit a stack map
                  // frame per field listing all of them.
                  val putFields: Expr[java.util.LinkedHashMap[String, ConfigValue]] => Expr[Unit] = { fieldsMap =>
                    fieldData.toList.foldLeft(Expr.quote(()): Expr[Unit]) {
                      case (acc, (fName, fieldValueExpr, nameOverride, emptyOption)) =>
                        // When the config was evaluable at compile time, pre-compute the
                        // mapped name as a constant string.
                        val keyExpr: Expr[String] = nameOverride match {
                          case Some(customName) => Expr(customName)
                          case None             =>
                            preComputedTransform match {
                              case Some(transform) => Expr(transform(fName))
                              case None            => Expr.quote(Expr.splice(transformExpr)(Expr.splice(Expr(fName))))
                            }
                        }
                        emptyOption match {
                          case Some(isEmpty) =>
                            Expr.quote {
                              Expr.splice(acc)
                              if (Expr.splice(isEmpty)) ()
                              else
                                SConfigDerivationUtils.putField(
                                  Expr.splice(fieldsMap),
                                  Expr.splice(keyExpr),
                                  Expr.splice(fieldValueExpr)
                                )
                            }
                          case None =>
                            Expr.quote {
                              Expr.splice(acc)
                              SConfigDerivationUtils.putField(
                                Expr.splice(fieldsMap),
                                Expr.splice(keyExpr),
                                Expr.splice(fieldValueExpr)
                              )
                            }
                        }
                    }
                  }
                  Expr.quote {
                    val writtenFields = new java.util.LinkedHashMap[String, ConfigValue]()
                    Expr.splice(putFields(Expr.quote(writtenFields)))
                    SConfigDerivationUtils.fieldsToObject(writtenFields)
                  }
                }
            case None =>
              MIO.pure(Expr.quote {
                SConfigDerivationUtils.writeFields(Nil)
              })
          }
      }
    }
  }
}
