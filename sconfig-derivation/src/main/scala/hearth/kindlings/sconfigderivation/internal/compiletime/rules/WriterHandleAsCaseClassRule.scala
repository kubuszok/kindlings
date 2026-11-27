package hearth.kindlings.sconfigderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.data.NonEmptyList
import hearth.fp.effect.*
import hearth.fp.syntax.*
import hearth.std.*

import org.ekrich.config.ConfigValue
import hearth.kindlings.sconfigderivation.ProductHint
import hearth.kindlings.sconfigderivation.annotations.{configKey, transientField}
import hearth.kindlings.sconfigderivation.internal.runtime.SConfigDerivationUtils
import hearth.kindlings.sconfigderivation.ConfigWriter

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
                    val nameOverride =
                      paramsByName.get(fName).flatMap(p => getAnnotationStringArg[configKey](p))
                    putFieldOf[Field](fieldExpr).map(put => (fName, nameOverride, put))
                  }
                }
                .map { fieldData =>
                  // One statement per field putting it into the object's map. Building `(key, value) :: ...` with an
                  // `if` inside each element made the compiler keep every previous element on the stack and emit a
                  // stack map frame per field listing all of them.
                  val putFields: Expr[java.util.LinkedHashMap[String, ConfigValue]] => Expr[Unit] = { fieldsMap =>
                    fieldData.toList.foldLeft(Expr.quote(()): Expr[Unit]) { case (acc, (fName, nameOverride, put)) =>
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
                      val putField = put(fieldsMap, keyExpr)
                      Expr.quote {
                        Expr.splice(acc)
                        Expr.splice(putField)
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

    /** How a field's value is put into the map of the object being written, given the map and the key.
      *
      * Like upstream PureConfig's derived writers: a field whose type has a `ConfigWriter` in scope is written with it,
      * and when that writer is a `WritesMissingKeys` (upstream's `Option` writer, user-provided ones) it decides
      * whether the key is written at all - so a user-provided plain writer of an `Option` type writes its empty values
      * too. A field whose writer is derived here is written, except for an empty `Option`, which is omitted (as
      * upstream's `Option` writer does).
      */
    @scala.annotation.nowarn("msg=is never used")
    private def putFieldOf[Field: Type](fieldExpr: Expr[Field])(implicit
        ctx: WriterCtx[?]
    ): MIO[(Expr[java.util.LinkedHashMap[String, ConfigValue]], Expr[String]) => Expr[Unit]] = {
      implicit val ConfigValueT: Type[ConfigValue] = WTypes.ConfigValue
      implicit val StringT: Type[String] = WTypes.String
      implicit val WriterField: Type[ConfigWriter[Field]] = WTypes.ConfigWriter[Field]
      val summoned =
        if (ctx.derivedType.exists(_.Underlying =:= Type[Field])) None
        else
          WTypes
            .ConfigWriter[Field]
            .summonExprIgnoring(WriterUseImplicitWhenAvailableRule.ignoredImplicits*)
            .toEither
            .toOption
      summoned match {
        case Some(writerExpr) =>
          ctx.cachedInstance[Field](writerExpr).map { cached => (fieldsMap, keyExpr) =>
            Expr.quote {
              SConfigDerivationUtils.putFieldWith[Field](
                Expr.splice(fieldsMap),
                Expr.splice(keyExpr),
                Expr.splice(cached),
                Expr.splice(fieldExpr)
              )
            }
          }
        case None =>
          deriveWriterRecursively[Field](using ctx.nest(fieldExpr)).map { fieldValueExpr =>
            Type[Field] match {
              case IsOption(_) =>
                implicit val AnyT: Type[Any] = WTypes.Any
                (fieldsMap, keyExpr) =>
                  Expr.quote {
                    if (Expr.splice(fieldExpr.upcast[Any]).asInstanceOf[Option[Any]].isEmpty) ()
                    else
                      SConfigDerivationUtils.putField(
                        Expr.splice(fieldsMap),
                        Expr.splice(keyExpr),
                        Expr.splice(fieldValueExpr)
                      )
                  }
              case _ =>
                (fieldsMap, keyExpr) =>
                  Expr.quote {
                    SConfigDerivationUtils.putField(
                      Expr.splice(fieldsMap),
                      Expr.splice(keyExpr),
                      Expr.splice(fieldValueExpr)
                    )
                  }
            }
          }
      }
    }
  }
}
