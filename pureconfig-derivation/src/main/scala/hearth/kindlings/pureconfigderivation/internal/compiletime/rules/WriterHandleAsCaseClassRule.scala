package hearth.kindlings.pureconfigderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.data.NonEmptyList
import hearth.fp.effect.*
import hearth.fp.syntax.*
import hearth.std.*

import com.typesafe.config.ConfigValue
import hearth.kindlings.pureconfigderivation.KindlingsProductHint
import hearth.kindlings.pureconfigderivation.annotations.{configKey, transientField}
import hearth.kindlings.pureconfigderivation.internal.runtime.PureConfigDerivationUtils

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
      implicit val ProductHintT: Type[KindlingsProductHint[A]] = WTypes.kindlingsProductHintType[A]

      // Per-type override mirrors the reader: if `KindlingsProductHint[A]` is in scope,
      // use it; otherwise fall back to the global `PureConfig` config.
      val maybeHint: Option[Expr[KindlingsProductHint[A]]] = Expr.summonImplicit[KindlingsProductHint[A]].toOption
      val transformExpr: Expr[String => String] = maybeHint match {
        case Some(h) => Expr.quote(Expr.splice(h).transformMemberNames)
        case None    => Expr.quote(Expr.splice(wctx.config).transformMemberNames)
      }

      val allFields = caseClass.caseFieldValuesAt(wctx.value).toList

      val paramsByName: Map[String, Parameter] =
        if (allFields.isEmpty) Map.empty
        else caseClass.primaryConstructor.parameters.flatten.toMap

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
                      (fName, fieldValueExpr, nameOverride)
                    }
                  }
                }
                .map { fieldData =>
                  fieldData.toList.foldRight(Expr.quote(List.empty[(String, ConfigValue)])) {
                    case ((fName, fieldValueExpr, Some(customName)), acc) =>
                      Expr.quote {
                        (
                          Expr.splice(Expr(customName)),
                          Expr.splice(fieldValueExpr)
                        ) ::
                          Expr.splice(acc)
                      }
                    case ((fName, fieldValueExpr, None), acc) =>
                      Expr.quote {
                        (
                          Expr.splice(transformExpr)(Expr.splice(Expr(fName))),
                          Expr.splice(fieldValueExpr)
                        ) ::
                          Expr.splice(acc)
                      }
                  }
                }
                .map { fieldsListExpr =>
                  Expr.quote {
                    PureConfigDerivationUtils.writeFields(Expr.splice(fieldsListExpr))
                  }
                }
            case None =>
              MIO.pure(Expr.quote {
                PureConfigDerivationUtils.writeFields(Nil)
              })
          }
      }
    }
  }
}
