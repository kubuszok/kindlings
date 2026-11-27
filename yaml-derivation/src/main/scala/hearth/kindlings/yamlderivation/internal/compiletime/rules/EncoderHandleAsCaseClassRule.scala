package hearth.kindlings.yamlderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.data.NonEmptyList
import hearth.fp.effect.*
import hearth.fp.syntax.*
import hearth.std.*

import hearth.kindlings.yamlderivation.annotations.{fieldName, transientField}
import hearth.kindlings.yamlderivation.internal.runtime.YamlDerivationUtils
import org.virtuslab.yaml.Node

trait EncoderHandleAsCaseClassRuleImpl {
  this: EncoderMacrosImpl & MacroCommons & StdExtensions & AnnotationSupport =>

  object EncoderHandleAsCaseClassRule extends EncoderDerivationRule("handle as case class when possible") {

    def apply[A: EncoderCtx]: MIO[Rule.Applicability[Expr[Node]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as a case class") >> {
        CaseClass.parse[A].toEither match {
          case Right(caseClass) =>
            encodeCaseClassFields[A](caseClass).map(Rule.matched)

          case Left(reason) =>
            MIO.pure(Rule.yielded(reason))
        }
      }

    @scala.annotation.nowarn("msg=is never used")
    private def productFieldAt[A: Type, Field: Type](value: Expr[A], index: Int): Expr[Field] = {
      implicit val IntT: Type[Int] = Types.Int
      Expr.quote {
        YamlDerivationUtils.productField(Expr.splice(value), Expr.splice(Expr(index))).asInstanceOf[Field]
      }
    }

    @scala.annotation.nowarn("msg=is never used")
    private def encodeCaseClassFields[A: EncoderCtx](
        caseClass: CaseClass[A]
    ): MIO[Expr[Node]] = {
      implicit val NodeT: Type[Node] = Types.Node
      implicit val StringT: Type[String] = Types.String
      implicit val fieldNameT: Type[fieldName] = Types.FieldName
      implicit val transientFieldT: Type[transientField] = Types.TransientField

      val accessibleFields = caseClass.caseFieldValuesAt(ectx.value).toList
      // `caseFieldValuesAt` has no `private val` constructor parameters: they are read by their position (scala-yaml
      // writes every field, and the decoder needs every constructor parameter).
      val allFields: List[(String, Expr_??)] =
        if (!(Type[A] <:< Type.of[Product])) accessibleFields
        else {
          val accessible = accessibleFields.toMap
          val firstParameterList = caseClass.primaryConstructor.totalParameters.headOption.toList.flatMap(_.toList)
          val inConstructor: List[(String, Expr_??)] = firstParameterList.zipWithIndex.map {
            case ((name, param), index) =>
              accessible.get(name) match {
                case Some(value) => name -> value
                case None        =>
                  import param.tpe.Underlying as Field
                  name -> productFieldAt[A, Field](ectx.value, index).as_??
              }
          }
          val constructorNames = inConstructor.map(_._1).toSet
          inConstructor ++ accessibleFields.filterNot { case (name, _) => constructorNames(name) }
        }

      val paramsByName: Map[String, Parameter] =
        if (allFields.isEmpty) Map.empty
        else caseClass.primaryConstructor.totalParameters.flatten.toMap

      paramsByName.collectFirst {
        case (name, param) if hasAnnotationType[transientField](param) && !param.hasDefault => name
      } match {
        case Some(name) =>
          val err = EncoderDerivationError.TransientFieldMissingDefault(name, Type[A].prettyPrint)
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
                  Log.namedScope(s"Encoding field ${ectx.value.prettyPrint}.$fName: ${Type[Field].prettyPrint}") {
                    deriveEncoderRecursively[Field](using ectx.nest(fieldExpr)).map { fieldNode =>
                      val nameOverride =
                        paramsByName.get(fName).flatMap(p => getAnnotationStringArg[fieldName](p))
                      (fName, fieldNode, nameOverride)
                    }
                  }
                }
                .flatMap { fieldPairs =>
                  // the names: a @fieldName, constants with a configuration known at compile time, otherwise computed
                  // once per instance
                  val toTransform = fieldPairs.toList.collect { case (fName, _, None) => fName }
                  val namesMIO: MIO[String => Expr[String]] = ectx.evaluatedConfig match {
                    case Some(evConfig) => MIO.pure((name: String) => Expr(evConfig.transformMemberNames(name)))
                    case None           => ectx.transformedMemberNames(toTransform)
                  }
                  namesMIO.map { nameOf =>
                    fieldPairs.toList.foldRight(Expr.quote(List.empty[(String, Node)])) {
                      case ((fName, fieldNode, nameOverride), acc) =>
                        val nameExpr: Expr[String] = nameOverride match {
                          case Some(customName) => Expr(customName)
                          case None             => nameOf(fName)
                        }
                        Expr.quote {
                          (
                            Expr.splice(nameExpr),
                            Expr.splice(fieldNode)
                          ) ::
                            Expr.splice(acc)
                        }
                    }
                  }
                }
                .map { fieldsListExpr =>
                  Expr.quote {
                    YamlDerivationUtils.nodeFromFields(Expr.splice(fieldsListExpr))
                  }
                }
            case None =>
              MIO.pure(Expr.quote {
                YamlDerivationUtils.nodeFromFields(Nil)
              })
          }
      }
    }
  }
}
