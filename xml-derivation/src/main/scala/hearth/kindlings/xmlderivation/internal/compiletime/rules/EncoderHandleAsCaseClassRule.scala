package hearth.kindlings.xmlderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.data.NonEmptyList
import hearth.fp.effect.*
import hearth.fp.syntax.*
import hearth.std.*

import hearth.kindlings.xmlderivation.{XmlConfig, XmlFieldMode}
import hearth.kindlings.xmlderivation.annotations.{
  transientField,
  xmlAttribute,
  xmlContent,
  xmlElement,
  xmlName,
  xmlWrapper
}
import hearth.kindlings.xmlderivation.internal.runtime.XmlDerivationUtils

trait EncoderHandleAsCaseClassRuleImpl {
  this: EncoderMacrosImpl & MacroCommons & StdExtensions & AnnotationSupport =>

  object EncoderHandleAsCaseClassRule extends EncoderDerivationRule("handle as case class when possible") {

    def apply[A: EncoderCtx]: MIO[Rule.Applicability[Expr[scala.xml.Elem]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as a case class") >> {
        CaseClass.parse[A].toEither match {
          case Right(caseClass) =>
            val allFields = caseClass.caseFieldValuesAt(ectx.value).toList
            if (allFields.isEmpty)
              MIO.pure(
                Rule.yielded(s"The type ${Type[A].prettyPrint} is an empty case class, handled by singleton rule")
              )
            else
              for {
                _ <- ectx.setHelper[A] { (value, name, config) =>
                  encodeCaseClassFields[A](caseClass)(
                    using ectx.nestInCache(value, name, config)
                  )
                }
                result <- ectx.getHelper[A].flatMap {
                  case Some(helperCall) =>
                    MIO.pure(Rule.matched(helperCall(ectx.value, ectx.elementName, ectx.config)))
                  case None => MIO.pure(Rule.yielded(s"Failed to build helper for ${Type[A].prettyPrint}"))
                }
              } yield result
          case Left(reason) =>
            MIO.pure(Rule.yielded(reason))
        }
      }

    @scala.annotation.nowarn("msg=is never used|dead code")
    private def encodeCaseClassFields[A: EncoderCtx](
        caseClass: CaseClass[A]
    ): MIO[Expr[scala.xml.Elem]] = {
      implicit val ElemT: Type[scala.xml.Elem] = Types.Elem
      implicit val StringT: Type[String] = Types.String
      implicit val NodeT: Type[scala.xml.Node] = Types.Node
      implicit val XmlConfigT: Type[XmlConfig] = Types.XmlConfig
      implicit val XmlFieldModeT: Type[XmlFieldMode] = Types.XmlFieldMode
      implicit val ProductType: Type[Product] = Types.Product
      implicit val IntType: Type[Int] = Types.Int
      implicit val AnyType: Type[Any] = Types.Any
      implicit val transientFieldT: Type[transientField] = Types.TransientField
      implicit val xmlNameT: Type[xmlName] = Types.XmlNameAnnotation
      implicit val xmlAttributeT: Type[xmlAttribute] = Types.XmlAttributeAnnotation
      implicit val xmlElementT: Type[xmlElement] = Types.XmlElementAnnotation
      implicit val xmlContentT: Type[xmlContent] = Types.XmlContentAnnotation
      implicit val xmlWrapperT: Type[xmlWrapper] = Types.XmlWrapperAnnotation

      val allFields = caseClass.caseFieldValuesAt(ectx.value).toList
      val paramsByName: Map[String, Parameter] =
        if (allFields.isEmpty) Map.empty
        else caseClass.primaryConstructor.parameters.flatten.toMap

      NonEmptyList.fromList(allFields) match {
        case Some(fieldValues) =>
          fieldValues
            .traverse { case (fName, fieldValue) =>
              import fieldValue.{Underlying as Field, value as fieldExpr}
              val paramOpt = paramsByName.get(fName)
              val isTransient = paramOpt.exists(p => hasAnnotationType[transientField](p))
              val customName = paramOpt.flatMap(p => getAnnotationStringArg[xmlName](p))
              val isAttrAnnotated = paramOpt.exists(p => hasAnnotationType[xmlAttribute](p))
              val isElemAnnotated = paramOpt.exists(p => hasAnnotationType[xmlElement](p))
              val isContentAnnotated = paramOpt.exists(p => hasAnnotationType[xmlContent](p))

              if (isTransient) {
                MIO.pure(FieldEncoding.Skip)
              } else if (isContentAnnotated) {
                deriveEncoderRecursively[Field](using ectx.nest(fieldExpr)).map { encodedExpr =>
                  FieldEncoding.Content(encodedExpr)
                }
              } else if (isAttrAnnotated) {
                val xmlFieldName = customName.getOrElse(fName)
                MIO.pure(FieldEncoding.Attr(xmlFieldName, fieldExpr.upcast[Any]))
              } else if (isElemAnnotated) {
                val xmlFieldName = customName.getOrElse(fName)
                val wrapperName = paramOpt.flatMap(p => getAnnotationStringArg[xmlWrapper](p))
                deriveEncoderRecursively[Field](using
                  ectx
                    .nest(fieldExpr)
                    .copy(
                      elementName = Expr(xmlFieldName)
                    )
                ).map { encodedExpr =>
                  wrapperName match {
                    case Some(wrapper) => FieldEncoding.WrappedChild(wrapper, encodedExpr)
                    case None          => FieldEncoding.Child(encodedExpr)
                  }
                }
              } else {
                // Default mode from config - use Element by default
                val xmlFieldName = customName.getOrElse(fName)
                deriveEncoderRecursively[Field](using
                  ectx
                    .nest(fieldExpr)
                    .copy(
                      elementName = Expr(xmlFieldName)
                    )
                ).map { encodedExpr =>
                  FieldEncoding.Child(encodedExpr)
                }
              }
            }
            .map { encodings =>
              val attrList: List[(String, Expr[Any])] = encodings.toList.collect {
                case FieldEncoding.Attr(name, expr) => (name, expr)
              }
              val childList: List[Expr[scala.xml.Elem]] = encodings.toList.collect {
                case FieldEncoding.Child(expr)           => expr
                case FieldEncoding.WrappedChild(_, expr) => expr
              }
              val contentOpt: Option[Expr[scala.xml.Elem]] = encodings.toList.collectFirst {
                case FieldEncoding.Content(expr) => expr
              }

              // Build attributes using foldRight
              val attrListExpr: Expr[List[(String, String)]] = attrList.foldRight(
                Expr.quote(List.empty[(String, String)])
              ) { case ((name, valueExpr), acc) =>
                Expr.quote {
                  (Expr.splice(Expr(name)), Expr.splice(valueExpr).toString) :: Expr.splice(acc)
                }
              }

              // Build children list using foldRight
              val childListExpr: Expr[List[scala.xml.Node]] = childList.foldRight(
                Expr.quote(List.empty[scala.xml.Node])
              ) { (elem, acc) =>
                Expr.quote {
                  (Expr.splice(elem): scala.xml.Node) :: Expr.splice(acc)
                }
              }

              contentOpt match {
                case Some(contentExpr) =>
                  // Content mode: attributes + content from annotated field
                  Expr.quote {
                    val attrs = Expr.splice(attrListExpr)
                    val contentElem = Expr.splice(contentExpr)
                    XmlDerivationUtils.combineAttributesAndChildren(
                      Expr.splice(ectx.elementName),
                      attrs,
                      contentElem.child.toList
                    )
                  }
                case None =>
                  // Normal mode: attributes + child elements
                  Expr.quote {
                    val attrs = Expr.splice(attrListExpr)
                    val children: List[scala.xml.Node] = Expr.splice(childListExpr)
                    XmlDerivationUtils.combineAttributesAndChildren(
                      Expr.splice(ectx.elementName),
                      attrs,
                      children
                    )
                  }
              }
            }
        case None =>
          MIO.pure(Expr.quote {
            XmlDerivationUtils.makeEmptyElem(Expr.splice(ectx.elementName))
          })
      }
    }
  }

}
