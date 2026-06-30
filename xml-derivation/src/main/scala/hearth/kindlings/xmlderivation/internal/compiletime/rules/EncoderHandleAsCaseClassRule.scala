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
              encodeCaseClassFields[A](caseClass).map(Rule.matched)
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
      implicit val BoolType: Type[Boolean] = Types.Boolean
      implicit val transientFieldT: Type[transientField] = Types.TransientField
      implicit val xmlNameT: Type[xmlName] = Types.XmlNameAnnotation
      implicit val xmlAttributeT: Type[xmlAttribute] = Types.XmlAttributeAnnotation
      implicit val xmlElementT: Type[xmlElement] = Types.XmlElementAnnotation
      implicit val xmlContentT: Type[xmlContent] = Types.XmlContentAnnotation
      implicit val xmlWrapperT: Type[xmlWrapper] = Types.XmlWrapperAnnotation

      val allFields = caseClass.caseFieldValuesAt(ectx.value).toList
      val paramsByName: Map[String, Parameter] =
        if (allFields.isEmpty) Map.empty
        else caseClass.primaryConstructor.totalParameters.flatten.toMap

      // Check if we can skip transient checks entirely (statically known config)
      val noTransientChecksNeeded: Boolean = ectx.evaluatedConfig.exists { c =>
        !c.transientNone && !c.transientEmpty
      }

      // Determine default field mode from config
      val defaultFieldModeIsAttribute: Option[Boolean] = ectx.evaluatedConfig.map { c =>
        c.defaultFieldMode == XmlFieldMode.Attribute
      }

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

              // Apply fieldNameMapper: annotation overrides config mapper
              // When evaluatedConfig is available, pre-compute at compile time; otherwise use runtime call
              val xmlFieldNameExpr: Expr[String] = customName match {
                case Some(name) => Expr(name)
                case None       =>
                  ectx.evaluatedConfig match {
                    case Some(cfg) => Expr(cfg.fieldNameMapper(fName))
                    case None      =>
                      Expr.quote(Expr.splice(ectx.config).fieldNameMapper(Expr.splice(Expr(fName))))
                  }
              }

              // Build transient skip conditions for this field
              val skipConditions: List[Expr[Boolean]] =
                if (noTransientChecksNeeded || isTransient) Nil
                else buildFieldSkipConditions[Field](fieldExpr, ectx.evaluatedConfig, ectx.config)

              if (isTransient) {
                MIO.pure(FieldEncoding.Skip)
              } else if (isContentAnnotated) {
                deriveEncoderRecursively[Field](using ectx.nest(fieldExpr)).map { encodedExpr =>
                  FieldEncoding.Content(encodedExpr)
                }
              } else if (isAttrAnnotated) {
                MIO.pure(FieldEncoding.AttrExpr(xmlFieldNameExpr, fieldExpr.upcast[Any], skipConditions))
              } else if (isElemAnnotated) {
                val wrapperName = paramOpt.flatMap(p => getAnnotationStringArg[xmlWrapper](p))
                deriveEncoderRecursively[Field](using
                  ectx
                    .nest(fieldExpr)
                    .copy(
                      elementName = xmlFieldNameExpr
                    )
                ).map { encodedExpr =>
                  wrapperName match {
                    case Some(wrapper) => FieldEncoding.WrappedChild(wrapper, encodedExpr, skipConditions)
                    case None          => FieldEncoding.Child(encodedExpr, skipConditions)
                  }
                }
              } else {
                // Default mode from config
                defaultFieldModeIsAttribute match {
                  case Some(true) =>
                    // Statically known to be attribute mode
                    MIO.pure(FieldEncoding.AttrExpr(xmlFieldNameExpr, fieldExpr.upcast[Any], skipConditions))
                  case Some(false) =>
                    // Statically known to be element mode
                    deriveEncoderRecursively[Field](using
                      ectx
                        .nest(fieldExpr)
                        .copy(
                          elementName = xmlFieldNameExpr
                        )
                    ).map { encodedExpr =>
                      FieldEncoding.Child(encodedExpr, skipConditions)
                    }
                  case None =>
                    // Not statically known — derive both paths and branch at runtime
                    deriveEncoderRecursively[Field](using
                      ectx
                        .nest(fieldExpr)
                        .copy(
                          elementName = xmlFieldNameExpr
                        )
                    ).map { encodedExpr =>
                      FieldEncoding.DynamicMode(
                        xmlFieldNameExpr,
                        fieldExpr.upcast[Any],
                        encodedExpr,
                        skipConditions
                      )
                    }
                }
              }
            }
            .map { encodings =>
              // Static attributes (from @xmlAttribute or statically-known attribute mode)
              val staticAttrList: List[(Expr[String], Expr[Any], List[Expr[Boolean]])] = encodings.toList.collect {
                case FieldEncoding.Attr(name, expr, skipConds)         => (Expr(name), expr, skipConds)
                case FieldEncoding.AttrExpr(nameExpr, expr, skipConds) => (nameExpr, expr, skipConds)
              }
              // Static children (from @xmlElement or statically-known element mode)
              val staticChildList: List[(Expr[scala.xml.Elem], List[Expr[Boolean]])] = encodings.toList.collect {
                case FieldEncoding.Child(expr, skipConds)           => (expr, skipConds)
                case FieldEncoding.WrappedChild(_, expr, skipConds) => (expr, skipConds)
              }
              val contentOpt: Option[Expr[scala.xml.Elem]] = encodings.toList.collectFirst {
                case FieldEncoding.Content(expr) => expr
              }
              // Dynamic mode fields (runtime attribute-or-element decision)
              val dynamicFields: List[FieldEncoding.DynamicMode] = encodings.toList.collect {
                case d: FieldEncoding.DynamicMode => d
              }

              // Build static attributes using foldRight, with skip conditions
              val staticAttrListExpr: Expr[List[(String, String)]] = staticAttrList.foldRight(
                Expr.quote(List.empty[(String, String)])
              ) { case ((nameExpr, valueExpr, skipConds), acc) =>
                if (skipConds.isEmpty) {
                  Expr.quote {
                    (Expr.splice(nameExpr), Expr.splice(valueExpr).toString) :: Expr.splice(acc)
                  }
                } else {
                  val skipExpr = skipConds.reduce { (a, b) =>
                    Expr.quote(Expr.splice(a) || Expr.splice(b))
                  }
                  Expr.quote {
                    if (Expr.splice(skipExpr)) Expr.splice(acc)
                    else (Expr.splice(nameExpr), Expr.splice(valueExpr).toString) :: Expr.splice(acc)
                  }
                }
              }

              // Build static children list using foldRight, with skip conditions
              val staticChildListExpr: Expr[List[scala.xml.Node]] = staticChildList.foldRight(
                Expr.quote(List.empty[scala.xml.Node])
              ) { case ((elem, skipConds), acc) =>
                if (skipConds.isEmpty) {
                  Expr.quote {
                    (Expr.splice(elem): scala.xml.Node) :: Expr.splice(acc)
                  }
                } else {
                  val skipExpr = skipConds.reduce { (a, b) =>
                    Expr.quote(Expr.splice(a) || Expr.splice(b))
                  }
                  Expr.quote {
                    if (Expr.splice(skipExpr)) Expr.splice(acc)
                    else (Expr.splice(elem): scala.xml.Node) :: Expr.splice(acc)
                  }
                }
              }

              // Build dynamic field contributions (runtime attribute-or-element)
              val dynAttrContribExpr: Expr[List[(String, String)]] = dynamicFields.foldRight(
                Expr.quote(List.empty[(String, String)])
              ) { case (FieldEncoding.DynamicMode(nameExpr, valueExpr, _, skipConds), acc) =>
                val addExpr: Expr[List[(String, String)]] = Expr.quote {
                  (Expr.splice(nameExpr), Expr.splice(valueExpr).toString) :: Expr.splice(acc)
                }
                if (skipConds.isEmpty) addExpr
                else {
                  val skipExpr = skipConds.reduce((a, b) => Expr.quote(Expr.splice(a) || Expr.splice(b)))
                  Expr.quote(if (Expr.splice(skipExpr)) Expr.splice(acc) else Expr.splice(addExpr))
                }
              }
              val dynChildContribExpr: Expr[List[scala.xml.Node]] = dynamicFields.foldRight(
                Expr.quote(List.empty[scala.xml.Node])
              ) { case (FieldEncoding.DynamicMode(_, _, elemExpr, skipConds), acc) =>
                val addExpr: Expr[List[scala.xml.Node]] = Expr.quote {
                  (Expr.splice(elemExpr): scala.xml.Node) :: Expr.splice(acc)
                }
                if (skipConds.isEmpty) addExpr
                else {
                  val skipExpr = skipConds.reduce((a, b) => Expr.quote(Expr.splice(a) || Expr.splice(b)))
                  Expr.quote(if (Expr.splice(skipExpr)) Expr.splice(acc) else Expr.splice(addExpr))
                }
              }

              val hasDynamicFields = dynamicFields.nonEmpty

              contentOpt match {
                case Some(contentExpr) =>
                  // Content mode: attributes + content from annotated field
                  Expr.quote {
                    val attrs = Expr.splice(staticAttrListExpr)
                    val contentElem = Expr.splice(contentExpr)
                    XmlDerivationUtils.combineAttributesAndChildren(
                      Expr.splice(ectx.elementName),
                      attrs,
                      contentElem.child.toList
                    )
                  }
                case None if !hasDynamicFields =>
                  // No dynamic fields: simple static assembly
                  Expr.quote {
                    val attrs = Expr.splice(staticAttrListExpr)
                    val children: List[scala.xml.Node] = Expr.splice(staticChildListExpr)
                    XmlDerivationUtils.combineAttributesAndChildren(
                      Expr.splice(ectx.elementName),
                      attrs,
                      children
                    )
                  }
                case None =>
                  // Dynamic fields present: branch at runtime based on config.defaultFieldMode
                  Expr.quote {
                    val isAttrMode = XmlDerivationUtils.isAttributeMode(Expr.splice(ectx.config).defaultFieldMode)
                    val dynAttrs: List[(String, String)] =
                      if (isAttrMode) Expr.splice(dynAttrContribExpr) else Nil
                    val dynChildren: List[scala.xml.Node] =
                      if (isAttrMode) Nil else Expr.splice(dynChildContribExpr)
                    val attrs = Expr.splice(staticAttrListExpr) ++ dynAttrs
                    val children: List[scala.xml.Node] = Expr.splice(staticChildListExpr) ++ dynChildren
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

    /** Build transient skip conditions for a single encoder field. Returns empty list when no conditions apply. */
    @scala.annotation.nowarn("msg=is never used")
    private def buildFieldSkipConditions[Field: Type](
        fieldExpr: Expr[Field],
        evaluatedConfig: Option[XmlConfig],
        configExpr: Expr[XmlConfig]
    )(implicit AnyT: Type[Any], BoolT: Type[Boolean], XmlConfigT: Type[XmlConfig]): List[Expr[Boolean]] = {
      val isOptionField = Type[Field] match {
        case IsOption(_) => true
        case _           => false
      }
      val isCollectionField = !isOptionField && {
        val isMap = Type[Field] match { case IsMap(_) => true; case _ => false }
        val isColl = Type[Field] match { case IsCollection(_) => true; case _ => false }
        isMap || isColl
      }
      val isStringField = Type[Field] =:= Types.String

      val conditions = List.newBuilder[Expr[Boolean]]

      // transientNone: skip Option fields that are None
      if (isOptionField) {
        // When evaluatedConfig is available and transientNone is statically false, skip this condition
        val staticallyFalse = evaluatedConfig.exists(!_.transientNone)
        if (!staticallyFalse) {
          val staticallyTrue = evaluatedConfig.exists(_.transientNone)
          if (staticallyTrue) {
            conditions += Expr.quote {
              !Expr.splice(fieldExpr).asInstanceOf[Option[Any]].isDefined
            }
          } else {
            conditions += Expr.quote {
              Expr.splice(configExpr).transientNone &&
              !Expr.splice(fieldExpr).asInstanceOf[Option[Any]].isDefined
            }
          }
        }
      }

      // transientEmpty: skip empty collections and strings
      if (isCollectionField || isStringField) {
        val staticallyFalse = evaluatedConfig.exists(!_.transientEmpty)
        if (!staticallyFalse) {
          val staticallyTrue = evaluatedConfig.exists(_.transientEmpty)
          if (isStringField) {
            if (staticallyTrue) {
              conditions += Expr.quote {
                Expr.splice(fieldExpr).asInstanceOf[String].isEmpty
              }
            } else {
              conditions += Expr.quote {
                Expr.splice(configExpr).transientEmpty &&
                Expr.splice(fieldExpr).asInstanceOf[String].isEmpty
              }
            }
          } else {
            if (staticallyTrue) {
              conditions += Expr.quote {
                Expr.splice(fieldExpr).asInstanceOf[Iterable[Any]].isEmpty
              }
            } else {
              conditions += Expr.quote {
                Expr.splice(configExpr).transientEmpty &&
                Expr.splice(fieldExpr).asInstanceOf[Iterable[Any]].isEmpty
              }
            }
          }
        }
      }

      conditions.result()
    }
  }

}
