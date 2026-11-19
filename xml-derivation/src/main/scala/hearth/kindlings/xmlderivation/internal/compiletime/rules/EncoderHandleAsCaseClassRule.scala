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
                MIO.pure(attributeEncoding[Field](xmlFieldNameExpr, fieldExpr, skipConditions))
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
              } else if (!isXmlAttributeValue(Type[Field])) {
                // Not a single text value (a collection, map, case class, sealed trait...): a child element in both
                // modes, as an attribute could not hold it
                deriveEncoderRecursively[Field](using ectx.nest(fieldExpr).copy(elementName = xmlFieldNameExpr))
                  .map(encodedExpr => FieldEncoding.Child(encodedExpr, skipConditions))
              } else {
                // Default mode from config
                defaultFieldModeIsAttribute match {
                  case Some(true) =>
                    // Statically known to be attribute mode
                    MIO.pure(attributeEncoding[Field](xmlFieldNameExpr, fieldExpr, skipConditions))
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
                        attributeEncoding[Field](xmlFieldNameExpr, fieldExpr, skipConditions),
                        encodedExpr,
                        skipConditions
                      )
                    }
                }
              }
            }
            .map { encodings =>
              val encodingList = encodings.toList
              val contentOpt: Option[Expr[scala.xml.Elem]] = encodingList.collectFirst {
                case FieldEncoding.Content(expr) => expr
              }
              val hasDynamicFields = encodingList.exists(_.isInstanceOf[FieldEncoding.DynamicMode])
              assembleElem(encodingList, contentOpt, hasDynamicFields)
            }
        case None =>
          MIO.pure(Expr.quote {
            XmlDerivationUtils.makeEmptyElem(Expr.splice(ectx.elementName))
          })
      }
    }

    /** The element of a case class, as flat code: one statement per field, appending to buffers of attributes and child
      * elements in field order, then one call building the element:
      * {{{
      *   val attrs = new ListBuffer[(String, String)]
      *   val children = new ListBuffer[Node]
      *   val isAttrMode = ... // only when `defaultFieldMode` is known only at runtime
      *   attrs += (("a", a.toString))
      *   if (!<skip b>) children += <b element>
      *   if (isAttrMode) { if (!<skip c>) attrs += (("c", <c text>)) } else { if (!<skip c>) children += <c element> }
      *   combineAttributesAndChildren(name, attrs.toList, children.toList)
      * }}}
      * (a field's code does not contain the code of the next fields, so it stays linear in the number of fields)
      *
      * With an `@xmlContent` field the element holds the attributes and the content (the children of the content's
      * element): the other fields are not written as child elements.
      */
    @scala.annotation.nowarn("msg=is never used")
    private def assembleElem[A: EncoderCtx](
        encodings: List[FieldEncoding],
        contentOpt: Option[Expr[scala.xml.Elem]],
        hasDynamicFields: Boolean
    ): Expr[scala.xml.Elem] = {
      implicit val ElemT: Type[scala.xml.Elem] = Types.Elem
      implicit val NodeT: Type[scala.xml.Node] = Types.Node
      implicit val StringT: Type[String] = Types.String
      implicit val AnyT: Type[Any] = Types.Any
      implicit val BoolT: Type[Boolean] = Types.Boolean
      implicit val UnitT: Type[Unit] = Types.Unit
      implicit val AttrBufferT: Type[scala.collection.mutable.ListBuffer[(String, String)]] = Types.AttrBuffer
      implicit val NodeBufferT: Type[scala.collection.mutable.ListBuffer[scala.xml.Node]] = Types.NodeBuffer

      def skipped(skipConditions: List[Expr[Boolean]], statement: Expr[Unit]): Expr[Unit] =
        skipConditions.reduceOption((a, b) => Expr.quote(Expr.splice(a) || Expr.splice(b))) match {
          case Some(skip) => Expr.quote(if (Expr.splice(skip)) () else Expr.splice(statement))
          case None       => statement
        }

      ValDefs
        .createVal[scala.collection.mutable.ListBuffer[(String, String)]](
          Expr.quote(new scala.collection.mutable.ListBuffer[(String, String)]),
          FreshName.FromPrefix("attrs")
        )
        .use { attrs =>
          ValDefs
            .createVal[scala.collection.mutable.ListBuffer[scala.xml.Node]](
              Expr.quote(new scala.collection.mutable.ListBuffer[scala.xml.Node]),
              FreshName.FromPrefix("children")
            )
            .use { children =>
              def addAttr(attr: FieldEncoding.AttrExpr): Expr[Unit] = skipped(
                attr.skipConditions,
                Expr.quote {
                  val _ = Expr.splice(attrs) += ((Expr.splice(attr.nameExpr), Expr.splice(attr.value).toString))
                  ()
                }
              )
              def addChild(elem: Expr[scala.xml.Elem], skipConditions: List[Expr[Boolean]]): Expr[Unit] =
                if (contentOpt.isDefined) Expr.quote(()) // (only the content is written)
                else
                  skipped(
                    skipConditions,
                    Expr.quote {
                      val _ = Expr.splice(children) += (Expr.splice(elem): scala.xml.Node)
                      ()
                    }
                  )

              // each statement with whether it goes into a local def of its own (the larger, runtime-mode ones)
              def build(isAttrMode: Option[Expr[Boolean]]): Expr[scala.xml.Elem] = {
                val statements: List[(Expr[Unit], Boolean)] = encodings.collect {
                  case FieldEncoding.Attr(name, value, skipConds) =>
                    (addAttr(FieldEncoding.AttrExpr(Expr(name), value, skipConds)), false)
                  case attr: FieldEncoding.AttrExpr                     => (addAttr(attr), false)
                  case FieldEncoding.Child(elem, skipConds)             => (addChild(elem, skipConds), false)
                  case FieldEncoding.WrappedChild(_, elem, skipConds)   => (addChild(elem, skipConds), false)
                  case FieldEncoding.DynamicMode(attr, elem, skipConds) =>
                    val asAttr = addAttr(attr)
                    val asChild = addChild(elem, skipConds)
                    (Expr.quote(if (Expr.splice(isAttrMode.get)) Expr.splice(asAttr) else Expr.splice(asChild)), true)
                }
                def sequence(remaining: List[(Expr[Unit], Boolean)], acc: Expr[Unit]): Expr[scala.xml.Elem] =
                  remaining match {
                    case (statement, true) :: rest =>
                      // (a `Boolean` result: Scala 2 lints a nullary def returning `Unit`)
                      val body: Expr[Boolean] = Expr.quote {
                        Expr.splice(statement)
                        true
                      }
                      ValDefs.createDef[Boolean](body, FreshName.FromPrefix("field")).use { fieldDef =>
                        sequence(
                          rest,
                          Expr.quote {
                            Expr.splice(acc)
                            val _ = Expr.splice(fieldDef)
                          }
                        )
                      }
                    case (statement, false) :: rest =>
                      sequence(
                        rest,
                        Expr.quote {
                          Expr.splice(acc)
                          Expr.splice(statement)
                        }
                      )
                    case Nil => finish(acc)
                  }
                sequence(statements, Expr.quote(()))
              }

              def finish(all: Expr[Unit]): Expr[scala.xml.Elem] =
                contentOpt match {
                  case Some(contentExpr) =>
                    Expr.quote {
                      Expr.splice(all)
                      val contentElem = Expr.splice(contentExpr)
                      val _ = Expr.splice(children) ++= contentElem.child
                      XmlDerivationUtils.combineAttributesAndChildren(
                        Expr.splice(ectx.elementName),
                        Expr.splice(attrs).toList,
                        Expr.splice(children).toList
                      )
                    }
                  case None =>
                    Expr.quote {
                      Expr.splice(all)
                      XmlDerivationUtils.combineAttributesAndChildren(
                        Expr.splice(ectx.elementName),
                        Expr.splice(attrs).toList,
                        Expr.splice(children).toList
                      )
                    }
                }

              if (hasDynamicFields)
                ValDefs
                  .createVal[Boolean](
                    Expr.quote(XmlDerivationUtils.isAttributeMode(Expr.splice(ectx.config).defaultFieldMode)),
                    FreshName.FromPrefix("isAttrMode")
                  )
                  .use(isAttrMode => build(Some(isAttrMode)))
              else build(None)
            }
        }
    }

    /** A field encoded as an attribute. An `Option` attribute is absent for `None` (how the decoder reads an absent
      * `Option` attribute) and holds the value for `Some` (not `Some(...)`).
      */
    @scala.annotation.nowarn("msg=is never used")
    private def attributeEncoding[Field: Type](
        nameExpr: Expr[String],
        fieldExpr: Expr[Field],
        skipConditions: List[Expr[Boolean]]
    )(implicit AnyT: Type[Any], BoolT: Type[Boolean]): FieldEncoding.AttrExpr = {
      implicit val StringT: Type[String] = Types.String
      Type[Field] match {
        // a text value (also inside an `Option`) is written as its text: a value type as its unwrapped value
        case IsOption(isOption) if isXmlTextValue(isOption.Underlying) =>
          import isOption.Underlying as Inner
          val option: Expr[Option[Any]] = Expr.quote(Expr.splice(fieldExpr).asInstanceOf[Option[Any]])
          val inner: Expr[Inner] = Expr.quote(Expr.splice(option).get.asInstanceOf[Inner]) // (`None`: skipped)
          FieldEncoding.AttrExpr(
            nameExpr,
            xmlTextOf(Type[Inner], inner).upcast[Any],
            skipConditions :+ Expr.quote(Expr.splice(option).isEmpty)
          )
        case _ if isXmlTextValue(Type[Field]) =>
          FieldEncoding.AttrExpr(nameExpr, xmlTextOf(Type[Field], fieldExpr).upcast[Any], skipConditions)
        case IsOption(_) =>
          val option: Expr[Option[Any]] = Expr.quote(Expr.splice(fieldExpr).asInstanceOf[Option[Any]])
          FieldEncoding.AttrExpr(
            nameExpr,
            Expr.quote(Expr.splice(option).getOrElse(())), // (not evaluated for `None`: skipped)
            skipConditions :+ Expr.quote(Expr.splice(option).isEmpty)
          )
        case _ => FieldEncoding.AttrExpr(nameExpr, fieldExpr.upcast[Any], skipConditions)
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
      // (`String` is an `Iterable[Char]` to `IsCollection`: not a collection here)
      val isCollectionField = !isOptionField && !(Type[Field] =:= Types.String) && {
        val isMap = Type[Field] match { case IsMap(_) => true; case _ => false }
        val isColl = Type[Field] match { case IsCollection(_) => true; case _ => false }
        isMap || isColl
      }

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

      // transientEmpty: skip empty collections (an empty `String` is still written: it is not an absent value)
      if (isCollectionField) {
        val staticallyFalse = evaluatedConfig.exists(!_.transientEmpty)
        if (!staticallyFalse) {
          val staticallyTrue = evaluatedConfig.exists(_.transientEmpty)
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

      conditions.result()
    }
  }

}
