package hearth.kindlings.xmlderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.data.NonEmptyList
import hearth.fp.effect.*
import hearth.fp.syntax.*
import hearth.std.*

import hearth.kindlings.xmlderivation.XmlConfig
import hearth.kindlings.xmlderivation.annotations.xmlName
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

    @scala.annotation.nowarn("msg=is never used|dead code|Non local returns")
    private def encodeCaseClassFields[A: EncoderCtx](
        caseClass: CaseClass[A]
    ): MIO[Expr[scala.xml.Elem]] = {
      implicit val ElemT: Type[scala.xml.Elem] = Types.Elem
      implicit val StringT: Type[String] = Types.String
      implicit val NodeT: Type[scala.xml.Node] = Types.Node
      implicit val XmlConfigT: Type[XmlConfig] = Types.XmlConfig
      implicit val AnyType: Type[Any] = Types.Any
      implicit val BoolType: Type[Boolean] = Types.Boolean
      implicit val UnitT: Type[Unit] = Types.Unit
      implicit val NodeBufferT: Type[scala.collection.mutable.ListBuffer[scala.xml.Node]] = Types.NodeBuffer
      implicit val xmlNameT: Type[xmlName] = Types.XmlNameAnnotation

      val allFields = caseClass.caseFieldValuesAt(ectx.value).toList
      val paramsByName: Map[String, Parameter] =
        if (allFields.isEmpty) Map.empty
        else caseClass.primaryConstructor.totalParameters.flatten.toMap

      // Check if we can skip transient checks entirely (statically known config)
      val noTransientChecksNeeded: Boolean = ectx.evaluatedConfig.exists { c =>
        !c.transientNone && !c.transientEmpty
      }

      // Where each field goes (shared with the decoder), or what is wrong with the annotations
      val annotatedFields = allFields.flatMap { case (fName, _) => paramsByName.get(fName).map(fName -> _) }
      val layoutsOrErrors = xmlFieldLayouts(
        Type[A].prettyPrint,
        annotatedFields,
        ectx.evaluatedConfig,
        EncoderUseImplicitWhenAvailableRule.hasUserEncoder(ectx.derivedType)
      )

      layoutsOrErrors match {
        case Left(errors) =>
          val err = EncoderDerivationError.InvalidFieldAnnotations(Type[A].prettyPrint, errors)
          Log.error(err.message) >> MIO.fail(err)
        case Right(layouts) =>
          val layoutByName: Map[String, XmlFieldLayout] = annotatedFields.map(_._1).zip(layouts).toMap
          NonEmptyList.fromList(allFields) match {
            case Some(fieldValues) =>
              fieldValues
                .traverse { case (fName, fieldValue) =>
                  import fieldValue.{Underlying as Field, value as fieldExpr}
                  val paramOpt = paramsByName.get(fName)
                  val customName = paramOpt.flatMap(p => getAnnotationStringArg[xmlName](p))
                  val layout = layoutByName.getOrElse(fName, XmlFieldLayout.Element)

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
                    if (noTransientChecksNeeded || layout == XmlFieldLayout.Transient) Nil
                    else buildFieldSkipConditions[Field](fieldExpr, ectx.evaluatedConfig, ectx.config)

                  def asElement: MIO[Expr[scala.xml.Elem]] =
                    deriveEncoderRecursively[Field](using ectx.nest(fieldExpr).copy(elementName = xmlFieldNameExpr))

                  layout match {
                    case XmlFieldLayout.Transient      => MIO.pure(FieldEncoding.Skip: FieldEncoding)
                    case XmlFieldLayout.Content(shape) =>
                      contentEncoding[A, Field](shape, fieldExpr).map(append => FieldEncoding.Content(append))
                    case XmlFieldLayout.Attribute(shape) =>
                      attributeEncoding[A, Field](shape, xmlFieldNameExpr, fieldExpr, skipConditions)
                        .map(attr => attr: FieldEncoding)
                    case XmlFieldLayout.Element =>
                      asElement.map(encodedExpr => FieldEncoding.Child(encodedExpr, skipConditions))
                    case XmlFieldLayout.ByMode(shape, needsEnumAsStrings) =>
                      // Not statically known — derive both paths and branch at runtime
                      for {
                        attr <- attributeEncoding[A, Field](shape, xmlFieldNameExpr, fieldExpr, skipConditions)
                        encodedExpr <- asElement
                      } yield FieldEncoding.DynamicMode(attr, encodedExpr, skipConditions, needsEnumAsStrings)
                    case XmlFieldLayout.Repeated =>
                      repeatedEncoding[A, Field](xmlFieldNameExpr, fieldExpr)
                        .map(append => FieldEncoding.Repeated(append))
                    case XmlFieldLayout.Wrapped(wrapper, true) =>
                      repeatedEncoding[A, Field](xmlFieldNameExpr, fieldExpr)
                        .map(append => FieldEncoding.WrappedRepeated(wrapper, append, skipConditions))
                    case XmlFieldLayout.Wrapped(wrapper, false) =>
                      asElement.map(encodedExpr => FieldEncoding.WrappedChild(wrapper, encodedExpr, skipConditions))
                  }
                }
                .map { encodings =>
                  val encodingList = encodings.toList
                  val hasDynamicFields = encodingList.exists(_.isInstanceOf[FieldEncoding.DynamicMode])
                  assembleElem(encodingList, hasDynamicFields)
                }
            case None =>
              MIO.pure(Expr.quote {
                XmlDerivationUtils.makeEmptyElem(Expr.splice(ectx.elementName))
              })
          }
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
      * (a field's code does not contain the code of the next fields, so it stays linear in the number of fields; the
      * larger statements - a field whose mode is only known at runtime, `@xmlUnwrapped` and `@xmlWrapper` fields - go
      * into local defs of their own)
      *
      * With an `@xmlContent` field the element holds the attributes and the content (the other fields are attributes).
      */
    @scala.annotation.nowarn("msg=is never used")
    private def assembleElem[A: EncoderCtx](
        encodings: List[FieldEncoding],
        hasDynamicFields: Boolean
    ): Expr[scala.xml.Elem] = {
      implicit val ElemT: Type[scala.xml.Elem] = Types.Elem
      implicit val NodeT: Type[scala.xml.Node] = Types.Node
      implicit val StringT: Type[String] = Types.String
      implicit val AnyT: Type[Any] = Types.Any
      implicit val BoolT: Type[Boolean] = Types.Boolean
      implicit val UnitT: Type[Unit] = Types.Unit
      implicit val XmlConfigT: Type[XmlConfig] = Types.XmlConfig
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
                  val _ = Expr.splice(attrs) += ((Expr.splice(attr.nameExpr), Expr.splice(attr.text)))
                  ()
                }
              )
              def addChild(elem: Expr[scala.xml.Elem], skipConditions: List[Expr[Boolean]]): Expr[Unit] =
                skipped(
                  skipConditions,
                  Expr.quote {
                    val _ = Expr.splice(children) += (Expr.splice(elem): scala.xml.Node)
                    ()
                  }
                )
              // the wrapper element with what `items` appends to a buffer of its own
              def addWrapped(
                  wrapper: String,
                  items: Expr[scala.collection.mutable.ListBuffer[scala.xml.Node]] => Expr[Unit],
                  skipConditions: List[Expr[Boolean]]
              ): Expr[Unit] = skipped(
                skipConditions,
                ValDefs
                  .createVal[scala.collection.mutable.ListBuffer[scala.xml.Node]](
                    Expr.quote(new scala.collection.mutable.ListBuffer[scala.xml.Node]),
                    FreshName.FromPrefix("wrapped")
                  )
                  .use { wrapped =>
                    Expr.quote {
                      Expr.splice(items(wrapped))
                      val _ = Expr.splice(children) += (XmlDerivationUtils
                        .makeElem(Expr.splice(Expr(wrapper)), Nil, Expr.splice(wrapped).toList): scala.xml.Node)
                      ()
                    }
                  }
              )

              // each statement with whether it goes into a local def of its own (the larger ones)
              def build(isAttrMode: Option[Expr[Boolean]]): Expr[scala.xml.Elem] = {
                val statements: List[(Expr[Unit], Boolean)] = encodings.collect {
                  case attr: FieldEncoding.AttrExpr                   => (addAttr(attr), false)
                  case FieldEncoding.Child(elem, skipConds)           => (addChild(elem, skipConds), false)
                  case FieldEncoding.Content(append)                  => (append(children), false)
                  case FieldEncoding.Repeated(append)                 => (append(children), true)
                  case FieldEncoding.WrappedRepeated(w, append, sks)  => (addWrapped(w, append, sks), true)
                  case FieldEncoding.WrappedChild(w, elem, skipConds) =>
                    val single: Expr[scala.collection.mutable.ListBuffer[scala.xml.Node]] => Expr[Unit] = buffer =>
                      Expr.quote {
                        val _ = Expr.splice(buffer) += (Expr.splice(elem): scala.xml.Node)
                        ()
                      }
                    (addWrapped(w, single, skipConds), true)
                  case FieldEncoding.DynamicMode(attr, elem, skipConds, needsEnumAsStrings) =>
                    val asAttr = addAttr(attr)
                    val asChild = addChild(elem, skipConds)
                    val isAttribute: Expr[Boolean] =
                      if (needsEnumAsStrings)
                        Expr.quote(Expr.splice(isAttrMode.get) && Expr.splice(ectx.config).enumAsStrings)
                      else isAttrMode.get
                    (Expr.quote(if (Expr.splice(isAttribute)) Expr.splice(asAttr) else Expr.splice(asChild)), true)
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
                Expr.quote {
                  Expr.splice(all)
                  XmlDerivationUtils.combineAttributesAndChildren(
                    Expr.splice(ectx.elementName),
                    Expr.splice(attrs).toList,
                    Expr.splice(children).toList
                  )
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

    /** The text of a single value: its text, the name of its enum case, or the text of the element written by its
      * user-provided encoder.
      */
    @scala.annotation.nowarn("msg=is never used")
    private def textOf[A: EncoderCtx, T: Type](shape: XmlTextShape, value: Expr[T]): MIO[Expr[String]] = {
      implicit val StringT: Type[String] = Types.String
      implicit val ElemT: Type[scala.xml.Elem] = Types.Elem
      shape match {
        case XmlTextShape.BuiltIn              => MIO.pure(xmlTextOf(Type[T], value))
        case XmlTextShape.StringEnum(subtypes) =>
          EncoderHandleAsEnumRule.enumNameOf[T](subtypes)(using ectx.nest(value)).map(_(value, ectx.config))
        case XmlTextShape.Custom =>
          deriveEncoderRecursively[T](using ectx.nest(value).copy(elementName = Expr("attr"))).map { elem =>
            Expr.quote(Expr.splice(elem).text)
          }
      }
    }

    /** A field encoded as an attribute. An `Option` attribute is absent for `None` (how the decoder reads an absent
      * `Option` attribute) and holds the value for `Some` (not `Some(...)`).
      */
    @scala.annotation.nowarn("msg=is never used")
    private def attributeEncoding[A: EncoderCtx, Field: Type](
        shape: XmlAttrShape,
        nameExpr: Expr[String],
        fieldExpr: Expr[Field],
        skipConditions: List[Expr[Boolean]]
    ): MIO[FieldEncoding.AttrExpr] = {
      implicit val AnyT: Type[Any] = Types.Any
      implicit val BoolT: Type[Boolean] = Types.Boolean
      Type[Field] match {
        case IsOption(isOption) if shape.optional =>
          import isOption.Underlying as Inner
          val option: Expr[Option[Any]] = Expr.quote(Expr.splice(fieldExpr).asInstanceOf[Option[Any]])
          val inner: Expr[Inner] = Expr.quote(Expr.splice(option).get.asInstanceOf[Inner]) // (`None`: skipped)
          textOf[A, Inner](shape.shape, inner).map { text =>
            FieldEncoding.AttrExpr(nameExpr, text, skipConditions :+ Expr.quote(Expr.splice(option).isEmpty))
          }
        case _ =>
          textOf[A, Field](shape.shape, fieldExpr).map(text => FieldEncoding.AttrExpr(nameExpr, text, skipConditions))
      }
    }

    /** The `@xmlContent` field: appends the element's content (nothing for `None`). */
    @scala.annotation.nowarn("msg=is never used")
    private def contentEncoding[A: EncoderCtx, Field: Type](
        shape: XmlAttrShape,
        fieldExpr: Expr[Field]
    ): MIO[Expr[scala.collection.mutable.ListBuffer[scala.xml.Node]] => Expr[Unit]] = {
      implicit val AnyT: Type[Any] = Types.Any
      implicit val UnitT: Type[Unit] = Types.Unit
      implicit val StringT: Type[String] = Types.String
      implicit val ElemT: Type[scala.xml.Elem] = Types.Elem
      implicit val NodeBufferT: Type[scala.collection.mutable.ListBuffer[scala.xml.Node]] = Types.NodeBuffer
      type Append = Expr[scala.collection.mutable.ListBuffer[scala.xml.Node]] => Expr[Unit]
      def contentOf[T: Type](value: Expr[T]): MIO[Append] = shape.shape match {
        case XmlTextShape.Custom =>
          // whatever the user-provided encoder writes into its element
          deriveEncoderRecursively[T](using ectx.nest(value)).map { elem =>
            val append: Append = buffer =>
              Expr.quote {
                val _ = Expr.splice(buffer) ++= Expr.splice(elem).child
                ()
              }
            append
          }
        case textShape =>
          textOf[A, T](textShape, value).map { text =>
            val append: Append = buffer =>
              Expr.quote {
                val _ = Expr.splice(buffer) += (scala.xml.Text(Expr.splice(text)): scala.xml.Node)
                ()
              }
            append
          }
      }
      Type[Field] match {
        case IsOption(isOption) if shape.optional =>
          import isOption.Underlying as Inner
          val option: Expr[Option[Any]] = Expr.quote(Expr.splice(fieldExpr).asInstanceOf[Option[Any]])
          val inner: Expr[Inner] = Expr.quote(Expr.splice(option).get.asInstanceOf[Inner])
          contentOf[Inner](inner).map { append =>
            val optional: Append =
              buffer => Expr.quote(if (Expr.splice(option).isEmpty) () else Expr.splice(append(buffer)))
            optional
          }
        case _ => contentOf[Field](fieldExpr)
      }
    }

    /** A collection (or map) field as one element per item (per entry), all named after the field: appended in order to
      * a buffer (the children of the element, or of the `@xmlWrapper` element).
      */
    @scala.annotation.nowarn("msg=is never used")
    private def repeatedEncoding[A: EncoderCtx, Field: Type](
        nameExpr: Expr[String],
        fieldExpr: Expr[Field]
    ): MIO[Expr[scala.collection.mutable.ListBuffer[scala.xml.Node]] => Expr[Unit]] = {
      implicit val UnitT: Type[Unit] = Types.Unit
      implicit val StringT: Type[String] = Types.String
      implicit val ElemT: Type[scala.xml.Elem] = Types.Elem
      implicit val NodeT: Type[scala.xml.Node] = Types.Node
      implicit val XmlConfigT: Type[XmlConfig] = Types.XmlConfig
      implicit val NodeBufferT: Type[scala.collection.mutable.ListBuffer[scala.xml.Node]] = Types.NodeBuffer
      type Append = Expr[scala.collection.mutable.ListBuffer[scala.xml.Node]] => Expr[Unit]
      // the cached encode helper of the items' type: (item, element name, config) => element
      def helperOf[T: Type]: MIO[(Expr[T], Expr[String], Expr[XmlConfig]) => Expr[scala.xml.Elem]] =
        deriveEncoderRecursively[T](using ectx.nest[T](Expr.quote(null.asInstanceOf[T]))) >>
          ectx.getHelper[T].flatMap {
            case Some(helper) => MIO.pure(helper)
            case None         => MIO.fail(new RuntimeException(s"No encode helper for ${Type[T].prettyPrint}"))
          }
      val configExpr = ectx.config
      Type[Field] match {
        case IsCollection(isCollection) =>
          import isCollection.Underlying as Item
          isCollection.value.asMap match {
            case Some(isMap) =>
              import isMap.Value
              val iterableExpr = isMap.asIterable(fieldExpr)
              helperOf[Value].map { helper =>
                val append: Append = buffer =>
                  Expr.quote {
                    val _ = Expr.splice(buffer) ++= XmlDerivationUtils.encodeMappedPairs[Value](
                      Expr.splice(iterableExpr).asInstanceOf[Iterable[(String, Value)]],
                      Expr.splice(nameExpr),
                      "key",
                      (v: Value, n: String) => Expr.splice(helper(Expr.quote(v), Expr.quote(n), configExpr))
                    )
                    ()
                  }
                append
              }
            case None =>
              val iterableExpr = isCollection.value.asIterable(fieldExpr)
              helperOf[Item].map { helper =>
                val append: Append = buffer =>
                  Expr.quote {
                    val itemName = Expr.splice(nameExpr)
                    val items = Expr.splice(iterableExpr).iterator
                    while (items.hasNext) {
                      val item = items.next()
                      val _ = Expr.splice(buffer) +=
                        (Expr.splice(helper(Expr.quote(item), Expr.quote(itemName), configExpr)): scala.xml.Node)
                    }
                  }
                append
              }
          }
        case _ =>
          MIO.fail(new RuntimeException(s"Expected a collection or a map, got ${Type[Field].prettyPrint}"))
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
