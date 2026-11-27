package hearth.kindlings.xmlderivation.internal.compiletime

import hearth.MacroCommons
import hearth.std.*

import hearth.kindlings.xmlderivation.{XmlConfig, XmlFieldMode}
import hearth.kindlings.xmlderivation.annotations.{
  transientField,
  xmlAttribute,
  xmlContent,
  xmlElement,
  xmlName,
  xmlUnwrapped,
  xmlWrapper
}
import hearth.kindlings.derivation.compiletime.StableChildrenOrder

/** Where each field of a case class goes in its element (attribute, child element, text content, repeated child
  * elements, inside a wrapper element) and the subtypes of a sealed hierarchy, decided once for the encoder and the
  * decoder, so that the decoder reads what the encoder writes.
  */
trait XmlFieldLayouts extends XmlTextValues { this: MacroCommons & StdExtensions & AnnotationSupport =>

  // --- Subtypes of sealed traits / enums ---

  /** A leaf subtype: its name in the source, the name set with `@xmlName` (used as it is, not transformed by
    * `constructorNameMapper`), and its type.
    */
  final case class XmlSubtype(rawName: String, customName: Option[String], tpe: ??) {

    /** The name with a configuration known at compile time. */
    def nameWith(config: XmlConfig): String = customName.getOrElse(config.constructorNameMapper(rawName))
  }

  private lazy val XmlNameAnnotationType: Type[xmlName] = Type.of[xmlName]

  /** Whether `C` is a subtype that has subtypes of its own to flatten: a sealed type that is not a singleton and has no
    * user-provided instance.
    */
  def isNestedXmlSum[C: Type](hasUserInstance: ?? => Boolean): Boolean =
    SingletonValue.unapply(Type[C]).isEmpty && !Type[C].isJavaEnum && !Type[C].isEnumeration &&
      Enum.parse[C].toOption.isDefined && !hasUserInstance(Type[C].as_??)

  /** The leaves of a sealed hierarchy, in the build-independent order: nested sealed subtypes are flattened (their
    * leaves are written with their own names), unless they have a user-provided instance.
    */
  def xmlSubtypesOf[A: Type](enumm: Enum[A])(hasUserInstance: ?? => Boolean): List[XmlSubtype] = {
    implicit val xmlNameT: Type[xmlName] = XmlNameAnnotationType
    def leaves[B: Type](enumB: Enum[B], depth: Int): List[XmlSubtype] =
      StableChildrenOrder.directChildren(XmlFieldLayouts.this)(enumB).flatMap { case (name, child) =>
        import child.Underlying as C
        val nested: Option[Enum[C]] =
          if (depth < 32 && isNestedXmlSum[C](hasUserInstance)) Enum.parse[C].toOption else None
        nested match {
          case Some(enumC) => leaves[C](enumC, depth + 1)
          case None        => List(XmlSubtype(name, getTypeAnnotationStringArg[xmlName, C], Type[C].as_??))
        }
      }
    leaves[A](enumm, 0)
  }

  /** The cases of `A` when it is an enum whose every case is a singleton (case objects, parameterless Scala 3 enum
    * cases, Java enum values, `Enumeration` values): what `enumAsStrings` writes as text, and what can be an attribute.
    */
  def xmlStringEnumOf[A](tpe: Type[A], hasUserInstance: ?? => Boolean): Option[List[XmlSubtype]] = {
    implicit val A: Type[A] = tpe
    if (isXmlTextValue(tpe) || SingletonValue.unapply(tpe).isDefined) None
    else
      Enum.parse[A].toOption.flatMap { enumm =>
        val subtypes = xmlSubtypesOf[A](enumm)(hasUserInstance)
        val allSingletons = Type[A].isJavaEnum || Type[A].isEnumeration || subtypes.forall { subtype =>
          SingletonValue.unapply(subtype.tpe.Underlying).isDefined
        }
        if (subtypes.nonEmpty && allSingletons) Some(subtypes) else None
      }
  }

  // --- Single values as text ---

  /** How a single value is written as text (an attribute value, or the text content of an element). */
  sealed trait XmlTextShape
  object XmlTextShape {

    /** A built-in text value or a value type over one: its text. */
    case object BuiltIn extends XmlTextShape

    /** An enum of singletons: the case name. */
    final case class StringEnum(subtypes: List[XmlSubtype]) extends XmlTextShape

    /** A type with a user-provided instance: the text of the element that instance writes/reads. */
    case object Custom extends XmlTextShape
  }

  /** A text shape, of the type itself or of the value of an `Option`. */
  final case class XmlAttrShape(shape: XmlTextShape, optional: Boolean)

  // (a value type over a text value may have a user-provided instance, which is then used)
  private def isBuiltInText[A](tpe: Type[A]): Boolean = isXmlBuiltInTextValue(tpe)

  private def textShapeOf[A](tpe: Type[A], hasUserInstance: ?? => Boolean): Option[XmlTextShape] = {
    implicit val A: Type[A] = tpe
    if (isBuiltInText(tpe)) Some(XmlTextShape.BuiltIn)
    else if (hasUserInstance(Type[A].as_??)) Some(XmlTextShape.Custom)
    else if (isXmlTextValue(tpe)) Some(XmlTextShape.BuiltIn)
    else xmlStringEnumOf(tpe, hasUserInstance).map(XmlTextShape.StringEnum(_))
  }

  /** How a field of type `A` is written as an attribute (or as text content), if it can be: a text value, an enum of
    * singletons, a type with a user-provided instance, or an `Option` of one of these.
    */
  def xmlAttrShapeOf[A](tpe: Type[A], hasUserInstance: ?? => Boolean): Option[XmlAttrShape] = {
    implicit val A: Type[A] = tpe
    if (isBuiltInText(tpe)) Some(XmlAttrShape(XmlTextShape.BuiltIn, optional = false))
    else
      Type[A] match {
        case IsOption(isOption) =>
          import isOption.Underlying as Inner
          textShapeOf(Type[Inner], hasUserInstance).map(XmlAttrShape(_, optional = true))
        case _ => textShapeOf(tpe, hasUserInstance).map(XmlAttrShape(_, optional = false))
      }
  }

  /** [[xmlAttrShapeOf]] for a field without an annotation, which `defaultFieldMode` may make an attribute: only text
    * values and enums of singletons (a type with a user-provided instance stays a child element). Looks for a
    * user-provided instance only for those, so that the other fields (case classes, collections...) cost no implicit
    * search here.
    */
  def xmlDefaultAttrShapeOf[A](tpe: Type[A], hasUserInstance: ?? => Boolean): Option[XmlAttrShape] = {
    def textLike[T](inner: Type[T]): Option[XmlTextShape] = {
      implicit val T: Type[T] = inner
      if (isBuiltInText(inner)) Some(XmlTextShape.BuiltIn)
      else {
        val candidate: Option[XmlTextShape] =
          if (isXmlTextValue(inner)) Some(XmlTextShape.BuiltIn)
          else xmlStringEnumOf(inner, hasUserInstance).map(XmlTextShape.StringEnum(_))
        candidate.filterNot(_ => hasUserInstance(Type[T].as_??))
      }
    }
    implicit val A: Type[A] = tpe
    if (isBuiltInText(tpe)) Some(XmlAttrShape(XmlTextShape.BuiltIn, optional = false))
    else
      Type[A] match {
        case IsOption(isOption) =>
          import isOption.Underlying as Inner
          textLike(Type[Inner]).map(XmlAttrShape(_, optional = true))
        case _ => textLike(tpe).map(XmlAttrShape(_, optional = false))
      }
  }

  // --- Fields of case classes ---

  sealed trait XmlFieldLayout
  object XmlFieldLayout {

    /** `@transientField`: not written, its default value when read. */
    case object Transient extends XmlFieldLayout

    /** `@xmlContent`: the text content of the element. */
    final case class Content(shape: XmlAttrShape) extends XmlFieldLayout

    /** An attribute. */
    final case class Attribute(shape: XmlAttrShape) extends XmlFieldLayout

    /** A child element named after the field. */
    case object Element extends XmlFieldLayout

    /** The configuration is only known at runtime: an attribute when `defaultFieldMode` is `Attribute` (and, for an
      * enum of singletons, `enumAsStrings` is set), a child element otherwise.
      */
    final case class ByMode(shape: XmlAttrShape, needsEnumAsStrings: Boolean) extends XmlFieldLayout

    /** `@xmlUnwrapped`: one child element per item of the collection (per entry of the map), named after the field. */
    case object Repeated extends XmlFieldLayout

    /** `@xmlWrapper(name)`: inside an element `name` - a collection/map as with `@xmlUnwrapped` (`repeated`), any other
      * field as its child element.
      */
    final case class Wrapped(wrapper: String, repeated: Boolean) extends XmlFieldLayout
  }

  private lazy val TransientFieldType: Type[transientField] = Type.of[transientField]
  private lazy val XmlAttributeType: Type[xmlAttribute] = Type.of[xmlAttribute]
  private lazy val XmlElementType: Type[xmlElement] = Type.of[xmlElement]
  private lazy val XmlContentType: Type[xmlContent] = Type.of[xmlContent]
  private lazy val XmlWrapperType: Type[xmlWrapper] = Type.of[xmlWrapper]
  private lazy val XmlUnwrappedType: Type[xmlUnwrapped] = Type.of[xmlUnwrapped]

  private val attributeTypes =
    "String, numbers, Boolean, Char, BigDecimal, BigInt, value types over them, enums of case objects, types with an " +
      "implicit instance in scope (the text of their element is used), and Options of these"

  /** Whether `A` is a collection or a map with `String` keys (not a `String`, not an `Option`): `Right(true)`; a map
    * with other keys is `Left`.
    */
  private def repeatableField[A](tpe: Type[A]): Either[String, Boolean] = {
    implicit val A: Type[A] = tpe
    if (Type[A] <:< Type.of[String]) Right(false)
    else
      Type[A] match {
        case IsOption(_)                => Right(false)
        case IsCollection(isCollection) =>
          isCollection.value.asMap match {
            case Some(isMap) =>
              import isMap.Key
              if (Key <:< Type.of[String]) Right(true)
              else Left(s"a map with ${Key.prettyPrint} keys is not supported (only String keys)")
            case None => Right(true)
          }
        case _ => Right(false)
      }
  }

  /** The layout of every field (in order), or the errors in how the fields are annotated.
    *
    * @param hasUserInstance
    *   whether a type has a user-provided instance of the type class being derived (`XmlEncoder` / `XmlDecoder`)
    */
  def xmlFieldLayouts(
      owner: String,
      fields: List[(String, Parameter)],
      evaluatedConfig: Option[XmlConfig],
      hasUserInstance: ?? => Boolean
  ): Either[List[String], List[XmlFieldLayout]] = {
    implicit val transientFieldT: Type[transientField] = TransientFieldType
    implicit val xmlAttributeT: Type[xmlAttribute] = XmlAttributeType
    implicit val xmlElementT: Type[xmlElement] = XmlElementType
    implicit val xmlContentT: Type[xmlContent] = XmlContentType
    implicit val xmlWrapperT: Type[xmlWrapper] = XmlWrapperType
    implicit val xmlUnwrappedT: Type[xmlUnwrapped] = XmlUnwrappedType

    val errors = List.newBuilder[String]
    def error(fName: String, message: String): Unit = errors += s"Field '$fName' of $owner: $message"

    val hasContent = fields.exists { case (_, param) =>
      !hasAnnotationType[transientField](param) && hasAnnotationType[xmlContent](param)
    }
    if (fields.count { case (_, param) => hasAnnotationType[xmlContent](param) } > 1)
      errors += s"$owner has more than one @xmlContent field: an element has one text content"

    val layouts = fields.map { case (fName, param) =>
      import param.tpe.Underlying as Field
      val isAttr = hasAnnotationType[xmlAttribute](param)
      val isElem = hasAnnotationType[xmlElement](param)
      val isContent = hasAnnotationType[xmlContent](param)
      val isUnwrapped = hasAnnotationType[xmlUnwrapped](param)
      val wrapper = getAnnotationStringArg[xmlWrapper](param)
      val isWrapped = hasAnnotationType[xmlWrapper](param)
      lazy val attrShape = xmlAttrShapeOf(Type[Field], hasUserInstance)
      val placements = List(
        isAttr -> "@xmlAttribute",
        isElem -> "@xmlElement",
        isContent -> "@xmlContent",
        isUnwrapped -> "@xmlUnwrapped",
        isWrapped -> "@xmlWrapper"
      ).collect { case (true, name) => name }
      def childElementNextToContent(): Unit =
        if (hasContent)
          error(
            fName,
            "is a child element, but the element's content is the @xmlContent field: next to @xmlContent the other " +
              s"fields have to be attributes ($attributeTypes)"
          )

      if (hasAnnotationType[transientField](param)) XmlFieldLayout.Transient
      else if (placements.size > 1 && placements != List("@xmlElement", "@xmlWrapper")) {
        error(fName, s"${placements.mkString(" and ")} cannot be combined")
        XmlFieldLayout.Element
      } else if (isContent)
        attrShape match {
          case Some(shape) => XmlFieldLayout.Content(shape)
          case None        =>
            error(
              fName,
              s"@xmlContent needs a single text value, ${Type[Field].prettyPrint} is not one. Supported: $attributeTypes"
            )
            XmlFieldLayout.Element
        }
      else if (isAttr)
        attrShape match {
          case Some(shape) => XmlFieldLayout.Attribute(shape)
          case None        =>
            error(
              fName,
              s"@xmlAttribute needs a single text value, ${Type[Field].prettyPrint} is not one (an XML attribute " +
                s"cannot hold a collection, a map or a case class). Supported: $attributeTypes. Remove @xmlAttribute to " +
                "write the field as a child element"
            )
            XmlFieldLayout.Element
        }
      else if (isUnwrapped) {
        childElementNextToContent()
        repeatableField(Type[Field]) match {
          case Right(true)  => ()
          case Right(false) =>
            error(
              fName,
              s"@xmlUnwrapped needs a collection or a map, ${Type[Field].prettyPrint} is not one (it writes one " +
                "child element per item, named after the field)"
            )
          case Left(reason) => error(fName, s"@xmlUnwrapped: $reason")
        }
        XmlFieldLayout.Repeated
      } else if (isWrapped) {
        childElementNextToContent()
        wrapper match {
          case Some(wrapperName) =>
            repeatableField(Type[Field]) match {
              case Right(repeated) => XmlFieldLayout.Wrapped(wrapperName, repeated)
              case Left(reason)    =>
                error(fName, s"@xmlWrapper: $reason")
                XmlFieldLayout.Element
            }
          case None =>
            error(fName, "the name of @xmlWrapper has to be a String literal")
            XmlFieldLayout.Element
        }
      } else if (isElem) {
        childElementNextToContent()
        XmlFieldLayout.Element
      } else if (hasContent)
        // next to the text content, a field can only be an attribute
        attrShape match {
          case Some(shape) => XmlFieldLayout.Attribute(shape)
          case None        =>
            childElementNextToContent()
            XmlFieldLayout.Element
        }
      else if (evaluatedConfig.exists(_.defaultFieldMode == XmlFieldMode.Element))
        // (the default configuration: every field without an annotation is a child element)
        XmlFieldLayout.Element
      else
        // `defaultFieldMode` decides for text values (and for enums of singletons written as strings); a type with a
        // user-provided instance stays a child element (as do collections, maps, case classes, other sealed traits)
        xmlDefaultAttrShapeOf(Type[Field], hasUserInstance) match {
          case Some(shape @ XmlAttrShape(XmlTextShape.BuiltIn, _)) =>
            evaluatedConfig.map(_.defaultFieldMode == XmlFieldMode.Attribute) match {
              case Some(true)  => XmlFieldLayout.Attribute(shape)
              case Some(false) => XmlFieldLayout.Element
              case None        => XmlFieldLayout.ByMode(shape, needsEnumAsStrings = false)
            }
          case Some(shape @ XmlAttrShape(_: XmlTextShape.StringEnum, _)) =>
            evaluatedConfig.map(c => c.defaultFieldMode == XmlFieldMode.Attribute && c.enumAsStrings) match {
              case Some(true)  => XmlFieldLayout.Attribute(shape)
              case Some(false) => XmlFieldLayout.Element
              case None        => XmlFieldLayout.ByMode(shape, needsEnumAsStrings = true)
            }
          case _ => XmlFieldLayout.Element
        }
    }

    val wrapperNames = layouts.collect { case XmlFieldLayout.Wrapped(name, _) => name }
    wrapperNames.diff(wrapperNames.distinct).distinct.foreach { name =>
      errors += s"$owner has more than one field with @xmlWrapper(\"$name\"): each wrapper element holds one field"
    }

    val allErrors = errors.result()
    if (allErrors.isEmpty) Right(layouts) else Left(allErrors)
  }
}
