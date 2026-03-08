package hearth.kindlings.xmlderivation.internal.runtime

import hearth.kindlings.xmlderivation.{KindlingsXmlCodec, XmlDecodingError, XmlFieldMode}

object XmlDerivationUtils {

  // --- Encoder helpers ---

  def makeElem(
      name: String,
      attributes: List[(String, String)],
      children: List[scala.xml.Node]
  ): scala.xml.Elem = {
    val meta = attributes.foldRight[scala.xml.MetaData](scala.xml.Null) { case ((key, value), acc) =>
      new scala.xml.UnprefixedAttribute(key, value, acc)
    }
    val childGroup = children match {
      case Nil   => scala.xml.NodeSeq.Empty
      case nodes => nodes
    }
    scala.xml.Elem(null, name, meta, scala.xml.TopScope, childGroup.isEmpty, childGroup*)
  }

  def makeTextElem(name: String, text: String): scala.xml.Elem =
    scala.xml.Elem(null, name, scala.xml.Null, scala.xml.TopScope, false, scala.xml.Text(text))

  def makeEmptyElem(name: String): scala.xml.Elem =
    scala.xml.Elem(null, name, scala.xml.Null, scala.xml.TopScope, true)

  def elemWithAttributes(
      name: String,
      attributes: List[(String, String)]
  ): scala.xml.Elem = {
    val meta = attributes.foldRight[scala.xml.MetaData](scala.xml.Null) { case ((key, value), acc) =>
      new scala.xml.UnprefixedAttribute(key, value, acc)
    }
    scala.xml.Elem(null, name, meta, scala.xml.TopScope, true)
  }

  def addAttributes(
      elem: scala.xml.Elem,
      attributes: List[(String, String)]
  ): scala.xml.Elem =
    attributes.foldLeft(elem) { case (e, (key, value)) =>
      e % new scala.xml.UnprefixedAttribute(key, value, scala.xml.Null)
    }

  def addChildren(
      elem: scala.xml.Elem,
      children: List[scala.xml.Node]
  ): scala.xml.Elem =
    elem.copy(
      minimizeEmpty = children.isEmpty,
      child = elem.child ++ children
    )

  def combineAttributesAndChildren(
      name: String,
      attributes: List[(String, String)],
      children: List[scala.xml.Node]
  ): scala.xml.Elem =
    makeElem(name, attributes, children)

  def encodeEnumAsString(typeName: String, elementName: String): scala.xml.Elem =
    makeTextElem(elementName, typeName)

  def addDiscriminator(
      elem: scala.xml.Elem,
      discriminatorAttribute: String,
      typeName: String
  ): scala.xml.Elem =
    elem % new scala.xml.UnprefixedAttribute(discriminatorAttribute, typeName, scala.xml.Null)

  def wrapWithTypeName(typeName: String, inner: scala.xml.Elem): scala.xml.Elem =
    scala.xml.Elem(null, typeName, scala.xml.Null, scala.xml.TopScope, false, inner)

  def encodeIterable[A](
      items: Iterable[A],
      itemName: String,
      encoder: (A, String) => scala.xml.Elem
  ): List[scala.xml.Node] =
    items.map(a => encoder(a, itemName)).toList

  def encodeIterableWrapped[A](
      items: Iterable[A],
      wrapperName: String,
      itemName: String,
      encoder: (A, String) => scala.xml.Elem
  ): scala.xml.Elem = {
    val children = encodeIterable(items, itemName, encoder)
    makeElem(wrapperName, Nil, children)
  }

  def encodeMappedPairs[V](
      entries: Iterable[(String, V)],
      entryName: String,
      keyAttrName: String,
      valueEncoder: (V, String) => scala.xml.Elem
  ): List[scala.xml.Node] =
    entries.map { case (key, value) =>
      val valueElem = valueEncoder(value, "value")
      val entryElem = makeElem(entryName, List((keyAttrName, key)), List(valueElem))
      entryElem
    }.toList

  // --- Decoder helpers ---

  def getAttribute(elem: scala.xml.Elem, attrName: String): Either[XmlDecodingError, String] =
    elem.attribute(attrName).flatMap(_.headOption).map(_.text) match {
      case Some(value) => Right(value)
      case None        => Left(XmlDecodingError.MissingAttribute(attrName, elem.label))
    }

  def getOptionalAttribute(elem: scala.xml.Elem, attrName: String): Either[XmlDecodingError, Option[String]] =
    Right(elem.attribute(attrName).flatMap(_.headOption).map(_.text))

  def getChildElem(elem: scala.xml.Elem, childName: String): Either[XmlDecodingError, scala.xml.Elem] =
    (elem \ childName).collectFirst { case e: scala.xml.Elem => e } match {
      case Some(child) => Right(child)
      case None        => Left(XmlDecodingError.MissingElement(childName, elem.label))
    }

  def getOptionalChildElem(
      elem: scala.xml.Elem,
      childName: String
  ): Either[XmlDecodingError, Option[scala.xml.Elem]] =
    Right((elem \ childName).collectFirst { case e: scala.xml.Elem => e })

  def getChildElems(elem: scala.xml.Elem, childName: String): List[scala.xml.Elem] =
    (elem \ childName).collect { case e: scala.xml.Elem => e }.toList

  def getTextContent(elem: scala.xml.Elem): Either[XmlDecodingError, String] = {
    val text = elem.text.trim
    if (text.isEmpty) Left(XmlDecodingError.MissingContent(elem.label))
    else Right(text)
  }

  def getOptionalTextContent(elem: scala.xml.Elem): Option[String] = {
    val text = elem.text.trim
    if (text.isEmpty) None else Some(text)
  }

  def parseString(value: String, @scala.annotation.unused context: String): Either[XmlDecodingError, String] =
    Right(value)

  def parseInt(value: String, context: String): Either[XmlDecodingError, Int] =
    try Right(value.toInt)
    catch { case _: NumberFormatException => Left(XmlDecodingError.InvalidValue(value, "Int", context)) }

  def parseLong(value: String, context: String): Either[XmlDecodingError, Long] =
    try Right(value.toLong)
    catch { case _: NumberFormatException => Left(XmlDecodingError.InvalidValue(value, "Long", context)) }

  def parseDouble(value: String, context: String): Either[XmlDecodingError, Double] =
    try Right(value.toDouble)
    catch { case _: NumberFormatException => Left(XmlDecodingError.InvalidValue(value, "Double", context)) }

  def parseFloat(value: String, context: String): Either[XmlDecodingError, Float] =
    try Right(value.toFloat)
    catch { case _: NumberFormatException => Left(XmlDecodingError.InvalidValue(value, "Float", context)) }

  def parseBoolean(value: String, context: String): Either[XmlDecodingError, Boolean] =
    value.toLowerCase match {
      case "true" | "1" | "yes"  => Right(true)
      case "false" | "0" | "no" => Right(false)
      case _                     => Left(XmlDecodingError.InvalidValue(value, "Boolean", context))
    }

  def parseShort(value: String, context: String): Either[XmlDecodingError, Short] =
    try Right(value.toShort)
    catch { case _: NumberFormatException => Left(XmlDecodingError.InvalidValue(value, "Short", context)) }

  def parseByte(value: String, context: String): Either[XmlDecodingError, Byte] =
    try Right(value.toByte)
    catch { case _: NumberFormatException => Left(XmlDecodingError.InvalidValue(value, "Byte", context)) }

  def parseChar(value: String, context: String): Either[XmlDecodingError, Char] =
    if (value.length == 1) Right(value.charAt(0))
    else Left(XmlDecodingError.InvalidValue(value, "Char", context))

  def parseBigDecimal(value: String, context: String): Either[XmlDecodingError, BigDecimal] =
    try Right(BigDecimal(value))
    catch { case _: NumberFormatException => Left(XmlDecodingError.InvalidValue(value, "BigDecimal", context)) }

  def parseBigInt(value: String, context: String): Either[XmlDecodingError, BigInt] =
    try Right(BigInt(value))
    catch { case _: NumberFormatException => Left(XmlDecodingError.InvalidValue(value, "BigInt", context)) }

  def decodeOptionFromElem[A](
      elemOpt: Option[scala.xml.Elem],
      decode: scala.xml.Elem => Either[XmlDecodingError, A]
  ): Either[XmlDecodingError, Option[A]] =
    elemOpt match {
      case Some(e) => decode(e).map(Some(_))
      case None    => Right(None)
    }

  def decodeOptionFromAttr[A](
      attrOpt: Option[String],
      parse: (String, String) => Either[XmlDecodingError, A],
      context: String
  ): Either[XmlDecodingError, Option[A]] =
    attrOpt match {
      case Some(v) => parse(v, context).map(Some(_))
      case None    => Right(None)
    }

  def decodeCollectionFromElems[Item, Coll](
      elems: List[scala.xml.Elem],
      decodeItem: scala.xml.Elem => Either[XmlDecodingError, Item],
      factory: scala.collection.Factory[Item, Coll]
  ): Either[XmlDecodingError, Coll] = {
    val builder = factory.newBuilder
    val iter = elems.iterator
    while (iter.hasNext)
      decodeItem(iter.next()) match {
        case Right(item) => builder += item
        case Left(err)   => return Left(err)
      }
    Right(builder.result())
  }

  def decodeWrapped(elem: scala.xml.Elem): Either[XmlDecodingError, (String, scala.xml.Elem)] = {
    val children = elem.child.collect { case e: scala.xml.Elem => e }.toList
    children match {
      case single :: Nil => Right((single.label, single))
      case _             => Left(XmlDecodingError.General(s"Expected exactly one child element for wrapped ADT in <${elem.label}>"))
    }
  }

  def decodeDiscriminator(
      elem: scala.xml.Elem,
      attributeName: String
  ): Either[XmlDecodingError, String] =
    getAttribute(elem, attributeName)
      .left
      .map(_ => XmlDecodingError.MissingDiscriminator(attributeName, elem.label))

  def failedToMatchSubtype(typeName: String, knownSubtypes: List[String]): XmlDecodingError =
    XmlDecodingError.UnknownDiscriminator(typeName, knownSubtypes)

  def sequenceDecodeResults(results: List[Either[XmlDecodingError, Any]]): Either[XmlDecodingError, Array[Any]] = {
    val arr = new Array[Any](results.size)
    var i = 0
    val iter = results.iterator
    while (iter.hasNext)
      iter.next() match {
        case Right(v) => arr(i) = v; i += 1
        case Left(e)  => return Left(e)
      }
    Right(arr)
  }

  /** Cast an `Any` value to `A`, using an `XmlDecoder[A]` purely for type inference. */
  @scala.annotation.nowarn("msg=unused explicit parameter")
  def unsafeCast[A](value: Any, decoder: hearth.kindlings.xmlderivation.XmlDecoder[A]): A = value.asInstanceOf[A]

  /** Cast an `Any` value to `A`, using a decode function purely for type inference.
    * This avoids path-dependent type issues when used inside Expr.quote on Scala 2. */
  @scala.annotation.nowarn("msg=unused explicit parameter")
  def unsafeCastWithFn[A](value: Any, decodeFn: scala.xml.Elem => Either[XmlDecodingError, A]): A =
    value.asInstanceOf[A]

  def decodeFieldWithDefault(
      elem: scala.xml.Elem,
      fieldName: String,
      isAttribute: Boolean,
      decoder: scala.xml.Elem => Either[XmlDecodingError, Any],
      parser: (String, String) => Either[XmlDecodingError, Any],
      default: Any
  ): Either[XmlDecodingError, Any] =
    if (isAttribute) {
      getOptionalAttribute(elem, fieldName).flatMap {
        case Some(v) => parser(v, s"attribute '$fieldName'")
        case None    => Right(default)
      }
    } else {
      getOptionalChildElem(elem, fieldName).flatMap {
        case Some(child) => decoder(child)
        case None        => Right(default)
      }
    }

  def elemToString(elem: scala.xml.Elem): String = {
    val printer = new scala.xml.PrettyPrinter(120, 2)
    printer.format(elem)
  }

  def parseXml(xml: String): Either[XmlDecodingError, scala.xml.Elem] =
    try Right(scala.xml.XML.loadString(xml))
    catch { case e: Exception => Left(XmlDecodingError.General(s"Failed to parse XML: ${e.getMessage}")) }

  def parseAndDecode[A](
      xml: String,
      decode: scala.xml.Elem => Either[XmlDecodingError, A]
  ): Either[XmlDecodingError, A] =
    parseXml(xml).flatMap(decode)

  def isAttributeMode(fieldMode: XmlFieldMode): Boolean = fieldMode == XmlFieldMode.Attribute

  // --- Codec combiner ---

  def xmlCodec[A](
      enc: hearth.kindlings.xmlderivation.XmlEncoder[A],
      dec: hearth.kindlings.xmlderivation.XmlDecoder[A]
  ): KindlingsXmlCodec[A] =
    new KindlingsXmlCodec[A] {
      override def encode(value: A, elementName: String): scala.xml.Elem = enc.encode(value, elementName)
      override def decode(elem: scala.xml.Elem): Either[XmlDecodingError, A] = dec.decode(elem)
    }
}
