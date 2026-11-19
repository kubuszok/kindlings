package hearth.kindlings.yamlderivation.internal.runtime

import org.virtuslab.yaml.{ConstructError, LoadSettings, Node, Tag, YamlDecoder, YamlError}
import org.virtuslab.yaml.Node.{MappingNode, ScalarNode, SequenceNode}

object YamlDerivationUtils {

  // --- Encoder helpers ---

  // Use convenience factories (no Tag parameter) to avoid Scala 3 overload resolution issues
  // with the private case class constructor.

  def nodeFromFields(fields: List[(String, Node)]): Node =
    MappingNode(fields.map { case (k, v) => (ScalarNode(k): Node, v) }.toMap)

  def wrapWithTypeName(typeName: String, inner: Node): Node =
    MappingNode(Map((ScalarNode(typeName): Node) -> inner))

  def addDiscriminator(discriminatorField: String, typeName: String, inner: Node): Node =
    inner match {
      case MappingNode(mappings, _) =>
        val discEntry: (Node, Node) = (ScalarNode(discriminatorField): Node) -> (ScalarNode(typeName): Node)
        MappingNode(Map(discEntry) ++ mappings)
      case other =>
        MappingNode(
          Map(
            (ScalarNode(discriminatorField): Node) -> (ScalarNode(typeName): Node),
            (ScalarNode("value"): Node) -> other
          )
        )
    }

  /** A subtype's node tagged as a configuration only known at runtime says (a discriminator field, a single-key
    * wrapper, or untagged: the node itself, or its name when `nameWhenUntagged`). One call per subtype keeps the
    * generated `match` over many subtypes small.
    */
  def tagSubtype(
      config: hearth.kindlings.yamlderivation.YamlConfig,
      name: String,
      node: Node,
      alwaysWrapped: Boolean,
      nameWhenUntagged: Boolean
  ): Node =
    config.discriminator match {
      case Some(discriminatorField) => addDiscriminator(discriminatorField, name, node)
      case None                     =>
        if (config.wrappedSubtypes || alwaysWrapped) wrapWithTypeName(name, node)
        else if (nameWhenUntagged) encodeEnumAsString(name)
        else node
    }

  /** The `names` transformed by `transform`, computed once per derived instance for a configuration only known at
    * runtime (the generated code reads them by index).
    */
  def transformNames(transform: String => String, names: List[String]): Array[String] = {
    val result = new Array[String](names.size)
    var i = 0
    var rest = names
    while (rest.nonEmpty) {
      result(i) = transform(rest.head)
      rest = rest.tail
      i += 1
    }
    result
  }

  def encodeEnumAsString(typeName: String): Node =
    ScalarNode(typeName)

  def decodeEnumFromString[A](node: Node, knownSubtypes: List[String])(
      dispatch: String => Either[ConstructError, A]
  ): Either[ConstructError, A] =
    node match {
      case ScalarNode(value, _) => dispatch(value)
      case other                =>
        Left(
          ConstructError.from(
            s"Expected a scalar node for enum value. Known values: ${knownSubtypes.mkString(", ")}",
            other
          )
        )
    }

  def encodeIterable[A](items: Iterable[A], encoder: A => Node): Node = {
    val nodes = items.map(encoder).toSeq
    SequenceNode(nodes*)
  }

  def encodeMappedPairs[P](pairs: Iterable[P], toPair: P => (String, Node)): Node = {
    val mappings = pairs.map { p =>
      val (k, v) = toPair(p)
      (ScalarNode(k): Node) -> v
    }.toMap
    MappingNode(mappings)
  }

  val nodeNull: Node = {
    import org.virtuslab.yaml.YamlEncoder
    YamlEncoder.forOption(YamlEncoder.forInt).asNode(None)
  }

  // --- Decoder helpers ---

  def decoderFromFn[A](decode: Node => Either[ConstructError, A]): YamlDecoder[A] =
    new YamlDecoder[A] {
      def construct(node: Node)(implicit settings: LoadSettings = LoadSettings.empty): Either[ConstructError, A] =
        decode(node)
    }

  def checkIsMapping(node: Node): Either[ConstructError, Unit] =
    node match {
      case _: MappingNode => Right(())
      case other => Left(ConstructError.from(s"Expected MappingNode, got ${other.getClass.getSimpleName}", other))
    }

  def getField(node: Node, fieldName: String): Either[ConstructError, Node] =
    node match {
      case MappingNode(mappings, _) =>
        mappings.collectFirst {
          case (ScalarNode(key, _), value) if key == fieldName => value
        } match {
          case Some(value) => Right(value)
          // scala-yaml's derived decoders' message
          case None => Left(ConstructError.from(s"Key $fieldName doesn't exist in parsed document", node))
        }
      case other =>
        Left(ConstructError.from(s"Expected MappingNode, got ${other.getClass.getSimpleName}", other))
    }

  /** The value node of each scalar key of a mapping, read once per decode (then each field is a hash lookup, not a scan
    * of the mapping). As scala-yaml's derived decoders: a repeated key gives its last value, a key that is not a scalar
    * fails with `Parameter of a class must be a scalar value`, a node that is not a mapping with `Expected MappingNode,
    * got <class>`.
    */
  def fieldNodes(node: Node): Either[ConstructError, java.util.HashMap[String, Node]] =
    node match {
      case MappingNode(mappings, _) =>
        val fields = new java.util.HashMap[String, Node](mappings.size * 2)
        val iter = mappings.iterator
        var error: ConstructError = null
        while (error == null && iter.hasNext) {
          val (key, value) = iter.next()
          key match {
            case ScalarNode(name, _) => val _ = fields.put(name, value)
            case _                   => error = ConstructError.from("Parameter of a class must be a scalar value", key)
          }
        }
        if (error == null) Right(fields) else Left(error)
      case other =>
        Left(ConstructError.from(s"Expected MappingNode, got ${other.getClass.getSimpleName}", other))
    }

  /** [[decodeRequiredField]] with the mapping's [[fieldNodes]]. */
  def decodeRequiredFieldOf(
      fields: java.util.HashMap[String, Node],
      node: Node,
      fieldName: String,
      decoder: YamlDecoder[?]
  ): Either[ConstructError, Any] = {
    val fieldNode = fields.get(fieldName)
    if (fieldNode == null) Left(ConstructError.from(s"Key $fieldName doesn't exist in parsed document", node))
    else decoder.construct(fieldNode)(LoadSettings.empty).asInstanceOf[Either[ConstructError, Any]]
  }

  /** [[decodeOptionalField]] with the mapping's [[fieldNodes]]. */
  def decodeOptionalFieldOf(
      fields: java.util.HashMap[String, Node],
      fieldName: String,
      decoder: YamlDecoder[?]
  ): Either[ConstructError, Any] = {
    val fieldNode = fields.get(fieldName)
    if (fieldNode == null) Right(None)
    else decoder.construct(fieldNode)(LoadSettings.empty).asInstanceOf[Either[ConstructError, Any]]
  }

  /** [[decodeFieldWithDefault]] with the mapping's [[fieldNodes]]. */
  def decodeFieldWithDefaultOf(
      fields: java.util.HashMap[String, Node],
      fieldName: String,
      decoder: YamlDecoder[?],
      default: Any
  ): Either[ConstructError, Any] = {
    val fieldNode = fields.get(fieldName)
    if (fieldNode == null) Right(default)
    else decoder.construct(fieldNode)(LoadSettings.empty).asInstanceOf[Either[ConstructError, Any]]
  }

  /** Decode a field that must be present: `getField(node, fieldName).flatMap(decoder.construct(_))`. */
  def decodeRequiredField(node: Node, fieldName: String, decoder: YamlDecoder[?]): Either[ConstructError, Any] =
    getField(node, fieldName) match {
      case Right(fieldNode) =>
        decoder.construct(fieldNode)(LoadSettings.empty).asInstanceOf[Either[ConstructError, Any]]
      case left => left
    }

  /** Decode an `Option` field: an absent key is `None` (as scala-yaml's derived decoders). */
  def decodeOptionalField(node: Node, fieldName: String, decoder: YamlDecoder[?]): Either[ConstructError, Any] =
    getOptionalField(node, fieldName).flatMap {
      case Some(fieldNode) =>
        decoder.construct(fieldNode)(LoadSettings.empty).asInstanceOf[Either[ConstructError, Any]]
      case None => Right(None)
    }

  def getOptionalField(node: Node, fieldName: String): Either[ConstructError, Option[Node]] =
    node match {
      case MappingNode(mappings, _) =>
        Right(mappings.collectFirst {
          case (ScalarNode(key, _), value) if key == fieldName => value
        })
      case other =>
        Left(ConstructError.from(s"Expected MappingNode, got ${other.getClass.getSimpleName}", other))
    }

  def decodeOptionFromFn[A](
      node: Node,
      decode: Node => Either[ConstructError, A]
  ): Either[ConstructError, Option[A]] =
    if (isNullNode(node)) Right(None)
    else decode(node).map(Some(_))

  class CollectionBuildException(val error: ConstructError) extends RuntimeException("yaml collection decoding error")

  def decodeCollectionWith[Item, Coll](
      node: Node,
      itemDecoder: YamlDecoder[Item],
      factory: scala.collection.Factory[Item, Coll]
  ): Either[ConstructError, Coll] =
    node match {
      case SequenceNode(nodes, _) =>
        val builder = factory.newBuilder
        val iter = nodes.iterator
        while (iter.hasNext)
          itemDecoder.construct(iter.next())(LoadSettings.empty) match {
            case Right(item) => builder += item
            case Left(err)   => return Left(err)
          }
        Right(builder.result())
      case other =>
        Left(ConstructError.from(s"Expected sequence node but got ${other.getClass.getSimpleName}", other))
    }

  def decodeMapWith[V, M](
      node: Node,
      valueDecoder: YamlDecoder[V],
      factory: scala.collection.Factory[(String, V), M]
  ): Either[ConstructError, M] =
    node match {
      case MappingNode(mappings, _) =>
        val builder = factory.newBuilder
        val iter = mappings.iterator
        while (iter.hasNext) {
          val (keyNode, valueNode) = iter.next()
          keyNode match {
            case ScalarNode(key, _) =>
              valueDecoder.construct(valueNode)(LoadSettings.empty) match {
                case Right(value) => builder += ((key, value))
                case Left(err)    => return Left(err)
              }
            case other =>
              return Left(ConstructError.from(s"Expected scalar key but got ${other.getClass.getSimpleName}", other))
          }
        }
        Right(builder.result())
      case other =>
        Left(ConstructError.from(s"Expected mapping node but got ${other.getClass.getSimpleName}", other))
    }

  /** The failure of scala-yaml's sum decoder when no subtype could decode `node`. */
  def cannotParse(node: Node): ConstructError = ConstructError.from(s"Cannot parse $node", node)

  /** How scala-yaml's decoder of a product without fields (e.g. a case object) decodes: any mapping with scalar keys.
    */
  def decodeAsEmptyProduct[A](node: Node, value: A): Either[ConstructError, A] =
    node match {
      case MappingNode(mappings, _) =>
        mappings.keys.collectFirst { case key if !key.isInstanceOf[ScalarNode] => key } match {
          case Some(key) => Left(ConstructError.from("Parameter of a class must be a scalar value", key))
          case None      => Right(value)
        }
      case other =>
        Left(ConstructError.from(s"Expected MappingNode, got ${other.getClass.getSimpleName}", other))
    }

  def decodeWrapped(node: Node): Either[ConstructError, (String, Node)] =
    node match {
      case MappingNode(mappings, _) =>
        mappings.headOption match {
          case Some((ScalarNode(typeName, _), innerNode)) =>
            Right((typeName, innerNode))
          case _ =>
            Left(ConstructError.from("Expected a mapping with a single key as type discriminator", node))
        }
      case other =>
        Left(ConstructError.from("Expected a mapping node for wrapped ADT encoding", other))
    }

  def decodeDiscriminator(node: Node, field: String): Either[ConstructError, (String, Node)] =
    node match {
      case MappingNode(mappings, _) =>
        mappings.collectFirst {
          case (ScalarNode(key, _), ScalarNode(typeName, _)) if key == field => typeName
        } match {
          case Some(typeName) => Right((typeName, node))
          case None           => Left(ConstructError.from(s"Missing discriminator field: $field", node))
        }
      case other =>
        Left(ConstructError.from("Expected a mapping node for discriminator ADT encoding", other))
    }

  def failedToMatchSubtype(typeName: String, node: Node, knownSubtypes: List[String]): ConstructError =
    ConstructError.from(
      s"Unknown type discriminator: $typeName. Expected one of: ${knownSubtypes.mkString(", ")}",
      node
    )

  def sequenceDecodeResults(results: List[Either[ConstructError, Any]]): Either[ConstructError, Array[Any]] = {
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

  /** Cast an `Any` value to `A`, using a `YamlDecoder[A]` purely for type inference. The decoder is not called - this
    * is a compile-time trick to avoid path-dependent type aliases in Scala 2 macro-generated code.
    */
  @scala.annotation.nowarn("msg=unused explicit parameter")
  def unsafeCast[A](value: Any, decoder: YamlDecoder[A]): A = value.asInstanceOf[A]

  def decodeFieldWithDefault(
      node: Node,
      fieldName: String,
      decoder: YamlDecoder[?],
      default: Any
  ): Either[ConstructError, Any] =
    getOptionalField(node, fieldName).flatMap {
      case Some(fieldNode) =>
        decoder.construct(fieldNode)(LoadSettings.empty).asInstanceOf[Either[ConstructError, Any]]
      case None => Right(default)
    }

  // --- Built-in type helpers ---

  def scalarNode(value: String): Node = ScalarNode(value)

  def getScalarValue(node: Node): Either[ConstructError, String] =
    node match {
      case ScalarNode(value, _) => Right(value)
      case other => Left(ConstructError.from(s"Expected scalar node but got ${other.getClass.getSimpleName}", other))
    }

  @scala.annotation.nowarn("msg=unused explicit parameter")
  def parseString(raw: String, node: Node): Either[ConstructError, String] =
    Right(raw)

  def parseBoolean(raw: String, node: Node): Either[ConstructError, Boolean] =
    raw.toLowerCase match {
      case "true"  => Right(true)
      case "false" => Right(false)
      case _       => Left(ConstructError.from(s"Cannot parse '$raw' as Boolean", node))
    }

  def parseInt(raw: String, node: Node): Either[ConstructError, Int] =
    try Right(raw.toInt)
    catch { case _: NumberFormatException => Left(ConstructError.from(s"Cannot parse '$raw' as Int", node)) }

  def parseLong(raw: String, node: Node): Either[ConstructError, Long] =
    try Right(raw.toLong)
    catch { case _: NumberFormatException => Left(ConstructError.from(s"Cannot parse '$raw' as Long", node)) }

  def parseDouble(raw: String, node: Node): Either[ConstructError, Double] =
    try Right(raw.toDouble)
    catch { case _: NumberFormatException => Left(ConstructError.from(s"Cannot parse '$raw' as Double", node)) }

  def parseFloat(raw: String, node: Node): Either[ConstructError, Float] =
    try Right(raw.toFloat)
    catch { case _: NumberFormatException => Left(ConstructError.from(s"Cannot parse '$raw' as Float", node)) }

  def parseShort(raw: String, node: Node): Either[ConstructError, Short] =
    try Right(raw.toShort)
    catch { case _: NumberFormatException => Left(ConstructError.from(s"Cannot parse '$raw' as Short", node)) }

  def parseByte(raw: String, node: Node): Either[ConstructError, Byte] =
    try Right(raw.toByte)
    catch { case _: NumberFormatException => Left(ConstructError.from(s"Cannot parse '$raw' as Byte", node)) }

  def parseChar(raw: String, node: Node): Either[ConstructError, Char] =
    if (raw.length == 1) Right(raw.charAt(0))
    else Left(ConstructError.from(s"Cannot parse '$raw' as Char (expected single character)", node))

  def parseBigDecimal(raw: String, node: Node): Either[ConstructError, BigDecimal] =
    try Right(BigDecimal(raw))
    catch { case _: NumberFormatException => Left(ConstructError.from(s"Cannot parse '$raw' as BigDecimal", node)) }

  def parseBigInt(raw: String, node: Node): Either[ConstructError, BigInt] =
    try Right(BigInt(raw))
    catch { case _: NumberFormatException => Left(ConstructError.from(s"Cannot parse '$raw' as BigInt", node)) }

  def isNullNode(node: Node): Boolean = node.tag == Tag.nullTag

  def nodeToYaml(node: Node): String = {
    import org.virtuslab.yaml.*
    node.asYaml
  }

  /** `nodeToYaml`, with empty sequences printed as `[]` and empty mappings as `{}` when `emptyCollectionsAsFlow`
    * (scala-yaml's printer writes them as an empty value, which reads back as `null`).
    */
  def nodeToYaml(node: Node, emptyCollectionsAsFlow: Boolean): String =
    if (emptyCollectionsAsFlow) nodeToYaml(withEmptyCollectionsAsFlow(node)) else nodeToYaml(node)

  // scala-yaml's printer writes a scalar's value as it is, so a `[]`/`{}` scalar prints as the flow-style empty
  // collection (the node is used for printing only)
  private def withEmptyCollectionsAsFlow(node: Node): Node = node match {
    case seq: Node.SequenceNode =>
      if (seq.nodes.isEmpty) Node.ScalarNode("[]")
      else Node.SequenceNode(seq.nodes.map(withEmptyCollectionsAsFlow)*)
    case map: Node.MappingNode =>
      if (map.mappings.isEmpty) Node.ScalarNode("{}")
      else Node.MappingNode(map.mappings.map { case (k, v) => k -> withEmptyCollectionsAsFlow(v) })
    case other => other
  }

  def parseAndDecode[A](
      yaml: String,
      decode: Node => Either[ConstructError, A]
  ): Either[YamlError, A] = {
    import org.virtuslab.yaml.*
    yaml.asNode.flatMap(decode(_))
  }

  // --- Codec combiner ---

  def yamlCodec[A](
      enc: org.virtuslab.yaml.YamlEncoder[A],
      dec: YamlDecoder[A]
  ): hearth.kindlings.yamlderivation.KindlingsYamlCodec[A] =
    new hearth.kindlings.yamlderivation.KindlingsYamlCodec[A] {
      override def asNode(obj: A): Node = enc.asNode(obj)
      override def construct(
          node: Node
      )(implicit settings: LoadSettings): Either[ConstructError, A] = dec.construct(node)
    }
}
