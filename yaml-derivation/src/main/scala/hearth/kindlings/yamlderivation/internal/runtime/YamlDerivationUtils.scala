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
      case other => Left(ConstructError.from(s"Expected mapping node but got ${other.getClass.getSimpleName}", other))
    }

  def getField(node: Node, fieldName: String): Either[ConstructError, Node] =
    node match {
      case MappingNode(mappings, _) =>
        mappings.collectFirst {
          case (ScalarNode(key, _), value) if key == fieldName => value
        } match {
          case Some(value) => Right(value)
          case None        => Left(ConstructError.from(s"Missing field: $fieldName", node))
        }
      case other =>
        Left(ConstructError.from(s"Expected mapping node but got ${other.getClass.getSimpleName}", other))
    }

  def getOptionalField(node: Node, fieldName: String): Either[ConstructError, Option[Node]] =
    node match {
      case MappingNode(mappings, _) =>
        Right(mappings.collectFirst {
          case (ScalarNode(key, _), value) if key == fieldName => value
        })
      case other =>
        Left(ConstructError.from(s"Expected mapping node but got ${other.getClass.getSimpleName}", other))
    }

  def decodeOptionFromFn[A](
      node: Node,
      decode: Node => Either[ConstructError, A]
  ): Either[ConstructError, Option[A]] =
    if (isNullNode(node)) Right(None)
    else decode(node).map(Some(_))

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

  def isNullNode(node: Node): Boolean = node.tag == Tag.nullTag

  def nodeToYaml(node: Node): String = {
    import org.virtuslab.yaml.*
    node.asYaml
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
