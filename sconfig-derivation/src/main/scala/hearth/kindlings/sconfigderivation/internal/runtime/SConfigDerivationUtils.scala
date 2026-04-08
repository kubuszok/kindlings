package hearth.kindlings.sconfigderivation.internal.runtime

import hearth.kindlings.sconfigderivation.{ConfigDecodingError, ConfigReader, ConfigWriter}
import org.ekrich.config.{ConfigList, ConfigObject, ConfigValue, ConfigValueFactory, ConfigValueType}

import scala.collection.Factory
import scala.jdk.CollectionConverters.*

object SConfigDerivationUtils {

  // --- Reader helpers ---

  def readerFromFn[A](read: ConfigValue => Either[ConfigDecodingError, A]): ConfigReader[A] =
    new ConfigReader[A] {
      override def from(value: ConfigValue): Either[ConfigDecodingError, A] = read(value)
    }

  /** Read a value as a `ConfigObject`, failing with `WrongType` otherwise. */
  def asObject(value: ConfigValue): Either[ConfigDecodingError, ConfigObject] =
    value.valueType match {
      case ConfigValueType.OBJECT => Right(value.asInstanceOf[ConfigObject])
      case other                  =>
        Left(ConfigDecodingError.WrongType(Nil, ConfigValueType.OBJECT.toString, other.toString))
    }

  /** Read a value as a `ConfigList`, failing with `WrongType` otherwise. */
  def asList(value: ConfigValue): Either[ConfigDecodingError, ConfigList] =
    value.valueType match {
      case ConfigValueType.LIST => Right(value.asInstanceOf[ConfigList])
      case other                =>
        Left(ConfigDecodingError.WrongType(Nil, ConfigValueType.LIST.toString, other.toString))
    }

  /** Read a value as a `String`, failing with `WrongType` otherwise. */
  def asString(value: ConfigValue): Either[ConfigDecodingError, String] =
    value.valueType match {
      case ConfigValueType.STRING => Right(value.unwrapped.asInstanceOf[String])
      case other                  =>
        Left(ConfigDecodingError.WrongType(Nil, ConfigValueType.STRING.toString, other.toString))
    }

  /** Read a required field from an object. */
  def readRequiredField(obj: ConfigObject, key: String): Either[ConfigDecodingError, ConfigValue] = {
    val v = obj.get(key)
    if (v == null) Left(ConfigDecodingError.Missing(Nil, key))
    else Right(v)
  }

  /** Read an optional field from an object. Returns `null`-shaped ConfigValue if missing. */
  def readOptionalField(obj: ConfigObject, key: String): ConfigValue = {
    val v = obj.get(key)
    if (v == null) ConfigValueFactory.fromAnyRef(null) else v
  }

  /** Read a discriminator field's string value. */
  def readDiscriminator(obj: ConfigObject, field: String): Either[ConfigDecodingError, String] =
    readRequiredField(obj, field).flatMap(asString)

  /** Read a "single-key wrapping" sealed-trait encoding: `{"VariantName": { …fields… }}`. */
  def readWrapped(obj: ConfigObject): Either[ConfigDecodingError, (String, ConfigValue)] = {
    val keys = obj.keySet().iterator().asScala.toList
    keys match {
      case key :: Nil => Right((key, obj.get(key)))
      case _          =>
        Left(
          ConfigDecodingError.CannotConvert(
            Nil,
            s"Expected a single-key wrapping object but got keys: ${keys.mkString(", ")}"
          )
        )
    }
  }

  def failedToMatchSubtype(
      typeName: String,
      knownSubtypes: List[String]
  ): ConfigDecodingError =
    ConfigDecodingError.CannotConvert(
      Nil,
      s"Unknown subtype '$typeName'. Expected one of: ${knownSubtypes.mkString(", ")}"
    )

  def sequenceResults(results: List[Either[ConfigDecodingError, Any]]): Either[ConfigDecodingError, Array[Any]] = {
    val arr = new Array[Any](results.size)
    var i = 0
    var failure: ConfigDecodingError = null
    val iter = results.iterator
    while (iter.hasNext) {
      iter.next() match {
        case Right(v) => arr(i) = v
        case Left(e)  =>
          failure = if (failure == null) e else ConfigDecodingError.merge(failure, e)
      }
      i += 1
    }
    if (failure != null) Left(failure) else Right(arr)
  }

  /** Read a list value into a custom collection via the given item reader and factory. */
  def decodeCollectionWith[Item, Coll](
      value: ConfigValue,
      itemReader: ConfigReader[Item],
      factory: Factory[Item, Coll]
  ): Either[ConfigDecodingError, Coll] = asList(value).flatMap { list =>
    val builder = factory.newBuilder
    var failure: ConfigDecodingError = null
    val iter = list.iterator()
    while (iter.hasNext && failure == null)
      itemReader.from(iter.next()) match {
        case Right(item) => builder += item
        case Left(e)     => failure = e
      }
    if (failure != null) Left(failure) else Right(builder.result())
  }

  /** Read an object value into a custom map collection via the given value reader and factory. */
  def decodeMapWith[V, M](
      value: ConfigValue,
      valueReader: ConfigReader[V],
      factory: Factory[(String, V), M]
  ): Either[ConfigDecodingError, M] = asObject(value).flatMap { obj =>
    val builder = factory.newBuilder
    var failure: ConfigDecodingError = null
    val iter = obj.entrySet().iterator().asScala
    while (iter.hasNext && failure == null) {
      val entry = iter.next()
      valueReader.from(entry.getValue) match {
        case Right(v) => builder += ((entry.getKey, v))
        case Left(e)  => failure = e.withParentPath(entry.getKey)
      }
    }
    if (failure != null) Left(failure) else Right(builder.result())
  }

  @scala.annotation.nowarn("msg=unused explicit parameter")
  def unsafeCast[A](value: Any, reader: ConfigReader[A]): A = value.asInstanceOf[A]

  /** Strict-mode check for `allowUnknownKeys = false`. After the decode result has been computed, this verifies that
    * the input object has no keys other than the ones the derivation expected. If unknown keys remain, the result is
    * replaced with a failure (mirrors PureConfig's `ProductHint.bottom`).
    */
  def checkUnknownKeys[A](
      cursor: ConfigValue,
      result: Either[ConfigDecodingError, A],
      expectedKeys: Set[String],
      allowUnknownKeys: Boolean
  ): Either[ConfigDecodingError, A] =
    if (allowUnknownKeys) result
    else
      asObject(cursor).flatMap { obj =>
        val unknown: List[String] = obj.entrySet.iterator.asScala
          .map(_.getKey)
          .toList
          .filterNot(expectedKeys.contains)
        unknown match {
          case Nil          => result
          case head :: Nil  => Left(ConfigDecodingError.UnknownKey(Nil, head))
          case head :: tail =>
            Left(
              ConfigDecodingError.Multiple(
                hearth.fp.data.NonEmptyList(
                  ConfigDecodingError.UnknownKey(Nil, head),
                  tail.map(k => ConfigDecodingError.UnknownKey(Nil, k))
                )
              )
            )
        }
      }

  // --- Writer helpers ---

  def writeFields(fields: List[(String, ConfigValue)]): ConfigValue = {
    val map = new java.util.LinkedHashMap[String, ConfigValue]()
    fields.foreach { case (k, v) => map.put(k, v) }
    ConfigValueFactory.fromMap(map)
  }

  def wrapWithTypeName(typeName: String, inner: ConfigValue): ConfigValue = {
    val map = new java.util.LinkedHashMap[String, ConfigValue]()
    map.put(typeName, inner)
    ConfigValueFactory.fromMap(map)
  }

  def addDiscriminator(discriminator: String, typeName: String, inner: ConfigValue): ConfigValue = {
    val map = new java.util.LinkedHashMap[String, ConfigValue]()
    map.put(discriminator, ConfigValueFactory.fromAnyRef(typeName))
    inner match {
      case obj: ConfigObject =>
        obj.entrySet().iterator().asScala.foreach { e =>
          map.put(e.getKey, e.getValue)
        }
      case other =>
        map.put("value", other)
    }
    ConfigValueFactory.fromMap(map)
  }

  def writeEnumAsString(typeName: String): ConfigValue =
    ConfigValueFactory.fromAnyRef(typeName)

  def writeIterable[A](items: Iterable[A], writer: A => ConfigValue): ConfigValue = {
    val list = new java.util.ArrayList[ConfigValue](items.size)
    items.foreach(a => list.add(writer(a)))
    ConfigValueFactory.fromIterable(list)
  }

  def writeMappedPairs[P](pairs: Iterable[P], toPair: P => (String, ConfigValue)): ConfigValue = {
    val map = new java.util.LinkedHashMap[String, ConfigValue]()
    pairs.foreach { p =>
      val (k, v) = toPair(p)
      map.put(k, v)
    }
    ConfigValueFactory.fromMap(map)
  }

  // --- Codec combiner ---

  def configCodec[A](
      r: ConfigReader[A],
      w: ConfigWriter[A]
  ): hearth.kindlings.sconfigderivation.ConfigCodec[A] =
    new hearth.kindlings.sconfigderivation.ConfigCodec[A] {
      override def from(value: ConfigValue): Either[ConfigDecodingError, A] = r.from(value)
      override def to(value: A): ConfigValue = w.to(value)
    }

  /** Constant empty object used for singleton case-object encoding. */
  val emptyConfigObject: ConfigValue = ConfigValueFactory.fromMap(new java.util.LinkedHashMap[String, AnyRef]())

  /** Lift a String error into a `ConfigDecodingError.CannotConvert`. */
  def liftStringError(msg: String): ConfigDecodingError =
    ConfigDecodingError.CannotConvert(Nil, msg)
}
