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

  class CollectionBuildException(val error: ConfigDecodingError)
      extends RuntimeException("sconfig collection decoding error")

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

  // --- Upstream PureConfig semantics (failure accumulation, paths, coproduct hints) ---

  /** Collects the failures of every item of a collection / value of a map, so that (like upstream PureConfig's
    * `mapList` / `mapObject`) all of them are reported, not only the first one.
    */
  final class FailureAccumulator {
    private var failures: ConfigDecodingError = null
    def add(e: ConfigDecodingError): Unit = failures =
      if (failures == null) e else ConfigDecodingError.merge(failures, e)
    def isEmpty: Boolean = failures == null
    def throwIfAny(): Unit = if (failures != null) throw new CollectionBuildException(failures)
  }

  /** Read the field `key` of `obj` the way upstream PureConfig does: the value's failures are reported under `key`; a
    * missing key is passed (as `null`) to readers that read missing keys (`readsMissingKeys`, e.g. `Option` fields, or
    * a [[hearth.kindlings.sconfigderivation.ReadsMissingKeys]] reader), and reported as `KeyNotFound` (with upstream's
    * candidate keys) at `obj` otherwise.
    */
  def readField[A](
      obj: ConfigObject,
      key: String,
      reader: ConfigReader[A],
      readsMissingKeys: Boolean
  ): Either[ConfigDecodingError, A] = {
    val v = obj.get(key)
    if (v != null || readsMissingKeys || reader.isInstanceOf[hearth.kindlings.sconfigderivation.ReadsMissingKeys])
      reader.from(v) match {
        case Left(e) => Left(e.withParentPath(key))
        case right   => right
      }
    else Left(ConfigDecodingError.KeyNotFound.forKeys(key, obj.keySet().asScala))
  }

  /** The `UnknownKey` failures (each at its key) upstream PureConfig's `ProductHint.bottom` reports for the keys of
    * `obj` that are not `usedKeys`, combined after the field failures of `result`.
    */
  def withUnknownKeys[A](
      obj: ConfigObject,
      result: Either[ConfigDecodingError, A],
      usedKeys: Set[String]
  ): Either[ConfigDecodingError, A] = {
    val unknown = obj.keySet().iterator().asScala.filterNot(usedKeys.contains).toList
    unknown match {
      case Nil  => result
      case keys =>
        val unknownFailures: ConfigDecodingError =
          keys
            .map(k => ConfigDecodingError.UnknownKey(List(k), k): ConfigDecodingError)
            .reduce(ConfigDecodingError.merge)
        result match {
          case Left(e) => Left(ConfigDecodingError.merge(e, unknownFailures))
          case _       => Left(unknownFailures)
        }
    }
  }

  /** Read the discriminator field of `obj` (upstream `FieldCoproductHint`): `KeyNotFound` when missing, the value's
    * failure under the field when it is not a string.
    */
  def readDiscriminatorField(obj: ConfigObject, field: String): Either[ConfigDecodingError, String] = {
    val v = obj.get(field)
    if (v == null) Left(ConfigDecodingError.KeyNotFound.forKeys(field, obj.keySet().asScala))
    else asString(v).left.map(_.withParentPath(field))
  }

  /** The failure upstream PureConfig's `FieldCoproductHint` reports for a discriminator value that names no subtype. */
  def unexpectedDiscriminatorValue(obj: ConfigObject, field: String): ConfigDecodingError =
    ConfigDecodingError.UnexpectedValueForFieldCoproductHint(List(field), obj.get(field))

  /** The failure for a string that names no case object of an enumeration (a sealed family of case objects read as
    * strings without a discriminator), like upstream's `EnumConfigReader`.
    */
  def invalidEnumValue(value: String, typeName: String): ConfigDecodingError =
    ConfigDecodingError.CannotConvert(
      Nil,
      s"Cannot convert '$value' to $typeName: The value is not a valid enum option."
    )

  /** `value` when `config` is an object (how upstream's derived reader of a case object reads), its failure otherwise.
    */
  def readObjectAs[A](config: ConfigValue, value: A): Either[ConfigDecodingError, A] = asObject(config).map(_ => value)

  /** The failure upstream `FirstSuccessCoproductHint` reports when no subtype could be read:
    * `NoValidCoproductOptionFound` with the failed results of every attempted subtype (in the order attempted).
    */
  def noValidCoproductOptionFromResults(
      value: ConfigValue,
      options: List[String],
      results: List[Either[ConfigDecodingError, Any]]
  ): ConfigDecodingError =
    ConfigDecodingError.NoValidCoproductOptionFound(
      Nil,
      value,
      options.zip(results).collect { case (option, Left(failure)) => (option, failure) }
    )

  /** Write a sealed-trait subtype with a discriminator field, the way upstream PureConfig's `FieldCoproductHint.to`
    * does: the discriminator comes first; a subtype value that already has the key, or is not an object, is rejected
    * with a `CoproductHintException`.
    */
  def addDiscriminatorField(discriminator: String, typeName: String, inner: ConfigValue): ConfigValue = inner match {
    case obj: ConfigObject if obj.containsKey(discriminator) =>
      throw hearth.kindlings.sconfigderivation.CoproductHintException(
        ConfigDecodingError.CollidingKeys(Nil, discriminator, obj.get(discriminator))
      )
    case _: ConfigObject => addDiscriminator(discriminator, typeName, inner)
    case other           =>
      throw hearth.kindlings.sconfigderivation.CoproductHintException(
        ConfigDecodingError.WrongType(Nil, ConfigValueType.OBJECT.toString, other.valueType.toString)
      )
  }

  /** Like [[writeFields]], but omits the fields whose value is `null` (the macro's marker for an empty `Option`, which
    * upstream PureConfig omits via `WritesMissingKeys`).
    */
  def writeFieldsOmittingMissing(fields: List[(String, ConfigValue)]): ConfigValue = {
    val map = new java.util.LinkedHashMap[String, ConfigValue]()
    fields.foreach { case (k, v) => if (v != null) { val _ = map.put(k, v) } }
    ConfigValueFactory.fromMap(map)
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
