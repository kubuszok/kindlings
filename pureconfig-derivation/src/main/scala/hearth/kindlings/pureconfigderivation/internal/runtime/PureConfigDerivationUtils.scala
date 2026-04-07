package hearth.kindlings.pureconfigderivation.internal.runtime

import com.typesafe.config.{ConfigValue, ConfigValueFactory}
import pureconfig.ConfigCursor
import pureconfig.ConfigReader.Result
import pureconfig.error.{
  ConfigReaderFailures,
  ConvertFailure,
  KeyNotFound,
  WrongType
}
import pureconfig.{ConfigReader, ConfigWriter}

import scala.jdk.CollectionConverters.*

object PureConfigDerivationUtils {

  // --- Reader helpers ---

  /** Wrap a function as a [[ConfigReader]]. Used by the macro to materialise lambdas as
    * standalone reader values when needed.
    */
  def readerFromFn[A](read: ConfigCursor => Result[A]): ConfigReader[A] =
    new ConfigReader[A] {
      override def from(cur: ConfigCursor): Result[A] = read(cur)
    }

  /** Read a (possibly missing) field, falling back to the supplied default when the
    * key is absent.
    */
  def readFieldWithDefault[A](
      obj: pureconfig.ConfigObjectCursor,
      key: String,
      reader: ConfigReader[A],
      default: => A
  ): Result[A] = {
    val cur = obj.atKeyOrUndefined(key)
    if (cur.isUndefined) Right(default)
    else reader.from(cur)
  }

  /** Read a required field. */
  def readRequiredField[A](
      obj: pureconfig.ConfigObjectCursor,
      key: String,
      reader: ConfigReader[A]
  ): Result[A] =
    obj.atKey(key).flatMap(reader.from)

  /** Read a field that may be absent. The reader receives an `isUndefined` cursor when the
    * key is missing, which lets `Option[T]` readers turn it into `None` rather than failing.
    */
  def readOptionalKey[A](
      obj: pureconfig.ConfigObjectCursor,
      key: String,
      reader: ConfigReader[A]
  ): Result[A] =
    reader.from(obj.atKeyOrUndefined(key))

  /** Read a discriminator field's string value (used by sealed-trait readers). */
  def readDiscriminator(obj: pureconfig.ConfigObjectCursor, field: String): Result[String] =
    obj.atKey(field).flatMap(_.asString)

  /** Read a "single-key wrapping" sealed-trait encoding: `{"VariantName": { …fields… }}`.
    * Returns the (typeName, innerCursor) pair.
    */
  def readWrapped(obj: pureconfig.ConfigObjectCursor): Result[(String, ConfigCursor)] = {
    val keys = obj.keys.toList
    keys match {
      case key :: Nil =>
        obj.atKey(key).map(inner => (key, inner))
      case _ =>
        Left(
          ConfigReaderFailures(
            ConvertFailure(
              reason = WrongType(
                foundType = com.typesafe.config.ConfigValueType.OBJECT,
                expectedTypes = Set(com.typesafe.config.ConfigValueType.OBJECT)
              ),
              origin = None,
              path = obj.path
            )
          )
        )
    }
  }

  /** Build a [[ConfigReaderFailures]] reporting that no sealed-trait subtype matched the
    * given discriminator value.
    */
  def failedToMatchSubtype(
      typeName: String,
      cur: ConfigCursor,
      knownSubtypes: List[String]
  ): ConfigReaderFailures =
    ConfigReaderFailures(
      ConvertFailure(
        reason = pureconfig.error.CannotConvert(
          value = typeName,
          toType = "one of [" + knownSubtypes.mkString(", ") + "]",
          because = s"Unknown subtype '$typeName'"
        ),
        origin = cur.origin,
        path = cur.path
      )
    )

  /** Sequence a list of decode results into an array. On any failure, returns the
    * combined failures so users see every problem at once instead of having to fix one
    * field and re-run.
    */
  def sequenceResults(results: List[Result[Any]]): Result[Array[Any]] = {
    val arr = new Array[Any](results.size)
    var i = 0
    var combined: ConfigReaderFailures = null
    val iter = results.iterator
    while (iter.hasNext) {
      iter.next() match {
        case Right(v) =>
          arr(i) = v
        case Left(failures) =>
          combined =
            if (combined == null) failures
            else {
              val merged: Seq[pureconfig.error.ConfigReaderFailure] =
                combined.tail ++ Seq(failures.head) ++ failures.tail
              ConfigReaderFailures(combined.head, merged*)
            }
      }
      i += 1
    }
    if (combined == null) Right(arr) else Left(combined)
  }

  /** Build a [[ConfigReaderFailures]] for a missing required field. */
  def missingFieldFailure(cur: ConfigCursor, key: String): ConfigReaderFailures =
    ConfigReaderFailures(
      ConvertFailure(
        reason = KeyNotFound(key, Set.empty),
        origin = cur.origin,
        path = cur.path
      )
    )

  /** Strict-mode check for `allowUnknownKeys = false`. After the decode result has been
    * computed, this verifies that the input object has no keys other than the ones the
    * derivation expected. If unknown keys remain, the result is replaced with a failure
    * listing each one as a separate `UnknownKey` failure (matching upstream PureConfig's
    * `ProductHint.bottom` behaviour).
    */
  def checkUnknownKeys[A](
      cursor: ConfigCursor,
      result: Result[A],
      expectedKeys: Set[String],
      allowUnknownKeys: Boolean
  ): Result[A] =
    if (allowUnknownKeys) result
    else
      cursor.asObjectCursor.flatMap { obj =>
        val unknown = obj.map.toList.collect {
          case (k, keyCur) if !expectedKeys.contains(k) =>
            keyCur.failureFor(pureconfig.error.UnknownKey(k))
        }
        unknown match {
          case Nil          => result
          case head :: tail => Left(ConfigReaderFailures(head, tail*))
        }
      }

  /** Cast `Any` to `A` using a `ConfigReader[A]` purely as a type-inference hint. The
    * reader is not invoked. This avoids leaking path-dependent field types into
    * `Expr.quote` on Scala 2.
    */
  @scala.annotation.nowarn("msg=unused explicit parameter")
  def unsafeCast[A](value: Any, reader: ConfigReader[A]): A = value.asInstanceOf[A]

  /** Read a list cursor and decode every item with the given reader, collecting into the
    * builder produced by the supplied factory.
    */
  def decodeCollectionWith[Item, Coll](
      cur: ConfigCursor,
      itemReader: ConfigReader[Item],
      factory: scala.collection.Factory[Item, Coll]
  ): Result[Coll] =
    cur.asListCursor.flatMap { listCur =>
      val builder = factory.newBuilder
      var failure: ConfigReaderFailures = null
      val iter = listCur.list.iterator
      while (iter.hasNext && failure == null)
        itemReader.from(iter.next()) match {
          case Right(item) => builder += item
          case Left(f)     => failure = f
        }
      if (failure != null) Left(failure)
      else Right(builder.result())
    }

  /** Read an object cursor and decode every value with the given reader, collecting into
    * the builder produced by the supplied factory. Map keys are always strings.
    */
  def decodeMapWith[V, M](
      cur: ConfigCursor,
      valueReader: ConfigReader[V],
      factory: scala.collection.Factory[(String, V), M]
  ): Result[M] =
    cur.asObjectCursor.flatMap { objCur =>
      val builder = factory.newBuilder
      var failure: ConfigReaderFailures = null
      val iter = objCur.map.iterator
      while (iter.hasNext && failure == null) {
        val (key, valCur) = iter.next()
        valueReader.from(valCur) match {
          case Right(value) => builder += ((key, value))
          case Left(f)      => failure = f
        }
      }
      if (failure != null) Left(failure)
      else Right(builder.result())
    }

  // --- Writer helpers ---

  /** Build a `ConfigValue` from `(name, value)` pairs (object encoding). */
  def writeFields(fields: List[(String, ConfigValue)]): ConfigValue = {
    val map = new java.util.LinkedHashMap[String, ConfigValue]()
    fields.foreach { case (k, v) => map.put(k, v) }
    ConfigValueFactory.fromMap(map)
  }

  /** Wrap a value with a single-key parent object: `{"TypeName": inner}`. */
  def wrapWithTypeName(typeName: String, inner: ConfigValue): ConfigValue = {
    val map = new java.util.LinkedHashMap[String, ConfigValue]()
    map.put(typeName, inner)
    ConfigValueFactory.fromMap(map)
  }

  /** Add a `discriminator -> typeName` pair to the front of `inner` (object encoding).
    * If `inner` is not an object, wrap it under a "value" key as a fallback.
    */
  def addDiscriminator(discriminator: String, typeName: String, inner: ConfigValue): ConfigValue = {
    val map = new java.util.LinkedHashMap[String, ConfigValue]()
    map.put(discriminator, ConfigValueFactory.fromAnyRef(typeName))
    inner match {
      case obj: com.typesafe.config.ConfigObject =>
        obj.entrySet().iterator().asScala.foreach { e =>
          map.put(e.getKey, e.getValue)
        }
      case other =>
        map.put("value", other)
    }
    ConfigValueFactory.fromMap(map)
  }

  /** Encode a singleton object as a string scalar. */
  def writeEnumAsString(typeName: String): ConfigValue =
    ConfigValueFactory.fromAnyRef(typeName)

  /** Encode an iterable into a `ConfigList`. */
  def writeIterable[A](items: Iterable[A], writer: A => ConfigValue): ConfigValue = {
    val javaList = new java.util.ArrayList[ConfigValue](items.size)
    items.foreach(a => javaList.add(writer(a)))
    ConfigValueFactory.fromIterable(javaList)
  }

  /** Encode a map (string-keyed) into a `ConfigObject`. */
  def writeMap[V](pairs: Iterable[(String, V)], writer: V => ConfigValue): ConfigValue = {
    val map = new java.util.LinkedHashMap[String, ConfigValue]()
    pairs.foreach { case (k, v) => map.put(k, writer(v)) }
    ConfigValueFactory.fromMap(map)
  }

  /** Encode an iterable of pairs into a `ConfigObject` via a `P => (String, ConfigValue)` extractor. */
  def writeMappedPairs[P](pairs: Iterable[P], toPair: P => (String, ConfigValue)): ConfigValue = {
    val map = new java.util.LinkedHashMap[String, ConfigValue]()
    pairs.foreach { p =>
      val (k, v) = toPair(p)
      map.put(k, v)
    }
    ConfigValueFactory.fromMap(map)
  }

  // --- Convert combiner ---

  def configConvert[A](
      r: ConfigReader[A],
      w: ConfigWriter[A]
  ): hearth.kindlings.pureconfigderivation.KindlingsConfigConvert[A] =
    new hearth.kindlings.pureconfigderivation.KindlingsConfigConvert[A] {
      override def from(cur: ConfigCursor): Result[A] = r.from(cur)
      override def to(a: A): ConfigValue = w.to(a)
    }

  // --- Singleton helpers ---

  /** A constant empty object cursor used as the cursor for singleton case objects. */
  val emptyConfigObject: ConfigValue = ConfigValueFactory.fromMap(new java.util.LinkedHashMap[String, AnyRef]())
}
