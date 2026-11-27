package hearth.kindlings.avroderivation.internal.runtime

import org.apache.avro.Schema
import org.apache.avro.generic.{GenericData, GenericRecord}

import hearth.kindlings.avroderivation.TimestampPrecision

import java.nio.ByteBuffer
import java.util

object AvroDerivationUtils {

  // --- Schema helpers ---

  def createRecord(name: String, namespace: String, fields: util.List[Schema.Field]): Schema = {
    val schema = Schema.createRecord(name, null, namespace, false)
    schema.setFields(fields)
    schema
  }

  def createRecordWithDoc(
      name: String,
      namespace: String,
      doc: String,
      fields: util.List[Schema.Field]
  ): Schema = {
    val schema = Schema.createRecord(name, doc, namespace, false)
    schema.setFields(fields)
    schema
  }

  def createRecordError(name: String, namespace: String, fields: util.List[Schema.Field]): Schema = {
    val schema = Schema.createRecord(name, null, namespace, true)
    schema.setFields(fields)
    schema
  }

  def createRecordWithDocError(
      name: String,
      namespace: String,
      doc: String,
      fields: util.List[Schema.Field]
  ): Schema = {
    val schema = Schema.createRecord(name, doc, namespace, true)
    schema.setFields(fields)
    schema
  }

  /** Creates an empty record schema (no fields) for forward-declaration in recursive schemas. */
  def createEmptyRecord(name: String, doc: String, namespace: String, isError: Boolean): Schema =
    Schema.createRecord(name, doc, namespace, isError)

  /** Sets fields on a pre-created record schema and returns it. Used with createEmptyRecord for recursive schemas. */
  def setRecordFields(schema: Schema, fields: util.List[Schema.Field]): Schema = {
    schema.setFields(fields)
    schema
  }

  /** Appends a field to the fields of a record schema being built (one statement per field in derived schemas). */
  def addRecordField(fields: util.ArrayList[Schema.Field], field: Schema.Field): Unit = {
    val _ = fields.add(field)
  }

  /** Sets fields from a Scala List on a pre-created record schema and returns it. */
  def setRecordFieldsFromList(schema: Schema, fields: List[Schema.Field]): Schema = {
    val javaFields = new util.ArrayList[Schema.Field](fields.size)
    fields.foreach(javaFields.add)
    schema.setFields(javaFields)
    schema
  }

  def addSchemaProp(schema: Schema, key: String, value: String): Unit =
    schema.addProp(key, parseJsonPropValue(value))

  def addFieldProp(field: Schema.Field, key: String, value: String): Unit =
    field.addProp(key, parseJsonPropValue(value))

  private def parseJsonPropValue(value: String): AnyRef = {
    val trimmed = value.trim
    if (
      trimmed.startsWith("{") || trimmed.startsWith("[") || trimmed.startsWith("\"") ||
      trimmed == "true" || trimmed == "false" || trimmed == "null" ||
      trimmed.headOption.exists(c => c.isDigit || c == '-')
    ) {
      try {
        val mapper = new com.fasterxml.jackson.databind.ObjectMapper()
        mapper.readTree(trimmed)
      } catch {
        case _: Exception => value
      }
    } else value
  }

  def addSchemaAlias(schema: Schema, alias: String): Unit =
    schema.addAlias(alias)

  def addFieldAlias(field: Schema.Field, alias: String): Unit =
    field.addAlias(alias)

  def createField(name: String, schema: Schema): Schema.Field =
    new Schema.Field(name, schema)

  def createFieldWithDoc(name: String, schema: Schema, doc: String): Schema.Field =
    new Schema.Field(name, schema, doc)

  def createFieldWithDefault(name: String, schema: Schema, defaultJson: String): Schema.Field = {
    val defaultValue = parseJsonDefault(defaultJson)
    new Schema.Field(name, schema, null, defaultValue)
  }

  def createFieldWithDocAndDefault(
      name: String,
      schema: Schema,
      doc: String,
      defaultJson: String
  ): Schema.Field = {
    val defaultValue = parseJsonDefault(defaultJson)
    new Schema.Field(name, schema, doc, defaultValue)
  }

  private def parseJsonDefault(json: String): Any = {
    val mapper = new com.fasterxml.jackson.databind.ObjectMapper()
    val node = mapper.readTree(json)
    jsonNodeToObject(node)
  }

  private def jsonNodeToObject(node: com.fasterxml.jackson.databind.JsonNode): Any = {
    import com.fasterxml.jackson.databind.node.*
    node match {
      case _: NullNode                                        => org.apache.avro.JsonProperties.NULL_VALUE
      case n: BooleanNode                                     => java.lang.Boolean.valueOf(n.booleanValue())
      case n: IntNode                                         => java.lang.Integer.valueOf(n.intValue())
      case n: LongNode                                        => java.lang.Long.valueOf(n.longValue())
      case n: FloatNode                                       => java.lang.Float.valueOf(n.floatValue())
      case n: DoubleNode                                      => java.lang.Double.valueOf(n.doubleValue())
      case n: com.fasterxml.jackson.databind.node.DecimalNode => java.lang.Double.valueOf(n.doubleValue())
      case n: TextNode                                        => n.textValue()
      case n: ArrayNode                                       =>
        val list = new java.util.ArrayList[Any](n.size())
        val iter = n.elements()
        while (iter.hasNext) { val _ = list.add(jsonNodeToObject(iter.next())) }
        list
      case n: ObjectNode =>
        val map = new java.util.LinkedHashMap[String, Any]()
        val iter = n.properties().iterator()
        while (iter.hasNext) {
          val entry = iter.next()
          val _ = map.put(entry.getKey, jsonNodeToObject(entry.getValue))
        }
        map
      case _ => node.asText()
    }
  }

  def createUnion(schemas: Schema*): Schema =
    Schema.createUnion(schemas*)

  /** Creates a union schema that flattens nested unions and deduplicates nulls. Avro does not allow nested unions, so
    * when creating Option[SealedTrait] or Option[Either[A, B]], the inner union must be flattened into the outer one.
    * Null schemas are deduplicated and moved to the front.
    */
  def createSafeUnion(schemas: Schema*): Schema = {
    val flat = new java.util.ArrayList[Schema]()
    schemas.foreach { s =>
      if (s.getType == Schema.Type.UNION) {
        val iter = s.getTypes.iterator()
        while (iter.hasNext) { val _ = flat.add(iter.next()) }
      } else {
        val _ = flat.add(s)
      }
    }
    // Deduplicate nulls and move null to front
    val deduped = new java.util.ArrayList[Schema]()
    var hasNull = false
    val iter = flat.iterator()
    while (iter.hasNext) {
      val s = iter.next()
      if (s.getType == Schema.Type.NULL) hasNull = true
      else { val _ = deduped.add(s) }
    }
    if (hasNull) { val _ = deduped.add(0, Schema.create(Schema.Type.NULL)) }
    Schema.createUnion(deduped)
  }

  def nullSchema: Schema = Schema.create(Schema.Type.NULL)

  // Primitive schema constructors — Java enum constants don't reify inside Scala 2 macro quotes
  def booleanSchema: Schema = Schema.create(Schema.Type.BOOLEAN)
  def intSchema: Schema = Schema.create(Schema.Type.INT)
  def longSchema: Schema = Schema.create(Schema.Type.LONG)
  def floatSchema: Schema = Schema.create(Schema.Type.FLOAT)
  def doubleSchema: Schema = Schema.create(Schema.Type.DOUBLE)
  def stringSchema: Schema = Schema.create(Schema.Type.STRING)
  def bytesSchema: Schema = Schema.create(Schema.Type.BYTES)

  def decimalSchema(precision: Int, scale: Int): Schema = {
    val schema = Schema.create(Schema.Type.BYTES)
    org.apache.avro.LogicalTypes.decimal(precision, scale).addToSchema(schema)
    schema
  }

  // Logical type schemas
  def uuidSchema: Schema = {
    val schema = Schema.create(Schema.Type.STRING)
    org.apache.avro.LogicalTypes.uuid().addToSchema(schema)
    schema
  }

  def timestampMillisSchema: Schema = {
    val schema = Schema.create(Schema.Type.LONG)
    org.apache.avro.LogicalTypes.timestampMillis().addToSchema(schema)
    schema
  }

  def dateSchema: Schema = {
    val schema = Schema.create(Schema.Type.INT)
    org.apache.avro.LogicalTypes.date().addToSchema(schema)
    schema
  }

  def timeMicrosSchema: Schema = {
    val schema = Schema.create(Schema.Type.LONG)
    org.apache.avro.LogicalTypes.timeMicros().addToSchema(schema)
    schema
  }

  def createFixed(name: String, namespace: String, size: Int): Schema =
    Schema.createFixed(name, null, namespace, size)

  def createEnum(name: String, namespace: String, symbols: util.List[String]): Schema =
    Schema.createEnum(name, null, namespace, symbols)

  def createEnumWithDefault(
      name: String,
      namespace: String,
      symbols: util.List[String],
      defaultSymbol: String
  ): Schema =
    Schema.createEnum(name, null, namespace, symbols, defaultSymbol)

  // --- Encoder helpers ---

  def encodeRecord(schema: Schema, fieldValues: util.List[(String, Any)]): GenericRecord = {
    val record = new GenericData.Record(schema)
    val iter = fieldValues.iterator()
    while (iter.hasNext) {
      val pair = iter.next()
      record.put(pair._1, pair._2)
    }
    record
  }

  def encodeOption[A](value: Option[A], encode: A => Any): Any = value match {
    case Some(a) => encode(a)
    case None    => null
  }

  def encodeIterable[A](items: Iterable[A], encode: A => Any): util.ArrayList[Any] = {
    val list = new util.ArrayList[Any]()
    items.foreach(a => list.add(encode(a)))
    list
  }

  def encodeMap[V](entries: Iterable[(String, V)], encodeValue: V => Any): util.HashMap[String, Any] = {
    val map = new util.HashMap[String, Any]()
    entries.foreach { case (k, v) => map.put(k, encodeValue(v)) }
    map
  }

  def encodeEnumSymbol(schema: Schema, name: String): GenericData.EnumSymbol =
    new GenericData.EnumSymbol(schema, name)

  def wrapByteArray(bytes: Array[Byte]): ByteBuffer = ByteBuffer.wrap(bytes)

  def wrapByteSeq(bytes: Seq[Byte]): ByteBuffer = ByteBuffer.wrap(bytes.toArray)

  // The schema of the FIXED values of fields annotated with `@avroFixed(size)`, one per size: built once, not for every
  // encoded value.
  private val anonymousFixedSchemas = new java.util.concurrent.ConcurrentHashMap[Integer, Schema]()
  private def anonymousFixedSchema(size: Int): Schema = {
    val key = Integer.valueOf(size)
    val cached = anonymousFixedSchemas.get(key)
    if (cached != null) cached
    else {
      val created = Schema.createFixed("fixed", null, null, size)
      val previous = anonymousFixedSchemas.putIfAbsent(key, created)
      if (previous != null) previous else created
    }
  }

  /** A FIXED value of `expectedSize` bytes: shorter input is padded with zeros (as avro4s does), longer is rejected. */
  def wrapByteArrayAsFixed(bytes: Array[Byte], expectedSize: Int): GenericData.Fixed = {
    if (bytes.length > expectedSize)
      throw new org.apache.avro.AvroRuntimeException(
        s"@avroFixed($expectedSize): expected at most $expectedSize bytes but got ${bytes.length}"
      )
    val padded = if (bytes.length == expectedSize) bytes else util.Arrays.copyOf(bytes, expectedSize)
    new GenericData.Fixed(anonymousFixedSchema(expectedSize), padded)
  }

  /** A FIXED value of `schema` (a named FIXED type, e.g. of a value class annotated with `@avroFixed`). */
  def wrapByteArrayAsFixed(bytes: Array[Byte], schema: Schema): GenericData.Fixed = {
    val size = schema.getFixedSize
    if (bytes.length > size)
      throw new org.apache.avro.AvroRuntimeException(
        s"@avroFixed($size): expected at most $size bytes but got ${bytes.length}"
      )
    new GenericData.Fixed(schema, if (bytes.length == size) bytes else util.Arrays.copyOf(bytes, size))
  }

  def wrapStringAsFixed(value: String, schema: Schema): GenericData.Fixed =
    wrapByteArrayAsFixed(value.getBytes(java.nio.charset.StandardCharsets.UTF_8), schema)

  /** The ENUM schema of a Java enum: `name`, `namespace` and no default, unless the enum carries the Java annotations
    * `@AvroJavaName`, `@AvroJavaNamespace`, `@AvroJavaProp` or (on a constant) `@AvroJavaEnumDefault` - read by
    * reflection, as avro4s reads its own (a Java enum cannot carry the Scala annotations). `symbols` are the symbols of
    * the constants, in the order of their ordinals.
    */
  def createJavaEnum(anyConstant: Any, name: String, namespace: String, symbols: util.List[String]): Schema = {
    import hearth.kindlings.avroderivation.annotations.{
      AvroJavaEnumDefault,
      AvroJavaName,
      AvroJavaNamespace,
      AvroJavaProp
    }
    val clazz: Class[?] = anyConstant.asInstanceOf[java.lang.Enum[?]].getDeclaringClass
    val nameAnnotation = clazz.getAnnotation(classOf[AvroJavaName])
    val namespaceAnnotation = clazz.getAnnotation(classOf[AvroJavaNamespace])
    val constants: Array[AnyRef] = clazz.getEnumConstants.asInstanceOf[Array[AnyRef]]
    var default: String = null
    var i = 0
    while (default == null && i < constants.length && i < symbols.size) {
      val constantName: String = constants(i).asInstanceOf[java.lang.Enum[?]].name
      if (clazz.getField(constantName).getAnnotation(classOf[AvroJavaEnumDefault]) != null) default = symbols.get(i)
      i += 1
    }
    val schema = Schema.createEnum(
      if (nameAnnotation != null) nameAnnotation.value else name,
      null,
      if (namespaceAnnotation != null) namespaceAnnotation.value else namespace,
      symbols,
      default
    )
    val propAnnotation = clazz.getAnnotation(classOf[AvroJavaProp])
    if (propAnnotation != null) schema.addProp(propAnnotation.key, propAnnotation.value: AnyRef)
    schema
  }

  /** The symbols of a `scala.Enumeration` or of a Java enum in the declaration order of `values` - the ids of the
    * enumeration's values, the ordinals of the enum's constants - as avro4s lists them. The order the macro sees the
    * values in is not reliable (by name for an enumeration; for a Java enum it can change between compilations).
    */
  def symbolsInDeclarationOrder(values: List[Any], symbols: List[String]): List[String] =
    values
      .zip(symbols)
      .sortBy {
        case (value: Enumeration#Value, _) => value.id
        case (value: java.lang.Enum[?], _) => value.ordinal
        case _                             => Int.MaxValue
      }
      .map(_._2)

  /** A String as a FIXED value: its UTF-8 bytes padded with zeros to `expectedSize` (as avro4s does). */
  def wrapStringAsFixed(value: String, expectedSize: Int): GenericData.Fixed =
    wrapByteArrayAsFixed(value.getBytes(java.nio.charset.StandardCharsets.UTF_8), expectedSize)

  def encodeBigDecimal(bd: BigDecimal, scale: Int): ByteBuffer = {
    val scaled = bd.bigDecimal.setScale(scale)
    ByteBuffer.wrap(scaled.unscaledValue.toByteArray)
  }

  def encodeEither[L, R](value: Either[L, R], encodeLeft: L => Any, encodeRight: R => Any): Any =
    value match {
      case Left(l)  => encodeLeft(l)
      case Right(r) => encodeRight(r)
    }

  // Logical type encoders
  def encodeUUID(uuid: java.util.UUID): Any = uuid.toString
  def encodeInstant(instant: java.time.Instant): Any = instant.toEpochMilli
  def encodeLocalDate(date: java.time.LocalDate): Any = date.toEpochDay.toInt
  def encodeLocalTime(time: java.time.LocalTime): Any = time.toNanoOfDay / 1000
  def encodeLocalDateTime(dt: java.time.LocalDateTime): Any =
    dt.toInstant(java.time.ZoneOffset.UTC).toEpochMilli
  def encodeOffsetDateTime(dt: java.time.OffsetDateTime): Any =
    dt.toInstant.toEpochMilli

  // --- Decoder helpers ---

  def checkIsRecord(value: Any): GenericRecord =
    value match {
      case r: GenericRecord => r
      case other            =>
        throw new org.apache.avro.AvroTypeException(
          s"Expected GenericRecord but got ${if (other == null) "null" else other.getClass.getSimpleName}"
        )
    }

  def decodeRecord(record: GenericRecord, fieldName: String): Any =
    record.get(fieldName)

  def getFieldByNameOrAlias(record: GenericRecord, name: String, aliases: List[String]): Any = {
    val schema = record.getSchema
    // One hash lookup (`getField`) + position access via `field.pos()`, instead of `getField(name)` followed by
    // `record.get(name)` — which internally repeats the `getField(name)` hash lookup. `field.pos()` is the field's
    // position in the *record's own* schema, so this stays correct for schema-evolved/reordered records (the case
    // that makes a compile-time constant index unsafe). Halves the per-field hash-lookup cost of the hot decode path.
    val field = schema.getField(name)
    if (field != null) return record.get(field.pos())
    var remaining = aliases
    while (remaining.nonEmpty) {
      val aliasField = schema.getField(remaining.head)
      if (aliasField != null) return record.get(aliasField.pos())
      remaining = remaining.tail
    }
    null
  }

  def getFieldByNameOrAlias(record: GenericRecord, name: String): Any = {
    val field = record.getSchema.getField(name)
    if (field != null) record.get(field.pos()) else null
  }

  def getFieldByNameOrAlias(record: GenericRecord, name: String, alias: String): Any = {
    val schema = record.getSchema
    val field = schema.getField(name)
    if (field != null) record.get(field.pos())
    else {
      val aliasField = schema.getField(alias)
      if (aliasField != null) record.get(aliasField.pos()) else null
    }
  }

  /** What `fieldOrMissing` returns when the record has no field with the name (nor any of the aliases). */
  val MissingField: AnyRef = new AnyRef {
    override def toString: String = "MissingField"
  }

  def isMissingField(value: Any): Boolean = value.asInstanceOf[AnyRef] eq MissingField

  /** Whether `NonFatal` would not match `error` (smaller than inlining the `NonFatal` extractor per decoded field). */
  def isFatal(error: Throwable): Boolean = !scala.util.control.NonFatal(error)

  /** The value of a record's field (found by name or alias), or [[MissingField]]: one lookup for a field with a
    * default, which decodes to the default when the record has no such field.
    */
  def fieldOrMissing(record: GenericRecord, name: String): Any = {
    val field = record.getSchema.getField(name)
    if (field != null) record.get(field.pos()) else MissingField
  }

  def fieldOrMissing(record: GenericRecord, name: String, alias: String): Any = {
    val schema = record.getSchema
    val field = schema.getField(name)
    if (field != null) record.get(field.pos())
    else {
      val aliasField = schema.getField(alias)
      if (aliasField != null) record.get(aliasField.pos()) else MissingField
    }
  }

  def fieldOrMissing(record: GenericRecord, name: String, aliases: List[String]): Any = {
    val schema = record.getSchema
    val field = schema.getField(name)
    if (field != null) return record.get(field.pos())
    var remaining = aliases
    while (remaining.nonEmpty) {
      val aliasField = schema.getField(remaining.head)
      if (aliasField != null) return record.get(aliasField.pos())
      remaining = remaining.tail
    }
    MissingField
  }

  @scala.annotation.nowarn("msg=unused explicit parameter")
  def unsafeCast[A](value: Any, schemaFor: hearth.kindlings.avroderivation.AvroSchemaFor[A]): A =
    value.asInstanceOf[A]

  def decodeOption[A](value: Any, decode: Any => A): Option[A] =
    if (value == null) None else Some(decode(value))

  /** The items of an Avro ARRAY value: Avro's readers produce a `java.util.Collection`; hand-built records may hold a
    * Java array or a Scala collection instead (accepted as avro4s' decoders accept them).
    */
  def asCollection(value: Any): util.Collection[Any] = value match {
    case collection: util.Collection[?] => collection.asInstanceOf[util.Collection[Any]]
    case array: Array[AnyRef]           => util.Arrays.asList(array*).asInstanceOf[util.Collection[Any]]
    case array: Array[?]                =>
      val list = new util.ArrayList[Any](array.length)
      var i = 0
      while (i < array.length) {
        val _ = list.add(array(i))
        i += 1
      }
      list
    case iterable: Iterable[?] =>
      val list = new util.ArrayList[Any]()
      iterable.foreach { item =>
        val _ = list.add(item)
      }
      list
    case other => cannotDecode(other, "ARRAY")
  }

  def decodeCollection[Item, Coll](
      value: Any,
      decodeItem: Any => Item,
      factory: scala.collection.Factory[Item, Coll]
  ): Coll = {
    val list = asCollection(value)
    val builder = factory.newBuilder
    val iter = list.iterator()
    while (iter.hasNext)
      builder += decodeItem(iter.next())
    builder.result()
  }

  def decodeMap[V, M](
      value: Any,
      decodeValue: Any => V,
      factory: scala.collection.Factory[(String, V), M]
  ): M = {
    val map = value.asInstanceOf[util.Map[CharSequence, Any]]
    val builder = factory.newBuilder
    val iter = map.entrySet().iterator()
    while (iter.hasNext) {
      val entry = iter.next()
      builder += ((entry.getKey.toString, decodeValue(entry.getValue)))
    }
    builder.result()
  }

  def decodeEnumSymbol(value: Any): String =
    value.toString

  def decodeFixed(value: Any): Array[Byte] = {
    val fixed = value.asInstanceOf[org.apache.avro.generic.GenericFixed]
    fixed.bytes().clone()
  }

  /** A String field annotated with `@avroFixed`: the FIXED value's bytes as UTF-8 (padding included, as avro4s). */
  def decodeFixedString(value: Any): String = decodeCharSequence(value)

  // Bytes are decoded from whatever Avro (or a hand-built record) holds them in - a ByteBuffer, a byte array or a
  // FIXED value - regardless of the schema, as avro4s' decoders do.

  def decodeToByteBuffer(value: Any): ByteBuffer = value match {
    case buffer: ByteBuffer                          => buffer
    case array: Array[Byte]                          => ByteBuffer.wrap(array)
    case fixed: org.apache.avro.generic.GenericFixed => ByteBuffer.wrap(fixed.bytes())
    case other                                       => cannotDecode(other, "BYTES")
  }

  def decodeByteBuffer(value: Any): Array[Byte] = value match {
    case bb: ByteBuffer =>
      val bytes = new Array[Byte](bb.remaining())
      bb.duplicate().get(bytes) // the decoded value keeps its position, so it can be decoded again
      bytes
    case array: Array[Byte]                          => array
    case fixed: org.apache.avro.generic.GenericFixed => fixed.bytes().clone()
    case other                                       => cannotDecode(other, "BYTES")
  }

  def decodeByteBufferToSeq(value: Any): Seq[Byte] = decodeByteBuffer(value).toSeq

  def decodeByteBufferToList(value: Any): List[Byte] = decodeByteBuffer(value).toList

  def decodeByteBufferToVector(value: Any): Vector[Byte] = decodeByteBuffer(value).toVector

  def decodeBigDecimal(value: Any, scale: Int): BigDecimal =
    BigDecimal(new java.math.BigDecimal(new java.math.BigInteger(decodeByteBuffer(value)), scale))

  def decodeEither[L, R](
      value: Any,
      unionSchema: Schema,
      decodeLeft: Any => L,
      decodeRight: Any => R
  ): Either[L, R] = {
    val index = GenericData.get().resolveUnion(unionSchema, value)
    if (index == 0) Left(decodeLeft(value))
    else Right(decodeRight(value))
  }

  /** Decodes an Either whose union was flattened from `leftSchema` and the right side's schema (either of which can be
    * a union itself, e.g. `Either[String, Option[Int]]` is `["null", "string", "int"]`): the value is a `Left` when the
    * union branch it resolves to comes from `leftSchema`.
    */
  def decodeEither[L, R](
      value: Any,
      unionSchema: Schema,
      leftSchema: Schema,
      decodeLeft: Any => L,
      decodeRight: Any => R
  ): Either[L, R] = {
    val branch = unionSchema.getTypes.get(GenericData.get().resolveUnion(unionSchema, value))
    if (isBranchOf(branch, leftSchema)) Left(decodeLeft(value))
    else Right(decodeRight(value))
  }

  private def sameBranch(a: Schema, b: Schema): Boolean =
    (a eq b) || (a.getType == b.getType && (a.getType match {
      case Schema.Type.RECORD | Schema.Type.ENUM | Schema.Type.FIXED => a.getFullName == b.getFullName
      case _                                                         => true
    }))

  private def isBranchOf(branch: Schema, schema: Schema): Boolean =
    if (schema.getType != Schema.Type.UNION) sameBranch(branch, schema)
    else {
      val types = schema.getTypes
      var i = 0
      var found = false
      while (!found && i < types.size) {
        found = sameBranch(branch, types.get(i))
        i += 1
      }
      found
    }

  /** A String from whatever holds it - a `CharSequence` (`Utf8`, `String`), or its UTF-8 bytes in a byte array, a
    * ByteBuffer or a FIXED value - regardless of the schema, as avro4s' decoder does. Anything else is rejected.
    */
  def decodeCharSequence(value: Any): String = value match {
    case chars: CharSequence => chars.toString
    case bytes: Array[Byte]  => new String(bytes, java.nio.charset.StandardCharsets.UTF_8)
    case buffer: ByteBuffer  =>
      val bytes = new Array[Byte](buffer.remaining())
      buffer.duplicate().get(bytes)
      new String(bytes, java.nio.charset.StandardCharsets.UTF_8)
    case fixed: org.apache.avro.generic.GenericFixed =>
      new String(fixed.bytes(), java.nio.charset.StandardCharsets.UTF_8)
    case other => cannotDecode(other, "STRING")
  }

  /** An Avro `Utf8` from a `Utf8` (as it is), a `String`, or UTF-8 bytes (as avro4s' decoder). */
  def decodeUtf8(value: Any): org.apache.avro.util.Utf8 = value match {
    case utf8: org.apache.avro.util.Utf8 => utf8
    case other                           => new org.apache.avro.util.Utf8(decodeCharSequence(other))
  }

  /** A `CharSequence`: a `CharSequence` as it is (so a `Utf8` stays a `Utf8`, as with avro4s), bytes as a `Utf8`. */
  def decodeAsCharSequence(value: Any): CharSequence = value match {
    case chars: CharSequence => chars
    case other               => new org.apache.avro.util.Utf8(decodeCharSequence(other))
  }

  // Logical type decoders
  def decodeUUID(value: Any): java.util.UUID = java.util.UUID.fromString(value.toString)
  def decodeInstant(value: Any): java.time.Instant = java.time.Instant.ofEpochMilli(value.asInstanceOf[Long])
  def decodeLocalDate(value: Any): java.time.LocalDate =
    java.time.LocalDate.ofEpochDay(decodeInt(value).toLong)
  def decodeLocalTime(value: Any): java.time.LocalTime =
    java.time.LocalTime.ofNanoOfDay(decodeLong(value) * 1000)
  def decodeLocalDateTime(value: Any): java.time.LocalDateTime =
    java.time.LocalDateTime
      .ofInstant(java.time.Instant.ofEpochMilli(value.asInstanceOf[Long]), java.time.ZoneOffset.UTC)
  def decodeOffsetDateTime(value: Any): java.time.OffsetDateTime =
    java.time.OffsetDateTime
      .ofInstant(java.time.Instant.ofEpochMilli(value.asInstanceOf[Long]), java.time.ZoneOffset.UTC)

  def sequenceDecodeResults(fieldValues: List[Any]): Array[Any] =
    fieldValues.toArray

  def failedToMatchSubtype(name: String, knownSubtypes: List[String]): Nothing =
    throw new IllegalArgumentException(
      s"Unknown Avro record type: $name. Expected one of: ${knownSubtypes.mkString(", ")}"
    )

  // --- Strict primitive decoders (avro4s' integral widening; `null` and other types are rejected) ---

  private def cannotDecode(value: Any, avroType: String): Nothing =
    throw new org.apache.avro.AvroTypeException(
      s"Cannot decode ${if (value == null) "null" else s"$value (${value.getClass.getName})"} as $avroType"
    )

  def decodeBoolean(value: Any): Boolean = value match {
    case b: java.lang.Boolean => b.booleanValue
    case other                => cannotDecode(other, "BOOLEAN")
  }

  def decodeInt(value: Any): Int = value match {
    case i: java.lang.Integer => i.intValue
    case s: java.lang.Short   => s.intValue
    case b: java.lang.Byte    => b.intValue
    case other                => cannotDecode(other, "INT")
  }

  def decodeLong(value: Any): Long = value match {
    case l: java.lang.Long    => l.longValue
    case i: java.lang.Integer => i.longValue
    case s: java.lang.Short   => s.longValue
    case b: java.lang.Byte    => b.longValue
    case other                => cannotDecode(other, "LONG")
  }

  def decodeFloat(value: Any): Float = value match {
    case f: java.lang.Float => f.floatValue
    case other              => cannotDecode(other, "FLOAT")
  }

  def decodeDouble(value: Any): Double = value match {
    case d: java.lang.Double => d.doubleValue
    case other               => cannotDecode(other, "DOUBLE")
  }

  def decodeByte(value: Any): Byte = value match {
    case b: java.lang.Byte    => b.byteValue
    case i: java.lang.Integer => i.byteValue
    case other                => cannotDecode(other, "INT (Byte)")
  }

  def decodeShort(value: Any): Short = value match {
    case s: java.lang.Short   => s.shortValue
    case b: java.lang.Byte    => b.shortValue
    case i: java.lang.Integer => i.shortValue
    case other                => cannotDecode(other, "INT (Short)")
  }

  // --- Record field lookup (schema evolution) ---

  /** Whether `record`'s schema has a field called `name` or one of `aliases`. */
  def hasFieldOrAlias(record: GenericRecord, name: String, aliases: List[String]): Boolean = {
    val schema = record.getSchema
    if (schema.getField(name) != null) true
    else aliases.exists(a => schema.getField(a) != null)
  }

  // --- Timestamps with configurable precision ---

  def timestampSchema(precision: TimestampPrecision): Schema = {
    val schema = Schema.create(Schema.Type.LONG)
    (precision match {
      case TimestampPrecision.Millis => org.apache.avro.LogicalTypes.timestampMillis()
      case TimestampPrecision.Micros => org.apache.avro.LogicalTypes.timestampMicros()
      case TimestampPrecision.Nanos  => org.apache.avro.LogicalTypes.timestampNanos()
    }).addToSchema(schema)
    schema
  }

  // Overflow is an ArithmeticException (as avro4s, which uses `Instant.toEpochMilli`), not a silently wrapped value.
  private def epochToLong(epochSecond: Long, nano: Int, precision: TimestampPrecision): Long = precision match {
    case TimestampPrecision.Millis => Math.addExact(Math.multiplyExact(epochSecond, 1000L), (nano / 1000000).toLong)
    case TimestampPrecision.Micros => Math.addExact(Math.multiplyExact(epochSecond, 1000000L), (nano / 1000).toLong)
    case TimestampPrecision.Nanos  => Math.addExact(Math.multiplyExact(epochSecond, 1000000000L), nano.toLong)
  }

  private def longToInstant(value: Any, precision: TimestampPrecision): java.time.Instant = {
    val l = decodeLong(value)
    precision match {
      case TimestampPrecision.Millis => java.time.Instant.ofEpochMilli(l)
      case TimestampPrecision.Micros =>
        java.time.Instant.ofEpochSecond(Math.floorDiv(l, 1000000L), Math.floorMod(l, 1000000L) * 1000L)
      case TimestampPrecision.Nanos =>
        java.time.Instant.ofEpochSecond(Math.floorDiv(l, 1000000000L), Math.floorMod(l, 1000000000L))
    }
  }

  def encodeInstant(instant: java.time.Instant, precision: TimestampPrecision): Any =
    epochToLong(instant.getEpochSecond, instant.getNano, precision)
  def decodeInstant(value: Any, precision: TimestampPrecision): java.time.Instant = longToInstant(value, precision)

  def encodeLocalDateTime(dt: java.time.LocalDateTime, precision: TimestampPrecision): Any =
    epochToLong(dt.toEpochSecond(java.time.ZoneOffset.UTC), dt.getNano, precision)
  def decodeLocalDateTime(value: Any, precision: TimestampPrecision): java.time.LocalDateTime =
    java.time.LocalDateTime.ofInstant(longToInstant(value, precision), java.time.ZoneOffset.UTC)

  def encodeSqlTimestamp(ts: java.sql.Timestamp, precision: TimestampPrecision): Any =
    encodeInstant(ts.toInstant, precision)
  def decodeSqlTimestamp(value: Any, precision: TimestampPrecision): java.sql.Timestamp =
    java.sql.Timestamp.from(longToInstant(value, precision))

  def encodeSqlDate(date: java.sql.Date): Any = date.toLocalDate.toEpochDay.toInt
  def decodeSqlDate(value: Any): java.sql.Date =
    java.sql.Date.valueOf(java.time.LocalDate.ofEpochDay(decodeInt(value).toLong))

  // --- OffsetDateTime: UTC timestamp-millis (default) or avro4s' ISO-8601 string ---

  def offsetDateTimeSchema(asIsoString: Boolean): Schema =
    if (asIsoString) {
      val schema = Schema.create(Schema.Type.STRING)
      // avro4s' custom logical type (not registered with Avro, so it is only a schema property)
      schema.addProp(org.apache.avro.LogicalType.LOGICAL_TYPE_PROP, "datetime-with-offset")
      schema
    } else timestampMillisSchema

  def encodeOffsetDateTime(dt: java.time.OffsetDateTime, asIsoString: Boolean): Any =
    if (asIsoString) dt.format(java.time.format.DateTimeFormatter.ISO_OFFSET_DATE_TIME)
    else dt.toInstant.toEpochMilli

  def decodeOffsetDateTime(value: Any, asIsoString: Boolean): java.time.OffsetDateTime =
    if (asIsoString)
      java.time.OffsetDateTime.parse(value.toString, java.time.format.DateTimeFormatter.ISO_OFFSET_DATE_TIME)
    else
      java.time.OffsetDateTime.ofInstant(java.time.Instant.ofEpochMilli(decodeLong(value)), java.time.ZoneOffset.UTC)

  // --- Unions ordered as avro4s orders sealed traits' subtypes ---

  /** A union of `schemas` sorted by descending priority (`@avroSortPriority`), then by full name, as avro4s does. */
  def createSortedUnion(schemas: List[Schema], priorities: List[Int]): Schema = {
    val sorted = schemas.zip(priorities).sortBy { case (s, p) => (-p, s.getFullName) }.map(_._1)
    val javaSchemas = new util.ArrayList[Schema](sorted.size)
    sorted.foreach(javaSchemas.add)
    Schema.createUnion(javaSchemas)
  }

  // --- Scala default values as schema defaults (AvroConfig.scalaDefaultsInSchema) ---

  /** A field whose default is the Scala default value `default`, converted to the JSON-like value Avro expects for
    * `schema` (a union is reordered so that the default's branch comes first, as Avro requires and avro4s does). When
    * the value cannot be represented for this schema, the field gets no default.
    */
  def createFieldWithScalaDefault(name: String, schema: Schema, doc: String, default: Any): Schema.Field =
    AvroScalaDefaults.resolve(default, schema) match {
      case Some((resolvedSchema, value)) => new Schema.Field(name, resolvedSchema, doc, value)
      case None                          => new Schema.Field(name, schema, doc)
    }

  // --- Type name helpers ---

  def parseAvroName(fullTypeName: String): String = {
    val bracketIdx = fullTypeName.indexOf('[')
    if (bracketIdx < 0) shortenToAvroName(fullTypeName)
    else {
      val baseName = shortenToAvroName(fullTypeName.substring(0, bracketIdx))
      val paramsStr = fullTypeName.substring(bracketIdx + 1, fullTypeName.length - 1)
      val params = splitTopLevelTypeParams(paramsStr).map(parseAvroName)
      // as avro4s: `Name__Arg1_Arg2` (a double underscore after the name, single underscores between the arguments)
      if (params.isEmpty) baseName else baseName + "__" + params.mkString("_")
    }
  }

  private def splitTopLevelTypeParams(s: String): List[String] = {
    val result = List.newBuilder[String]
    var depth = 0
    var start = 0
    var i = 0
    while (i < s.length) {
      s.charAt(i) match {
        case '['               => depth += 1
        case ']'               => depth -= 1
        case ',' if depth == 0 =>
          result += s.substring(start, i).trim
          start = i + 1
        case _ =>
      }
      i += 1
    }
    if (start < s.length) result += s.substring(start).trim
    result.result()
  }

  def parseAvroNameFqn(fullTypeName: String): String = {
    val bracketIdx = fullTypeName.indexOf('[')
    if (bracketIdx < 0) shortenToAvroName(fullTypeName)
    else {
      val baseName = shortenToAvroName(fullTypeName.substring(0, bracketIdx))
      val paramsStr = fullTypeName.substring(bracketIdx + 1, fullTypeName.length - 1)
      val params = splitTopLevelTypeParams(paramsStr).map(parseAvroParamFqn)
      if (params.isEmpty) baseName else (baseName :: params).mkString("__")
    }
  }

  private def parseAvroParamFqn(fullTypeName: String): String = {
    val bracketIdx = fullTypeName.indexOf('[')
    if (bracketIdx < 0) fqnToAvroName(fullTypeName)
    else {
      val baseFqn = fqnToAvroName(fullTypeName.substring(0, bracketIdx))
      val paramsStr = fullTypeName.substring(bracketIdx + 1, fullTypeName.length - 1)
      val params = splitTopLevelTypeParams(paramsStr).map(parseAvroParamFqn)
      if (params.isEmpty) baseFqn else (baseFqn :: params).mkString("__")
    }
  }

  private def stripTypeSuffix(name: String): String =
    if (name.endsWith(".type")) name.substring(0, name.length - 5) else name

  private def shortenToAvroName(fullName: String): String = {
    val stripped = stripTypeSuffix(fullName)
    val dotIdx = stripped.lastIndexOf('.')
    val name = if (dotIdx < 0) stripped else stripped.substring(dotIdx + 1)
    NonAvroNameChars.matcher(name).replaceAll("_")
  }

  private def fqnToAvroName(fullName: String): String =
    NonAvroNameChars.matcher(stripTypeSuffix(fullName)).replaceAll("_")

  // compiled once (`String.replaceAll` compiles its regex on every call)
  private val NonAvroNameChars: java.util.regex.Pattern = java.util.regex.Pattern.compile("[^A-Za-z0-9_]")

  // --- Field-level @avroNamespace ---

  /** `schema` with the namespace of every named type in it (records, enums, fixed - through unions, arrays, maps and
    * record fields) replaced by `namespace`, as avro4s does for a field annotated with `@AvroNamespace`
    * (`SchemaHelper.overrideNamespace`). Unlike avro4s, docs, defaults, aliases, properties and logical types are all
    * kept, and recursive schemas are supported.
    */
  def overrideNamespace(schema: Schema, namespace: String): Schema =
    overrideNamespace(schema, namespace, new util.IdentityHashMap[Schema, Schema]())

  private def copyProps(from: org.apache.avro.JsonProperties, to: org.apache.avro.JsonProperties): Unit = {
    val it = from.getObjectProps.entrySet().iterator()
    while (it.hasNext) {
      val e = it.next()
      if (to.getObjectProp(e.getKey) == null) to.addProp(e.getKey, e.getValue)
    }
  }

  private def copyNamedExtras(from: Schema, to: Schema): Schema = {
    val logicalType = from.getLogicalType
    if (logicalType != null) { val _ = logicalType.addToSchema(to) }
    copyProps(from, to)
    val aliases = from.getAliases.iterator()
    while (aliases.hasNext) to.addAlias(aliases.next())
    to
  }

  private def hasFields(record: Schema): Boolean =
    try { record.getFields; true }
    catch { case _: org.apache.avro.AvroRuntimeException => false }

  private def overrideNamespace(schema: Schema, namespace: String, seen: util.IdentityHashMap[Schema, Schema]): Schema =
    schema.getType match {
      case Schema.Type.RECORD =>
        val done = seen.get(schema)
        if (done != null) done
        // a reference to a record that is still being built (a recursive type): left as it is
        else if (!hasFields(schema)) schema
        else {
          val copy = Schema.createRecord(schema.getName, schema.getDoc, namespace, schema.isError)
          val _ = seen.put(schema, copy)
          val fields = new util.ArrayList[Schema.Field](schema.getFields.size)
          val it = schema.getFields.iterator()
          while (it.hasNext) {
            val f = it.next()
            val field =
              new Schema.Field(f.name, overrideNamespace(f.schema, namespace, seen), f.doc, f.defaultVal(), f.order)
            copyProps(f, field)
            val aliases = f.aliases.iterator()
            while (aliases.hasNext) field.addAlias(aliases.next())
            val _ = fields.add(field)
          }
          copy.setFields(fields)
          copyNamedExtras(schema, copy)
        }
      case Schema.Type.UNION =>
        val types = new util.ArrayList[Schema](schema.getTypes.size)
        val it = schema.getTypes.iterator()
        while (it.hasNext) { val _ = types.add(overrideNamespace(it.next(), namespace, seen)) }
        Schema.createUnion(types)
      case Schema.Type.ENUM =>
        copyNamedExtras(
          schema,
          Schema.createEnum(schema.getName, schema.getDoc, namespace, schema.getEnumSymbols, schema.getEnumDefault)
        )
      case Schema.Type.FIXED =>
        copyNamedExtras(schema, Schema.createFixed(schema.getName, schema.getDoc, namespace, schema.getFixedSize))
      case Schema.Type.MAP =>
        val copy = Schema.createMap(overrideNamespace(schema.getValueType, namespace, seen))
        copyProps(schema, copy)
        copy
      case Schema.Type.ARRAY =>
        val copy = Schema.createArray(overrideNamespace(schema.getElementType, namespace, seen))
        copyProps(schema, copy)
        copy
      case _ => schema
    }

  /** `schema` as an error record (`@avroError` on a field, as avro4s' `@AvroError`): a record is copied with the error
    * flag set; any other schema is returned as it is.
    */
  def asErrorRecord(schema: Schema): Schema =
    if (schema.getType != Schema.Type.RECORD || schema.isError || !hasFields(schema)) schema
    else {
      val copy = Schema.createRecord(schema.getName, schema.getDoc, schema.getNamespace, true)
      val fields = new util.ArrayList[Schema.Field](schema.getFields.size)
      val it = schema.getFields.iterator()
      while (it.hasNext) {
        val f = it.next()
        val _ = fields.add(new Schema.Field(f, f.schema))
      }
      copy.setFields(fields)
      copyNamedExtras(schema, copy)
    }

  /** Rebinds an encoded value to `schema`: the records, enum symbols and fixed values in it (built with the schemas of
    * their own types) are re-created with the corresponding schemas found in `schema`, matched by simple name. Used for
    * fields annotated with `@avroNamespace`, whose named types live in another namespace than the one their encoders
    * know: Avro resolves union branches by the full name of the datum's schema.
    */
  def rebindToSchema(value: Any, schema: Schema): Any =
    if (value == null) null
    else
      schema.getType match {
        case Schema.Type.RECORD =>
          value match {
            case record: GenericRecord if record.getSchema ne schema =>
              val fields = schema.getFields
              val copy = new GenericData.Record(schema)
              var i = 0
              while (i < fields.size) {
                copy.put(i, rebindToSchema(record.get(i), fields.get(i).schema))
                i += 1
              }
              copy
            case _ => value
          }
        case Schema.Type.ENUM =>
          value match {
            case symbol: GenericData.EnumSymbol if symbol.getSchema ne schema =>
              new GenericData.EnumSymbol(schema, symbol.toString)
            case _ => value
          }
        case Schema.Type.FIXED =>
          value match {
            case fixed: org.apache.avro.generic.GenericFixed if fixed.getSchema ne schema =>
              new GenericData.Fixed(schema, fixed.bytes())
            case _ => value
          }
        case Schema.Type.ARRAY =>
          value match {
            case items: util.Collection[?] =>
              val copy = new util.ArrayList[Any](items.size)
              val it = items.iterator()
              while (it.hasNext) { val _ = copy.add(rebindToSchema(it.next(), schema.getElementType)) }
              copy
            case _ => value
          }
        case Schema.Type.MAP =>
          value match {
            case entries: util.Map[?, ?] =>
              val copy = new util.HashMap[Any, Any](entries.size * 2)
              val it = entries.entrySet().iterator()
              while (it.hasNext) {
                val e = it.next()
                val _ = copy.put(e.getKey, rebindToSchema(e.getValue, schema.getValueType))
              }
              copy
            case _ => value
          }
        case Schema.Type.UNION =>
          val branch = unionBranchFor(value, schema)
          if (branch == null) value else rebindToSchema(value, branch)
        case _ => value
      }

  /** The branch of `union` a container value belongs to (named types by simple name), or null for other values. */
  private def unionBranchFor(value: Any, union: Schema): Schema = {
    val (tpe, name) = value match {
      case c: org.apache.avro.generic.GenericContainer if c.getSchema.getType == Schema.Type.RECORD =>
        (Schema.Type.RECORD, c.getSchema.getName)
      case c: org.apache.avro.generic.GenericContainer if c.getSchema.getType == Schema.Type.ENUM =>
        (Schema.Type.ENUM, c.getSchema.getName)
      case c: org.apache.avro.generic.GenericContainer if c.getSchema.getType == Schema.Type.FIXED =>
        (Schema.Type.FIXED, c.getSchema.getName)
      case _: util.Collection[?] => (Schema.Type.ARRAY, null)
      case _: util.Map[?, ?]     => (Schema.Type.MAP, null)
      case _                     => (null, null)
    }
    if (tpe == null) null
    else {
      val types = union.getTypes
      var i = 0
      var found: Schema = null
      while (found == null && i < types.size) {
        val candidate = types.get(i)
        if (candidate.getType == tpe && (name == null || candidate.getName == name)) found = candidate
        i += 1
      }
      found
    }
  }

  /** Reads a case class field that has no accessor (`private val`) through `Product`. */
  def productElement(value: Any, index: Int): Any = value.asInstanceOf[Product].productElement(index)
}
