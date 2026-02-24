package hearth.kindlings.avroderivation.internal.runtime

import org.apache.avro.Schema
import org.apache.avro.generic.{GenericData, GenericRecord}

import java.nio.ByteBuffer
import java.util

object AvroDerivationUtils {

  // --- Schema helpers ---

  def createRecord(name: String, namespace: String, fields: util.List[Schema.Field]): Schema = {
    val schema = Schema.createRecord(name, null, namespace, false)
    schema.setFields(fields)
    schema
  }

  def createField(name: String, schema: Schema): Schema.Field =
    new Schema.Field(name, schema)

  def createUnion(schemas: Schema*): Schema =
    Schema.createUnion(schemas*)

  def nullSchema: Schema = Schema.create(Schema.Type.NULL)

  // Primitive schema constructors — Java enum constants don't reify inside Scala 2 macro quotes
  def booleanSchema: Schema = Schema.create(Schema.Type.BOOLEAN)
  def intSchema: Schema = Schema.create(Schema.Type.INT)
  def longSchema: Schema = Schema.create(Schema.Type.LONG)
  def floatSchema: Schema = Schema.create(Schema.Type.FLOAT)
  def doubleSchema: Schema = Schema.create(Schema.Type.DOUBLE)
  def stringSchema: Schema = Schema.create(Schema.Type.STRING)
  def bytesSchema: Schema = Schema.create(Schema.Type.BYTES)

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

  def createEnum(name: String, namespace: String, symbols: util.List[String]): Schema =
    Schema.createEnum(name, null, namespace, symbols)

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

  // Logical type encoders
  def encodeUUID(uuid: java.util.UUID): Any = uuid.toString
  def encodeInstant(instant: java.time.Instant): Any = instant.toEpochMilli
  def encodeLocalDate(date: java.time.LocalDate): Any = date.toEpochDay.toInt
  def encodeLocalTime(time: java.time.LocalTime): Any = time.toNanoOfDay / 1000
  def encodeLocalDateTime(dt: java.time.LocalDateTime): Any =
    dt.toInstant(java.time.ZoneOffset.UTC).toEpochMilli

  // --- Decoder helpers ---

  def decodeRecord(record: GenericRecord, fieldName: String): Any =
    record.get(fieldName)

  @scala.annotation.nowarn("msg=unused explicit parameter")
  def unsafeCast[A](value: Any, schemaFor: hearth.kindlings.avroderivation.AvroSchemaFor[A]): A =
    value.asInstanceOf[A]

  def decodeOption[A](value: Any, decode: Any => A): Option[A] =
    if (value == null) None else Some(decode(value))

  def decodeCollection[Item, Coll](
      value: Any,
      decodeItem: Any => Item,
      factory: scala.collection.Factory[Item, Coll]
  ): Coll = {
    val list = value.asInstanceOf[util.Collection[Any]]
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

  def decodeByteBuffer(value: Any): Array[Byte] = {
    val bb = value.asInstanceOf[ByteBuffer]
    val bytes = new Array[Byte](bb.remaining())
    bb.get(bytes)
    bytes
  }

  def decodeCharSequence(value: Any): String =
    value.toString

  // Logical type decoders
  def decodeUUID(value: Any): java.util.UUID = java.util.UUID.fromString(value.toString)
  def decodeInstant(value: Any): java.time.Instant = java.time.Instant.ofEpochMilli(value.asInstanceOf[Long])
  def decodeLocalDate(value: Any): java.time.LocalDate =
    java.time.LocalDate.ofEpochDay(value.asInstanceOf[Int].toLong)
  def decodeLocalTime(value: Any): java.time.LocalTime =
    java.time.LocalTime.ofNanoOfDay(value.asInstanceOf[Long] * 1000)
  def decodeLocalDateTime(value: Any): java.time.LocalDateTime =
    java.time.LocalDateTime
      .ofInstant(java.time.Instant.ofEpochMilli(value.asInstanceOf[Long]), java.time.ZoneOffset.UTC)

  def sequenceDecodeResults(fieldValues: List[Any]): Array[Any] =
    fieldValues.toArray

  def failedToMatchSubtype(name: String, knownSubtypes: List[String]): Nothing =
    throw new IllegalArgumentException(
      s"Unknown Avro record type: $name. Expected one of: ${knownSubtypes.mkString(", ")}"
    )
}
