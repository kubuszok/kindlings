package hearth.kindlings.avroderivation

import hearth.MacroSuite
import hearth.kindlings.avroderivation.annotations.{avroDefault, avroFixed, avroNamespace, avroNoDefault}
import hearth.kindlings.avroderivation.internal.runtime.{AvroDerivationUtils, AvroScalaDefaults}
import org.apache.avro.{JsonProperties, Schema}
import org.apache.avro.generic.GenericData

import scala.jdk.CollectionConverters.*

// Fixtures for the avro4s-parity behaviours (checked against avro4s itself in upstream-oracle-tests).

case class ParLegacy(date: java.sql.Date, timestamp: java.sql.Timestamp)
case class ParTimes(instant: java.time.Instant, dateTime: java.time.LocalDateTime)
case class ParOffset(at: java.time.OffsetDateTime)
case class ParWithDefaults(
    name: String,
    nickname: Option[String] = None,
    score: Int = 10,
    lucky: Option[Int] = Some(7),
    tags: List[String] = List("a"),
    meta: Map[String, Int] = Map("k" -> 1),
    color: Color = Red,
    shape: Shape = Circle(1.0),
    address: Address = Address("Main", "NYC"),
    @avroDefault("42") annotated: Int = 1,
    @avroNoDefault suppressed: Int = 2
)
case class ParFixed(@avroFixed(2) token: Array[Byte])
case class ParFixedNs(@avroFixed(2) @avroNamespace("other.ns") token: Array[Byte])
object ParWeekday extends Enumeration {
  val Mon, Tue = Value
}
case class ParUsesWeekday(day: ParWeekday.Value)
sealed trait ParMixed
case object ParNothing extends ParMixed
case class ParSomething(value: Int) extends ParMixed
case class ParLong(value: Long)
case class ParPrimitives(b: Boolean, i: Int, l: Long, f: Float, d: Double, by: Byte, s: Short)

final class AvroUpstreamParitySpec extends MacroSuite {

  private def roundTrip[A](value: A, encoder: AvroEncoder[A], decoder: AvroDecoder[A]): A =
    AvroIO.fromBinary[A](AvroIO.toBinary(value)(encoder))(decoder)

  group("java.sql types (as avro4s)") {

    test("java.sql.Date is a date int, java.sql.Timestamp a timestamp-millis long") {
      val schema = AvroSchemaFor.schemaOf[ParLegacy]
      schema.getField("date").schema().getLogicalType.getName ==> "date"
      schema.getField("timestamp").schema().getLogicalType.getName ==> "timestamp-millis"
      val value = ParLegacy(java.sql.Date.valueOf("2024-01-15"), new java.sql.Timestamp(1700000000123L))
      val record = AvroEncoder.encode(value).asInstanceOf[GenericData.Record]
      record.get("date") ==> java.time.LocalDate.of(2024, 1, 15).toEpochDay.toInt
      record.get("timestamp") ==> 1700000000123L
      val encoder: AvroEncoder[ParLegacy] = AvroEncoder.derived[ParLegacy]
      val decoder: AvroDecoder[ParLegacy] = AvroDecoder.derived[ParLegacy]
      roundTrip(value, encoder, decoder) ==> value
    }
  }

  group("timestamp precision") {

    test("defaults: Instant in millis, LocalDateTime in nanos (as avro4s)") {
      val schema = AvroSchemaFor.schemaOf[ParTimes]
      schema.getField("instant").schema().getLogicalType.getName ==> "timestamp-millis"
      schema.getField("dateTime").schema().getLogicalType.getName ==> "timestamp-nanos"
      val encoder: AvroEncoder[ParTimes] = AvroEncoder.derived[ParTimes]
      val decoder: AvroDecoder[ParTimes] = AvroDecoder.derived[ParTimes]
      val value = ParTimes(
        java.time.Instant.ofEpochMilli(1700000000123L),
        java.time.LocalDateTime.of(2024, 1, 15, 1, 2, 3, 456789123)
      )
      roundTrip(value, encoder, decoder) ==> value
    }

    test("micros and nanos") {
      implicit val config: AvroConfig = AvroConfig.default
        .withInstantPrecision(TimestampPrecision.Micros)
        .withLocalDateTimePrecision(TimestampPrecision.Micros)
      val schema = AvroSchemaFor.schemaOf[ParTimes]
      schema.getField("instant").schema().getLogicalType.getName ==> "timestamp-micros"
      schema.getField("dateTime").schema().getLogicalType.getName ==> "timestamp-micros"
      val instant = java.time.Instant.ofEpochSecond(1700000000L, 123456789L)
      val record = AvroEncoder
        .encode(ParTimes(instant, java.time.LocalDateTime.of(1969, 12, 31, 23, 59, 59, 999999000)))
        .asInstanceOf[GenericData.Record]
      record.get("instant") ==> 1700000000123456L
      record.get("dateTime") ==> -1L
      AvroDecoder.decode[ParTimes](record).dateTime ==> java.time.LocalDateTime.of(1969, 12, 31, 23, 59, 59, 999999000)
      AvroDecoder.decode[ParTimes](record).instant ==> java.time.Instant.ofEpochSecond(1700000000L, 123456000L)
    }

    test("nanos") {
      implicit val config: AvroConfig = AvroConfig.default.withInstantPrecision(TimestampPrecision.Nanos)
      val instant = java.time.Instant.ofEpochSecond(1700000000L, 123456789L)
      AvroSchemaFor.schemaOf[java.time.Instant].getLogicalType.getName ==> "timestamp-nanos"
      AvroEncoder.encode(instant) ==> 1700000000123456789L
      AvroDecoder.decode[java.time.Instant](1700000000123456789L: Any) ==> instant
    }
  }

  group("OffsetDateTime") {

    test("default: UTC timestamp-millis") {
      AvroSchemaFor.schemaOf[ParOffset].getField("at").schema().getLogicalType.getName ==> "timestamp-millis"
    }

    test("withOffsetDateTimeAsIsoString: avro4s' ISO-8601 string keeping the offset") {
      implicit val config: AvroConfig = AvroConfig.default.withOffsetDateTimeAsIsoString
      val fieldSchema = AvroSchemaFor.schemaOf[ParOffset].getField("at").schema()
      fieldSchema.getType ==> Schema.Type.STRING
      fieldSchema.getProp("logicalType") ==> "datetime-with-offset"
      val value = ParOffset(java.time.OffsetDateTime.parse("2024-01-15T10:30:15.123+02:00"))
      AvroEncoder.encode(value).asInstanceOf[GenericData.Record].get("at") ==> "2024-01-15T10:30:15.123+02:00"
      val encoder: AvroEncoder[ParOffset] = AvroEncoder.derived[ParOffset]
      val decoder: AvroDecoder[ParOffset] = AvroDecoder.derived[ParOffset]
      roundTrip(value, encoder, decoder) ==> value
    }
  }

  group("decoding (as avro4s)") {

    test("integral widening: Int/Short/Byte decode as Long, Byte/Short as Int") {
      AvroDecoder.decode[Long](Integer.valueOf(3): Any) ==> 3L
      AvroDecoder.decode[Long](java.lang.Short.valueOf(3.toShort): Any) ==> 3L
      AvroDecoder.decode[Int](java.lang.Byte.valueOf(3.toByte): Any) ==> 3
      val schema = new Schema.Parser()
        .parse("""{"type":"record","name":"ParLong","fields":[{"name":"value","type":"int"}]}""")
      val record = new GenericData.Record(schema)
      record.put("value", 5)
      AvroDecoder.decode[ParLong](record) ==> ParLong(5L)
    }

    test("null and values of other types are rejected for primitives (instead of decoding to 0)") {
      val _ = intercept[org.apache.avro.AvroTypeException](AvroDecoder.decode[Int](null: Any))
      val _ = intercept[org.apache.avro.AvroTypeException](AvroDecoder.decode[Long]("1": Any))
      val _ =
        intercept[org.apache.avro.AvroTypeException](AvroDecoder.decode[Double](java.lang.Float.valueOf(1.5f): Any))
      val _ = intercept[org.apache.avro.AvroTypeException](AvroDecoder.decode[Boolean](null: Any))
      val record = AvroEncoder.encode(ParPrimitives(true, 1, 2L, 3f, 4d, 5, 6))
      AvroDecoder.decode[ParPrimitives](record) ==> ParPrimitives(true, 1, 2L, 3f, 4d, 5, 6)
    }

    test("missing fields with Scala defaults decode to the defaults") {
      val schema = new Schema.Parser().parse(
        """{"type":"record","name":"ParWithDefaults","fields":[{"name":"name","type":"string"}]}"""
      )
      val record = new GenericData.Record(schema)
      record.put("name", "a")
      AvroDecoder.decode[ParWithDefaults](record) ==> ParWithDefaults("a")
    }

    test("fields with Scala defaults that cannot be decoded decode to the defaults") {
      val schema = new Schema.Parser().parse(
        """{"type":"record","name":"ParWithDefaults","fields":[{"name":"name","type":"string"},{"name":"score","type":"string"}]}"""
      )
      val record = new GenericData.Record(schema)
      record.put("name", "a")
      record.put("score", "high")
      AvroDecoder.decode[ParWithDefaults](record).score ==> 10
    }

    test("a missing field without a default is rejected") {
      val schema = new Schema.Parser().parse(
        """{"type":"record","name":"SimplePerson","fields":[{"name":"name","type":"string"}]}"""
      )
      val record = new GenericData.Record(schema)
      record.put("name", "a")
      val _ = intercept[org.apache.avro.AvroTypeException](AvroDecoder.decode[SimplePerson](record))
    }
  }

  group("Scala defaults as schema defaults") {

    test("withoutScalaDefaultsInSchema: only @avroDefault becomes a schema default") {
      implicit val config: AvroConfig = AvroConfig.default.withoutScalaDefaultsInSchema
      val schema = AvroSchemaFor.schemaOf[ParWithDefaults]
      schema.getFields.asScala.filter(_.hasDefaultValue).map(_.name).toList ==> List("annotated")
    }

    test("default (as avro4s): Scala defaults are schema defaults, with the default's union branch first") {
      val schema = AvroSchemaFor.schemaOf[ParWithDefaults]
      def default(name: String): Any = schema.getField(name).defaultVal()
      schema.getField("name").hasDefaultValue ==> false
      default("nickname") ==> JsonProperties.NULL_VALUE
      schema.getField("nickname").schema().getTypes.get(0).getType ==> Schema.Type.NULL
      default("score") ==> 10
      default("lucky") ==> 7
      schema.getField("lucky").schema().getTypes.asScala.map(_.getType).toList ==> List(
        Schema.Type.INT,
        Schema.Type.NULL
      )
      default("tags") ==> List("a").asJava
      default("meta") ==> Map("k" -> 1).asJava
      default("color") ==> "Red"
      default("shape") ==> Map("radius" -> 1.0).asJava
      schema.getField("shape").schema().getTypes.get(0).getName ==> "Circle"
      default("address") ==> Map("street" -> "Main", "city" -> "NYC").asJava
      default("annotated") ==> 42
      schema.getField("suppressed").hasDefaultValue ==> false
      // the default's branch first is only in this field's schema: the shared union keeps its order
      AvroSchemaFor.schemaOf[Shape].getTypes.asScala.map(_.getName).toList ==> List("Circle", "Rectangle")
    }

    test("schema evolution: old data reads with the reader schema's defaults") {
      val writer = new Schema.Parser().parse(
        """{"type":"record","name":"ParWithDefaults","namespace":"hearth.kindlings.avroderivation","fields":[{"name":"name","type":"string"},{"name":"suppressed","type":"int"}]}"""
      )
      val old = new GenericData.Record(writer)
      old.put("name", "a")
      old.put("suppressed", 3)
      val reader = AvroSchemaFor.schemaOf[ParWithDefaults]
      val bytes = {
        val out = new java.io.ByteArrayOutputStream()
        val enc = org.apache.avro.io.EncoderFactory.get().binaryEncoder(out, null)
        new org.apache.avro.generic.GenericDatumWriter[Any](writer).write(old, enc)
        enc.flush()
        out.toByteArray
      }
      val read = new org.apache.avro.generic.GenericDatumReader[Any](writer, reader)
        .read(null, org.apache.avro.io.DecoderFactory.get().binaryDecoder(bytes, null))
      AvroDecoder.decode[ParWithDefaults](read) ==> ParWithDefaults("a", annotated = 42, suppressed = 3)
    }

    test("AvroScalaDefaults: values that cannot be represented give no default") {
      val intSchema = Schema.create(Schema.Type.INT)
      AvroScalaDefaults.resolve("not an int", intSchema) ==> None
      AvroDerivationUtils.createFieldWithScalaDefault("f", intSchema, null, "x").hasDefaultValue ==> false
      AvroScalaDefaults
        .resolve(java.util.UUID.fromString("550e8400-e29b-41d4-a716-446655440000"), AvroDerivationUtils.uuidSchema)
        .map(_._2) ==> Some("550e8400-e29b-41d4-a716-446655440000")
      AvroScalaDefaults
        .resolve(BigDecimal("1.50"), AvroDerivationUtils.decimalSchema(4, 2))
        .map(_._2.asInstanceOf[Array[Byte]].toList) ==>
        Some(BigInt(150).toByteArray.toList)
    }
  }

  group("schema names and namespaces (as avro4s)") {

    test("generic names: Name__Arg1_Arg2") {
      AvroDerivationUtils.parseAvroName("a.Pair[scala.Predef.String, b.Box[scala.Int]]") ==> "Pair__String_Box__Int"
    }

    test("@avroFixed: in the enclosing record's namespace, or the field's @avroNamespace") {
      AvroSchemaFor
        .schemaOf[ParFixed]
        .getField("token")
        .schema()
        .getFullName ==> "hearth.kindlings.avroderivation.token"
      AvroSchemaFor.schemaOf[ParFixedNs].getField("token").schema().getFullName ==> "other.ns.token"
    }

    test("scala.Enumeration: named after the enumeration object, in its package") {
      val schema = AvroSchemaFor.schemaOf[ParUsesWeekday].getField("day").schema()
      schema.getFullName ==> "hearth.kindlings.avroderivation.ParWeekday"
      schema.getEnumSymbols.asScala.toList ==> List("Mon", "Tue")
    }

    test("case objects in a UNION are records in their package") {
      val union = AvroSchemaFor.schemaOf[ParMixed]
      union.getTypes.asScala.map(_.getFullName).toList ==>
        List("hearth.kindlings.avroderivation.ParNothing", "hearth.kindlings.avroderivation.ParSomething")
    }
  }
}
