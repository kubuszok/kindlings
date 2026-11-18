package hearth.kindlings.avroderivation

import hearth.MacroSuite
import hearth.kindlings.avroderivation.annotations.avroDoc
import hearth.kindlings.avroderivation.internal.runtime.{AvroDerivationUtils, AvroScalaDefaults}
import org.apache.avro.{JsonProperties, LogicalTypes, Schema, SchemaBuilder}

import scala.jdk.CollectionConverters.*

case class SdId(value: String) extends AnyVal
case class SdDocumented(@avroDoc("the count") count: Int = 3)

/** Unit tests of the conversion of Scala default values into Avro schema defaults (`AvroScalaDefaults`), the strict
  * primitive decoders and `AvroConfig`'s builders.
  */
final class AvroScalaDefaultsSpec extends MacroSuite {

  private def schema(tpe: Schema.Type): Schema = Schema.create(tpe)
  private def withLogical(tpe: Schema.Type, logicalType: org.apache.avro.LogicalType): Schema =
    logicalType.addToSchema(Schema.create(tpe))
  private def value(v: Any, s: Schema): Option[Any] = AvroScalaDefaults.resolve(v, s).map(_._2)

  private val instant = java.time.Instant.ofEpochSecond(1700000000L, 123456789L)

  group("AvroScalaDefaults: primitives") {

    test("booleans, integral and floating-point numbers") {
      value(true, schema(Schema.Type.BOOLEAN)) ==> Some(true)
      value(3.toShort, schema(Schema.Type.INT)) ==> Some(3)
      value(4.toByte, schema(Schema.Type.INT)) ==> Some(4)
      value(5L, schema(Schema.Type.LONG)) ==> Some(5L)
      value(6, schema(Schema.Type.LONG)) ==> Some(6L)
      value(1.5f, schema(Schema.Type.FLOAT)) ==> Some(1.5f)
      value(2.5f, schema(Schema.Type.DOUBLE)) ==> Some(2.5d)
      value(true, schema(Schema.Type.INT)) ==> None
    }

    test("value classes are their underlying value") {
      value(SdId("x"), schema(Schema.Type.STRING)) ==> Some("x")
    }
  }

  group("AvroScalaDefaults: temporal types") {

    test("dates and times") {
      val date = java.time.LocalDate.of(2024, 1, 15)
      value(date, AvroDerivationUtils.dateSchema) ==> Some(date.toEpochDay.toInt)
      value(java.sql.Date.valueOf(date), AvroDerivationUtils.dateSchema) ==> Some(date.toEpochDay.toInt)
      value(java.time.LocalTime.of(1, 2, 3), AvroDerivationUtils.timeMicrosSchema) ==> Some(3723000000L)
    }

    test("timestamps follow the logical type's precision") {
      value(instant, withLogical(Schema.Type.LONG, LogicalTypes.timestampMillis())) ==> Some(1700000000123L)
      value(instant, withLogical(Schema.Type.LONG, LogicalTypes.timestampMicros())) ==> Some(1700000000123456L)
      value(instant, withLogical(Schema.Type.LONG, LogicalTypes.timestampNanos())) ==> Some(1700000000123456789L)
      value(java.sql.Timestamp.from(instant), AvroDerivationUtils.timestampMillisSchema) ==> Some(1700000000123L)
      val dateTime = java.time.LocalDateTime.ofInstant(instant, java.time.ZoneOffset.UTC)
      value(dateTime, AvroDerivationUtils.timestampSchema(TimestampPrecision.Nanos)) ==> Some(1700000000123456789L)
    }

    test("OffsetDateTime as a timestamp or as avro4s' ISO-8601 string") {
      val at = java.time.OffsetDateTime.parse("2024-01-15T10:30:15.123+02:00")
      value(at, AvroDerivationUtils.offsetDateTimeSchema(asIsoString = false)) ==> Some(at.toInstant.toEpochMilli)
      value(at, AvroDerivationUtils.offsetDateTimeSchema(asIsoString = true)) ==> Some("2024-01-15T10:30:15.123+02:00")
    }
  }

  group("AvroScalaDefaults: bytes, enums and containers") {

    test("bytes from arrays, ByteBuffers and byte sequences") {
      def bytes(v: Any, s: Schema): Option[List[Byte]] = value(v, s).map(_.asInstanceOf[Array[Byte]].toList)
      bytes(Array[Byte](1, 2), schema(Schema.Type.BYTES)) ==> Some(List[Byte](1, 2))
      bytes(Array[Byte](1, 2), Schema.createFixed("f", null, null, 2)) ==> Some(List[Byte](1, 2))
      bytes(java.nio.ByteBuffer.wrap(Array[Byte](3)), schema(Schema.Type.BYTES)) ==> Some(List[Byte](3))
      bytes(Vector[Byte](4, 5), schema(Schema.Type.BYTES)) ==> Some(List[Byte](4, 5))
    }

    test("enum symbols of Java enums, scala.Enumeration values and case objects") {
      val enumSchema = Schema.createEnum("E", null, null, List("RED", "Mon", "Red").asJava)
      value(JavaColor.RED, enumSchema) ==> Some("RED")
      value(ScalaColor.withName("Red"), enumSchema) ==> Some("Red")
      value(Red, enumSchema) ==> Some("Red")
      value(Green, enumSchema) ==> None
    }

    test("arrays, maps and nested unions") {
      val ints = Schema.createArray(schema(Schema.Type.INT))
      value(Array(1, 2), ints) ==> Some(List(1, 2).asJava)
      value(List[Any](1, "x"), ints) ==> None
      val optionalInts = Schema.createArray(Schema.createUnion(schema(Schema.Type.NULL), schema(Schema.Type.INT)))
      value(List(Some(1), None), optionalInts) ==> Some(List[Any](1, JsonProperties.NULL_VALUE).asJava)
      value(Map("a" -> 1), Schema.createMap(schema(Schema.Type.INT))) ==> Some(Map("a" -> 1).asJava)
    }

    test("records: by field name, by position when renamed, else no default") {
      val renamed = SchemaBuilder.record("Address").fields().requiredString("s").requiredString("c").endRecord()
      value(Address("Main", "NYC"), renamed) ==> Some(Map("s" -> "Main", "c" -> "NYC").asJava)
      val tooFew = SchemaBuilder.record("Address").fields().requiredString("s").endRecord()
      value(Address("Main", "NYC"), tooFew) ==> None
    }

    test("a documented field with a Scala default keeps its doc") {
      val field = AvroSchemaFor.schemaOf[SdDocumented].getField("count")
      field.doc ==> "the count"
      field.defaultVal() ==> 3
    }
  }

  group("scalaDefaultsInSchema, known at compile time or only at runtime") {

    // `config` is a parameter, so the macro cannot evaluate it and chooses at runtime
    def schemaWith(config: AvroConfig): Schema = {
      implicit val runtimeConfig: AvroConfig = config
      AvroSchemaFor.schemaOf[SdDocumented]
    }

    test("a config known at compile time") {
      val withDefaults = AvroSchemaFor.schemaOf[SdDocumented].getField("count")
      withDefaults.doc ==> "the count"
      withDefaults.defaultVal() ==> 3
      val field = {
        implicit val withoutDefaults: AvroConfig = AvroConfig.default.withoutScalaDefaultsInSchema
        AvroSchemaFor.schemaOf[SdDocumented].getField("count")
      }
      field.doc ==> "the count"
      field.hasDefaultValue ==> false
    }

    test("a config known only at runtime") {
      val withDefaults = schemaWith(AvroConfig.default).getField("count")
      withDefaults.doc ==> "the count"
      withDefaults.defaultVal() ==> 3
      val withoutDefaults = schemaWith(AvroConfig.default.withoutScalaDefaultsInSchema).getField("count")
      withoutDefaults.doc ==> "the count"
      withoutDefaults.hasDefaultValue ==> false
    }
  }

  group("strict primitive decoders") {

    test("widening and rejections") {
      AvroDerivationUtils.decodeInt(java.lang.Short.valueOf(1.toShort)) ==> 1
      AvroDerivationUtils.decodeLong(java.lang.Byte.valueOf(2.toByte)) ==> 2L
      AvroDerivationUtils.decodeByte(java.lang.Byte.valueOf(3.toByte)) ==> 3.toByte
      AvroDerivationUtils.decodeShort(java.lang.Short.valueOf(4.toShort)) ==> 4.toShort
      AvroDerivationUtils.decodeShort(java.lang.Byte.valueOf(5.toByte)) ==> 5.toShort
      val _ = intercept[org.apache.avro.AvroTypeException](AvroDerivationUtils.decodeFloat(1.5d))
      val _ = intercept[org.apache.avro.AvroTypeException](AvroDerivationUtils.decodeByte("x"))
      val _ = intercept[org.apache.avro.AvroTypeException](AvroDerivationUtils.decodeShort(1L))
    }
  }

  group("AvroConfig") {

    test("withScalaDefaultsInSchema / withoutScalaDefaultsInSchema") {
      AvroConfig.default.scalaDefaultsInSchema ==> true
      AvroConfig.default.withoutScalaDefaultsInSchema.withScalaDefaultsInSchema.scalaDefaultsInSchema ==> true
    }

    test("positional arguments of the 0.3.x fields still compile, the new fields get their defaults") {
      val viaApply = AvroConfig(Some("ns"), identity, identity, None)
      val viaConstructor = new AvroConfig(Some("ns"), identity, identity, None)
      val custom = AvroConfig.default.withInstantPrecision(TimestampPrecision.Micros)
      val viaCopy = custom.copy(Some("ns"), custom.transformFieldNames, custom.transformConstructorNames, None)
      viaApply.namespace ==> Some("ns")
      viaApply.scalaDefaultsInSchema ==> true
      viaConstructor.localDateTimePrecision ==> TimestampPrecision.Nanos
      viaCopy.namespace ==> Some("ns")
      viaCopy.instantPrecision ==> TimestampPrecision.Micros
    }
  }
}
