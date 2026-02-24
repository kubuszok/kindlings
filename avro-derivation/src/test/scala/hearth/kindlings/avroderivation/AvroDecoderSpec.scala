package hearth.kindlings.avroderivation

import hearth.MacroSuite
import org.apache.avro.generic.GenericData

import java.nio.ByteBuffer

final class AvroDecoderSpec extends MacroSuite {

  group("AvroDecoder") {

    group("primitive types") {

      test("Int") {
        val result = AvroDecoder.decode[Int](42: Any)
        result ==> 42
      }

      test("Long") {
        val result = AvroDecoder.decode[Long](42L: Any)
        result ==> 42L
      }

      test("Double") {
        val result = AvroDecoder.decode[Double](3.14: Any)
        result ==> 3.14
      }

      test("Float") {
        val result = AvroDecoder.decode[Float](1.5f: Any)
        result ==> 1.5f
      }

      test("Boolean") {
        val result = AvroDecoder.decode[Boolean](true: Any)
        result ==> true
      }

      test("String from Utf8") {
        val result = AvroDecoder.decode[String](new org.apache.avro.util.Utf8("hello"): Any)
        result ==> "hello"
      }

      test("Byte from Int") {
        val result = AvroDecoder.decode[Byte](42: Any)
        result ==> 42.toByte
      }

      test("Short from Int") {
        val result = AvroDecoder.decode[Short](42: Any)
        result ==> 42.toShort
      }

      test("Char from CharSequence") {
        val result = AvroDecoder.decode[Char](new org.apache.avro.util.Utf8("x"): Any)
        result ==> 'x'
      }

      test("Array[Byte] from ByteBuffer") {
        val bytes = Array[Byte](1, 2, 3)
        val result = AvroDecoder.decode[Array[Byte]](ByteBuffer.wrap(bytes): Any)
        result.toList ==> bytes.toList
      }

      test("BigDecimal from String") {
        val result = AvroDecoder.decode[BigDecimal](new org.apache.avro.util.Utf8("3.14"): Any)
        result ==> BigDecimal("3.14")
      }
    }

    group("case classes") {

      test("simple case class from GenericRecord") {
        val schema = AvroSchemaFor.schemaOf[SimplePerson]
        val record = new GenericData.Record(schema)
        record.put("name", "Alice")
        record.put("age", 30)
        val result = AvroDecoder.decode[SimplePerson](record: Any)
        result ==> SimplePerson("Alice", 30)
      }

      test("empty case class") {
        val schema = AvroSchemaFor.schemaOf[EmptyClass]
        val record = new GenericData.Record(schema)
        val result = AvroDecoder.decode[EmptyClass](record: Any)
        result ==> EmptyClass()
      }

      test("nested case class") {
        val personSchema = AvroSchemaFor.schemaOf[PersonWithAddress]
        val addressSchema = personSchema.getField("address").schema()
        val addressRecord = new GenericData.Record(addressSchema)
        addressRecord.put("street", "Main St")
        addressRecord.put("city", "NYC")
        val personRecord = new GenericData.Record(personSchema)
        personRecord.put("name", "Bob")
        personRecord.put("age", 25)
        personRecord.put("address", addressRecord)
        val result = AvroDecoder.decode[PersonWithAddress](personRecord: Any)
        result ==> PersonWithAddress("Bob", 25, Address("Main St", "NYC"))
      }
    }

    group("value classes") {

      test("value class decodes underlying") {
        val result = AvroDecoder.decode[WrappedInt](42: Any)
        result ==> WrappedInt(42)
      }
    }

    group("Option") {

      test("decode Some") {
        val result = AvroDecoder.decode[Option[Int]](42: Any)
        result ==> Some(42)
      }

      test("decode None from null") {
        val result = AvroDecoder.decode[Option[Int]](null: Any)
        result ==> None
      }
    }

    group("collections") {

      test("List from java Collection") {
        val javaList = new java.util.ArrayList[Any]()
        javaList.add(1)
        javaList.add(2)
        javaList.add(3)
        val result = AvroDecoder.decode[List[Int]](javaList: Any)
        result ==> List(1, 2, 3)
      }

      test("Vector from java Collection") {
        val javaList = new java.util.ArrayList[Any]()
        javaList.add("a")
        javaList.add("b")
        val result = AvroDecoder.decode[Vector[String]](javaList: Any)
        result ==> Vector("a", "b")
      }
    }

    group("maps") {

      test("Map[String, V] from java Map") {
        val javaMap = new java.util.HashMap[CharSequence, Any]()
        javaMap.put("a", 1)
        javaMap.put("b", 2)
        val result = AvroDecoder.decode[Map[String, Int]](javaMap: Any)
        result ==> Map("a" -> 1, "b" -> 2)
      }
    }

    group("sealed traits - case objects") {

      test("enum from GenericData.EnumSymbol") {
        val schema = AvroSchemaFor.schemaOf[Color]
        val symbol = new GenericData.EnumSymbol(schema, "Red")
        val result = AvroDecoder.decode[Color](symbol: Any)
        result ==> Red
      }
    }

    group("sealed traits - case classes") {

      test("union record decoded by schema name") {
        val schema = AvroSchemaFor.schemaOf[Shape]
        val circleSchema = schema.getTypes.get(0) // Circle
        val record = new GenericData.Record(circleSchema)
        record.put("radius", 5.0)
        val result = AvroDecoder.decode[Shape](record: Any)
        result ==> Circle(5.0)
      }
    }

    group("generic case classes") {

      test("Box[Int] from GenericRecord") {
        val schema = AvroSchemaFor.schemaOf[Box[Int]]
        val record = new GenericData.Record(schema)
        record.put("value", 42)
        val result = AvroDecoder.decode[Box[Int]](record: Any)
        result ==> Box(42)
      }

      test("Pair[String, Int] from GenericRecord") {
        val schema = AvroSchemaFor.schemaOf[Pair[String, Int]]
        val record = new GenericData.Record(schema)
        record.put("first", "hello")
        record.put("second", 42)
        val result = AvroDecoder.decode[Pair[String, Int]](record: Any)
        result ==> Pair("hello", 42)
      }
    }

    group("deeply nested") {

      test("PersonFull with 3-level nesting") {
        val personSchema = AvroSchemaFor.schemaOf[PersonFull]
        val addressSchema = personSchema.getField("address").schema()
        val geoSchema = addressSchema.getField("geo").schema()

        val geoRecord = new GenericData.Record(geoSchema)
        geoRecord.put("lat", 40.7)
        geoRecord.put("lon", -74.0)

        val addressRecord = new GenericData.Record(addressSchema)
        addressRecord.put("street", "123 Main")
        addressRecord.put("city", "NYC")
        addressRecord.put("geo", geoRecord)

        val personRecord = new GenericData.Record(personSchema)
        personRecord.put("name", "Alice")
        personRecord.put("address", addressRecord)

        val result = AvroDecoder.decode[PersonFull](personRecord: Any)
        result ==> PersonFull("Alice", FullAddress("123 Main", "NYC", GeoCoordinates(40.7, -74.0)))
      }
    }

    group("type aliases") {

      test("WithAlias from GenericRecord") {
        val schema = AvroSchemaFor.schemaOf[WithAlias]
        val record = new GenericData.Record(schema)
        record.put("name", "Alice")
        record.put("age", 30)
        val result = AvroDecoder.decode[WithAlias](record: Any)
        result ==> WithAlias("Alice", 30)
      }
    }

    group("logical types") {

      test("UUID from String") {
        val result = AvroDecoder.decode[java.util.UUID]("550e8400-e29b-41d4-a716-446655440000": Any)
        result ==> java.util.UUID.fromString("550e8400-e29b-41d4-a716-446655440000")
      }

      test("UUID from Utf8") {
        val result =
          AvroDecoder.decode[java.util.UUID](new org.apache.avro.util.Utf8("550e8400-e29b-41d4-a716-446655440000"): Any)
        result ==> java.util.UUID.fromString("550e8400-e29b-41d4-a716-446655440000")
      }

      test("Instant from Long") {
        val result = AvroDecoder.decode[java.time.Instant](1700000000000L: Any)
        result ==> java.time.Instant.ofEpochMilli(1700000000000L)
      }

      test("LocalDate from Int") {
        val epochDay = java.time.LocalDate.of(2024, 1, 15).toEpochDay.toInt
        val result = AvroDecoder.decode[java.time.LocalDate](epochDay: Any)
        result ==> java.time.LocalDate.of(2024, 1, 15)
      }

      test("LocalTime from Long (micros)") {
        val time = java.time.LocalTime.of(14, 30, 0)
        val micros = time.toNanoOfDay / 1000
        val result = AvroDecoder.decode[java.time.LocalTime](micros: Any)
        result ==> time
      }

      test("LocalDateTime from Long (epoch millis)") {
        val dt = java.time.LocalDateTime.of(2024, 1, 15, 14, 30, 0)
        val millis = dt.toInstant(java.time.ZoneOffset.UTC).toEpochMilli
        val result = AvroDecoder.decode[java.time.LocalDateTime](millis: Any)
        result ==> dt
      }
    }

    group("per-field annotations") {

      test("@fieldName decodes from custom field name") {
        val schema = AvroSchemaFor.schemaOf[AvroWithFieldName]
        val record = new GenericData.Record(schema)
        record.put("user_name", "Alice")
        record.put("age", 30)
        val result = AvroDecoder.decode[AvroWithFieldName](record: Any)
        result ==> AvroWithFieldName("Alice", 30)
      }

      test("@transientField uses default value during decoding") {
        val schema = AvroSchemaFor.schemaOf[AvroWithTransient]
        val record = new GenericData.Record(schema)
        record.put("name", "Alice")
        val result = AvroDecoder.decode[AvroWithTransient](record: Any)
        result ==> AvroWithTransient("Alice", None)
      }

      test("@fieldName and @transientField combined") {
        val schema = AvroSchemaFor.schemaOf[AvroWithBothAnnotations]
        val record = new GenericData.Record(schema)
        record.put("display_name", "Alice")
        record.put("active", true)
        val result = AvroDecoder.decode[AvroWithBothAnnotations](record: Any)
        result ==> AvroWithBothAnnotations("Alice", 0, true)
      }
    }

    group("derived instance") {

      test("derive creates AvroDecoder instance") {
        val decoder = AvroDecoder.derive[SimplePerson]
        val schema = decoder.schema
        val record = new GenericData.Record(schema)
        record.put("name", "Test")
        record.put("age", 99)
        val result = decoder.decode(record)
        result ==> SimplePerson("Test", 99)
      }
    }
  }
}
