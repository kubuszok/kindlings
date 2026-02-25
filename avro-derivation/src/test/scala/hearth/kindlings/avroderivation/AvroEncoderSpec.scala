package hearth.kindlings.avroderivation

import hearth.MacroSuite
import org.apache.avro.generic.{GenericData, GenericRecord}

import java.nio.ByteBuffer

final class AvroEncoderSpec extends MacroSuite {

  group("AvroEncoder") {

    group("primitive types") {

      test("Int") {
        val result = AvroEncoder.encode(42)
        result ==> 42
      }

      test("Long") {
        val result = AvroEncoder.encode(42L)
        result ==> 42L
      }

      test("Double") {
        val result = AvroEncoder.encode(3.14)
        result ==> 3.14
      }

      test("Float") {
        val result = AvroEncoder.encode(1.5f)
        result ==> 1.5f
      }

      test("Boolean") {
        val result = AvroEncoder.encode(true)
        result ==> true
      }

      test("String") {
        val result = AvroEncoder.encode("hello")
        result ==> "hello"
      }

      test("Byte maps to Int") {
        val result = AvroEncoder.encode(42.toByte)
        result ==> 42
      }

      test("Short maps to Int") {
        val result = AvroEncoder.encode(42.toShort)
        result ==> 42
      }

      test("Char maps to String") {
        val result = AvroEncoder.encode('x')
        result ==> "x"
      }

      test("Array[Byte] maps to ByteBuffer") {
        val bytes = Array[Byte](1, 2, 3)
        val result = AvroEncoder.encode(bytes)
        result.isInstanceOf[ByteBuffer] ==> true
        val bb = result.asInstanceOf[ByteBuffer]
        val arr = new Array[Byte](bb.remaining())
        bb.get(arr)
        arr.toList ==> bytes.toList
      }

      test("BigDecimal maps to String") {
        val result = AvroEncoder.encode(BigDecimal("3.14"))
        result ==> "3.14"
      }
    }

    group("case classes") {

      test("simple case class encodes to GenericRecord") {
        val result = AvroEncoder.encode(SimplePerson("Alice", 30))
        result.isInstanceOf[GenericRecord] ==> true
        val record = result.asInstanceOf[GenericRecord]
        record.get("name").toString ==> "Alice"
        record.get("age").asInstanceOf[Int] ==> 30
      }

      test("empty case class") {
        val result = AvroEncoder.encode(EmptyClass())
        result.isInstanceOf[GenericRecord] ==> true
      }

      test("nested case class") {
        val result = AvroEncoder.encode(PersonWithAddress("Bob", 25, Address("Main St", "NYC")))
        result.isInstanceOf[GenericRecord] ==> true
        val record = result.asInstanceOf[GenericRecord]
        record.get("name").toString ==> "Bob"
        val addressRecord = record.get("address").asInstanceOf[GenericRecord]
        addressRecord.get("street").toString ==> "Main St"
        addressRecord.get("city").toString ==> "NYC"
      }
    }

    group("value classes") {

      test("value class encodes underlying") {
        val result = AvroEncoder.encode(WrappedInt(42))
        result ==> 42
      }
    }

    group("Option") {

      test("Some encodes inner value") {
        val result = AvroEncoder.encode(Option(42))
        result ==> 42
      }

      test("None encodes to null") {
        val result = AvroEncoder.encode(Option.empty[Int])
        result ==> null
      }
    }

    group("collections") {

      test("List encodes to java ArrayList") {
        val result = AvroEncoder.encode(List(1, 2, 3))
        result.isInstanceOf[java.util.ArrayList[?]] ==> true
        val list = result.asInstanceOf[java.util.ArrayList[Int]]
        list.size() ==> 3
        list.get(0) ==> 1
        list.get(1) ==> 2
        list.get(2) ==> 3
      }

      test("List of case classes") {
        val result = AvroEncoder.encode(List(SimplePerson("A", 1), SimplePerson("B", 2)))
        result.isInstanceOf[java.util.ArrayList[?]] ==> true
        val list = result.asInstanceOf[java.util.ArrayList[GenericRecord]]
        list.size() ==> 2
        list.get(0).get("name").toString ==> "A"
      }
    }

    group("maps") {

      test("Map[String, V] encodes to java HashMap") {
        val result = AvroEncoder.encode(Map("a" -> 1, "b" -> 2))
        result.isInstanceOf[java.util.HashMap[?, ?]] ==> true
        val map = result.asInstanceOf[java.util.HashMap[String, Int]]
        map.get("a") ==> 1
        map.get("b") ==> 2
      }
    }

    group("sealed traits - case objects") {

      test("case object encodes to EnumSymbol") {
        val result = AvroEncoder.encode[Color](Red)
        result.isInstanceOf[GenericData.EnumSymbol] ==> true
        result.toString ==> "Red"
      }
    }

    group("sealed traits - case classes") {

      test("case class subtype encodes to GenericRecord") {
        val result = AvroEncoder.encode[Shape](Circle(5.0))
        result.isInstanceOf[GenericRecord] ==> true
        val record = result.asInstanceOf[GenericRecord]
        record.getSchema.getName ==> "Circle"
        record.get("radius").asInstanceOf[Double] ==> 5.0
      }
    }

    group("derived instance") {

      test("derive creates AvroEncoder instance") {
        val encoder = AvroEncoder.derive[SimplePerson]
        val result = encoder.encode(SimplePerson("Test", 99))
        result.isInstanceOf[GenericRecord] ==> true
        val record = result.asInstanceOf[GenericRecord]
        record.get("name").toString ==> "Test"
        record.get("age").asInstanceOf[Int] ==> 99
      }
    }

    group("generic case classes") {

      test("Box[Int] encodes to GenericRecord") {
        val result = AvroEncoder.encode(Box(42))
        result.isInstanceOf[GenericRecord] ==> true
        val record = result.asInstanceOf[GenericRecord]
        record.get("value").asInstanceOf[Int] ==> 42
      }

      test("Pair[String, Int] encodes to GenericRecord") {
        val result = AvroEncoder.encode(Pair("hello", 42))
        result.isInstanceOf[GenericRecord] ==> true
        val record = result.asInstanceOf[GenericRecord]
        record.get("first").toString ==> "hello"
        record.get("second").asInstanceOf[Int] ==> 42
      }
    }

    group("deeply nested") {

      test("PersonFull with 3-level nesting") {
        val result =
          AvroEncoder.encode(PersonFull("Alice", FullAddress("123 Main", "NYC", GeoCoordinates(40.7, -74.0))))
        result.isInstanceOf[GenericRecord] ==> true
        val record = result.asInstanceOf[GenericRecord]
        record.get("name").toString ==> "Alice"
        val addressRecord = record.get("address").asInstanceOf[GenericRecord]
        addressRecord.get("street").toString ==> "123 Main"
        val geoRecord = addressRecord.get("geo").asInstanceOf[GenericRecord]
        geoRecord.get("lat").asInstanceOf[Double] ==> 40.7
        geoRecord.get("lon").asInstanceOf[Double] ==> -74.0
      }
    }

    group("type aliases") {

      test("WithAlias encodes to GenericRecord") {
        val result = AvroEncoder.encode(WithAlias("Alice", 30))
        result.isInstanceOf[GenericRecord] ==> true
        val record = result.asInstanceOf[GenericRecord]
        record.get("name").toString ==> "Alice"
        record.get("age").asInstanceOf[Int] ==> 30
      }
    }

    group("logical types") {

      test("UUID encodes to String") {
        val uuid = java.util.UUID.fromString("550e8400-e29b-41d4-a716-446655440000")
        val result = AvroEncoder.encode(uuid)
        result ==> "550e8400-e29b-41d4-a716-446655440000"
      }

      test("Instant encodes to Long (epoch millis)") {
        val instant = java.time.Instant.ofEpochMilli(1700000000000L)
        val result = AvroEncoder.encode(instant)
        result ==> 1700000000000L
      }

      test("LocalDate encodes to Int (epoch day)") {
        val date = java.time.LocalDate.of(2024, 1, 15)
        val result = AvroEncoder.encode(date)
        result ==> date.toEpochDay.toInt
      }

      test("LocalTime encodes to Long (micros)") {
        val time = java.time.LocalTime.of(14, 30, 0)
        val result = AvroEncoder.encode(time)
        result ==> (time.toNanoOfDay / 1000)
      }

      test("LocalDateTime encodes to Long (epoch millis UTC)") {
        val dt = java.time.LocalDateTime.of(2024, 1, 15, 14, 30, 0)
        val result = AvroEncoder.encode(dt)
        result ==> dt.toInstant(java.time.ZoneOffset.UTC).toEpochMilli
      }
    }

    group("per-field annotations") {

      test("@fieldName encodes with custom field name") {
        val result = AvroEncoder.encode(AvroWithFieldName("Alice", 30))
        result.isInstanceOf[GenericRecord] ==> true
        val record = result.asInstanceOf[GenericRecord]
        record.get("user_name").toString ==> "Alice"
        record.get("age").asInstanceOf[Int] ==> 30
      }

      test("@transientField excludes field from encoding") {
        val result = AvroEncoder.encode(AvroWithTransient("Alice", Some("cached")))
        result.isInstanceOf[GenericRecord] ==> true
        val record = result.asInstanceOf[GenericRecord]
        record.get("name").toString ==> "Alice"
        record.getSchema.getField("cache") ==> null
      }

      test("@fieldName and @transientField combined") {
        val result = AvroEncoder.encode(AvroWithBothAnnotations("Alice", 42, true))
        result.isInstanceOf[GenericRecord] ==> true
        val record = result.asInstanceOf[GenericRecord]
        record.get("display_name").toString ==> "Alice"
        record.get("active").asInstanceOf[Boolean] ==> true
        record.getSchema.getField("internal") ==> null
      }

      test("@fieldName overrides config transform") {
        implicit val config: AvroConfig = AvroConfig().withSnakeCaseFieldNames
        val result = AvroEncoder.encode(AvroWithFieldName("Alice", 30))
        result.isInstanceOf[GenericRecord] ==> true
        val record = result.asInstanceOf[GenericRecord]
        record.get("user_name").toString ==> "Alice"
      }
    }

    group("tuples") {

      test("Tuple2 encodes to GenericRecord") {
        val result = AvroEncoder.encode(("hello", 42))
        result.isInstanceOf[GenericRecord] ==> true
        val record = result.asInstanceOf[GenericRecord]
        record.get("_1").toString ==> "hello"
        record.get("_2").asInstanceOf[Int] ==> 42
      }

      test("Tuple3 encodes to GenericRecord") {
        val result = AvroEncoder.encode((1, "world", true))
        result.isInstanceOf[GenericRecord] ==> true
        val record = result.asInstanceOf[GenericRecord]
        record.get("_1").asInstanceOf[Int] ==> 1
        record.get("_2").toString ==> "world"
        record.get("_3").asInstanceOf[Boolean] ==> true
      }
    }

    group("configuration") {

      test("snake_case field names") {
        implicit val config: AvroConfig = AvroConfig().withSnakeCaseFieldNames
        val result = AvroEncoder.encode(PersonWithAddress("Bob", 25, Address("Main St", "NYC")))
        result.isInstanceOf[GenericRecord] ==> true
        // Fields should still be accessible (by their transformed names)
        val record = result.asInstanceOf[GenericRecord]
        record.getSchema.getField("name").schema().getType ==> org.apache.avro.Schema.Type.STRING
      }
    }
  }
}
