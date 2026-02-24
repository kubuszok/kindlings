package hearth.kindlings.avroderivation

import hearth.MacroSuite
import org.apache.avro.generic.{GenericData, GenericRecord}

import java.nio.ByteBuffer

final class AvroEncoderSpec extends MacroSuite {

  group("AvroEncoder") {

    group("primitive types") {

      test("Int") {
        val result = AvroEncoder.encode(42)
        assertEquals(result, 42)
      }

      test("Long") {
        val result = AvroEncoder.encode(42L)
        assertEquals(result, 42L)
      }

      test("Double") {
        val result = AvroEncoder.encode(3.14)
        assertEquals(result, 3.14)
      }

      test("Float") {
        val result = AvroEncoder.encode(1.5f)
        assertEquals(result, 1.5f)
      }

      test("Boolean") {
        val result = AvroEncoder.encode(true)
        assertEquals(result, true)
      }

      test("String") {
        val result = AvroEncoder.encode("hello")
        assertEquals(result, "hello")
      }

      test("Byte maps to Int") {
        val result = AvroEncoder.encode(42.toByte)
        assertEquals(result, 42)
      }

      test("Short maps to Int") {
        val result = AvroEncoder.encode(42.toShort)
        assertEquals(result, 42)
      }

      test("Char maps to String") {
        val result = AvroEncoder.encode('x')
        assertEquals(result, "x")
      }

      test("Array[Byte] maps to ByteBuffer") {
        val bytes = Array[Byte](1, 2, 3)
        val result = AvroEncoder.encode(bytes)
        assert(result.isInstanceOf[ByteBuffer])
        val bb = result.asInstanceOf[ByteBuffer]
        val arr = new Array[Byte](bb.remaining())
        bb.get(arr)
        assertEquals(arr.toList, bytes.toList)
      }

      test("BigDecimal maps to String") {
        val result = AvroEncoder.encode(BigDecimal("3.14"))
        assertEquals(result, "3.14")
      }
    }

    group("case classes") {

      test("simple case class encodes to GenericRecord") {
        val result = AvroEncoder.encode(SimplePerson("Alice", 30))
        assert(result.isInstanceOf[GenericRecord])
        val record = result.asInstanceOf[GenericRecord]
        assertEquals(record.get("name").toString, "Alice")
        assertEquals(record.get("age").asInstanceOf[Int], 30)
      }

      test("empty case class") {
        val result = AvroEncoder.encode(EmptyClass())
        assert(result.isInstanceOf[GenericRecord])
      }

      test("nested case class") {
        val result = AvroEncoder.encode(PersonWithAddress("Bob", 25, Address("Main St", "NYC")))
        assert(result.isInstanceOf[GenericRecord])
        val record = result.asInstanceOf[GenericRecord]
        assertEquals(record.get("name").toString, "Bob")
        val addressRecord = record.get("address").asInstanceOf[GenericRecord]
        assertEquals(addressRecord.get("street").toString, "Main St")
        assertEquals(addressRecord.get("city").toString, "NYC")
      }
    }

    group("value classes") {

      test("value class encodes underlying") {
        val result = AvroEncoder.encode(WrappedInt(42))
        assertEquals(result, 42)
      }
    }

    group("Option") {

      test("Some encodes inner value") {
        val result = AvroEncoder.encode(Option(42))
        assertEquals(result, 42)
      }

      test("None encodes to null") {
        val result = AvroEncoder.encode(Option.empty[Int])
        assertEquals(result, null)
      }
    }

    group("collections") {

      test("List encodes to java ArrayList") {
        val result = AvroEncoder.encode(List(1, 2, 3))
        assert(result.isInstanceOf[java.util.ArrayList[?]])
        val list = result.asInstanceOf[java.util.ArrayList[Int]]
        assertEquals(list.size(), 3)
        assertEquals(list.get(0), 1)
        assertEquals(list.get(1), 2)
        assertEquals(list.get(2), 3)
      }

      test("List of case classes") {
        val result = AvroEncoder.encode(List(SimplePerson("A", 1), SimplePerson("B", 2)))
        assert(result.isInstanceOf[java.util.ArrayList[?]])
        val list = result.asInstanceOf[java.util.ArrayList[GenericRecord]]
        assertEquals(list.size(), 2)
        assertEquals(list.get(0).get("name").toString, "A")
      }
    }

    group("maps") {

      test("Map[String, V] encodes to java HashMap") {
        val result = AvroEncoder.encode(Map("a" -> 1, "b" -> 2))
        assert(result.isInstanceOf[java.util.HashMap[?, ?]])
        val map = result.asInstanceOf[java.util.HashMap[String, Int]]
        assertEquals(map.get("a"), 1)
        assertEquals(map.get("b"), 2)
      }
    }

    group("sealed traits - case objects") {

      test("case object encodes to EnumSymbol") {
        val result = AvroEncoder.encode[Color](Red)
        assert(result.isInstanceOf[GenericData.EnumSymbol])
        assertEquals(result.toString, "Red")
      }
    }

    group("sealed traits - case classes") {

      test("case class subtype encodes to GenericRecord") {
        val result = AvroEncoder.encode[Shape](Circle(5.0))
        assert(result.isInstanceOf[GenericRecord])
        val record = result.asInstanceOf[GenericRecord]
        assertEquals(record.getSchema.getName, "Circle")
        assertEquals(record.get("radius").asInstanceOf[Double], 5.0)
      }
    }

    group("derived instance") {

      test("derive creates AvroEncoder instance") {
        val encoder = AvroEncoder.derive[SimplePerson]
        val result = encoder.encode(SimplePerson("Test", 99))
        assert(result.isInstanceOf[GenericRecord])
        val record = result.asInstanceOf[GenericRecord]
        assertEquals(record.get("name").toString, "Test")
        assertEquals(record.get("age").asInstanceOf[Int], 99)
      }
    }

    group("configuration") {

      test("snake_case field names") {
        implicit val config: AvroConfig = AvroConfig().withSnakeCaseFieldNames
        val result = AvroEncoder.encode(PersonWithAddress("Bob", 25, Address("Main St", "NYC")))
        assert(result.isInstanceOf[GenericRecord])
        // Fields should still be accessible (by their transformed names)
        val record = result.asInstanceOf[GenericRecord]
        assertEquals(record.getSchema.getField("name").schema().getType, org.apache.avro.Schema.Type.STRING)
      }
    }
  }
}
