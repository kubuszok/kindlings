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
