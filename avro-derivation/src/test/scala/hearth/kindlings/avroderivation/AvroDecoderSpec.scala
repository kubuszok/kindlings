package hearth.kindlings.avroderivation

import hearth.MacroSuite
import org.apache.avro.generic.GenericData

import java.nio.ByteBuffer

final class AvroDecoderSpec extends MacroSuite {

  group("AvroDecoder") {

    group("primitive types") {

      test("Int") {
        val result = AvroDecoder.decode[Int](42: Any)
        assertEquals(result, 42)
      }

      test("Long") {
        val result = AvroDecoder.decode[Long](42L: Any)
        assertEquals(result, 42L)
      }

      test("Double") {
        val result = AvroDecoder.decode[Double](3.14: Any)
        assertEquals(result, 3.14)
      }

      test("Float") {
        val result = AvroDecoder.decode[Float](1.5f: Any)
        assertEquals(result, 1.5f)
      }

      test("Boolean") {
        val result = AvroDecoder.decode[Boolean](true: Any)
        assertEquals(result, true)
      }

      test("String from Utf8") {
        val result = AvroDecoder.decode[String](new org.apache.avro.util.Utf8("hello"): Any)
        assertEquals(result, "hello")
      }

      test("Byte from Int") {
        val result = AvroDecoder.decode[Byte](42: Any)
        assertEquals(result, 42.toByte)
      }

      test("Short from Int") {
        val result = AvroDecoder.decode[Short](42: Any)
        assertEquals(result, 42.toShort)
      }

      test("Char from CharSequence") {
        val result = AvroDecoder.decode[Char](new org.apache.avro.util.Utf8("x"): Any)
        assertEquals(result, 'x')
      }

      test("Array[Byte] from ByteBuffer") {
        val bytes = Array[Byte](1, 2, 3)
        val result = AvroDecoder.decode[Array[Byte]](ByteBuffer.wrap(bytes): Any)
        assertEquals(result.toList, bytes.toList)
      }

      test("BigDecimal from String") {
        val result = AvroDecoder.decode[BigDecimal](new org.apache.avro.util.Utf8("3.14"): Any)
        assertEquals(result, BigDecimal("3.14"))
      }
    }

    group("case classes") {

      test("simple case class from GenericRecord") {
        val schema = AvroSchemaFor.schemaOf[SimplePerson]
        val record = new GenericData.Record(schema)
        record.put("name", "Alice")
        record.put("age", 30)
        val result = AvroDecoder.decode[SimplePerson](record: Any)
        assertEquals(result, SimplePerson("Alice", 30))
      }

      test("empty case class") {
        val schema = AvroSchemaFor.schemaOf[EmptyClass]
        val record = new GenericData.Record(schema)
        val result = AvroDecoder.decode[EmptyClass](record: Any)
        assertEquals(result, EmptyClass())
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
        assertEquals(result, PersonWithAddress("Bob", 25, Address("Main St", "NYC")))
      }
    }

    group("value classes") {

      test("value class decodes underlying") {
        val result = AvroDecoder.decode[WrappedInt](42: Any)
        assertEquals(result, WrappedInt(42))
      }
    }

    group("Option") {

      test("decode Some") {
        val result = AvroDecoder.decode[Option[Int]](42: Any)
        assertEquals(result, Some(42))
      }

      test("decode None from null") {
        val result = AvroDecoder.decode[Option[Int]](null: Any)
        assertEquals(result, None)
      }
    }

    group("collections") {

      test("List from java Collection") {
        val javaList = new java.util.ArrayList[Any]()
        javaList.add(1)
        javaList.add(2)
        javaList.add(3)
        val result = AvroDecoder.decode[List[Int]](javaList: Any)
        assertEquals(result, List(1, 2, 3))
      }

      test("Vector from java Collection") {
        val javaList = new java.util.ArrayList[Any]()
        javaList.add("a")
        javaList.add("b")
        val result = AvroDecoder.decode[Vector[String]](javaList: Any)
        assertEquals(result, Vector("a", "b"))
      }
    }

    group("maps") {

      test("Map[String, V] from java Map") {
        val javaMap = new java.util.HashMap[CharSequence, Any]()
        javaMap.put("a", 1)
        javaMap.put("b", 2)
        val result = AvroDecoder.decode[Map[String, Int]](javaMap: Any)
        assertEquals(result, Map("a" -> 1, "b" -> 2))
      }
    }

    group("sealed traits - case objects") {

      test("enum from GenericData.EnumSymbol") {
        val schema = AvroSchemaFor.schemaOf[Color]
        val symbol = new GenericData.EnumSymbol(schema, "Red")
        val result = AvroDecoder.decode[Color](symbol: Any)
        assertEquals(result, Red)
      }
    }

    group("sealed traits - case classes") {

      test("union record decoded by schema name") {
        val schema = AvroSchemaFor.schemaOf[Shape]
        val circleSchema = schema.getTypes.get(0) // Circle
        val record = new GenericData.Record(circleSchema)
        record.put("radius", 5.0)
        val result = AvroDecoder.decode[Shape](record: Any)
        assertEquals(result, Circle(5.0))
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
        assertEquals(result, SimplePerson("Test", 99))
      }
    }
  }
}
