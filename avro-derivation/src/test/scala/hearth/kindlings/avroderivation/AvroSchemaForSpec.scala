package hearth.kindlings.avroderivation

import hearth.MacroSuite
import org.apache.avro.Schema

case class SimplePerson(name: String, age: Int)
case class EmptyClass()
case class SingleField(value: Int)
case class Address(street: String, city: String)
case class PersonWithAddress(name: String, age: Int, address: Address)
case class TeamWithMembers(name: String, members: List[SimplePerson])
case class RecursiveTree(value: Int, children: List[RecursiveTree])
final case class WrappedInt(value: Int) extends AnyVal

sealed trait Shape
case class Circle(radius: Double) extends Shape
case class Rectangle(width: Double, height: Double) extends Shape

sealed trait Color
case object Red extends Color
case object Green extends Color
case object Blue extends Color

sealed trait Animal
case class Dog(name: String, breed: String) extends Animal
case class Cat(name: String, indoor: Boolean) extends Animal

final class AvroSchemaForSpec extends MacroSuite {

  group("AvroSchemaFor") {

    group("primitive types") {

      test("Int schema") {
        val schema = AvroSchemaFor.schemaOf[Int]
        assertEquals(schema.getType, Schema.Type.INT)
      }

      test("Long schema") {
        val schema = AvroSchemaFor.schemaOf[Long]
        assertEquals(schema.getType, Schema.Type.LONG)
      }

      test("Double schema") {
        val schema = AvroSchemaFor.schemaOf[Double]
        assertEquals(schema.getType, Schema.Type.DOUBLE)
      }

      test("Float schema") {
        val schema = AvroSchemaFor.schemaOf[Float]
        assertEquals(schema.getType, Schema.Type.FLOAT)
      }

      test("Boolean schema") {
        val schema = AvroSchemaFor.schemaOf[Boolean]
        assertEquals(schema.getType, Schema.Type.BOOLEAN)
      }

      test("String schema") {
        val schema = AvroSchemaFor.schemaOf[String]
        assertEquals(schema.getType, Schema.Type.STRING)
      }

      test("Byte schema maps to INT") {
        val schema = AvroSchemaFor.schemaOf[Byte]
        assertEquals(schema.getType, Schema.Type.INT)
      }

      test("Short schema maps to INT") {
        val schema = AvroSchemaFor.schemaOf[Short]
        assertEquals(schema.getType, Schema.Type.INT)
      }

      test("Char schema maps to STRING") {
        val schema = AvroSchemaFor.schemaOf[Char]
        assertEquals(schema.getType, Schema.Type.STRING)
      }

      test("Array[Byte] schema maps to BYTES") {
        val schema = AvroSchemaFor.schemaOf[Array[Byte]]
        assertEquals(schema.getType, Schema.Type.BYTES)
      }

      test("BigDecimal schema maps to STRING") {
        val schema = AvroSchemaFor.schemaOf[BigDecimal]
        assertEquals(schema.getType, Schema.Type.STRING)
      }
    }

    group("case classes") {

      test("simple case class") {
        val schema = AvroSchemaFor.schemaOf[SimplePerson]
        assertEquals(schema.getType, Schema.Type.RECORD)
        assertEquals(schema.getName, "SimplePerson")
        assertEquals(schema.getFields.size(), 2)
        assertEquals(schema.getField("name").schema().getType, Schema.Type.STRING)
        assertEquals(schema.getField("age").schema().getType, Schema.Type.INT)
      }

      test("empty case class") {
        val schema = AvroSchemaFor.schemaOf[EmptyClass]
        assertEquals(schema.getType, Schema.Type.RECORD)
        assertEquals(schema.getFields.size(), 0)
      }

      test("nested case class") {
        val schema = AvroSchemaFor.schemaOf[PersonWithAddress]
        assertEquals(schema.getType, Schema.Type.RECORD)
        val addressField = schema.getField("address")
        assertEquals(addressField.schema().getType, Schema.Type.RECORD)
        assertEquals(addressField.schema().getName, "Address")
      }
    }

    group("value classes") {

      test("value class uses underlying schema") {
        val schema = AvroSchemaFor.schemaOf[WrappedInt]
        assertEquals(schema.getType, Schema.Type.INT)
      }
    }

    group("Option") {

      test("Option creates UNION(null, T)") {
        val schema = AvroSchemaFor.schemaOf[Option[Int]]
        assertEquals(schema.getType, Schema.Type.UNION)
        assertEquals(schema.getTypes.size(), 2)
        assertEquals(schema.getTypes.get(0).getType, Schema.Type.NULL)
        assertEquals(schema.getTypes.get(1).getType, Schema.Type.INT)
      }
    }

    group("collections") {

      test("List creates ARRAY") {
        val schema = AvroSchemaFor.schemaOf[List[Int]]
        assertEquals(schema.getType, Schema.Type.ARRAY)
        assertEquals(schema.getElementType.getType, Schema.Type.INT)
      }

      test("Vector creates ARRAY") {
        val schema = AvroSchemaFor.schemaOf[Vector[String]]
        assertEquals(schema.getType, Schema.Type.ARRAY)
        assertEquals(schema.getElementType.getType, Schema.Type.STRING)
      }
    }

    group("maps") {

      test("Map[String, V] creates MAP") {
        val schema = AvroSchemaFor.schemaOf[Map[String, Int]]
        assertEquals(schema.getType, Schema.Type.MAP)
        assertEquals(schema.getValueType.getType, Schema.Type.INT)
      }
    }

    group("sealed traits with case objects only") {

      test("pure enum creates ENUM schema") {
        val schema = AvroSchemaFor.schemaOf[Color]
        assertEquals(schema.getType, Schema.Type.ENUM)
        assertEquals(schema.getEnumSymbols.size(), 3)
        assert(schema.getEnumSymbols.contains("Red"))
        assert(schema.getEnumSymbols.contains("Green"))
        assert(schema.getEnumSymbols.contains("Blue"))
      }
    }

    group("sealed traits with case classes") {

      test("mixed sealed trait creates UNION") {
        val schema = AvroSchemaFor.schemaOf[Shape]
        assertEquals(schema.getType, Schema.Type.UNION)
        assertEquals(schema.getTypes.size(), 2)
        assertEquals(schema.getTypes.get(0).getName, "Circle")
        assertEquals(schema.getTypes.get(1).getName, "Rectangle")
      }
    }

    group("configuration") {

      test("namespace") {
        implicit val config: AvroConfig = AvroConfig(namespace = Some("com.example"))
        val schema = AvroSchemaFor.schemaOf[SimplePerson]
        assertEquals(schema.getNamespace, "com.example")
      }

      test("snake_case field names") {
        implicit val config: AvroConfig = AvroConfig().withSnakeCaseFieldNames
        val schema = AvroSchemaFor.schemaOf[PersonWithAddress]
        assert(schema.getField("name") != null)
        assert(schema.getField("age") != null)
        // PersonWithAddress has "address" which stays the same in snake_case
      }
    }

    group("derived instance") {

      test("derive creates AvroSchemaFor instance") {
        val instance = AvroSchemaFor.derive[SimplePerson]
        assertEquals(instance.schema.getType, Schema.Type.RECORD)
        assertEquals(instance.schema.getName, "SimplePerson")
      }
    }

    group("collections of case classes") {

      test("List of case classes creates array of records") {
        val schema = AvroSchemaFor.schemaOf[TeamWithMembers]
        assertEquals(schema.getType, Schema.Type.RECORD)
        val membersField = schema.getField("members")
        assertEquals(membersField.schema().getType, Schema.Type.ARRAY)
        assertEquals(membersField.schema().getElementType.getType, Schema.Type.RECORD)
        assertEquals(membersField.schema().getElementType.getName, "SimplePerson")
      }
    }
  }
}
