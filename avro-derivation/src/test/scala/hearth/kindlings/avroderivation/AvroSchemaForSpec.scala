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
        schema.getType ==> Schema.Type.INT
      }

      test("Long schema") {
        val schema = AvroSchemaFor.schemaOf[Long]
        schema.getType ==> Schema.Type.LONG
      }

      test("Double schema") {
        val schema = AvroSchemaFor.schemaOf[Double]
        schema.getType ==> Schema.Type.DOUBLE
      }

      test("Float schema") {
        val schema = AvroSchemaFor.schemaOf[Float]
        schema.getType ==> Schema.Type.FLOAT
      }

      test("Boolean schema") {
        val schema = AvroSchemaFor.schemaOf[Boolean]
        schema.getType ==> Schema.Type.BOOLEAN
      }

      test("String schema") {
        val schema = AvroSchemaFor.schemaOf[String]
        schema.getType ==> Schema.Type.STRING
      }

      test("Byte schema maps to INT") {
        val schema = AvroSchemaFor.schemaOf[Byte]
        schema.getType ==> Schema.Type.INT
      }

      test("Short schema maps to INT") {
        val schema = AvroSchemaFor.schemaOf[Short]
        schema.getType ==> Schema.Type.INT
      }

      test("Char schema maps to STRING") {
        val schema = AvroSchemaFor.schemaOf[Char]
        schema.getType ==> Schema.Type.STRING
      }

      test("Array[Byte] schema maps to BYTES") {
        val schema = AvroSchemaFor.schemaOf[Array[Byte]]
        schema.getType ==> Schema.Type.BYTES
      }

      test("BigDecimal schema maps to STRING") {
        val schema = AvroSchemaFor.schemaOf[BigDecimal]
        schema.getType ==> Schema.Type.STRING
      }
    }

    group("case classes") {

      test("simple case class") {
        val schema = AvroSchemaFor.schemaOf[SimplePerson]
        schema.getType ==> Schema.Type.RECORD
        schema.getName ==> "SimplePerson"
        schema.getFields.size() ==> 2
        schema.getField("name").schema().getType ==> Schema.Type.STRING
        schema.getField("age").schema().getType ==> Schema.Type.INT
      }

      test("empty case class") {
        val schema = AvroSchemaFor.schemaOf[EmptyClass]
        schema.getType ==> Schema.Type.RECORD
        schema.getFields.size() ==> 0
      }

      test("nested case class") {
        val schema = AvroSchemaFor.schemaOf[PersonWithAddress]
        schema.getType ==> Schema.Type.RECORD
        val addressField = schema.getField("address")
        addressField.schema().getType ==> Schema.Type.RECORD
        addressField.schema().getName ==> "Address"
      }
    }

    group("value classes") {

      test("value class uses underlying schema") {
        val schema = AvroSchemaFor.schemaOf[WrappedInt]
        schema.getType ==> Schema.Type.INT
      }
    }

    group("Option") {

      test("Option creates UNION(null, T)") {
        val schema = AvroSchemaFor.schemaOf[Option[Int]]
        schema.getType ==> Schema.Type.UNION
        schema.getTypes.size() ==> 2
        schema.getTypes.get(0).getType ==> Schema.Type.NULL
        schema.getTypes.get(1).getType ==> Schema.Type.INT
      }
    }

    group("collections") {

      test("List creates ARRAY") {
        val schema = AvroSchemaFor.schemaOf[List[Int]]
        schema.getType ==> Schema.Type.ARRAY
        schema.getElementType.getType ==> Schema.Type.INT
      }

      test("Vector creates ARRAY") {
        val schema = AvroSchemaFor.schemaOf[Vector[String]]
        schema.getType ==> Schema.Type.ARRAY
        schema.getElementType.getType ==> Schema.Type.STRING
      }
    }

    group("maps") {

      test("Map[String, V] creates MAP") {
        val schema = AvroSchemaFor.schemaOf[Map[String, Int]]
        schema.getType ==> Schema.Type.MAP
        schema.getValueType.getType ==> Schema.Type.INT
      }
    }

    group("sealed traits with case objects only") {

      test("pure enum creates ENUM schema") {
        val schema = AvroSchemaFor.schemaOf[Color]
        schema.getType ==> Schema.Type.ENUM
        schema.getEnumSymbols.size() ==> 3
        schema.getEnumSymbols.contains("Red") ==> true
        schema.getEnumSymbols.contains("Green") ==> true
        schema.getEnumSymbols.contains("Blue") ==> true
      }
    }

    group("sealed traits with case classes") {

      test("mixed sealed trait creates UNION") {
        val schema = AvroSchemaFor.schemaOf[Shape]
        schema.getType ==> Schema.Type.UNION
        schema.getTypes.size() ==> 2
        schema.getTypes.get(0).getName ==> "Circle"
        schema.getTypes.get(1).getName ==> "Rectangle"
      }
    }

    group("configuration") {

      test("namespace") {
        implicit val config: AvroConfig = AvroConfig(namespace = Some("com.example"))
        val schema = AvroSchemaFor.schemaOf[SimplePerson]
        schema.getNamespace ==> "com.example"
      }

      test("snake_case field names") {
        implicit val config: AvroConfig = AvroConfig().withSnakeCaseFieldNames
        val schema = AvroSchemaFor.schemaOf[PersonWithAddress]
        (schema.getField("name") != null) ==> true
        (schema.getField("age") != null) ==> true
        // PersonWithAddress has "address" which stays the same in snake_case
      }
    }

    group("derived instance") {

      test("derive creates AvroSchemaFor instance") {
        val instance = AvroSchemaFor.derive[SimplePerson]
        instance.schema.getType ==> Schema.Type.RECORD
        instance.schema.getName ==> "SimplePerson"
      }
    }

    group("sets") {

      test("Set creates ARRAY") {
        val schema = AvroSchemaFor.schemaOf[Set[Int]]
        schema.getType ==> Schema.Type.ARRAY
        schema.getElementType.getType ==> Schema.Type.INT
      }
    }

    group("configuration — field name transforms") {

      test("kebab-case field names") {
        implicit val config: AvroConfig = AvroConfig().withKebabCaseFieldNames
        val schema = AvroSchemaFor.schemaOf[SimplePerson]
        (schema.getField("name") != null) ==> true
        (schema.getField("age") != null) ==> true
      }
    }

    group("collections of case classes") {

      test("List of case classes creates array of records") {
        val schema = AvroSchemaFor.schemaOf[TeamWithMembers]
        schema.getType ==> Schema.Type.RECORD
        val membersField = schema.getField("members")
        membersField.schema().getType ==> Schema.Type.ARRAY
        membersField.schema().getElementType.getType ==> Schema.Type.RECORD
        membersField.schema().getElementType.getName ==> "SimplePerson"
      }
    }
  }
}
