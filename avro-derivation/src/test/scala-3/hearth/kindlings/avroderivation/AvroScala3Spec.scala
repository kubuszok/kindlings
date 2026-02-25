package hearth.kindlings.avroderivation

import hearth.MacroSuite
import org.apache.avro.Schema
import org.apache.avro.generic.{GenericData, GenericRecord}

final class AvroScala3Spec extends MacroSuite {

  group("Scala 3 enums") {

    group("simple enum (case objects)") {

      test("schema is ENUM") {
        val schema = AvroSchemaFor.schemaOf[Fruit]
        schema.getType ==> Schema.Type.ENUM
        schema.getEnumSymbols.size() ==> 3
        schema.getEnumSymbols.contains("Apple") ==> true
        schema.getEnumSymbols.contains("Banana") ==> true
        schema.getEnumSymbols.contains("Cherry") ==> true
      }

      test("encode to EnumSymbol") {
        val result = AvroEncoder.encode[Fruit](Fruit.Apple)
        result.isInstanceOf[GenericData.EnumSymbol] ==> true
        result.toString ==> "Apple"
      }

      test("decode from EnumSymbol") {
        val schema = AvroSchemaFor.schemaOf[Fruit]
        val symbol = new GenericData.EnumSymbol(schema, "Banana")
        val result = AvroDecoder.decode[Fruit](symbol: Any)
        result ==> Fruit.Banana
      }

      test("round-trip") {
        implicit val encoder: AvroEncoder[Fruit] = AvroEncoder.derive[Fruit]
        implicit val decoder: AvroDecoder[Fruit] = AvroDecoder.derive[Fruit]
        val original = Fruit.Cherry
        val bytes = AvroIO.toBinary(original)
        val decoded = AvroIO.fromBinary[Fruit](bytes)
        decoded ==> original
      }
    }

    group("parameterized enum (case classes)") {

      test("schema is UNION") {
        val schema = AvroSchemaFor.schemaOf[Vehicle]
        schema.getType ==> Schema.Type.UNION
        schema.getTypes.size() ==> 2
        schema.getTypes.get(0).getName ==> "Car"
        schema.getTypes.get(1).getName ==> "Bike"
      }

      test("encode Car") {
        val result = AvroEncoder.encode[Vehicle](Vehicle.Car("Toyota", 2024))
        result.isInstanceOf[GenericRecord] ==> true
        val record = result.asInstanceOf[GenericRecord]
        record.getSchema.getName ==> "Car"
        record.get("make").toString ==> "Toyota"
        record.get("year").asInstanceOf[Int] ==> 2024
      }

      test("decode Car") {
        val schema = AvroSchemaFor.schemaOf[Vehicle]
        val carSchema = schema.getTypes.get(0)
        val record = new GenericData.Record(carSchema)
        record.put("make", "Honda")
        record.put("year", 2023)
        val result = AvroDecoder.decode[Vehicle](record: Any)
        result ==> Vehicle.Car("Honda", 2023)
      }

      test("round-trip") {
        implicit val encoder: AvroEncoder[Vehicle] = AvroEncoder.derive[Vehicle]
        implicit val decoder: AvroDecoder[Vehicle] = AvroDecoder.derive[Vehicle]
        val original = Vehicle.Bike(21)
        val bytes = AvroIO.toBinary(original)
        val decoded = AvroIO.fromBinary[Vehicle](bytes)
        decoded ==> original
      }
    }
  }

  group("named tuples (Scala 3.7+)") {

    test("schema is RECORD with named fields") {
      val schema = AvroSchemaFor.schemaOf[(name: String, age: Int)]
      schema.getType ==> Schema.Type.RECORD
      schema.getFields.size() ==> 2
      schema.getField("name").schema().getType ==> Schema.Type.STRING
      schema.getField("age").schema().getType ==> Schema.Type.INT
    }

    test("encode named tuple to GenericRecord") {
      val nt: (name: String, age: Int) = ("Alice", 42)
      val result = AvroEncoder.encode(nt)
      result.isInstanceOf[GenericRecord] ==> true
      val record = result.asInstanceOf[GenericRecord]
      record.get("name").toString ==> "Alice"
      record.get("age").asInstanceOf[Int] ==> 42
    }

    test("decode GenericRecord to named tuple") {
      val schema = AvroSchemaFor.schemaOf[(name: String, age: Int)]
      val record = new GenericData.Record(schema)
      record.put("name", "Bob")
      record.put("age", 25)
      val result = AvroDecoder.decode[(name: String, age: Int)](record: Any)
      result ==> ("Bob", 25)
    }

    test("named tuple with nested case class") {
      val nt: (person: SimplePerson, score: Int) = (SimplePerson("Alice", 30), 100)
      val result = AvroEncoder.encode(nt)
      result.isInstanceOf[GenericRecord] ==> true
      val record = result.asInstanceOf[GenericRecord]
      record.get("score").asInstanceOf[Int] ==> 100
      val personRecord = record.get("person").asInstanceOf[GenericRecord]
      personRecord.get("name").toString ==> "Alice"
      personRecord.get("age").asInstanceOf[Int] ==> 30
    }

    test("round-trip via binary") {
      implicit val encoder: AvroEncoder[(name: String, age: Int)] = AvroEncoder.derive[(name: String, age: Int)]
      implicit val decoder: AvroDecoder[(name: String, age: Int)] = AvroDecoder.derive[(name: String, age: Int)]
      val original: (name: String, age: Int) = ("Alice", 42)
      val bytes = AvroIO.toBinary(original)
      val decoded = AvroIO.fromBinary[(name: String, age: Int)](bytes)
      decoded ==> original
    }
  }

  group("opaque types") {

    test("encode standalone opaque type") {
      import AvroOpaqueTypes.*
      val result = AvroEncoder.encode(UserId(42))
      result ==> 42
    }

    test("decode standalone opaque type") {
      import AvroOpaqueTypes.*
      val result = AvroDecoder.decode[UserId](42: Any)
      result ==> UserId(42)
    }

    test("schema for case class with opaque type field") {
      import AvroOpaqueTypes.*
      val schema = AvroSchemaFor.schemaOf[AvroUserWithOpaque]
      schema.getType ==> Schema.Type.RECORD
      schema.getField("id").schema().getType ==> Schema.Type.INT
      schema.getField("name").schema().getType ==> Schema.Type.STRING
    }

    test("encode case class with opaque type field") {
      import AvroOpaqueTypes.*
      val result = AvroEncoder.encode(AvroUserWithOpaque(UserId(42), "Alice"))
      result.isInstanceOf[GenericRecord] ==> true
      val record = result.asInstanceOf[GenericRecord]
      record.get("id").asInstanceOf[Int] ==> 42
      record.get("name").toString ==> "Alice"
    }

    test("round-trip case class with opaque type") {
      import AvroOpaqueTypes.*
      implicit val encoder: AvroEncoder[AvroUserWithOpaque] = AvroEncoder.derive[AvroUserWithOpaque]
      implicit val decoder: AvroDecoder[AvroUserWithOpaque] = AvroDecoder.derive[AvroUserWithOpaque]
      val original = AvroUserWithOpaque(UserId(42), "Alice")
      val bytes = AvroIO.toBinary(original)
      val decoded = AvroIO.fromBinary[AvroUserWithOpaque](bytes)
      decoded ==> original
    }
  }
}
