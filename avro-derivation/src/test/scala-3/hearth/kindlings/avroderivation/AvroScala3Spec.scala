package hearth.kindlings.avroderivation

import hearth.MacroSuite
import org.apache.avro.Schema
import org.apache.avro.generic.{GenericData, GenericRecord}

enum Fruit {
  case Apple, Banana, Cherry
}

enum Vehicle {
  case Car(make: String, year: Int)
  case Bike(gears: Int)
}

final class AvroScala3Spec extends MacroSuite {

  group("Scala 3 enums") {

    group("simple enum (case objects)") {

      test("schema is ENUM") {
        val schema = AvroSchemaFor.schemaOf[Fruit]
        assertEquals(schema.getType, Schema.Type.ENUM)
        assertEquals(schema.getEnumSymbols.size(), 3)
        assert(schema.getEnumSymbols.contains("Apple"))
        assert(schema.getEnumSymbols.contains("Banana"))
        assert(schema.getEnumSymbols.contains("Cherry"))
      }

      test("encode to EnumSymbol") {
        val result = AvroEncoder.encode[Fruit](Fruit.Apple)
        assert(result.isInstanceOf[GenericData.EnumSymbol])
        assertEquals(result.toString, "Apple")
      }

      test("decode from EnumSymbol") {
        val schema = AvroSchemaFor.schemaOf[Fruit]
        val symbol = new GenericData.EnumSymbol(schema, "Banana")
        val result = AvroDecoder.decode[Fruit](symbol: Any)
        assertEquals(result, Fruit.Banana)
      }

      test("round-trip") {
        implicit val encoder: AvroEncoder[Fruit] = AvroEncoder.derive[Fruit]
        implicit val decoder: AvroDecoder[Fruit] = AvroDecoder.derive[Fruit]
        val original = Fruit.Cherry
        val bytes = AvroIO.toBinary(original)
        val decoded = AvroIO.fromBinary[Fruit](bytes)
        assertEquals(decoded, original)
      }
    }

    group("parameterized enum (case classes)") {

      test("schema is UNION") {
        val schema = AvroSchemaFor.schemaOf[Vehicle]
        assertEquals(schema.getType, Schema.Type.UNION)
        assertEquals(schema.getTypes.size(), 2)
        assertEquals(schema.getTypes.get(0).getName, "Car")
        assertEquals(schema.getTypes.get(1).getName, "Bike")
      }

      test("encode Car") {
        val result = AvroEncoder.encode[Vehicle](Vehicle.Car("Toyota", 2024))
        assert(result.isInstanceOf[GenericRecord])
        val record = result.asInstanceOf[GenericRecord]
        assertEquals(record.getSchema.getName, "Car")
        assertEquals(record.get("make").toString, "Toyota")
        assertEquals(record.get("year").asInstanceOf[Int], 2024)
      }

      test("decode Car") {
        val schema = AvroSchemaFor.schemaOf[Vehicle]
        val carSchema = schema.getTypes.get(0)
        val record = new GenericData.Record(carSchema)
        record.put("make", "Honda")
        record.put("year", 2023)
        val result = AvroDecoder.decode[Vehicle](record: Any)
        assertEquals(result, Vehicle.Car("Honda", 2023))
      }

      test("round-trip") {
        implicit val encoder: AvroEncoder[Vehicle] = AvroEncoder.derive[Vehicle]
        implicit val decoder: AvroDecoder[Vehicle] = AvroDecoder.derive[Vehicle]
        val original = Vehicle.Bike(21)
        val bytes = AvroIO.toBinary(original)
        val decoded = AvroIO.fromBinary[Vehicle](bytes)
        assertEquals(decoded, original)
      }
    }
  }
}
