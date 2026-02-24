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
}
