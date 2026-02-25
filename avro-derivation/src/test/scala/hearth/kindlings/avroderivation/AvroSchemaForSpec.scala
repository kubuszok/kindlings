package hearth.kindlings.avroderivation

import hearth.MacroSuite
import org.apache.avro.Schema

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

    group("generic case classes") {

      test("Box[Int] schema") {
        val schema = AvroSchemaFor.schemaOf[Box[Int]]
        schema.getType ==> Schema.Type.RECORD
        schema.getFields.size() ==> 1
        schema.getField("value").schema().getType ==> Schema.Type.INT
      }

      test("Pair[String, Int] schema") {
        val schema = AvroSchemaFor.schemaOf[Pair[String, Int]]
        schema.getType ==> Schema.Type.RECORD
        schema.getFields.size() ==> 2
        schema.getField("first").schema().getType ==> Schema.Type.STRING
        schema.getField("second").schema().getType ==> Schema.Type.INT
      }
    }

    group("deeply nested") {

      test("PersonFull with 3-level nesting") {
        val schema = AvroSchemaFor.schemaOf[PersonFull]
        schema.getType ==> Schema.Type.RECORD
        val addressField = schema.getField("address")
        addressField.schema().getType ==> Schema.Type.RECORD
        val geoField = addressField.schema().getField("geo")
        geoField.schema().getType ==> Schema.Type.RECORD
        geoField.schema().getField("lat").schema().getType ==> Schema.Type.DOUBLE
        geoField.schema().getField("lon").schema().getType ==> Schema.Type.DOUBLE
      }
    }

    group("type aliases") {

      test("WithAlias schema") {
        val schema = AvroSchemaFor.schemaOf[WithAlias]
        schema.getType ==> Schema.Type.RECORD
        schema.getField("name").schema().getType ==> Schema.Type.STRING
        schema.getField("age").schema().getType ==> Schema.Type.INT
      }
    }

    group("logical types") {

      test("UUID schema is STRING with uuid logical type") {
        val schema = AvroSchemaFor.schemaOf[java.util.UUID]
        schema.getType ==> Schema.Type.STRING
        schema.getLogicalType.getName ==> "uuid"
      }

      test("Instant schema is LONG with timestamp-millis logical type") {
        val schema = AvroSchemaFor.schemaOf[java.time.Instant]
        schema.getType ==> Schema.Type.LONG
        schema.getLogicalType.getName ==> "timestamp-millis"
      }

      test("LocalDate schema is INT with date logical type") {
        val schema = AvroSchemaFor.schemaOf[java.time.LocalDate]
        schema.getType ==> Schema.Type.INT
        schema.getLogicalType.getName ==> "date"
      }

      test("LocalTime schema is LONG with time-micros logical type") {
        val schema = AvroSchemaFor.schemaOf[java.time.LocalTime]
        schema.getType ==> Schema.Type.LONG
        schema.getLogicalType.getName ==> "time-micros"
      }

      test("LocalDateTime schema is LONG with timestamp-millis logical type") {
        val schema = AvroSchemaFor.schemaOf[java.time.LocalDateTime]
        schema.getType ==> Schema.Type.LONG
        schema.getLogicalType.getName ==> "timestamp-millis"
      }

      test("case class with logical type fields") {
        val schema = AvroSchemaFor.schemaOf[EventRecord]
        schema.getType ==> Schema.Type.RECORD
        schema.getField("id").schema().getLogicalType.getName ==> "uuid"
        schema.getField("timestamp").schema().getLogicalType.getName ==> "timestamp-millis"
        schema.getField("date").schema().getLogicalType.getName ==> "date"
        schema.getField("time").schema().getLogicalType.getName ==> "time-micros"
        schema.getField("localTimestamp").schema().getLogicalType.getName ==> "timestamp-millis"
      }
    }

    group("per-field annotations") {

      test("@fieldName overrides schema field name") {
        val schema = AvroSchemaFor.schemaOf[AvroWithFieldName]
        schema.getType ==> Schema.Type.RECORD
        schema.getFields.size() ==> 2
        (schema.getField("user_name") != null) ==> true
        (schema.getField("age") != null) ==> true
      }

      test("@transientField excludes field from schema") {
        val schema = AvroSchemaFor.schemaOf[AvroWithTransient]
        schema.getType ==> Schema.Type.RECORD
        schema.getFields.size() ==> 1
        (schema.getField("name") != null) ==> true
      }

      test("@fieldName and @transientField combined") {
        val schema = AvroSchemaFor.schemaOf[AvroWithBothAnnotations]
        schema.getType ==> Schema.Type.RECORD
        schema.getFields.size() ==> 2
        (schema.getField("display_name") != null) ==> true
        (schema.getField("active") != null) ==> true
      }

      test("@fieldName overrides config transform") {
        implicit val config: AvroConfig = AvroConfig().withSnakeCaseFieldNames
        val schema = AvroSchemaFor.schemaOf[AvroWithFieldName]
        // @fieldName("user_name") should take precedence over config snake_case
        (schema.getField("user_name") != null) ==> true
        (schema.getField("age") != null) ==> true
      }
    }

    group("tuples") {

      test("Tuple2 schema is RECORD with _1, _2 fields") {
        val schema = AvroSchemaFor.schemaOf[(String, Int)]
        schema.getType ==> Schema.Type.RECORD
        schema.getFields.size() ==> 2
        schema.getField("_1").schema().getType ==> Schema.Type.STRING
        schema.getField("_2").schema().getType ==> Schema.Type.INT
      }

      test("Tuple3 schema is RECORD with _1, _2, _3 fields") {
        val schema = AvroSchemaFor.schemaOf[(Int, String, Boolean)]
        schema.getType ==> Schema.Type.RECORD
        schema.getFields.size() ==> 3
        schema.getField("_1").schema().getType ==> Schema.Type.INT
        schema.getField("_2").schema().getType ==> Schema.Type.STRING
        schema.getField("_3").schema().getType ==> Schema.Type.BOOLEAN
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

    group("@avroDoc annotation") {

      test("class-level @avroDoc sets record doc") {
        val schema = AvroSchemaFor.schemaOf[DocumentedPerson]
        schema.getDoc ==> "A documented person record"
      }

      test("field-level @avroDoc sets field doc") {
        val schema = AvroSchemaFor.schemaOf[DocumentedPerson]
        schema.getField("name").doc() ==> "The person's full name"
        schema.getField("age").doc() ==> "Age in years"
      }

      test("undocumented fields have null doc") {
        val schema = AvroSchemaFor.schemaOf[SimplePerson]
        assert(schema.getField("name").doc() == null)
      }
    }

    group("@avroNamespace annotation") {

      test("@avroNamespace overrides config namespace") {
        val schema = AvroSchemaFor.schemaOf[CustomNamespacePerson]
        schema.getNamespace ==> "com.example.custom"
      }

      test("@avroNamespace with config namespace uses annotation") {
        implicit val config: AvroConfig = AvroConfig(namespace = Some("com.example.config"))
        val schema = AvroSchemaFor.schemaOf[CustomNamespacePerson]
        schema.getNamespace ==> "com.example.custom"
      }
    }

    group("combined @avroDoc and @avroNamespace") {

      test("both annotations on same class") {
        val schema = AvroSchemaFor.schemaOf[FullyAnnotatedRecord]
        schema.getDoc ==> "A record with custom namespace"
        schema.getNamespace ==> "com.example.docs"
        schema.getField("id").doc() ==> "The identifier"
        assert(schema.getField("value").doc() == null)
      }
    }

    group("@avroDefault annotation") {

      test("field with @avroDefault has default value in schema") {
        val schema = AvroSchemaFor.schemaOf[WithDefaults]
        assert(!schema.getField("name").hasDefaultValue)
        assert(schema.getField("age").hasDefaultValue)
        schema.getField("age").defaultVal() ==> 0
        assert(schema.getField("role").hasDefaultValue)
        schema.getField("role").defaultVal() ==> "unknown"
      }

      test("Option field with @avroDefault(\"null\") has null default") {
        val schema = AvroSchemaFor.schemaOf[WithOptionalDefault]
        assert(!schema.getField("name").hasDefaultValue)
        assert(schema.getField("nickname").hasDefaultValue)
        assert(schema.getField("nickname").defaultVal() == org.apache.avro.JsonProperties.NULL_VALUE)
      }
    }

    group("BigDecimal as decimal logical type") {

      test("BigDecimal with decimalConfig produces BYTES with decimal logical type") {
        implicit val config: AvroConfig = AvroConfig().withDecimalConfig(10, 2)
        val schema = AvroSchemaFor.schemaOf[BigDecimal]
        schema.getType ==> Schema.Type.BYTES
        schema.getLogicalType.getName ==> "decimal"
        schema.getLogicalType.asInstanceOf[org.apache.avro.LogicalTypes.Decimal].getPrecision ==> 10
        schema.getLogicalType.asInstanceOf[org.apache.avro.LogicalTypes.Decimal].getScale ==> 2
      }

      test("BigDecimal without decimalConfig produces STRING (default)") {
        val schema = AvroSchemaFor.schemaOf[BigDecimal]
        schema.getType ==> Schema.Type.STRING
      }

      test("case class with BigDecimal field and decimal config") {
        implicit val config: AvroConfig = AvroConfig().withDecimalConfig(10, 2)
        val schema = AvroSchemaFor.schemaOf[WithBigDecimal]
        schema.getType ==> Schema.Type.RECORD
        val amountSchema = schema.getField("amount").schema()
        amountSchema.getType ==> Schema.Type.BYTES
        amountSchema.getLogicalType.getName ==> "decimal"
      }
    }

    group("Either as union") {

      test("Either[String, Int] produces UNION(STRING, INT)") {
        val schema = AvroSchemaFor.schemaOf[Either[String, Int]]
        schema.getType ==> Schema.Type.UNION
        schema.getTypes.size() ==> 2
        schema.getTypes.get(0).getType ==> Schema.Type.STRING
        schema.getTypes.get(1).getType ==> Schema.Type.INT
      }

      test("Either[String, SimplePerson] produces UNION(STRING, RECORD)") {
        val schema = AvroSchemaFor.schemaOf[Either[String, SimplePerson]]
        schema.getType ==> Schema.Type.UNION
        schema.getTypes.size() ==> 2
        schema.getTypes.get(0).getType ==> Schema.Type.STRING
        schema.getTypes.get(1).getType ==> Schema.Type.RECORD
        schema.getTypes.get(1).getName ==> "SimplePerson"
      }

      test("case class with Either field") {
        val schema = AvroSchemaFor.schemaOf[WithEither]
        schema.getType ==> Schema.Type.RECORD
        val valueSchema = schema.getField("value").schema()
        valueSchema.getType ==> Schema.Type.UNION
        valueSchema.getTypes.size() ==> 2
        valueSchema.getTypes.get(0).getType ==> Schema.Type.STRING
        valueSchema.getTypes.get(1).getType ==> Schema.Type.INT
      }
    }

    group("schema evolution with defaults") {

      test("schema with defaults enables forward compatibility") {
        val readerSchema = AvroSchemaFor.schemaOf[WithDefaults]
        // Fields with @avroDefault enable schema evolution:
        // a reader with these defaults can read data that omits these fields
        assert(readerSchema.getField("age").hasDefaultValue)
        assert(readerSchema.getField("role").hasDefaultValue)
        assert(!readerSchema.getField("name").hasDefaultValue)
      }
    }
  }
}
