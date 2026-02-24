package hearth.kindlings.avroderivation

import hearth.MacroSuite

final class AvroRoundTripSpec extends MacroSuite {

  group("AvroIO round-trip") {

    group("binary") {

      test("simple case class") {
        val encoder: AvroEncoder[SimplePerson] = AvroEncoder.derive[SimplePerson]
        val decoder: AvroDecoder[SimplePerson] = AvroDecoder.derive[SimplePerson]
        val original = SimplePerson("Alice", 30)
        val bytes = AvroIO.toBinary(original)(encoder)
        val decoded = AvroIO.fromBinary[SimplePerson](bytes)(decoder)
        decoded ==> original
      }

      test("empty case class") {
        val encoder: AvroEncoder[EmptyClass] = AvroEncoder.derive[EmptyClass]
        val decoder: AvroDecoder[EmptyClass] = AvroDecoder.derive[EmptyClass]
        val original = EmptyClass()
        val bytes = AvroIO.toBinary(original)(encoder)
        val decoded = AvroIO.fromBinary[EmptyClass](bytes)(decoder)
        decoded ==> original
      }

      test("nested case class") {
        val encoder: AvroEncoder[PersonWithAddress] = AvroEncoder.derive[PersonWithAddress]
        val decoder: AvroDecoder[PersonWithAddress] = AvroDecoder.derive[PersonWithAddress]
        val original = PersonWithAddress("Bob", 25, Address("Main St", "NYC"))
        val bytes = AvroIO.toBinary(original)(encoder)
        val decoded = AvroIO.fromBinary[PersonWithAddress](bytes)(decoder)
        decoded ==> original
      }

      test("case class with collection") {
        val encoder: AvroEncoder[TeamWithMembers] = AvroEncoder.derive[TeamWithMembers]
        val decoder: AvroDecoder[TeamWithMembers] = AvroDecoder.derive[TeamWithMembers]
        val original = TeamWithMembers("Team A", List(SimplePerson("A", 1), SimplePerson("B", 2)))
        val bytes = AvroIO.toBinary(original)(encoder)
        val decoded = AvroIO.fromBinary[TeamWithMembers](bytes)(decoder)
        decoded ==> original
      }

      test("value class") {
        val encoder: AvroEncoder[WrappedInt] = AvroEncoder.derive[WrappedInt]
        val decoder: AvroDecoder[WrappedInt] = AvroDecoder.derive[WrappedInt]
        val original = WrappedInt(42)
        val bytes = AvroIO.toBinary(original)(encoder)
        val decoded = AvroIO.fromBinary[WrappedInt](bytes)(decoder)
        decoded ==> original
      }
    }

    group("generic case classes") {

      test("Box[Int] binary round-trip") {
        val encoder: AvroEncoder[Box[Int]] = AvroEncoder.derive[Box[Int]]
        val decoder: AvroDecoder[Box[Int]] = AvroDecoder.derive[Box[Int]]
        val original = Box(42)
        val bytes = AvroIO.toBinary(original)(encoder)
        val decoded = AvroIO.fromBinary[Box[Int]](bytes)(decoder)
        decoded ==> original
      }

      test("Pair[String, Int] binary round-trip") {
        val encoder: AvroEncoder[Pair[String, Int]] = AvroEncoder.derive[Pair[String, Int]]
        val decoder: AvroDecoder[Pair[String, Int]] = AvroDecoder.derive[Pair[String, Int]]
        val original = Pair("hello", 42)
        val bytes = AvroIO.toBinary(original)(encoder)
        val decoded = AvroIO.fromBinary[Pair[String, Int]](bytes)(decoder)
        decoded ==> original
      }
    }

    group("deeply nested") {

      test("PersonFull binary round-trip") {
        val encoder: AvroEncoder[PersonFull] = AvroEncoder.derive[PersonFull]
        val decoder: AvroDecoder[PersonFull] = AvroDecoder.derive[PersonFull]
        val original = PersonFull("Alice", FullAddress("123 Main", "NYC", GeoCoordinates(40.7, -74.0)))
        val bytes = AvroIO.toBinary(original)(encoder)
        val decoded = AvroIO.fromBinary[PersonFull](bytes)(decoder)
        decoded ==> original
      }
    }

    group("type aliases") {

      test("WithAlias binary round-trip") {
        val encoder: AvroEncoder[WithAlias] = AvroEncoder.derive[WithAlias]
        val decoder: AvroDecoder[WithAlias] = AvroDecoder.derive[WithAlias]
        val original = WithAlias("Alice", 30)
        val bytes = AvroIO.toBinary(original)(encoder)
        val decoded = AvroIO.fromBinary[WithAlias](bytes)(decoder)
        decoded ==> original
      }
    }

    group("sets") {

      test("Set of ints round-trip") {
        val encoder: AvroEncoder[Set[Int]] = AvroEncoder.derive[Set[Int]]
        val decoder: AvroDecoder[Set[Int]] = AvroDecoder.derive[Set[Int]]
        val original = Set(1, 2, 3)
        val bytes = AvroIO.toBinary(original)(encoder)
        val decoded = AvroIO.fromBinary[Set[Int]](bytes)(decoder)
        decoded ==> original
      }
    }

    group("logical types") {

      test("UUID binary round-trip") {
        val encoder: AvroEncoder[java.util.UUID] = AvroEncoder.derive[java.util.UUID]
        val decoder: AvroDecoder[java.util.UUID] = AvroDecoder.derive[java.util.UUID]
        val original = java.util.UUID.fromString("550e8400-e29b-41d4-a716-446655440000")
        val bytes = AvroIO.toBinary(original)(encoder)
        val decoded = AvroIO.fromBinary[java.util.UUID](bytes)(decoder)
        decoded ==> original
      }

      test("Instant binary round-trip") {
        val encoder: AvroEncoder[java.time.Instant] = AvroEncoder.derive[java.time.Instant]
        val decoder: AvroDecoder[java.time.Instant] = AvroDecoder.derive[java.time.Instant]
        val original = java.time.Instant.ofEpochMilli(1700000000000L)
        val bytes = AvroIO.toBinary(original)(encoder)
        val decoded = AvroIO.fromBinary[java.time.Instant](bytes)(decoder)
        decoded ==> original
      }

      test("LocalDate binary round-trip") {
        val encoder: AvroEncoder[java.time.LocalDate] = AvroEncoder.derive[java.time.LocalDate]
        val decoder: AvroDecoder[java.time.LocalDate] = AvroDecoder.derive[java.time.LocalDate]
        val original = java.time.LocalDate.of(2024, 1, 15)
        val bytes = AvroIO.toBinary(original)(encoder)
        val decoded = AvroIO.fromBinary[java.time.LocalDate](bytes)(decoder)
        decoded ==> original
      }

      test("LocalTime binary round-trip") {
        val encoder: AvroEncoder[java.time.LocalTime] = AvroEncoder.derive[java.time.LocalTime]
        val decoder: AvroDecoder[java.time.LocalTime] = AvroDecoder.derive[java.time.LocalTime]
        val original = java.time.LocalTime.of(14, 30, 0)
        val bytes = AvroIO.toBinary(original)(encoder)
        val decoded = AvroIO.fromBinary[java.time.LocalTime](bytes)(decoder)
        decoded ==> original
      }

      test("LocalDateTime binary round-trip") {
        val encoder: AvroEncoder[java.time.LocalDateTime] = AvroEncoder.derive[java.time.LocalDateTime]
        val decoder: AvroDecoder[java.time.LocalDateTime] = AvroDecoder.derive[java.time.LocalDateTime]
        val original = java.time.LocalDateTime.of(2024, 1, 15, 14, 30, 0)
        val bytes = AvroIO.toBinary(original)(encoder)
        val decoded = AvroIO.fromBinary[java.time.LocalDateTime](bytes)(decoder)
        decoded ==> original
      }

      test("EventRecord with all logical types binary round-trip") {
        val encoder: AvroEncoder[EventRecord] = AvroEncoder.derive[EventRecord]
        val decoder: AvroDecoder[EventRecord] = AvroDecoder.derive[EventRecord]
        val original = EventRecord(
          id = java.util.UUID.fromString("550e8400-e29b-41d4-a716-446655440000"),
          timestamp = java.time.Instant.ofEpochMilli(1700000000000L),
          date = java.time.LocalDate.of(2024, 1, 15),
          time = java.time.LocalTime.of(14, 30, 0),
          localTimestamp = java.time.LocalDateTime.of(2024, 1, 15, 14, 30, 0)
        )
        val bytes = AvroIO.toBinary(original)(encoder)
        val decoded = AvroIO.fromBinary[EventRecord](bytes)(decoder)
        decoded ==> original
      }
    }

    group("per-field annotations") {

      test("@fieldName round-trip") {
        val encoder: AvroEncoder[AvroWithFieldName] = AvroEncoder.derive[AvroWithFieldName]
        val decoder: AvroDecoder[AvroWithFieldName] = AvroDecoder.derive[AvroWithFieldName]
        val original = AvroWithFieldName("Alice", 30)
        val bytes = AvroIO.toBinary(original)(encoder)
        val decoded = AvroIO.fromBinary[AvroWithFieldName](bytes)(decoder)
        decoded ==> original
      }

      test("@transientField round-trip preserves non-transient fields") {
        val encoder: AvroEncoder[AvroWithTransient] = AvroEncoder.derive[AvroWithTransient]
        val decoder: AvroDecoder[AvroWithTransient] = AvroDecoder.derive[AvroWithTransient]
        val original = AvroWithTransient("Alice", Some("cached"))
        val bytes = AvroIO.toBinary(original)(encoder)
        val decoded = AvroIO.fromBinary[AvroWithTransient](bytes)(decoder)
        // Transient field defaults to None after round-trip
        decoded ==> AvroWithTransient("Alice", None)
      }

      test("@fieldName and @transientField combined round-trip") {
        val encoder: AvroEncoder[AvroWithBothAnnotations] = AvroEncoder.derive[AvroWithBothAnnotations]
        val decoder: AvroDecoder[AvroWithBothAnnotations] = AvroDecoder.derive[AvroWithBothAnnotations]
        val original = AvroWithBothAnnotations("Alice", 42, true)
        val bytes = AvroIO.toBinary(original)(encoder)
        val decoded = AvroIO.fromBinary[AvroWithBothAnnotations](bytes)(decoder)
        // Transient field defaults to 0 after round-trip
        decoded ==> AvroWithBothAnnotations("Alice", 0, true)
      }
    }

    group("JSON") {

      test("simple case class") {
        val encoder: AvroEncoder[SimplePerson] = AvroEncoder.derive[SimplePerson]
        val decoder: AvroDecoder[SimplePerson] = AvroDecoder.derive[SimplePerson]
        val original = SimplePerson("Alice", 30)
        val json = AvroIO.toJson(original)(encoder)
        val decoded = AvroIO.fromJson[SimplePerson](json)(decoder)
        decoded ==> original
      }

      test("nested case class") {
        val encoder: AvroEncoder[PersonWithAddress] = AvroEncoder.derive[PersonWithAddress]
        val decoder: AvroDecoder[PersonWithAddress] = AvroDecoder.derive[PersonWithAddress]
        val original = PersonWithAddress("Bob", 25, Address("Main St", "NYC"))
        val json = AvroIO.toJson(original)(encoder)
        val decoded = AvroIO.fromJson[PersonWithAddress](json)(decoder)
        decoded ==> original
      }
    }
  }
}
