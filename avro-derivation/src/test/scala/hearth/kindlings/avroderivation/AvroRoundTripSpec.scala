package hearth.kindlings.avroderivation

import hearth.MacroSuite

final class AvroRoundTripSpec extends MacroSuite {

  group("AvroIO round-trip") {

    group("binary") {

      test("simple case class") {
        val encoder: AvroEncoder[SimplePerson] = AvroEncoder.derived[SimplePerson]
        val decoder: AvroDecoder[SimplePerson] = AvroDecoder.derived[SimplePerson]
        val original = SimplePerson("Alice", 30)
        val bytes = AvroIO.toBinary(original)(encoder)
        val decoded = AvroIO.fromBinary[SimplePerson](bytes)(decoder)
        decoded ==> original
      }

      test("empty case class") {
        val encoder: AvroEncoder[EmptyClass] = AvroEncoder.derived[EmptyClass]
        val decoder: AvroDecoder[EmptyClass] = AvroDecoder.derived[EmptyClass]
        val original = EmptyClass()
        val bytes = AvroIO.toBinary(original)(encoder)
        val decoded = AvroIO.fromBinary[EmptyClass](bytes)(decoder)
        decoded ==> original
      }

      test("nested case class") {
        val encoder: AvroEncoder[PersonWithAddress] = AvroEncoder.derived[PersonWithAddress]
        val decoder: AvroDecoder[PersonWithAddress] = AvroDecoder.derived[PersonWithAddress]
        val original = PersonWithAddress("Bob", 25, Address("Main St", "NYC"))
        val bytes = AvroIO.toBinary(original)(encoder)
        val decoded = AvroIO.fromBinary[PersonWithAddress](bytes)(decoder)
        decoded ==> original
      }

      test("case class with collection") {
        val encoder: AvroEncoder[TeamWithMembers] = AvroEncoder.derived[TeamWithMembers]
        val decoder: AvroDecoder[TeamWithMembers] = AvroDecoder.derived[TeamWithMembers]
        val original = TeamWithMembers("Team A", List(SimplePerson("A", 1), SimplePerson("B", 2)))
        val bytes = AvroIO.toBinary(original)(encoder)
        val decoded = AvroIO.fromBinary[TeamWithMembers](bytes)(decoder)
        decoded ==> original
      }

      test("value class") {
        val encoder: AvroEncoder[WrappedInt] = AvroEncoder.derived[WrappedInt]
        val decoder: AvroDecoder[WrappedInt] = AvroDecoder.derived[WrappedInt]
        val original = WrappedInt(42)
        val bytes = AvroIO.toBinary(original)(encoder)
        val decoded = AvroIO.fromBinary[WrappedInt](bytes)(decoder)
        decoded ==> original
      }
    }

    group("generic case classes") {

      test("Box[Int] binary round-trip") {
        val encoder: AvroEncoder[Box[Int]] = AvroEncoder.derived[Box[Int]]
        val decoder: AvroDecoder[Box[Int]] = AvroDecoder.derived[Box[Int]]
        val original = Box(42)
        val bytes = AvroIO.toBinary(original)(encoder)
        val decoded = AvroIO.fromBinary[Box[Int]](bytes)(decoder)
        decoded ==> original
      }

      test("Pair[String, Int] binary round-trip") {
        val encoder: AvroEncoder[Pair[String, Int]] = AvroEncoder.derived[Pair[String, Int]]
        val decoder: AvroDecoder[Pair[String, Int]] = AvroDecoder.derived[Pair[String, Int]]
        val original = Pair("hello", 42)
        val bytes = AvroIO.toBinary(original)(encoder)
        val decoded = AvroIO.fromBinary[Pair[String, Int]](bytes)(decoder)
        decoded ==> original
      }
    }

    group("deeply nested") {

      test("PersonFull binary round-trip") {
        val encoder: AvroEncoder[PersonFull] = AvroEncoder.derived[PersonFull]
        val decoder: AvroDecoder[PersonFull] = AvroDecoder.derived[PersonFull]
        val original = PersonFull("Alice", FullAddress("123 Main", "NYC", GeoCoordinates(40.7, -74.0)))
        val bytes = AvroIO.toBinary(original)(encoder)
        val decoded = AvroIO.fromBinary[PersonFull](bytes)(decoder)
        decoded ==> original
      }
    }

    group("type aliases") {

      test("WithAlias binary round-trip") {
        val encoder: AvroEncoder[WithAlias] = AvroEncoder.derived[WithAlias]
        val decoder: AvroDecoder[WithAlias] = AvroDecoder.derived[WithAlias]
        val original = WithAlias("Alice", 30)
        val bytes = AvroIO.toBinary(original)(encoder)
        val decoded = AvroIO.fromBinary[WithAlias](bytes)(decoder)
        decoded ==> original
      }
    }

    group("sets") {

      test("Set of ints round-trip") {
        val encoder: AvroEncoder[Set[Int]] = AvroEncoder.derived[Set[Int]]
        val decoder: AvroDecoder[Set[Int]] = AvroDecoder.derived[Set[Int]]
        val original = Set(1, 2, 3)
        val bytes = AvroIO.toBinary(original)(encoder)
        val decoded = AvroIO.fromBinary[Set[Int]](bytes)(decoder)
        decoded ==> original
      }
    }

    group("logical types") {

      test("UUID binary round-trip") {
        val encoder: AvroEncoder[java.util.UUID] = AvroEncoder.derived[java.util.UUID]
        val decoder: AvroDecoder[java.util.UUID] = AvroDecoder.derived[java.util.UUID]
        val original = java.util.UUID.fromString("550e8400-e29b-41d4-a716-446655440000")
        val bytes = AvroIO.toBinary(original)(encoder)
        val decoded = AvroIO.fromBinary[java.util.UUID](bytes)(decoder)
        decoded ==> original
      }

      test("Instant binary round-trip") {
        val encoder: AvroEncoder[java.time.Instant] = AvroEncoder.derived[java.time.Instant]
        val decoder: AvroDecoder[java.time.Instant] = AvroDecoder.derived[java.time.Instant]
        val original = java.time.Instant.ofEpochMilli(1700000000000L)
        val bytes = AvroIO.toBinary(original)(encoder)
        val decoded = AvroIO.fromBinary[java.time.Instant](bytes)(decoder)
        decoded ==> original
      }

      test("LocalDate binary round-trip") {
        val encoder: AvroEncoder[java.time.LocalDate] = AvroEncoder.derived[java.time.LocalDate]
        val decoder: AvroDecoder[java.time.LocalDate] = AvroDecoder.derived[java.time.LocalDate]
        val original = java.time.LocalDate.of(2024, 1, 15)
        val bytes = AvroIO.toBinary(original)(encoder)
        val decoded = AvroIO.fromBinary[java.time.LocalDate](bytes)(decoder)
        decoded ==> original
      }

      test("LocalTime binary round-trip") {
        val encoder: AvroEncoder[java.time.LocalTime] = AvroEncoder.derived[java.time.LocalTime]
        val decoder: AvroDecoder[java.time.LocalTime] = AvroDecoder.derived[java.time.LocalTime]
        val original = java.time.LocalTime.of(14, 30, 0)
        val bytes = AvroIO.toBinary(original)(encoder)
        val decoded = AvroIO.fromBinary[java.time.LocalTime](bytes)(decoder)
        decoded ==> original
      }

      test("LocalDateTime binary round-trip") {
        val encoder: AvroEncoder[java.time.LocalDateTime] = AvroEncoder.derived[java.time.LocalDateTime]
        val decoder: AvroDecoder[java.time.LocalDateTime] = AvroDecoder.derived[java.time.LocalDateTime]
        val original = java.time.LocalDateTime.of(2024, 1, 15, 14, 30, 0)
        val bytes = AvroIO.toBinary(original)(encoder)
        val decoded = AvroIO.fromBinary[java.time.LocalDateTime](bytes)(decoder)
        decoded ==> original
      }

      test("EventRecord with all logical types binary round-trip") {
        val encoder: AvroEncoder[EventRecord] = AvroEncoder.derived[EventRecord]
        val decoder: AvroDecoder[EventRecord] = AvroDecoder.derived[EventRecord]
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
        val encoder: AvroEncoder[AvroWithFieldName] = AvroEncoder.derived[AvroWithFieldName]
        val decoder: AvroDecoder[AvroWithFieldName] = AvroDecoder.derived[AvroWithFieldName]
        val original = AvroWithFieldName("Alice", 30)
        val bytes = AvroIO.toBinary(original)(encoder)
        val decoded = AvroIO.fromBinary[AvroWithFieldName](bytes)(decoder)
        decoded ==> original
      }

      test("@transientField round-trip preserves non-transient fields") {
        val encoder: AvroEncoder[AvroWithTransient] = AvroEncoder.derived[AvroWithTransient]
        val decoder: AvroDecoder[AvroWithTransient] = AvroDecoder.derived[AvroWithTransient]
        val original = AvroWithTransient("Alice", Some("cached"))
        val bytes = AvroIO.toBinary(original)(encoder)
        val decoded = AvroIO.fromBinary[AvroWithTransient](bytes)(decoder)
        // Transient field defaults to None after round-trip
        decoded ==> AvroWithTransient("Alice", None)
      }

      test("@fieldName and @transientField combined round-trip") {
        val encoder: AvroEncoder[AvroWithBothAnnotations] = AvroEncoder.derived[AvroWithBothAnnotations]
        val decoder: AvroDecoder[AvroWithBothAnnotations] = AvroDecoder.derived[AvroWithBothAnnotations]
        val original = AvroWithBothAnnotations("Alice", 42, true)
        val bytes = AvroIO.toBinary(original)(encoder)
        val decoded = AvroIO.fromBinary[AvroWithBothAnnotations](bytes)(decoder)
        // Transient field defaults to 0 after round-trip
        decoded ==> AvroWithBothAnnotations("Alice", 0, true)
      }
    }

    group("tuples") {

      test("Tuple2 binary round-trip") {
        val encoder: AvroEncoder[(String, Int)] = AvroEncoder.derived[(String, Int)]
        val decoder: AvroDecoder[(String, Int)] = AvroDecoder.derived[(String, Int)]
        val original = ("hello", 42)
        val bytes = AvroIO.toBinary(original)(encoder)
        val decoded = AvroIO.fromBinary[(String, Int)](bytes)(decoder)
        decoded ==> original
      }

      test("Tuple3 binary round-trip") {
        val encoder: AvroEncoder[(Int, String, Boolean)] = AvroEncoder.derived[(Int, String, Boolean)]
        val decoder: AvroDecoder[(Int, String, Boolean)] = AvroDecoder.derived[(Int, String, Boolean)]
        val original = (1, "world", true)
        val bytes = AvroIO.toBinary(original)(encoder)
        val decoded = AvroIO.fromBinary[(Int, String, Boolean)](bytes)(decoder)
        decoded ==> original
      }
    }

    group("BigDecimal decimal") {

      test("BigDecimal decimal round-trip via binary") {
        implicit val config: AvroConfig = AvroConfig().withDecimalConfig(10, 2)
        val encoder: AvroEncoder[BigDecimal] = AvroEncoder.derived[BigDecimal]
        val decoder: AvroDecoder[BigDecimal] = AvroDecoder.derived[BigDecimal]
        val original = BigDecimal("123.45")
        val bytes = AvroIO.toBinary(original)(encoder)
        val decoded = AvroIO.fromBinary[BigDecimal](bytes)(decoder)
        decoded ==> original
      }

      test("case class with BigDecimal decimal round-trip") {
        implicit val config: AvroConfig = AvroConfig().withDecimalConfig(10, 2)
        val encoder: AvroEncoder[WithBigDecimal] = AvroEncoder.derived[WithBigDecimal]
        val decoder: AvroDecoder[WithBigDecimal] = AvroDecoder.derived[WithBigDecimal]
        val original = WithBigDecimal(BigDecimal("999.99"))
        val bytes = AvroIO.toBinary(original)(encoder)
        val decoded = AvroIO.fromBinary[WithBigDecimal](bytes)(decoder)
        decoded ==> original
      }
    }

    group("Either") {

      test("Either[String, Int] Left round-trip") {
        val encoder: AvroEncoder[Either[String, Int]] = AvroEncoder.derived[Either[String, Int]]
        val decoder: AvroDecoder[Either[String, Int]] = AvroDecoder.derived[Either[String, Int]]
        val original: Either[String, Int] = Left("error")
        val bytes = AvroIO.toBinary(original)(encoder)
        val decoded = AvroIO.fromBinary[Either[String, Int]](bytes)(decoder)
        decoded ==> original
      }

      test("Either[String, Int] Right round-trip") {
        val encoder: AvroEncoder[Either[String, Int]] = AvroEncoder.derived[Either[String, Int]]
        val decoder: AvroDecoder[Either[String, Int]] = AvroDecoder.derived[Either[String, Int]]
        val original: Either[String, Int] = Right(42)
        val bytes = AvroIO.toBinary(original)(encoder)
        val decoded = AvroIO.fromBinary[Either[String, Int]](bytes)(decoder)
        decoded ==> original
      }

      test("case class with Either field round-trip") {
        val encoder: AvroEncoder[WithEither] = AvroEncoder.derived[WithEither]
        val decoder: AvroDecoder[WithEither] = AvroDecoder.derived[WithEither]
        val original = WithEither(Right(42))
        val bytes = AvroIO.toBinary(original)(encoder)
        val decoded = AvroIO.fromBinary[WithEither](bytes)(decoder)
        decoded ==> original
      }
    }

    group("Java enum round-trip") {

      test("Java enum binary round-trip") {
        val encoder: AvroEncoder[JavaColor] = AvroEncoder.derived[JavaColor]
        val decoder: AvroDecoder[JavaColor] = AvroDecoder.derived[JavaColor]
        val original = JavaColor.GREEN
        val bytes = AvroIO.toBinary(original)(encoder)
        val decoded = AvroIO.fromBinary[JavaColor](bytes)(decoder)
        decoded ==> original
      }
    }

    group("Scala Enumeration round-trip") {

      test("Scala Enumeration binary round-trip") {
        val encoder: AvroEncoder[ScalaColor.Value] = AvroEncoder.derived[ScalaColor.Value]
        val decoder: AvroDecoder[ScalaColor.Value] = AvroDecoder.derived[ScalaColor.Value]
        val original: ScalaColor.Value = ScalaColor.Blue
        val bytes = AvroIO.toBinary(original)(encoder)
        val decoded = AvroIO.fromBinary[ScalaColor.Value](bytes)(decoder)
        decoded ==> original
      }
    }

    group("@avroFixed") {

      test("WithFixedBytes binary round-trip") {
        val encoder: AvroEncoder[WithFixedBytes] = AvroEncoder.derived[WithFixedBytes]
        val decoder: AvroDecoder[WithFixedBytes] = AvroDecoder.derived[WithFixedBytes]
        val original = WithFixedBytes(Array[Byte](1, 2, 3, 4))
        val bytes = AvroIO.toBinary(original)(encoder)
        val decoded = AvroIO.fromBinary[WithFixedBytes](bytes)(decoder)
        decoded.id.toList ==> original.id.toList
      }

      test("WithFixedAndRegularBytes binary round-trip (mixed FIXED + BYTES)") {
        val encoder: AvroEncoder[WithFixedAndRegularBytes] = AvroEncoder.derived[WithFixedAndRegularBytes]
        val decoder: AvroDecoder[WithFixedAndRegularBytes] = AvroDecoder.derived[WithFixedAndRegularBytes]
        val original = WithFixedAndRegularBytes(
          token = Array.fill[Byte](16)(0x42),
          data = Array[Byte](10, 20, 30)
        )
        val bytes = AvroIO.toBinary(original)(encoder)
        val decoded = AvroIO.fromBinary[WithFixedAndRegularBytes](bytes)(decoder)
        decoded.token.toList ==> original.token.toList
        decoded.data.toList ==> original.data.toList
      }
    }

    group("ByteBuffer") {

      test("WithByteBuffer binary round-trip") {
        val encoder: AvroEncoder[WithByteBuffer] = AvroEncoder.derived[WithByteBuffer]
        val decoder: AvroDecoder[WithByteBuffer] = AvroDecoder.derived[WithByteBuffer]
        val original = WithByteBuffer(java.nio.ByteBuffer.wrap(Array[Byte](1, 2, 3, 4)))
        val bytes = AvroIO.toBinary(original)(encoder)
        val decoded = AvroIO.fromBinary[WithByteBuffer](bytes)(decoder)
        val decodedBytes = new Array[Byte](decoded.data.remaining())
        decoded.data.get(decodedBytes)
        val originalBytes = Array[Byte](1, 2, 3, 4)
        decodedBytes.toList ==> originalBytes.toList
      }
    }

    group("@avroError") {

      test("@avroError record round-trip") {
        val encoder: AvroEncoder[AvroErrorRecord] = AvroEncoder.derived[AvroErrorRecord]
        val decoder: AvroDecoder[AvroErrorRecord] = AvroDecoder.derived[AvroErrorRecord]
        val original = AvroErrorRecord(500, "Internal Server Error")
        val bytes = AvroIO.toBinary(original)(encoder)
        val decoded = AvroIO.fromBinary[AvroErrorRecord](bytes)(decoder)
        decoded ==> original
      }
    }

    group("JSON") {

      test("simple case class") {
        val encoder: AvroEncoder[SimplePerson] = AvroEncoder.derived[SimplePerson]
        val decoder: AvroDecoder[SimplePerson] = AvroDecoder.derived[SimplePerson]
        val original = SimplePerson("Alice", 30)
        val json = AvroIO.toJson(original)(encoder)
        val decoded = AvroIO.fromJson[SimplePerson](json)(decoder)
        decoded ==> original
      }

      test("nested case class") {
        val encoder: AvroEncoder[PersonWithAddress] = AvroEncoder.derived[PersonWithAddress]
        val decoder: AvroDecoder[PersonWithAddress] = AvroDecoder.derived[PersonWithAddress]
        val original = PersonWithAddress("Bob", 25, Address("Main St", "NYC"))
        val json = AvroIO.toJson(original)(encoder)
        val decoded = AvroIO.fromJson[PersonWithAddress](json)(decoder)
        decoded ==> original
      }
    }

    group("annotation round-trips") {

      test("@avroNoDefault round-trip (decode without default)") {
        val encoder: AvroEncoder[WithNoDefault] = AvroEncoder.derived[WithNoDefault]
        val decoder: AvroDecoder[WithNoDefault] = AvroDecoder.derived[WithNoDefault]
        val original = WithNoDefault("Alice", 42)
        val json = AvroIO.toJson(original)(encoder)
        val decoded = AvroIO.fromJson[WithNoDefault](json)(decoder)
        decoded ==> original
      }

      test("@avroEnumDefault round-trip") {
        val encoder: AvroEncoder[SizeWithDefault] = AvroEncoder.derived[SizeWithDefault]
        val decoder: AvroDecoder[SizeWithDefault] = AvroDecoder.derived[SizeWithDefault]
        val original: SizeWithDefault = Large
        val json = AvroIO.toJson(original)(encoder)
        val decoded = AvroIO.fromJson[SizeWithDefault](json)(decoder)
        decoded ==> original
      }
    }

    group("recursive types") {

      test("self-recursive case class (RecursiveNode) binary round-trip") {
        val encoder: AvroEncoder[RecursiveNode] = AvroEncoder.derived[RecursiveNode]
        val decoder: AvroDecoder[RecursiveNode] = AvroDecoder.derived[RecursiveNode]
        val original = RecursiveNode(1, List(RecursiveNode(2, List()), RecursiveNode(3, List())))
        val bytes = AvroIO.toBinary(original)(encoder)
        val decoded = AvroIO.fromBinary[RecursiveNode](bytes)(decoder)
        decoded ==> original
      }

      test("recursive via Option (LinkedNode) binary round-trip") {
        val encoder: AvroEncoder[LinkedNode] = AvroEncoder.derived[LinkedNode]
        val decoder: AvroDecoder[LinkedNode] = AvroDecoder.derived[LinkedNode]
        val original = LinkedNode("a", Some(LinkedNode("b", Some(LinkedNode("c", None)))))
        val bytes = AvroIO.toBinary(original)(encoder)
        val decoded = AvroIO.fromBinary[LinkedNode](bytes)(decoder)
        decoded ==> original
      }

      test("indirect recursive type (RecursiveParent) binary round-trip") {
        val encoder: AvroEncoder[RecursiveParent] = AvroEncoder.derived[RecursiveParent]
        val decoder: AvroDecoder[RecursiveParent] = AvroDecoder.derived[RecursiveParent]
        val original = RecursiveParent(
          "root",
          List(
            RecursiveNode(1, List(RecursiveNode(2, List()))),
            RecursiveNode(3, List())
          )
        )
        val bytes = AvroIO.toBinary(original)(encoder)
        val decoded = AvroIO.fromBinary[RecursiveParent](bytes)(decoder)
        decoded ==> original
      }

      test("mixed sealed trait (case objects + case classes) round-trip") {
        val encoder: AvroEncoder[MixedEvent] = AvroEncoder.derived[MixedEvent]
        val decoder: AvroDecoder[MixedEvent] = AvroDecoder.derived[MixedEvent]
        val values: List[MixedEvent] = List(Started, Stopped, Error("boom"))
        values.foreach { original =>
          val bytes = AvroIO.toBinary(original)(encoder)
          val decoded = AvroIO.fromBinary[MixedEvent](bytes)(decoder)
          decoded ==> original
        }
      }

      test("mutually recursive types (MutRecA/MutRecB) binary round-trip") {
        val encoder: AvroEncoder[MutRecA] = AvroEncoder.derived[MutRecA]
        val decoder: AvroDecoder[MutRecA] = AvroDecoder.derived[MutRecA]
        val original = MutRecA(1, Some(MutRecB("x", Some(MutRecA(2, None)))))
        val bytes = AvroIO.toBinary(original)(encoder)
        val decoded = AvroIO.fromBinary[MutRecA](bytes)(decoder)
        decoded ==> original
      }

      test("mutually recursive types (MutRecA) with None terminators round-trip") {
        val encoder: AvroEncoder[MutRecA] = AvroEncoder.derived[MutRecA]
        val decoder: AvroDecoder[MutRecA] = AvroDecoder.derived[MutRecA]
        val original = MutRecA(99, None)
        val bytes = AvroIO.toBinary(original)(encoder)
        val decoded = AvroIO.fromBinary[MutRecA](bytes)(decoder)
        decoded ==> original
      }

      test("mutually recursive types (MutRecB) binary round-trip") {
        val encoder: AvroEncoder[MutRecB] = AvroEncoder.derived[MutRecB]
        val decoder: AvroDecoder[MutRecB] = AvroDecoder.derived[MutRecB]
        val original = MutRecB("hello", Some(MutRecA(10, Some(MutRecB("world", None)))))
        val bytes = AvroIO.toBinary(original)(encoder)
        val decoded = AvroIO.fromBinary[MutRecB](bytes)(decoder)
        decoded ==> original
      }

      test("indirect recursion via container (Forest/TreeCaseClass) binary round-trip") {
        val encoder: AvroEncoder[Forest] = AvroEncoder.derived[Forest]
        val decoder: AvroDecoder[Forest] = AvroDecoder.derived[Forest]
        val original = Forest(
          List(
            TreeCaseClass(1, List(TreeCaseClass(2, List()), TreeCaseClass(3, List()))),
            TreeCaseClass(4, List())
          )
        )
        val bytes = AvroIO.toBinary(original)(encoder)
        val decoded = AvroIO.fromBinary[Forest](bytes)(decoder)
        decoded ==> original
      }

      test("@avroScalePrecision on BigDecimal field (issue #110)") {
        val encoder: AvroEncoder[WithPerFieldDecimal] = AvroEncoder.derived[WithPerFieldDecimal]
        val decoder: AvroDecoder[WithPerFieldDecimal] = AvroDecoder.derived[WithPerFieldDecimal]
        val original = WithPerFieldDecimal(price = BigDecimal("123.4567"), label = "test")
        val bytes = AvroIO.toBinary(original)(encoder)
        val decoded = AvroIO.fromBinary[WithPerFieldDecimal](bytes)(decoder)
        decoded ==> original
      }

      test("@avroName on sealed trait subtypes (issue #108)") {
        val encoder: AvroEncoder[OuterWithRenamedInner] = AvroEncoder.derived[OuterWithRenamedInner]
        val decoder: AvroDecoder[OuterWithRenamedInner] = AvroDecoder.derived[OuterWithRenamedInner]
        val values = List(
          OuterWithRenamedInner(RenamedFoo("hello")),
          OuterWithRenamedInner(RenamedBar("world"))
        )
        values.foreach { original =>
          val bytes = AvroIO.toBinary(original)(encoder)
          val decoded = AvroIO.fromBinary[OuterWithRenamedInner](bytes)(decoder)
          decoded ==> original
        }
      }
    }
  }
}
