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
