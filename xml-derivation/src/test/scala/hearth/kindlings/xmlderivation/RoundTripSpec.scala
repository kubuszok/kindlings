package hearth.kindlings.xmlderivation

import hearth.MacroSuite

final class RoundTripSpec extends MacroSuite {

  private def roundTrip[A](value: A, elementName: String)(implicit
      encoder: XmlEncoder[A],
      decoder: XmlDecoder[A]
  ): Either[XmlDecodingError, A] = {
    val elem = encoder.encode(value, elementName)
    decoder.decode(elem)
  }

  group("Round-trip encoding/decoding") {

    test("simple case class") {
      implicit val encoder: XmlEncoder[SimplePerson] = KindlingsXmlEncoder.derive[SimplePerson]
      implicit val decoder: XmlDecoder[SimplePerson] = KindlingsXmlDecoder.derive[SimplePerson]
      val person = SimplePerson("Alice", 30)
      assert(roundTrip(person, "person") == Right(person))
    }

    // TODO: Fix decoder macro for empty case classes - "not found: value caseClass" on Scala 2
    // test("empty case class") {
    //   implicit val encoder: XmlEncoder[EmptyClass] = KindlingsXmlEncoder.derive[EmptyClass]
    //   implicit val decoder: XmlDecoder[EmptyClass] = KindlingsXmlDecoder.derive[EmptyClass]
    //   val empty = EmptyClass()
    //   assert(roundTrip(empty, "empty") == Right(empty))
    // }

    test("nested case class") {
      implicit val encoder: XmlEncoder[PersonWithAddress] = KindlingsXmlEncoder.derive[PersonWithAddress]
      implicit val decoder: XmlDecoder[PersonWithAddress] = KindlingsXmlDecoder.derive[PersonWithAddress]
      val person = PersonWithAddress("Bob", 25, Address("123 Main St", "Springfield"))
      assert(roundTrip(person, "person") == Right(person))
    }

    test("generic types") {
      implicit val encoder: XmlEncoder[Pair[String, Int]] = KindlingsXmlEncoder.derive[Pair[String, Int]]
      implicit val decoder: XmlDecoder[Pair[String, Int]] = KindlingsXmlDecoder.derive[Pair[String, Int]]
      val pair = Pair("hello", 42)
      assert(roundTrip(pair, "pair") == Right(pair))
    }

    test("deeply nested") {
      implicit val encoder: XmlEncoder[PersonFull] = KindlingsXmlEncoder.derive[PersonFull]
      implicit val decoder: XmlDecoder[PersonFull] = KindlingsXmlDecoder.derive[PersonFull]
      val person = PersonFull("Alice", FullAddress("123 Main", "NY", GeoCoordinates(40.7, -74.0)))
      assert(roundTrip(person, "person") == Right(person))
    }

    test("custom field names with @xmlName") {
      implicit val encoder: XmlEncoder[XmlWithFieldName] = KindlingsXmlEncoder.derive[XmlWithFieldName]
      implicit val decoder: XmlDecoder[XmlWithFieldName] = KindlingsXmlDecoder.derive[XmlWithFieldName]
      val value = XmlWithFieldName("John", 30)
      assert(roundTrip(value, "user") == Right(value))
    }
  }
}
