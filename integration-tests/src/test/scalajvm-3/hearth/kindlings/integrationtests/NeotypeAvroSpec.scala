package hearth.kindlings.integrationtests

import hearth.MacroSuite
import hearth.kindlings.avroderivation.{AvroDecoder, AvroEncoder}

final class NeotypeAvroSpec extends MacroSuite {

  group("Neotype + Avro") {

    test("round-trip") {
      val encoder: AvroEncoder[NeotypePerson] = AvroEncoder.derived[NeotypePerson]
      val decoder: AvroDecoder[NeotypePerson] = AvroDecoder.derived[NeotypePerson]
      val person = NeotypePerson(NeotypeEmail("alice@example.com"), NeotypeAge(30), NeotypeNickname("ally"))
      decoder.decode(encoder.encode(person)) ==> person
    }
  }
}
