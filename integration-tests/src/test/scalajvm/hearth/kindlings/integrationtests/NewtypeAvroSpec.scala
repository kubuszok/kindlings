package hearth.kindlings.integrationtests

import hearth.MacroSuite
import hearth.kindlings.avroderivation.{AvroDecoder, AvroEncoder}

final class NewtypeAvroSpec extends MacroSuite {

  group("scala-newtype + Avro") {

    test("round-trip") {
      val encoder: AvroEncoder[NewtypeUser] = AvroEncoder.derived[NewtypeUser]
      val decoder: AvroDecoder[NewtypeUser] = AvroDecoder.derived[NewtypeUser]
      decoder.decode(encoder.encode(NewtypeUser.example)) ==> NewtypeUser.example
    }
  }
}
