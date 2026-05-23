package hearth.kindlings.integrationtests

import io.github.iltotore.iron.*
import io.github.iltotore.iron.constraint.numeric.*
import hearth.MacroSuite
import hearth.kindlings.avroderivation.{AvroDecoder, AvroEncoder}

final class IronAvroSpec extends MacroSuite {

  group("Iron + Avro") {

    test("round-trip") {
      val encoder: AvroEncoder[IronPerson] = AvroEncoder.derive[IronPerson]
      val decoder: AvroDecoder[IronPerson] = AvroDecoder.derive[IronPerson]
      val v = IronPerson("Alice", 30)
      val decoded = decoder.decode(encoder.encode(v))
      assert(decoded.name == "Alice")
    }
  }
}
