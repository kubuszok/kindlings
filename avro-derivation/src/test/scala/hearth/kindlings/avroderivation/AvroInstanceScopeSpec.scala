package hearth.kindlings.avroderivation

import hearth.MacroSuite
import org.apache.avro.Schema

/** A field type whose encoder/decoder are `def`s counting how often they are created. */
final case class AvroCounted(value: Int)
object AvroCounted {
  @volatile var instancesCreated: Int = 0
  // one implicit that is an encoder, a decoder and a schema (two would make `AvroSchemaFor` ambiguous)
  implicit def countedCodec: AvroEncoder[AvroCounted] with AvroDecoder[AvroCounted] = {
    instancesCreated += 1
    new AvroEncoder[AvroCounted] with AvroDecoder[AvroCounted] {
      val schema: Schema = Schema.create(Schema.Type.INT)
      def encode(value: AvroCounted): Any = value.value
      def decode(value: Any): AvroCounted = AvroCounted(value.asInstanceOf[Int])
    }
  }
}
final case class WithAvroCounted(counted: AvroCounted, other: Int)
final case class WithCountedEither(either: Either[AvroCounted, String])

/** Derived encoders and decoders build schemas and resolve the instances of their fields once per instance, not on
  * every encoded/decoded value.
  */
final class AvroInstanceScopeSpec extends MacroSuite {

  // Calls `op` three times and returns how many field instances the 2nd and 3rd call created.
  private def createdByRepeatedCalls(op: () => Any): Int = {
    op()
    val before = AvroCounted.instancesCreated
    op()
    op()
    AvroCounted.instancesCreated - before
  }

  test("encoder and decoder of a case class with a field instance") {
    val encoder = AvroEncoder.derived[WithAvroCounted]
    val decoder = AvroDecoder.derived[WithAvroCounted]
    val value = WithAvroCounted(AvroCounted(1), 2)
    val encoded = encoder.encode(value)
    decoder.decode(encoded) ==> value
    createdByRepeatedCalls(() => encoder.encode(value)) ==> 0
    createdByRepeatedCalls(() => decoder.decode(encoded)) ==> 0
  }

  test("decoder of an Either (its union schema)") {
    val encoder = AvroEncoder.derived[WithCountedEither]
    val decoder = AvroDecoder.derived[WithCountedEither]
    List(WithCountedEither(Left(AvroCounted(1))), WithCountedEither(Right("x"))).foreach { value =>
      val encoded = encoder.encode(value)
      decoder.decode(encoded) ==> value
      createdByRepeatedCalls(() => decoder.decode(encoded)) ==> 0
    }
  }

  test("enums of case objects round-trip (schema and symbols)") {
    val encoder = AvroEncoder.derived[Color]
    val decoder = AvroDecoder.derived[Color]
    List[Color](Red, Green, Blue).foreach { c =>
      decoder.decode(encoder.encode(c)) ==> c
    }
  }
}
