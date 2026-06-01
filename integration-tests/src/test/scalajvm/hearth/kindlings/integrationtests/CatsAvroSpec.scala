package hearth.kindlings.integrationtests

import cats.data.{Chain, Const, NonEmptyList, NonEmptyVector}
import hearth.MacroSuite
import hearth.kindlings.avroderivation.{AvroDecoder, AvroEncoder}

final class CatsAvroSpec extends MacroSuite {

  private def roundTrip[A](value: A)(encoder: AvroEncoder[A], decoder: AvroDecoder[A]): A =
    decoder.decode(encoder.encode(value))

  group("Cats + Avro") {

    group("NonEmptyList") {

      test("round-trip") {
        val encoder: AvroEncoder[WithNEL] = AvroEncoder.derived[WithNEL]
        val decoder: AvroDecoder[WithNEL] = AvroDecoder.derived[WithNEL]
        val v = WithNEL(NonEmptyList.of(1, 2, 3))
        val decoded = roundTrip(v)(encoder, decoder)
        decoded.values ==> NonEmptyList.of(1, 2, 3)
      }
    }

    group("NonEmptyVector") {

      test("round-trip") {
        val encoder: AvroEncoder[WithNEV] = AvroEncoder.derived[WithNEV]
        val decoder: AvroDecoder[WithNEV] = AvroDecoder.derived[WithNEV]
        val v = WithNEV(NonEmptyVector.of(10, 20))
        val decoded = roundTrip(v)(encoder, decoder)
        decoded.values ==> NonEmptyVector.of(10, 20)
      }
    }

    group("Chain") {

      test("round-trip") {
        val encoder: AvroEncoder[WithChain] = AvroEncoder.derived[WithChain]
        val decoder: AvroDecoder[WithChain] = AvroDecoder.derived[WithChain]
        val v = WithChain(Chain(1, 2, 3))
        val decoded = roundTrip(v)(encoder, decoder)
        assert(decoded.values.toList == List(1, 2, 3))
      }
    }

    group("Const") {

      test("round-trip") {
        val encoder: AvroEncoder[WithConst] = AvroEncoder.derived[WithConst]
        val decoder: AvroDecoder[WithConst] = AvroDecoder.derived[WithConst]
        val v = WithConst(Const("hello"))
        val decoded = roundTrip(v)(encoder, decoder)
        decoded.value.getConst ==> "hello"
      }
    }
  }
}
