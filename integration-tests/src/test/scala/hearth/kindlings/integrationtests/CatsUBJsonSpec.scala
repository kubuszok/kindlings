package hearth.kindlings.integrationtests

import cats.data.{Chain, Const, NonEmptyChain, NonEmptyList, NonEmptyMap, NonEmptySet, NonEmptyVector}
import hearth.MacroSuite
import hearth.kindlings.ubjsonderivation.UBJsonValueCodec
import hearth.kindlings.ubjsonderivation.internal.runtime.UBJsonDerivationUtils

final class CatsUBJsonSpec extends MacroSuite {

  private def roundTrip[A](value: A)(implicit codec: hearth.kindlings.ubjsonderivation.UBJsonValueCodec[A]): A =
    UBJsonDerivationUtils.readFromBytes[A](UBJsonDerivationUtils.writeToBytes[A](value)(codec))(codec)

  group("Cats + UBJson") {

    group("NonEmptyList") {

      test("round-trip") {
        implicit val codec: hearth.kindlings.ubjsonderivation.UBJsonValueCodec[WithNEL] =
          UBJsonValueCodec.derived[WithNEL]
        val v = WithNEL(NonEmptyList.of(1, 2, 3))
        val decoded = roundTrip(v)
        decoded.values ==> NonEmptyList.of(1, 2, 3)
      }
    }

    group("NonEmptyVector") {

      test("round-trip") {
        implicit val codec: hearth.kindlings.ubjsonderivation.UBJsonValueCodec[WithNEV] =
          UBJsonValueCodec.derived[WithNEV]
        val v = WithNEV(NonEmptyVector.of(10, 20))
        val decoded = roundTrip(v)
        decoded.values ==> NonEmptyVector.of(10, 20)
      }
    }

    group("NonEmptyChain") {

      test("round-trip") {
        implicit val codec: hearth.kindlings.ubjsonderivation.UBJsonValueCodec[WithNEC] =
          UBJsonValueCodec.derived[WithNEC]
        val v = WithNEC(NonEmptyChain.of(7, 8))
        val decoded = roundTrip(v)
        assert(decoded.values.toList == List(7, 8))
      }
    }

    group("Chain") {

      test("round-trip") {
        implicit val codec: hearth.kindlings.ubjsonderivation.UBJsonValueCodec[WithChain] =
          UBJsonValueCodec.derived[WithChain]
        val v = WithChain(Chain(1, 2, 3))
        val decoded = roundTrip(v)
        assert(decoded.values.toList == List(1, 2, 3))
      }

      test("round-trip empty") {
        implicit val codec: hearth.kindlings.ubjsonderivation.UBJsonValueCodec[WithChain] =
          UBJsonValueCodec.derived[WithChain]
        val v = WithChain(Chain.empty)
        val decoded = roundTrip(v)
        assert(decoded.values.toList == List.empty)
      }
    }

    group("NonEmptyMap") {

      test("round-trip") {
        implicit val codec: hearth.kindlings.ubjsonderivation.UBJsonValueCodec[WithNEM] =
          UBJsonValueCodec.derived[WithNEM]
        val v = WithNEM(NonEmptyMap.of("x" -> 1, "y" -> 2))
        val decoded = roundTrip(v)
        assert(decoded.values.toSortedMap.size == 2)
      }
    }

    group("NonEmptySet") {

      test("round-trip") {
        implicit val codec: hearth.kindlings.ubjsonderivation.UBJsonValueCodec[WithNES] =
          UBJsonValueCodec.derived[WithNES]
        val v = WithNES(NonEmptySet.of(3, 1, 2))
        val decoded = roundTrip(v)
        assert(decoded.values.toSortedSet == Set(1, 2, 3))
      }
    }

    group("Const") {

      test("round-trip") {
        implicit val codec: hearth.kindlings.ubjsonderivation.UBJsonValueCodec[WithConst] =
          UBJsonValueCodec.derived[WithConst]
        val v = WithConst(Const("hello"))
        val decoded = roundTrip(v)
        decoded.value.getConst ==> "hello"
      }
    }
  }
}
