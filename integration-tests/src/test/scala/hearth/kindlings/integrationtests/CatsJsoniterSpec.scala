package hearth.kindlings.integrationtests

import cats.data.{Chain, Const, NonEmptyChain, NonEmptyList, NonEmptyMap, NonEmptySet, NonEmptyVector}
import hearth.MacroSuite
import hearth.kindlings.jsoniterderivation.KindlingsJsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.core.*

final class CatsJsoniterSpec extends MacroSuite {

  private def roundTrip[A](value: A)(implicit codec: JsonValueCodec[A]): A =
    readFromString[A](writeToString(value))

  group("Cats + Jsoniter") {

    group("NonEmptyList") {

      test("round-trip") {
        implicit val codec: JsonValueCodec[WithNEL] = KindlingsJsonValueCodec.derive[WithNEL]
        val v = WithNEL(NonEmptyList.of(1, 2, 3))
        val decoded = roundTrip(v)
        decoded.values ==> NonEmptyList.of(1, 2, 3)
      }
    }

    group("NonEmptyVector") {

      test("round-trip") {
        implicit val codec: JsonValueCodec[WithNEV] = KindlingsJsonValueCodec.derive[WithNEV]
        val v = WithNEV(NonEmptyVector.of(10, 20))
        val decoded = roundTrip(v)
        decoded.values ==> NonEmptyVector.of(10, 20)
      }
    }

    group("NonEmptyChain") {

      test("round-trip") {
        implicit val codec: JsonValueCodec[WithNEC] = KindlingsJsonValueCodec.derive[WithNEC]
        val v = WithNEC(NonEmptyChain.of(7, 8))
        val decoded = roundTrip(v)
        assert(decoded.values.iterator.toList == List(7, 8))
      }
    }

    group("Chain") {

      test("round-trip") {
        implicit val codec: JsonValueCodec[WithChain] = KindlingsJsonValueCodec.derive[WithChain]
        val v = WithChain(Chain(1, 2, 3))
        val decoded = roundTrip(v)
        assert(decoded.values.toList == List(1, 2, 3))
      }

      test("round-trip empty") {
        implicit val codec: JsonValueCodec[WithChain] = KindlingsJsonValueCodec.derive[WithChain]
        val v = WithChain(Chain.empty)
        val decoded = roundTrip(v)
        assert(decoded.values.toList == List.empty)
      }
    }

    group("NonEmptyMap") {

      test("round-trip") {
        implicit val codec: JsonValueCodec[WithNEM] = KindlingsJsonValueCodec.derive[WithNEM]
        val v = WithNEM(NonEmptyMap.of("x" -> 1, "y" -> 2))
        val decoded = roundTrip(v)
        assert(decoded.values.toSortedMap.size == 2)
      }
    }

    group("NonEmptySet") {

      test("round-trip") {
        implicit val codec: JsonValueCodec[WithNES] = KindlingsJsonValueCodec.derive[WithNES]
        val v = WithNES(NonEmptySet.of(3, 1, 2))
        val decoded = roundTrip(v)
        assert(decoded.values.toSortedSet == Set(1, 2, 3))
      }
    }

    group("Const") {

      test("round-trip") {
        implicit val codec: JsonValueCodec[WithConst] = KindlingsJsonValueCodec.derive[WithConst]
        val v = WithConst(Const("hello"))
        val decoded = roundTrip(v)
        decoded.value.getConst ==> "hello"
      }
    }
  }
}
