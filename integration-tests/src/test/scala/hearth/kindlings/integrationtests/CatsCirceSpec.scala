package hearth.kindlings.integrationtests

import cats.data.{Chain, Const, NonEmptyChain, NonEmptyList, NonEmptyMap, NonEmptySet, NonEmptyVector}
import hearth.MacroSuite
import hearth.kindlings.circederivation.{KindlingsDecoder, KindlingsEncoder}
import io.circe.Json

final class CatsCirceSpec extends MacroSuite {

  group("Cats + Circe") {

    group("NonEmptyList") {

      test("encode") {
        val v = WithNEL(NonEmptyList.of(1, 2, 3))
        val json = KindlingsEncoder.encode(v)
        val expected = Json.obj("values" -> Json.arr(Json.fromInt(1), Json.fromInt(2), Json.fromInt(3)))
        json ==> expected
      }

      test("decode") {
        val json = Json.obj("values" -> Json.arr(Json.fromInt(1), Json.fromInt(2)))
        val result = KindlingsDecoder.decode[WithNEL](json)
        assert(result.isRight, s"Expected Right but got $result")
        result.foreach(v => v.values ==> NonEmptyList.of(1, 2))
      }

      test("decode rejects empty") {
        val json = Json.obj("values" -> Json.arr())
        val result = KindlingsDecoder.decode[WithNEL](json)
        assert(result.isLeft, s"Expected Left but got $result")
      }
    }

    group("NonEmptyVector") {

      test("encode") {
        val v = WithNEV(NonEmptyVector.of(10, 20))
        val json = KindlingsEncoder.encode(v)
        val expected = Json.obj("values" -> Json.arr(Json.fromInt(10), Json.fromInt(20)))
        json ==> expected
      }

      test("decode") {
        val json = Json.obj("values" -> Json.arr(Json.fromInt(5)))
        val result = KindlingsDecoder.decode[WithNEV](json)
        assert(result.isRight, s"Expected Right but got $result")
        result.foreach(v => v.values ==> NonEmptyVector.of(5))
      }

      test("decode rejects empty") {
        val json = Json.obj("values" -> Json.arr())
        val result = KindlingsDecoder.decode[WithNEV](json)
        assert(result.isLeft, s"Expected Left but got $result")
      }
    }

    group("NonEmptyChain") {

      test("encode") {
        val v = WithNEC(NonEmptyChain.of(7, 8))
        val json = KindlingsEncoder.encode(v)
        val expected = Json.obj("values" -> Json.arr(Json.fromInt(7), Json.fromInt(8)))
        json ==> expected
      }

      test("decode") {
        val json = Json.obj("values" -> Json.arr(Json.fromInt(3), Json.fromInt(4)))
        val result = KindlingsDecoder.decode[WithNEC](json)
        assert(result.isRight, s"Expected Right but got $result")
      }

      test("decode rejects empty") {
        val json = Json.obj("values" -> Json.arr())
        val result = KindlingsDecoder.decode[WithNEC](json)
        assert(result.isLeft, s"Expected Left but got $result")
      }
    }

    group("Chain") {

      test("encode") {
        val v = WithChain(Chain(1, 2, 3))
        val json = KindlingsEncoder.encode(v)
        val expected = Json.obj("values" -> Json.arr(Json.fromInt(1), Json.fromInt(2), Json.fromInt(3)))
        json ==> expected
      }

      test("encode empty") {
        val v = WithChain(Chain.empty)
        val json = KindlingsEncoder.encode(v)
        val expected = Json.obj("values" -> Json.arr())
        json ==> expected
      }

      test("decode") {
        val json = Json.obj("values" -> Json.arr(Json.fromInt(9)))
        val result = KindlingsDecoder.decode[WithChain](json)
        assert(result.isRight, s"Expected Right but got $result")
      }

      test("decode empty") {
        val json = Json.obj("values" -> Json.arr())
        val result = KindlingsDecoder.decode[WithChain](json)
        assert(result.isRight, s"Expected Right but got $result")
      }
    }

    group("NonEmptyMap") {

      test("encode") {
        val v = WithNEM(NonEmptyMap.of("x" -> 1, "y" -> 2))
        val json = KindlingsEncoder.encode(v)
        // Map encoded as object with string keys
        val obj = json.hcursor.downField("values").focus.get
        assert(obj.isObject, s"Expected object but got $obj")
      }

      test("decode") {
        val json = Json.obj("values" -> Json.obj("a" -> Json.fromInt(1)))
        val result = KindlingsDecoder.decode[WithNEM](json)
        assert(result.isRight, s"Expected Right but got $result")
      }

      test("decode rejects empty") {
        val json = Json.obj("values" -> Json.obj())
        val result = KindlingsDecoder.decode[WithNEM](json)
        assert(result.isLeft, s"Expected Left but got $result")
      }
    }

    group("NonEmptySet") {

      test("encode") {
        val v = WithNES(NonEmptySet.of(3, 1, 2))
        val json = KindlingsEncoder.encode(v)
        val arr = json.hcursor.downField("values").focus.flatMap(_.asArray).get
        assert(arr.nonEmpty, s"Expected non-empty array but got $arr")
      }

      test("decode") {
        val json = Json.obj("values" -> Json.arr(Json.fromInt(5), Json.fromInt(10)))
        val result = KindlingsDecoder.decode[WithNES](json)
        assert(result.isRight, s"Expected Right but got $result")
      }

      test("decode rejects empty") {
        val json = Json.obj("values" -> Json.arr())
        val result = KindlingsDecoder.decode[WithNES](json)
        assert(result.isLeft, s"Expected Left but got $result")
      }
    }

    group("Const") {

      test("encode") {
        val v = WithConst(Const("hello"))
        val json = KindlingsEncoder.encode(v)
        val expected = Json.obj("value" -> Json.fromString("hello"))
        json ==> expected
      }

      test("decode") {
        val json = Json.obj("value" -> Json.fromString("world"))
        val result = KindlingsDecoder.decode[WithConst](json)
        assert(result.isRight, s"Expected Right but got $result")
        result.foreach(v => v.value.getConst ==> "world")
      }
    }

    // TODO: Validated/ValidatedNel Circe tests — requires Circe to use IsEither for Validated
    // (currently Circe treats it as a sealed trait enum, which fails on Scala 2 due to
    // Invalid[E]/Valid[A] type parameter mismatch with Validated[E, A])
  }
}
