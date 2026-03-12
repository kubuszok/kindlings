package hearth.kindlings.integrationtests

import cats.data.{Chain, Const, NonEmptyChain, NonEmptyList, NonEmptyMap, NonEmptySet, NonEmptyVector}
import hearth.MacroSuite
import hearth.kindlings.xmlderivation.{KindlingsXmlDecoder, KindlingsXmlEncoder}

final class CatsXmlSpec extends MacroSuite {

  private def roundTrip[A](value: A, elementName: String)(implicit
      encoder: hearth.kindlings.xmlderivation.XmlEncoder[A],
      decoder: hearth.kindlings.xmlderivation.XmlDecoder[A]
  ): Either[hearth.kindlings.xmlderivation.XmlDecodingError, A] = {
    val elem = encoder.encode(value, elementName)
    decoder.decode(elem)
  }

  group("Cats + XML") {

    group("NonEmptyList") {

      test("encode") {
        val encoder = KindlingsXmlEncoder.derive[WithNEL]
        val v = WithNEL(NonEmptyList.of(1, 2, 3))
        val result = encoder.encode(v, "withNEL")
        assert(result.label == "withNEL")
        assert((result \ "values").nonEmpty)
      }

      test("round-trip") {
        implicit val encoder: hearth.kindlings.xmlderivation.XmlEncoder[WithNEL] =
          KindlingsXmlEncoder.derive[WithNEL]
        implicit val decoder: hearth.kindlings.xmlderivation.XmlDecoder[WithNEL] =
          KindlingsXmlDecoder.derive[WithNEL]
        val v = WithNEL(NonEmptyList.of(1, 2))
        val result = roundTrip(v, "withNEL")
        assert(result.isRight, s"Expected Right but got $result")
        result.foreach(decoded => decoded.values ==> NonEmptyList.of(1, 2))
      }
    }

    group("NonEmptyVector") {

      test("encode") {
        val encoder = KindlingsXmlEncoder.derive[WithNEV]
        val v = WithNEV(NonEmptyVector.of(10, 20))
        val result = encoder.encode(v, "withNEV")
        assert(result.label == "withNEV")
      }

      test("round-trip") {
        implicit val encoder: hearth.kindlings.xmlderivation.XmlEncoder[WithNEV] =
          KindlingsXmlEncoder.derive[WithNEV]
        implicit val decoder: hearth.kindlings.xmlderivation.XmlDecoder[WithNEV] =
          KindlingsXmlDecoder.derive[WithNEV]
        val v = WithNEV(NonEmptyVector.of(5))
        val result = roundTrip(v, "withNEV")
        assert(result.isRight, s"Expected Right but got $result")
        result.foreach(decoded => decoded.values ==> NonEmptyVector.of(5))
      }
    }

    group("NonEmptyChain") {

      test("encode") {
        val encoder = KindlingsXmlEncoder.derive[WithNEC]
        val v = WithNEC(NonEmptyChain.of(7, 8))
        val result = encoder.encode(v, "withNEC")
        assert(result.label == "withNEC")
      }

      test("round-trip") {
        implicit val encoder: hearth.kindlings.xmlderivation.XmlEncoder[WithNEC] =
          KindlingsXmlEncoder.derive[WithNEC]
        implicit val decoder: hearth.kindlings.xmlderivation.XmlDecoder[WithNEC] =
          KindlingsXmlDecoder.derive[WithNEC]
        val v = WithNEC(NonEmptyChain.of(3, 4))
        val result = roundTrip(v, "withNEC")
        assert(result.isRight, s"Expected Right but got $result")
      }
    }

    group("Chain") {

      test("encode") {
        val encoder = KindlingsXmlEncoder.derive[WithChain]
        val v = WithChain(Chain(1, 2, 3))
        val result = encoder.encode(v, "withChain")
        assert(result.label == "withChain")
      }

      test("encode empty") {
        val encoder = KindlingsXmlEncoder.derive[WithChain]
        val v = WithChain(Chain.empty)
        val result = encoder.encode(v, "withChain")
        assert(result.label == "withChain")
      }

      test("round-trip") {
        implicit val encoder: hearth.kindlings.xmlderivation.XmlEncoder[WithChain] =
          KindlingsXmlEncoder.derive[WithChain]
        implicit val decoder: hearth.kindlings.xmlderivation.XmlDecoder[WithChain] =
          KindlingsXmlDecoder.derive[WithChain]
        val v = WithChain(Chain(9))
        val result = roundTrip(v, "withChain")
        assert(result.isRight, s"Expected Right but got $result")
      }

      test("round-trip empty") {
        implicit val encoder: hearth.kindlings.xmlderivation.XmlEncoder[WithChain] =
          KindlingsXmlEncoder.derive[WithChain]
        implicit val decoder: hearth.kindlings.xmlderivation.XmlDecoder[WithChain] =
          KindlingsXmlDecoder.derive[WithChain]
        val v = WithChain(Chain.empty)
        val result = roundTrip(v, "withChain")
        assert(result.isRight, s"Expected Right but got $result")
      }
    }

    group("NonEmptyMap") {

      test("encode") {
        val encoder = KindlingsXmlEncoder.derive[WithNEM]
        val v = WithNEM(NonEmptyMap.of("x" -> 1, "y" -> 2))
        val result = encoder.encode(v, "withNEM")
        assert(result.label == "withNEM")
      }

      test("round-trip") {
        implicit val encoder: hearth.kindlings.xmlderivation.XmlEncoder[WithNEM] =
          KindlingsXmlEncoder.derive[WithNEM]
        implicit val decoder: hearth.kindlings.xmlderivation.XmlDecoder[WithNEM] =
          KindlingsXmlDecoder.derive[WithNEM]
        val v = WithNEM(NonEmptyMap.of("a" -> 1))
        val result = roundTrip(v, "withNEM")
        assert(result.isRight, s"Expected Right but got $result")
      }
    }

    group("NonEmptySet") {

      test("encode") {
        val encoder = KindlingsXmlEncoder.derive[WithNES]
        val v = WithNES(NonEmptySet.of(3, 1, 2))
        val result = encoder.encode(v, "withNES")
        assert(result.label == "withNES")
      }

      test("round-trip") {
        implicit val encoder: hearth.kindlings.xmlderivation.XmlEncoder[WithNES] =
          KindlingsXmlEncoder.derive[WithNES]
        implicit val decoder: hearth.kindlings.xmlderivation.XmlDecoder[WithNES] =
          KindlingsXmlDecoder.derive[WithNES]
        val v = WithNES(NonEmptySet.of(5, 10))
        val result = roundTrip(v, "withNES")
        assert(result.isRight, s"Expected Right but got $result")
      }
    }

    group("Const") {

      test("encode") {
        val encoder = KindlingsXmlEncoder.derive[WithConst]
        val v = WithConst(Const("hello"))
        val result = encoder.encode(v, "withConst")
        assert(result.label == "withConst")
        assert((result \ "value").text == "hello")
      }

      test("round-trip") {
        implicit val encoder: hearth.kindlings.xmlderivation.XmlEncoder[WithConst] =
          KindlingsXmlEncoder.derive[WithConst]
        implicit val decoder: hearth.kindlings.xmlderivation.XmlDecoder[WithConst] =
          KindlingsXmlDecoder.derive[WithConst]
        val v = WithConst(Const("world"))
        val result = roundTrip(v, "withConst")
        assert(result.isRight, s"Expected Right but got $result")
        result.foreach(decoded => decoded.value.getConst ==> "world")
      }
    }
  }
}
