package hearth.kindlings.integrationtests

import cats.data.{Chain, Const, NonEmptyList, NonEmptyVector}
import hearth.MacroSuite
import hearth.kindlings.yamlderivation.{KindlingsYamlDecoder, KindlingsYamlEncoder}

final class CatsYamlSpec extends MacroSuite {

  private def roundTrip[A](value: A)(implicit
      encoder: org.virtuslab.yaml.YamlEncoder[A],
      decoder: org.virtuslab.yaml.YamlDecoder[A]
  ): Either[?, A] = {
    val yaml = KindlingsYamlEncoder.toYamlString(value)
    KindlingsYamlDecoder.fromYamlString[A](yaml)
  }

  group("Cats + YAML") {

    group("NonEmptyList") {

      test("round-trip") {
        implicit val encoder: org.virtuslab.yaml.YamlEncoder[WithNEL] =
          KindlingsYamlEncoder.derived[WithNEL]
        implicit val decoder: org.virtuslab.yaml.YamlDecoder[WithNEL] =
          KindlingsYamlDecoder.derived[WithNEL]
        val v = WithNEL(NonEmptyList.of(1, 2, 3))
        val result = roundTrip(v)
        assert(result.isRight, s"Expected Right but got $result")
      }
    }

    group("NonEmptyVector") {

      test("round-trip") {
        implicit val encoder: org.virtuslab.yaml.YamlEncoder[WithNEV] =
          KindlingsYamlEncoder.derived[WithNEV]
        implicit val decoder: org.virtuslab.yaml.YamlDecoder[WithNEV] =
          KindlingsYamlDecoder.derived[WithNEV]
        val v = WithNEV(NonEmptyVector.of(10, 20))
        val result = roundTrip(v)
        assert(result.isRight, s"Expected Right but got $result")
      }
    }

    group("Chain") {

      test("round-trip") {
        implicit val encoder: org.virtuslab.yaml.YamlEncoder[WithChain] =
          KindlingsYamlEncoder.derived[WithChain]
        implicit val decoder: org.virtuslab.yaml.YamlDecoder[WithChain] =
          KindlingsYamlDecoder.derived[WithChain]
        val v = WithChain(Chain(1, 2, 3))
        val result = roundTrip(v)
        assert(result.isRight, s"Expected Right but got $result")
      }
    }

    group("Const") {

      test("round-trip") {
        implicit val encoder: org.virtuslab.yaml.YamlEncoder[WithConst] =
          KindlingsYamlEncoder.derived[WithConst]
        implicit val decoder: org.virtuslab.yaml.YamlDecoder[WithConst] =
          KindlingsYamlDecoder.derived[WithConst]
        val v = WithConst(Const("hello"))
        val result = roundTrip(v)
        assert(result.isRight, s"Expected Right but got $result")
      }
    }
  }
}
