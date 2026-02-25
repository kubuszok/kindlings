package hearth.kindlings.circederivation

import hearth.MacroSuite

final class RoundTripJvmSpec extends MacroSuite {

  group("RoundTrip (JVM-only)") {

    group("Java enum roundtrip") {

      test("Java enum roundtrip with enumAsStrings") {
        implicit val config: Configuration = Configuration(enumAsStrings = true)
        val value: JavaColor = JavaColor.BLUE
        val json = KindlingsEncoder.encode[JavaColor](value)
        KindlingsDecoder.decode[JavaColor](json) ==> Right(value)
      }
    }
  }
}
