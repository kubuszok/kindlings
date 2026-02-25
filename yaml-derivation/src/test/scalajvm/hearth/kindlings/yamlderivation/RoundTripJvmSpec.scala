package hearth.kindlings.yamlderivation

import hearth.MacroSuite

final class RoundTripJvmSpec extends MacroSuite {

  group("RoundTrip (JVM-only)") {

    group("Java enum roundtrip") {

      test("Java enum roundtrip with enumAsStrings") {
        implicit val config: YamlConfig = YamlConfig(enumAsStrings = true)
        val value: JavaColor = JavaColor.BLUE
        val node = KindlingsYamlEncoder.encode[JavaColor](value)
        KindlingsYamlDecoder.decode[JavaColor](node) ==> Right(value)
      }
    }
  }
}
