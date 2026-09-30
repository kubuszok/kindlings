package hearth.kindlings.integrationtests

import hearth.MacroSuite
import hearth.kindlings.yamlderivation.{KindlingsYamlDecoder, KindlingsYamlEncoder}

final class NewtypeYamlSpec extends MacroSuite {

  group("scala-newtype + YAML") {

    test("encode then decode preserves value") {
      val node = KindlingsYamlEncoder.encode(NewtypeUser.example)
      KindlingsYamlDecoder.decode[NewtypeUser](node) ==> Right(NewtypeUser.example)
    }
  }
}
