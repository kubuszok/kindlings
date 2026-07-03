package hearth.kindlings.optics

import hearth.MacroSuite

/** Smoke test for the opt-in generation logging: importing `hearth.kindlings.optics.debug.*` puts the `LogGeneration`
  * marker in scope, which makes the `modify` macro emit the parsed path + generated code as a compiler-info message.
  * The message never fails compilation, so exercising the macro with the import in scope both proves the logging path
  * compiles and that the generated code is still correct.
  */
final class GenerationLoggingSpec extends MacroSuite {

  import hearth.kindlings.optics.syntax.*
  import hearth.kindlings.optics.debug.* // enables the generation log for macros in this scope

  group("modify with generation logging enabled") {

    test("still generates correct code for a nested field modify") {
      val p = GenerationLoggingSpec.Person("a", GenerationLoggingSpec.Address("OldCity"))
      p.modify(_.address.city).setTo("NewCity") ==>
        GenerationLoggingSpec.Person("a", GenerationLoggingSpec.Address("NewCity"))
    }
  }
}

object GenerationLoggingSpec {
  final case class Address(city: String)
  final case class Person(name: String, address: Address)
}
