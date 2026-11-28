package hearth.kindlings.mock

/** ScalaMock's `MockTest` "mock an embedded trait": a member whose type is an inner trait of the mocked trait (`def
  * referencesEmbedded(): Embedded`): not supported on Scala 2 nor 3. Only here: on Scala 2 the error comes from the
  * `refchecks` phase, which `compileErrors` does not run.
  */
final class EmbeddedTraitMockSpec extends hearth.MacroSuite {

  test("mock an embedded trait (path-dependent types): not supported on Scala 3, documented limitation") {
    // Hearth's `AnonymousInstance` declares the override on Scala 3 with the type projection `WithEmbedded#Embedded`
    // instead of `this.Embedded`, which does not override the member
    // (docs/research/hearth-gap-anonymous-instance-inner-types.md; the same on Scala 2). When this starts
    // compiling, add ScalaMock's test to the shared ScalaMockParitySpec.
    val errors: String = compileErrors("""
      implicit val ctx: MockContext = new MockContext
      Mock.mock[ScalaMockParitySpec.WithEmbedded]
    """)
    assert(errors.contains("referencesEmbedded"), errors)
  }
}
