package hearth.kindlings.catstaglessderivation

import hearth.MacroSuite

/** Shapes from cats-tagless' own test suites that cats-tagless derives but Kindlings cannot yet, because of Hearth gaps
  * (`docs/research/hearth-gap-cats-tagless-trait-algebras.md`, listed as Gap in `docs/user-guide/feature-parity.md`).
  * Each test asserts today's compile error: when Hearth fixes the gap the test fails, so the shape gets a real test and
  * the docs get updated.
  */
final class KnownGapsSpec extends MacroSuite {

  private def assertGap(errors: String): Unit =
    assert(errors.nonEmpty, "derivation now compiles: move the case to UpstreamParitySpec and update feature-parity.md")

  test("refined algebras with type members (autoFunctorKTests.AlgWithTypeMember) - Hearth gap 4") {
    assertGap(compileErrors("KindlingsFunctorK.derived[UpstreamAlgebras.AlgWithTypeMemberString]"))
    assertGap(compileErrors("KindlingsInvariantK.derived[UpstreamAlgebras.AlgWithTypeBoundB]"))
    assertGap(compileErrors("KindlingsFunctorK.derived[UpstreamAlgebras.AlgWithAbstractTypeClassShow]"))
  }

  test("dependent method types (autoApplyKTests.AlgWithDependentType) - Hearth gap 3") {
    assertGap(compileErrors("KindlingsApplyK.derived[UpstreamAlgebras.AlgWithDependentType]"))
  }

  test("an alias of an algebra with extra type parameters (autoFunctorKTests.AlgWithExtraTP) - Hearth gap 5") {
    assertGap(compileErrors("KindlingsFunctorK.derived[UpstreamAlgebras.AlgWithExtraTPString]"))
  }

  test(
    "by-name parameter of a method type parameter (autoSemigroupalKTests.AlgWithConstantReturnTypes) - Hearth gap 2".ignoreOnScala2_13
  ) {
    // Derived on Scala 2.13 (checked below), not on Scala 3.
    assertGap(compileErrors("KindlingsSemigroupalK.derived[UpstreamAlgebras.AlgWithConstantReturnTypes]"))
  }

  test("by-name parameter of a method type parameter is derived on Scala 2.13".ignoreOnScala3) {
    assertEquals(compileErrors("KindlingsSemigroupalK.derived[UpstreamAlgebras.AlgWithConstantReturnTypes]"), "")
  }
}
