package hearth.kindlings.mock

package object debug {

  /** Import into the scope of a `Mock.mock`/`Mock.stub` (or `expects`/`when`/`verify`) call to
    * preview how the mock is synthesized — the overridden members and the generated code. Placed
    * outside the `Mock` companion so the implicit is never summoned automatically.
    */
  implicit val logGenerationForMock: Mock.LogGeneration = Mock.LogGeneration
}
