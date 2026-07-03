package hearth.kindlings.di

package object debug {

  /** Import into the scope of a wiring call (`DI.wire`/`wireRec`/`autowire`/`DI.plan(...).build`)
    * to preview how the graph is assembled — the resolution logic and the generated code. Placed
    * outside the `DI` companion so the implicit is never summoned automatically.
    *
    * To see only the wiring *graph* (ZIO-Magic style) rather than the full generation log, use the
    * `di.logWiring=tree|mermaid` scalac option or a `DI.plan(...).debug(...)` call instead.
    */
  implicit val logGenerationForDI: DI.LogGeneration = DI.LogGeneration
}
