package hearth.kindlings.dicats

package object debug {

  /** Import into the scope of a `wireResource` call to preview how the `Resource[F, _]` graph is assembled — the
    * resolution logic and the generated code. Placed outside the `DICats` companion so the implicit is never summoned
    * automatically.
    *
    * To see only the wiring *graph* (ZIO-Magic style) rather than the full generation log, use the
    * `diCats.logWiring=tree|mermaid` scalac option instead.
    */
  implicit val logGenerationForDICats: DICats.LogGeneration = DICats.LogGeneration
}
