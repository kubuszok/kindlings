package hearth.kindlings.optics

package object debug {

  /** Import into the scope of a `modify`/`modifyAll`/`modifyLens` call to preview how the optics macro rewrites your
    * path — the parsed steps and the generated code. Placed outside the `optics` package object so the implicit is
    * never summoned automatically.
    */
  implicit val logGenerationForOptics: LogGeneration = LogGeneration
}
