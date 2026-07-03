package hearth.kindlings.optics

/** Special marker type — if its implicit is in scope, the optics macros (`modify`/`modifyAll`/
  * `modifyLens`) log the generated code and the logic (parsed path steps) that produced it.
  *
  * Enable by importing `hearth.kindlings.optics.debug.*`, or globally with the scalac option
  * `-Xmacro-settings:optics.logGeneration=true`. Kept out of the `optics` package object so it is
  * never summoned automatically.
  */
sealed trait LogGeneration
object LogGeneration extends LogGeneration
