package hearth.kindlings.oracle.optics

/** Behaviours of quicklens that differ between its Scala 2 and Scala 3 implementations. */
object QuicklensVersion {

  /** quicklens for Scala 2 checks `.index(i)` with `i < size` only, so a negative index throws
    * `IndexOutOfBoundsException`; for Scala 3 it checks `isDefinedAt(i)` and a negative index is a no-op.
    */
  val negativeIndexThrows: Boolean = true
}
