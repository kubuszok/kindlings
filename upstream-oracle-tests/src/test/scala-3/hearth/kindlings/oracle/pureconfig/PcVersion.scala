package hearth.kindlings.oracle.pureconfig

/** Behaviours of pureconfig's derivation that differ between its Scala 2 and Scala 3 implementations. */
object PcVersion {

  /** pureconfig-generic-scala3 (like the built-in `derives ConfigReader`) reads a tuple only from a list.
    */
  val upstreamReadsTuplesFromObjects: Boolean = false
}
