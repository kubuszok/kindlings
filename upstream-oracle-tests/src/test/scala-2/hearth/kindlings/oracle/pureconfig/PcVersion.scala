package hearth.kindlings.oracle.pureconfig

/** Behaviours of pureconfig's derivation that differ between its Scala 2 and Scala 3 implementations. */
object PcVersion {

  /** pureconfig-generic (Shapeless) reads a tuple from a list and, when the value is not a list but is an object, from
    * the tuple's product representation (`{ _1 = ..., _2 = ... }`).
    */
  val upstreamReadsTuplesFromObjects: Boolean = true
}
