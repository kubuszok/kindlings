package hearth.kindlings.oracle

import sttp.tapir.Schema

/** Recursive schemas declared the way tapir's docs say for Scala 2: an `implicit lazy val` (a `def` loops forever). */
object TapirUpstreamRecursive {
  implicit lazy val treeSchema: Schema[OTree] = Schema.derived[OTree]
}
