package hearth.kindlings.oracle

import sttp.tapir.Schema

/** Recursive schemas declared the way tapir's docs say for Scala 3: an `implicit def` (a `lazy val` deadlocks). */
object TapirUpstreamRecursive {
  implicit def treeSchema: Schema[OTree] = Schema.derived[OTree]
}
