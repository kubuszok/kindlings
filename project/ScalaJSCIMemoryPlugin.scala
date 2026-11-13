import sbt.*
import sbt.Keys.*
import org.scalajs.sbtplugin.ScalaJSPlugin
import org.scalajs.sbtplugin.ScalaJSPlugin.autoImport.*

/** Keeps Scala.js linking within CI memory limits.
  *
  * By default the Scala.js linker retains its intermediate state in memory for incremental re-linking, and in a build
  * with many Scala.js projects that state accumulates over one sbt session until linking fails with an
  * `OutOfMemoryError`. On CI every project is linked once, so we:
  *
  *   - turn on the linker's batch mode, which releases that state after each link (see
  *     https://github.com/scala-js/scala-js/issues/3268),
  *   - link one Scala.js project at a time, to bound the peak memory used by concurrent linkers.
  *
  * Local builds keep incremental linking (faster re-linking while iterating).
  */
object ScalaJSCIMemoryPlugin extends AutoPlugin {

  private val isCI: Boolean = sys.env.get("CI").exists(_.nonEmpty)

  override def requires = ScalaJSPlugin
  override def trigger = allRequirements

  override def globalSettings: Seq[Def.Setting[?]] =
    if (isCI) Seq(Global / concurrentRestrictions += Tags.limit(ScalaJSTags.Link, 1)) else Nil

  override def projectSettings: Seq[Def.Setting[?]] =
    if (isCI) Seq(scalaJSLinkerConfig ~= (_.withBatchMode(true))) else Nil
}
