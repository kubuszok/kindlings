package hearth.kindlings.macros.compiletime

/** Shared, opt-in "print the generated code and the logic that led to it" capability for the
  * direct-style (non-MIO) macro modules — `optics`, `mock`, `di`.
  *
  * The derivation modules (fast-show-pretty, cats, circe, ...) already offer this through the
  * `LogDerivation` marker, but that rides on Hearth's MIO `Log` engine (`Log.namedScope`/`Log.info`
  * rendered by `runToExprOrFail`). The modules mixing this trait in are plain direct-style macros
  * (they call `Environment.reportErrorAndAbort` directly, never MIO), so they cannot reuse that
  * machinery. This trait reproduces the same *user experience* — enable by importing the module's
  * `debug` implicit OR by setting `-Xmacro-settings:<namespace>.logGeneration=true` — with a tiny
  * direct-style trace accumulator instead.
  *
  * Mixed into a per-platform macro bundle (via the module's `...MacrosImpl` trait). Each module
  * supplies its settings [[generationLoggingNamespace]] (e.g. `"optics"`) and whether its own
  * `LogGeneration` marker is [[generationMarkerImported]]; everything else is shared here.
  */
private[kindlings] trait GenerationLogging { this: hearth.MacroCommons =>

  /** Per-module settings namespace for `-Xmacro-settings:<ns>.logGeneration=true`, e.g.
    * `"optics"` | `"mock"` | `"di"`.
    */
  protected def generationLoggingNamespace: String

  /** Per-module: is the module's opt-in `LogGeneration` marker implicit currently in scope? Each
    * module implements this with `Expr.summonImplicit[<its LogGeneration>].isDefined`.
    */
  protected def generationMarkerImported: Boolean

  /** A mutable, single-expansion, indented trace of the decisions a macro made while generating
    * code. Built regardless of whether logging is on (the calls are cheap string appends); it is
    * only rendered and reported by [[emitGenerationLog]] when logging is enabled.
    */
  protected final class Trace {
    private val sb = new StringBuilder
    private var depth = 0

    /** Record one decision/step at the current indentation. */
    def step(message: => String): Unit = {
      appendLines(message)
    }

    /** Record `name` as a step, then indent every step emitted inside `body` one level deeper. */
    def scope[A](name: => String)(body: => A): A = {
      step(name)
      depth += 1
      try body
      finally depth -= 1
    }

    def render: String = if (sb.isEmpty) "(no steps recorded)" else sb.result()

    private def appendLines(message: String): Unit = {
      val indent = "  " * depth
      // keep multi-line messages (e.g. a small rendered sub-tree) aligned under the current depth
      message.linesIterator.foreach(line => sb.append(indent).append(line).append('\n'))
    }
  }

  /** True when generation logging is enabled — either the module's `LogGeneration` marker is
    * imported, or `-Xmacro-settings:<namespace>.logGeneration=true` was passed.
    */
  protected lazy val shouldWeLogGeneration: Boolean =
    generationMarkerImported || logGenerationSetGlobally

  private def logGenerationSetGlobally: Boolean =
    (for {
      data <- Environment.typedSettings.toOption
      moduleSettings <- data.get(generationLoggingNamespace)
      flag <- moduleSettings.get("logGeneration").flatMap(_.asBoolean)
    } yield flag).getOrElse(false)

  /** Emit the accumulated `trace` and the `generated` code as a single compiler-info message —
    * but only when [[shouldWeLogGeneration]] is true. `generated` is by-name so the (potentially
    * expensive) `expr.prettyPrint` is never computed when logging is off.
    */
  protected def emitGenerationLog(macroName: String, trace: Trace, generated: => String): Unit =
    if (shouldWeLogGeneration) {
      Environment.reportInfo(
        s"""$macroName — generation log
           |${trace.render}
           |Generated code:
           |$generated
           |
           |(enable/disable with: import hearth.kindlings.$generationLoggingNamespace.debug.* or scalac option -Xmacro-settings:$generationLoggingNamespace.logGeneration=true)""".stripMargin
      )
    }
}
