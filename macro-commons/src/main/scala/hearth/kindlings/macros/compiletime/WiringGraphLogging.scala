package hearth.kindlings.macros.compiletime

/** Shared, opt-in "show how things are wired together" capability for the DI-style macro modules (`di`, `di-cats`),
  * modelled on ZIO Magic / ZIO 2.0's automatic layer wiring (`ZLayer.Debug.tree` / `ZLayer.Debug.mermaid`).
  *
  * A module collects a flat map of [[WiringGraph.RawNode]]s during resolution, then this trait renders it — either as a
  * DAG-aware ASCII dependency tree or a Mermaid diagram — independently of the full generation log, so a user can see
  * *only* how a single wiring run combines its entities/resources. Enable per-module with
  * `-Xmacro-settings:<namespace>.logWiring=tree|mermaid` (or `=true`, meaning `tree`).
  */
private[kindlings] trait WiringGraphLogging { this: hearth.MacroCommons =>

  /** Per-module settings namespace for `-Xmacro-settings:<ns>.logWiring=...`, e.g. `"di"` | `"diCats"`. */
  protected def wiringLoggingNamespace: String

  sealed protected trait WiringLogMode
  protected object WiringLogMode {
    case object Tree extends WiringLogMode
    case object Mermaid extends WiringLogMode
  }

  /** The wiring-graph mode requested via `-Xmacro-settings:<ns>.logWiring=tree|mermaid` (or `=true` → tree). */
  protected def wiringLogModeFromSettings: Option[WiringLogMode] =
    (for {
      data <- Environment.typedSettings.toOption
      ns <- data.get(wiringLoggingNamespace)
      v <- ns.get("logWiring")
    } yield v).flatMap { v =>
      v.asString
        .map(_.trim.toLowerCase)
        .collect {
          case "mermaid" => WiringLogMode.Mermaid
          case "tree"    => WiringLogMode.Tree
        }
        .orElse(v.asBoolean.collect { case true => WiringLogMode.Tree })
    }

  private def renderWiring(root: WiringNode, mode: WiringLogMode): String = mode match {
    case WiringLogMode.Tree    => WiringGraph.renderTree(root)
    case WiringLogMode.Mermaid => WiringGraph.renderMermaid(root)
  }

  /** Emit a standalone wiring graph (only when `mode` is defined). Roots the tree at `rootKey`, forcing that node's
    * kind to [[NodeKind.Root]] so the root prints without a storage annotation.
    */
  protected def emitWiringGraphIfEnabled(
      endpoint: String,
      rootKey: String,
      nodes: scala.collection.Map[String, WiringGraph.RawNode],
      mode: Option[WiringLogMode]
  ): Unit = mode.foreach { m =>
    WiringGraph.fromResolved(rootKey, nodes).foreach { root =>
      Environment.reportInfo(s"$endpoint — wiring graph\n${renderWiring(root.copy(kind = NodeKind.Root), m)}")
    }
  }

  /** The wiring graph rendered as an ASCII tree, for embedding inside a full generation log (when both are on). */
  protected def wiringTreeFor(
      rootKey: String,
      nodes: scala.collection.Map[String, WiringGraph.RawNode]
  ): String =
    WiringGraph
      .fromResolved(rootKey, nodes)
      .map(root => WiringGraph.renderTree(root.copy(kind = NodeKind.Root)))
      .getOrElse("")
}
