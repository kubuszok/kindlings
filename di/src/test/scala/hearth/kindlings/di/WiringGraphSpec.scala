package hearth.kindlings.di

import hearth.MacroSuite
import hearth.kindlings.macros.compiletime.{NodeKind, Storage, WiringGraph}

/** Pure unit tests for the ZIO-Magic-style wiring-graph renderers (no macro expansion). Exercises a diamond graph
  * (`App` depends on `Service` and `Handler`, both of which depend on the shared `Db`) to prove the tree view is
  * DAG-aware: the shared `Db` is expanded once and later referenced, not duplicated.
  */
final class WiringGraphSpec extends MacroSuite {

  private val diamond: Map[String, WiringGraph.RawNode] = Map(
    "App" -> WiringGraph.RawNode("App", "App", NodeKind.Constructed, Storage.Val, List("Service", "Handler")),
    "Service" -> WiringGraph.RawNode("Service", "Service", NodeKind.Constructed, Storage.Val, List("Db")),
    "Handler" -> WiringGraph.RawNode("Handler", "Handler", NodeKind.Constructed, Storage.LazyVal, List("Db")),
    "Db" -> WiringGraph.RawNode("Db", "Db", NodeKind.Constructed, Storage.Val, Nil)
  )

  group("fromResolved") {

    test("assembles a DAG-aware tree, referencing a shared node instead of duplicating it") {
      val root = WiringGraph.fromResolved("App", diamond).get
      root.tpe ==> "App"
      root.deps.map(_.tpe) ==> List("Service", "Handler")
      // Db is expanded under the first consumer (Service)...
      val service = root.deps.head
      service.deps.map(_.tpe) ==> List("Db")
      service.deps.head.kind ==> NodeKind.Constructed
      // ...and referenced (not expanded) under the second consumer (Handler).
      val handler = root.deps(1)
      handler.deps.map(_.tpe) ==> List("Db")
      handler.deps.head.kind ==> NodeKind.Reference
      handler.deps.head.deps ==> Nil
    }

    test("returns None for an unknown root") {
      WiringGraph.fromResolved("Missing", diamond) ==> None
    }
  }

  group("renderTree") {

    test("renders every node and marks the shared node once") {
      val tree = WiringGraph.renderTree(WiringGraph.fromResolved("App", diamond).get)
      assert(tree.contains("App"))
      assert(tree.contains("Service"))
      assert(tree.contains("Handler"))
      assert(tree.contains("Db"))
      // exactly one "shared" marker (the second, referenced Db)
      "↻ \\(shared".r.findAllIn(tree).size ==> 1
    }
  }

  group("renderMermaid") {

    test("emits a graph definition with deduped nodes, all edges, and a render link") {
      val mermaid = WiringGraph.renderMermaid(WiringGraph.fromResolved("App", diamond).get)
      assert(mermaid.contains("graph TD"))
      assert(mermaid.contains("App --> Service"))
      assert(mermaid.contains("App --> Handler"))
      assert(mermaid.contains("Service --> Db"))
      assert(mermaid.contains("Handler --> Db"))
      assert(mermaid.contains("mermaid.ink"))
      // Db is declared exactly once even though two edges point at it.
      "Db\\[Db\\]".r.findAllIn(mermaid).size ==> 1
    }
  }
}
