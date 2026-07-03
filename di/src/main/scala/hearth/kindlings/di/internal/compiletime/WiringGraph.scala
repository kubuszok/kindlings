package hearth.kindlings.di
package internal.compiletime

/** A rendered view of how a wiring builds its object graph, inspired by ZIO Magic / ZIO 2.0's
  * automatic layer wiring (`ZLayer.Debug.tree` and `ZLayer.Debug.mermaid`).
  *
  * The macros populate a flat map of [[WiringGraph.RawNode]]s during resolution (keyed by a stable
  * per-type key), then [[WiringGraph.fromResolved]] assembles a DAG-aware display tree: each shared
  * dependency is drawn once and later reuses become a [[NodeKind.Reference]] leaf (so a diamond
  * dependency is not duplicated). [[WiringGraph.renderTree]] prints that as an ASCII tree;
  * [[WiringGraph.renderMermaid]] prints a Mermaid `graph` you can render as a real diagram.
  *
  * This is compile-time-only code (executed by the macro on the JVM), but it lives in shared source
  * that also links for Scala.js / Scala Native, so it deliberately avoids `java.util.*` (no
  * `Base64`/`Deflater`, which the JS/Native javalibs do not provide) — the base64 used for the
  * Mermaid link is a tiny pure-Scala implementation.
  */
private[di] sealed trait NodeKind
private[di] object NodeKind {

  /** The root type the wiring was asked to build. */
  case object Root extends NodeKind

  /** Built by invoking a public constructor / companion `apply`. */
  case object Constructed extends NodeKind

  /** Taken from a value found in the enclosing lexical scope (`wire`/`wireRec`). */
  case object FromScope extends NodeKind

  /** Supplied explicitly — an `autowire` dependency, or a `DI.plan(...).provide[T](...)` override.
    * `label` describes the source (e.g. `"instance"`, `"factory"`, `"provided"`).
    */
  final case class Provided(label: String) extends NodeKind

  /** Resolved by implicit search. */
  case object Summoned extends NodeKind

  /** A node already wired earlier in the graph — drawn as a leaf to keep the tree a DAG view. */
  case object Reference extends NodeKind
}

/** How a constructed value is stored in the generated code. */
private[di] sealed abstract class Storage(val label: String)
private[di] object Storage {
  case object Inline extends Storage("inline")
  case object Val extends Storage("val")
  case object LazyVal extends Storage("lazy val")
  case object Def extends Storage("def")
}

/** One node of the assembled display tree. */
private[di] final case class WiringNode(
    tpe: String,
    kind: NodeKind,
    storage: Storage,
    deps: List[WiringNode]
)

private[di] object WiringGraph {

  /** A node as collected during resolution, before the DAG-aware tree is assembled. `key` is a
    * stable identity (a type FQCN); `depKeys` reference other collected nodes.
    */
  final case class RawNode(key: String, tpe: String, kind: NodeKind, storage: Storage, depKeys: List[String])

  /** Assemble the display tree rooted at `rootKey`. The first time a key is reached it is expanded
    * with its dependencies; any later reuse becomes a [[NodeKind.Reference]] leaf so shared
    * dependencies (diamonds) are drawn exactly once — ZIO `Debug.tree` semantics.
    */
  def fromResolved(rootKey: String, nodes: scala.collection.Map[String, RawNode]): Option[WiringNode] = {
    val seen = scala.collection.mutable.Set.empty[String]
    def build(key: String): Option[WiringNode] = nodes.get(key).map { raw =>
      if (seen(key)) WiringNode(raw.tpe, NodeKind.Reference, raw.storage, Nil)
      else {
        seen += key
        WiringNode(raw.tpe, raw.kind, raw.storage, raw.depKeys.flatMap(build))
      }
    }
    build(rootKey)
  }

  // ----------------------------------------------------------------------------------------------
  // ASCII tree (ZLayer.Debug.tree analog)
  // ----------------------------------------------------------------------------------------------

  def renderTree(root: WiringNode): String = {
    val sb = new StringBuilder
    val _ = sb.append(shortName(root.tpe)).append(annotation(root)).append('\n')
    renderChildren(root.deps, "", sb)
    sb.result()
  }

  private def renderChildren(deps: List[WiringNode], prefix: String, sb: StringBuilder): Unit =
    deps.zipWithIndex.foreach { case (node, i) =>
      val last = i == deps.size - 1
      val _ = sb
        .append(prefix)
        .append(if (last) "╰─ " else "├─ ")
        .append(shortName(node.tpe))
        .append(annotation(node))
        .append('\n')
      renderChildren(node.deps, prefix + (if (last) "   " else "│  "), sb)
    }

  private def annotation(node: WiringNode): String = node.kind match {
    case NodeKind.Root          => ""
    case NodeKind.Reference     => "  ↻ (shared, wired above)"
    case NodeKind.Provided(lbl) => s"  ⇐ provided ($lbl)"
    case NodeKind.FromScope     => "  ⇐ from scope"
    case NodeKind.Summoned      => "  ⇐ implicit"
    case NodeKind.Constructed   => if (node.storage == Storage.Inline) "" else s"  [${node.storage.label}]"
  }

  // ----------------------------------------------------------------------------------------------
  // Mermaid (ZLayer.Debug.mermaid analog)
  // ----------------------------------------------------------------------------------------------

  def renderMermaid(root: WiringNode): String = {
    val decls = scala.collection.mutable.LinkedHashMap.empty[String, String] // id -> label line
    val edges = scala.collection.mutable.LinkedHashSet.empty[String]
    def walk(node: WiringNode): Unit = {
      val id = mermaidId(node.tpe)
      val _ = decls.getOrElseUpdate(id, s"  $id[${shortName(node.tpe)}]")
      node.deps.foreach { dep =>
        edges += s"  $id --> ${mermaidId(dep.tpe)}"
        walk(dep)
      }
    }
    walk(root)
    val source = ("graph TD" :: decls.values.toList ::: edges.toList).mkString("\n")
    val link = s"https://mermaid.ink/svg/${base64Url(source)}"
    s"""$source
       |
       |Render: $link
       |(or paste the graph above into https://mermaid.live)""".stripMargin
  }

  // ----------------------------------------------------------------------------------------------
  // helpers
  // ----------------------------------------------------------------------------------------------

  private def shortName(fqcn: String): String = {
    val noGenerics = fqcn
    val base = noGenerics.split('.').last
    base
  }

  private def mermaidId(fqcn: String): String = {
    val cleaned = fqcn.map(c => if (c.isLetterOrDigit) c else '_')
    if (cleaned.headOption.exists(_.isDigit)) s"n$cleaned" else cleaned
  }

  /** Standard base64url (no padding), pure Scala — avoids `java.util.Base64` for JS/Native linking. */
  private def base64Url(s: String): String = {
    val alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_"
    val bytes = s.getBytes("UTF-8")
    val out = new StringBuilder
    var i = 0
    while (i < bytes.length) {
      val b0 = bytes(i) & 0xff
      val b1 = if (i + 1 < bytes.length) bytes(i + 1) & 0xff else 0
      val b2 = if (i + 2 < bytes.length) bytes(i + 2) & 0xff else 0
      val triple = (b0 << 16) | (b1 << 8) | b2
      out.append(alphabet((triple >> 18) & 0x3f))
      out.append(alphabet((triple >> 12) & 0x3f))
      if (i + 1 < bytes.length) out.append(alphabet((triple >> 6) & 0x3f))
      if (i + 2 < bytes.length) out.append(alphabet(triple & 0x3f))
      i += 3
    }
    out.result()
  }
}
