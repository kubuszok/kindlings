package hearth.kindlings.mock

import scala.collection.mutable

/** A node in the expectation-ordering tree that backs [[MockContext.inSequence]] / [[MockContext.inAnyOrder]].
  *
  * The tree mirrors ScalaMock's nested ordering semantics exactly:
  *   - an **ordered** node (`inSequence`) requires its children to be fully satisfied left-to-right;
  *   - an **unordered** node (`inAnyOrder`) imposes no order on its children.
  *
  * Children are either further nodes (nested blocks) or leaf [[CallHandler]]s. A handler is eligible to fire only when,
  * for every ordered ancestor on its path to the root, all earlier-positioned siblings are already complete and no
  * later-positioned sibling has been started. This makes arbitrarily deep `inSequence`/`inAnyOrder` nesting work.
  */
private[mock] final class OrderingNode(val ordered: Boolean, val parent: OrderingNode) {

  /** Either a nested node or a leaf handler, in registration order. */
  val children: mutable.ListBuffer[Either[OrderingNode, CallHandler]] = mutable.ListBuffer.empty

  /** This node's position among its parent's children (set when attached). */
  var positionInParent: Int = 0

  def addChild(child: Either[OrderingNode, CallHandler]): Unit = {
    child match {
      case Left(node) => node.positionInParent = children.length
      case Right(h)   => h.orderingNode = this; h.orderingPos = children.length
    }
    val _ = children.append(child)
  }

  /** Has any handler under this node fired? */
  def isStarted: Boolean = children.exists {
    case Left(node) => node.isStarted
    case Right(h)   => h.callCount > 0
  }

  /** Are all children of this node complete (satisfied and unable to take further calls)? */
  def isComplete: Boolean = children.forall {
    case Left(node) => node.isComplete
    case Right(h)   => h.isSatisfied
  }
}

private[mock] object OrderingNode {

  /** Walk from a handler up to the root: at every ordered ancestor, require all earlier siblings on the path to be
    * complete and all later siblings to be untouched. Returns true iff the handler may fire now.
    */
  def eligible(handler: CallHandler): Boolean = {
    if (handler.orderingNode == null) return true

    // The child of `node` that lies on the path toward `handler`.
    var child: Either[OrderingNode, CallHandler] = Right(handler)
    var node: OrderingNode = handler.orderingNode
    while (node != null) {
      if (node.ordered) {
        val pos = child match {
          case Left(n)  => n.positionInParent
          case Right(h) => h.orderingPos
        }
        var i = 0
        val ok = node.children.forall { sibling =>
          val res =
            if (i < pos) siblingComplete(sibling)
            else if (i > pos) !siblingStarted(sibling)
            else true
          i += 1
          res
        }
        if (!ok) return false
      }
      child = Left(node)
      node = node.parent
    }
    true
  }

  private def siblingComplete(c: Either[OrderingNode, CallHandler]): Boolean = c match {
    case Left(node) => node.isComplete
    case Right(h)   => h.isSatisfied
  }

  private def siblingStarted(c: Either[OrderingNode, CallHandler]): Boolean = c match {
    case Left(node) => node.isStarted
    case Right(h)   => h.callCount > 0
  }
}
