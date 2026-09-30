package hearth.kindlings.parser
package internal.runtime

/** Thrown by the generated recursive-descent parser when it gives up (see `Machine`'s descent API): no stack trace, one
  * instance.
  */
private[parser] object DescentBail extends scala.util.control.ControlThrowable
