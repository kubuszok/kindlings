package hearth.kindlings.parser
package internal.runtime

/** Thrown by generated code when a value is rejected after it was parsed (a collection's smart constructor rejected the
  * repeated values); the machine turns it into a [[ParseError]] reported through the engine's error channel.
  */
final class RejectedValue(message: String) extends RuntimeException(message, null, false, false)
object RejectedValue {

  /** A rejection of `what` with the smart constructor's `error` (a `String`, a `Throwable` or an `Iterable` of them).
    */
  def apply(what: String, error: Any): RejectedValue = new RejectedValue(s"Invalid $what: ${describe(error)}")

  private def describe(error: Any): String = error match {
    case t: Throwable    => String.valueOf(t.getMessage)
    case it: Iterable[?] => it.map(describe).mkString("; ")
    case other           => String.valueOf(other)
  }
}
