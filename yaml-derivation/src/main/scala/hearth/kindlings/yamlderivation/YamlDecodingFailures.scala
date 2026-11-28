package hearth.kindlings.yamlderivation

import org.virtuslab.yaml.ConstructError

/** One failure of a decoder that accumulates errors ([[YamlConfig.withAccumulatingErrors]]).
  *
  * @param path
  *   where the failure happened, relative to the decoded value: `.field` for a case class field or a map entry,
  *   `[index]` for a collection item, e.g. `.address.zip` or `.items[2].name`
  * @param error
  *   the failure, as the decoder of the value at `path` reported it
  */
final case class YamlDecodingFailure(path: String, error: ConstructError)

/** All the failures of a decoder that accumulates errors ([[YamlConfig.withAccumulatingErrors]]).
  *
  * The decoder returns one `ConstructError` whose message lists every failure with its path; this is that error's
  * cause, so the failures can also be read one by one with [[YamlDecodingFailures.of]].
  */
final class YamlDecodingFailures(val failures: List[YamlDecodingFailure])
    extends RuntimeException(YamlDecodingFailures.render(failures), null, false, false)

object YamlDecodingFailures {

  /** The individual failures in `error`: the accumulated ones when it was returned by a decoder that accumulates
    * errors, otherwise `error` itself (with an empty path).
    */
  def of(error: ConstructError): List[YamlDecodingFailure] = error.getCause match {
    case failures: YamlDecodingFailures => failures.failures
    case _                              => List(YamlDecodingFailure("", error))
  }

  /** One `ConstructError` listing all the `failures` (see [[YamlConfig.accumulatingErrors]]). */
  def aggregate(failures: List[YamlDecodingFailure]): ConstructError = {
    val cause = new YamlDecodingFailures(failures)
    val error = ConstructError(cause.getMessage, None, None)
    val _ = error.initCause(cause)
    error
  }

  private[yamlderivation] def render(failures: List[YamlDecodingFailure]): String = {
    val count = failures.size
    val entries = failures.map { failure =>
      val error = failure.error
      val path = if (failure.path.isEmpty) "." else failure.path
      // a multi-line message (e.g. with the position's source line) stays readable under its entry
      val message = error.errorMsg.trim.replace("\n", "\n   ")
      val position = error.node.flatMap(_.pos) match {
        case Some(range) => s" (at ${range.start.line}:${range.start.column})"
        case None        => ""
      }
      s"\n - $path: $message$position"
    }
    s"Decoding failed with $count ${if (count == 1) "error" else "errors"}:${entries.mkString}"
  }
}
