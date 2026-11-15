package hearth.kindlings.oracle.jsoniter

import com.github.plokhotnyuk.jsoniter_scala.core.{
  readFromString,
  writeToString,
  JsonReaderException,
  JsonValueCodec,
  ReaderConfig
}

/** The same type's codecs from both sides: Kindlings with a config known at compile time (`optimized`, the codegen
  * without runtime config checks), Kindlings with the same config only known at runtime (`runtime`, the generic
  * codegen) and jsoniter-scala's `JsonCodecMaker.make` with the equivalent `CodecMakerConfig` (`upstream`).
  */
final case class JoCodecs[A](optimized: JsonValueCodec[A], runtime: JsonValueCodec[A], upstream: JsonValueCodec[A])

/** Observable behaviour of jsoniter codecs, rendered as strings to compare: the JSON a value encodes to (or the error
  * encoding throws), whether a JSON input is accepted (and as what) or rejected (and with which message), and the
  * codec's `nullValue` (what user-written codecs see for absent fields).
  */
object JsoniterOracle {

  // Error messages without the hex dump (the offset stays: it says where the error was found).
  private val readerConfig: ReaderConfig = ReaderConfig.withAppendHexDumpToParseException(false)

  private def shorten(s: String): String = if (s.length > 120) s"${s.take(60)}...(${s.length} chars)" else s

  private def render(value: Any): String = shorten(value match {
    case null        => "null"
    case a: Array[?] => a.map(render).mkString("Array(", ", ", ")")
    case s: String   => s"\"$s\""
    case other       => other.toString
  })

  def encode[A](codec: JsonValueCodec[A], value: A): String =
    try writeToString(value)(codec)
    catch { case e: Throwable => s"<threw ${e.getClass.getName}: ${e.getMessage}>" }

  def decode[A](codec: JsonValueCodec[A], json: String): String =
    try s"accepted ${render(readFromString(json, readerConfig)(codec))}"
    catch {
      case e: JsonReaderException => s"rejected: ${e.getMessage}"
      case e: Throwable           => s"<threw ${e.getClass.getName}: ${e.getMessage}>"
    }

  /** Every observed difference, as `<what>: kindlings=<x> upstream=<y>` (or `optimized=<x> runtime=<y> upstream=<z>`
    * when Kindlings' two codegens disagree too). The JSON upstream encodes each value to is also decoded by all codecs.
    */
  def differences[A](codecs: JoCodecs[A], values: Seq[A], inputs: Seq[String]): List[String] = {
    def compare(what: String, optimized: String, runtime: String, upstream: String): List[String] =
      if (optimized == upstream && runtime == upstream) Nil
      else if (optimized == runtime) List(s"$what: kindlings=$optimized upstream=$upstream")
      else List(s"$what: optimized=$optimized runtime=$runtime upstream=$upstream")

    val encoded = values.toList.flatMap { value =>
      compare(
        s"encode ${render(value)}",
        encode(codecs.optimized, value),
        encode(codecs.runtime, value),
        encode(codecs.upstream, value)
      )
    }
    val upstreamJsons = values.map(encode(codecs.upstream, _)).filterNot(_.startsWith("<threw"))
    val decoded = (inputs ++ upstreamJsons).distinct.toList.flatMap { json =>
      val label = shorten(json)
      compare(
        s"decode $label",
        decode(codecs.optimized, json),
        decode(codecs.runtime, json),
        decode(codecs.upstream, json)
      )
    }
    def nullValue(codec: JsonValueCodec[A]): String =
      try render(codec.nullValue)
      catch { case e: Throwable => s"<threw ${e.getClass.getName}: ${e.getMessage}>" }
    val nullValues =
      compare("nullValue", nullValue(codecs.optimized), nullValue(codecs.runtime), nullValue(codecs.upstream))
    encoded ++ decoded ++ nullValues
  }
}
