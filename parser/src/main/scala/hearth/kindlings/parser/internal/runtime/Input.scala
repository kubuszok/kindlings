package hearth.kindlings.parser
package internal.runtime

/** Where the machine reads characters from. Positions are absolute `Long` offsets (inputs may exceed 2^31 chars).
  *
  * The machine never performs I/O itself: when [[ensure]] answers [[Input.NeedMore]] it stops with `Machine.NeedInput`
  * and the engine calls [[refill]] - directly, or wrapped in its effect (e.g. `Sync.blocking`).
  */
abstract private[parser] class Input {

  /** Whether the char at `pos` is available: [[Input.Available]], [[Input.NeedMore]] or [[Input.End]]. */
  def ensure(pos: Long): Int

  /** The char at `pos`; only valid after [[ensure]] answered [[Input.Available]]. */
  def charAt(pos: Long): Char

  /** A copy of the text in `[start, end)`; only valid for positions not yet released. */
  def slice(start: Long, end: Long): String

  /** Nothing before `pos` will be read again: buffered inputs may discard it. */
  def release(pos: Long): Unit

  /** Reads more input (may block). */
  def refill(): Unit

  /** 1-based line and column of `pos` (only used to report errors). */
  def lineColumn(pos: Long): (Int, Int)
}
private[parser] object Input {
  final val Available = 1
  final val NeedMore = 0
  final val End = -1
}

/** A `String` input: chars are read in place (no copy of the input) and never released. */
final private[parser] class StringInput(text: String) extends Input {

  private val length = text.length.toLong

  def ensure(pos: Long): Int = if (pos < length) Input.Available else Input.End
  def charAt(pos: Long): Char = text.charAt(pos.toInt)
  def slice(start: Long, end: Long): String = text.substring(start.toInt, end.toInt)
  def release(pos: Long): Unit = ()
  def refill(): Unit = ()

  def lineColumn(pos: Long): (Int, Int) = {
    var line = 1
    var lineStart = 0
    var i = 0
    val p = math.min(pos, length).toInt
    while (i < p) {
      if (text.charAt(i) == '\n') { line += 1; lineStart = i + 1 }
      i += 1
    }
    (line, p - lineStart + 1)
  }
}

/** A `java.io.Reader` input read in chunks into a buffer. Text before the last released position is discarded when the
  * buffer is refilled, so memory stays bounded by the buffer size plus the longest token, whatever the input size.
  */
final private[parser] class ReaderInput(reader: java.io.Reader, initialBufferSize: Int) extends Input {

  private var buffer = new Array[Char](math.max(initialBufferSize, 16))
  private var start = 0L // absolute position of buffer(0)
  private var limit = 0 // valid chars in buffer
  private var ended = false
  private var released = 0L

  // line tracking of discarded text
  private var discardedLines = 0
  private var lastLineStart = 0L // absolute position of the start of the last line seen in discarded text

  /** Current buffer size in chars (for tests). */
  def capacity: Int = buffer.length

  def ensure(pos: Long): Int =
    if (pos - start < limit) Input.Available
    else if (ended) Input.End
    else Input.NeedMore

  def charAt(pos: Long): Char = buffer((pos - start).toInt)

  def slice(from: Long, until: Long): String = new String(buffer, (from - start).toInt, (until - from).toInt)

  def release(pos: Long): Unit = if (pos > released) released = pos

  def refill(): Unit = {
    val drop = (released - start).toInt
    if (drop > 0) {
      var i = 0
      while (i < drop) {
        if (buffer(i) == '\n') { discardedLines += 1; lastLineStart = start + i + 1 }
        i += 1
      }
      System.arraycopy(buffer, drop, buffer, 0, limit - drop)
      limit -= drop
      start += drop
    }
    if (limit == buffer.length) buffer = java.util.Arrays.copyOf(buffer, buffer.length * 2)
    val read = reader.read(buffer, limit, buffer.length - limit)
    if (read < 0) ended = true else limit += read
  }

  def lineColumn(pos: Long): (Int, Int) = {
    var line = discardedLines + 1
    var lineStart = lastLineStart
    var p = start
    val until = math.min(pos, start + limit)
    while (p < until) {
      if (buffer((p - start).toInt) == '\n') { line += 1; lineStart = p + 1 }
      p += 1
    }
    (line, (pos - lineStart + 1).toInt)
  }
}

/** A chunk-fed input for push-based drivers (streams, REPLs): [[feed]] appends text, [[endOfInput]] marks the end.
  * [[refill]] cannot read anything by itself: when the machine answers `NeedInput`, the driver must feed more (or end
  * the input). Released text is discarded when new chunks arrive, so memory stays bounded as with [[ReaderInput]].
  */
final private[parser] class PushInput(initialBufferSize: Int) extends Input {

  private var buffer = new Array[Char](math.max(initialBufferSize, 16))
  private var start = 0L
  private var limit = 0
  private var ended = false
  private var released = 0L
  private var discardedLines = 0
  private var lastLineStart = 0L

  def ensure(pos: Long): Int =
    if (pos - start < limit) Input.Available
    else if (ended) Input.End
    else Input.NeedMore

  def charAt(pos: Long): Char = buffer((pos - start).toInt)

  def slice(from: Long, until: Long): String = new String(buffer, (from - start).toInt, (until - from).toInt)

  def release(pos: Long): Unit = if (pos > released) released = pos

  def refill(): Unit = ()

  def feed(chars: String): Unit = {
    if (ended) throw new IllegalStateException("input already ended")
    compact()
    val needed = limit + chars.length
    if (needed > buffer.length) {
      var size = buffer.length
      while (size < needed) size *= 2
      buffer = java.util.Arrays.copyOf(buffer, size)
    }
    chars.getChars(0, chars.length, buffer, limit)
    limit += chars.length
  }

  def endOfInput(): Unit = ended = true

  private def compact(): Unit = {
    val drop = (released - start).toInt
    if (drop > 0) {
      var i = 0
      while (i < drop) {
        if (buffer(i) == '\n') { discardedLines += 1; lastLineStart = start + i + 1 }
        i += 1
      }
      System.arraycopy(buffer, drop, buffer, 0, limit - drop)
      limit -= drop
      start += drop
    }
  }

  def lineColumn(pos: Long): (Int, Int) = {
    var line = discardedLines + 1
    var lineStart = lastLineStart
    var p = start
    val until = math.min(pos, start + limit)
    while (p < until) {
      if (buffer((p - start).toInt) == '\n') { line += 1; lineStart = p + 1 }
      p += 1
    }
    (line, (pos - lineStart + 1).toInt)
  }
}
