package hearth.kindlings.jsoniterderivation.internal.runtime

import com.github.plokhotnyuk.jsoniter_scala.core.{JsonReader, JsonWriter}

/** Reads and writes the values of one `scala.Enumeration`, as jsoniter-scala does: by name (`toString`) or by id, and
  * with the values the enumeration has *at runtime* (`Enumeration#values`), so values that are not public `val`s
  * (private, lazy, created by a method, added after the codec was created) work too.
  *
  * One instance per derived codec and enumeration. Nothing is read from the enumeration before the first use (a codec
  * may be created while the enumeration object is still being initialized). Lookups go through immutable snapshots
  * (names by id for writing, values by name hash and by id for reading) which are rebuilt when a value is missing; a
  * JSON name / id which is still missing after that is an illegal enum value.
  *
  * As in jsoniter-scala, the public non-lazy `val`s of the enumeration known at compile time (`stableValues`, with the
  * names of their `val`s in `stableNames`) are written by generated code with constant names: [[stableIndex]] gives the
  * position of a value among them (from 1; 0 for any other value, or a value whose name is not the name of its `val`),
  * and the generated code writes the name of that position.
  */
final class JsoniterScalaEnumeration[E](
    enumeration: => Enumeration,
    stableValues: => Array[AnyRef],
    stableNames: Array[String]
) {
  import JsoniterScalaEnumeration.*

  /** No stable values. */
  def this(enumeration: => Enumeration) = this(enumeration, new Array[AnyRef](0), new Array[String](0))

  private lazy val enumObject: Enumeration = enumeration

  // Positions (from 1) of the stable values, indexed by their ids. Not volatile (as `snapshot`): an array read before
  // its elements are visible gives 0, which writes the name looked up at runtime.
  private var stableIndexes: Array[Int] = null
  // Whether the stable value at each position (from 0) has the name of its `val`.
  private var stableFlags: Array[Boolean] = null

  /** The position (from 1) of `x` among the stable values, or 0. */
  def stableIndex(x: E): Int = {
    var is = stableIndexes
    if (is eq null) is = initStableIndexes()
    val id = x.asInstanceOf[Enumeration#Value].id
    if ((is ne null) && id >= 0 && id < is.length) is(id) else 0
  }

  /** Whether the stable value at position `i` (from 0) has the name of its `val`, so that the generated code can read
    * it by that name (as jsoniter-scala, which compares the name read with the names of the stable values first).
    */
  def isStableName(i: Int): Boolean = {
    var fs = stableFlags
    if (fs eq null) {
      val _ = initStableIndexes()
      fs = stableFlags
    }
    (fs ne null) && fs(i)
  }

  /** The value whose name is the `l` characters read into `in`'s buffer, looked up at runtime (an illegal enum value
    * error when there is none).
    */
  def readName(in: JsonReader, l: Int): E = byName(in, l)

  // As in jsoniter-scala: only once all stable values are initialized (a codec can be used while the enumeration
  // object is being initialized); a value whose name is not the name of its `val` (`val A = Value("a")`) gets no
  // position, nor do any values with ids much larger than their number.
  private def initStableIndexes(): Array[Int] = {
    val vs = stableValues
    var i = 0
    while (i < vs.length && (vs(i) ne null)) i += 1
    if (i < vs.length) null
    else {
      var maxId = -1
      i = 0
      while (i < vs.length) {
        val id = vs(i).asInstanceOf[Enumeration#Value].id
        if (id > maxId) maxId = id
        i += 1
      }
      if (maxId > (vs.length << 2) + 64) maxId = -1
      val is = new Array[Int](maxId + 1)
      val fs = new Array[Boolean](vs.length)
      i = 0
      while (i < vs.length) {
        val v = vs(i)
        val id = v.asInstanceOf[Enumeration#Value].id
        fs(i) = v.toString == stableNames(i)
        if (id >= 0 && id <= maxId && fs(i)) is(id) = i + 1
        i += 1
      }
      stableFlags = fs
      stableIndexes = is
      is
    }
  }

  // Not volatile: a `Snapshot` is immutable (final fields), so a racy read sees either the previous one or the new
  // one, and a miss rebuilds it.
  private var snapshot: Snapshot = emptySnapshot

  private def rebuild(): Snapshot = synchronized {
    val values = enumObject.values.toArray[Enumeration#Value]
    val fresh = Snapshot.of(values)
    snapshot = fresh
    fresh
  }

  // Writing

  /** The name to write for `x` when it can be written as a non-escaped ASCII string, otherwise `null`. */
  private def asciiName(x: Enumeration#Value): String = {
    val id = x.id
    var s = snapshot
    if (id < 0 || id >= maxIndexedId) null
    else {
      if (id >= s.namesById.length || (s.valuesById(id) ne x)) s = rebuild()
      if (id < s.namesById.length && (s.valuesById(id) eq x)) s.namesById(id) else null
    }
  }

  def writeVal(x: E, out: JsonWriter): Unit = {
    val name = asciiName(x.asInstanceOf[Enumeration#Value])
    if (name ne null) out.writeNonEscapedAsciiVal(name) else out.writeVal(x.toString)
  }

  def writeKey(x: E, out: JsonWriter): Unit = {
    val name = asciiName(x.asInstanceOf[Enumeration#Value])
    if (name ne null) out.writeNonEscapedAsciiKey(name) else out.writeKey(x.toString)
  }

  def writeIdVal(x: E, out: JsonWriter, stringified: Boolean): Unit =
    if (stringified) out.writeValAsString(x.asInstanceOf[Enumeration#Value].id)
    else out.writeVal(x.asInstanceOf[Enumeration#Value].id)

  def writeIdKey(x: E, out: JsonWriter): Unit = out.writeKey(x.asInstanceOf[Enumeration#Value].id)

  // Reading

  private def byName(in: JsonReader, l: Int): E = {
    val h = in.charBufToHashCode(l)
    var x = snapshot.find(in, l, h)
    if (x eq null) x = rebuild().find(in, l, h)
    if (x eq null) in.enumValueError(l)
    x.asInstanceOf[E]
  }

  private def byId(i: Int): Enumeration#Value = {
    var x = snapshot.find(i)
    if (x eq null) x = rebuild().find(i)
    x
  }

  def readVal(in: JsonReader, default: E): E =
    if (in.isNextToken('"')) {
      in.rollbackToken()
      byName(in, in.readStringAsCharBuf())
    } else in.readNullOrTokenError(default, '"')

  def readKey(in: JsonReader): E = byName(in, in.readKeyAsCharBuf())

  def readIdVal(in: JsonReader, default: E, stringified: Boolean): E =
    if (stringified) {
      if (in.isNextToken('"')) {
        in.rollbackToken()
        val i = in.readStringAsInt()
        val x = byId(i)
        if (x eq null) in.enumValueError(i.toString)
        x.asInstanceOf[E]
      } else in.readNullOrTokenError(default, '"')
    } else {
      val t = in.nextToken()
      if ((t >= '0' && t <= '9') || t == '-') {
        in.rollbackToken()
        val i = in.readInt()
        val x = byId(i)
        if (x eq null) in.decodeError("illegal enum value " + i)
        x.asInstanceOf[E]
      } else in.readNullOrError(default, "expected digit")
    }

  def readIdKey(in: JsonReader): E = {
    val i = in.readKeyAsInt()
    val x = byId(i)
    if (x eq null) in.enumValueError(i.toString)
    x.asInstanceOf[E]
  }
}

object JsoniterScalaEnumeration {

  /** Ids below this are looked up in arrays indexed by id; other ids (negative, sparse and huge ones) in a hash map. */
  private val maxIndexedId = 1024

  /** An immutable view of an enumeration's values: by id (`valuesById`, with `namesById` holding the names which can be
    * written without escaping) and by the hash of the name (open addressing, `mask + 1` slots).
    */
  final private class Snapshot(
      val valuesById: Array[Enumeration#Value],
      val namesById: Array[String],
      otherIds: java.util.HashMap[Integer, Enumeration#Value],
      hashes: Array[Int],
      names: Array[String],
      values: Array[Enumeration#Value],
      mask: Int
  ) {

    def find(id: Int): Enumeration#Value =
      if (id >= 0 && id < valuesById.length) valuesById(id)
      else if (otherIds eq null) null
      else otherIds.get(id)

    def find(in: JsonReader, l: Int, h: Int): Enumeration#Value = {
      var i = h & mask
      var x: Enumeration#Value = null
      while ({
        val name = names(i)
        if (name eq null) false
        else if (hashes(i) == h && in.isCharBufEqualsTo(l, name)) {
          x = values(i)
          false
        } else {
          i = (i + 1) & mask
          true
        }
      }) ()
      x
    }
  }

  private object Snapshot {

    private def isNonEscapedAscii(s: String): Boolean = {
      var i = 0
      while (i < s.length) {
        val ch = s.charAt(i)
        if (ch < ' ' || ch >= 127 || ch == '"' || ch == '\\') return false
        i += 1
      }
      true
    }

    /** `JsonReader.charBufToHashCode` of the characters of `s`. */
    private def hashOf(s: String): Int = {
      var h = 0
      var i = 0
      while (i < s.length) {
        h = (h << 5) + (s.charAt(i) - h)
        i += 1
      }
      h
    }

    def of(all: Array[Enumeration#Value]): Snapshot = {
      var maxId = -1
      var i = 0
      while (i < all.length) {
        val id = all(i).id
        if (id > maxId && id < maxIndexedId) maxId = id
        i += 1
      }
      val valuesById = new Array[Enumeration#Value](maxId + 1)
      val namesById = new Array[String](maxId + 1)
      var otherIds: java.util.HashMap[Integer, Enumeration#Value] = null
      var size = 4
      while (size < (all.length << 1)) size <<= 1
      val mask = size - 1
      val hashes = new Array[Int](size)
      val names = new Array[String](size)
      val values = new Array[Enumeration#Value](size)
      i = 0
      while (i < all.length) {
        val value = all(i)
        val id = value.id
        val name = value.toString
        if (id >= 0 && id <= maxId) {
          valuesById(id) = value
          if (isNonEscapedAscii(name)) namesById(id) = name
        } else {
          if (otherIds eq null) otherIds = new java.util.HashMap[Integer, Enumeration#Value]
          otherIds.put(id, value)
        }
        // As in jsoniter-scala: of values with the same name, the first one (in the order of ids) is found.
        val h = hashOf(name)
        var j = h & mask
        while ((names(j) ne null) && !(hashes(j) == h && names(j) == name)) j = (j + 1) & mask
        if (names(j) eq null) {
          hashes(j) = h
          names(j) = name
          values(j) = value
        }
        i += 1
      }
      new Snapshot(valuesById, namesById, otherIds, hashes, names, values, mask)
    }
  }

  private val emptySnapshot: Snapshot = Snapshot.of(new Array[Enumeration#Value](0))
}
