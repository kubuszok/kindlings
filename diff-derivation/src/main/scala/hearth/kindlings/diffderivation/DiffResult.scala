package hearth.kindlings.diffderivation

sealed abstract class DiffResult(
    _prettyName: => String,
    _plainName: => String,
    _simpleName: => String,
    _shortName: => String
) extends Product
    with Serializable {
  lazy val prettyName: String = _prettyName
  lazy val plainName: String = _plainName
  lazy val simpleName: String = _simpleName
  lazy val shortName: String = _shortName
  def isIdentical: Boolean
}

object DiffResult {

  // ── Leaf nodes ──────────────────────────────────────────────

  final class Identical private (
      _prettyName: => String,
      _plainName: => String,
      _simpleName: => String,
      _shortName: => String,
      val display: String
  ) extends DiffResult(_prettyName, _plainName, _simpleName, _shortName) {
    def isIdentical: Boolean = true
    override def productPrefix: String = "Identical"
    override def productArity: Int = 1
    override def productElement(n: Int): Any = n match {
      case 0 => display
      case _ => throw new IndexOutOfBoundsException(n.toString)
    }
    override def canEqual(that: Any): Boolean = that.isInstanceOf[Identical]
    override def toString: String = s"Identical($display)"
  }
  object Identical {
    def apply(
        prettyName: => String,
        plainName: => String,
        simpleName: => String,
        shortName: => String,
        display: String
    ): Identical = new Identical(prettyName, plainName, simpleName, shortName, display)
    def unapply(v: Identical): Some[String] = Some(v.display)
  }

  final class ValueChanged private (
      _prettyName: => String,
      _plainName: => String,
      _simpleName: => String,
      _shortName: => String,
      val left: String,
      val right: String
  ) extends DiffResult(_prettyName, _plainName, _simpleName, _shortName) {
    def isIdentical: Boolean = false
    override def productPrefix: String = "ValueChanged"
    override def productArity: Int = 2
    override def productElement(n: Int): Any = n match {
      case 0 => left
      case 1 => right
      case _ => throw new IndexOutOfBoundsException(n.toString)
    }
    override def canEqual(that: Any): Boolean = that.isInstanceOf[ValueChanged]
    override def toString: String = s"ValueChanged($left, $right)"
  }
  object ValueChanged {
    def apply(
        prettyName: => String,
        plainName: => String,
        simpleName: => String,
        shortName: => String,
        left: String,
        right: String
    ): ValueChanged = new ValueChanged(prettyName, plainName, simpleName, shortName, left, right)
    def unapply(v: ValueChanged): Some[(String, String)] = Some((v.left, v.right))
  }

  // ── Product (case class) ───────────────────────────────────

  final class Record private (
      _prettyName: => String,
      _plainName: => String,
      _simpleName: => String,
      _shortName: => String,
      val fields: Vector[(String, DiffResult)]
  ) extends DiffResult(_prettyName, _plainName, _simpleName, _shortName) {
    def isIdentical: Boolean = fields.forall(_._2.isIdentical)
    override def productPrefix: String = "Record"
    override def productArity: Int = 1
    override def productElement(n: Int): Any = n match {
      case 0 => fields
      case _ => throw new IndexOutOfBoundsException(n.toString)
    }
    override def canEqual(that: Any): Boolean = that.isInstanceOf[Record]
    override def toString: String = s"Record($simpleName, $fields)"
  }
  object Record {
    def apply(
        prettyName: => String,
        plainName: => String,
        simpleName: => String,
        shortName: => String,
        fields: Vector[(String, DiffResult)]
    ): Record = new Record(prettyName, plainName, simpleName, shortName, fields)
    def unapply(v: Record): Some[Vector[(String, DiffResult)]] = Some(v.fields)
  }

  // ── Coproduct (sealed trait / enum) ────────────────────────

  final class Variant private (
      _prettyName: => String,
      _plainName: => String,
      _simpleName: => String,
      _shortName: => String,
      val variantName: String,
      val body: DiffResult
  ) extends DiffResult(_prettyName, _plainName, _simpleName, _shortName) {
    def isIdentical: Boolean = body.isIdentical
    override def productPrefix: String = "Variant"
    override def productArity: Int = 2
    override def productElement(n: Int): Any = n match {
      case 0 => variantName
      case 1 => body
      case _ => throw new IndexOutOfBoundsException(n.toString)
    }
    override def canEqual(that: Any): Boolean = that.isInstanceOf[Variant]
    override def toString: String = s"Variant($simpleName, $variantName, $body)"
  }
  object Variant {
    def apply(
        prettyName: => String,
        plainName: => String,
        simpleName: => String,
        shortName: => String,
        variantName: String,
        body: DiffResult
    ): Variant = new Variant(prettyName, plainName, simpleName, shortName, variantName, body)
    def unapply(v: Variant): Some[(String, DiffResult)] = Some((v.variantName, v.body))
  }

  final class TypeMismatch private (
      _prettyName: => String,
      _plainName: => String,
      _simpleName: => String,
      _shortName: => String,
      val leftVariant: String,
      val leftSnapshot: DiffResult,
      val rightVariant: String,
      val rightSnapshot: DiffResult
  ) extends DiffResult(_prettyName, _plainName, _simpleName, _shortName) {
    def isIdentical: Boolean = false
    override def productPrefix: String = "TypeMismatch"
    override def productArity: Int = 4
    override def productElement(n: Int): Any = n match {
      case 0 => leftVariant
      case 1 => leftSnapshot
      case 2 => rightVariant
      case 3 => rightSnapshot
      case _ => throw new IndexOutOfBoundsException(n.toString)
    }
    override def canEqual(that: Any): Boolean = that.isInstanceOf[TypeMismatch]
    override def toString: String = s"TypeMismatch($simpleName, $leftVariant, $rightVariant)"
  }
  object TypeMismatch {
    def apply(
        prettyName: => String,
        plainName: => String,
        simpleName: => String,
        shortName: => String,
        leftVariant: String,
        leftSnapshot: DiffResult,
        rightVariant: String,
        rightSnapshot: DiffResult
    ): TypeMismatch =
      new TypeMismatch(prettyName, plainName, simpleName, shortName, leftVariant, leftSnapshot, rightVariant,
        rightSnapshot)
    def unapply(v: TypeMismatch): Some[(String, DiffResult, String, DiffResult)] =
      Some((v.leftVariant, v.leftSnapshot, v.rightVariant, v.rightSnapshot))
  }

  // ── Ordered collections (Myers-based) ──────────────────────

  final class SeqDiff private (
      _prettyName: => String,
      _plainName: => String,
      _simpleName: => String,
      _shortName: => String,
      val edits: Vector[Edit[DiffResult]]
  ) extends DiffResult(_prettyName, _plainName, _simpleName, _shortName) {
    def isIdentical: Boolean = edits.forall {
      case Edit.Equal(d) => d.isIdentical
      case _             => false
    }
    override def productPrefix: String = "SeqDiff"
    override def productArity: Int = 1
    override def productElement(n: Int): Any = n match {
      case 0 => edits
      case _ => throw new IndexOutOfBoundsException(n.toString)
    }
    override def canEqual(that: Any): Boolean = that.isInstanceOf[SeqDiff]
    override def toString: String = s"SeqDiff($simpleName, ${edits.size} edits)"
  }
  object SeqDiff {
    def apply(
        prettyName: => String,
        plainName: => String,
        simpleName: => String,
        shortName: => String,
        edits: Vector[Edit[DiffResult]]
    ): SeqDiff = new SeqDiff(prettyName, plainName, simpleName, shortName, edits)
    def unapply(v: SeqDiff): Some[Vector[Edit[DiffResult]]] = Some(v.edits)
  }

  // ── Map ────────────────────────────────────────────────────

  sealed trait MapEntry extends Product with Serializable {
    def isIdentical: Boolean
  }
  object MapEntry {
    final case class Matched(key: String, valueDiff: DiffResult) extends MapEntry {
      def isIdentical: Boolean = valueDiff.isIdentical
    }
    final case class Added(key: String, snapshot: DiffResult) extends MapEntry {
      def isIdentical: Boolean = false
    }
    final case class Removed(key: String, snapshot: DiffResult) extends MapEntry {
      def isIdentical: Boolean = false
    }
  }

  final class MapDiff private (
      _prettyName: => String,
      _plainName: => String,
      _simpleName: => String,
      _shortName: => String,
      val entries: Vector[MapEntry]
  ) extends DiffResult(_prettyName, _plainName, _simpleName, _shortName) {
    def isIdentical: Boolean = entries.forall(_.isIdentical)
    override def productPrefix: String = "MapDiff"
    override def productArity: Int = 1
    override def productElement(n: Int): Any = n match {
      case 0 => entries
      case _ => throw new IndexOutOfBoundsException(n.toString)
    }
    override def canEqual(that: Any): Boolean = that.isInstanceOf[MapDiff]
    override def toString: String = s"MapDiff($simpleName, ${entries.size} entries)"
  }
  object MapDiff {
    def apply(
        prettyName: => String,
        plainName: => String,
        simpleName: => String,
        shortName: => String,
        entries: Vector[MapEntry]
    ): MapDiff = new MapDiff(prettyName, plainName, simpleName, shortName, entries)
    def unapply(v: MapDiff): Some[Vector[MapEntry]] = Some(v.entries)
  }

  // ── Set ────────────────────────────────────────────────────

  sealed trait SetEntry extends Product with Serializable {
    def isIdentical: Boolean
  }
  object SetEntry {
    final case class Matched(diff: DiffResult) extends SetEntry {
      def isIdentical: Boolean = diff.isIdentical
    }
    final case class Added(snapshot: DiffResult) extends SetEntry {
      def isIdentical: Boolean = false
    }
    final case class Removed(snapshot: DiffResult) extends SetEntry {
      def isIdentical: Boolean = false
    }
  }

  final class SetDiff private (
      _prettyName: => String,
      _plainName: => String,
      _simpleName: => String,
      _shortName: => String,
      val entries: Vector[SetEntry]
  ) extends DiffResult(_prettyName, _plainName, _simpleName, _shortName) {
    def isIdentical: Boolean = entries.forall(_.isIdentical)
    override def productPrefix: String = "SetDiff"
    override def productArity: Int = 1
    override def productElement(n: Int): Any = n match {
      case 0 => entries
      case _ => throw new IndexOutOfBoundsException(n.toString)
    }
    override def canEqual(that: Any): Boolean = that.isInstanceOf[SetDiff]
    override def toString: String = s"SetDiff($simpleName, ${entries.size} entries)"
  }
  object SetDiff {
    def apply(
        prettyName: => String,
        plainName: => String,
        simpleName: => String,
        shortName: => String,
        entries: Vector[SetEntry]
    ): SetDiff = new SetDiff(prettyName, plainName, simpleName, shortName, entries)
    def unapply(v: SetDiff): Some[Vector[SetEntry]] = Some(v.entries)
  }

  // ── Optional ───────────────────────────────────────────────

  sealed trait OptionalContent extends Product with Serializable
  object OptionalContent {
    final case class BothPresent(diff: DiffResult) extends OptionalContent
    final case class LeftOnly(snapshot: DiffResult) extends OptionalContent
    final case class RightOnly(snapshot: DiffResult) extends OptionalContent
    case object BothAbsent extends OptionalContent
  }

  final class OptionalDiff private (
      _prettyName: => String,
      _plainName: => String,
      _simpleName: => String,
      _shortName: => String,
      val inner: OptionalContent
  ) extends DiffResult(_prettyName, _plainName, _simpleName, _shortName) {
    def isIdentical: Boolean = inner match {
      case OptionalContent.BothPresent(d) => d.isIdentical
      case OptionalContent.BothAbsent     => true
      case _                              => false
    }
    override def productPrefix: String = "OptionalDiff"
    override def productArity: Int = 1
    override def productElement(n: Int): Any = n match {
      case 0 => inner
      case _ => throw new IndexOutOfBoundsException(n.toString)
    }
    override def canEqual(that: Any): Boolean = that.isInstanceOf[OptionalDiff]
    override def toString: String = s"OptionalDiff($simpleName, $inner)"
  }
  object OptionalDiff {
    def apply(
        prettyName: => String,
        plainName: => String,
        simpleName: => String,
        shortName: => String,
        inner: OptionalContent
    ): OptionalDiff = new OptionalDiff(prettyName, plainName, simpleName, shortName, inner)
    def unapply(v: OptionalDiff): Some[OptionalContent] = Some(v.inner)
  }

  // ── String (hierarchical Myers) ────────────────────────────

  sealed trait CharChunk extends Product with Serializable
  object CharChunk {
    final case class EqualChar(text: String) extends CharChunk
    final case class InsertChar(text: String) extends CharChunk
    final case class DeleteChar(text: String) extends CharChunk
  }

  sealed trait WordChunk extends Product with Serializable
  object WordChunk {
    final case class EqualWord(text: String) extends WordChunk
    final case class InsertWord(text: String) extends WordChunk
    final case class DeleteWord(text: String) extends WordChunk
    final case class ChangedWord(chars: Vector[CharChunk]) extends WordChunk
  }

  sealed trait StringChunk extends Product with Serializable
  object StringChunk {
    final case class EqualLine(text: String) extends StringChunk
    final case class InsertLine(text: String) extends StringChunk
    final case class DeleteLine(text: String) extends StringChunk
    final case class ChangedLine(words: Vector[WordChunk]) extends StringChunk
  }

  final class StringDiff private (
      _prettyName: => String,
      _plainName: => String,
      _simpleName: => String,
      _shortName: => String,
      val chunks: Vector[StringChunk]
  ) extends DiffResult(_prettyName, _plainName, _simpleName, _shortName) {
    def isIdentical: Boolean = chunks.forall {
      case StringChunk.EqualLine(_) => true
      case _                        => false
    }
    override def productPrefix: String = "StringDiff"
    override def productArity: Int = 1
    override def productElement(n: Int): Any = n match {
      case 0 => chunks
      case _ => throw new IndexOutOfBoundsException(n.toString)
    }
    override def canEqual(that: Any): Boolean = that.isInstanceOf[StringDiff]
    override def toString: String = s"StringDiff($simpleName, ${chunks.size} chunks)"
  }
  object StringDiff {
    def apply(
        prettyName: => String,
        plainName: => String,
        simpleName: => String,
        shortName: => String,
        chunks: Vector[StringChunk]
    ): StringDiff = new StringDiff(prettyName, plainName, simpleName, shortName, chunks)
    def unapply(v: StringDiff): Some[Vector[StringChunk]] = Some(v.chunks)
  }
}
