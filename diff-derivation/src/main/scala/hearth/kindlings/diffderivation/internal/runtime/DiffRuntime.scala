package hearth.kindlings.diffderivation.internal.runtime

import hearth.kindlings.diffderivation.*
import hearth.kindlings.diffderivation.DiffResult.*

object DiffRuntime {

  def diffSeq[A](
      prettyName: => String,
      plainName: => String,
      simpleName: => String,
      shortName: => String,
      left: Iterable[A],
      right: Iterable[A],
      elemDiff: Diff[A]
  ): DiffResult = {
    val leftSeq = left.toIndexedSeq
    val rightSeq = right.toIndexedSeq

    val rawEdits = Myers.diff[A](leftSeq, rightSeq, (a, b) => elemDiff.diff(a, b).isIdentical)

    rebuildSeqEdits(prettyName, plainName, simpleName, shortName, leftSeq, rightSeq, elemDiff, rawEdits)
  }

  private def rebuildSeqEdits[A](
      prettyName: => String,
      plainName: => String,
      simpleName: => String,
      shortName: => String,
      leftSeq: IndexedSeq[A],
      rightSeq: IndexedSeq[A],
      elemDiff: Diff[A],
      rawEdits: Vector[Edit[A]]
  ): DiffResult = {
    var li = 0
    var ri = 0
    val resultEdits = Vector.newBuilder[Edit[DiffResult]]

    rawEdits.foreach {
      case Edit.Equal(_) =>
        resultEdits += Edit.Equal(elemDiff.diff(leftSeq(li), rightSeq(ri)))
        li += 1
        ri += 1
      case Edit.Insert(v) =>
        resultEdits += Edit.Insert(elemDiff.snapshot(v))
        ri += 1
      case Edit.Delete(v) =>
        resultEdits += Edit.Delete(elemDiff.snapshot(v))
        li += 1
    }

    SeqDiff(prettyName, plainName, simpleName, shortName, resultEdits.result())
  }

  def diffMap[K, V](
      prettyName: => String,
      plainName: => String,
      simpleName: => String,
      shortName: => String,
      left: Iterable[(K, V)],
      right: Iterable[(K, V)],
      keyShow: K => String,
      valueDiff: Diff[V]
  ): DiffResult = {
    val leftMap = left.toVector
    val rightMap = right.toVector
    val leftKeys = leftMap.map { case (k, _) => keyShow(k) }
    val rightKeys = rightMap.map { case (k, _) => keyShow(k) }

    val leftByKey = leftMap.map { case (k, v) => keyShow(k) -> v }.toMap
    val rightByKey = rightMap.map { case (k, v) => keyShow(k) -> v }.toMap
    val allKeys = (leftKeys ++ rightKeys).distinct

    val entries = allKeys.map { key =>
      (leftByKey.get(key), rightByKey.get(key)) match {
        case (Some(lv), Some(rv)) => MapEntry.Matched(key, valueDiff.diff(lv, rv))
        case (None, Some(rv))     => MapEntry.Added(key, valueDiff.snapshot(rv))
        case (Some(lv), None)     => MapEntry.Removed(key, valueDiff.snapshot(lv))
        case (None, None)         => MapEntry.Matched(key, valueDiff.snapshot(leftByKey.values.head))
      }
    }

    MapDiff(prettyName, plainName, simpleName, shortName, entries)
  }

  def diffSet[A](
      prettyName: => String,
      plainName: => String,
      simpleName: => String,
      shortName: => String,
      left: Iterable[A],
      right: Iterable[A],
      elemDiff: Diff[A]
  ): DiffResult = {
    val leftSeq = left.toVector
    val rightSeq = right.toVector
    val entries = Vector.newBuilder[SetEntry]
    val matchedRight = new scala.collection.mutable.BitSet(rightSeq.length)

    leftSeq.foreach { l =>
      var found = false
      var j = 0
      while (j < rightSeq.length && !found) {
        if (!matchedRight(j)) {
          val d = elemDiff.diff(l, rightSeq(j))
          if (d.isIdentical) {
            entries += SetEntry.Matched(d)
            matchedRight += j
            found = true
          }
        }
        j += 1
      }
      if (!found) entries += SetEntry.Removed(elemDiff.snapshot(l))
    }

    var j = 0
    while (j < rightSeq.length) {
      if (!matchedRight(j)) entries += SetEntry.Added(elemDiff.snapshot(rightSeq(j)))
      j += 1
    }

    SetDiff(prettyName, plainName, simpleName, shortName, entries.result())
  }

  def diffOption[A](
      prettyName: => String,
      plainName: => String,
      simpleName: => String,
      shortName: => String,
      left: Option[A],
      right: Option[A],
      elemDiff: Diff[A]
  ): DiffResult = {
    val content = (left, right) match {
      case (Some(l), Some(r)) => OptionalContent.BothPresent(elemDiff.diff(l, r))
      case (Some(l), None)    => OptionalContent.LeftOnly(elemDiff.snapshot(l))
      case (None, Some(r))    => OptionalContent.RightOnly(elemDiff.snapshot(r))
      case (None, None)       => OptionalContent.BothAbsent
    }
    OptionalDiff(prettyName, plainName, simpleName, shortName, content)
  }

  def diffString(
      prettyName: => String,
      plainName: => String,
      simpleName: => String,
      shortName: => String,
      left: String,
      right: String
  ): DiffResult =
    if (left == right) Identical(prettyName, plainName, simpleName, shortName, escapeString(left))
    else StringDiff(prettyName, plainName, simpleName, shortName, StringDiffer.diff(left, right))

  def snapshotString(
      prettyName: => String,
      plainName: => String,
      simpleName: => String,
      shortName: => String,
      value: String
  ): DiffResult =
    Identical(prettyName, plainName, simpleName, shortName, escapeString(value))

  private def escapeString(s: String): String = {
    val sb = new StringBuilder(s.length + 2)
    sb += '"'
    var i = 0
    while (i < s.length) {
      s.charAt(i) match {
        case '"'  => sb ++= "\\\""
        case '\\' => sb ++= "\\\\"
        case '\n' => sb ++= "\\n"
        case '\t' => sb ++= "\\t"
        case '\r' => sb ++= "\\r"
        case c    => sb += c
      }
      i += 1
    }
    sb += '"'
    sb.result()
  }
}
