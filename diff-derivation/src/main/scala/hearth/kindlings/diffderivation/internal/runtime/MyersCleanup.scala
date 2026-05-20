package hearth.kindlings.diffderivation.internal.runtime

import hearth.kindlings.diffderivation.Edit

object MyersCleanup {

  def merge[A](edits: Vector[Edit[A]]): Vector[Edit[A]] = {
    if (edits.length <= 1) return edits
    val builder = Vector.newBuilder[Edit[A]]
    var i = 0
    while (i < edits.length) {
      builder += edits(i)
      i += 1
    }
    builder.result()
  }

  def semanticCleanup(edits: Vector[Edit[String]]): Vector[Edit[String]] = {
    if (edits.length < 3) return edits
    val buf = new scala.collection.mutable.ArrayBuffer[Edit[String]](edits.length)
    buf ++= edits

    var changed = true
    while (changed) {
      changed = false
      var i = 1
      while (i < buf.length - 1) {
        buf(i) match {
          case Edit.Equal(eq) =>
            val deleteBefore = countChars(buf, i, -1)
            val insertBefore = countChars(buf, i, -1, insertsOnly = true)
            val deleteAfter = countChars(buf, i, 1)
            val insertAfter = countChars(buf, i, 1, insertsOnly = true)
            val changesBefore = deleteBefore + insertBefore
            val changesAfter = deleteAfter + insertAfter

            if (eq.length <= math.max(changesBefore, changesAfter) / 2 && changesBefore > 0 && changesAfter > 0) {
              buf(i) = Edit.Delete(eq)
              buf.insert(i + 1, Edit.Insert(eq))
              changed = true
              if (i > 0) i -= 1
            } else {
              i += 1
            }
          case _ => i += 1
        }
      }

      if (changed) mergeAdjacentInPlace(buf)
    }

    buf.toVector
  }

  def semanticLossless(edits: Vector[Edit[String]]): Vector[Edit[String]] = {
    if (edits.length < 3) return edits
    val buf = new scala.collection.mutable.ArrayBuffer[Edit[String]](edits.length)
    buf ++= edits

    var i = 1
    while (i < buf.length - 1) {
      (buf(i - 1), buf(i + 1)) match {
        case (Edit.Equal(eqBefore), Edit.Equal(eqAfter)) =>
          buf(i) match {
            case Edit.Delete(del) =>
              val (newBefore, newDel, newAfter) = slideBest(eqBefore, del, eqAfter)
              buf(i - 1) = Edit.Equal(newBefore)
              buf(i) = Edit.Delete(newDel)
              buf(i + 1) = Edit.Equal(newAfter)
            case Edit.Insert(ins) =>
              val (newBefore, newIns, newAfter) = slideBest(eqBefore, ins, eqAfter)
              buf(i - 1) = Edit.Equal(newBefore)
              buf(i) = Edit.Insert(newIns)
              buf(i + 1) = Edit.Equal(newAfter)
            case _ => ()
          }
          i += 1
        case _ => i += 1
      }
    }

    buf.iterator.filter {
      case Edit.Equal("") => false
      case _              => true
    }.toVector
  }

  def efficiencyCleanup(edits: Vector[Edit[String]], editCost: Int): Vector[Edit[String]] = {
    if (edits.length < 3) return edits
    val buf = new scala.collection.mutable.ArrayBuffer[Edit[String]](edits.length)
    buf ++= edits

    var changed = true
    while (changed) {
      changed = false
      var i = 1
      while (i < buf.length - 1) {
        buf(i) match {
          case Edit.Equal(eq) if eq.length < editCost =>
            val hasDeleteNeighbor = isDeleteOrInsert(buf, i - 1) && isDeleteOrInsert(buf, i + 1)
            if (hasDeleteNeighbor) {
              buf(i) = Edit.Delete(eq)
              buf.insert(i + 1, Edit.Insert(eq))
              changed = true
            }
            i += 1
          case _ => i += 1
        }
      }
      if (changed) mergeAdjacentInPlace(buf)
    }

    buf.toVector
  }

  private def isDeleteOrInsert[A](buf: scala.collection.mutable.ArrayBuffer[Edit[A]], idx: Int): Boolean =
    if (idx < 0 || idx >= buf.length) false
    else
      buf(idx) match {
        case Edit.Equal(_) => false
        case _             => true
      }

  private def countChars(
      buf: scala.collection.mutable.ArrayBuffer[Edit[String]],
      from: Int,
      dir: Int,
      insertsOnly: Boolean = false
  ): Int = {
    var count = 0
    var j = from + dir
    while (j >= 0 && j < buf.length) {
      buf(j) match {
        case Edit.Equal(_) => return count
        case Edit.Delete(s) =>
          if (!insertsOnly) count += s.length
          j += dir
        case Edit.Insert(s) =>
          count += s.length
          j += dir
      }
    }
    count
  }

  private def mergeAdjacentInPlace(buf: scala.collection.mutable.ArrayBuffer[Edit[String]]): Unit = {
    var i = 0
    while (i < buf.length - 1) {
      (buf(i), buf(i + 1)) match {
        case (Edit.Equal(a), Edit.Equal(b)) =>
          buf(i) = Edit.Equal(a + b)
          val _ = buf.remove(i + 1)
        case (Edit.Delete(a), Edit.Delete(b)) =>
          buf(i) = Edit.Delete(a + b)
          val _ = buf.remove(i + 1)
        case (Edit.Insert(a), Edit.Insert(b)) =>
          buf(i) = Edit.Insert(a + b)
          val _ = buf.remove(i + 1)
        case _ => i += 1
      }
    }
  }

  private def slideBest(
      eqBefore: String,
      edit: String,
      eqAfter: String
  ): (String, String, String) = {
    var bestBefore = eqBefore
    var bestEdit = edit
    var bestAfter = eqAfter
    var bestScore = boundaryScore(eqBefore) + boundaryScore(eqAfter)

    var curBefore = eqBefore
    var curEdit = edit
    var curAfter = eqAfter

    while (curAfter.nonEmpty && curEdit.nonEmpty && curEdit.last == curAfter.head) {
      curBefore = curBefore + curEdit.charAt(0)
      curEdit = curEdit.substring(1) + curAfter.charAt(0)
      curAfter = curAfter.substring(1)
      val score = boundaryScore(curBefore) + boundaryScore(curAfter)
      if (score >= bestScore) {
        bestScore = score
        bestBefore = curBefore
        bestEdit = curEdit
        bestAfter = curAfter
      }
    }

    (bestBefore, bestEdit, bestAfter)
  }

  private[runtime] def boundaryScore(text: String): Int = {
    if (text.isEmpty) return 5
    val last = text.last
    if (last == '\n') {
      if (text.length >= 2 && text.charAt(text.length - 2) == '\n') 4
      else 3
    } else if (last == ' ' || last == '\t') 2
    else if (!last.isLetterOrDigit) 1
    else 0
  }
}
