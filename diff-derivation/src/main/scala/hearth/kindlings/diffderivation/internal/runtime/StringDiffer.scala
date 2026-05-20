package hearth.kindlings.diffderivation.internal.runtime

import hearth.kindlings.diffderivation.Edit
import hearth.kindlings.diffderivation.DiffResult.{CharChunk, StringChunk, WordChunk}

object StringDiffer {

  private val SimilarityThreshold = 0.5

  def diff(left: String, right: String): Vector[StringChunk] = {
    if (left == right) return Vector(StringChunk.EqualLine(left))

    val leftLines = splitLines(left)
    val rightLines = splitLines(right)

    val lineEdits = Myers.diff[String](leftLines, rightLines, _ == _)
    val cleaned = MyersCleanup.semanticLossless(MyersCleanup.semanticCleanup(lineEdits))

    processLineEdits(cleaned)
  }

  private def splitLines(s: String): IndexedSeq[String] =
    s.replace("\r\n", "\n").split("\n", -1).toIndexedSeq

  private def processLineEdits(edits: Vector[Edit[String]]): Vector[StringChunk] = {
    val result = Vector.newBuilder[StringChunk]
    var i = 0
    while (i < edits.length)
      edits(i) match {
        case Edit.Equal(line) =>
          result += StringChunk.EqualLine(line)
          i += 1

        case Edit.Delete(_) | Edit.Insert(_) =>
          val deletes = new scala.collection.mutable.ArrayBuffer[String]()
          val inserts = new scala.collection.mutable.ArrayBuffer[String]()
          var j = i
          while (j < edits.length)
            edits(j) match {
              case Edit.Delete(line) => deletes += line; j += 1
              case Edit.Insert(line) => inserts += line; j += 1
              case _                 => j = edits.length
            }
          val pairCount = math.min(deletes.length, inserts.length)
          var p = 0
          while (p < pairCount) {
            result += diffLinePair(deletes(p), inserts(p))
            p += 1
          }
          while (p < deletes.length) {
            result += StringChunk.DeleteLine(deletes(p))
            p += 1
          }
          p = pairCount
          while (p < inserts.length) {
            result += StringChunk.InsertLine(inserts(p))
            p += 1
          }
          i = if (j <= edits.length) i + deletes.length + inserts.length else edits.length
      }
    result.result()
  }

  private def diffLinePair(leftLine: String, rightLine: String): StringChunk = {
    if (leftLine == rightLine) return StringChunk.EqualLine(leftLine)
    if (leftLine.isEmpty) return StringChunk.InsertLine(rightLine)
    if (rightLine.isEmpty) return StringChunk.DeleteLine(leftLine)

    val leftWords = tokenize(leftLine)
    val rightWords = tokenize(rightLine)
    val wordEdits = Myers.diff[String](leftWords, rightWords, _ == _)
    val cleaned = MyersCleanup.semanticLossless(wordEdits)

    val wordChunks = processWordEdits(cleaned)
    StringChunk.ChangedLine(wordChunks)
  }

  private def processWordEdits(edits: Vector[Edit[String]]): Vector[WordChunk] = {
    val result = Vector.newBuilder[WordChunk]
    var i = 0
    while (i < edits.length)
      edits(i) match {
        case Edit.Equal(word) =>
          result += WordChunk.EqualWord(word)
          i += 1

        case Edit.Delete(_) | Edit.Insert(_) =>
          val deletes = new scala.collection.mutable.ArrayBuffer[String]()
          val inserts = new scala.collection.mutable.ArrayBuffer[String]()
          var j = i
          while (j < edits.length)
            edits(j) match {
              case Edit.Delete(w) => deletes += w; j += 1
              case Edit.Insert(w) => inserts += w; j += 1
              case _              => j = edits.length
            }
          val deleteStr = deletes.mkString
          val insertStr = inserts.mkString
          val pairCount = math.min(deletes.length, inserts.length)

          if (pairCount > 0 && deleteStr.nonEmpty && insertStr.nonEmpty) {
            var p = 0
            while (p < pairCount) {
              result += diffWordPair(deletes(p), inserts(p))
              p += 1
            }
            while (p < deletes.length) { result += WordChunk.DeleteWord(deletes(p)); p += 1 }
            p = pairCount
            while (p < inserts.length) { result += WordChunk.InsertWord(inserts(p)); p += 1 }
          } else {
            deletes.foreach(w => result += WordChunk.DeleteWord(w))
            inserts.foreach(w => result += WordChunk.InsertWord(w))
          }
          i += deletes.length + inserts.length
      }
    result.result()
  }

  private def diffWordPair(leftWord: String, rightWord: String): WordChunk = {
    if (leftWord == rightWord) return WordChunk.EqualWord(leftWord)

    val leftChars = leftWord.map(_.toString).toIndexedSeq
    val rightChars = rightWord.map(_.toString).toIndexedSeq
    val charEdits = Myers.diff[String](leftChars, rightChars, _ == _)

    val equalCount = charEdits.count { case Edit.Equal(_) => true; case _ => false }
    val similarity = if (charEdits.isEmpty) 0.0 else equalCount.toDouble / charEdits.length

    if (similarity < SimilarityThreshold) {
      WordChunk.ChangedWord(Vector(CharChunk.DeleteChar(leftWord), CharChunk.InsertChar(rightWord)))
    } else {
      val chars = charEdits.map {
        case Edit.Equal(c)  => CharChunk.EqualChar(c)
        case Edit.Insert(c) => CharChunk.InsertChar(c)
        case Edit.Delete(c) => CharChunk.DeleteChar(c)
      }
      WordChunk.ChangedWord(chars)
    }
  }

  private def tokenize(line: String): IndexedSeq[String] = {
    if (line.isEmpty) return IndexedSeq.empty
    val words = new scala.collection.mutable.ArrayBuffer[String]()
    val current = new StringBuilder()
    var i = 0
    while (i < line.length) {
      val c = line.charAt(i)
      if (c == ' ') {
        if (current.nonEmpty) {
          words += current.result()
          current.clear()
        }
        words += " "
      } else {
        current += c
      }
      i += 1
    }
    if (current.nonEmpty) words += current.result()
    words.toIndexedSeq
  }
}
