package hearth.kindlings.diffderivation

import hearth.kindlings.diffderivation.internal.runtime.StringDiffer
import hearth.kindlings.diffderivation.DiffResult.{CharChunk, StringChunk, WordChunk}

final class StringDifferSpec extends hearth.MacroSuite {

  group("StringDiffer.diff") {

    test("identical strings") {
      val result = StringDiffer.diff("hello", "hello")
      assertEquals(result, Vector(StringChunk.EqualLine("hello")))
    }

    test("single line word change") {
      val result = StringDiffer.diff("the cat sat", "the dog sat")
      assert(result.nonEmpty, "expected non-empty result")
      result.head match {
        case StringChunk.ChangedLine(words) =>
          val hasEqual = words.exists { case WordChunk.EqualWord(_) => true; case _ => false }
          assert(hasEqual, s"expected some equal words in $words")
        case other =>
          fail(s"expected ChangedLine, got $other")
      }
    }

    test("multi-line with added line") {
      val left = "line1\nline2\nline3"
      val right = "line1\nline2\nnewline\nline3"
      val result = StringDiffer.diff(left, right)
      val insertCount = result.count { case StringChunk.InsertLine(_) => true; case _ => false }
      assert(insertCount >= 1, s"expected at least 1 InsertLine, got $result")
    }

    test("multi-line with removed line") {
      val left = "line1\nline2\nline3"
      val right = "line1\nline3"
      val result = StringDiffer.diff(left, right)
      val deleteOrChange = result.count {
        case StringChunk.DeleteLine(_)  => true
        case StringChunk.ChangedLine(_) => true
        case _                          => false
      }
      assert(deleteOrChange >= 1, s"expected at least 1 delete/change, got $result")
    }

    test("character-level drill down") {
      val result = StringDiffer.diff("cat", "car")
      result.head match {
        case StringChunk.ChangedLine(words) =>
          words.head match {
            case WordChunk.ChangedWord(chars) =>
              val hasInsert = chars.exists { case CharChunk.InsertChar(_) => true; case _ => false }
              val hasDelete = chars.exists { case CharChunk.DeleteChar(_) => true; case _ => false }
              assert(hasInsert && hasDelete, s"expected char-level edits in $chars")
            case other =>
              fail(s"expected ChangedWord, got $other")
          }
        case other =>
          fail(s"expected ChangedLine, got $other")
      }
    }

    test("empty left string") {
      val result = StringDiffer.diff("", "hello")
      val hasInsert = result.exists {
        case StringChunk.InsertLine(_)  => true
        case StringChunk.ChangedLine(_) => true
        case _                          => false
      }
      assert(hasInsert, s"expected insert for empty->non-empty, got $result")
    }

    test("empty right string") {
      val result = StringDiffer.diff("hello", "")
      val hasDelete = result.exists {
        case StringChunk.DeleteLine(_)  => true
        case StringChunk.ChangedLine(_) => true
        case _                          => false
      }
      assert(hasDelete, s"expected delete for non-empty->empty, got $result")
    }
  }
}
