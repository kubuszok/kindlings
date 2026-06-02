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

  group("edge cases") {

    test("strings with escape characters") {
      val left = "line1\tvalue\nline2\\end"
      val right = "line1\tchanged\nline2\\end"
      val result = StringDiffer.diff(left, right)
      assert(result.nonEmpty, "expected non-empty result")
      val hasChange = result.exists {
        case StringChunk.ChangedLine(_) => true
        case _                          => false
      }
      assert(hasChange, s"expected a ChangedLine for tab-containing lines, got $result")
      // The unchanged line with backslash should be preserved as equal
      val hasEqual = result.exists { case StringChunk.EqualLine(t) => t.contains("\\"); case _ => false }
      assert(hasEqual, s"expected EqualLine with backslash, got $result")
    }

    test("strings with embedded quotes") {
      val left = "say \"hello\" now"
      val right = "say \"goodbye\" now"
      val result = StringDiffer.diff(left, right)
      result.head match {
        case StringChunk.ChangedLine(words) =>
          val hasEqual = words.exists { case WordChunk.EqualWord(_) => true; case _ => false }
          assert(hasEqual, s"expected some equal words around quoted text, got $words")
        case other =>
          fail(s"expected ChangedLine, got $other")
      }
    }

    test("unicode multi-byte characters") {
      val left = "éèê"
      val right = "éëê"
      val result = StringDiffer.diff(left, right)
      val hasChange = result.exists {
        case StringChunk.ChangedLine(_) => true
        case _                          => false
      }
      assert(hasChange, s"expected ChangedLine for unicode diff, got $result")
    }

    test("emoji characters") {
      val left = "hello 😀 world"
      val right = "hello 😢 world"
      val result = StringDiffer.diff(left, right)
      result.head match {
        case StringChunk.ChangedLine(words) =>
          val hasEqual = words.exists { case WordChunk.EqualWord(_) => true; case _ => false }
          assert(hasEqual, s"expected equal words around emoji, got $words")
        case other =>
          fail(s"expected ChangedLine, got $other")
      }
    }

    test("empty vs non-empty string") {
      val result = StringDiffer.diff("", "non-empty")
      val hasInsert = result.exists {
        case StringChunk.InsertLine(_)  => true
        case StringChunk.ChangedLine(_) => true
        case _                          => false
      }
      assert(hasInsert, s"expected insert for empty->non-empty, got $result")

      val result2 = StringDiffer.diff("non-empty", "")
      val hasDelete = result2.exists {
        case StringChunk.DeleteLine(_)  => true
        case StringChunk.ChangedLine(_) => true
        case _                          => false
      }
      assert(hasDelete, s"expected delete for non-empty->empty, got $result2")
    }

    test("strings differing only in whitespace") {
      val left = "hello world"
      val right = "hello  world"
      val result = StringDiffer.diff(left, right)
      assert(result != Vector(StringChunk.EqualLine(left)), s"expected difference for whitespace change, got $result")
    }

    test("leading and trailing whitespace difference") {
      val left = "hello"
      val right = " hello "
      val result = StringDiffer.diff(left, right)
      val hasChange = result.exists {
        case StringChunk.ChangedLine(_) => true
        case _                          => false
      }
      assert(hasChange, s"expected ChangedLine for whitespace-only diff, got $result")
    }

    test("long identical strings with single character difference") {
      val prefix = "a" * 500
      val suffix = "b" * 500
      val left = prefix + "X" + suffix
      val right = prefix + "Y" + suffix
      val result = StringDiffer.diff(left, right)
      result.head match {
        case StringChunk.ChangedLine(words) =>
          words.head match {
            case WordChunk.ChangedWord(chars) =>
              val equalCount = chars.count { case CharChunk.EqualChar(_) => true; case _ => false }
              assert(equalCount > 0, s"expected equal chars around single-char diff, got $chars")
              val insertCount = chars.count { case CharChunk.InsertChar(_) => true; case _ => false }
              val deleteCount = chars.count { case CharChunk.DeleteChar(_) => true; case _ => false }
              assert(insertCount >= 1 && deleteCount >= 1, s"expected char-level insert+delete, got $chars")
            case other =>
              fail(s"expected ChangedWord, got $other")
          }
        case other =>
          fail(s"expected ChangedLine, got $other")
      }
    }
  }
}
