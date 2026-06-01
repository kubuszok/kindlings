package hearth.kindlings.diffderivation

import hearth.kindlings.diffderivation.internal.runtime.DiffRuntime

final class DiffRuntimeSpec extends hearth.MacroSuite {

  private val pn = "test.Pretty"
  private val fn = "test.Full"
  private val sn = "Test"
  private val sh = "Test"

  // A simple Diff for Int that compares by value
  private val intDiff: Diff[Int] = new Diff[Int] {
    val prettyName = "Int"
    val plainName = "Int"
    val simpleName = "Int"
    val shortName = "Int"
    def diff(left: Int, right: Int): DiffResult =
      if (left == right) DiffResult.Identical(pn, fn, "Int", "Int", left.toString)
      else DiffResult.ValueChanged(pn, fn, "Int", "Int", left.toString, right.toString)
    def snapshot(value: Int): DiffResult =
      DiffResult.Identical(pn, fn, "Int", "Int", value.toString)
  }

  // A simple Diff for String that compares by value
  private val stringDiff: Diff[String] = new Diff[String] {
    val prettyName = "String"
    val plainName = "String"
    val simpleName = "String"
    val shortName = "String"
    def diff(left: String, right: String): DiffResult =
      if (left == right) DiffResult.Identical(pn, fn, "String", "String", s""""$left"""")
      else DiffResult.ValueChanged(pn, fn, "String", "String", s""""$left"""", s""""$right"""")
    def snapshot(value: String): DiffResult =
      DiffResult.Identical(pn, fn, "String", "String", s""""$value"""")
  }

  group("DiffRuntime") {

    group("diffSeq") {

      test("identical sequences") {
        val result = DiffRuntime.diffSeq(pn, fn, sn, sh, List(1, 2, 3), List(1, 2, 3), intDiff)
        assert(result.isIdentical, s"expected identical, got $result")
        assert(result.isInstanceOf[DiffResult.SeqDiff], s"expected SeqDiff, got $result")
      }

      test("empty sequences") {
        val result = DiffRuntime.diffSeq(pn, fn, sn, sh, List.empty[Int], List.empty[Int], intDiff)
        assert(result.isIdentical, s"expected identical, got $result")
      }

      test("insertion") {
        val result = DiffRuntime.diffSeq(pn, fn, sn, sh, List(1, 3), List(1, 2, 3), intDiff)
        assert(!result.isIdentical, s"expected not identical, got $result")
        result match {
          case sd: DiffResult.SeqDiff =>
            val inserts = sd.edits.collect { case Edit.Insert(_) => () }
            assert(inserts.nonEmpty, s"expected insert edits, got ${sd.edits}")
          case _ => fail(s"expected SeqDiff, got $result")
        }
      }

      test("deletion") {
        val result = DiffRuntime.diffSeq(pn, fn, sn, sh, List(1, 2, 3), List(1, 3), intDiff)
        assert(!result.isIdentical, s"expected not identical, got $result")
        result match {
          case sd: DiffResult.SeqDiff =>
            val deletes = sd.edits.collect { case Edit.Delete(_) => () }
            assert(deletes.nonEmpty, s"expected delete edits, got ${sd.edits}")
          case _ => fail(s"expected SeqDiff, got $result")
        }
      }

      test("empty left non-empty right") {
        val result = DiffRuntime.diffSeq(pn, fn, sn, sh, List.empty[Int], List(1, 2), intDiff)
        assert(!result.isIdentical, s"expected not identical, got $result")
        result match {
          case sd: DiffResult.SeqDiff =>
            assert(sd.edits.forall(_.isInstanceOf[Edit.Insert[?]]), s"expected all inserts, got ${sd.edits}")
          case _ => fail(s"expected SeqDiff, got $result")
        }
      }

      test("non-empty left empty right") {
        val result = DiffRuntime.diffSeq(pn, fn, sn, sh, List(1, 2), List.empty[Int], intDiff)
        assert(!result.isIdentical, s"expected not identical, got $result")
        result match {
          case sd: DiffResult.SeqDiff =>
            assert(sd.edits.forall(_.isInstanceOf[Edit.Delete[?]]), s"expected all deletes, got ${sd.edits}")
          case _ => fail(s"expected SeqDiff, got $result")
        }
      }

      test("replacement in middle") {
        val result = DiffRuntime.diffSeq(pn, fn, sn, sh, List(1, 2, 3), List(1, 99, 3), intDiff)
        assert(!result.isIdentical, s"expected not identical, got $result")
      }
    }

    group("diffMap") {

      test("identical maps") {
        val result = DiffRuntime.diffMap[String, Int](
          pn,
          fn,
          sn,
          sh,
          Map("a" -> 1, "b" -> 2),
          Map("a" -> 1, "b" -> 2),
          _.toString,
          intDiff
        )
        assert(result.isIdentical, s"expected identical, got $result")
        assert(result.isInstanceOf[DiffResult.MapDiff], s"expected MapDiff, got $result")
      }

      test("added key") {
        val result = DiffRuntime.diffMap[String, Int](
          pn,
          fn,
          sn,
          sh,
          Map("a" -> 1),
          Map("a" -> 1, "b" -> 2),
          _.toString,
          intDiff
        )
        assert(!result.isIdentical)
        result match {
          case md: DiffResult.MapDiff =>
            val added = md.entries.collect { case a: DiffResult.MapEntry.Added => a }
            assert(added.exists(_.key == "b"), s"expected added key 'b', entries: ${md.entries}")
          case _ => fail(s"expected MapDiff, got $result")
        }
      }

      test("removed key") {
        val result = DiffRuntime.diffMap[String, Int](
          pn,
          fn,
          sn,
          sh,
          Map("a" -> 1, "b" -> 2),
          Map("a" -> 1),
          _.toString,
          intDiff
        )
        assert(!result.isIdentical)
        result match {
          case md: DiffResult.MapDiff =>
            val removed = md.entries.collect { case r: DiffResult.MapEntry.Removed => r }
            assert(removed.exists(_.key == "b"), s"expected removed key 'b', entries: ${md.entries}")
          case _ => fail(s"expected MapDiff, got $result")
        }
      }

      test("changed value") {
        val result = DiffRuntime.diffMap[String, Int](
          pn,
          fn,
          sn,
          sh,
          Map("a" -> 1),
          Map("a" -> 99),
          _.toString,
          intDiff
        )
        assert(!result.isIdentical)
        result match {
          case md: DiffResult.MapDiff =>
            val matched = md.entries.collect { case m: DiffResult.MapEntry.Matched => m }
            assert(
              matched.exists(e => e.key == "a" && !e.valueDiff.isIdentical),
              s"expected changed value, entries: ${md.entries}"
            )
          case _ => fail(s"expected MapDiff, got $result")
        }
      }

      test("empty maps") {
        val result = DiffRuntime.diffMap[String, Int](
          pn,
          fn,
          sn,
          sh,
          Map.empty[String, Int],
          Map.empty[String, Int],
          _.toString,
          intDiff
        )
        assert(result.isIdentical, s"expected identical, got $result")
      }

      test("empty vs non-empty") {
        val result = DiffRuntime.diffMap[String, Int](
          pn,
          fn,
          sn,
          sh,
          Map.empty[String, Int],
          Map("x" -> 42),
          _.toString,
          intDiff
        )
        assert(!result.isIdentical)
        result match {
          case md: DiffResult.MapDiff =>
            assert(md.entries.nonEmpty)
            val added = md.entries.collect { case a: DiffResult.MapEntry.Added => a }
            assert(added.nonEmpty, s"expected added entries")
          case _ => fail(s"expected MapDiff, got $result")
        }
      }

      test("non-empty vs empty") {
        val result = DiffRuntime.diffMap[String, Int](
          pn,
          fn,
          sn,
          sh,
          Map("x" -> 42),
          Map.empty[String, Int],
          _.toString,
          intDiff
        )
        assert(!result.isIdentical)
        result match {
          case md: DiffResult.MapDiff =>
            val removed = md.entries.collect { case r: DiffResult.MapEntry.Removed => r }
            assert(removed.nonEmpty, s"expected removed entries")
          case _ => fail(s"expected MapDiff, got $result")
        }
      }
    }

    group("diffSet") {

      test("identical sets") {
        val result = DiffRuntime.diffSet(pn, fn, sn, sh, Set("a", "b"), Set("a", "b"), stringDiff)
        assert(result.isIdentical, s"expected identical, got $result")
        assert(result.isInstanceOf[DiffResult.SetDiff], s"expected SetDiff, got $result")
      }

      test("added element") {
        val result = DiffRuntime.diffSet(pn, fn, sn, sh, Set("a"), Set("a", "b"), stringDiff)
        assert(!result.isIdentical)
        result match {
          case sd: DiffResult.SetDiff =>
            val added = sd.entries.collect { case a: DiffResult.SetEntry.Added => a }
            assert(added.nonEmpty, s"expected added entry, entries: ${sd.entries}")
          case _ => fail(s"expected SetDiff, got $result")
        }
      }

      test("removed element") {
        val result = DiffRuntime.diffSet(pn, fn, sn, sh, Set("a", "b"), Set("a"), stringDiff)
        assert(!result.isIdentical)
        result match {
          case sd: DiffResult.SetDiff =>
            val removed = sd.entries.collect { case r: DiffResult.SetEntry.Removed => r }
            assert(removed.nonEmpty, s"expected removed entry, entries: ${sd.entries}")
          case _ => fail(s"expected SetDiff, got $result")
        }
      }

      test("empty sets") {
        val result = DiffRuntime.diffSet(pn, fn, sn, sh, Set.empty[String], Set.empty[String], stringDiff)
        assert(result.isIdentical, s"expected identical, got $result")
      }

      test("empty vs non-empty") {
        val result = DiffRuntime.diffSet(pn, fn, sn, sh, Set.empty[String], Set("x"), stringDiff)
        assert(!result.isIdentical)
        result match {
          case sd: DiffResult.SetDiff =>
            val added = sd.entries.collect { case a: DiffResult.SetEntry.Added => a }
            assert(added.nonEmpty, s"expected added entries")
          case _ => fail(s"expected SetDiff, got $result")
        }
      }

      test("non-empty vs empty") {
        val result = DiffRuntime.diffSet(pn, fn, sn, sh, Set("x"), Set.empty[String], stringDiff)
        assert(!result.isIdentical)
        result match {
          case sd: DiffResult.SetDiff =>
            val removed = sd.entries.collect { case r: DiffResult.SetEntry.Removed => r }
            assert(removed.nonEmpty, s"expected removed entries")
          case _ => fail(s"expected SetDiff, got $result")
        }
      }

      test("completely different sets") {
        val result = DiffRuntime.diffSet(pn, fn, sn, sh, Set("a", "b"), Set("c", "d"), stringDiff)
        assert(!result.isIdentical)
        result match {
          case sd: DiffResult.SetDiff =>
            val removed = sd.entries.collect { case r: DiffResult.SetEntry.Removed => r }
            val added = sd.entries.collect { case a: DiffResult.SetEntry.Added => a }
            assertEquals(removed.size, 2)
            assertEquals(added.size, 2)
          case _ => fail(s"expected SetDiff, got $result")
        }
      }
    }

    group("diffOption") {

      test("Some/Some identical") {
        val result = DiffRuntime.diffOption(pn, fn, sn, sh, Some(42), Some(42), intDiff)
        assert(result.isIdentical, s"expected identical, got $result")
        result match {
          case od: DiffResult.OptionalDiff =>
            od.inner match {
              case DiffResult.OptionalContent.BothPresent(d) => assert(d.isIdentical)
              case other                                     => fail(s"expected BothPresent, got $other")
            }
          case _ => fail(s"expected OptionalDiff, got $result")
        }
      }

      test("Some/Some different") {
        val result = DiffRuntime.diffOption(pn, fn, sn, sh, Some(1), Some(2), intDiff)
        assert(!result.isIdentical)
        result match {
          case od: DiffResult.OptionalDiff =>
            od.inner match {
              case DiffResult.OptionalContent.BothPresent(d) => assert(!d.isIdentical)
              case other                                     => fail(s"expected BothPresent, got $other")
            }
          case _ => fail(s"expected OptionalDiff, got $result")
        }
      }

      test("Some/None") {
        val result = DiffRuntime.diffOption(pn, fn, sn, sh, Some(42), None, intDiff)
        assert(!result.isIdentical)
        result match {
          case od: DiffResult.OptionalDiff =>
            od.inner match {
              case _: DiffResult.OptionalContent.LeftOnly => ()
              case other                                  => fail(s"expected LeftOnly, got $other")
            }
          case _ => fail(s"expected OptionalDiff, got $result")
        }
      }

      test("None/Some") {
        val result = DiffRuntime.diffOption(pn, fn, sn, sh, None, Some(42), intDiff)
        assert(!result.isIdentical)
        result match {
          case od: DiffResult.OptionalDiff =>
            od.inner match {
              case _: DiffResult.OptionalContent.RightOnly => ()
              case other                                   => fail(s"expected RightOnly, got $other")
            }
          case _ => fail(s"expected OptionalDiff, got $result")
        }
      }

      test("None/None") {
        val result = DiffRuntime.diffOption(pn, fn, sn, sh, None, None, intDiff)
        assert(result.isIdentical, s"expected identical, got $result")
        result match {
          case od: DiffResult.OptionalDiff =>
            od.inner match {
              case DiffResult.OptionalContent.BothAbsent => ()
              case other                                 => fail(s"expected BothAbsent, got $other")
            }
          case _ => fail(s"expected OptionalDiff, got $result")
        }
      }
    }

    group("diffString") {

      test("identical strings") {
        val result = DiffRuntime.diffString(pn, fn, sn, sh, "hello", "hello")
        assert(result.isIdentical, s"expected identical, got $result")
        assert(result.isInstanceOf[DiffResult.Identical], s"expected Identical, got $result")
      }

      test("different strings") {
        val result = DiffRuntime.diffString(pn, fn, sn, sh, "hello", "world")
        assert(!result.isIdentical, s"expected not identical, got $result")
        assert(result.isInstanceOf[DiffResult.StringDiff], s"expected StringDiff, got $result")
      }

      test("empty vs non-empty") {
        val result = DiffRuntime.diffString(pn, fn, sn, sh, "", "hello")
        assert(!result.isIdentical)
      }

      test("multiline strings") {
        val result = DiffRuntime.diffString(pn, fn, sn, sh, "line1\nline2", "line1\nline3")
        assert(!result.isIdentical)
        result match {
          case sd: DiffResult.StringDiff =>
            assert(sd.chunks.nonEmpty, "expected non-empty chunks")
          case _ => fail(s"expected StringDiff, got $result")
        }
      }
    }

    group("snapshotString") {

      test("snapshot returns Identical") {
        val result = DiffRuntime.snapshotString(pn, fn, sn, sh, "hello")
        assert(result.isIdentical)
        assert(result.isInstanceOf[DiffResult.Identical])
      }

      test("snapshot escapes special characters") {
        val result = DiffRuntime.snapshotString(pn, fn, sn, sh, "line1\nline2")
        result match {
          case i: DiffResult.Identical =>
            assert(i.display.contains("\\n"), s"expected escaped newline in: ${i.display}")
          case _ => fail(s"expected Identical, got $result")
        }
      }

      test("snapshot escapes tabs") {
        val result = DiffRuntime.snapshotString(pn, fn, sn, sh, "a\tb")
        result match {
          case i: DiffResult.Identical =>
            assert(i.display.contains("\\t"), s"expected escaped tab in: ${i.display}")
          case _ => fail(s"expected Identical, got $result")
        }
      }

      test("snapshot escapes carriage return") {
        val result = DiffRuntime.snapshotString(pn, fn, sn, sh, "a\rb")
        result match {
          case i: DiffResult.Identical =>
            assert(i.display.contains("\\r"), s"expected escaped CR in: ${i.display}")
          case _ => fail(s"expected Identical, got $result")
        }
      }

      test("snapshot escapes quotes") {
        val result = DiffRuntime.snapshotString(pn, fn, sn, sh, "say \"hello\"")
        result match {
          case i: DiffResult.Identical =>
            assert(i.display.contains("\\\""), s"expected escaped quote in: ${i.display}")
          case _ => fail(s"expected Identical, got $result")
        }
      }

      test("snapshot escapes backslash") {
        val result = DiffRuntime.snapshotString(pn, fn, sn, sh, "a\\b")
        result match {
          case i: DiffResult.Identical =>
            assert(i.display.contains("\\\\"), s"expected escaped backslash in: ${i.display}")
          case _ => fail(s"expected Identical, got $result")
        }
      }

      test("snapshot wraps in quotes") {
        val result = DiffRuntime.snapshotString(pn, fn, sn, sh, "hello")
        result match {
          case i: DiffResult.Identical =>
            assert(i.display.startsWith("\""), s"expected leading quote in: ${i.display}")
            assert(i.display.endsWith("\""), s"expected trailing quote in: ${i.display}")
          case _ => fail(s"expected Identical, got $result")
        }
      }
    }
  }
}
