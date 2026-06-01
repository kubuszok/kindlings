package hearth.kindlings.diffderivation

final class DiffResultSpec extends hearth.MacroSuite {

  private val pn = "test.Pretty"
  private val fn = "test.Full"
  private val sn = "Test"
  private val sh = "Test"

  group("DiffResult") {

    group("Identical") {

      test("isIdentical returns true") {
        val r = DiffResult.Identical(pn, fn, sn, sh, "42")
        assert(r.isIdentical)
      }

      test("display is accessible") {
        val r = DiffResult.Identical(pn, fn, sn, sh, "hello")
        assertEquals(r.display, "hello")
      }

      test("productPrefix is Identical") {
        val r = DiffResult.Identical(pn, fn, sn, sh, "x")
        assertEquals(r.productPrefix, "Identical")
      }

      test("productArity is 1") {
        val r = DiffResult.Identical(pn, fn, sn, sh, "x")
        assertEquals(r.productArity, 1)
      }

      test("productElement(0) returns display") {
        val r = DiffResult.Identical(pn, fn, sn, sh, "hello")
        assertEquals(r.productElement(0), "hello")
      }

      test("productElement out of bounds throws") {
        val r = DiffResult.Identical(pn, fn, sn, sh, "x")
        var threw = false
        try r.productElement(1)
        catch { case _: IndexOutOfBoundsException => threw = true }
        assert(threw, "expected IndexOutOfBoundsException")
      }

      test("canEqual works") {
        val r1 = DiffResult.Identical(pn, fn, sn, sh, "x")
        val r2 = DiffResult.Identical(pn, fn, sn, sh, "y")
        assert(r1.canEqual(r2))
        assert(!r1.canEqual("not a DiffResult"))
      }

      test("toString includes display") {
        val r = DiffResult.Identical(pn, fn, sn, sh, "42")
        assertEquals(r.toString, "Identical(42)")
      }

      test("unapply extracts display") {
        val r = DiffResult.Identical(pn, fn, sn, sh, "hello")
        r match {
          case DiffResult.Identical(d) => assertEquals(d, "hello")
          case _                       => fail("unapply failed")
        }
      }

      test("name accessors") {
        val r = DiffResult.Identical("pretty", "plain", "simple", "short", "v")
        assertEquals(r.prettyName, "pretty")
        assertEquals(r.plainName, "plain")
        assertEquals(r.simpleName, "simple")
        assertEquals(r.shortName, "short")
      }
    }

    group("ValueChanged") {

      test("isIdentical returns false") {
        val r = DiffResult.ValueChanged(pn, fn, sn, sh, "1", "2")
        assert(!r.isIdentical)
      }

      test("left and right are accessible") {
        val r = DiffResult.ValueChanged(pn, fn, sn, sh, "old", "new")
        assertEquals(r.left, "old")
        assertEquals(r.right, "new")
      }

      test("productPrefix is ValueChanged") {
        val r = DiffResult.ValueChanged(pn, fn, sn, sh, "1", "2")
        assertEquals(r.productPrefix, "ValueChanged")
      }

      test("productArity is 2") {
        val r = DiffResult.ValueChanged(pn, fn, sn, sh, "1", "2")
        assertEquals(r.productArity, 2)
      }

      test("productElement returns left and right") {
        val r = DiffResult.ValueChanged(pn, fn, sn, sh, "a", "b")
        assertEquals(r.productElement(0), "a")
        assertEquals(r.productElement(1), "b")
      }

      test("productElement out of bounds throws") {
        val r = DiffResult.ValueChanged(pn, fn, sn, sh, "1", "2")
        var threw = false
        try r.productElement(2)
        catch { case _: IndexOutOfBoundsException => threw = true }
        assert(threw, "expected IndexOutOfBoundsException")
      }

      test("canEqual works") {
        val r1 = DiffResult.ValueChanged(pn, fn, sn, sh, "1", "2")
        val r2 = DiffResult.ValueChanged(pn, fn, sn, sh, "3", "4")
        assert(r1.canEqual(r2))
        assert(!r1.canEqual(42))
      }

      test("toString") {
        val r = DiffResult.ValueChanged(pn, fn, sn, sh, "a", "b")
        assertEquals(r.toString, "ValueChanged(a, b)")
      }

      test("unapply extracts left and right") {
        val r = DiffResult.ValueChanged(pn, fn, sn, sh, "x", "y")
        r match {
          case DiffResult.ValueChanged(l, rt) =>
            assertEquals(l, "x")
            assertEquals(rt, "y")
          case _ => fail("unapply failed")
        }
      }
    }

    group("Record") {

      test("isIdentical when all fields are identical") {
        val fields = Vector(
          "a" -> DiffResult.Identical(pn, fn, sn, sh, "1"),
          "b" -> DiffResult.Identical(pn, fn, sn, sh, "2")
        )
        val r = DiffResult.Record(pn, fn, sn, sh, fields)
        assert(r.isIdentical)
      }

      test("isIdentical false when any field differs") {
        val fields = Vector(
          "a" -> DiffResult.Identical(pn, fn, sn, sh, "1"),
          "b" -> DiffResult.ValueChanged(pn, fn, sn, sh, "2", "3")
        )
        val r = DiffResult.Record(pn, fn, sn, sh, fields)
        assert(!r.isIdentical)
      }

      test("productPrefix and arity") {
        val r = DiffResult.Record(pn, fn, sn, sh, Vector.empty)
        assertEquals(r.productPrefix, "Record")
        assertEquals(r.productArity, 1)
      }

      test("productElement(0) returns fields") {
        val fields = Vector("x" -> DiffResult.Identical(pn, fn, sn, sh, "1"))
        val r = DiffResult.Record(pn, fn, sn, sh, fields)
        assertEquals(r.productElement(0), fields)
      }

      test("productElement out of bounds throws") {
        val r = DiffResult.Record(pn, fn, sn, sh, Vector.empty)
        var threw = false
        try r.productElement(1)
        catch { case _: IndexOutOfBoundsException => threw = true }
        assert(threw, "expected IndexOutOfBoundsException")
      }

      test("canEqual works") {
        val r = DiffResult.Record(pn, fn, sn, sh, Vector.empty)
        assert(r.canEqual(DiffResult.Record(pn, fn, sn, sh, Vector.empty)))
        assert(!r.canEqual("x"))
      }

      test("toString includes simpleName") {
        val r = DiffResult.Record(pn, fn, "Foo", sh, Vector.empty)
        assert(r.toString.contains("Foo"), s"expected Foo in ${r.toString}")
      }

      test("unapply extracts fields") {
        val fields = Vector("x" -> DiffResult.Identical(pn, fn, sn, sh, "1"))
        val r = DiffResult.Record(pn, fn, sn, sh, fields)
        r match {
          case DiffResult.Record(fs) => assertEquals(fs, fields)
          case _                     => fail("unapply failed")
        }
      }
    }

    group("Variant") {

      test("isIdentical delegates to body") {
        val identical = DiffResult.Variant(pn, fn, sn, sh, "V", DiffResult.Identical(pn, fn, sn, sh, "x"))
        assert(identical.isIdentical)

        val changed = DiffResult.Variant(pn, fn, sn, sh, "V", DiffResult.ValueChanged(pn, fn, sn, sh, "1", "2"))
        assert(!changed.isIdentical)
      }

      test("productPrefix, arity, elements") {
        val body = DiffResult.Identical(pn, fn, sn, sh, "x")
        val r = DiffResult.Variant(pn, fn, sn, sh, "MyVariant", body)
        assertEquals(r.productPrefix, "Variant")
        assertEquals(r.productArity, 2)
        assertEquals(r.productElement(0), "MyVariant")
        assertEquals(r.productElement(1), body)
      }

      test("productElement out of bounds throws") {
        val r = DiffResult.Variant(pn, fn, sn, sh, "V", DiffResult.Identical(pn, fn, sn, sh, "x"))
        var threw = false
        try r.productElement(2)
        catch { case _: IndexOutOfBoundsException => threw = true }
        assert(threw, "expected IndexOutOfBoundsException")
      }

      test("canEqual works") {
        val r = DiffResult.Variant(pn, fn, sn, sh, "V", DiffResult.Identical(pn, fn, sn, sh, "x"))
        assert(r.canEqual(DiffResult.Variant(pn, fn, sn, sh, "W", DiffResult.Identical(pn, fn, sn, sh, "y"))))
        assert(!r.canEqual(42))
      }

      test("toString") {
        val r = DiffResult.Variant(pn, fn, "Shape", sh, "Circle", DiffResult.Identical(pn, fn, sn, sh, "x"))
        assert(r.toString.contains("Shape"), s"expected Shape in ${r.toString}")
        assert(r.toString.contains("Circle"), s"expected Circle in ${r.toString}")
      }

      test("unapply") {
        val body = DiffResult.Identical(pn, fn, sn, sh, "x")
        val r = DiffResult.Variant(pn, fn, sn, sh, "V", body)
        r match {
          case DiffResult.Variant(name, b) =>
            assertEquals(name, "V")
            assertEquals(b, body)
          case _ => fail("unapply failed")
        }
      }
    }

    group("TypeMismatch") {

      test("isIdentical always false") {
        val ls = DiffResult.Identical(pn, fn, sn, sh, "a")
        val rs = DiffResult.Identical(pn, fn, sn, sh, "b")
        val r = DiffResult.TypeMismatch(pn, fn, sn, sh, "A", ls, "B", rs)
        assert(!r.isIdentical)
      }

      test("fields are accessible") {
        val ls = DiffResult.Identical(pn, fn, sn, sh, "left")
        val rs = DiffResult.Identical(pn, fn, sn, sh, "right")
        val r = DiffResult.TypeMismatch(pn, fn, sn, sh, "Foo", ls, "Bar", rs)
        assertEquals(r.leftVariant, "Foo")
        assertEquals(r.rightVariant, "Bar")
        assertEquals(r.leftSnapshot, ls)
        assertEquals(r.rightSnapshot, rs)
      }

      test("productPrefix, arity, elements") {
        val ls = DiffResult.Identical(pn, fn, sn, sh, "l")
        val rs = DiffResult.Identical(pn, fn, sn, sh, "r")
        val r = DiffResult.TypeMismatch(pn, fn, sn, sh, "A", ls, "B", rs)
        assertEquals(r.productPrefix, "TypeMismatch")
        assertEquals(r.productArity, 4)
        assertEquals(r.productElement(0), "A")
        assertEquals(r.productElement(1), ls)
        assertEquals(r.productElement(2), "B")
        assertEquals(r.productElement(3), rs)
      }

      test("productElement out of bounds throws") {
        val ls = DiffResult.Identical(pn, fn, sn, sh, "l")
        val rs = DiffResult.Identical(pn, fn, sn, sh, "r")
        val r = DiffResult.TypeMismatch(pn, fn, sn, sh, "A", ls, "B", rs)
        var threw = false
        try r.productElement(4)
        catch { case _: IndexOutOfBoundsException => threw = true }
        assert(threw, "expected IndexOutOfBoundsException")
      }

      test("canEqual works") {
        val ls = DiffResult.Identical(pn, fn, sn, sh, "l")
        val rs = DiffResult.Identical(pn, fn, sn, sh, "r")
        val r = DiffResult.TypeMismatch(pn, fn, sn, sh, "A", ls, "B", rs)
        assert(r.canEqual(DiffResult.TypeMismatch(pn, fn, sn, sh, "C", ls, "D", rs)))
        assert(!r.canEqual("not"))
      }

      test("toString") {
        val ls = DiffResult.Identical(pn, fn, sn, sh, "l")
        val rs = DiffResult.Identical(pn, fn, sn, sh, "r")
        val r = DiffResult.TypeMismatch(pn, fn, "Shape", sh, "Circle", ls, "Rect", rs)
        assert(r.toString.contains("Shape"), s"expected Shape in ${r.toString}")
        assert(r.toString.contains("Circle"), s"expected Circle in ${r.toString}")
        assert(r.toString.contains("Rect"), s"expected Rect in ${r.toString}")
      }

      test("unapply") {
        val ls = DiffResult.Identical(pn, fn, sn, sh, "l")
        val rs = DiffResult.Identical(pn, fn, sn, sh, "r")
        val r = DiffResult.TypeMismatch(pn, fn, sn, sh, "A", ls, "B", rs)
        r match {
          case DiffResult.TypeMismatch(lv, lsnap, rv, rsnap) =>
            assertEquals(lv, "A")
            assertEquals(rv, "B")
            assertEquals(lsnap, ls)
            assertEquals(rsnap, rs)
          case _ => fail("unapply failed")
        }
      }
    }

    group("SeqDiff") {

      test("isIdentical when all edits are Equal with identical diffs") {
        val edits = Vector(
          Edit.Equal(DiffResult.Identical(pn, fn, sn, sh, "1")),
          Edit.Equal(DiffResult.Identical(pn, fn, sn, sh, "2"))
        )
        val r = DiffResult.SeqDiff(pn, fn, sn, sh, edits)
        assert(r.isIdentical)
      }

      test("isIdentical false when Insert present") {
        val edits = Vector(
          Edit.Equal(DiffResult.Identical(pn, fn, sn, sh, "1")),
          Edit.Insert(DiffResult.Identical(pn, fn, sn, sh, "2"))
        )
        val r = DiffResult.SeqDiff(pn, fn, sn, sh, edits)
        assert(!r.isIdentical)
      }

      test("isIdentical false when Delete present") {
        val edits = Vector(
          Edit.Delete(DiffResult.Identical(pn, fn, sn, sh, "1"))
        )
        val r = DiffResult.SeqDiff(pn, fn, sn, sh, edits)
        assert(!r.isIdentical)
      }

      test("isIdentical false when Equal contains non-identical diff") {
        val edits = Vector(
          Edit.Equal(DiffResult.ValueChanged(pn, fn, sn, sh, "1", "2"))
        )
        val r = DiffResult.SeqDiff(pn, fn, sn, sh, edits)
        assert(!r.isIdentical)
      }

      test("productPrefix, arity, element") {
        val edits = Vector.empty[Edit[DiffResult]]
        val r = DiffResult.SeqDiff(pn, fn, sn, sh, edits)
        assertEquals(r.productPrefix, "SeqDiff")
        assertEquals(r.productArity, 1)
        assertEquals(r.productElement(0), edits)
      }

      test("productElement out of bounds throws") {
        val r = DiffResult.SeqDiff(pn, fn, sn, sh, Vector.empty)
        var threw = false
        try r.productElement(1)
        catch { case _: IndexOutOfBoundsException => threw = true }
        assert(threw, "expected IndexOutOfBoundsException")
      }

      test("canEqual works") {
        val r = DiffResult.SeqDiff(pn, fn, sn, sh, Vector.empty)
        assert(r.canEqual(DiffResult.SeqDiff(pn, fn, sn, sh, Vector.empty)))
        assert(!r.canEqual(42))
      }

      test("toString") {
        val edits = Vector(Edit.Equal(DiffResult.Identical(pn, fn, sn, sh, "1")))
        val r = DiffResult.SeqDiff(pn, fn, "List", sh, edits)
        assert(r.toString.contains("List"), s"expected List in ${r.toString}")
        assert(r.toString.contains("1 edits"), s"expected count in ${r.toString}")
      }

      test("unapply") {
        val edits = Vector(Edit.Equal(DiffResult.Identical(pn, fn, sn, sh, "1")))
        val r = DiffResult.SeqDiff(pn, fn, sn, sh, edits)
        r match {
          case DiffResult.SeqDiff(es) => assertEquals(es, edits)
          case _                      => fail("unapply failed")
        }
      }
    }

    group("MapDiff") {

      test("isIdentical when all entries are identical Matched") {
        val entries = Vector(
          DiffResult.MapEntry.Matched("k", DiffResult.Identical(pn, fn, sn, sh, "v"))
        )
        val r = DiffResult.MapDiff(pn, fn, sn, sh, entries)
        assert(r.isIdentical)
      }

      test("isIdentical false when Added present") {
        val entries = Vector(
          DiffResult.MapEntry.Added("k", DiffResult.Identical(pn, fn, sn, sh, "v"))
        )
        val r = DiffResult.MapDiff(pn, fn, sn, sh, entries)
        assert(!r.isIdentical)
      }

      test("isIdentical false when Removed present") {
        val entries = Vector(
          DiffResult.MapEntry.Removed("k", DiffResult.Identical(pn, fn, sn, sh, "v"))
        )
        val r = DiffResult.MapDiff(pn, fn, sn, sh, entries)
        assert(!r.isIdentical)
      }

      test("isIdentical false when Matched value differs") {
        val entries = Vector(
          DiffResult.MapEntry.Matched("k", DiffResult.ValueChanged(pn, fn, sn, sh, "1", "2"))
        )
        val r = DiffResult.MapDiff(pn, fn, sn, sh, entries)
        assert(!r.isIdentical)
      }

      test("MapEntry.Matched isIdentical delegates to valueDiff") {
        val identical = DiffResult.MapEntry.Matched("k", DiffResult.Identical(pn, fn, sn, sh, "v"))
        assert(identical.isIdentical)
        val changed = DiffResult.MapEntry.Matched("k", DiffResult.ValueChanged(pn, fn, sn, sh, "1", "2"))
        assert(!changed.isIdentical)
      }

      test("MapEntry.Added isIdentical always false") {
        val e = DiffResult.MapEntry.Added("k", DiffResult.Identical(pn, fn, sn, sh, "v"))
        assert(!e.isIdentical)
      }

      test("MapEntry.Removed isIdentical always false") {
        val e = DiffResult.MapEntry.Removed("k", DiffResult.Identical(pn, fn, sn, sh, "v"))
        assert(!e.isIdentical)
      }

      test("productPrefix, arity, element") {
        val r = DiffResult.MapDiff(pn, fn, sn, sh, Vector.empty)
        assertEquals(r.productPrefix, "MapDiff")
        assertEquals(r.productArity, 1)
        assertEquals(r.productElement(0), Vector.empty)
      }

      test("productElement out of bounds throws") {
        val r = DiffResult.MapDiff(pn, fn, sn, sh, Vector.empty)
        var threw = false
        try r.productElement(1)
        catch { case _: IndexOutOfBoundsException => threw = true }
        assert(threw, "expected IndexOutOfBoundsException")
      }

      test("canEqual works") {
        val r = DiffResult.MapDiff(pn, fn, sn, sh, Vector.empty)
        assert(r.canEqual(DiffResult.MapDiff(pn, fn, sn, sh, Vector.empty)))
        assert(!r.canEqual("x"))
      }

      test("toString") {
        val entries = Vector(DiffResult.MapEntry.Matched("k", DiffResult.Identical(pn, fn, sn, sh, "v")))
        val r = DiffResult.MapDiff(pn, fn, "Map", sh, entries)
        assert(r.toString.contains("Map"), s"expected Map in ${r.toString}")
        assert(r.toString.contains("1 entries"), s"expected count in ${r.toString}")
      }

      test("unapply") {
        val entries = Vector(DiffResult.MapEntry.Added("k", DiffResult.Identical(pn, fn, sn, sh, "v")))
        val r = DiffResult.MapDiff(pn, fn, sn, sh, entries)
        r match {
          case DiffResult.MapDiff(es) => assertEquals(es, entries)
          case _                      => fail("unapply failed")
        }
      }
    }

    group("SetDiff") {

      test("isIdentical when all entries are identical Matched") {
        val entries = Vector(
          DiffResult.SetEntry.Matched(DiffResult.Identical(pn, fn, sn, sh, "a"))
        )
        val r = DiffResult.SetDiff(pn, fn, sn, sh, entries)
        assert(r.isIdentical)
      }

      test("isIdentical false when Added present") {
        val entries = Vector(
          DiffResult.SetEntry.Added(DiffResult.Identical(pn, fn, sn, sh, "a"))
        )
        val r = DiffResult.SetDiff(pn, fn, sn, sh, entries)
        assert(!r.isIdentical)
      }

      test("isIdentical false when Removed present") {
        val entries = Vector(
          DiffResult.SetEntry.Removed(DiffResult.Identical(pn, fn, sn, sh, "a"))
        )
        val r = DiffResult.SetDiff(pn, fn, sn, sh, entries)
        assert(!r.isIdentical)
      }

      test("SetEntry.Matched isIdentical delegates to diff") {
        val identical = DiffResult.SetEntry.Matched(DiffResult.Identical(pn, fn, sn, sh, "a"))
        assert(identical.isIdentical)
        val changed = DiffResult.SetEntry.Matched(DiffResult.ValueChanged(pn, fn, sn, sh, "1", "2"))
        assert(!changed.isIdentical)
      }

      test("SetEntry.Added isIdentical always false") {
        val e = DiffResult.SetEntry.Added(DiffResult.Identical(pn, fn, sn, sh, "a"))
        assert(!e.isIdentical)
      }

      test("SetEntry.Removed isIdentical always false") {
        val e = DiffResult.SetEntry.Removed(DiffResult.Identical(pn, fn, sn, sh, "a"))
        assert(!e.isIdentical)
      }

      test("productPrefix, arity, element") {
        val r = DiffResult.SetDiff(pn, fn, sn, sh, Vector.empty)
        assertEquals(r.productPrefix, "SetDiff")
        assertEquals(r.productArity, 1)
        assertEquals(r.productElement(0), Vector.empty)
      }

      test("productElement out of bounds throws") {
        val r = DiffResult.SetDiff(pn, fn, sn, sh, Vector.empty)
        var threw = false
        try r.productElement(1)
        catch { case _: IndexOutOfBoundsException => threw = true }
        assert(threw, "expected IndexOutOfBoundsException")
      }

      test("canEqual works") {
        val r = DiffResult.SetDiff(pn, fn, sn, sh, Vector.empty)
        assert(r.canEqual(DiffResult.SetDiff(pn, fn, sn, sh, Vector.empty)))
        assert(!r.canEqual(42))
      }

      test("toString") {
        val entries = Vector(DiffResult.SetEntry.Added(DiffResult.Identical(pn, fn, sn, sh, "a")))
        val r = DiffResult.SetDiff(pn, fn, "Set", sh, entries)
        assert(r.toString.contains("Set"), s"expected Set in ${r.toString}")
        assert(r.toString.contains("1 entries"), s"expected count in ${r.toString}")
      }

      test("unapply") {
        val entries = Vector(DiffResult.SetEntry.Removed(DiffResult.Identical(pn, fn, sn, sh, "a")))
        val r = DiffResult.SetDiff(pn, fn, sn, sh, entries)
        r match {
          case DiffResult.SetDiff(es) => assertEquals(es, entries)
          case _                      => fail("unapply failed")
        }
      }
    }

    group("OptionalDiff") {

      test("isIdentical for BothPresent with identical inner") {
        val inner = DiffResult.OptionalContent.BothPresent(DiffResult.Identical(pn, fn, sn, sh, "x"))
        val r = DiffResult.OptionalDiff(pn, fn, sn, sh, inner)
        assert(r.isIdentical)
      }

      test("isIdentical false for BothPresent with changed inner") {
        val inner = DiffResult.OptionalContent.BothPresent(DiffResult.ValueChanged(pn, fn, sn, sh, "1", "2"))
        val r = DiffResult.OptionalDiff(pn, fn, sn, sh, inner)
        assert(!r.isIdentical)
      }

      test("isIdentical true for BothAbsent") {
        val r = DiffResult.OptionalDiff(pn, fn, sn, sh, DiffResult.OptionalContent.BothAbsent)
        assert(r.isIdentical)
      }

      test("isIdentical false for LeftOnly") {
        val inner = DiffResult.OptionalContent.LeftOnly(DiffResult.Identical(pn, fn, sn, sh, "x"))
        val r = DiffResult.OptionalDiff(pn, fn, sn, sh, inner)
        assert(!r.isIdentical)
      }

      test("isIdentical false for RightOnly") {
        val inner = DiffResult.OptionalContent.RightOnly(DiffResult.Identical(pn, fn, sn, sh, "x"))
        val r = DiffResult.OptionalDiff(pn, fn, sn, sh, inner)
        assert(!r.isIdentical)
      }

      test("productPrefix, arity, element") {
        val inner = DiffResult.OptionalContent.BothAbsent
        val r = DiffResult.OptionalDiff(pn, fn, sn, sh, inner)
        assertEquals(r.productPrefix, "OptionalDiff")
        assertEquals(r.productArity, 1)
        assertEquals(r.productElement(0), inner)
      }

      test("productElement out of bounds throws") {
        val r = DiffResult.OptionalDiff(pn, fn, sn, sh, DiffResult.OptionalContent.BothAbsent)
        var threw = false
        try r.productElement(1)
        catch { case _: IndexOutOfBoundsException => threw = true }
        assert(threw, "expected IndexOutOfBoundsException")
      }

      test("canEqual works") {
        val r = DiffResult.OptionalDiff(pn, fn, sn, sh, DiffResult.OptionalContent.BothAbsent)
        assert(r.canEqual(DiffResult.OptionalDiff(pn, fn, sn, sh, DiffResult.OptionalContent.BothAbsent)))
        assert(!r.canEqual("x"))
      }

      test("toString") {
        val r = DiffResult.OptionalDiff(pn, fn, "Option", sh, DiffResult.OptionalContent.BothAbsent)
        assert(r.toString.contains("Option"), s"expected Option in ${r.toString}")
        assert(r.toString.contains("BothAbsent"), s"expected BothAbsent in ${r.toString}")
      }

      test("unapply") {
        val inner = DiffResult.OptionalContent.LeftOnly(DiffResult.Identical(pn, fn, sn, sh, "x"))
        val r = DiffResult.OptionalDiff(pn, fn, sn, sh, inner)
        r match {
          case DiffResult.OptionalDiff(i) => assertEquals(i, inner)
          case _                          => fail("unapply failed")
        }
      }
    }

    group("StringDiff") {

      test("isIdentical when all chunks are EqualLine") {
        val chunks = Vector(
          DiffResult.StringChunk.EqualLine("line 1"),
          DiffResult.StringChunk.EqualLine("line 2")
        )
        val r = DiffResult.StringDiff(pn, fn, sn, sh, chunks)
        assert(r.isIdentical)
      }

      test("isIdentical false when InsertLine present") {
        val chunks = Vector(DiffResult.StringChunk.InsertLine("new"))
        val r = DiffResult.StringDiff(pn, fn, sn, sh, chunks)
        assert(!r.isIdentical)
      }

      test("isIdentical false when DeleteLine present") {
        val chunks = Vector(DiffResult.StringChunk.DeleteLine("old"))
        val r = DiffResult.StringDiff(pn, fn, sn, sh, chunks)
        assert(!r.isIdentical)
      }

      test("isIdentical false when ChangedLine present") {
        val chunks = Vector(DiffResult.StringChunk.ChangedLine(Vector.empty))
        val r = DiffResult.StringDiff(pn, fn, sn, sh, chunks)
        assert(!r.isIdentical)
      }

      test("productPrefix, arity, element") {
        val chunks = Vector.empty[DiffResult.StringChunk]
        val r = DiffResult.StringDiff(pn, fn, sn, sh, chunks)
        assertEquals(r.productPrefix, "StringDiff")
        assertEquals(r.productArity, 1)
        assertEquals(r.productElement(0), chunks)
      }

      test("productElement out of bounds throws") {
        val r = DiffResult.StringDiff(pn, fn, sn, sh, Vector.empty)
        var threw = false
        try r.productElement(1)
        catch { case _: IndexOutOfBoundsException => threw = true }
        assert(threw, "expected IndexOutOfBoundsException")
      }

      test("canEqual works") {
        val r = DiffResult.StringDiff(pn, fn, sn, sh, Vector.empty)
        assert(r.canEqual(DiffResult.StringDiff(pn, fn, sn, sh, Vector.empty)))
        assert(!r.canEqual(42))
      }

      test("toString") {
        val chunks = Vector(DiffResult.StringChunk.EqualLine("hi"), DiffResult.StringChunk.InsertLine("bye"))
        val r = DiffResult.StringDiff(pn, fn, "String", sh, chunks)
        assert(r.toString.contains("String"), s"expected String in ${r.toString}")
        assert(r.toString.contains("2 chunks"), s"expected count in ${r.toString}")
      }

      test("unapply") {
        val chunks = Vector(DiffResult.StringChunk.EqualLine("hi"))
        val r = DiffResult.StringDiff(pn, fn, sn, sh, chunks)
        r match {
          case DiffResult.StringDiff(cs) => assertEquals(cs, chunks)
          case _                         => fail("unapply failed")
        }
      }
    }
  }
}
