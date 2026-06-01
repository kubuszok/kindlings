package hearth.kindlings.diffderivation

final class DiffRendererSpec extends hearth.MacroSuite {

  private val pn = "test.Pretty"
  private val fn = "test.Full"
  private val sn = "Test"
  private val sh = "Test"

  group("DiffRenderer") {

    group("Identical") {

      test("render Identical") {
        val result = DiffResult.Identical(pn, fn, sn, sh, "42")
        val rendered = DiffRenderer.render(result, RenderConfig.plain)
        assertEquals(rendered, "42")
      }

      test("render Identical with ANSI mode produces same output") {
        val result = DiffResult.Identical(pn, fn, sn, sh, "42")
        val rendered = DiffRenderer.render(result, RenderConfig.default)
        assertEquals(rendered, "42")
      }
    }

    group("ValueChanged") {

      test("render ValueChanged with plain mode") {
        val result = DiffResult.ValueChanged(pn, fn, sn, sh, "1", "2")
        val rendered = DiffRenderer.render(result, RenderConfig.plain)
        assert(rendered.contains("1"), s"expected left value in: $rendered")
        assert(rendered.contains("2"), s"expected right value in: $rendered")
        assert(rendered.contains("->"), s"expected arrow in: $rendered")
      }

      test("render ValueChanged with ANSI mode has color codes") {
        val result = DiffResult.ValueChanged(pn, fn, sn, sh, "old", "new")
        val rendered = DiffRenderer.render(result, RenderConfig.default)
        // ANSI mode should still contain the values
        assert(rendered.contains("old"), s"expected left value in: $rendered")
        assert(rendered.contains("new"), s"expected right value in: $rendered")
        assert(rendered.contains("->"), s"expected arrow in: $rendered")
        // Should contain escape sequences
        assert(rendered.contains("["), s"expected ANSI codes in: $rendered")
      }
    }

    group("Record") {

      test("render Record shows type name") {
        val fields = Vector(
          "name" -> DiffResult.Identical(pn, fn, "String", "String", "\"Alice\""),
          "age" -> DiffResult.ValueChanged(pn, fn, "Int", "Int", "30", "31")
        )
        val result = DiffResult.Record(pn, fn, "Person", "Person", fields)
        val rendered = DiffRenderer.render(result, RenderConfig.plain)
        assert(rendered.contains("Person"), s"expected type name in: $rendered")
        assert(rendered.contains("age"), s"expected field name in: $rendered")
      }

      test("render all-identical Record shows ellipsis") {
        val fields = Vector(
          "x" -> DiffResult.Identical(pn, fn, "Int", "Int", "1"),
          "y" -> DiffResult.Identical(pn, fn, "Int", "Int", "2")
        )
        val result = DiffResult.Record(pn, fn, "Point", "Point", fields)
        val rendered = DiffRenderer.render(result, RenderConfig.plain)
        assert(rendered.contains("..."), s"expected ellipsis for identical record in: $rendered")
      }

      test("render Record with nested indent") {
        val innerFields = Vector(
          "x" -> DiffResult.ValueChanged(pn, fn, "Int", "Int", "1", "2")
        )
        val innerRecord = DiffResult.Record(pn, fn, "Inner", "Inner", innerFields)
        val outerFields = Vector(
          "inner" -> innerRecord
        )
        val result = DiffResult.Record(pn, fn, "Outer", "Outer", outerFields)
        val rendered = DiffRenderer.render(result, RenderConfig.plain)
        assert(rendered.contains("Outer"), s"expected Outer in: $rendered")
        assert(rendered.contains("Inner"), s"expected Inner in: $rendered")
      }

      test("render Record with Tab indentation") {
        val fields = Vector("x" -> DiffResult.ValueChanged(pn, fn, "Int", "Int", "1", "2"))
        val result = DiffResult.Record(pn, fn, "Point", "Point", fields)
        val rendered = DiffRenderer.render(result, RenderConfig(NameStyle.Simple, ColorMode.Plain, Indent.Tab))
        assert(rendered.contains("\t"), s"expected tab in: $rendered")
      }
    }

    group("Variant") {

      test("render Variant with changed body") {
        val body = DiffResult.ValueChanged(pn, fn, "Double", "Double", "1.0", "2.0")
        val result = DiffResult.Variant(pn, fn, "Shape", "Shape", "Circle", body)
        val rendered = DiffRenderer.render(result, RenderConfig.plain)
        assert(rendered.contains("Shape"), s"expected type name in: $rendered")
        assert(rendered.contains("Circle"), s"expected variant name in: $rendered")
      }

      test("render identical Variant shows ellipsis") {
        val body = DiffResult.Identical(pn, fn, "Double", "Double", "1.0")
        val result = DiffResult.Variant(pn, fn, "Shape", "Shape", "Circle", body)
        val rendered = DiffRenderer.render(result, RenderConfig.plain)
        assert(rendered.contains("..."), s"expected ellipsis for identical variant in: $rendered")
      }
    }

    group("TypeMismatch") {

      test("render TypeMismatch shows both variant names") {
        val leftSnap = DiffResult.Identical(pn, fn, "Circle", "Circle", "Circle(1.0)")
        val rightSnap = DiffResult.Identical(pn, fn, "Rectangle", "Rectangle", "Rectangle(2.0, 3.0)")
        val result = DiffResult.TypeMismatch(pn, fn, "Shape", "Shape", "Circle", leftSnap, "Rectangle", rightSnap)
        val rendered = DiffRenderer.render(result, RenderConfig.plain)
        assert(rendered.contains("Circle"), s"expected left variant in: $rendered")
        assert(rendered.contains("Rectangle"), s"expected right variant in: $rendered")
        assert(rendered.contains("->"), s"expected arrow in: $rendered")
        assert(rendered.contains("- "), s"expected minus prefix in: $rendered")
        assert(rendered.contains("+ "), s"expected plus prefix in: $rendered")
      }

      test("render TypeMismatch with ANSI colors") {
        val leftSnap = DiffResult.Identical(pn, fn, "A", "A", "A")
        val rightSnap = DiffResult.Identical(pn, fn, "B", "B", "B")
        val result = DiffResult.TypeMismatch(pn, fn, "Parent", "Parent", "A", leftSnap, "B", rightSnap)
        val rendered = DiffRenderer.render(result, RenderConfig.default)
        assert(rendered.contains("["), s"expected ANSI codes in: $rendered")
        assert(rendered.contains("A"), s"expected left variant in: $rendered")
        assert(rendered.contains("B"), s"expected right variant in: $rendered")
      }
    }

    group("SeqDiff") {

      test("render SeqDiff with all edit types") {
        val edits = Vector(
          Edit.Equal(DiffResult.Identical(pn, fn, "Int", "Int", "1")),
          Edit.Insert(DiffResult.Identical(pn, fn, "Int", "Int", "2")),
          Edit.Delete(DiffResult.Identical(pn, fn, "Int", "Int", "3"))
        )
        val result = DiffResult.SeqDiff(pn, fn, "List[Int]", "List", edits)
        val rendered = DiffRenderer.render(result, RenderConfig.plain)
        assert(rendered.contains("List[Int]"), s"expected type name in: $rendered")
        assert(rendered.contains("+ "), s"expected + marker in: $rendered")
        assert(rendered.contains("- "), s"expected - marker in: $rendered")
        assert(rendered.contains("1"), s"expected equal element in: $rendered")
      }

      test("render SeqDiff with ANSI colors") {
        val edits = Vector(
          Edit.Insert(DiffResult.Identical(pn, fn, "Int", "Int", "42")),
          Edit.Delete(DiffResult.Identical(pn, fn, "Int", "Int", "13"))
        )
        val result = DiffResult.SeqDiff(pn, fn, "List[Int]", "List", edits)
        val rendered = DiffRenderer.render(result, RenderConfig.default)
        assert(rendered.contains("[32m"), s"expected green ANSI for insert in: $rendered")
        assert(rendered.contains("[31m"), s"expected red ANSI for delete in: $rendered")
      }

      test("render empty SeqDiff") {
        val result = DiffResult.SeqDiff(pn, fn, "List[Int]", "List", Vector.empty)
        val rendered = DiffRenderer.render(result, RenderConfig.plain)
        assert(rendered.contains("List[Int]"), s"expected type name in: $rendered")
      }
    }

    group("MapDiff") {

      test("render MapDiff with matched entry") {
        val entries = Vector(
          DiffResult.MapEntry.Matched("key1", DiffResult.ValueChanged(pn, fn, "Int", "Int", "1", "2"))
        )
        val result = DiffResult.MapDiff(pn, fn, "Map[String, Int]", "Map", entries)
        val rendered = DiffRenderer.render(result, RenderConfig.plain)
        assert(rendered.contains("Map[String, Int]"), s"expected type name in: $rendered")
        assert(rendered.contains("key1"), s"expected key in: $rendered")
        assert(rendered.contains("->"), s"expected arrow in: $rendered")
      }

      test("render MapDiff with added entry") {
        val entries = Vector(
          DiffResult.MapEntry.Added("newKey", DiffResult.Identical(pn, fn, "Int", "Int", "42"))
        )
        val result = DiffResult.MapDiff(pn, fn, "Map[String, Int]", "Map", entries)
        val rendered = DiffRenderer.render(result, RenderConfig.plain)
        assert(rendered.contains("+ "), s"expected + marker in: $rendered")
        assert(rendered.contains("newKey"), s"expected added key in: $rendered")
      }

      test("render MapDiff with removed entry") {
        val entries = Vector(
          DiffResult.MapEntry.Removed("oldKey", DiffResult.Identical(pn, fn, "Int", "Int", "42"))
        )
        val result = DiffResult.MapDiff(pn, fn, "Map[String, Int]", "Map", entries)
        val rendered = DiffRenderer.render(result, RenderConfig.plain)
        assert(rendered.contains("- "), s"expected - marker in: $rendered")
        assert(rendered.contains("oldKey"), s"expected removed key in: $rendered")
      }

      test("render MapDiff with ANSI colors") {
        val entries = Vector(
          DiffResult.MapEntry.Added("a", DiffResult.Identical(pn, fn, "Int", "Int", "1")),
          DiffResult.MapEntry.Removed("b", DiffResult.Identical(pn, fn, "Int", "Int", "2"))
        )
        val result = DiffResult.MapDiff(pn, fn, "Map[String, Int]", "Map", entries)
        val rendered = DiffRenderer.render(result, RenderConfig.default)
        assert(rendered.contains("[32m"), s"expected green ANSI for added in: $rendered")
        assert(rendered.contains("[31m"), s"expected red ANSI for removed in: $rendered")
      }

      test("render MapDiff with mixed entries") {
        val entries = Vector(
          DiffResult.MapEntry.Matched("same", DiffResult.Identical(pn, fn, "Int", "Int", "1")),
          DiffResult.MapEntry.Added("new", DiffResult.Identical(pn, fn, "Int", "Int", "2")),
          DiffResult.MapEntry.Removed("gone", DiffResult.Identical(pn, fn, "Int", "Int", "3"))
        )
        val result = DiffResult.MapDiff(pn, fn, "Map", "Map", entries)
        val rendered = DiffRenderer.render(result, RenderConfig.plain)
        assert(rendered.contains("same"), s"expected matched key in: $rendered")
        assert(rendered.contains("new"), s"expected added key in: $rendered")
        assert(rendered.contains("gone"), s"expected removed key in: $rendered")
      }
    }

    group("SetDiff") {

      test("render SetDiff with matched entry") {
        val entries = Vector(
          DiffResult.SetEntry.Matched(DiffResult.Identical(pn, fn, "String", "String", "\"a\""))
        )
        val result = DiffResult.SetDiff(pn, fn, "Set[String]", "Set", entries)
        val rendered = DiffRenderer.render(result, RenderConfig.plain)
        assert(rendered.contains("Set[String]"), s"expected type name in: $rendered")
        assert(rendered.contains("\"a\""), s"expected element in: $rendered")
      }

      test("render SetDiff with added entry") {
        val entries = Vector(
          DiffResult.SetEntry.Added(DiffResult.Identical(pn, fn, "String", "String", "\"new\""))
        )
        val result = DiffResult.SetDiff(pn, fn, "Set[String]", "Set", entries)
        val rendered = DiffRenderer.render(result, RenderConfig.plain)
        assert(rendered.contains("+ "), s"expected + marker in: $rendered")
      }

      test("render SetDiff with removed entry") {
        val entries = Vector(
          DiffResult.SetEntry.Removed(DiffResult.Identical(pn, fn, "String", "String", "\"old\""))
        )
        val result = DiffResult.SetDiff(pn, fn, "Set[String]", "Set", entries)
        val rendered = DiffRenderer.render(result, RenderConfig.plain)
        assert(rendered.contains("- "), s"expected - marker in: $rendered")
      }

      test("render SetDiff with ANSI colors") {
        val entries = Vector(
          DiffResult.SetEntry.Added(DiffResult.Identical(pn, fn, "String", "String", "\"x\"")),
          DiffResult.SetEntry.Removed(DiffResult.Identical(pn, fn, "String", "String", "\"y\""))
        )
        val result = DiffResult.SetDiff(pn, fn, "Set[String]", "Set", entries)
        val rendered = DiffRenderer.render(result, RenderConfig.default)
        assert(rendered.contains("[32m"), s"expected green ANSI for added in: $rendered")
        assert(rendered.contains("[31m"), s"expected red ANSI for removed in: $rendered")
      }
    }

    group("OptionalDiff") {

      test("render BothPresent") {
        val inner = DiffResult.OptionalContent.BothPresent(
          DiffResult.ValueChanged(pn, fn, "String", "String", "\"a\"", "\"b\"")
        )
        val result = DiffResult.OptionalDiff(pn, fn, "Option[String]", "Option", inner)
        val rendered = DiffRenderer.render(result, RenderConfig.plain)
        assert(rendered.contains("Option[String]"), s"expected type name in: $rendered")
        assert(rendered.contains("\"a\""), s"expected left value in: $rendered")
        assert(rendered.contains("\"b\""), s"expected right value in: $rendered")
      }

      test("render LeftOnly (Some -> None)") {
        val inner = DiffResult.OptionalContent.LeftOnly(
          DiffResult.Identical(pn, fn, "String", "String", "\"hello\"")
        )
        val result = DiffResult.OptionalDiff(pn, fn, "Option[String]", "Option", inner)
        val rendered = DiffRenderer.render(result, RenderConfig.plain)
        assert(rendered.contains("Some"), s"expected Some in: $rendered")
        assert(rendered.contains("None"), s"expected None in: $rendered")
        assert(rendered.contains("->"), s"expected arrow in: $rendered")
      }

      test("render RightOnly (None -> Some)") {
        val inner = DiffResult.OptionalContent.RightOnly(
          DiffResult.Identical(pn, fn, "String", "String", "\"world\"")
        )
        val result = DiffResult.OptionalDiff(pn, fn, "Option[String]", "Option", inner)
        val rendered = DiffRenderer.render(result, RenderConfig.plain)
        assert(rendered.contains("None"), s"expected None in: $rendered")
        assert(rendered.contains("Some"), s"expected Some in: $rendered")
        assert(rendered.contains("->"), s"expected arrow in: $rendered")
      }

      test("render BothAbsent") {
        val inner = DiffResult.OptionalContent.BothAbsent
        val result = DiffResult.OptionalDiff(pn, fn, "Option[String]", "Option", inner)
        val rendered = DiffRenderer.render(result, RenderConfig.plain)
        assert(rendered.contains("None"), s"expected None in: $rendered")
      }

      test("render LeftOnly with ANSI colors") {
        val inner = DiffResult.OptionalContent.LeftOnly(
          DiffResult.Identical(pn, fn, "Int", "Int", "42")
        )
        val result = DiffResult.OptionalDiff(pn, fn, "Option[Int]", "Option", inner)
        val rendered = DiffRenderer.render(result, RenderConfig.default)
        assert(rendered.contains("["), s"expected ANSI codes in: $rendered")
      }
    }

    group("StringDiff") {

      test("render StringDiff with equal lines") {
        val chunks = Vector(DiffResult.StringChunk.EqualLine("hello world"))
        val result = DiffResult.StringDiff(pn, fn, "String", "String", chunks)
        val rendered = DiffRenderer.render(result, RenderConfig.plain)
        assert(rendered.contains("hello world"), s"expected text in: $rendered")
      }

      test("render StringDiff with insert and delete lines") {
        val chunks = Vector(
          DiffResult.StringChunk.DeleteLine("old line"),
          DiffResult.StringChunk.InsertLine("new line")
        )
        val result = DiffResult.StringDiff(pn, fn, "String", "String", chunks)
        val rendered = DiffRenderer.render(result, RenderConfig.plain)
        assert(rendered.contains("- old line"), s"expected delete marker in: $rendered")
        assert(rendered.contains("+ new line"), s"expected insert marker in: $rendered")
      }

      test("render StringDiff with changed line containing word chunks") {
        val words = Vector(
          DiffResult.WordChunk.EqualWord("hello"),
          DiffResult.WordChunk.DeleteWord(" old"),
          DiffResult.WordChunk.InsertWord(" new")
        )
        val chunks = Vector(DiffResult.StringChunk.ChangedLine(words))
        val result = DiffResult.StringDiff(pn, fn, "String", "String", chunks)
        val rendered = DiffRenderer.render(result, RenderConfig.plain)
        assert(rendered.contains("~ "), s"expected changed line prefix in: $rendered")
        assert(rendered.contains("hello"), s"expected equal word in: $rendered")
        assert(rendered.contains(" old"), s"expected delete word in: $rendered")
        assert(rendered.contains(" new"), s"expected insert word in: $rendered")
      }

      test("render StringDiff with changed word containing char chunks") {
        val chars = Vector(
          DiffResult.CharChunk.EqualChar("h"),
          DiffResult.CharChunk.DeleteChar("i"),
          DiffResult.CharChunk.InsertChar("ello")
        )
        val words = Vector(DiffResult.WordChunk.ChangedWord(chars))
        val chunks = Vector(DiffResult.StringChunk.ChangedLine(words))
        val result = DiffResult.StringDiff(pn, fn, "String", "String", chunks)
        val rendered = DiffRenderer.render(result, RenderConfig.plain)
        assert(rendered.contains("h"), s"expected equal char in: $rendered")
        assert(rendered.contains("i"), s"expected delete char in: $rendered")
        assert(rendered.contains("ello"), s"expected insert char in: $rendered")
      }

      test("render StringDiff with ANSI colors") {
        val chunks = Vector(
          DiffResult.StringChunk.DeleteLine("removed"),
          DiffResult.StringChunk.InsertLine("added")
        )
        val result = DiffResult.StringDiff(pn, fn, "String", "String", chunks)
        val rendered = DiffRenderer.render(result, RenderConfig.default)
        assert(rendered.contains("[31m"), s"expected red ANSI for delete in: $rendered")
        assert(rendered.contains("[32m"), s"expected green ANSI for insert in: $rendered")
      }
    }

    group("NameStyle") {

      test("render with FullyQualified name style") {
        val fields = Vector("x" -> DiffResult.ValueChanged(pn, "com.example.Int", "Int", "Int", "1", "2"))
        val result = DiffResult.Record(pn, "com.example.Foo", "Foo", "Foo", fields)
        val rendered =
          DiffRenderer.render(result, RenderConfig(NameStyle.FullyQualified, ColorMode.Plain, Indent.Spaces(2)))
        assert(rendered.contains("com.example.Foo"), s"expected fully qualified name in: $rendered")
      }

      test("render with Short name style") {
        val fields = Vector("x" -> DiffResult.ValueChanged(pn, fn, "Int", "I", "1", "2"))
        val result = DiffResult.Record(pn, fn, "FooBarBaz", "FBB", fields)
        val rendered = DiffRenderer.render(result, RenderConfig(NameStyle.Short, ColorMode.Plain, Indent.Spaces(2)))
        assert(rendered.contains("FBB"), s"expected short name in: $rendered")
      }

      test("render with Pretty name style") {
        val fields = Vector("x" -> DiffResult.ValueChanged("Pretty[Int]", fn, "Int", "Int", "1", "2"))
        val result = DiffResult.Record("Pretty[Foo]", fn, "Foo", "Foo", fields)
        val rendered = DiffRenderer.render(result, RenderConfig(NameStyle.Pretty, ColorMode.Plain, Indent.Spaces(2)))
        assert(rendered.contains("Pretty[Foo]"), s"expected pretty name in: $rendered")
      }
    }

    group("indentation") {

      test("render with Spaces(4) indentation") {
        val fields = Vector("x" -> DiffResult.ValueChanged(pn, fn, "Int", "Int", "1", "2"))
        val result = DiffResult.Record(pn, fn, "Point", "Point", fields)
        val rendered = DiffRenderer.render(result, RenderConfig(NameStyle.Simple, ColorMode.Plain, Indent.Spaces(4)))
        assert(rendered.contains("    x"), s"expected 4-space indent in: $rendered")
      }

      test("render with Tab indentation") {
        val fields = Vector("x" -> DiffResult.ValueChanged(pn, fn, "Int", "Int", "1", "2"))
        val result = DiffResult.Record(pn, fn, "Point", "Point", fields)
        val rendered = DiffRenderer.render(result, RenderConfig(NameStyle.Simple, ColorMode.Plain, Indent.Tab))
        assert(rendered.contains("\t"), s"expected tab in: $rendered")
      }
    }
  }
}
