package hearth.kindlings.diffderivation

final class DiffRendererSpec extends hearth.MacroSuite {

  private val pn = "test.Pretty"
  private val fn = "test.Full"
  private val sn = "Test"
  private val sh = "Test"

  group("DiffRenderer") {

    test("render Identical") {
      val result = DiffResult.Identical(pn, fn, sn, sh, "42")
      val rendered = DiffRenderer.render(result, RenderConfig.plain)
      assertEquals(rendered, "42")
    }

    test("render ValueChanged with plain mode") {
      val result = DiffResult.ValueChanged(pn, fn, sn, sh, "1", "2")
      val rendered = DiffRenderer.render(result, RenderConfig.plain)
      assert(rendered.contains("1"), s"expected left value in: $rendered")
      assert(rendered.contains("2"), s"expected right value in: $rendered")
      assert(rendered.contains("->"), s"expected arrow in: $rendered")
    }

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

    test("render with FullyQualified name style") {
      val result = DiffResult.Identical(pn, "com.example.Foo", "Foo", "Foo", "42")
      val rendered =
        DiffRenderer.render(result, RenderConfig(NameStyle.FullyQualified, ColorMode.Plain, Indent.Spaces(2)))
      assertEquals(rendered, "42")
    }

    test("render SeqDiff with plain markers") {
      val edits = Vector(
        Edit.Equal(DiffResult.Identical(pn, fn, "Int", "Int", "1")),
        Edit.Insert(DiffResult.Identical(pn, fn, "Int", "Int", "2")),
        Edit.Delete(DiffResult.Identical(pn, fn, "Int", "Int", "3"))
      )
      val result = DiffResult.SeqDiff(pn, fn, "List[Int]", "List", edits)
      val rendered = DiffRenderer.render(result, RenderConfig.plain)
      assert(rendered.contains("+"), s"expected + marker in: $rendered")
      assert(rendered.contains("-"), s"expected - marker in: $rendered")
    }

    test("render with Tab indentation") {
      val fields = Vector("x" -> DiffResult.ValueChanged(pn, fn, "Int", "Int", "1", "2"))
      val result = DiffResult.Record(pn, fn, "Point", "Point", fields)
      val rendered = DiffRenderer.render(result, RenderConfig(NameStyle.Simple, ColorMode.Plain, Indent.Tab))
      assert(rendered.contains("\t"), s"expected tab in: $rendered")
    }
  }
}
