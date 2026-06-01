package hearth.kindlings.diffderivation

final class DiffSpec extends hearth.MacroSuite {

  group("Diff.derived") {

    group("case classes") {

      test("identical case classes") {
        val d = Diff.derived[Person]
        val result = d.diff(Person("Alice", 30), Person("Alice", 30))
        assert(result.isIdentical, s"expected identical, got $result")
      }

      test("changed primitive field") {
        val d = Diff.derived[Person]
        val result = d.diff(Person("Alice", 30), Person("Alice", 31))
        assert(!result.isIdentical, s"expected not identical, got $result")
        result match {
          case r: DiffResult.Record =>
            val ageField = r.fields.find(_._1 == "age")
            assert(ageField.isDefined, s"expected age field in $result")
            assert(!ageField.get._2.isIdentical, s"expected age to differ")
          case _ => fail(s"expected Record, got $result")
        }
      }

      test("nested case class") {
        val d = Diff.derived[PersonWithAddress]
        val left = PersonWithAddress(Person("Alice", 30), Address("Main St", "NYC"))
        val right = PersonWithAddress(Person("Alice", 31), Address("Main St", "NYC"))
        val result = d.diff(left, right)
        assert(!result.isIdentical, s"expected not identical")
      }
    }

    group("sealed traits") {

      test("same variant") {
        val d = Diff.derived[Shape]
        val result = d.diff(Circle(1.0), Circle(2.0))
        assert(!result.isIdentical, s"expected not identical, got $result")
      }

      test("different variants") {
        val d = Diff.derived[Shape]
        val result = d.diff(Circle(1.0), Rectangle(2.0, 3.0))
        assert(!result.isIdentical, s"expected not identical")
        result match {
          case _: DiffResult.TypeMismatch => ()
          case _                          =>
            result match {
              case v: DiffResult.Variant if !v.isIdentical => ()
              case _ => fail(s"expected TypeMismatch or non-identical Variant, got $result")
            }
        }
      }

      test("identical singletons") {
        val d = Diff.derived[SimpleEnum]
        val result = d.diff(Yes, Yes)
        assert(result.isIdentical, s"expected identical, got $result")
      }
    }

    group("recursive sealed trait") {

      test("identical trees") {
        val d = Diff.derived[TreeNode]
        val tree: TreeNode = Branch(Leaf(1), Branch(Leaf(2), Leaf(3)))
        val result = d.diff(tree, tree)
        assert(result.isIdentical, s"expected identical, got $result")
      }

      test("different leaf values") {
        val d = Diff.derived[TreeNode]
        val left: TreeNode = Branch(Leaf(1), Leaf(2))
        val right: TreeNode = Branch(Leaf(1), Leaf(99))
        val result = d.diff(left, right)
        assert(!result.isIdentical, s"expected not identical, got $result")
      }

      test("branch vs leaf") {
        val d = Diff.derived[TreeNode]
        val left: TreeNode = Branch(Leaf(1), Leaf(2))
        val right: TreeNode = Leaf(1)
        val result = d.diff(left, right)
        assert(!result.isIdentical, s"expected not identical, got $result")
        result match {
          case _: DiffResult.TypeMismatch => ()
          case _                          =>
            result match {
              case v: DiffResult.Variant if !v.isIdentical => ()
              case _ => fail(s"expected TypeMismatch or non-identical Variant, got $result")
            }
        }
      }
    }

    group("snapshot") {

      test("snapshot of case class") {
        val d = Diff.derived[Person]
        val snap = d.snapshot(Person("Alice", 30))
        assert(snap.isIdentical, s"snapshot should be identical, got $snap")
      }
    }

    group("rendering") {

      test("render changed record in plain mode") {
        val d = Diff.derived[Person]
        val result = d.diff(Person("Alice", 30), Person("Bob", 30))
        val rendered = DiffRenderer.render(result, RenderConfig.plain)
        assert(rendered.contains("Person"), s"expected Person in: $rendered")
        assert(rendered.contains("name"), s"expected name field in: $rendered")
      }

      test("render with simple name style") {
        val d = Diff.derived[Person]
        val result = d.diff(Person("Alice", 30), Person("Alice", 31))
        val rendered = DiffRenderer.render(result, RenderConfig.plain)
        assert(rendered.contains("Person"), s"expected type name in: $rendered")
      }
    }
  }
}
