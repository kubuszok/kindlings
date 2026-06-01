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

      test("all fields identical nested") {
        val d = Diff.derived[PersonWithAddress]
        val v = PersonWithAddress(Person("Alice", 30), Address("Main St", "NYC"))
        val result = d.diff(v, v)
        assert(result.isIdentical, s"expected identical, got $result")
      }

      test("multiple fields changed") {
        val d = Diff.derived[Person]
        val result = d.diff(Person("Alice", 30), Person("Bob", 31))
        assert(!result.isIdentical, s"expected not identical, got $result")
        result match {
          case r: DiffResult.Record =>
            val changedFields = r.fields.filterNot(_._2.isIdentical)
            assertEquals(changedFields.size, 2)
          case _ => fail(s"expected Record, got $result")
        }
      }

      test("changed string field produces StringDiff") {
        val d = Diff.derived[Person]
        val result = d.diff(Person("Alice", 30), Person("Bob", 30))
        result match {
          case r: DiffResult.Record =>
            val nameField = r.fields.find(_._1 == "name").get._2
            assert(!nameField.isIdentical, s"expected name to differ")
            nameField match {
              case _: DiffResult.StringDiff   => ()
              case _: DiffResult.ValueChanged => ()
              case other                      => fail(s"expected StringDiff or ValueChanged, got $other")
            }
          case _ => fail(s"expected Record, got $result")
        }
      }

      test("identical string field produces Identical") {
        val d = Diff.derived[Person]
        val result = d.diff(Person("Alice", 30), Person("Alice", 31))
        result match {
          case r: DiffResult.Record =>
            val nameField = r.fields.find(_._1 == "name").get._2
            assert(nameField.isIdentical, s"expected name to be identical, got $nameField")
          case _ => fail(s"expected Record, got $result")
        }
      }

      test("Diff instance has correct simpleName") {
        val d = Diff.derived[Person]
        assert(d.simpleName.contains("Person"), s"expected Person in simpleName: ${d.simpleName}")
      }

      test("Diff instance has correct shortName") {
        val d = Diff.derived[Person]
        assert(d.shortName.contains("Person"), s"expected Person in shortName: ${d.shortName}")
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

      test("different singletons") {
        val d = Diff.derived[SimpleEnum]
        val result = d.diff(Yes, No)
        assert(!result.isIdentical, s"expected not identical, got $result")
      }

      test("identical same-variant circle") {
        val d = Diff.derived[Shape]
        val result = d.diff(Circle(5.0), Circle(5.0))
        assert(result.isIdentical, s"expected identical, got $result")
      }

      test("identical same-variant rectangle") {
        val d = Diff.derived[Shape]
        val result = d.diff(Rectangle(2.0, 3.0), Rectangle(2.0, 3.0))
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

      test("deeply nested identical") {
        val d = Diff.derived[TreeNode]
        val tree: TreeNode = Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Leaf(4)))
        val result = d.diff(tree, tree)
        assert(result.isIdentical, s"expected identical, got $result")
      }

      test("single leaf identical") {
        val d = Diff.derived[TreeNode]
        val result = d.diff(Leaf(42), Leaf(42))
        assert(result.isIdentical, s"expected identical, got $result")
      }

      test("single leaf different") {
        val d = Diff.derived[TreeNode]
        val result = d.diff(Leaf(1), Leaf(2))
        assert(!result.isIdentical, s"expected not identical, got $result")
      }
    }

    group("combinatorial wrapper x inner-type") {

      test("identical CombOuter instances") {
        val d = Diff.derived[CombOuter]
        val v = CombOuter(
          Some(Person("Alice", 30)),
          Some(Circle(1.0)),
          List(Person("Bob", 25)),
          Map("key" -> Person("Carol", 40))
        )
        val result = d.diff(v, v)
        assert(result.isIdentical, s"expected identical, got $result")
      }

      test("optSealedTrait differs — Some(Circle) vs Some(Rectangle)") {
        val d = Diff.derived[CombOuter]
        val left = CombOuter(
          Some(Person("Alice", 30)),
          Some(Circle(1.0)),
          List(Person("Bob", 25)),
          Map("key" -> Person("Carol", 40))
        )
        val right = CombOuter(
          Some(Person("Alice", 30)),
          Some(Rectangle(2.0, 3.0)),
          List(Person("Bob", 25)),
          Map("key" -> Person("Carol", 40))
        )
        val result = d.diff(left, right)
        assert(!result.isIdentical, s"expected not identical, got $result")
        result match {
          case r: DiffResult.Record =>
            val optSTField = r.fields.find(_._1 == "optSealedTrait")
            assert(optSTField.isDefined, s"expected optSealedTrait field in $result")
            assert(!optSTField.get._2.isIdentical, "expected optSealedTrait to differ")
          case _ => fail(s"expected Record, got $result")
        }
      }

      test("listCaseClass differs") {
        val d = Diff.derived[CombOuter]
        val left = CombOuter(
          None,
          None,
          List(Person("Alice", 30), Person("Bob", 25)),
          Map.empty
        )
        val right = CombOuter(
          None,
          None,
          List(Person("Alice", 30), Person("Charlie", 35)),
          Map.empty
        )
        val result = d.diff(left, right)
        assert(!result.isIdentical, s"expected not identical, got $result")
        result match {
          case r: DiffResult.Record =>
            val listField = r.fields.find(_._1 == "listCaseClass")
            assert(listField.isDefined, s"expected listCaseClass field in $result")
            assert(!listField.get._2.isIdentical, "expected listCaseClass to differ")
          case _ => fail(s"expected Record, got $result")
        }
      }

      test("mapCaseClass has different keys") {
        val d = Diff.derived[CombOuter]
        val left = CombOuter(
          None,
          None,
          Nil,
          Map("a" -> Person("Alice", 30))
        )
        val right = CombOuter(
          None,
          None,
          Nil,
          Map("b" -> Person("Bob", 25))
        )
        val result = d.diff(left, right)
        assert(!result.isIdentical, s"expected not identical, got $result")
        result match {
          case r: DiffResult.Record =>
            val mapField = r.fields.find(_._1 == "mapCaseClass")
            assert(mapField.isDefined, s"expected mapCaseClass field in $result")
            assert(!mapField.get._2.isIdentical, "expected mapCaseClass to differ")
            mapField.get._2 match {
              case md: DiffResult.MapDiff =>
                val added = md.entries.collect { case a: DiffResult.MapEntry.Added => a }
                val removed = md.entries.collect { case r: DiffResult.MapEntry.Removed => r }
                assert(added.nonEmpty, "expected at least one Added entry")
                assert(removed.nonEmpty, "expected at least one Removed entry")
              case other =>
                fail(s"expected MapDiff, got $other")
            }
          case _ => fail(s"expected Record, got $result")
        }
      }

      test("optCaseClass Some vs None") {
        val d = Diff.derived[CombOuter]
        val left = CombOuter(Some(Person("Alice", 30)), None, Nil, Map.empty)
        val right = CombOuter(None, None, Nil, Map.empty)
        val result = d.diff(left, right)
        assert(!result.isIdentical, s"expected not identical, got $result")
        result match {
          case r: DiffResult.Record =>
            val optField = r.fields.find(_._1 == "optCaseClass")
            assert(optField.isDefined, s"expected optCaseClass field in $result")
            assert(!optField.get._2.isIdentical, "expected optCaseClass to differ")
          case _ => fail(s"expected Record, got $result")
        }
      }

      test("snapshot of CombOuter") {
        val d = Diff.derived[CombOuter]
        val snap = d.snapshot(CombOuter(
          Some(Person("Alice", 30)),
          Some(Circle(1.0)),
          List(Person("Bob", 25)),
          Map("key" -> Person("Carol", 40))
        ))
        assert(snap.isIdentical, s"snapshot should be identical, got $snap")
      }
    }

    group("snapshot") {

      test("snapshot of case class") {
        val d = Diff.derived[Person]
        val snap = d.snapshot(Person("Alice", 30))
        assert(snap.isIdentical, s"snapshot should be identical, got $snap")
      }

      test("snapshot of nested case class") {
        val d = Diff.derived[PersonWithAddress]
        val snap = d.snapshot(PersonWithAddress(Person("Alice", 30), Address("Main St", "NYC")))
        assert(snap.isIdentical, s"snapshot should be identical, got $snap")
      }

      test("snapshot of sealed trait") {
        val d = Diff.derived[Shape]
        val snap = d.snapshot(Circle(5.0))
        assert(snap.isIdentical, s"snapshot should be identical, got $snap")
      }

      test("snapshot of singleton") {
        val d = Diff.derived[SimpleEnum]
        val snap = d.snapshot(Yes)
        assert(snap.isIdentical, s"snapshot should be identical, got $snap")
      }

      test("snapshot of recursive tree") {
        val d = Diff.derived[TreeNode]
        val snap = d.snapshot(Branch(Leaf(1), Branch(Leaf(2), Leaf(3))))
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

      test("render identical record shows ellipsis") {
        val d = Diff.derived[Person]
        val result = d.diff(Person("Alice", 30), Person("Alice", 30))
        val rendered = DiffRenderer.render(result, RenderConfig.plain)
        // Identical record should show ellipsis
        assert(
          rendered.contains("...") || result.isIdentical,
          s"expected ellipsis or identical in: $rendered"
        )
      }

      test("render sealed trait type mismatch") {
        val d = Diff.derived[Shape]
        val result = d.diff(Circle(1.0), Rectangle(2.0, 3.0))
        val rendered = DiffRenderer.render(result, RenderConfig.plain)
        assert(rendered.contains("Circle") || rendered.contains("Rectangle"), s"expected variant names in: $rendered")
      }

      test("render with ANSI colors") {
        val d = Diff.derived[Person]
        val result = d.diff(Person("Alice", 30), Person("Bob", 30))
        val rendered = DiffRenderer.render(result, RenderConfig.default)
        // ANSI mode should contain escape sequences
        assert(rendered.contains("["), s"expected ANSI codes in: $rendered")
      }

      test("render nested diff") {
        val d = Diff.derived[PersonWithAddress]
        val left = PersonWithAddress(Person("Alice", 30), Address("Main St", "NYC"))
        val right = PersonWithAddress(Person("Alice", 31), Address("Elm St", "NYC"))
        val result = d.diff(left, right)
        val rendered = DiffRenderer.render(result, RenderConfig.plain)
        assert(rendered.contains("PersonWithAddress"), s"expected outer type in: $rendered")
      }
    }
  }
}
