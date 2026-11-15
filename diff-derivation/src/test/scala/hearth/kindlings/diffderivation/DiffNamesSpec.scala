package hearth.kindlings.diffderivation

import hearth.kindlings.diffderivation.internal.runtime.DiffRuntime

/** Type names in derived diffs: the generated code carries plain names only (the colors of the compile-time
  * `Type.prettyPrint` depend on how the type was written or read, so they differed between builds and ended up in the
  * output); `prettyName` colors the plain name at runtime, and only `ColorMode.Ansi` rendering uses it.
  */
final class DiffNamesSpec extends hearth.MacroSuite {

  private val Esc = '\u001b'

  private def allNodes(result: DiffResult): List[DiffResult] = result :: (result match {
    case r: DiffResult.Record       => r.fields.toList.flatMap { case (_, d) => allNodes(d) }
    case v: DiffResult.Variant      => allNodes(v.body)
    case t: DiffResult.TypeMismatch => allNodes(t.leftSnapshot) ++ allNodes(t.rightSnapshot)
    case s: DiffResult.SeqDiff      =>
      s.edits.toList.flatMap {
        case Edit.Equal(d)  => allNodes(d)
        case Edit.Insert(d) => allNodes(d)
        case Edit.Delete(d) => allNodes(d)
      }
    case o: DiffResult.OptionalDiff =>
      o.inner match {
        case DiffResult.OptionalContent.BothPresent(d) => allNodes(d)
        case DiffResult.OptionalContent.LeftOnly(d)    => allNodes(d)
        case DiffResult.OptionalContent.RightOnly(d)   => allNodes(d)
        case _                              => Nil
      }
    case _ => Nil
  })

  private def assertPlainNames[A](d: Diff[A], left: A, right: A): Unit = {
    List(d.plainName, d.simpleName, d.shortName).foreach(n => assert(!n.contains(Esc), n))
    d.prettyName ==> DiffRuntime.prettyTypeName(d.plainName)
    val result = d.diff(left, right)
    allNodes(result).foreach { node =>
      List(node.plainName, node.simpleName, node.shortName).foreach(n => assert(!n.contains(Esc), n))
      DiffRuntime.plainTypeName(node.prettyName) ==> node.plainName
    }
    val styles = List(NameStyle.Simple, NameStyle.Short, NameStyle.FullyQualified, NameStyle.Pretty)
    styles.foreach { style =>
      val rendered = DiffRenderer.render(result, RenderConfig(style, ColorMode.Plain, Indent.Spaces(2)))
      assert(!rendered.contains(Esc), s"$style: $rendered")
    }
  }

  group("derived diffs carry plain type names") {

    test("case class") {
      assertPlainNames(Diff.derived[Person], Person("a", 1), Person("b", 2))
    }

    test("nested case classes") {
      assertPlainNames(
        Diff.derived[PersonWithAddress],
        PersonWithAddress(Person("a", 1), Address("s", "c")),
        PersonWithAddress(Person("b", 1), Address("s", "d"))
      )
    }

    test("recursive case class through a List") {
      assertPlainNames(Diff.derived[Tree], Tree(1, List(Tree(2, Nil))), Tree(1, List(Tree(3, Nil), Tree(4, Nil))))
    }

    test("sealed trait, same and different subtypes") {
      val d = Diff.derived[Shape]
      assertPlainNames[Shape](d, Circle(1.0), Circle(2.0))
      assertPlainNames[Shape](d, Circle(1.0), Rectangle(1.0, 2.0))
    }

    test("recursive sealed trait") {
      assertPlainNames[TreeNode](Diff.derived[TreeNode], Branch(Leaf(1), Leaf(2)), Branch(Leaf(1), Leaf(3)))
    }

    test("collections, maps and options") {
      assertPlainNames(Diff.derived[Team], Team("t", List(Person("a", 1))), Team("t", List(Person("b", 1))))
      assertPlainNames(Diff.derived[Config], Config(Map("a" -> 1)), Config(Map("a" -> 2, "b" -> 3)))
      assertPlainNames(Diff.derived[Option[Person]], Some(Person("a", 1)), None)
    }

    test("the pretty name is the same for the same type however it is written") {
      Diff.derived[Person].prettyName ==> Diff.derived[hearth.kindlings.diffderivation.Person].prettyName
    }
  }

  group("DiffRuntime.prettyTypeName") {

    test("colors every identifier and keeps the punctuation") {
      val pretty = DiffRuntime.prettyTypeName("scala.Option[java.lang.String]")
      DiffRuntime.plainTypeName(pretty) ==> "scala.Option[java.lang.String]"
      pretty ==> "\u001b[32mscala\u001b[0m.\u001b[32mOption\u001b[0m[\u001b[32mjava\u001b[0m.\u001b[32mlang\u001b[0m." +
        "\u001b[32mString\u001b[0m]"
    }

    test("removes the colors of names compiled by older versions first") {
      DiffRuntime.prettyTypeName("\u001b[32mscala\u001b[0m.Int") ==> DiffRuntime.prettyTypeName("scala.Int")
    }
  }
}
