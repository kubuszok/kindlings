package hearth.kindlings.integrationtests.crossmodule

// Recursive fixtures compiled in the MAIN sources, so that the tests (in `src/test`) always read them from the
// classpath - like users deriving instances for recursive types defined in another module. On Scala 3 Hearth's
// `Type.prettyPrint` of such a type is colored differently depending on whether it comes from the macro call site or
// from a field of a type read from TASTy, so a derivation that keyed its recursion detection or cache by `prettyPrint`
// did not recognize the recursion on the first level (tapir: one extra level of the product before the `SRef`).
//
// `CrossModuleRecursionSpec` compares every derivation against the same fixtures defined in the test sources
// (`hearth.kindlings.integrationtests.samemodule`), which must stay identical to these (apart from the package).

final case class RecTree(value: Int, children: List[RecTree])

final case class RecOpt(value: String, child: Option[RecOpt])

final case class RecMap(name: String, children: Map[String, RecMap])

sealed trait RecNode
final case class RecBranch(left: RecNode, right: RecNode) extends RecNode
final case class RecLeaf(value: Int) extends RecNode

final case class RecDept(name: String, employees: List[RecEmp])
final case class RecEmp(name: String, dept: Option[RecDept])

final case class RecGTree[A](value: A, children: List[RecGTree[A]])

object RecSamples {
  val tree: RecTree = RecTree(1, List(RecTree(2, Nil), RecTree(3, List(RecTree(4, Nil)))))
  val opt: RecOpt = RecOpt("a", Some(RecOpt("b", Some(RecOpt("c", None)))))
  val map: RecMap =
    RecMap("root", Map("x" -> RecMap("x1", Map("y" -> RecMap("y1", Map.empty))), "z" -> RecMap("z1", Map.empty)))
  val node: RecNode = RecBranch(RecLeaf(1), RecBranch(RecLeaf(2), RecLeaf(3)))
  val dept: RecDept = RecDept("d", List(RecEmp("e", Some(RecDept("d2", List(RecEmp("g", None))))), RecEmp("f", None)))
  val gtree: RecGTree[Int] = RecGTree(1, List(RecGTree(2, Nil), RecGTree(3, List(RecGTree(4, Nil)))))
}
