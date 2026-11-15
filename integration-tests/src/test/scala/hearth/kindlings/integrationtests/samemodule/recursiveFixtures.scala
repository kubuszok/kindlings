package hearth.kindlings.integrationtests.samemodule

// Copies of `hearth.kindlings.integrationtests.crossmodule` (main sources, read from the classpath) compiled together with
// the tests. Keep them identical to those (apart from the package): `CrossModuleRecursionSpec` asserts that both behave
// the same.

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
