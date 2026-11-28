package hearth.kindlings.derivation.compiletime

import hearth.MacroCommons

/** Scala 3 generic tuples (`EmptyTuple`, `H *: T`, tuples with more than 22 elements), which Hearth's `CaseClass` does
  * not decompose. Scala 2 has only `TupleN`, which `CaseClass` handles: every method here returns `None`.
  */
trait GenericTuples { this: MacroCommons =>

  /** The element types of a tuple that is not a `TupleN` case class (always `None` on Scala 2). */
  protected def genericTupleElementTypes[A: Type]: Option[List[??]] = None

  /** `scala.Tuple.fromArray(Array[Object](elements*)).asInstanceOf[A]` (always `None` on Scala 2). */
  protected def constructGenericTuple[A: Type](elements: List[Expr[Any]]): Option[Expr[A]] = {
    val _ = elements
    None
  }
}
