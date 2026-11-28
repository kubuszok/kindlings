package hearth.kindlings.diffderivation

import hearth.MacroSuite

final case class DiffPrivate(a: Int, private val b: String, c: Boolean) {
  def bValue: String = b
}

// Hearth's `caseFieldValuesAt` has no `private val` constructor parameters (they have no accessor the generated code could
// call): derivations which read fields include them anyway (through `Product.productElement`), matched by name, so the
// fields after them are not shifted.
final class PrivateFieldsSpec extends MacroSuite {

  test("private val constructor fields are compared") {
    val d = Diff.derived[DiffPrivate]
    d.diff(DiffPrivate(1, "two", c = true), DiffPrivate(1, "two", c = true)).isIdentical ==> true
    d.diff(DiffPrivate(1, "two", c = true), DiffPrivate(1, "three", c = true)).isIdentical ==> false
    d.diff(DiffPrivate(1, "two", c = true), DiffPrivate(1, "two", c = false)).isIdentical ==> false
  }
}
