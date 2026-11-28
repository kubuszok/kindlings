package hearth.kindlings.fastshowpretty

import hearth.MacroSuite

final case class FspPrivate(a: Int, private val b: String, c: Boolean) {
  def bValue: String = b
}

// Hearth's `caseFieldValuesAt` has no `private val` constructor parameters (they have no accessor the generated code could
// call): derivations which read fields include them anyway (through `Product.productElement`), matched by name, so the
// fields after them are not shifted.
final class PrivateFieldsSpec extends MacroSuite {

  test("private val constructor fields are rendered, as by toString") {
    FastShowPretty.render(FspPrivate(1, "two", c = true), RenderConfig.Compact) ==>
      "FspPrivate(\na = 1,\nb = \"two\",\nc = true\n)"
  }
}
